use std::io::Write;

use crate::{
    node::Node,
    parser::{Object, VarObject},
    ty::Type,
};

const ARG_REGS8: [&'static str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
const ARG_REGS16: [&'static str; 6] = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
const ARG_REGS32: [&'static str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
const ARG_REGS64: [&'static str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

struct CodeGenerator<'a> {
    writer: &'a mut dyn Write,
    program: &'a Vec<Object>,
    counter: usize,
    current_fn_name: String,
    depth: usize,
}

impl CodeGenerator<'_> {
    fn gen_data(&mut self) {
        for global in self.program.iter() {
            let Object::VarObject(o) = global else {
                continue;
            };

            self.emit(&format!(".data"));
            self.emit(&format!("  .globl {}", o.name));
            self.emit(&format!("{}:", o.name));

            if o.init_data.is_empty() {
                self.emit(&format!("  .zero {}", o.ty.size()));
            } else {
                for c in o.init_data.as_bytes() {
                    self.emit(&format!("  .byte {}", c));
                }
                self.emit("  .byte 0");
            }
        }
        self.emit(""); // newline
    }

    fn emit(&mut self, s: &str) {
        let _ = writeln!(self.writer, "{s}");
    }

    fn push(&mut self) {
        self.emit("  push %rax");
        self.depth += 1;
    }

    fn pop(&mut self, reg: &str) {
        self.emit(&format!("  pop {}", reg));
        self.depth -= 1;
    }

    fn next_count(&mut self) -> usize {
        let c = self.counter;
        self.counter += 1;
        c
    }

    fn load(&mut self, ty: &Type) {
        if matches!(
            ty,
            Type::Struct { .. } | Type::Union { .. } | Type::Array { .. }
        ) {
            // If it is an array, do not attempt to load a value to the
            // register because in general we can't load an entire array to a
            // register. As a result, the result of an evaluation of an array
            // becomes not the array itself but the address of the array.
            // This is where "array is automatically converted to a pointer to
            // the first element of the array in C" occurs.
            return;
        }

        match ty.size() {
            1 => self.emit("  mov %al, (%rdi)"),
            2 => self.emit("  mov %ax, (%rdi)"),
            4 => self.emit("  mov %eax, (%rdi)"),
            8 => self.emit("  mov %rax, (%rdi)"),
            _ => {
                eprintln!("Unexpected ty size {}", ty.size());
                panic!();
            }
        }
    }

    fn store(&mut self, ty: &Type) {
        self.pop("%rdi");

        if matches!(ty, Type::Struct { .. } | Type::Union { .. }) {
            for i in 0..ty.size() {
                self.emit(&format!("  mov {}(%rax), %r8b", i));
                self.emit(&format!("  mov %r8b, {}(%rdi)", i));
            }
            return;
        }

        match ty.size() {
            1 => self.emit("  mov %al, (%rdi)"),
            2 => self.emit("  mov %ax, (%rdi)"),
            4 => self.emit("  mov %eax, (%rdi)"),
            8 => self.emit("  mov %rax, (%rdi)"),
            _ => {
                eprintln!("Unexpected ty size {}", ty.size());
                panic!();
            }
        }
    }

    fn gen_address(&mut self, node: &Node) {
        match node {
            Node::Variable(v) => {
                let var = v.var.as_var_object();
                if var.is_local {
                    self.emit(&format!("  lea {}(%rbp), %rax", var.offset.get()));
                } else {
                    self.emit(&format!("  lea {}(%rip), %rax", var.name));
                }
            }
            Node::Dereference(d) => self.gen_expr(&*d.lhs),
            Node::Comma(c) => {
                self.gen_expr(&*c.lhs);
                self.gen_address(&*c.rhs);
            }
            Node::StructMember(s) => {
                self.gen_address(&*s.lhs);
                self.emit(&format!("  add ${}, %rax", s.member.offset));
            }
            _ => {
                eprintln!("Not an lvalue");
                panic!();
            }
        }
    }

    fn gen_expr(&mut self, node: &Node) {
        match node {
            Node::Numeric(num) => self.emit(&format!("  mov ${}, %rax", num.val)),
            Node::Neg(n) => {
                self.gen_expr(&*n.lhs);
                self.emit("  neg %rax");
            }
            Node::Variable(_) | Node::StructMember(_) => {
                self.gen_address(node);
                self.load(node.ty());
            }
            Node::Assign(a) => {
                self.gen_address(&*a.lhs);
                self.push();
                self.gen_expr(&*a.rhs);
                self.store(&a.ty);
            }
            Node::AddressOf(a) => self.gen_address(&*a.lhs),
            Node::Dereference(d) => {
                self.gen_expr(&*d.lhs);
                self.load(&d.ty);
            }
            Node::StmtExpr(s) => {
                for node in s.block_body.iter() {
                    self.gen_stmt(node);
                }
            }
            // TODO function call
            Node::Comma(c) => {
                self.gen_expr(&*c.lhs);
                self.gen_expr(&*c.rhs);
            }
            Node::Cast(c) => {
                self.gen_expr(&*c.lhs);
                // TODO cast
            }
            _ => (),
        }

        let mut di = "";
        let mut ax = "";
        if let Some(b) = node.binary_node() {
            self.gen_expr(&*b.lhs);
            self.push();
            self.gen_expr(&*b.rhs);
            self.pop("%rdi");

            if b.lhs.ty().size() == 8 {
                ax = "%rax";
                di = "%rdi";
            } else {
                ax = "%eax";
                di = "%edi";
            }
        }

        match node {
            Node::Add(_) => self.emit(&format!("  add {}, {}", di, ax)),
            Node::Sub(_) => self.emit(&format!("  sub {}, {}", di, ax)),
            Node::Mul(_) => self.emit(&format!("  imul {}, {}", di, ax)),
            Node::Div(div) => {
                if div.ty.size() == 8 {
                    self.emit("  cqo");
                } else {
                    self.emit("  cdq");
                }
                self.emit(&format!("  idiv {}", di));
            }
            Node::LessThan(_) | Node::LessThanEq(_) | Node::Eq(_) | Node::NotEq(_) => {
                self.emit(&format!("  cmp {}, {}", di, ax));
                match node {
                    Node::Eq(_) => self.emit("  sete %al"),
                    Node::NotEq(_) => self.emit("  setne %al"),
                    Node::LessThanEq(_) => self.emit("  setle %al"),
                    Node::LessThan(_) => self.emit("  setl %al"),
                    _ => unreachable!(),
                }
                self.emit("  movzb %al, %rax");
            }
            _ => (),
        }
    }

    fn gen_stmt(&mut self, node: &Node) {
        match node {
            Node::For(f) => {
                let c = self.next_count();
                self.gen_stmt(&*f.init);
                self.emit(&format!(".L.begin.{}:", c));

                if let Some(cond) = &f.cond {
                    self.gen_expr(&*cond);
                    self.emit("  cmp $0, %rax");
                    self.emit(&format!("  je .L.end.{}", c));
                }

                self.gen_stmt(&*f.then);
                if let Some(inc) = &f.inc {
                    self.gen_expr(&*inc);
                }
                self.emit(&format!("  jmp .L.begin.{}", c));
                self.emit(&format!(".L.end.{}:", c));
            }
            Node::While(w) => {
                let c = self.next_count();

                self.emit(&format!(".L.begin.{}:", c));

                self.gen_expr(&*w.cond);
                self.emit("  cmp $0, %rax");
                self.emit(&format!("  je .L.end.{}", c));
                self.gen_stmt(&*w.then);
                self.emit(&format!("  jmp .L.begin.{}", c));
                self.emit(&format!(".L.end.{}:", c));
            }
            Node::If(i) => {
                let c = self.next_count();
                self.gen_expr(&*i.cond);

                self.emit(&format!("  cmp $0, %rax"));
                self.emit(&format!("  je .L.else.{}", c));
                self.gen_stmt(&*i.then);
                self.emit(&format!("  jmp .L.end.{}", c));
                self.emit(&format!(".L.else.{}:", c));
                if let Some(els) = &i.els {
                    self.gen_stmt(&*els);
                }
                self.emit(&format!(".L.end.{}:", c));
            }
            Node::Block(block) => {
                for node in block.block_body.iter() {
                    self.gen_stmt(&node)
                }
            }
            Node::Return(r) => {
                self.gen_expr(&*r.lhs);
                self.emit(&format!("  jmp .L.return.{}", self.current_fn_name));
            }
            Node::ExprStmt(e) => self.gen_expr(&*e.lhs),
            _ => (),
        }
    }

    fn align_to(n: usize, align: usize) -> usize {
        return ((n + align - 1) / align) * align;
    }

    fn assign_offset_to_local_vars(locals: &Vec<VarObject>) -> usize {
        let mut off = 0;
        for local in locals.iter().rev() {
            off += local.ty.size();
            off = Self::align_to(off, local.ty.alignment());
            let local_offset = i32::try_from(off);
            if let Err(e) = local_offset {
                println!("usize -> i32 conversion failed: {e}");
                panic!();
            }
            local.offset.set(-local_offset.unwrap());
        }
        Self::align_to(off, 16)
    }

    fn generate(&mut self) {
        // generate data for globals
        self.gen_data();

        for global in self.program.iter() {
            let Object::FunctionObject(f) = global else {
                continue;
            };
            if !f.is_func_def {
                continue;
            }

            let stack_size = Self::assign_offset_to_local_vars(&f.locals);

            self.emit(&format!("  .globl {}", f.name));
            self.emit(&format!("  .text"));
            self.emit(&format!("{}:", f.name));

            if stack_size > 0 {
                self.emit(&format!("  sub ${}, %rsp", stack_size));
            }

            let mut i = 0;
            for param in f.params.iter() {
                let Object::VarObject(v) = param else {
                    continue;
                };

                match param.ty().size() {
                    1 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS8[i], v.offset.get());
                        self.emit(&code);
                        i += 1;
                    }
                    2 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS16[i], v.offset.get());
                        self.emit(&code);
                        i += 1;
                    }
                    4 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS32[i], v.offset.get());
                        self.emit(&code);
                        i += 1;
                    }
                    8 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS64[i], v.offset.get());
                        self.emit(&code);
                        i += 1;
                    }
                    _ => {
                        eprintln!("Unknown ty size");
                        panic!();
                    }
                }

                if i >= 6 {
                    eprintln!("More than 6 args not supported");
                    panic!();
                }
            }

            self.current_fn_name = f.name.clone();

            for node in f.body.iter() {
                self.gen_stmt(node);
            }

            // prologue
            self.emit("  push %rbp");
            self.emit("  mov %rsp, %rbp");

            // epilogue
            self.emit(&format!(".L.return.{}:", f.name));
            self.emit("  mov %rbp, %rsp");
            self.emit("  pop %rbp");
            self.emit("  ret");
        }
    }
}

pub fn generate(writer: &mut dyn Write, program: &Vec<Object>) {
    let mut gen = CodeGenerator {
        writer,
        program,
        counter: 1,
        current_fn_name: String::new(),
        depth: 0,
    };
    gen.generate();
}
