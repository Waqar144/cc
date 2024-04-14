use crate::debug::node_name;
use std::{cell::Cell, io::Write};

use crate::{
    node::Node,
    parser::{FunctionObject, Object},
    ty::Type,
};

const ARG_REGS8: [&'static str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
const ARG_REGS16: [&'static str; 6] = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
const ARG_REGS32: [&'static str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
const ARG_REGS64: [&'static str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

static mut TRACE_DEPTH: usize = 1;

struct CodeGenerator<'a> {
    writer: &'a mut dyn Write,
    program: &'a Vec<Object>,
    counter: usize,
    current_fn_name: String,
    depth: usize,
    current_fn: Cell<Option<&'a FunctionObject>>,
}

struct TraceRaii {}

impl TraceRaii {
    fn new() -> TraceRaii {
        unsafe {
            TRACE_DEPTH += 1;
        };
        TraceRaii {}
    }
}

impl Drop for TraceRaii {
    fn drop(&mut self) {
        unsafe {
            TRACE_DEPTH -= 1;
        };
    }
}

fn trace(_s: &str) {
    // unsafe {
    //     eprintln!(
    //         "\x1b[1;32m{:>depth$} {}\x1b[0m",
    //         "==",
    //         s,
    //         depth = TRACE_DEPTH
    //     );
    // }
}

enum TypeId {
    I8 = 0,
    I16 = 1,
    I32 = 2,
    I64 = 3,
}

fn get_type_id_for_type(ty: &Type) -> TypeId {
    match ty {
        Type::Char { .. } => TypeId::I8,
        Type::Short { .. } => TypeId::I16,
        Type::Int { .. } => TypeId::I32,
        _ => TypeId::I64,
    }
}

impl CodeGenerator<'_> {
    fn gen_data(&mut self) {
        let _t = TraceRaii::new();
        trace("gen_data");
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

    fn cast(&mut self, from: &Type, to: &Type) {
        let from = get_type_id_for_type(from);
        let to = get_type_id_for_type(to);
        match (from, to) {
            // TODO
            // (TypeId::I8, TypeId::I8) => todo!(),
            // (TypeId::I8, TypeId::I16) => todo!(),
            // (TypeId::I8, TypeId::I32) => todo!(),
            (TypeId::I8, TypeId::I64) => self.emit("movsxd %eax, %rax"),
            (TypeId::I16, TypeId::I8) => self.emit("movsbl %al, %eax"),
            // (TypeId::I16, TypeId::I16) => todo!(),
            // (TypeId::I16, TypeId::I32) => todo!(),
            (TypeId::I16, TypeId::I64) => self.emit("movsxd %eax, %rax"),
            (TypeId::I32, TypeId::I8) => self.emit("movsbl %al, %eax"),
            (TypeId::I32, TypeId::I16) => self.emit("movswl %ax, %eax"),
            // (TypeId::I32, TypeId::I32) => todo!(),
            (TypeId::I32, TypeId::I64) => self.emit("movsxd %eax, %rax"),
            (TypeId::I64, TypeId::I8) => self.emit("movsbl %al, %eax"),
            (TypeId::I64, TypeId::I16) => self.emit("movswl %ax, %eax"),
            // (TypeId::I64, TypeId::I32) => todo!(),
            // (TypeId::I64, TypeId::I64) => todo!(),
            _ => (),
        }
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
            1 => self.emit("  movsbl (%rax), %eax"),
            2 => self.emit("  movswl (%rax), %eax"),
            4 => self.emit("  movsxd (%rax), %rax"),
            8 => self.emit("  mov (%rax), %rax"),
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

    fn var_object_for_idx(&self, idx: usize, is_local: bool) -> Option<&Object> {
        if is_local {
            if let Some(f) = self.current_fn.get() {
                return f.locals.get(idx);
            }
        } else {
            return self.program.get(idx);
        }
        None
    }

    fn gen_address(&mut self, node: &Node) {
        trace(&format!("gen_address({})", node_name(node)));
        let _t = TraceRaii::new();
        match node {
            Node::Variable(v) => {
                let var = self.var_object_for_idx(v.idx, v.is_local);
                let Some(Object::VarObject(var)) = var else {
                    eprintln!(
                        "Expected to find a variable for given var idx, idx: {}, is_local: {}",
                        v.idx, v.is_local
                    );
                    panic!();
                };

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
        trace(&format!("gen_expr({})", node_name(node)));
        let _t = TraceRaii::new();
        let mut handled = true;
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
            Node::FunctionCall(f) => {
                for arg in f.args.iter() {
                    self.gen_expr(arg);
                    self.push();
                }

                for n in (0..f.args.len()).rev() {
                    self.pop(ARG_REGS64[n]);
                }

                self.emit("  xor %rax, %rax\t #clear rax, func call ");
                self.emit(&format!("  call {}", f.name));
            }
            Node::Comma(c) => {
                self.gen_expr(&*c.lhs);
                self.gen_expr(&*c.rhs);
            }
            Node::Cast(c) => {
                self.gen_expr(&*c.lhs);
                self.cast(c.lhs.ty(), &c.ty);
            }
            _ => handled = false,
        }

        if handled {
            return;
        }

        let mut di = "";
        let mut ax = "";
        if let Some(b) = node.binary_node() {
            self.gen_expr(&*b.rhs);
            self.push();
            self.gen_expr(&*b.lhs);
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
        trace(&format!("gen_stmt({})", node_name(node)));
        let _t = TraceRaii::new();
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

    fn assign_offset_to_local_vars(locals: &Vec<Object>) -> usize {
        let mut off = 0;
        for local in locals.iter().rev() {
            let Object::VarObject(local) = local else {
                eprintln!("Expected VarObject");
                panic!();
            };
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
        trace("generate");
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

            // prologue
            self.emit("  push %rbp");
            self.emit("  mov %rsp, %rbp");
            if stack_size > 0 {
                self.emit(&format!("  sub ${}, %rsp", stack_size));
            }

            let mut i = 0;
            for param in f.params.iter() {
                let l = f.locals.get(*param);
                let Some(Object::VarObject(v)) = l else {
                    eprintln!("Expected a local for given param idx {param}");
                    panic!();
                };

                match v.ty.size() {
                    1 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS8[i], v.offset.get());
                        self.emit(&code);
                    }
                    2 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS16[i], v.offset.get());
                        self.emit(&code);
                    }
                    4 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS32[i], v.offset.get());
                        self.emit(&code);
                    }
                    8 => {
                        let code = format!("  mov {}, {}(%rbp)", ARG_REGS64[i], v.offset.get());
                        self.emit(&code);
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
                i += 1;
            }

            self.current_fn_name = f.name.clone();
            // store a ref to current function
            self.current_fn.set(Some(f));

            trace(&format!(
                "BEGIN Iterating function body, body nodes size: {}",
                f.body.len()
            ));
            for node in f.body.iter() {
                self.gen_stmt(node);
            }
            trace(&format!(
                "END Iterating function body, body nodes size: {}",
                f.body.len()
            ));

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
        current_fn: Cell::new(None),
    };
    gen.generate();
}
