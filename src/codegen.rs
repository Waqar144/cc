use std::io::Write;

use crate::{
    node::Node,
    parser::{Object, VarObject},
};

struct CodeGenerator<'a> {
    writer: &'a mut dyn Write,
    program: &'a Vec<Object>,
}

impl CodeGenerator<'_> {
    fn gen_data(&mut self) {
        for global in self.program.iter() {
            if let Object::FunctionObject(_) = global {
                continue;
            }
        }
    }

    fn emit(&mut self, s: &str) {
        let _ = writeln!(self.writer, "{s}");
    }

    fn gen_stmt(&mut self, node: &Node) {
        //
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

            for param in f.params.iter() {
                match param.ty().size() {
                    1 => (),
                    2 => (),
                    4 => (),
                    8 => (),
                    _ => (),
                }
            }

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
    let mut gen = CodeGenerator { writer, program };
    gen.generate();
}
