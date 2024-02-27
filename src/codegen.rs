use std::io::Write;

use crate::parser::Object;

// static void gen_data(const std::vector<std::unique_ptr<Object>>& prog)
// {
//     for (const auto& global : prog) {
//         if (global->isFunction) {
//             continue;
//         }
//
//         println(".data");
//         println("  .globl {}", global->name);
//         println("{}:", global->name);
//         if (global->initData.empty()) {
//             println("  .zero {}", global->type->size);
//         } else {
//             for (int c : global->initData)
//                 println("  .byte {}", c);
//             println("  .byte 0");
//         }
//     }
//     println("");
// }

struct CodeGenerator<'a> {
    writer: &'a mut dyn Write,
    program: &'a Vec<Object>,
}

impl CodeGenerator<'_> {
    fn gen_data(&mut self) {
        for global in self.program {
            if let Object::FunctionObject(_) = global {
                continue;
            }
        }
    }

    fn emit(&mut self, s: &str) {
        let _ = writeln!(self.writer, "{s}");
    }

    fn generate(&mut self) {
        self.gen_data();

        for global in self.program {
            let Object::FunctionObject(f) = global else {
                continue;
            };
            if !f.is_func_def {
                continue;
            }

            self.emit(&format!("  .globl {}", f.name));
            self.emit(&format!("  .text"));
            self.emit(&format!("{}:", f.name));

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
