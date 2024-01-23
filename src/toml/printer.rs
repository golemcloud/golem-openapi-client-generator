use crate::printer::{PrintContext, TreePrinter};

pub struct StringContext {
    ctx: String,
}

impl StringContext {
    pub fn new() -> StringContext {
        StringContext { ctx: String::new() }
    }

    pub fn print_to_string(mut self, p: TreePrinter<StringContext>) -> String {
        p.print(&mut self);

        self.ctx
    }
}

impl PrintContext for StringContext {
    fn print_str(&mut self, s: &str) {
        self.ctx.push_str(s)
    }
}

pub fn unit() -> TreePrinter<StringContext> {
    TreePrinter::unit()
}
