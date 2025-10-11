use crate::ir::{LLVMBuilder, LLVMContext, LLVMModule};

#[derive(Debug)]
pub struct IRGenHelper {
    pub(crate) context: LLVMContext,
    pub(crate) builder: LLVMBuilder,
    pub(crate) module: LLVMModule,
}

impl Default for IRGenHelper {
    fn default() -> Self {
        let mut context = LLVMContext::default();
        let builder = context.create_builder();
        let module = context.create_module("crate");

        Self {
            context,
            builder,
            module,
        }
    }
}
