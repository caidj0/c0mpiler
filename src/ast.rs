macro_rules! define_nodes {
    ($($id:ident), *) => {
        paste::paste!{

            pub trait AbstractNode {
                fn visit(node: Node) -> Option<Node> {
                    match(node) {

                        $(
                            Node::$id(specified) => Self::[<visit_ $id:lower>](specified),
                        )*

                    }
                }
                $(
                    #[allow(unused_variables)]
                    fn [<visit_ $id:lower>](node: [<$id Node>]) -> Option<[<$id Node>]> {unimplemented!();}
                )*
            }

            $(
                pub struct [<$id Node>];
                impl AbstractNode for [<$id Node>]{}
            )*

            pub enum Node {
                $(
                    $id([<$id Node>]),
                )*
            }

            impl Node {
                fn visit(self) -> Option<Node> {
                    match(node) {

                        $(
                            Node::$id(specified) => Self::[<visit_ $id:lower>](specified),
                        )*

                    }
                }
            }

        }
    };
}

define_nodes! {
    Root,
    Crate,
        InnerAttribute,
        Item,
            OuterAttribute,
            VisItem,
            MacroItem,
            Visibility,

    Module,
    ExternCrate,
    UseDeclaration,
    Function,
        FunctionQualifiers,
            ItemSafety,
            Abi,
        GenericParams,
        FunctionParameters,
        FunctionReturnType,
        WhereClause,
        BlockExpression,
    TypeAlias,
    Struct,
    Enumeration,
    Union,
    ConstantItem,
    StaticItem,
    Trait,
    Implementation,
    ExternBlock,

    GenericParam,
    TypeParam,
        TypeParamBounds
}

pub fn aaa() {
    let a = RootNode;
    AbstractNode::visit_root(a);
}