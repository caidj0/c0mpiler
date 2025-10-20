use crate::ast::{
    Crate,
    expr::{
        AddrOfExpr, ArrayExpr, AssignExpr, AssignOpExpr, BinaryExpr, BlockExpr, BreakExpr,
        CallExpr, CastExpr, ConstBlockExpr, ContinueExpr, Expr, FieldExpr, ForLoopExpr, IfExpr,
        IndexExpr, LetExpr, LitExpr, LoopExpr, MatchExpr, MethodCallExpr, PathExpr, RangeExpr,
        RepeatExpr, RetExpr, StructExpr, TupExpr, UnaryExpr, UnderscoreExpr, WhileExpr,
    },
    item::{
        AssocItemKind, ConstItem, EnumItem, FnItem, ImplItem, Item, ModItem, StructItem, TraitItem,
    },
    pat::{
        IdentPat, LitPat, OrPat, Pat, PathPat, RangePat, RefPat, RestPat, SlicePat, StructPat,
        TuplePat, WildPat,
    },
    stmt::{LocalStmt, Stmt},
};

macro_rules! func_sig {
    ($cat:ident, $res:ident, $($name:ident)?) => {
        paste::paste!{
            fn [<visit_ $($name:snake _)? $cat:snake>]<'tmp>(&mut self, [<$cat:snake>] : &'ast [<$($name:camel)? $cat:camel>], extra: Self::[<$cat:camel Extra>]<'tmp>) -> Self::[<$res:camel Res>]<'_>;
        }
    };
}

macro_rules! add_func {
    ($cat:ident, $res:ident, ($($name:ident),*)) => {
        func_sig!{$cat, $res,}
        $(
            func_sig!{$cat, $res, $name}
        )*
    };
}

pub trait Visitor<'ast> {
    type DefaultRes<'res>
    where
        Self: 'res;
    type ExprRes<'res>
    where
        Self: 'res;
    type PatRes<'res>
    where
        Self: 'res;
    type StmtRes<'res>
    where
        Self: 'res;

    type CrateExtra<'tmp>;
    type ItemExtra<'tmp>;
    type StmtExtra<'tmp>;
    type ExprExtra<'tmp>;
    type PatExtra<'tmp>;

    fn visit_crate<'tmp>(
        &mut self,
        krate: &'ast Crate,
        extra: Self::CrateExtra<'tmp>,
    ) -> Self::DefaultRes<'_>;

    add_func! {
        Item, Default,
        (
            Const, Fn, Mod, Enum, Struct, Trait, Impl
        )
    }

    fn visit_associate_item<'tmp>(
        &mut self,
        item: &'ast Item<AssocItemKind>,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_>;

    add_func! {
        Stmt, Stmt,
        (Local)
    }

    add_func! {
        Expr, Expr,
        (
            Array,ConstBlock,Call,MethodCall,Tup,Binary,Unary,Lit,Cast,Let,If,While,ForLoop,Loop,Match,Block,Assign,AssignOp,Field,Index,Range,Underscore,Path,AddrOf,Break,Continue,Ret,Struct,Repeat
        )
    }

    add_func! {
        Pat, Pat,
        (
            Wild, Ident, Struct, Or, Path, Tuple, Ref, Lit, Range, Slice, Rest
        )
    }
}
