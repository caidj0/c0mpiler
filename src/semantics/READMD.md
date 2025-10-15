# Analyzer 结构
```
HashMap<ResolvedTy, Impls> // 存储各种实现
HashMap<NodeId, Scope> // 存储 Scope 的相关信息
```

# ResolvedTy 结构 
名字（可选）+ 类型（BuiltIn, Ref, Array, Struct, Enum, Tup, Fn, ImplicitSelf, Never）

# Expr 类型记录
Semantic 应记录每个 Expr 的类型，记录每个 Path Expression, Method Call Expression 的对象，对于 trait 应？

# 类型反向约束
应实现一个类型求解器，传入两个类型指针，将两个类型统一，同时修改两个类型

TypePtr 是一个指针的指针，当统一两个类型时，只需修改一个类型，并将另一个指向它