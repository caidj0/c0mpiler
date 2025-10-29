IRGen 通过调用 LLVMIRBuilder 将 ast 翻译到 IR。

不需要对空长度类型特殊处理：clang 可以 handle 它们. 
<!-- gener 要对空长度类型 (使用 `E` 指代) 特殊处理，这具体包括 `()`, `struct{}`, `[ty; 0]`, `enum{}`, `struct {E,E,...}`, `[E; num]`, `!`. 空长度类型不出现在 ir 中，使用 `void` 代替，在 struct 中出现则直接删去那一项（注意其余项下标的改变）. 空长度 struct 和 enum 直接也不进行定义. -->

gener 需要能够翻译 item，包括 struct, enum, constant 和 function. Item 的命名要保证唯一性，这可以通过 `{scopeid}.name` 实现. struct 要注意前向声明（先添加 opaque, 再添加 body）. 简单常量（数字）应直接放到 ConstantInt 中，复杂常量应添加到 globalvariable 中，使用指针调用。Enum 可以参照 rust 直接转为数字（ir 中不会出现 enum 类型）。Free function 应该比较容易处理，对于 impl 内的 function 需要处理 Self 类型，对于 impl trait 中的 function 还需要为每个 impl 都单独实现一份函数, 记得实现 default function 和 default constant.

在翻译 block 时, gener 需要能够正确获取全局变量（常量和函数），特别是 `{receiver}.{method}()` 的情况，它们在可以实现在 ir 中名称唯一，只要能找到就行。对于局部变量，gener 需要维护变量的类型（~~或者让 analyzer 记录每个有名字的变量的类型~~let 和 function arg 都是有类型的，直接使用那个类型即可）。对于尾随表达式，处理方法应该是将其作为 blockexpr 的类型，将 blockexpr 的 valueptr 设为尾随表达式的 valueptr（如果尾随表达式是一个 pathexpr，则设为对应的 valueptr，否则设为 instruction 的 valueptr），特别地，若 blockexpr 为 `void` 类型，则没有 valueptr（这个好像只能靠 analyzer 维护？）

path expression 怎么找到对应的变量？method expression 怎么找到对应的方法？如何处理为 trait 实现的函数，特别是 Default 函数？

💀💀💀对于返回聚合类型的函数，翻译为 ir 时它的实际返回值为 void，并且需要添加一个指针类型的参数，调用时调用函数先在栈上开出空间，将指针传给被调用函数，被调用函数直接往指针里写。💀💀💀

Let Stmt 直接翻译为 alloca + store.

`if cond {block}` 可以翻译为
```llvm 
{last_label}:
...
br cond

cond:
...
br i1 %cond, label take, label next

take:
...
br label next

next:
```

`if cond {ifture} else {iffalse}` 可以翻译为 
```llvm 
{last_label}:
...
br label cond

cond:
...
br i1 %cond, label take, label next

take:
...
br label next

else:
...
br label next

next:
%ifresult = phi <ty> [{takeval}, take], [{elseval}, else] ; 如果 ifblock 类型不为 void 
...
```

更长的 chain if 应该可以逐步拆开.

`loop {}` 可以翻译为 
```llvm 
{last_label}:
...
br label loop

loop:
...
br label loop
```
如果其中有 `break`, 翻译为 
```llvm
{where_break1_is}:
...
br label next

{where_break2_is}:
...
br label next

next:
%loopresult = phi <ty> [{break1val}, {where_break1_is}], [{break2val}, where_break2_is] ; 如果 loop 类型不为 void 
...
``` 
load & store 应该更好吧？

`while cond {block}` 可以翻译为 
```llvm 
{last_label}:
...
br label cond

cond:
...
br i1 %cond, label while, label next

while:
...
br label cond

next:
...
```

LLVMIR 允许 reg 存放复杂类型，然而实践中经常把复杂类型放到指针里（即使高级语言没有出现指针），故 irgen 应记录一个 value 是指针还是值，对于简单指令（加减乘除、cast 等）指针应自动取值.

- Field Access 和 Index 如何翻译？如果 receiver 是指针，就 getelementptr，如果 receiver 是值就 extractvalue. 应该从不需要 insertvalue，因为右值不能赋值
- Method Call 如何翻译？取 receiver 的地址，放到 call 里.
- 注意计算解了多少重引用，自动 load

Struct Expression, Array Expression 如何翻译？先 Alloca，再依次 GEP, store

Grouped Expression 直接继承内部的 valueptr

Borrow Expression 对于有地址的，直接取其地址，转化为值，否则 alloca store 给一个地址

Deref Expression 进行 load 作为值

Neg Expression 翻译为 `sub <ty> 0, {val}`

二元运算、比较运算应该容易翻译，`&&` 可以翻译为 
```llvm 
{prev_label}:
...

right:
...
br label next

next:
%andresult = phi i1, [true, {prev_label}], [{rightval}, right]
``` 
类似地可以翻译 `||`.

Cast Expression 应翻译为 `zext` 或 `trunc`

Assign Expression 可以翻译为 store

Compound assignment expression 可以翻译为 
```llvm 
%tmp = <op> <ty> {op1}, {op2}
store <ty> ...
```

