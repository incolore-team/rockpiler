# Mem2Reg

Mem2Reg（Memory to Register）是 LLVM（Low-Level Virtual Machine）中一个重要的优化传递，其主要目的是将堆分配（memory）中的值提升到栈分配（register），从而提高代码运行速度。它通过将 Alloca 指令替换为 SSA（Static Single Assignment）形式的 PHI 节点和寄存器，以便利用现代处理器的寄存器文件。

下面举一个简单的例子来说明 Mem2Reg pass 的作用：

假设我们有以下 C 语言代码片段：

```c
int foo(int a, int b) {
    int x;
    if (a > b) {
        x = a;
    } else {
        x = b;
    }
    return x;
}
```

未经 Mem2Reg 优化前，生成的 LLVM IR（Intermediate Representation）代码可能如下：

```llvm
define i32 @foo(i32 %a, i32 %b) {
entry:
  %x = alloca i32
  %cmp = icmp sgt i32 %a, %b
  br i1 %cmp, label %if.then, label %if.else

if.then:
  store i32 %a, i32* %x
  br label %if.end

if.else:
  store i32 %b, i32* %x
  br label %if.end

if.end:
  %x.val = load i32, i32* %x
  ret i32 %x.val
}
```

可以看到，变量 x 被分配在内存中（alloca 指令），并在 if.then 和 if.else 基本块中通过 store 指令进行存储，然后在 if.end 基本块中通过 load 指令加载。

应用 Mem2Reg 优化后，生成的 LLVM IR 代码可能如下：

```llvm
define i32 @foo(i32 %a, i32 %b) {
entry:
  %cmp = icmp sgt i32 %a, %b
  br i1 %cmp, label %if.then, label %if.else

if.then:
  br label %if.end, %a

if.else:
  br label %if.end, %b

if.end:
  %x.val = phi i32 [ %a, %if.then ], [ %b, %if.else ]
  ret i32 %x.val
}
```

在这个优化后的版本中，alloca 指令被移除，变量 x 的值直接通过 PHI 节点在寄存器中表示。这样就避免了内存访问的开销，提高了程序运行速度。
