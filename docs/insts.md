# 常见的指令介绍

## GEP

```llvm
 gep [inbounds] [volatile] 
     <pointer_type> <pointer_value>, [integer_type] <index> 
     [, <integer_type> <index> ...]
```

GEP 指令：用于计算地址（无副作用，不会读写数据）

参数：
   pointer_type: 指针类型，是计算地址的基准类型
   pointer_value: 指针值，是计算地址的基地址
   之后的参数：索引，用于计算地址，因为可以是多维数组，所以索引可以有多个
