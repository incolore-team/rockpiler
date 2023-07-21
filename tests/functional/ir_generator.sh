#!/usr/bin/env bash

# 使用Clang生成LLVM IR作为参考
clang -x c $1.sy -O0 -Xclang -disable-O0-optnone -fno-discard-value-names -S -emit-llvm -o $1.ll