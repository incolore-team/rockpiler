#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <input.c>"
  exit 1
fi

INPUT_C="$1"
INPUT_LL="${INPUT_C%.c}.ll"
OUTPUT_LL="${INPUT_C%.c}_mem2reg.ll"

# Compile C code to LLVM IR
clang -S -emit-llvm -O0 -Xclang -disable-O0-optnone -o "$INPUT_LL" "$INPUT_C"

# Convert LLVM IR using mem2reg pass
opt -mem2reg -S -o "$OUTPUT_LL" "$INPUT_LL"

# Compare the two LLVM IR files
diff "$INPUT_LL" "$OUTPUT_LL"