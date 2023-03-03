<h1 align="center">Polygon Project</h1>
<h2 align="center">Low Level Polygon</h2>
<p align="center"><b>“多边”——面向未来的多语言编程语言。 </b></p>
<p align="center">Polygon, a multilingual programming language. </p>

<p align="center">
<img style="width: 200px" src="https://raw.githubusercontent.com/pluveto/0images/master/2023/01/upgit_20230124_1674557731.svg" />
</p>

Low Level Polygon (llpolygon, or llp) is a subset of Polygon.

## Basic grammar

### Variable and constants

variables:

```llp
let year = 2021
let next_year = year + 1
```

constants:

```llp
let PI const = 3.14159
````

### Data type

#### Integer

| Type | Description | Range |
| --- | --- | --- |
| i8 | 8-bit signed integer | -128 ~ 127 |
| i16 | 16-bit signed integer | -32768 ~ 32767 |
| i32 | 32-bit signed integer | -2147483648 ~ 2147483647 |
| i64 | 64-bit signed integer | -9223372036854775808 ~ 9223372036854775807 |
| u8 | 8-bit unsigned integer | 0 ~ 255 |
| u16 | 16-bit unsigned integer | 0 ~ 65535 |
| u32 | 32-bit unsigned integer | 0 ~ 4294967295 |
| u64 | 64-bit unsigned integer | 0 ~ 18446744073709551615 |

```llp
let year: i16 = 2021
let next_year: i16 = year + 1
```

Literal integer by default is i32.

#### Float

| Type | Description | Range |
| --- | --- | --- |
| f32 | 32-bit floating point number | 1.2E-38 ~ 3.4E+38 |
| f64 | 64-bit floating point number | 2.3E-308 ~ 1.7E+308 |

```llp
let PI: const f32 = 3.14159
let E: const f64 = 2.718281828459045
```

Literal float by default is f64.

#### Boolean

```llp
let is_true: bool = true
let is_false: bool = false
```

Boolean is a alias of u8.

### Byte, Offset, Addr, Buffer

+ `byte` is a alias of u8.
+ `offset` is a alias of u64.
+ `addr` is a alias of u64.
+ `buf` is a alias of Addr.

#### Pointer

```llp
let ptr: addr = addr of year
```

Pointer is a alias of u64. Use `addr of` to get the address of a variable.

#### Static Array

```llp
let arr : addr = [1, 2, 3]
let byte_arr: [3 times u8] = [0x01, 0x02, 0x03]
```

Static array is stored in global data section. The variable `arr` is a pointer to the array.

#### Bytes

```llp
let greeting: bytes = "Hello, world!"
```

Bytes or plain string is a alias of a addr that points to static array of u8.

`string_bytes` is a alias of bytes;

#### Struct

By default, struct is compact.

```llp
define Person struct {
    name: bytes
    age: i32
}
```

Equivalent to:

```llp
define Person struct {
    name    :   64bit
    age     :   32bit
}
```

or

```llp
define Person struct {
    name    :   0
    age     :   8
}
```

or

```llp
let Person.name = 0;
let Person.age = 8;
```

Usage:

```llp
let person = Person {
    name: "John",
    age:  18,
}

write_str(stdout, person ! Person.name)

// or

write_str(stdout, ! (person + Person.name))
```

#### Enum

```llp
define Color enum {
    Red = 1
    Green
    Blue
}
```

Equivalent to:

```llp
let Color.Red = 1 as offset
let Color.Green = 2 as offset
let Color.Blue = 3 as offset
```

#### Union

```llp
define Number union {
    int: i32
    float: f32
}
```

Equivalent to:

```llp
let Number.int = 0
let Number.float = 0
```

#### Simple Function

Define

```llp
let pi const = 3.14159

define sqrt function (x: f32) -> f32 {
    return x * x
}

define circle_area function (r: f32) -> f32 {
    return pi * sqrt(r)
}
```

Call

```llp
let area = circle_area(2.0)
```

#### N-ary Function

```llp
define add function (a: i32, b: i32) -> i32 {
    return a + b
}
```

Call

```llp
let sum = 2 %add 3
```

#### Lambda

```llp
let sqrt = function (x: f32) -> f32 {
    return x * x
}
```

Capture

```llp
let pi const = 3.14159
let circle_area = function (r: f32) with context (pi) -> f32 {
    return pi * sqrt(r)
}

let area = circle_area(2.0) with context (pi)
```

#### Bind function

```llp
define Person struct {
    name: bytes
    age: i32
}

on Person {
    define say_hello function (self: Person) -> bytes {
        return "Hello, I am " + self.name
    }

    define greet function (self: Person, other: Person) -> bytes {
        return (self %say_hello) + ", nice to meet you, " + (other ! Person.name)
    }
}
```

Usage:

```llp
let person = Person {
    name: "John",
    age:  18,
}

person %Person.say_hello () // or Person.say_hello(person)

person %Person.greet (Person {
    name: "Jane",
    age:  18,
})
```

### Control flow

#### if

```llp
if (condition) {
    // do something
} elseif (condition) {
    // do something
} else {
    // do something
}
```

#### for

```llp
let sum = 0
for i: i32 from 0 to 100 inclusive {
    sum = sum + i
}
```

#### while

```llp
let arr = [1, 2, 3]
let len = len_of(arr)
let i = 0
while (i < len) {
    write_str(stdout, arr.get(i))
    if(i != len - 1) {
        write_str(stdout, ", ")
    } else {
        write_str(stdout, "\n")
    }
    i = i + 1
}
```

### Generic

Not supported.

### Module

#### Create module

Given that a definition of `Matrix` is in `struct_defs/math/matrix.pg`:

```llp

define Matrix struct {
    col: i32
    row: i32
    data: DyArr<DyArr<f32>>
}
```

Create a module `matrix_op` in `util/math/matrix_op.pg`:

```llp
import struct_defs::math::matrix

on Matrix {
    define add function (self: Matrix, other: Matrix) -> Matrix {
        let result = Matrix::new(self.col, other.row)
        for i: i32 from 0 to self.col (exclude) {
            for j: i32 from 0 to other.row (exclude) {
                let added = self.data.get(i).get(j) + other.data.get(i).get(j)
                result.data.get(i).set(j, added)
            }
        }
        return result
    }
}
```
