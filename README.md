# bracket: A Compiler for a Racket-like Language

`bracket` is a compiler for a small, statically-typed, Racket-like language. It translates source code into LLVM Intermediate Representation (IR), which can then be compiled into a native executable.

This project is an implementation of the **LFun** language, as described in Jeremy Siek's book, "[Essentials of Compilation: An Incremental Approach](https://www.google.com/search?q=essentials+of+compilation+an+incremental+approach)" (EoC). For a more in-depth understanding of the language's design and semantics please refer to the relevant sections of the book and the corresponding code (there may be minor differences). 

> [!WARNING]  
> The implementation is not an exact replica of behavior mentioned in EoC. For instance, there is no short circuiting of logical operators, nor is there garbage collection for tuples. Refer to the examples in tests and the source code to fully understand the behavior.

> [!NOTE]  
> This is part of the course project for the Compilers course at IIITH as part of a team with @GnanaPrakashSG2004. This README was partially generated with an LLM.


## Table of Contents
- [Syntax](#syntax)
- [Language Features](#language-features)
- [Example Program](#example-program)
- [Prerequisites](#prerequisites)
- [Installation and Building](#installation-and-building)
  - [macOS (Apple Silicon & Intel)](#macos-apple-silicon--intel)
  - [Linux (Debian/Ubuntu Example)](#linux-debianubuntu-example)
  - [Building the Compiler](#building-the-compiler)
- [Usage](#usage)
  - [Compiling to LLVM IR](#compiling-to-llvm-ir)
  - [Creating an Executable](#creating-an-executable)
- [Running Tests](#running-tests)
- [Project Structure](#project-structure)


## Syntax
See LFun in EoC Section 7.1

#### Concrete Syntax

    type ::= Integer
    exp  ::= int | (read) | (- exp) | (+ exp exp) | (- exp exp)
    exp  ::= var | (let ([var exp]) exp)

    type ::= Boolean
    bool ::= #t | #f
    cmp  ::= eq? | < | <= | > | >=
    exp  ::= bool | (and exp exp) | (or exp exp) | (not exp) | (cmp exp exp) | (if exp exp exp)

    type ::= Void
    exp  ::= (set! var exp) | (begin exp* exp) | (while exp exp) | (void)

    type ::= (Vector type*)
    exp  ::= (vector exp*) | (vector-length exp) | (vector-ref exp int) | (vector-set! exp int exp)

    type ::= (type... -> type)
    exp  ::= (exp exp...)

    def  ::= (define (var [var:type]...) : type exp)
    LFun ::= def... exp

## Language Features

### General
The language enforces static type checking. All variable bindings, function parameters, and return values must have their types explicitly declared or inferrable at compile time. This helps catch errors at compile time before the program is run. Comments are not supported. A statement within parentheses evaluates to and returns a value (except for: the `set!`, `void`, `define` and `while` expressions which do not return anything, and the `begin` expression, which returns the value of the last corresponding `exp`). The last expression value is printed to `stdout`.

The program
```
(+ 1 2)
```
prints `3`.

### Variables and `let` Expressions
Local variables are introduced using `let` expressions and are visible within the corresponding `let` block. A `let` binding shadows any existing variables with the same name within its scope.
The program

    (let ([x 10])
      (let ([x 20])
        x))
evaluates to `20`.  

The `read` expression can be used to get a single int/bool user input from `stdin`. The program 
```
(let ([x (read)]) (+ x 10))
```
with an input of `32` evaluates to and prints `42`.

### Booleans and Conditionals
Boolean values for true and false are written as `#t` and `#f`, respectively.

*   **Conditional Logic**: The `(if e1 e2 e3)` expression evaluates `e1`. If the result is `#t`, it evaluates `e2`; otherwise, it evaluates `e3`.
*   **Logical Operators**: The language provides `and`, `or`, and `not`, which behave according to standard propositional logic.
*   **Comparison Operators**: Integers can be compared using `eq?`, `<`, `<=`, `>`, and `>=`. The `eq?` operator is also used for checking pointer equality on vectors.

#### Examples:
The program
```
(if (and #t #f) 1 0)
```
prints `0`.   
The program 
```
(let ([x 5])
  (let ([result (if (> x 3) 
                    (let ([x 10]) (+ x 5))
                    (let ([x 2]) (- x 1)))])
    result))
```
prints `15`.  
The programs
```
(if 5 1 0)
```
and
```
(let ([x 5])
  (if x 1 0))
```
throw an error because the condition being evaluated (`e1`) must return a boolean, and both branches (`e2` and `e3` must be of the same type).

### Vectors (Tuples) and Aliasing
The language supports vectors, which are used as fixed-size, heterogeneous tuples.

*   `(vector exp*)`: Creates a new vector containing the evaluated results of the expressions. Does not return a value. 
*   `(vector-ref vec-exp index)`: Retrieves the element at the given integer `index`.
*   `(vector-set! vec-exp index val-exp)`: Modifies the element at `index` to a new value. Does not return a value. 
*   `(vector-length vec-exp)`: Returns the number of elements in the vector.


**Aliasing and Shallow Copies:** When a vector is bound to a variable or passed to a function, a shallow copy is performed. This means multiple variables can point to the exact same vector in memory. The `eq?` operator checks for this aliasing (pointer equality), not for deep structural equality.

In this example, `t1` and `t2` are aliases for the same vector, while `t3` is a different vector with identical content. The program correctly identifies this and returns `42`.

    (let ([t1 (vector 3 7)])
      (let ([t2 t1])
        (let ([t3 (vector 3 7)])
          (if (and (eq? t1 t2) (not (eq? t1 t3)))
            42
            0))))

### Functions
*   **Global Scope**: All `define`d functions exist in a global scope and are visible throughout the entire program. The order of function definitions does not matter.
*   **First-Class Citizens**: Functions can be passed as arguments, returned from other functions, and stored in data structures. The type syntax for a function is `(type1 ... -> typeR)`.
*   **No Lexical Scoping**: A key limitation compared to Racket is that functions are not lexically scoped. The only external variables a function can access are other globally defined functions. Function definitions cannot be nested.

#### Examples:
```
(define (sum [n : Integer]) : Integer
  (if (eq? n 0)
      0
      (+ n (sum (- n 1)))))
(sum 5)
```
prints `15`.
```
(define (neg [b : Boolean]) : Boolean
  (not b))

(define (get-neg [x : Integer]) : (Boolean -> Boolean)
  neg)

(let ([f (get-neg 1)]) (f #f))
```
prints `#t`.
```
(define (add-one [x : Integer]) : Integer
  (+ x 1))
(define (compose [f : (Integer -> Integer)] [g : (Integer -> Integer)] [x : Integer]) : Integer
  (f (g x)))
(let ([y 10])
  (compose add-one add-one y))
```
prints `12`.

## Example Program

The following program defines a `map` function that applies another function `inc` to each element of a vector. This demonstrates the use of first-class functions. The final expression returns `42` which is printed to `stdout`.

    (define (map [f : (Integer -> Integer)] [v : (Vector Integer Integer)])
      : (Vector Integer Integer)
      (vector (f (vector-ref v 0)) (f (vector-ref v 1))))

    (define (inc [x : Integer]) : Integer
      (+ x 1))

    (vector-ref (map inc (vector 0 41)) 1)

## Prerequisites

Before building `bracket`, you need:

*   **LLVM and Clang** (version 19 or newer is recommended)
*   **CMake**
*   **Ninja** build system
*   **Python** (for running the test suite)

> [!WARNING]  
> The main branch requires `rtti` (run time type information for dynamic casting of objects). If you want to compile without `rtti`, switch to the `stage4` branch.


## Installation and Building

The easiest way to install the dependencies on macOS is with [Homebrew](https://brew.sh/).
See the [LLVM Github](https://github.com/llvm/llvm-project) and the [LLVM Getting Started](https://llvm.org/docs/GettingStarted.html) page on installing LLVM. 

Homebrew (and possibly your distro's package manager) installs LLVM and clang in a non-standard path to avoid conflicts with the system's tools. You need to export some environment variables so that CMake can find the correct LLVM installation. The following are the ones I used, ymmv.

```sh
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib/clang/19 -L/opt/homebrew/opt/llvm/lib/c++ -L/opt/homebrew/opt/llvm/lib/unwind -lunwind"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
```

Use the included `make_build.sh` script, replacing these exports or build it yourself by creating a `build` directory and running CMake from the project root.

```sh
mkdir build
mkdir build
cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug \
-DCMAKE_C_COMPILER=/opt/homebrew/opt/llvm/bin/clang \
-DCMAKE_CXX_COMPILER=/opt/homebrew/opt/llvm/bin/clang++ \
-DLLVM_BIN_DIR=/opt/homebrew/opt/llvm/bin \
-S . -B build
```

> [!NOTE]  
> NOTE: On Linux, add `-DLLVM_DIR=<your_llvm_dir>` to the `cmake` command instead of the `-DLLVM_BIN_DIR` line.

### Building the Compiler

Once the project is configured, run the build command from the project root:

```sh
cmake --build ./build
```

The `llracket` binary will be located in `./build/bin/`.

## Usage

### Compiling to LLVM IR

To compile a `bracket` source file (e.g., `program.rkt`) into LLVM IR, run the compiler and specify an output file.

```sh
./build/bin/llracket program.rkt -o program.ll
```

If you omit the `-o` flag, the output will be printed to standard output.

### Creating an Executable

The generated LLVM IR depends on a small C runtime for I/O operations like `read`. To create a standalone executable, you must compile the `.ll` file and link it with `runtime.c`.

1.  **Compile LLVM IR to an object file:**
    ```sh
    llc -filetype=obj program.ll -o program.o
    ```

2.  **Link the object file with the runtime to create an executable:**
    ```sh
    clang program.o ./tools/runtime/runtime.c -o my_program
    ```

3.  **Run your compiled program:**
    ```sh
    ./my_program
    ```

## Running Tests

The project includes a test suite that you can run using `ninja`.

1.  Make sure you have built the project as described above.
2.  Navigate into the `build` directory and run `ninja check`.

    ```sh
    cd build
    ninja check
    ```

You can also use the provided helper script from the project root, which rebuilds and runs the tests:
```sh
bash ./run_tests.sh
```

## Project Structure

The codebase is organized as:

*   `include/`: header files for the compiler libraries
*   `lib/`: The source code for the compiler's phases (Lexer, Parser, Sema, CodeGen) and some common utils (Basic).
*   `tools/`:
    *   `driver/`: The source for the `llracket` command-line executable.
    *   `runtime/`: The C runtime library required by compiled programs.
*   `tests/`: Test files and scripts for verifying the compiler's correctness. To add a new test, add a `<name>.rkt` file with the program, with a `<name>.rkt.in` for stdin input (optional), `<name>.rkt.out` for expected output or `<name>.rkt.err` if an error is expected.
*   `make_build.sh`: Creates a build directory and builds the compiler executable.
*   `run_tests.sh`: Runs `make_build.sh` and runs all tests.