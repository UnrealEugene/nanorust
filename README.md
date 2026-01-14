# nanorust

**nanorust** is a small Rust-inspired programming language implemented in Rust.  
It includes a custom lexer, parser, Hindley–Milner type inference, an intermediate
representation (IR), and a tree-walk interpreter.

The project focuses on exploring language design, static typing, and compilation
pipelines in a compact and inspectable codebase.

---

## Features

### Implemented
- Rust-like syntax with `fn` functions and `let` bindings
- Mutable and immutable variables using `mut`
- Expression-oriented blocks
- Control flow with `if / else` and `while`
- `return`, `break`, and `continue`
- Types: `i32`, `bool`, and unit
- Hindley–Milner type inference with polymorphic immutable bindings
- Arithmetic, comparison, and boolean operators
- Tree-walk interpreter over a lowered IR
- Detailed, span-based diagnostics with stable error codes

### Parsed but not yet executable
- Closures
- `for` loops
- Ranges
- References and dereferences
- Casts
- Non-empty tuples

---

## Example

```rust
fn f(mut n: i32) -> i32 {
    let mut res = 0;
    while n > 1 {
        println(n);
        if n % 2 == 0 {
            n = n / 2;
        } else {
            n = 3 * n + 1;
        }
        res = res + 1;
    }
    res
}

f(77031)
````

````
77031
231094
115547
...
8
4
2
Number(350)
````

---

## Type Inference Error Examples

### Monomorphic binding with polymorphic function

```rust
fn id<T>(x: T) -> T { x }

let mut g = id;  // mutable bindings are monomorphic
let _ = g(1);
let _ = g(true); // type error
````

```
[E05] Error: mismatched types
   ╭─[test.nrs:5:9]
   │
 5 │ let _ = g(true);
   │         ┬  
   │         ╰── expected `fn(i32) -> i32`, found `fn(bool) -> T24`
───╯
```

If `mut` is omitted from `let` binding, it becomes polymorphic and code compiles:

```rust
fn id<T>(x: T) -> T { x }

let g = id;      // immutable bindings are polymorphic
let _ = g(1);
let _ = g(true); // OK
````

---

### Occurs check failure (self-application)

```rust
fn self_apply<T>(f: T) -> i32 {
    f(f); // type error
    0
}
```

```
[E06] Error: type unification occurs check failed
   ╭─[test.nrs:2:5]
   │
 2 │     f(f); // type error
   │     ┬  
   │     ╰── cannot have a value which is both `T17` and `fn(T17) -> T18`
───╯
```

---

## Project Structure

* `src/lexer.rs` – Tokenization
* `src/parser.rs` – AST construction
* `src/expr.rs` – AST definitions
* `src/ir.rs` – IR construction and semantic checks
* `src/typing.rs` – Type inference
* `src/interpret.rs` – IR interpreter
* `src/error.rs` – Semantic and type errors
* `nanorust-macros/` – Procedural macros
* `data/prelude.nrs` – Language prelude
* `test.nrs` – Example program

---

## Running

```bash
cargo run -- interpret path/to/file.nrs
```

---

## Near future

* Implement closures and variable capture
* Add `for` loops and range iteration
* Implement references and dereferences
* Add cast expressions
* Support non-empty tuples
* Support arrays and slices
* Support heap data allocation
* Make minimal standard library
* Fully support type generics

## Far future

* Translate IR into WebAssembly
* Translate IR into LLVM IR
* Compile IR into x86 32-bit assembly using GAS AT&T syntax
* Implement Rust-like type traits
