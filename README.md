# Forsp

A Forsp interpreter, written in Rust and compiled to WASM.

## What is Forsp?

Forsp is a Forth+Lisp hybrid lambda calculus language with a call-by-push-value
evaluation model. I'll let [Anthony Bonkoski, who came up with it, explain it
himself](https://xorvoid.com/forsp.html).

## Running

### julianferrone.com

You can try out the interpreter at [my personal
website](https://forsp.julianferrone.com/).

It runs 100% client-side -- no data is collected (besides Nginx access logs) and
no login is required.

Fair warning: the site is terrible on mobile, the interpreter is not
particularly fast, and the REPL will let you send multiple inputs even while
it's processing.

You could send a slow command, then immediately send `stack print` to print the
current state, and the REPL won't stop you at all. The commands will eventually
get processed, but it might seem like it's frozen.

### DIY

To build this repo, you'll need wasm-pack installed.

```shell cargo install wasm-pack ```

Build the WASM binaries with:

```shell wasm-pack build --out-dir www/pkg --target no-modules ```

Which will produce compilation output under the **/www/pkg** folder.

You can then run this by running a local HTTP server from the **/www** folder,
such as by using Python's built-in server:

```shell cd ./www python3 -m http.server --bind 127.0.0.1 ```

## Todo

- [ ] Make the CSS look better
  - [x] Make the webpage fill the screen instead of being an 80x24 character
    terminal
  - [ ] Make the design responsive so it looks good on mobile
- [x] More efficient Sexpr representation
  - Right now, I'm using Serde's Serialize/Deserialize because enum variants
    with associated data aren't supported with `#[wasm_bindgen]`
  - It would be nice to have a more efficient Sexpr representation, probably
    using `Vec`s, so that 1. I can use wasm_bindgen 2. I can make the WASM
    binary smaller
  - I've not fixed the "enum variants with associated data" issue, but I've
    update Sexprs from a `Nil, Single(T), Pair(<Box<Sexpr<T>>, Box<Sexpr<T>>)`
    representation to one that uses `Vec`s: `Single(T), 
    List(Vec<Box<Sexpr<T>>>)`
- [ ] Convert reader/scanner/parser to do parsing on demand (using Iterator)
  instead of all at once
  - This will save memory and should simplify the parsing (we can pull one
    instruction at a time--e.g. when we see a special form "^foo", we'll have to
    pull two instructions--one being Instruction::Quote("foo"), the next being
    Instruction::Push)
- [ ] Update scan/read/parse types
  - scan should pull characters and output Tokens
  - read should pull Tokens and output a Sexpr<Atom>
    - read should only do 2 things: 1. expand the syntactic special forms `' $
      ^` 2. collect stuff between parens as a list
  - parse should pull Sexpr<Token>s and output an Instruction
    - parse creates the proper AST

In Haskell syntax, these types would look like:

```haskell 
data Atom = Named String | Num Int 

data Token = 
  TAtom Atom 
  | ParenOpen 
  | ParenClose 
  | Quote 
  | Dollar 
  | Caret 
  | Whitespace 
  | Newline 
  | Semicolon

data Sexpr a = Single a 
  | Many [Sexpr a]

type Env = Map String Instruction

data Value = 
  VAtom Atom 
  | List (Sexpr Value) 
  | Closure [Instruction] Env

data Primitive = Push | Pop | Car | ...

data Instruction = 
  IValue Value            -- Add this value to top of stack 
  | ICall String          -- Call function with name 
  | IPrimitive Primitive  -- Call primitive function on stack 
```

- [ ] Add ability to save and load system states as files
- [ ] Create pipeline to inline all the files into one HTML file
  - I'd like for people to be able to just download one file that they can run
    on their local computers
- [ ] Rewrite as bytecode VM
  - This could be a lot faster than a tree-based interpreter (what this
    currently is)
  - We could write the bytecode VM in parallel, and switch over the WASM module
    to use the VM---the interpreter won't be imported into the module, so it
    won't bloat out the binary size
  - Should write lots of tests to ensure that tree interpreter and VM have same
    output---maybe add property tests somehow? Writing a generator for arbitrary
    values/commands could be an interesting challenge...
- [ ] Stop user from being able to input commands while REPL is computing a new
  state
  - Maybe I should allow users to perform a `Ctrl`+`C` command to stop the REPL
    from executing the current command, and then reset the state to what it was
    beforehand
- [ ] Add line number / column number information to Token so that we can output
  more useful error messages when parsing
