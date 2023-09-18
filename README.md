
# A Compiler for a Turtle Graphics Language

This is a tutorial on implementing a very simple compiler for a very simple programming language. The goal is not to understand the principles of interpreting or compiling, but to get some concrete experience of:

  1. Working with abstract syntax trees and their relationship with concrete syntax, which we discuss during the first part of the unit.
  2. Representing a snapshot of a computation using state, which is an significant idea in the second part of the unit, when we look at the operational semantics of While programs.
  3. Writing programs that produce other programs (especially according to some fixed template), which is an important feature of the third part of the unit on Computability.

You should aim to complete this tutorial during Weeks 2, 3 and 4.  You can do it all together or spread it over several weeks as you prefer.  There is actually very little code to write, but I guess your Haskell is rusty...

## 0: Haskell lists and other datatypes

Before we start proper, recall some useful Haskell functions for working with lists:

Lists are constructed from the empty list `[]`, and by adding a new element `x` onto the front of an existing list `xs` using the constructor cons, written `:`, to give the resulting list `x:xs`.  
  
For example, the first 3 consecutive natural numbers are written as a list by `0:1:2:[]`.  For this to make sense, we have to know that cons is *right-associative*, so this really means `0:(1:(2:[]))`.  In other words, we started from the empty list `[]` and then added `2` onto the front, then we took this list we made and added `1` onto the front, then we took that list and added `0` onto the front.

Lists are an example of an *algebraic data type*.  Almost all types in Haskell are algebraic data types.  An important thing to understand about algebraic data types is that the values of the type are essentially abstract syntax trees.  Cons `(:)`, or any other data type constructor, should **not** be thought of as a command that gets executed and returns a longer list!  The cons is an integral part of the list itself, because in Haskell `0:1:2:[]` is never more than an abstract syntax tree:
    
```text
     :
    / \
  0    :
      / \
    1    :
        / \
      2    []
```
This is an important difference between arithmetic operators and data type constructors in Haskell.  When we write:
```haskell 
  (2 + 3) * 6 
```
This expression *gets evaluated* (i.e. some steps of execution take place), resulting in the number `30`.  However, when we write:
```haskell
  2 : 3 : 6 : []
```
No execution takes place (conceptually), `2:3:6:[]` is already *fully evaluated* -- this expression/syntax tree *is* itself the list.

Haskell as some additional syntax for expressing lists in a more natural style, instead of writing `2:3:6:[]`, we can write `[2,3,6]`.  However, `[2,3,6]` is just a convenience, the real underlying list is `2:3:6:[]`.

Since the expression/syntax tree `2:3:6:[]` *is* the list, when we have a function that is given a list `ys` as input, we think of it as being given a syntax tree.  The function can decide what to do by taking a look at the structure of this syntax tree.  In Haskell, this is expressed as a case analysis:
```haskell
  case ys of 
    []   ->  -- what to do when the tree ys is just a leaf []
    x:xs ->  -- what to do when the tree ys is rooted at cons (:) 
              --   since cons (:) has arity 2 the root has two 
              --   children, we'll call the left child x and we'll 
              --   call the right child xs.
```

In case you were wondering, the following two definitions of `length`, which counts the number of elements in a list, are equivalent in Haskell:
    
```haskell
  length [] = 0
  length (x:xs) = 1 + length xs
```

```haskell
  length ys = 
    case ys of 
      []   -> 0
      x:xs -> 1 + length xs
```
but the `case ... of` expression is the more fundamental.  The Haskell compiler actually converts the first into the second during the first part of compilation.

We would normally think of abstract syntax trees as being associated with a particular language, e.g. ASTs for the C programming language, or ASTs for arithmetic expressions or ASTs for our turtle language.  What language are Haskell lists the ASTs for?  In Haskell, the data type definition describes the language.  The Haskell datatype `[Integer]` of all lists of integers can be defined in Haskell as:

```haskell
  data [Integer] = [] | Integer : [Integer]
```

Now take a look at the following grammar defining the language of integer list expressions, which has one non-terminal L and one terminal `:` of arity 2 and one terminal `[]` of arity 0:
```text
  L ::= [] | n : L

    where n stands for any integer
```

By comparing these two, I hope you can see that Haskell datatype definitions are just a kind of tree grammar, with Haskell types (like `[Integer]`) acting as the non-terminals.

In the tree grammar above, the non-terminal L generates all the trees:
```text
  [], 1:[], 2:[], ..., 1:(1:[]), 1:(2:[]), ..., 1:(2:(3:[])), ...
```
and these are also exactly the values that inhabit the Haskell type `[Integer]`.

### Tasks

  1. Get the code from this repository by clicking on the green "<> Code" button above, and choosing whichever method you are most comfortable with.  If you are using a lab machine, then this simplest way is using git in a terminal:
      ```text
        $ git clone ...
      ```
      Then navigate to inside this directory and run `ghci Compiler.hs`.
      ```text
        $ cd turtle
        $ ghci Compiler.hs
        Loaded package environment from /Users/sr17466/.ghc/aarch64-darwin-9.2.7/environments/default
        GHCi, version 9.2.7: https://www.haskell.org/ghc/  :? for help
        [1 of 3] Compiling Parser           ( Parser.hs, interpreted )
        [2 of 3] Compiling Interpreter      ( Interpreter.hs, interpreted )
        [3 of 3] Compiling Compiler         ( Compiler.hs, interpreted )
        Ok, three modules loaded.
      ```

## 1: Parsing a Turtle program

In the file `Parser.hs` you can find the definition of the abstract syntax trees for the Turtle language:
```haskell
-- Abstract syntax tree for Turtle programs
data Program = Cmd Command | Seq Command Program
  deriving (Eq, Show)

-- Abstract syntax tree nodes corresponding to atomic commands
data Command = Fd Int | Lt Int | Rt Int | Up | Dn
  deriving (Eq, Show)
```
You may recall that `deriving (Eq, Show)` is an instruction to the compiler to automatically generate equality testing functions `(==)` and functions `show` for converting these syntax trees into strings (along with appropriate typeclass instances).

Let's look more closely at the definition of `Program`.  The first alternative in the definition is: 
```haskell
  Cmd Command
```
This says that `Cmd` is a new arity 1 constructor, whose single argument is required to be a tree of type `Command`.  The second alternative in the definition is:
```haskell
  Seq Command Program
```
which introduces `Seq` as a new constructor of arity 2 and asserts that the two arguments of `Seq` are required to be trees of type `Command` and `Program` respectively.

So ASTs for turtle programs are trees such as:
```haskell
  Seq (Fd 20) (Seq (Lt 10) (Cmd Up)) 
```
which, when written in two dimensions, looks like:
```text
    Seq
    / \
  Fd   Seq
  |    / \
  20  Lt  Cmd
      |    |
      10   Up
```

Parsing is the process by which we tranform a piece of concrete syntax (i.e. a value of type `String`) into an abstract syntax tree (i.e. a value of type `Program`).  Let me remind you of the concrete syntax for Turtle programs.  There are two non-terminals `P` (for programs) and `C` (for commands):
```text
  P ::= C | C ; P
  C ::= up | down | fd(n) | left(n) | right(n)
      where n stands for any natural number
```

Once the parser is completed, we should have:
```haskell
  parse "fd(20); left(10); up" = Seq (Fd 20) (Seq (Lt 10) (Cmd Up))
```
in other words, applying the `parse` function to a valid program in the concrete syntax produces the corresponding abstract syntax tree.

Recall that, in Haskell, strings are just lists of characters.  So the following are all identical:
```haskell
  'a':'b':'c':[] = [a,b,c] = "abc"
  'h':'e':'l':'l':'o':[] = [h,e,l,l,o] = "hello"
```
This means that eveything about lists in the background section above also applies to strings.  In particular, strings are just abstract syntax trees too, and we can pattern-match on them using case expressions:
```haskell
  startsWithE :: String -> Bool
  startsWithE s =
    case s of
      'E':xs -> True
      _      -> False
```
Recall that the pattern `_` will match any tree not already matched by one of the cases that precede it.

It also means that we can use list functions, like the concatenation operator `++` to work with strings:
```haskell
  [h,e,l,l,o] ++ [w,o,r,l,d] = "hello" ++ "world" = "helloworld"
```

In [Parser.hs](Parser.hs) you will find a partially completed parser.  The most important functions are:
  
  * `parseCmd` which is responsible for parsing a single command, e.g. "fd(20)".  It takes as input a string, which you can think of as the whole of the program that has not yet been parsed at the time this function was called.  It then eats the first part of the string corresponding to one of the commands "up", "down", "left(..)", "right(..)" and "fd(..)" and it returns as its output a pair consisting of a syntax tree node corresponding to the command that was parsed and the rest of the string that comes after.  Only the code for parsing "up" has been implemented so far.

  * `parseProg` which uses `parseCmd` internally, and is responsible for parsing a complete Turtle program.  This function takes as input a string, which you can think of as the whole of the program that has not yet been parsed.  It is not implemented yet.

  * `parse` which wraps around `parseProg` and is responsible for an initial pass over the string which removes all whitespace to make parsing easier.  Consequently, we can assume that the inputs to `parseProg` and `parseCmd` do not contain any whitespace.

### Tasks

  1. Complete the `parseCmd` function, by writing the remaining cases for commands "down", "left(..)", "right(..)" and "fd(..)".  You will find it useful to use the function `parseArg` which is provided and documented in the module.  
  
      Check that your implementation behaves correctly by executing the following in GHCI:
      ```haskell
        ghci> parseCmd "down;fd(20);right(60)"
        (Dn,";fd(20);right(60)")
        
        ghci> parseCmd "fd(23);right(10);up"
        (Fd 23,";right(10);up")
        
        ghci> parseCmd "left(100);right(10);up"
        (Lt 100,";right(10);up")
        
        ghci> parseCmd "right(90)"
        (Rt 90,"")
      ```

  2. Implement the `parseProg` function by replacing `undefined` by some working code.  When this function is given a string `s`, it has to transform it into the corresponding abstract syntax tree of type `Program`.  If `s` is a valid Turtle program then, according to the concrete syntax grammar, it is either a single command, or a command followed by a semicolon followed by another program.  In either case, the first thing to do is to parse a command, after this first command is taken care of, we can look to see if there is a semicolon or the end of the input.  
  
      Hence, this function should implement the following algorithm:
      
      Given string `s` as input:
            
        1. Parse a command from the front of `s` using parseCmd, yielding an AST node `c` and the rest of the string `t`.
        2. Look at the shape of `t`:
            - If `t` is empty, then this program is a single command, so return the AST `Cmd c`
            - Otherwise the first character of `t` should be `;` (remember we are ignoring whitespace).  Recursively parse a program `p` from the remainder of the string after the semicolon.  Then return the AST `Seq c p`.
            - In any other case, throw an exception using the Haskell function `error`.
  
      Check that your implementation works correctly by executing the following:
      ```haskell
        ghci> parse "fd(100)"
        Cmd (Fd 100)

        ghci> parse "down; fd(100); left(90)"
        Seq Dn (Seq (Fd 100) (Cmd (Lt 90)))

        ghci> parse "right(20); fd(100); right(100); up"
        Seq (Rt 20) (Seq (Fd 100) (Seq (Rt 100) (Cmd Up)))
      ```

## 2: Interpreting a Turtle program

In this part we are going to write a simple interpreter for Turtle programs.  Some useful datatypes and helper functions have already been supplied in [Interpreter.hs](Interpreter.hs).  The interpreter will execute a program by walking down the syntax tree, but in this part, we won't actually draw any lines, we will just track the turtle as it moves across the page.

If you imagine executing a Turtle program, then at any point in time we can describe the state of the Turtle by specifying the following data:
  * Its current position on the page as an `(x,y)` coordinate.
  * Its current orientation (which way it is facing) in radians.
  * A flag to say whether the pen is currently up or down.

This gives rise to the Haskell datatype `TurtleState`:
```haskell
  data TurtleState =
    State {
      pen :: PenState,
      pos :: (Int, Int),
      ang :: Float
    } deriving (Eq, Show)
```
This is a Haskell record type with three fields.  Given a value of this type, say `s` you can get the values of the fields as `pen s`, `pos s` and `ang s`.  The first describes the state of the turtle's pen, the second its position as a coordinate and the third it orientation in radians.

We can build a new turtle state by using some record syntax:
```haskell
  State { pen = PenUp, pos = (0,500), ang = 0.0 }
```
This turtle state actually describes the initial state of the turtle - its position, angle and pen status before any commands get executed.  This is hardcoded into the program as `initialTurtleState`.

In the following part we will be compiling Turtle programs down to Javascript, and we will use an HTML Canvas object to render the lines.  You can read about it [here](https://www.w3schools.com/graphics/canvas_coordinates.asp).   The canvas has its origin in the top left, but turtle programs start the turtle in the bottom left.  Since our canvas will be 500 * 500 units, the turtle is initially positioned at `(0,500)` facing due east.

Haskell provides some useful syntax for creating new records that are mostly the same as existing ones.  For example, to create a new turtle state that has the same orientation and pen status as `initialTurtleState`, but with position `(100,500)` one can write:
```haskell
  initialTurtleState { pos = (100,500) }
```

There are three main functions for interpreting Turtle programs.  
  * `interpretCmd` is responsible for interpreting a single command in the AST, e.g. `Dn` or `Fd 100`.  The idea is that we want to compute the new position, orientation and pen status as a result of executing this single command.  To work out the new position or the new angle, we will need to know the current position and current angle.  So this function takes as input both the command (AST node) to be executed and the current state of the turtle at this instant in time, and it returns the state of the turtle after executing the command.

  * `interpretProg` is responsible for interpreting a Turtle program AST (which may be the whole program, or a subtree of it).  It will use `interpretCmd` internally.  It takes as input the program AST that is to be executed and the current state of the turtle and it returns the state the turtle reaches at the end of execution.

  * `interpret` is a wrapper around `interpretProg` which sets the current state of the turtle to `initialTurtleState`.

### Tasks

  1. Finish implementing the function `interpretCmd` by writing one clause for each of the remaining cases.  To compute the new position of the turtle as a result of moving forward some number of units, it is suggested to use the provided function `computeNewPosition`.

      Check that your implementation works by evaluating the following:
      ```haskell 
        ghci> interpretCmd Dn (State {pen=PenUp, pos=(0,500), ang=pi/2})
        State {pen = PenDown, pos = (0,500), ang = 1.5707964}

        ghci> interpretCmd (Fd 10) (State {pen=PenUp, pos=(0,500), ang=0})
        State {pen = PenUp, pos = (10,500), ang = 0.0}
        
        ghci> interpretCmd (Lt 90) (State {pen=PenUp, pos=(0,500), ang=0})
        State {pen = PenUp, pos = (0,500), ang = 1.5707964}
        
        ghci> interpretCmd (Fd 50) (State {pen=PenUp, pos=(0,500), ang=pi/2})
        State {pen = PenUp, pos = (0,450), ang = 1.5707964}
      ```

  2. Implement the function `interpretProg`.  Recall that programs (ASTs) are either just a single command `Cmd c` or a sequential composition `Seq c q` of a command `c` followed by a program `q`.  Therefore, this function should implement the following algorithm:
      
      Given a program AST `p` and current state `s`:
          
      - Check the form of `p`:
          - If `p` is a single command, return the state reached after interpreting it in state `s`
          - If `p` is a sequence of `c` followed by `q`, then:
              - Interpret `c` in state `s` to yield new state `t`
              - Recursively interpret `q` in state `t` to yield a final state `u`
              - Return `u`

      Check that your implementation works by interpreting the following programs from state `s = State {pen=PenUp, pos=(0,500), ang=0}`:

      ```haskell
        ghci> s = State {pen=PenUp, pos=(0,500), ang=0}
        
        ghci> interpretProg (parse "fd(10)") s
        State {pen = PenUp, pos = (10,500), ang = 0.0}

        ghci> interpretProg (parse "fd(10); fd(20)") s
        State {pen = PenUp, pos = (30,500), ang = 0.0}

        ghci> interpretProg (parse "fd(10); left(90); down; fd(50)") s
        State {pen = PenDown, pos = (10,450), ang = 1.5707964}
      ```

## 3: Compiling a Turtle program to Javascript

In this third part, we will implement a simple compiler to Javascript.  The starting code is in [Compiler.hs](Compiler.hs)  The idea is that compiling a Turtle program outputs a Javascript program which, if executed by your browser, will draw the picture described by the turtle program (actually the compiler produces an HTML file, but this is mostly just a wrapper around the Javascript).  

Rather than emitting Javascript to handle keeping track of the state of the turtle, we will reuse our interpreter from the previous part and only emit Javascript to actually draw the lines.  Consequently, we will only need to use two Javascript methods of the `ctx` object:
  * `ctx.moveTo(x,y)` sets the current position on the canvas to `(x,y)`.
  * `ctx.lineTo(x,y)` trace a straight line from the current position on the canvas to the position `(x,y)`.

The idea is that we will use `moveTo` when the turtle's pen is up, and we will use `lineTo` when the turtle's pen is down.

There are four main functions that comprise the compiler:
  * `compileCmd` is responsible for interpreting a single Turtle command and, as a side effect, producing the Javascript instructions required to draw any line.

  * `compileProg`, it is completely analogous to `interpretProg` except it has to handle the fact that `compileCmd` has a side-effect.  Consequently, it uses a `do` block to sequence the effects.

  * `compile`, is a wrapper around `compileProg` and `parse`, first parsing the input string as a program AST and then compiling the AST into Javascript.

  * `compileFile` is a wrapper around `compile` that takes a filePath containing a Turtle program as input.

### Tasks

  1. Finish the implementation `compileCmd`.  This function calls `interpretCmd` internally in order to calculate the new position etc of the turtle after the command is executed.  As a side effect, it appends a Javascript canvas command to the output file.    
      - When interpreting a `Fd n` command, `canvasCmd` should be either: 
          - `ctx.moveTo(x,y);` for some appropriate `x` and `y`, if the turtle's pen was up
          - `ctx.lineTo(x,y);` for some appropriate `x` and `y`, if the turtle's pen was down
      - In all other cases, the command can just be the empty string "".

      You may want to prepend some spaces onto the front and append a newline character onto the end of the `canvasCmd` in the first two cases, in order for the output file to look neat, but this will become more clear after completing the next task.

      You can check that your implementation works correctly by executing the following, and then viewing `output.html` in your browser.

      ```haskell
        ghci> compile "down; fd(100); left(90); fd(100); left(90); fd(100); left(90); fd(100)"
      ```

      Viewing `output.html` in your browser, you should see a small square.

      ```haskell
        ghci> compileFile "sq-in-sq.tg"
      ```

      Viewing (refreshing) `output.html` in your browser you should see four squares of different sizes, 


