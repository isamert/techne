# Techne <sup name="a1">[1](#fn1)</sup>
Techne is a functional programming language with some OOP features baked into syntax. It has Hindley-Milner type inference with typeclasses<sup name="a2">[2](#fn2)</sup>.  It is an interpreted language for now but a full machine code compiler is planned.

## Basics
Here is a basic Techne program:

```
main _ = println("Hello world!")
```

The syntax is a mix of ML-style and C-style languages. Here is the definition of map function with parameter level pattern matching:

```
map [], _ = []
[x, xs@...], f = f(x) :: map(xs, f)
```

This function can also be written as:

```
map [], _ = []
[x, xs@...], f = x.f() :: xs.map(f)
```

As you can see functions can be called upon objects even though they don't belong to them. Observe that all of these three implementations does the same thing and the first two are desugared into the last one:

```
[1,2,3].map(λa → a + 1)
([1,2,3], λa → a + 1).map()
map([1,2,3], λa → a + 1)
```

## Infix/prefix/postfix operators
You can easily define infix, prefix or a postfix operator:

```
# The following declaration should be at the top of the file, after the imports (if any)
infixr 4 ^

# This function can be defined anywhere
a ^ b = a.pow(b)
```

Prefix or postfix operators are declared with `postfix` and `prefix` keywords:

```
postfix 5 ??

Just(a)?? = a
Nothing?? = error("Trying to unpack a Nothing value")
```

Prefix version of the example above:

```
prefix 5 ??

??Just(a) = a
??Nothing = error("Trying to unpack a Nothing value")
```

## Call operators
Instead of calling a function using `.`, you can use user-defined call operators to call another function. Observe this:

```
a?.b = match a with
         Just(x) -> b(x),
         Nothing -> Nothing
       end
```

From now on, you can use `?.` operator to call functions with type of `~a -> ~b` on variables with type of `maybe<~a>`. For example:

```
Just(3)?.pow(2)?.plus(5)
# the result of this expression is Just(14)
```

`?.` operator is just a simple `functor` definition for `maybe` type. Call operators makes function chaining easy.<sup name="a3">[3](#fn3)</sup>

## Data types
Data types are pretty simple to define:

```
data maybe a = Just(it: a) | Nothing
```

With this definition, compiler also generates a function named: `maybe->it` which is a simple accessor:

```
maybe->it(Just(3)) # or Just(3).maybe->it() gives 3
```

## Tuples and lists
```
(1, 'd', "hey") # A tuple
[1,2,3,4]       # A list
"techne"        # [char]
```

Tuples plays a very important role in Techne. If you call a function on a tuple, elements of the tuple will be supplied to the function. For example `(1,2).a()` is equivalent to `a(1,2)`. If the `a` function requires a tuple of type `(int, int)`, you have to wrap it in another tuple, like: `((1,2)).a()` which is equivalent to `a((1,2))`.

## Anonymous functions and placeholders
Anonymous functions are used everywhere in Techne and they are pretty easy to create:

```
[1,2,3,4].map(fn x -> x+1)
```

You can also use unicode characters to make it more pretty:

```
[1,2,3,4].map(λx → x*5)
```

Placeholders are kind of shortcuts for creating anonymous functions:

```
[1,2,3,4].map($1.pow(3))
# This line is equivalent to this:
[1,2,3,4].map(λx → x.pow(3))
```

A placeholder starts with $ and only contain numbers after that. Numbers indicate the parameter ordering. Observe this:

```
[1,2,3,4].foldr(5, $2/$1)
# This line is equivalent to this:
[1,2,3,4].foldr(5, λx,y → y/x)
```

The last example shows that you can use placeholders to flip or reorder the parameters of the given function.

## Concepts
Concepts are like Haskell’s typeclasses. A concept is defined like this:

```
concept show of a
    reqs show : a -> string
```

To implement a concept, use an `impl` block:

```
impl show for list
    impls show = ...
```

They are not functional yet.


# Building and using
Techne is in a very experimental state thus it's not distributed in a binary form. If you want to experiment with it, you can build it yourself. Techne is distributed as a `stack` project. To build, you need to install [stack](https://docs.haskellstack.org/en/stable/README/). Then you can simply build the project with:

```
git clone https://github.com/isamert/techne.git
cd techne
stack build
```

Also you can install the binary with `stack install`. You need to add stack's binary path to your PATH environment variable. (On GNU/Linux add `~/.local/bin` to your `$PATH`). Now you can use `techne` binary to execute `.techne` files:

```
techne main.techne
```

To fire up a REPL:

```
techne -i
```

To use without installing, equivalent commands are (in project directory):

```
stack exec techne-exe -- main.techne # Running a techne file
stack exec techne-exe -- -i          # REPL
```

<a name="fn1">1</a>: The name refers to [this](https://plato.stanford.edu/entries/episteme-techne/) [↩](#a1)
<a name="fn2">2</a>: Typeclasses not yet fully implemented. [↩](#a2)
<a name="fn3">3</a>: When type classes are implemented, they will be more powerful. [↩](#a3)
