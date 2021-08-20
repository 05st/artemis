# Artemis

Artemis is a statically typed, functional programming language with some inspirations from Rust, ML, and Haskell. It doesn't have too many exciting features as I am writing it for learning purposes.

## Features
At the moment, Artemis has all of these implemented:
- Type inference (Hindley-Milner)
- Algebraic data types
- Pattern matching
- Module system via namespaces/imports
- User-defined prefix/infix operators
- A small standard library
- Implementations for common data types (lists, tuples, etc.)
- Non-significant whitespace
- Automatic currying/partial application
- Immutable variables by default
- Built-in functions/types
- Recursive functions/types

## Planned
It would be great to have these implemented eventually:
- Typeclasses (for ad-hoc polymorphism)
- Exhaustiveness/redundancy checking for pattern matching
- An improved standard library
- Bytecode VM

## Examples
```
data List<a> = Cons(a, List<a>) | Empty;

let fold : (a -> b -> b) -> b -> List<a> -> b
  = fn(f, init, list)
  => match list with
    Cons(a, list') -> f(a, fold(f, init, list')),
    Empty -> init;
    
// 'fold' was automatically curried
let sumInts = fold(addInt, 0);

// outputs 15
print(showInt(sumInts([1, 2, 3, 4, 5])));
```
More can be found [here](https://github.com/05st/artemis/tree/master/examples).

## Contributing
Feel free to open a pull request!

## Credits
[brightly-salty](https://github.com/brightly-salty)
