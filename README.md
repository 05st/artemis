# Artemis

Artemis is a statically typed, functional programming language with some inspirations from Rust, ML, Haskell. It doesn't have too many exciting features as I am writing it for learning purposes.

## Features
At the moment, Artemis has all of these implemented:
- Type inference (Hindley-Milner)
- Algebraic data types
- Pattern patching
- Built-in functions/types
- Recursive functions/types
- Implementations for common data types (`List<a>`, `Maybe<a>`, `Either<a, b>`)
- Non-significant whitespace
- Automatic currying/partial application

## Planned
It would be great to have these implemented eventually:
- Typeclasses (for ad-hoc polymorphism)
- Exhaustiveness/redundancy checking for pattern matching
- A small prelude library
- Module system
- Bytecode VM

## Examples
```
data List<a> = Elem(a, List<a>) | Empty;

let fold : (a -> b -> b) -> b -> List<a> -> b
  = fn(f, init, list)
  => match list with
    Elem(a, list') -> f(a, fold(f, init, list')),
    Empty -> init;
    
// 'fold' was automatically curried
let sumInts = fold(addInt, 0);

// outputs 15
print(showInt(sumInts([1, 2, 3, 4, 5])));
```
More can be found [here](https://github.com/05st/artemis/tree/master/examples).

## Contributing
Feel free to open a pull request!
