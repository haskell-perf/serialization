A benchmark that compares the serialization and deserialization performances of the latest versions of the following Haskell serialization libraries:


| Package                                                                      | Laziness    | Compatibility              |
| ---                                                                          | ---         | ---                        |
| [store](https://hackage.haskell.org/package/store)                           | Strict      | Haskell-Same-Architecture  |
| [packman](http://hackage.haskell.org/package/packman)                        | Lazy        | Haskell-Same-Architecture  |
| [cereal](http://hackage.haskell.org/package/cereal)                          | Strict      | Haskell                    |
| [binary](http://hackage.haskell.org/package/binary)                          | Lazy        | Haskell                    |
| [binary-serialise-cbor](https://github.com/well-typed/binary-serialise-cbor) | Lazy        | Multi-Language             |
| [flat](https://github.com/tittoassini/flat)                                  | Lazy        | Multi-Language             |

Compatibility Levels (lowest to highest):
* Haskell-Same-Architecture
  - Compatible across Haskell systems sharing the same CPU and endianness
* Haskell
  - Compatible across Haskell systems
* Multi-Language
  - Compatible across different programming languages

## Disclaimer

I am the author of [flat](https://github.com/tittoassini/flat). 

## Run the Tests

git clone https://github.com/tittoassini/serialization-bench;cd serialization-bench;stack build --exec serialization-bench

## Full Results

[Full Criterion Report](http://htmlpreview.github.io/?https://github.com/tittoassini/serialization-bench/blob/master/report.html).

## Summary Results


```

```
