A benchmark that compares the serialization and deserialization performances of the latest versions of the following Haskell serialization libraries:


| Package                                                                      | Laziness    | Compatibility              |
| ---                                                                          | ---         | ---                        |
| [store](https://hackage.haskell.org/package/store)                           | Strict      | Haskell-Same-Architecture  |
| [packman](http://hackage.haskell.org/package/packman)                        | Lazy        | Haskell-Same-Architecture  |
| [cereal](http://hackage.haskell.org/package/cereal)                          | Strict      | Haskell                    |
| [binary](http://hackage.haskell.org/package/binary)                          | Lazy        | Haskell                    |
| [binary-serialise-cbor](https://github.com/well-typed/binary-serialise-cbor) | Lazy        | Multi-Language             |
| [flat](https://github.com/tittoassini/flat)                                  | Strict Encoder - Lazy Decoder | Multi-Language             |

Compatibility Levels (lowest to highest):
* Haskell-Same-Architecture
  - Compatible across Haskell systems sharing the same CPU and endianness
* Haskell
  - Compatible across Haskell systems
* Multi-Language
  - Compatible across different programming languages

## Tests

To run the tests:

`stack bench`

If you get this error:

`...<stdout>: commitBuffer: invalid argument (invalid character)`

Try:

`export LC_ALL=C.UTF-8`

## Test Data

| Test              | Description                                                          |
| ---               | ---                                                                  |
| BinTree Direction | Binary Tree of a simple enumeration data type                        |
| BinTree Int       | Binary Tree of Ints                                                  |
| [Direction]       | A List of a simple enumeration data type                             |
| Cars              | A dataset of Cars descriptions (mostly lists, enumerations and Ints) |
| Iris              | A data set of Iris descriptions (lists and floats)                   |
|                   |                                                                      |

## Full Results

[Full Criterion Report](http://htmlpreview.github.io/?https://github.com/tittoassini/serialization-bench/blob/master/report.html).

## Summary Results

```
deserialization (time)/BinTree Direction (best first)
store                   1.0
cereal                  1.0
flat                    1.7
binary                  4.8
binary_serialise_cbor   5.1
packman                 7.1

deserialization (time)/BinTree Int (best first)
store                   1.0
cereal                  1.2
flat                    1.8
binary_serialise_cbor   3.1
binary                  3.2
packman                 8.4

deserialization (time)/Cars dataset (best first)
store                   1.0
cereal                  1.1
packman                 2.1
flat                    3.3
binary_serialise_cbor   4.2
binary                  4.8

deserialization (time)/Iris dataset (best first)
store                   1.0
flat                    1.9
binary_serialise_cbor   2.2
packman                 2.3
cereal                  2.9
binary                 10.5

deserialization (time)/[Direction] (best first)
store                   1.0
cereal                  1.1
flat                    2.0
binary_serialise_cbor   4.9
binary                  5.5
packman                 6.8

serialisation (size)/BinTree Direction (best first)
flat                    1.0
binary                  5.5
cereal                  5.5
store                   5.5
binary_serialise_cbor  10.9
packman                87.3

serialisation (size)/BinTree Int (best first)
flat                    1.0
binary_serialise_cbor   4.2
binary                  8.0
cereal                  8.0
store                   8.0
packman                41.3

serialisation (size)/Cars dataset (best first)
flat                    1.0
binary_serialise_cbor   5.3
binary                  6.1
cereal                  6.1
store                   6.1
packman                11.3

serialisation (size)/Iris dataset (best first)
flat                    1.0
cereal                  1.0
store                   1.0
packman                 1.0
binary_serialise_cbor   1.2
binary                  3.1

serialisation (size)/[Direction] (best first)
flat                    1.0
binary                  4.7
cereal                  4.7
store                   4.7
binary_serialise_cbor   9.4
packman                75.3

serialization (time)/BinTree Direction (best first)
flat                    1.0
store                   3.3
cereal                  7.6
binary                  8.6
binary_serialise_cbor  14.2
packman                20.1

serialization (time)/BinTree Int (best first)
flat                    1.0
store                   4.5
binary                 11.0
cereal                 14.8
binary_serialise_cbor  16.8
packman                32.1

serialization (time)/Cars dataset (best first)
store                   1.0
flat                    2.1
binary_serialise_cbor   5.1
cereal                  7.0
binary                  7.6
packman                10.1

serialization (time)/Iris dataset (best first)
store                   1.0
flat                    7.7
binary_serialise_cbor  12.6
cereal                 20.3
packman                29.1
binary                 94.8

serialization (time)/[Direction] (best first)
flat                    1.0
store                   1.5
binary_serialise_cbor   1.6
cereal                  2.4
binary                  3.0
packman                17.6
```
