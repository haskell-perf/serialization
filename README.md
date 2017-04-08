Time and size benchmarks for the following Haskell serialization libraries:


| Package                                                                              | Laziness                      | Compatibility             |
| ---                                                                                  | ---                           | ---                       |
| [store-0.4.2](https://hackage.haskell.org/package/store)                             | Strict                        | Haskell-Same-Architecture |
| [packman-0.3.0](http://hackage.haskell.org/package/packman)                          | Lazy                          | Haskell-Same-Architecture |
| [cereal-0.5.4.0](http://hackage.haskell.org/package/cereal)                          | Strict                        | Haskell                   |
| [binary-0.8.5.1](http://hackage.haskell.org/package/binary)                          | Lazy                          | Haskell                   |
| [binary-serialise-cbor-0.1.1.0](https://github.com/well-typed/binary-serialise-cbor) | Lazy                          | Multi-Language            |
| [flat-0.1.5](https://github.com/tittoassini/flat)                                    | Strict  | Multi-Language            |

Compatibility Levels (lowest to highest):
* Haskell-Same-Architecture
  - Compatible across Haskell systems sharing the same CPU and endianness
* Haskell
  - Compatible across Haskell systems
* Multi-Language
  - Compatible across different programming languages

## Tests

To run the benchmarks:

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
| Iris              | A dataset of Iris descriptions (mostly lists and floats)             |

Shout if you would like other tests to be added!

## Full Results

[Full Criterion Report](http://htmlpreview.github.io/?https://github.com/haskell-perf/serialization/blob/master/report.html).

## Summary Results

```
deserialization (mSecs)/[Direction] (best first)
store                   1.0
cereal                  1.1
flat                    2.0
binary_serialise_cbor   5.0
binary                  5.5
packman                 6.8
               
deserialization (time)/BinTree Direction (best first)
store                   1.0
cereal                  1.0
flat                    1.2
binary                  4.8
binary_serialise_cbor   5.9
packman                 7.0
               
deserialization (time)/BinTree Int (best first)
store                   1.0
flat                    1.2
cereal                  1.3
binary                  3.2
binary_serialise_cbor   3.9
packman                 8.3
               
deserialization (time)/Cars dataset (best first)
store                   1.0
cereal                  1.1
flat                    1.3
packman                 2.1
binary                  4.8
binary_serialise_cbor   5.0
               
deserialization (time)/Iris dataset (best first)
store                   1.0
flat                    1.6
packman                 2.4
binary_serialise_cbor   2.7
cereal                  3.0
binary                 10.4
               
deserialization (time)/[Direction] (best first)
store                   1.0
cereal                  1.1
flat                    1.4
binary                  5.4
binary_serialise_cbor   5.5
packman                 6.8
               
serialisation (bytes)/BinTree Direction (best first)
flat                    1.0
binary                  5.5
cereal                  5.5
store                   5.5
binary_serialise_cbor  10.9
packman                87.3
               
serialisation (bytes)/BinTree Int (best first)
flat                    1.0
binary_serialise_cbor   4.2
binary                  8.0
cereal                  8.0
store                   8.0
packman                41.3
               
serialisation (bytes)/Cars dataset (best first)
flat                    1.0
binary_serialise_cbor   5.3
binary                  6.1
cereal                  6.1
store                   6.1
packman                11.3
               
serialisation (bytes)/Iris dataset (best first)
packman                 1.0
flat                    1.0
cereal                  1.0
store                   1.0
binary_serialise_cbor   1.2
binary                  3.1
               
serialisation (bytes)/[Direction] (best first)
flat                    1.0
binary                  4.7
cereal                  4.7
store                   4.7
binary_serialise_cbor   9.4
packman                75.3
               
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
               
serialization (mSecs)/[Direction] (best first)
flat                    1.0
store                   1.4
binary_serialise_cbor   1.7
cereal                  2.4
binary                  3.1
packman                17.9
               
serialization (time)/BinTree Direction (best first)
flat                    1.0
store                   3.1
cereal                  7.1
binary                  7.9
binary_serialise_cbor  13.0
packman                18.6
               
serialization (time)/BinTree Int (best first)
flat                    1.0
store                   4.5
binary                 11.1
cereal                 14.6
binary_serialise_cbor  16.1
packman                31.0
               
serialization (time)/Cars dataset (best first)
store                   1.0
flat                    2.1
binary_serialise_cbor   4.4
cereal                  7.1
binary                  7.4
packman                 9.9
               
serialization (time)/Iris dataset (best first)
store                   1.0
flat                    9.6
binary_serialise_cbor  12.1
cereal                 18.6
packman                31.2
binary                100.6
               
serialization (time)/[Direction] (best first)
flat                    1.0
store                   1.1
binary_serialise_cbor   1.3
cereal                  1.8
binary                  2.1
packman                12.8
```
