Time and size benchmarks for the following Haskell serialization libraries:


| Package                                                                    | Laziness | Compatibility             |
| ---                                                                        | ---      | ---                       |
| [store-0.4.3.1](https://hackage.haskell.org/package/store)                 | Strict   | Haskell-Same-Architecture |
| [packman-0.3.0](http://hackage.haskell.org/package/packman)                | Lazy     | Haskell-Same-Architecture |
| [cereal-0.5.4.0](http://hackage.haskell.org/package/cereal)                | Strict   | Haskell                   |
| [binary-0.8.5.1](http://hackage.haskell.org/package/binary)                | Lazy     | Haskell                   |
| [serialise-0.1.0.0](https://hackage.haskell.org/package/serialise-0.1.0.0) | Lazy     | Multi-Language            |
| [flat-0.3](https://github.com/tittoassini/flat)                            | Strict   | Multi-Language            |

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

[Full Criterion Report](https://rawgit.com/haskell-perf/serialization/master/report.html), also in [json format](https://raw.githubusercontent.com/haskell-perf/serialization/master/report.json).

## Summary Results

```
deserialization (time)/BinTree Direction (best first)
store       1.0
cereal      1.0
flat        1.2
binary      4.8
serialise   5.6
packman     6.9

deserialization (time)/BinTree Int (best first)
store       1.0
flat        1.2
cereal      1.2
binary      3.2
serialise   3.8
packman     8.3

deserialization (time)/Cars dataset (best first)
store       1.0
cereal      1.1
flat        1.3
packman     2.1
binary      4.9
serialise   5.1

deserialization (time)/Iris dataset (best first)
store       1.0
flat        1.8
packman     2.3
serialise   2.6
cereal      3.0
binary     10.1

deserialization (time)/[Direction] (best first)
store       1.0
cereal      1.1
flat        1.4
binary      5.4
serialise   5.5
packman     6.7

serialization (time)/BinTree Direction (best first)
flat        1.0
store       3.1
cereal      7.1
binary      8.2
serialise  12.7
packman    18.1

serialization (time)/BinTree Int (best first)
flat        1.0
store       4.4
binary     10.9
cereal     14.2
serialise  15.9
packman    30.0

serialization (time)/Cars dataset (best first)
store       1.0
flat        2.0
serialise   4.2
binary      6.7
cereal      7.0
packman     9.5

serialization (time)/Iris dataset (best first)
store       1.0
flat        9.1
serialise  10.3
cereal     16.3
packman    27.0
binary     90.5

serialization (time)/[Direction] (best first)
flat        1.0
store       1.1
cereal      1.8
binary      2.2
serialise   2.2
packman    12.6

size (bytes)/BinTree Direction (best first)
flat        1.0
binary      5.5
cereal      5.5
store       5.5
serialise  10.9
packman    87.3

size (bytes)/BinTree Int (best first)
flat        1.0
serialise   4.2
binary      8.0
cereal      8.0
store       8.0
packman    41.3

size (bytes)/Cars dataset (best first)
flat        1.0
serialise   5.3
binary      6.1
cereal      6.1
store       6.1
packman    11.3

size (bytes)/Iris dataset (best first)
packman     1.0
flat        1.0
cereal      1.0
store       1.0
serialise   1.2
binary      3.1

size (bytes)/[Direction] (best first)
flat        1.0
binary      4.7
cereal      4.7
store       4.7
serialise   9.4
packman    75.3
```
