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

## Run the Tests

`git clone https://github.com/tittoassini/serialization-bench;cd serialization-bench;stack build --exec serialization-bench`

## Full Results

[Full Criterion Report](http://htmlpreview.github.io/?https://github.com/tittoassini/serialization-bench/blob/master/report.html).

## Summary Results


```
deserialization-BinTree Direction ordered by Time (cereal: 512 mSecs)
cereal                  1.0
store                   1.3
flat                    3.1
binary                  4.5
binary-serialise-cbor   5.3
packman                 6.8

deserialization-BinTree Int ordered by Time (store: 426 mSecs)
store                   1.0
cereal                  1.2
binary                  3.2
binary-serialise-cbor   3.5
flat                    4.5
packman                 9.0

deserialization-Cars dataset ordered by Time (cereal: 233 mSecs)
cereal                  1.0
packman                 1.5
store                   1.7
flat                    2.5
binary-serialise-cbor   4.1
binary                  4.4

deserialization-Iris dataset ordered by Time (store: 200 mSecs)
store                   1.0
binary-serialise-cbor   1.6
packman                 1.7
cereal                  2.2
binary                  7.4
flat                   19.1

deserialization-[Direction] ordered by Time (cereal: 173 mSecs)
cereal                  1.0
store                   1.4
flat                    1.7
binary                  2.2
binary-serialise-cbor   2.7
packman                 6.0

serialization-BinTree Direction ordered by Time (store: 111 mSecs)
store                   1.0
flat                    5.9
binary                  7.3
cereal                  9.7
binary-serialise-cbor  18.9
packman                25.9

serialization-BinTree Int ordered by Time (store: 93 mSecs)
store                   1.0
flat                    7.8
binary-serialise-cbor   8.1
binary                 12.6
cereal                 16.5
packman                35.2

serialization-Cars dataset ordered by Time (store: 28 mSecs)
store                   1.0
binary-serialise-cbor   4.6
flat                    6.8
binary                  7.3
cereal                  7.3
packman                11.7

serialization-Iris dataset ordered by Time (store: 7 mSecs)
store                   1.0
binary-serialise-cbor  11.8
cereal                 17.0
packman                28.9
binary                 86.8
flat                  113.3

serialization-[Direction] ordered by Time (store: 21 mSecs)
store                   1.0
cereal                  3.4
binary                  3.6
flat                    4.3
binary-serialise-cbor   4.8
packman                41.9

serialisation-[Direction] ordered by Size (flat: 425103 bytes)
flat                    1.0
store                   2.4
binary                  2.4
cereal                  2.4
binary-serialise-cbor   4.7
packman                75.3

serialisation-BinTree Direction ordered by Size (flat: 1153547 bytes)
flat                    1.0
store                   5.5
binary                  5.5
cereal                  5.5
binary-serialise-cbor  10.9
packman                87.3

serialisation-BinTree Int ordered by Size (flat: 2621440 bytes)
flat                    1.0
binary-serialise-cbor   4.2
store                   8.0
binary                  8.0
cereal                  8.0
packman                41.3

serialisation-Cars dataset ordered by Size (flat: 1148401 bytes)
flat                    1.0
binary-serialise-cbor   5.3
store                   6.1
binary                  6.1
cereal                  6.1
packman                 9.8

serialisation-Iris dataset ordered by Size (packman: 9616872 bytes)
packman                 1.0
store                   1.0
cereal                  1.0
flat                    1.1
binary-serialise-cbor   1.2
binary                  3.2


```
