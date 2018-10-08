Benchmarks for Haskell serialization libraries.

For every library we measure the serialization time, deserialization time and size of the encoded output for a set of test datasets.

To merge all these measures in a concrete use case, we also estimate the total transfer time at different transmission speeds, that's to say the the time that it takes to serialize, move across a network (with no compression and no protocol overheads) and deserialize a given dataset.      

## Summary Results

For every test and every network speed, the table lists all packages whose transfer speeds are within 30% of the best result (with best package listed first).

||BinTree Direction|BinTree Int|Cars dataset|Iris dataset|[Direction]|
| ---| ---| ---| ---| ---| ---|
|deserialization (time)|flat0.3.2,store0.5.0|store0.5.0,cereal0.5.7.0,flat0.3.2|flat0.3.2,store0.5.0|store0.5.0,flat0.3.2|flat0.3.2,cereal0.5.7.0,store0.5.0|
|serialization (time)|store0.5.0|store0.5.0|store0.5.0|store0.5.0|store0.5.0|
|size (bytes)|flat0.3.2,flat-0.3.2|flat-0.3.2,flat0.3.2|flat-0.3.2,flat0.3.2|packman-0.5.0,packman0.5.0,flat-0.3.2,flat0.3.2,[store-0.5.0](https://hackage.haskell.org/package/store),cereal-0.5.7.0,cereal0.5.7.0,store-0.5.0,store0.5.0,serialise-0.2.0.0,serialise0.2.0.0|flat-0.3.2,flat0.3.2|
|transfer [10 MBits] (time)|flat0.3.2|flat0.3.2|flat0.3.2|flat0.3.2,store0.5.0,packman0.5.0,cereal0.5.7.0,serialise0.2.0.0|flat0.3.2|
|transfer [100 MBits] (time)|flat0.3.2,store0.5.0|flat0.3.2|flat0.3.2|store0.5.0,flat0.3.2|store0.5.0,flat0.3.2,cereal0.5.7.0|
|transfer [1000 MBits] (time)|store0.5.0,flat0.3.2|flat0.3.2,store0.5.0|store0.5.0,flat0.3.2|store0.5.0|store0.5.0,flat0.3.2|


## Full Results

[Full Results](https://rawgit.com/haskell-perf/serialization/master/report.md)

* [Raw Criterion Results](https://rawgit.com/haskell-perf/serialization/master/report.html)

* [Raw Criterion Results in JSON Format], also in [json format](https://raw.githubusercontent.com/haskell-perf/serialization/master/report.json).


## Tested Libraries

Performance is not the only relevant property, depending on your needs you should also consider other features like laziness and compatibility.

| Package                                                            | Laziness | Compatibility             |
| ---                                                                | ---      | ---                       |
| [store-0.4.3.1](https://hackage.haskell.org/package/store)         | Strict   | Haskell-Same-Architecture |
| [packman-0.3.0](http://hackage.haskell.org/package/packman)        | Lazy     | Haskell-Same-Architecture |
| [cereal-0.5.4.0](http://hackage.haskell.org/package/cereal)        | Strict   | Haskell                   |
| [binary-0.8.5.1](http://hackage.haskell.org/package/binary)        | Lazy     | Haskell                   |
| [serialise-0.1.0.0](https://hackage.haskell.org/package/serialise) | Lazy     | Multi-Language            |
| [flat-0.3](https://github.com/tittoassini/flat)                    | Strict   | Multi-Language            |


Compatibility Levels (lowest to highest):
* Haskell-Same-Architecture
  - Compatible across Haskell systems sharing the same CPU and endianness
* Haskell
  - Compatible across Haskell systems
* Multi-Language
  - Compatible across different programming languages

All compiled with GHC 8.4.3.

## Test Data

| Test              | Description                                                          |
| ---               | ---                                                                  |
| BinTree Direction | Binary Tree of a simple enumeration data type                        |
| BinTree Int       | Binary Tree of Ints                                                  |
| [Direction]       | A List of a simple enumeration data type                             |
| Cars              | A dataset of Cars descriptions (mostly lists, enumerations and Ints) |
| Iris              | A dataset of Iris descriptions (mostly lists and floats)             |

Shout if you would like other tests to be added!

## Running the Benchmarks

Run the benchmarks with:

`stack bench :all`

If you get this error:

`...<stdout>: commitBuffer: invalid argument (invalid character)`

Try:

`export LC_ALL=C.UTF-8`









