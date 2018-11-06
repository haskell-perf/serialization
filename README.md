Benchmarks for Haskell serialization libraries.

For every library, we measure the serialization time, deserialization time and size of the encoded output for a set of test datasets.

## Summary Results

The summary tables list, for every test, all libraries that are within 30% of the best result (with best library listed first).

### Transfer Time

To merge all measures in a concrete use case, we estimate the total transfer time at different transmission speeds, that's to say the the time that it takes to serialize, move across a network (with no compression and no protocol overheads) and deserialize a given dataset.

|Dataset\Measure|transfer [10 MBits]|transfer [100 MBits]|transfer [1000 MBits]|
| ---| ---| ---| ---|              
|BinTree Direction|[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat),[persist](https://hackage.haskell.org/package/persist),[store](https://hackage.haskell.org/package/store)|
|BinTree Int|[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat),[store](https://hackage.haskell.org/package/store),[persist](https://hackage.haskell.org/package/persist)|
|Cars|[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat),[store](https://hackage.haskell.org/package/store),[persist](https://hackage.haskell.org/package/persist)|
|Iris|[flat](https://hackage.haskell.org/package/flat),[store](https://hackage.haskell.org/package/store),[packman](https://hackage.haskell.org/package/packman),[persist](https://hackage.haskell.org/package/persist),[cereal](https://hackage.haskell.org/package/cereal),[serialise](https://hackage.haskell.org/package/serialise)|[store](https://hackage.haskell.org/package/store),[flat](https://hackage.haskell.org/package/flat),[persist](https://hackage.haskell.org/package/persist)|[store](https://hackage.haskell.org/package/store),[flat](https://hackage.haskell.org/package/flat)|
|[Direction]|[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat),[persist](https://hackage.haskell.org/package/persist)|[flat](https://hackage.haskell.org/package/flat),[persist](https://hackage.haskell.org/package/persist),[store](https://hackage.haskell.org/package/store)|

When the network speed is low, transfer times are dominated by the size of the encoded dataset. At higher speeds, by the (de)serialisation times.

Tests are executed on an Intel Xeon W3520 @ 2.67GHz. On a slower machine, de(serialization) times would matter more, on a faster machine size would be more significant.

### (De)serialization Times and Size

|Dataset\Measure|deserialization|serialization|size|
| ---| ---| ---| ---|              
|BinTree Direction|[flat](https://hackage.haskell.org/package/flat),[persist](https://hackage.haskell.org/package/persist),[store](https://hackage.haskell.org/package/store)|[persist](https://hackage.haskell.org/package/persist),[store](https://hackage.haskell.org/package/store)|[flat](https://hackage.haskell.org/package/flat)|
|BinTree Int|[store](https://hackage.haskell.org/package/store),[persist](https://hackage.haskell.org/package/persist),[flat](https://hackage.haskell.org/package/flat),[cereal](https://hackage.haskell.org/package/cereal)|[flat](https://hackage.haskell.org/package/flat),[persist](https://hackage.haskell.org/package/persist),[store](https://hackage.haskell.org/package/store)|[flat](https://hackage.haskell.org/package/flat)|
|Cars|[persist](https://hackage.haskell.org/package/persist),[flat](https://hackage.haskell.org/package/flat),[store](https://hackage.haskell.org/package/store)|[store](https://hackage.haskell.org/package/store)|[flat](https://hackage.haskell.org/package/flat)|
|Iris|[store](https://hackage.haskell.org/package/store),[persist](https://hackage.haskell.org/package/persist),[flat](https://hackage.haskell.org/package/flat)|[store](https://hackage.haskell.org/package/store)|[packman](https://hackage.haskell.org/package/packman),[flat](https://hackage.haskell.org/package/flat),[cereal](https://hackage.haskell.org/package/cereal),[persist](https://hackage.haskell.org/package/persist),[store](https://hackage.haskell.org/package/store),[serialise](https://hackage.haskell.org/package/serialise)|
|[Direction]|[flat](https://hackage.haskell.org/package/flat),[persist](https://hackage.haskell.org/package/persist),[cereal](https://hackage.haskell.org/package/cereal),[store](https://hackage.haskell.org/package/store)|[persist](https://hackage.haskell.org/package/persist),[store](https://hackage.haskell.org/package/store),[flat](https://hackage.haskell.org/package/flat)|[flat](https://hackage.haskell.org/package/flat)|


## Full Results

[Full Results](report.md)

* [Criterion Results](https://rawgit.com/haskell-perf/serialization/master/report.html)

* [Criterion Results in JSON Format](https://raw.githubusercontent.com/haskell-perf/serialization/master/report.json)

## Tested Libraries

Performance is not the only relevant property, depending on your needs you should also consider other features like laziness and compatibility.

| Package                                                            | Laziness | Compatibility             |
| ---                                                                | ---      | ---                       |
| [store-0.5.0.1](https://hackage.haskell.org/package/store)         | Strict   | Haskell-Same-Architecture |
| [packman-0.5.0](http://hackage.haskell.org/package/packman)        | Lazy     | Haskell-Same-Architecture |
| [cereal-0.5.7.0](http://hackage.haskell.org/package/cereal)        | Strict   | Haskell                   |
| [binary-0.8.5.1](http://hackage.haskell.org/package/binary)        | Lazy     | Haskell                   |
| [serialise-0.2.1.0](https://hackage.haskell.org/package/serialise) | Lazy     | Multi-Language            |
| [flat-0.3.4](https://hackage.haskell.org/package/flat)             | Strict   | Multi-Language            |
| [persist-0.1](https://hackage.haskell.org/package/persist)         | Strict   | Haskell            |

Compatibility Levels (lowest to highest):
* Haskell-Same-Architecture
  - Compatible across Haskell systems sharing the same CPU and endianness
* Haskell
  - Compatible across Haskell systems
* Multi-Language
  - Compatible across different programming languages

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

The executable will write the *report.md*, *report.html* and *report.json* files containing the full test results and will print out the two summary results tables in markdown format.

Tests will be compiled with GHC 8.4.4.

