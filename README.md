This is a micro-benchmark that compares `binary`, `cereal` and `packman`
serialization and deserialization performances.

`packman` is not on Hackage, so we include it here as a Git submodule. Make sure
to initialize submodules and add `packman` as source in your Cabal sandbox.
Something like this should work:

```
$ git submodule update --init --recursive
$ cabal sandbox init
$ cabal sandbox add-source packman/
$ cabal install
```

## Results

Versions:

- binary: 0.7.6.1
- cereal: 0.5.1.0
- packman: 0.2

```
benchmarking serialization/binary
time                 1.934 ns   (1.928 ns .. 1.942 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.938 ns   (1.932 ns .. 1.943 ns)
std dev              18.99 ps   (16.36 ps .. 22.03 ps)
variance introduced by outliers: 11% (moderately inflated)

benchmarking serialization/cereal
time                 1.929 ns   (1.922 ns .. 1.937 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.922 ns   (1.917 ns .. 1.927 ns)
std dev              17.08 ps   (14.02 ps .. 20.56 ps)

benchmarking serialization/packman
time                 7.980 s    (NaN s .. 8.323 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.307 s    (8.221 s .. 8.351 s)
std dev              74.09 ms   (0.0 s .. 76.94 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking deserialization/binary
time                 1.906 ns   (1.905 ns .. 1.907 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.906 ns   (1.905 ns .. 1.907 ns)
std dev              3.480 ps   (2.746 ps .. 4.998 ps)

benchmarking deserialization/cereal
time                 704.5 ms   (600.7 ms .. 787.5 ms)
                     0.998 R²   (NaN R² .. 1.000 R²)
mean                 741.0 ms   (733.5 ms .. 747.0 ms)
std dev              9.351 ms   (0.0 s .. 10.38 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking deserialization/packman
time                 434.1 ms   (34.34 ms .. 892.5 ms)
                     0.882 R²   (0.700 R² .. 1.000 R²)
mean                 539.0 ms   (480.4 ms .. 584.8 ms)
std dev              70.85 ms   (0.0 s .. 79.31 ms)
variance introduced by outliers: 24% (moderately inflated)
```
