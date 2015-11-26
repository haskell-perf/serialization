This is a micro-benchmark that compares `binary`, `binary-serialise-cbor`,
`cereal` and `packman` serialization and deserialization performances.

`binary-serialise-cbor` and `packman` are not on Hackage, so we include them
here as Git submodules. Make sure to initialize submodules and add
`binary-serialise-cbor` and `packman` as sources in your Cabal sandbox.
Something like this should work:

```
$ git submodule update --init --recursive
$ cabal sandbox init
$ cabal sandbox add-source packman/
$ cabal sandbox add-source binary-serialise-cbor/
$ cabal install
```

## Results

Versions:

- binary: 0.7.6.1
- binary-serialise-cbor: 0.1.1.0
- cereal: 0.5.1.0
- packman: 0.2

```
benchmarking serialization/binary
time                 1.940 ns   (1.936 ns .. 1.945 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.939 ns   (1.935 ns .. 1.942 ns)
std dev              13.07 ps   (11.19 ps .. 15.40 ps)

benchmarking serialization/cereal
time                 1.934 ns   (1.930 ns .. 1.937 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.937 ns   (1.933 ns .. 1.940 ns)
std dev              10.55 ps   (8.495 ps .. 14.00 ps)

benchmarking serialization/packman
time                 7.912 s    (7.766 s .. 8.181 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.420 s    (8.225 s .. 8.564 s)
std dev              219.1 ms   (0.0 s .. 249.5 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking serialization/binary-CBOR
time                 1.956 ns   (1.947 ns .. 1.964 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.941 ns   (1.935 ns .. 1.947 ns)
std dev              21.34 ps   (17.49 ps .. 25.79 ps)
variance introduced by outliers: 13% (moderately inflated)

benchmarking deserialization/binary
time                 1.937 ns   (1.934 ns .. 1.940 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.940 ns   (1.937 ns .. 1.944 ns)
std dev              12.25 ps   (10.33 ps .. 15.01 ps)

benchmarking deserialization/cereal
time                 697.0 ms   (601.6 ms .. 772.0 ms)
                     0.998 R²   (0.992 R² .. 1.000 R²)
mean                 742.9 ms   (736.5 ms .. 747.8 ms)
std dev              7.562 ms   (136.0 as .. 8.498 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking deserialization/packman
time                 518.2 ms   (82.76 ms .. 845.9 ms)
                     0.926 R²   (0.748 R² .. 1.000 R²)
mean                 571.6 ms   (504.1 ms .. 609.6 ms)
std dev              59.82 ms   (0.0 s .. 65.78 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking deserialization/binary-CBOR
time                 1.936 ns   (1.933 ns .. 1.939 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.933 ns   (1.929 ns .. 1.936 ns)
std dev              11.36 ps   (9.370 ps .. 14.93 ps)
```
