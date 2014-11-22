```
Generating tree... Done.
benchmarking binary/serialize
analysing with 1000 resamples
measurement overhead 507.5 ns
bootstrapping with 42 of 339 samples (12%)
time                 1.936 ns   (1.933 ns .. 1.939 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.937 ns   (1.934 ns .. 1.942 ns)
std dev              13.22 ps   (9.122 ps .. 22.00 ps)
found 2 outliers among 42 samples (4.8%)
  1 (2.4%) high mild
  1 (2.4%) high severe
variance introduced by outliers: 1% (slightly inflated)

benchmarking binary/serialize + deserialize
measurement took 39.03 s
analysing with 1000 resamples
bootstrapping with 3 of 4 samples (75%)
time                 2.629 s    (2.366 s .. NaN s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 2.617 s    (2.589 s .. 2.663 s)
std dev              40.69 ms   (0.0 s .. 42.97 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking cereal/serialize
analysing with 1000 resamples
bootstrapping with 42 of 339 samples (12%)
time                 1.932 ns   (1.930 ns .. 1.934 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.932 ns   (1.930 ns .. 1.935 ns)
std dev              8.964 ps   (7.190 ps .. 11.75 ps)
found 2 outliers among 42 samples (4.8%)
  2 (4.8%) high mild
variance introduced by outliers: 0% (unaffected)

benchmarking cereal/serialize + deserialize
measurement took 23.59 s
analysing with 1000 resamples
bootstrapping with 3 of 4 samples (75%)
time                 1.571 s    (1.347 s .. 1.807 s)
                     0.997 R²   (0.989 R² .. 1.000 R²)
mean                 1.594 s    (1.552 s .. 1.628 s)
std dev              54.42 ms   (0.0 s .. 59.41 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking packman/serialize
measurement took 105.9 s
analysing with 1000 resamples
bootstrapping with 3 of 4 samples (75%)
time                 6.990 s    (6.392 s .. 7.730 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 6.856 s    (6.717 s .. 6.940 s)
std dev              128.3 ms   (0.0 s .. 146.2 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking packman/serialize + deserialize
measurement took 175.1 s
analysing with 1000 resamples
bootstrapping with 3 of 4 samples (75%)
time                 11.74 s    (11.42 s .. 11.95 s)
                     1.000 R²   (1.000 R² .. NaN R²)
mean                 11.71 s    (11.65 s .. 11.74 s)
std dev              56.65 ms   (0.0 s .. 64.46 ms)
variance introduced by outliers: 19% (moderately inflated)
```
