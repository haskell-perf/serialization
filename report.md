

||BinTree Direction|BinTree Int|Cars dataset|Iris dataset|[Direction]|
| ---   | ---      | ---         | ---              | --- | --- |
|deserialization (time)|flat0.3.2,store0.5.0|store0.5.0,cereal0.5.7.0,flat0.3.2|flat0.3.2,store0.5.0|store0.5.0,flat0.3.2|flat0.3.2,cereal0.5.7.0,store0.5.0|
|serialization (time)|store0.5.0|store0.5.0|store0.5.0|store0.5.0|store0.5.0|
|size (bytes)|flat-0.3.2,flat0.3.2|flat-0.3.2,flat0.3.2|flat-0.3.2,flat0.3.2|packman-0.5.0,packman0.5.0,flat-0.3.2,flat0.3.2,[store-0.5.0](https://hackage.haskell.org/package/store),cereal-0.5.7.0,cereal0.5.7.0,store-0.5.0,store0.5.0,serialise-0.2.0.0,serialise0.2.0.0|flat0.3.2,flat-0.3.2|
|transfer [10 MBits] (time)|flat0.3.2|flat0.3.2|flat0.3.2|flat0.3.2,store0.5.0,packman0.5.0,cereal0.5.7.0,serialise0.2.0.0|flat0.3.2|
|transfer [100 MBits] (time)|flat0.3.2,store0.5.0|flat0.3.2|flat0.3.2|store0.5.0,flat0.3.2|store0.5.0,flat0.3.2,cereal0.5.7.0|
|transfer [1000 MBits] (time)|store0.5.0,flat0.3.2|flat0.3.2,store0.5.0|store0.5.0,flat0.3.2|store0.5.0|store0.5.0,flat0.3.2|

