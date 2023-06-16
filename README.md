# hyperloglog

A HyperLogLog implementation in Nim. HyperLogLog is a compact data structure for estimating cardinality (the number of elements) of large sets. You can read more about it [here](http://antirez.com/news/75).

This library is a direct port of [Redis implementation](https://github.com/redis/redis/blob/unstable/src/hyperloglog.c) of HyperLogLog. Just like Redis, this implementation uses 16384 registers 6 bits each (~12k bytes in total). It is written natively in Nim and has no dependencies.

As an added bonus, you can also pass an optional parameter `exactMaxSize` to `initHLL` constructor and this will enable exact counting for small sets (about 500-5000 elements).

API Reference: https://denull.github.io/hyperloglog/

## Installation

```
nimble install hyperloglog
```

## Usage

API of this library mimics HashSet: you create a HLL using `initHLL`, and add elements using `incl`.

For retrieving the cardinality, there're 3 methods with slightly different behavior:
* `len(hll: HLL)` Computes cardinality without modifying the HLL.
* `getLen(hll: var HLL)` Computes cardinality, and potentially updates the cached value. Note that HLL should be a `var`.
* `card(hlls: varargs[HLL])` Computes cardinality of an union of multiple HLL's. If you pass a single HLL, it behaves just like `len()`.

Example:

```nim
import hyperloglog

var hll1 = initHLL()

hll1.incl(1)
hll1.incl(2)
hll1.incl(3)
hll1.incl(3)
echo hll1.len # Should print 3

var hll2 = initHLL()
# You can add elements of arbitrary (and different) types to HLL (as long as they have a `hash` function defined for them)
hll2.incl(123.0)
hll2.incl("Hello world!")

echo card(hll1, hll2) # Same as union(hll1, hll2).len, but a bit faster

let byteSeq = (hll1 + hll2).toBytes # hll1 + hll2 in an alias for union(hll1, hll2)
echo "Serialized to ", byteSeq.len, " bytes"

let hll3 = byteSeq.toHLL # `toBytes` and `toHLL` can be used to (de)serialize HyperLogLogs
echo card(hll3) # Should still have 5 elements
```

## Exact representation

In addition to Sparse and Dense encodings used by Redis, there's also an experimental Exact encoding, which is intended to be used for very small sets (up to 500-5000 elements, depending on `exactMaxBytes` parameter). Each elemented is represented by its 8-byte hash, so `exactMaxBytes` must be 8 times the number of elements when the switch to sparse (or dense) representation should happen (4000 bytes for 500 elements; 40000 bytes for 5000 elements; a value of 8000 is recommended).

This mode is enabled by specifying `exactMaxBytes` parameter: up until that size, HLL will store exact values as a plain list of hashes. When new value is added, the list is iterated to check if this hash already exists, and if it doesn't, it's appended at the end.

This means that adding new values in this mode is basically `O(N)` and memory used is also `O(N)`, so it's only reasonable for very small sets. However, for those cases it will provide exact (unless there're no hash collisions, of course), not approximate cardinality.

## Configuration

There're 4 configurable parameters.

`HyperLogLogP` and `HyperLogLogBits` integers define the values of `P` (number of bits to use for indexing registers, 14 by default) and `Bits` (size of each register, 6 by default). You can adjust them using `-d` command-line flag:

```
nim c -d=HyperLogLogP:13 -d=HyperLogLogBits:5 yourapp.nim
```

You can also set the value of `sparseMaxBytes` and `exactMaxBytes` parameters by passing it to `initHLL` constructor. They define thresholds after which the HLL will be converted from Sparse to Dense and from Exact to Sparse representations.

For `sparseMaxBytes` Redis [recommends](https://redis.io/docs/management/config-file/) value of 3000 unless "CPU is not a concern" (in which case it can be raised up to 10000). For `exactMaxBytes` I personally recommend value of 8000 (which corresponds to cardinalities up to 1000 elements - each element is represented as a 8-byte hash). However, as it's an experimental mode, by default it's set to 0 (i.e. exact mode is disabled).

## Performance

Here are some benchmarks with different values of `exactMaxBytes`.

![exactMaxBytes = 0](https://raw.githubusercontent.com/deNULL/hyperloglog/images/graph-1m-noexact.png)

![exactMaxBytes = 8000](https://raw.githubusercontent.com/deNULL/hyperloglog/images/graph-1m-exact8000.png)

![exactMaxBytes = 32000](https://raw.githubusercontent.com/deNULL/hyperloglog/images/graph-1m-exact32000.png)