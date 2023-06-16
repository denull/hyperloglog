# hyperloglog

A HyperLogLog implementation in Nim. HyperLogLog is a compact data structure for estimating cardinality (the number of elements) of large sets. You can read more about it [here](http://antirez.com/news/75).

This library is a direct port of [Redis implementation](https://github.com/redis/redis/blob/unstable/src/hyperloglog.c) of HyperLogLog. Just like Redis, this implementation uses 16384 registers 6 bits each (~12k bytes in total). It is written natively in Nim and has no dependencies.

As an added bonus, you can also pass an optional parameter `exactMaxSize` to `initHLL` constructor and this will enable exact counting for small sets (about 100-1000 elements).

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

In addition to Sparse and Dense encodings used by Redis, there's also an experimental Exact encoding, which is intended to be used for very small sets (up to 100-1000 elements, depending on `exactMaxBytes` parameter).

This mode is enabled by specifying `exactMaxBytes` parameter: up until that size, HLL will store exact values.

To be precise, in exact mode first `HyperLogLogExactBytes` (128 by default) are used to store a bitmap which allows skipping full scans when adding unique values, and plain list of added hashes. When new value is added, it's first checked in the bitmap and if the corresponding bit is set, the list of hashes is iterated to check for uniqueness. If value is not added, it's simply appended to the list.

This means that adding new values in this mode is basically `O(N)` and memory used is also `O(N)`, so it's only reasonable for very small sets. However, for those cases it will provide exact (unless there're no hash collisions, of course), not approximate cardinality.

## Configuration

There're 5 configurable parameters.

`HyperLogLogP`, `HyperLogLogBits` and `HyperLogLogExactBytes` integers define the values of `P` (number of bits to use for indexing registers, 14 by default), `Bits` (size of each register, 6 by default) and `ExactBytes` (number of bytes to use for bitmap when HLL is in exact mode). You can adjust them using `-d` command-line flag:

```
nim c -d=HyperLogLogP:13 -d=HyperLogLogBits:5 yourapp.nim
```

You can also set the value of `sparseMaxBytes` and `exactMaxBytes` parameters by passing it to `initHLL` constructor. It defines thresholds after which the HLL will be converted from Sparse to Dense and from Exact to Sparse representations.