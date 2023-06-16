import unittest
import hyperloglog

test "create empty hll":
  let hll = initHLL()
  check hll.len == 0

test "add elements, estimate cardinality":
  var hll = initHLL()
  for i in 1..1000000:
    hll.incl(i * 7)
    if i == 1 or i == 10 or i == 100 or i == 1000 or i == 10000 or i == 100000 or i == 1000000:
      let diff = abs(i - hll.getLen())
      let relDiff = float(diff) / float(i)
      check relDiff < 0.02 # Within 2%

test "estimate cardinality of merged sets":
  var hll1 = initHLL()
  var hll2 = initHLL()
  for i in 1..1000000:
    hll1.incl(i * 7)
    hll2.incl((i + 1000000) * 7)
    if i == 1 or i == 10 or i == 100 or i == 1000 or i == 10000 or i == 100000 or i == 1000000:
      let sum = i * 2
      let num = card(hll1, hll2)
      let diff = abs(sum - num)
      let relDiff = float(diff) / float(sum)
      check relDiff < 0.02 # Within 2%
      check union(hll1, hll2).len == num

test "serialize and deserialize":
  var hll1 = initHLL()
  for i in 1..100000:
    hll1.incl(i * 3)
  let num = hll1.getLen()
  let hashVal = hash(hll1)
  let byteRepr = hll1.toBytes()
  let hll2 = byteRepr.toHLL()
  check hash(hll2) == hashVal
  check card(hll2) == num

test "hash":
  var hll1 = initHLL()
  var hll2 = initHLL()
  for i in 1..1000000:
    hll1.incl(i * 7)
    hll2.incl(i * 3)
  check hash(hll1) != hash(hll2)