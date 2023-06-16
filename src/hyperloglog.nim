import math, hashes, sets

# This library provides two configurable parameters: P and Bits. Redis implementation (which this library was based upon) uses 14 and 6
const HyperLogLogP {.intdefine.} = 14
const HyperLogLogBits {.intdefine.} = 6
const HyperLogLogExactBytes {.intdefine.} = 128

type
  HLLEncoding = enum
    Dense, Sparse, Exact

  HLL* = object
    ## HyperLogLog data structure. Allows getting approximate cardinality of large sets in constant time and memory.
    exactMaxBytes: int
    sparseMaxBytes: int
    card: uint64
    encoding: HLLEncoding
    reglen: int # unfortunately, Nim does not (yet?) expose capacity of a sequence, so we store actual length here (and capacity of registers is registers.len)
    registers: seq[byte]

const
  P = HyperLogLogP
  Q = 64 - P
  Registers = 1 shl P
  PMask = uint64(Registers - 1)
  Bits = HyperLogLogBits
  RegisterMax = (1 shl Bits) - 1
  CacheInvalidFlag = 1'u64 shl 63

  DenseSize = (Registers * Bits) div 8 + 1
  SparseXZeroBit = 0x40
  SparseValBit = 0x80
  SparseValMaxValue = 32
  SparseValMaxLen = 4
  SparseZeroMaxLen = 64
  SparseXZeroMaxLen = 16384
  AlphaInf = 0.721347520444481703680

  ExactBitmapBytes = HyperLogLogExactBytes
  ExactBitmapLen = ExactBitmapBytes * 8

type 
  RawRegisters = array[0..(Registers - 1), byte]

func isCacheValid(hll: HLL): bool {.inline.} =
  (hll.card and CacheInvalidFlag) == 0
func invalidateCache(hll: var HLL) {.inline.} =
  hll.card = hll.card or CacheInvalidFlag

func denseGetRegister(regs: seq[byte], regnum: int): int {.inline.} =
  let i = (regnum * Bits) shr 3
  let fb = (regnum * Bits) and 7
  let fb8 = 8 - fb
  let b0 = int(regs[i])
  let b1 = int(regs[i + 1])
  return ((b0 shr fb) or (b1 shl fb8)) and RegisterMax

func denseSetRegister(regs: var seq[byte], regnum, val: int) {.inline.} =
  let i = (regnum * Bits) shr 3
  let fb = (regnum * Bits) and 7
  let fb8 = 8 - fb
  regs[i] = (regs[i] and not(byte(RegisterMax shl fb))) or byte(val shl fb)
  regs[i + 1] = (regs[i + 1] and not(byte(RegisterMax shr fb8))) or byte(val shr fb8)

func sparseIsZero(regs: openArray[byte], i: int): bool {.inline.} =
  (regs[i] and 0xC0) == 0
func sparseIsXZero(regs: openArray[byte], i: int): bool {.inline.} =
  (regs[i] and 0xC0) == SparseXZeroBit
func sparseIsVal(regs: openArray[byte], i: int): bool {.inline.} =
  bool(regs[i] and SparseValBit)
func sparseZeroLen(regs: openArray[byte], i: int): int {.inline.} =
  int(regs[i] and 0x3F) + 1
func sparseXZeroLen(regs: openArray[byte], i: int): int {.inline.} =
  ((int(regs[i] and 0x3F) shl 8) or int(regs[i + 1])) + 1
func sparseValValue(regs: openArray[byte], i: int): int {.inline.} =
  (int(regs[i] shr 2) and 0x1F) + 1
func sparseValLen(regs: openArray[byte], i: int): int {.inline.} =
  int(regs[i] and 0x03) + 1
func sparseValSet(regs: var openArray[byte], i: int, val, len: int) {.inline.} =
  regs[i] = ((byte(val) - 1) shl 2) or (byte(len) - 1) or SparseValBit
func sparseZeroSet(regs: var openArray[byte], i: int, len: int) {.inline.} =
  regs[i] = byte(len) - 1
func sparseXZeroSet(regs: var openArray[byte], i: int, len: int) {.inline.} =
  let l = len - 1
  regs[i] = byte(l shr 8) or SparseXZeroBit
  regs[i + 1] = byte(l and 0xFF)

func patHashLen(h: uint64): tuple[index, count: int] =
  var h = h
  result.index = int(h and PMask)
  h = (h shr P) or (1 shl Q)
  var bit = 1'u64
  result.count = 1
  while (h and bit) == 0:
    result.count += 1
    bit = bit shl 1

func patLen[A](key: A): tuple[index, count: int] =
  patHashLen(uint64(hash(key)))

proc denseSet(hll: var HLL, index, count: int): bool {.discardable.} =
  let oldcount = denseGetRegister(hll.registers, index)
  if count > oldcount:
    denseSetRegister(hll.registers, index, count)
    return true

proc denseRegHisto(hll: HLL, reghisto: var array[0..63, int]) =
  when Registers == 16384 and Bits == 6:
    var r = hll.registers
    var r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15: uint64
    for j in 0..1023:
      r0 = r[0] and 63
      r1 = ((r[0] shr 6) or (r[1] shl 2)) and 63
      r2 = ((r[1] shr 4) or (r[2] shl 4)) and 63
      r3 = (r[2] shr 2) and 63
      r4 = r[3] and 63
      r5 = ((r[3] shr 6) or (r[4] shl 2)) and 63
      r6 = ((r[4] shr 4) or (r[5] shl 4)) and 63
      r7 = (r[5] shr 2) and 63
      r8 = r[6] and 63
      r9 = ((r[6] shr 6) or (r[7] shl 2)) and 63
      r10 = ((r[7] shr 4) or (r[8] shl 4)) and 63
      r11 = (r[8] shr 2) and 63
      r12 = r[9] and 63
      r13 = ((r[9] shr 6) or (r[10] shl 2)) and 63
      r14 = ((r[10] shr 4) or (r[11] shl 4)) and 63
      r15 = (r[11] shr 2) and 63

      reghisto[r0] += 1
      reghisto[r1] += 1
      reghisto[r2] += 1
      reghisto[r3] += 1
      reghisto[r4] += 1
      reghisto[r5] += 1
      reghisto[r6] += 1
      reghisto[r7] += 1
      reghisto[r8] += 1
      reghisto[r9] += 1
      reghisto[r10] += 1
      reghisto[r11] += 1
      reghisto[r12] += 1
      reghisto[r13] += 1
      reghisto[r14] += 1
      reghisto[r15] += 1

      r = r[12..^1]
  else:
    when Bits == 8: # Another trivial case (if we are sacrificing memory for performance)
      for j in 0..<Registers:
        reghisto[hll.registers[j]] += 1
    else:
      for j in 0..<Registers:
        let reg = denseGetRegister(hll.registers, j)
        reghisto[reg] += 1

proc sparseToDense(hll: var HLL) =
  if hll.encoding == Dense:
    return
  hll.encoding = Dense
  var dense = newSeq[byte](DenseSize)
  var i, idx, runlen, regval = 0
  while i < hll.reglen:
    if sparseIsZero(hll.registers, i):
      runlen = sparseZeroLen(hll.registers, i)
      idx += runlen
      i += 1
    elif sparseIsXZero(hll.registers, i):
      runlen = sparseXZeroLen(hll.registers, i)
      idx += runlen
      i += 2
    else:
      runlen = sparseValLen(hll.registers, i)
      regval = sparseValValue(hll.registers, i)
      if runlen + idx > Registers:
        break
      while runlen > 0:
        runlen -= 1
        denseSetRegister(dense, idx, regval)
        idx += 1
      i += 1
  assert idx == Registers
  hll.reglen = dense.len
  hll.registers = dense

proc sparseSet(hll: var HLL, index, count: int): bool {.discardable.} =
  if count > SparseValMaxValue:
    hll.sparseToDense()
    result = hll.denseSet(index, count)
    assert result
    return

  if hll.registers.len < hll.sparseMaxBytes and (hll.registers.len - hll.reglen) < 3:
    var newlen = hll.reglen + 3
    newlen += min(newlen, 300)
    newlen = min(newlen, hll.sparseMaxBytes)
    hll.registers.setLen(newlen)

  var i = 0
  var first = 0
  var prev, next = -1
  var span = 0
  while i < hll.reglen:
    var oplen = 1
    if sparseIsZero(hll.registers, i):
      span = sparseZeroLen(hll.registers, i)
    elif sparseIsVal(hll.registers, i):
      span = sparseValLen(hll.registers, i)
    else:
      span = sparseXZeroLen(hll.registers, i)
      oplen = 2
    if index <= first + span - 1:
      break
    prev = i
    i += oplen
    first += span
  assert span != 0 and i < hll.reglen

  next = if sparseIsXZero(hll.registers, i): (i + 2) else: (i + 1)
  if next >= hll.reglen:
    next = -1
  
  var isZero, isXZero, isVal: bool
  var runlen: int
  if sparseIsZero(hll.registers, i):
    isZero = true
    runlen = sparseZeroLen(hll.registers, i)
  elif sparseIsXZero(hll.registers, i):
    isXZero = true
    runlen = sparseXZeroLen(hll.registers, i)
  else:
    isVal = true
    runlen = sparseValLen(hll.registers, i)

  if isVal:
    let oldcount = sparseValValue(hll.registers, i)
    if oldcount >= count:
      return false

    if runlen == 1:
      sparseValSet(hll.registers, i, count, 1)

  elif isZero and runlen == 1:
    sparseValSet(hll.registers, i, count, 1)

  else:
    var sq: array[0..4, byte]
    var last = first + span - 1
    var len: int
    var n = 0
    if isZero or isXZero:
      if index != first:
        len = index - first
        if len > SparseZeroMaxLen:
          sparseXZeroSet(sq, n, len)
          n += 2
        else:
          sparseZeroSet(sq, n, len)
          n += 1
      sparseValSet(sq, n, count, 1)
      n += 1
      if index != last:
        len = last - index
        if len > SparseZeroMaxLen:
          sparseXZeroSet(sq, n, len)
          n += 2
        else:
          sparseZeroSet(sq, n, len)
          n += 1
    else:
      let curval = sparseValValue(hll.registers, i)
      if index != first:
        len = index - first
        sparseValSet(sq, n, curval, len)
        n += 1
      sparseValSet(sq, n, count, 1)
      n += 1
      if index != last:
        len = last - index
        sparseValSet(sq, n, curval, len)
        n += 1

    let oldlen = if isXZero: 2 else: 1
    let deltalen = n - oldlen
    if deltalen > 0 and hll.reglen + deltalen > hll.sparseMaxBytes:
      hll.sparseToDense()
      result = hll.denseSet(index, count)
      assert result
      return
    assert hll.reglen + deltalen <= hll.registers.len
    if deltalen > 0 and next > -1:
      moveMem(addr hll.registers[next + deltalen], addr hll.registers[next], hll.reglen - next)
    hll.reglen += deltalen
    copyMem(addr hll.registers[i], addr sq[0], n)
  
  # updated
  i = if prev > -1: prev else: 0
  for scanlen in 0..5:
    if i >= hll.reglen:
      break
    if sparseIsXZero(hll.registers, i):
      i += 2
      continue
    elif sparseIsZero(hll.registers, i):
      i += 1
      continue
    if i + 1 < hll.reglen and sparseIsVal(hll.registers, i + 1):
      let v1 = sparseValValue(hll.registers, i)
      let v2 = sparseValValue(hll.registers, i + 1)
      if v1 == v2:
        let len = sparseValLen(hll.registers, i) + sparseValLen(hll.registers, i + 1)
        if len <= SparseValMaxLen:
          sparseValSet(hll.registers, i + 1, v1, len)
          moveMem(addr hll.registers[i], addr hll.registers[i + 1], hll.reglen - i)
          hll.reglen -= 1
          continue
      i += 1
    
  return true

proc exactToSparse(hll: var HLL) =
  if hll.encoding == Sparse or hll.encoding == Dense:
    return
  let reglen = hll.reglen
  let regs = hll.registers

  hll.encoding = Sparse
  hll.reglen = ((Registers + (SparseXZeroMaxLen - 1)) div SparseXZeroMaxLen) * 2
  hll.registers = newSeq[byte](hll.reglen)
  var i = 0
  for aux in countdown(Registers, 1, SparseXZeroMaxLen):
    let xzero = min(SparseXZeroMaxLen, aux)
    sparseXZeroSet(hll.registers, i, xzero)
    i += 2
  assert i == hll.reglen

  for i in countup(ExactBitmapBytes, reglen - 1, 8):
    let h = cast[ptr uint64](unsafeAddr regs[i])[]
    let (index, count) = patHashLen(h)
    sparseSet(hll, index, count)

proc exactSet(hll: var HLL, h: uint64): bool {.discardable.} =
  let bm = h mod ExactBitmapLen
  let bmi = bm shr 3
  let bmf = byte(1 shl (bm and 7))
  if hll.registers.len > ExactBitmapBytes and (hll.registers[bmi] and bmf) != 0:
    for i in countup(ExactBitmapBytes, hll.reglen - 1, 8):
      let eh = cast[ptr uint64](unsafeAddr hll.registers[i])[]
      if eh == h:
        return false

  if hll.reglen + 8 > hll.exactMaxBytes:
    hll.exactToSparse()
    let (index, count) = patHashLen(h)
    hll.sparseSet(index, count)
    return true

  hll.reglen += 8
  if hll.registers.len < hll.reglen:
    var newlen = hll.reglen
    newlen += min(newlen, 256)
    newlen = min(newlen, hll.exactMaxBytes)
    hll.registers.setLen(newlen)
  copyMem(addr hll.registers[hll.reglen - 8], unsafeAddr h, 8)
  hll.registers[bmi] = hll.registers[bmi] or bmf # Update bitmap
  hll.card = uint64((hll.reglen - ExactBitmapBytes) div 8)
  return true

proc sparseRegHisto(hll: HLL, reghisto: var array[0..63, int]) =
  var i, idx, runlen, regval = 0
  while i < hll.reglen:
    if sparseIsZero(hll.registers, i):
      runlen = sparseZeroLen(hll.registers, i)
      idx += runlen
      reghisto[0] += runlen
      i += 1
    elif sparseIsXZero(hll.registers, i):
      runlen = sparseXZeroLen(hll.registers, i)
      idx += runlen
      reghisto[0] += runlen
      i += 2
    else:
      runlen = sparseValLen(hll.registers, i)
      regval = sparseValValue(hll.registers, i)
      idx += runlen
      reghisto[regval] += runlen
      i += 1
  assert idx == Registers

proc rawRegHisto(regs: openArray[byte], reghisto: var array[0..63, int]) =
  var i = 0
  for j in 0..<(Registers div 8):
    let word = cast[ptr uint64](unsafeAddr regs[i])[]
    if word == 0:
      reghisto[0] += 8
    else:
      reghisto[word and 0xFF] += 1
      reghisto[(word shr 8) and 0xFF] += 1
      reghisto[(word shr 16) and 0xFF] += 1
      reghisto[(word shr 24) and 0xFF] += 1
      reghisto[(word shr 32) and 0xFF] += 1
      reghisto[(word shr 40) and 0xFF] += 1
      reghisto[(word shr 48) and 0xFF] += 1
      reghisto[(word shr 56) and 0xFF] += 1
    i += 8

func sigma(x: float64): float64 =
  if x == 1.0:
    return Inf
  var x = x
  var y, z = 1.0
  result = x
  while result != z:
    x *= x
    z = result
    result += x * y
    y += y

func tau(x: float64): float64 =
  if x == 0 or x == 1:
    return 0
  var x = x
  var y = 1.0
  var z = 1.0
  result = 1 - x
  while result != z:
    x = sqrt(x)
    z = result
    y *= 0.5
    result -= pow(1 - x, 2) * y
  return result / 3

proc count(hll: HLL or RawRegisters): uint64 =
  var m: float64 = Registers
  var reghisto: array[0..63, int]
  when hll is HLL:
    case hll.encoding
    of Dense: denseRegHisto(hll, reghisto)
    of Sparse: sparseRegHisto(hll, reghisto)
    of Exact: return uint64((hll.reglen - ExactBitmapBytes) div 8)
  else:
    rawRegHisto(hll, reghisto)
  var z = m * tau((m - float64(reghisto[Q + 1])) / m)
  for j in countdown(Q, 1):
    z += float64(reghisto[j])
    z *= 0.5
  z += m * sigma(float64(reghisto[0]) / m)
  return round(AlphaInf * m * m / z).uint64

proc merge(hset: var HashSet, hll: HLL) =
  assert hll.encoding == Exact
  for i in countup(ExactBitmapBytes, hll.reglen - 1, 8):
    let h = cast[ptr uint64](unsafeAddr hll.registers[i])[]
    hset.incl(h)

proc merge(max: var RawRegisters, hll: HLL) =
  if hll.encoding == Dense:
    for i in 0..<Registers:
      max[i] = max(max[i], byte(denseGetRegister(hll.registers, i)))
  else:
    var i = 0
    var idx = 0
    while i < hll.reglen:
      if sparseIsZero(hll.registers, i):
        idx += sparseZeroLen(hll.registers, i)
        i += 1
      elif sparseIsXZero(hll.registers, i):
        idx += sparseXZeroLen(hll.registers, i)
        i += 2
      else:
        let runlen = sparseValLen(hll.registers, i)
        let regval = sparseValValue(hll.registers, i)
        assert runlen + idx <= Registers
        for j in 0..<runlen:
          max[idx] = max(max[idx], byte(regval))
          idx += 1
        i += 1
    assert idx == Registers

proc initHLL*(sparseMaxBytes: int = 3000, exactMaxBytes: int = 0): HLL =
  ## Creates an HLL object. By default, HLL is created using sparse encoding (unless exactMaxBytes is greater than 0).
  ## This will be upgraded to the sparse/dense representation as needed.
  ## You can pass sparseMaxBytes/exactMaxBytes arguments to specify the actual thresholds.
  result.encoding = Exact
  result.exactMaxBytes = exactMaxBytes
  result.sparseMaxBytes = sparseMaxBytes
  result.reglen = ExactBitmapBytes
  if exactMaxBytes == 0:
    result.exactToSparse()

proc incl*[A](hll: var HLL, key: A): bool {.discardable.} =
  ## Includes an element `key` in `hll`. Returns `true` if the data structure was modified by this operation.
  if hll.encoding == Exact:
    let h = uint64(hash(key))
    result = hll.exactSet(h)
    return result

  let (index, count) = patLen(key)
  case hll.encoding
  of Dense: result = hll.denseSet(index, count)
  of Sparse: result = hll.sparseSet(index, count)
  else: discard
  if result:
    hll.invalidateCache()

proc add*[A](hll: var HLL, key: A): bool {.discardable.} =
  ## Alias for `incl`.
  ## 
  ## See also:
  ## * `incl proc <#incl,HLL,A>`_
  incl(hll, key)

func len*(hll: HLL): int =
  ## Returns the number of elements in `hll`.
  ## This will use cached cardinality if it's available, but won't update it, so it's side-effect free.
  ## 
  ## See also:
  ## * `card proc <#card,varargs[HLL]>`_
  ## * `getLen proc <#getLen,HLL>`_
  if not hll.isCacheValid():
    return int(hll.count())
  return int(hll.card)

proc getLen*(hll: var HLL): int =
  ## Returns the number of elements in `hll`.
  ## This will use cached cardinality if it's available, otherwise it will be computed and stored (so this call has side effects).
  ## 
  ## See also:
  ## * `card proc <#card,varargs[HLL]>`_
  ## * `getLen proc <#getLen,HLL>`_
  if not hll.isCacheValid():
    hll.card = hll.count()
  return int(hll.card)

proc card*(hlls: varargs[HLL]): int =
  ## Returns the number of elements in the union of all given `hlls`.
  ## This is equivalent to union(hlls).len, but a little bit more performant.
  ## 
  ## See also:
  ## * `len proc <#len,HLL>`_
  ## * `getLen proc <#getLen,HLL>`_
  if hlls.len == 1:
    return hlls[0].len

  var max: RawRegisters
  for i in 0..<len(hlls):
    merge(max, hlls[i])
  return int(count(max))

proc union*(hlls: varargs[HLL]): HLL =
  ## Returns the union of the HyperLogLogs `hlls`.
  if hlls.len == 0:
    return initHLL()

  var encoding = Exact
  result.exactMaxBytes = hlls[0].exactMaxBytes
  result.sparseMaxBytes = hlls[0].sparseMaxBytes
  for i in 0..<len(hlls):
    if hlls[i].encoding == Dense:
      encoding = Dense
    elif encoding == Exact and hlls[i].encoding != Exact:
      encoding = Sparse
  result = initHLL()
  if encoding == Exact:
    var hset: HashSet[uint64]
    for i in 0..<len(hlls):
      merge(hset, hlls[i])
    if hset.card * 8 + ExactBitmapBytes > result.exactMaxBytes:
      result.exactToSparse()
      for h in hset:
        let (index, count) = patHashLen(h)
        result.sparseSet(index, count)
    else:
      for h in hset:
        result.exactSet(h)
    result.card = uint64(hset.card)
    return

  var max: RawRegisters
  for i in 0..<len(hlls):
    merge(max, hlls[i])
  if encoding == Sparse:
    result.exactToSparse()
  elif encoding == Dense:
    result.sparseToDense()
  for j in 0..<Registers:
    if max[j] == 0:
      continue
    case result.encoding
    of Dense: result.denseSet(j, int(max[j]))
    of Sparse: result.sparseSet(j, int(max[j]))
    else: discard
  result.invalidateCache()

proc merge*(hlls: varargs[HLL]): HLL =
  ## Alias for `union(hlls)`
  ## 
  ## See also:
  ## * `union proc <#union,varargs[HLL]>`_
  union(hlls)

proc `+`*(a, b: HLL): HLL = union(a, b)
  ## Alias for `union(a, b)`
  ## 
  ## See also:
  ## * `union proc <#union,varargs[HLL]>`_

proc clear*(hll: var HLL) =
  ## Removes all elements from `hll` (without freeing up memory, so it can be reused).
  hll.card = 0
  if hll.exactMaxBytes > 0:
    hll.encoding = Exact
    hll.reglen = ExactBitmapBytes
    if hll.registers.len < hll.reglen:
      hll.registers.setLen(hll.reglen)
    zeroMem(addr hll.registers[0], hll.reglen)
  else:
    hll.encoding = Sparse

proc hash*(hll: HLL): Hash =
  ## Returns hash of `hll`.
  var h: Hash = 0
  h = h !& int(hll.encoding)
  h = h !& hashData(unsafeAddr hll.registers[0], hll.reglen)
  result = !$h

proc toBytes*(hll: HLL): seq[byte] =
  ## Serializes `hll` to a byte array, so it can be stored and deserialized later.
  ## 
  ## See also:
  ## * `toHLL proc <#toHLL,seq[byte],int,int>`_
  result = newSeq[byte](9 + hll.reglen)
  result[0] = byte(hll.encoding)
  copyMem(addr result[1], unsafeAddr hll.card, 8)
  copyMem(addr result[9], unsafeAddr hll.registers[0], hll.reglen)

proc toHLL*(bytes: seq[byte], sparseMaxBytes: int = 3000, exactMaxBytes: int = 0): HLL =
  ## Restores HyperLogLog from a byte array `bytes`.
  ## 
  ## See also:
  ## * `toBytes proc <#toBytes,HLL>`_
  result.sparseMaxBytes = sparseMaxBytes
  result.exactMaxBytes = exactMaxBytes
  result.reglen = bytes.len - 9
  result.registers = newSeq[byte](result.reglen)
  result.encoding = HLLEncoding(bytes[0])
  copyMem(addr result.card, unsafeAddr bytes[1], 8)
  copyMem(addr result.registers[0], unsafeAddr bytes[9], result.reglen)