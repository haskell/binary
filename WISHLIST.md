# Wishlist for the Next Breaking Release

Changes that are desirable for correctness, safety, or cleanup, but require a
breaking API change (PVP major version bump).

## Safe `Binary` decoding for `Set` and `Map`

`Binary (Set.Set a)` and `Binary (Map.Map k e)` currently have no `Ord`
constraint on their element/key type:

```haskell
instance (Binary a) => Binary (Set.Set a)
instance (Binary k, Binary e) => Binary (Map.Map k e)
```

Because `get` uses `Set.fromDistinctAscList` / `Map.fromDistinctAscList`
without verifying that decoded keys are strictly ascending, malformed or
untrusted input can silently construct a `Set` or `Map` with corrupted BST
invariants.

For the next breaking release:
- Add `Ord a` to `instance (Ord a, Binary a) => Binary (Set.Set a)` and
  `Ord k` to `instance (Ord k, Binary k, Binary e) => Binary (Map.Map k e)`.
- Validate that decoded keys are strictly ascending in a single pass while
  decoding (matching `Binary IntSet` and `Binary (IntMap e)`), failing cleanly
  on duplicate or out-of-order keys.

## Deprecated API cleanup

Consider removing long-deprecated functions in a major release:
- `Data.Binary.Get.runGetState`
- `Data.Binary.Get.getBytes`
- `Data.Binary.Get.remaining`

## Primitive-element container `put` (`putElems` / `Prim` path)

`Set`, `Seq`, `Array`, and `UArray` currently encode elements with a strict
`Builder` fold (`foldr (\x acc -> let b = execPut (put x) in b \`seq\` (b <> acc))`),
which still allocates per-element `BuildStep` closures (~180 B/element for
`UArray Int Int`, vs ~11 B/element for `[Int]` using `Prim.primMapListFixed`).
They cannot reuse `putList` directly because `putList` also writes a length
prefix. Adding a lengthless `putElems :: [a] -> Put` (or `putElemsBuilder`)
method to `Binary` in a future release would let containers reuse
`Prim.primMapListFixed` for primitive element types.
