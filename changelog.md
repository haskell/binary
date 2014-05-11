binary
======


binary-0.7.2.0
-----------

- Add `isolate :: Int -> Get a -> Get a`.
- Add `label :: String -> Get a -> Get a`.

binary-0.7.1.0
--------------

- Add `lookAheadE :: Get (Either a b) -> Get (Either a b)`.
- Add MonadPlus instance for Get. 


binary-0.7.0.1
--------------

- Updates to documentation.

binary-0.7.0.0
--------------

- Add `lookAhead :: Get a -> Get a`.
- Add `lookAheadM :: Get (Maybe a) -> Get (Maybe a)`.
- Add Alternative instance for Get (provides `<|>`).
- Add `decodeOrFail :: Binary a => L.ByteString -> Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, a)`
- Add `decodeFileOrFail :: Binary a => FilePath -> IO (Either (ByteOffset, String) a)`.
- Remove `Ord` class constraint from `Set` and `Map` Binary instances.

binary-0.6.4
------------

- Add `runGetOrFail :: Get a -> L.ByteString -> Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, a)`.

binary-0.6.3
------------

- Documentation tweeks, internal restructuring, more tests.

binary-0.6.2
------------

- `some` and `many` more efficient.
- Fix bug where `bytesRead` returned the wrong value.
- Documentation improvements.

binary-0.6.1
------------

- Fix bug where a decoder could return with `Partial` after the previous reply was `Nothing`.

binary-0.6.0.0
--------------
