{-# LANGUAGE BangPatterns, ConstraintKinds #-}

module JSONToMsgPack where

import           Control.Exception
import           Control.Monad         (replicateM_)
import qualified Data.Bits             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char             as C
import qualified Data.Word             as W
import           System.Environment    (getArgs)
import           System.IO
import           System.IO.Error       (isEOFError)

data Imp handle m = I {
    getchar   :: handle -> m (Maybe Char),
    lookahead :: handle -> m (Maybe Char),
    seek      :: handle -> SeekMode -> Integer -> m (),

    putchar   :: Char   -> m (),
    tell      :: handle -> m Integer
  }

maybeEOF :: IO Char -> IO (Maybe Char)
maybeEOF f = do x <- try f
                case x of
                  Right c                -> pure (Just c)
                  Left  e | isEOFError e -> pure Nothing
                  Left  e                -> throw e

io :: Imp Handle IO
io = I {
    getchar   = maybeEOF . hGetChar,
    lookahead = maybeEOF . hLookAhead,
    seek      = hSeek,

    putchar   = putChar,
    tell      = hTell
  }

spaceLike :: Char -> Bool
spaceLike c = C.isSpace c || elem c ",:"

-- | Seek the given handle forwards until it's at a nonspace char. We treat ','
--   and ':' as space.
spaceEater :: Monad m => Imp handle m -> handle -> m ()
spaceEater i h = do
  x <- lookahead i h
  case x of
    Nothing              -> pure ()
    Just c | spaceLike c -> getchar i h >> spaceEater i h
    _                    -> pure ()

putBytes :: Monad m => Imp handle m -> [W.Word8] -> m ()
putBytes i = mapM_ (putchar i) . BSC.unpack . BS.pack

-- | Read a JSON string from h, count how many bytes it will take to represent
--   as MsgPack. In particular a \-escape in JSON takes 2 bytes (the \ and the
--   char we're escaping); we don't need the \ in MsgPack, so we save 1 byte.
stringLength :: Handle -> IO Int
stringLength = stringLength' io

stringLength' :: Monad m => Imp handle m -> handle -> m Int
stringLength' i h = do x <- getchar i h
                       case x of
                         Just '"' -> go 0
                         Just c   -> error ("Expected string, hit " ++ show c)
                         Nothing  -> error "Expected string, hit EOF"

  where go !n = do x <- getchar i h
                   case x of
                     Nothing   -> error "Unterminated string"
                     Just '\\' -> do y <- getchar i h
                                     case y of
                                       Nothing -> error "Can't escape EOF"
                                       Just  _ -> go (n+1)  -- Ignore \ and skip
                     Just '"'  -> pure n
                     Just _    -> go (n+1)

fixedLength :: Handle -> IO Int
fixedLength = fixedLength' io

fixedLength' :: Monad m => Imp handle m -> handle -> m Int
fixedLength' i h = do x <- getchar i h
                      let n = case x of
                                Nothing  -> error "Unexpected EOF"
                                Just 'n' -> 3
                                Just 't' -> 3
                                Just 'f' -> 4
                                Just c   -> error ("Unexpected char " ++ show c)
                      seek i h RelativeSeek (fromIntegral n)
                      pure (n+1)

arrayLength :: Handle -> IO Int
arrayLength = arrayLength' io

arrayLength' :: Monad m => Imp handle m -> handle -> m Int
arrayLength' i h = do x <- getchar i h
                      case x of
                        Nothing  -> error "Expected array, not EOF"
                        Just '[' -> go 0
                        Just c   -> error ("Expected array, not " ++ show c)
  where go !n = do spaceEater i h
                   c <- lookahead i h
                   case c of
                     Nothing  -> error "EOF reading array"
                     Just ']' -> getchar    i h >> pure n
                     Just _   -> skipValue' i h >> go (n+1)

objectLength :: Handle -> IO Int
objectLength = objectLength' io

objectLength' :: Monad m => Imp handle m -> handle -> m Int
objectLength' i h = do x <- getchar i h
                       case x of
                         Nothing  -> error "EOF reading object"
                         Just '{' -> go 0
                         Just c   -> error ("Expected '{' not " ++ show c)
  where go !n = do spaceEater i h
                   c <- lookahead i h
                   case c of
                     Nothing  -> error "Unterminated object"
                     Just '}' -> getchar    i h >> pure n
                     Just _   -> skipValue' i h >> spaceEater i h >>
                                 skipValue' i h >> go (n+1)

skipValue :: Handle -> IO ()
skipValue = skipValue' io

skipValue' :: Monad m => Imp handle m -> handle -> m ()
skipValue' i h = do x <- lookahead i h
                    _ <- case x of
                           Nothing  -> error "Expected value, hit EOF"
                           Just '[' ->  arrayLength' i h
                           Just '{' -> objectLength' i h
                           Just '"' -> stringLength' i h
                           Just _   ->  fixedLength' i h
                    pure ()

-- | Write a 32bit word as bytes in big-endian order
writeInt32 :: Monad m => Imp handle m -> W.Word32 -> m ()
writeInt32 i n = putBytes i bytes
  where bytes :: [W.Word8]
        bytes = [fromIntegral (n `B.shiftR` 24),
                 fromIntegral (n `B.shiftR` 16),
                 fromIntegral (n `B.shiftR` 8 ),
                 fromIntegral  n               ]

-- | Read a JSON object from h and write the equivalent MsgPack to stdout.
writeMap :: Monad m => Imp handle m -> handle -> m ()
writeMap i h = do
  startPos <- tell          i h
  n        <- objectLength' i h
  seek i h AbsoluteSeek (startPos + 1)  -- Swallow the '{'

  putBytes   i [0xdf]                                     -- Tag
  writeInt32 i (fromIntegral n)                           -- Len
  replicateM_ (2*n) (writeValue i h >> spaceEater i h)  -- Ks+vs
  x <- getchar i h  -- Ready for the next value
  case x of
    Just '}' -> pure ()
    _        -> undefined
  pure ()

-- | Read a JSON keyword (null, true or false) from h and write the equivalent
--   MsgPack to stdout.
writeFixed :: Monad m => Imp handle m -> handle -> m ()
writeFixed i h = do x <- lookahead i h
                    let (b, n) = case x of
                                   Just 'n' -> (0xc0, 4)  -- null
                                   Just 'f' -> (0xc2, 5)  -- false
                                   Just 't' -> (0xc3, 4)  -- true
                                   Just c   -> error ("Unexpected char " ++
                                                      show c)
                                   Nothing  -> error "Hit EOF"
                    putBytes i [b]
                    seek i h RelativeSeek n
                    spaceEater i h

-- | Read a JSON array from h and write the equivalent MsgPack to stdout.
writeArray :: Monad m => Imp handle m -> handle -> m ()
writeArray i h = do
  startPos <- tell         i h
  n        <- arrayLength' i h
  seek i h AbsoluteSeek (startPos + 1)  -- Swallow the '['

  putBytes   i [0xdd]                                -- Tag
  writeInt32 i (fromIntegral n)                      -- Length
  replicateM_ n (writeValue i h >> spaceEater i h)  -- Elements
  Just ']' <- getchar i h -- Ready for the next value
  pure ()

-- | Read a JSON string from h and write the equivalent MsgPack to stdout.
writeString :: Monad m => Imp handle m -> handle -> m ()
writeString i h = do startPos <- tell i h
                     n        <- stringLength' i h
                     seek i h AbsoluteSeek (startPos + 1)  -- Swallow the '"'

                     putBytes   i [0xdb]
                     writeInt32 i (fromIntegral n)
                     go n
                     Just '"' <- getchar i h
                     pure ()
  where go 0 = pure ()
        go n = do x <- getchar i h
                  case x of
                    Just '\\' -> do y <- getchar i h
                                    case y of
                                      Just c  -> putchar i c >> go (n-1)
                                      Nothing -> error "Unexpected EOF"
                    Just c    ->                putchar i c >> go (n-1)
                    Nothing   -> error "Unexpected EOF"

-- | Reads a single JSON value from h, possibly prefixed by whitespace and/or
--   ',' or ':'. Writes the equivalent MsgPack to stdout.
writeValue :: Monad m => Imp handle m -> handle -> m ()
writeValue i h = do spaceEater     i h
                    c <- lookahead i h
                    case c of
                      Just '{' -> writeMap    i h
                      Just '[' -> writeArray  i h
                      Just '"' -> writeString i h
                      Just _   -> writeFixed  i h
                      Nothing  -> error "Unexpected EOF"

main :: IO ()
main = do [f] <- getArgs
          withFile f ReadMode (writeValue io)
