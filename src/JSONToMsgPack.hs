{-# LANGUAGE BangPatterns, ConstraintKinds #-}

module JSONToMsgPack where

import           Control.Exception
import           Control.Monad      (replicateM_)
import qualified Data.Bits          as B
import qualified Data.ByteString    as BS
import qualified Data.Char          as C
import qualified Data.Word          as W
import           System.Environment (getArgs)
import           System.IO
import           System.IO.Error    (isEOFError)

data Imp handle m = I {
    getchar   :: handle -> m (Maybe Char),
    lookahead :: handle -> m (Maybe Char)
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
    lookahead = maybeEOF . hLookAhead
  }

-- | Seek the given handle forwards until it's at a nonspace char. We treat ','
--   and ':' as space.
chompSpace' :: Monad m => Imp handle m -> handle -> m ()
chompSpace' i h = do
  x <- lookahead i h
  case x of
    Nothing                             -> pure ()
    Just c | C.isSpace c || elem c ",:" -> getchar i h >> chompSpace' i h
    _                                   -> pure ()

chompSpace :: Handle -> IO ()
chompSpace = chompSpace' io

putBytes :: [W.Word8] -> IO ()
putBytes = BS.putStr . BS.pack

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
fixedLength h = do c <- hGetChar h
                   let n = case c of
                             'n' -> 3
                             't' -> 3
                             'f' -> 4
                             _   -> error ("Fixed char " ++ show c)
                   hSeek h RelativeSeek (fromIntegral n)
                   pure (n+1)

arrayLength :: Handle -> IO Int
arrayLength h = do '[' <- hGetChar h
                   go 0
  where go !n = do chompSpace h
                   c <- hLookAhead h
                   case c of
                     ']' -> hGetChar  h >> pure n
                     _   -> skipValue h >> go (n+1)

objectLength :: Handle -> IO Int
objectLength h = do '{' <- hGetChar h
                    go 0
  where go !n = do chompSpace h
                   c <- hLookAhead h
                   case c of
                     '}' -> hGetChar  h >> pure n
                     _   -> skipValue h >> chompSpace h >>
                            skipValue h >> go (n+1)

skipValue :: Handle -> IO ()
skipValue h = do c <- hLookAhead h
                 _ <- case c of
                        '[' ->  arrayLength h
                        '{' -> objectLength h
                        '"' -> stringLength h
                        _   ->  fixedLength h
                 pure ()

-- | Write a 32bit word as bytes in big-endian order
writeInt32 :: W.Word32 -> IO ()
writeInt32 n = putBytes bytes
  where bytes :: [W.Word8]
        bytes = [fromIntegral (n `B.shiftR` 24),
                 fromIntegral (n `B.shiftR` 16),
                 fromIntegral (n `B.shiftR` 8 ),
                 fromIntegral  n             ]

-- | Read a JSON object from h and write the equivalent MsgPack to stdout.
writeMap :: Handle -> IO ()
writeMap h = do startPos <- hTell        h
                n        <- objectLength h
                hSeek h AbsoluteSeek (startPos + 1)  -- Swallow the '{'

                putBytes [0xdf]                                   -- Tag
                writeInt32 (fromIntegral n)                       -- Length
                replicateM_ (2*n) (writeValue h >> chompSpace h)  -- Keys & vals
                '}' <- hGetChar h  -- Ready for the next value
                pure ()

-- | Read a JSON keyword (null, true or false) from h and write the equivalent
--   MsgPack to stdout.
writeFixed :: Handle -> IO ()
writeFixed h = do c <- hLookAhead h
                  let (b, n) = case c of
                                 'n' -> (0xc0, 4)  -- null
                                 'f' -> (0xc2, 5)  -- false
                                 't' -> (0xc3, 4)  -- true
                                 _   -> error ("Unexpected char " ++ show c)
                  putBytes [b]
                  hSeek h RelativeSeek n
                  chompSpace h

-- | Read a JSON array from h and write the equivalent MsgPack to stdout.
writeArray :: Handle -> IO ()
writeArray h = do startPos <- hTell       h
                  n        <- arrayLength h
                  hSeek h AbsoluteSeek (startPos + 1)  -- Swallow the '['

                  putBytes [0xdd]                               -- Tag
                  writeInt32 (fromIntegral n)                   -- Length
                  replicateM_ n (writeValue h >> chompSpace h)  -- Elements
                  ']' <- hGetChar h -- Ready for the next value
                  pure ()

-- | Read a JSON string from h and write the equivalent MsgPack to stdout.
writeString :: Handle -> IO ()
writeString h = do startPos <- hTell h
                   n        <- stringLength h
                   hSeek h AbsoluteSeek (startPos + 1)  -- Swallow the '"'

                   putBytes [0xdb]
                   writeInt32 (fromIntegral n)
                   go n
                   '"' <- hGetChar h
                   pure ()
  where go 0 = pure ()
        go n = do c <- hGetChar h
                  case c of
                    '\\' -> hGetChar h >>= putChar   >> go (n-1)
                    _    ->                putChar c >> go (n-1)

-- | Reads a single JSON value from h, possibly prefixed by whitespace and/or
--   ',' or ':'. Writes the equivalent MsgPack to stdout.
writeValue :: Handle -> IO ()
writeValue h = do chompSpace h
                  c <- hLookAhead h
                  case c of
                    '{' -> writeMap    h
                    '[' -> writeArray  h
                    '"' -> writeString h
                    _   -> writeFixed  h

main :: IO ()
main = do [f] <- getArgs
          withFile f ReadMode writeValue
