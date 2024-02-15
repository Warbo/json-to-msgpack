{-# LANGUAGE BangPatterns #-}

module Writers where

import           Control.Monad.State.Lazy   (get, put, replicateM_, runState, State)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Functor.Identity      (Identity)
import qualified Data.List                  as L
import qualified Data.MessagePack           as MP
import qualified Data.Text                  as T
import qualified Data.Word                  as W
import           Generators
import qualified JSONToMsgPack              as J
import           System.IO                  (SeekMode(..))
import           Test.QuickCheck

-- | Use a zipper for the file handle, so we can wind it forward and backwards.
--   Use a (reversed) string as the output.
type TestM = State ((String, String), String)

instance MonadFail Identity where
  fail = error

writeImp :: J.Imp () TestM
writeImp = J.I {
    J.getchar   = getchar,
    J.lookahead = lookahead,
    J.seek      = seek,

    J.putchar   = putchar,
    J.tell      = tell
  }
  where getchar :: () -> TestM (Maybe Char)
        getchar () = do s <- get
                        case s of
                          ((_ , ""  ), _) -> pure Nothing
                          ((xs, y:ys), z) -> do put ((y:xs, ys), z)
                                                pure (Just y)

        lookahead :: () -> TestM (Maybe Char)
        lookahead () = do s <- get
                          pure (case s of
                                 ((_, "" ), _) -> Nothing
                                 ((_, c:_), _) -> Just c)

        putchar :: Char -> TestM ()
        putchar c = do (h, s) <- get
                       put (h, c:s)

        tell :: () -> TestM Integer
        tell () = do ((pre, _), _) <- get
                     pure (fromIntegral (length pre))

        seek :: () -> SeekMode -> Integer -> TestM ()
        seek () mode n = do ((pre, post), out) <- get
                            put (case mode of
                                  AbsoluteSeek -> absStep n pre post
                                  RelativeSeek -> relStep n pre post
                                  SeekFromEnd  -> error "Shouldn't SeekFromEnd",
                                 out)

        absStep n pre post = let whole = reverse pre ++ post
                                 n'    = fromIntegral n
                              in (reverse (take n' whole), drop n' whole)

        relStep 0 pre post = (pre, post)
        relStep n pre post = case (n > 0, pre, post) of
                               (True , _   , c:cs) -> relStep (n-1) (c:pre) cs
                               (True , _   , _   ) -> error "Seeked too far"
                               (False, c:cs, _   ) -> relStep (n+1) cs (c:post)
                               (False, _   , _   ) -> error "Went back too far"

-- | Creates a state where the given String is immediately after the cursor,
--   runs the given function with this state, then check that the given property
--   holds for the result and output. Some things to note:
--    - We insert arbitrary junk before the cursor, and check that it's still
--      in the zipper after running.
--    - We insert arbitrary junk after the given String, and check that it's
--      still in the zipper after running.
--    - We insert arbitrary junk in the output, and check that it's still there
--      at the beginning after running.
--    - We assert that after running, the cursor is positioned after the given
--      string and before the added junk.
--    - The String we give to the Property function is the output, reversed to
--      counteract the effect of consing one Char at a time. We also remove our
--      initial junk.
runOn :: (J.Imp () TestM -> TestM a)  -- The stateful function to run
      -> (a -> String -> Property)    -- Property of the output to check
      -> String                       -- Input to initialise the state
      -> Property                     -- Checks the given Property and more
runOn func prop str = forAll arbitrary go
  where go args@(pre, post, out, NSL c) =
          let (result, ((pre', post'), out')) = run args
           in pre'  === reverse str ++ pre .&&.  -- Cursor should be after str
              post' === c:post             .&&.  -- Cursor should be before junk
              out `L.isSuffixOf` out'      .&&.  -- Output occurs after junk
              -- Chop out off the end of out'; reverse into correct order
              prop result (reverse (take (length out' - length out) out'))

        -- Set up initial state. We need a non-space-like Char after str to
        -- prevent 'chompSpace
        mkState (pre, post, out, NSL c) = ((pre, str ++ (c:post)), out)

        run = runState (func writeImp) . mkState

-- | Chars which aren't swallowed by spaceEater
newtype NonSpaceLike = NSL Char deriving (Eq, Ord, Show)

instance Arbitrary NonSpaceLike where
  arbitrary = fmap NSL (arbitrary `suchThat` (not . J.spaceLike))

prop_writeFixed :: Int -> Property
prop_writeFixed n = runOn go prop choice
  where go i        = J.writeFixed i ()
        prop () out = length out === 1
        choice      = case abs n `mod` 3 of
                        0 -> "null"
                        1 -> "true"
                        2 -> "false"
                        _ -> undefined

prop_writeString :: Int -> Property
prop_writeString n' = forAll (genString n) (runOn go check)
  where n = abs n' `mod` 1000

        -- Run writeString, but also return the input we were given, to compare
        go :: J.Imp () TestM -> TestM String
        go i       = do ((_, post),  _) <- get
                        J.writeString i ()
                        ((_, post'), _) <- get
                        pure (take (length post - length post') post)

        -- Check the result is parsable and gives the same string
        check s out = let Just (MP.ObjectStr s') = parseMP out
                       in drop 5 out  === stripQuotes s .&&.
                          T.unpack s' === stripQuotes s

prop_writeArray :: Int -> Property
prop_writeArray n' = forAll (genSizedArray n n)
                            (runOn (`J.writeArray` ()) check)
  where n            = abs n' `mod` 1000
        check () out = let Just (MP.ObjectArray got) = parseMP out
                        in length got === n

prop_writeMap :: Int -> Property
prop_writeMap n' = forAll (genObject (abs n' `mod` 1000))
                          (runOn (`J.writeMap` ()) check)
  where check () out = let Just (MP.ObjectMap kvs)  = parseMP out
                        in forceMP (map fst kvs ++ map snd kvs)

prop_writeValue :: Int -> Property
prop_writeValue n' = forAll (genValue (abs n' `mod` 1000))
                            (runOn (`J.writeValue` ()) canParse)
  where canParse () out = let Just got = parseMP out
                           in forceMP [got]

stripQuotes :: String -> String
stripQuotes = go "" . reverse . drop 1 . reverse . drop 1
  where go !acc ""          = reverse acc
        go !acc ('\\':c:cs) = go (c:acc) cs
        go !acc (     c:cs) = go (c:acc) cs

parseLength :: String -> Int
parseLength s = sum (zipWith shift [3, 2, 1, 0] [a, b, c, d])
  where [a, b, c, d] = map fromIntegral (BS.unpack (BSC.pack s))
        shift :: Int -> Int -> Int
        shift p n    = (256^p) * n

prop_parseLength :: W.Word32 -> String -> Property
prop_parseLength w s = runOn go canParse s
  where go :: J.Imp () TestM -> TestM ()
        go i            = do replicateM_ (length s) (J.getchar i ())
                             J.writeInt32 i w

        canParse () out = fromIntegral (parseLength out) === w

parseMP :: String -> Maybe MP.Object
parseMP = MP.unpack . BSL.pack

forceMP :: [MP.Object] -> Property
forceMP []     = property True
forceMP (x:xs) = case x of
                 -- These are the only values we handle. Force their
                 -- contents so we're not just skipping over byte ranges
                 MP.ObjectNil         -> forceMP xs
                 MP.ObjectBool  True  -> forceMP xs
                 MP.ObjectBool  False -> forceMP xs
                 MP.ObjectStr   s     -> total s .&&. forceMP xs
                 MP.ObjectArray ys    -> forceMP (ys ++ xs)
                 MP.ObjectMap   ys    -> forceMP (map fst ys ++ map snd ys
                                                             ++ xs)
                 _                    -> error ("Unexpected " ++ show x)
