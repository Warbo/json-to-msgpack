{-# LANGUAGE BangPatterns #-}

module Writers where

import           Control.Monad.State.Lazy   (get, put, runState, State)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
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

prop_writeFixed :: Int -> Property
prop_writeFixed n = pre  === reverse choice .&&.
                    post === ""             .&&.
                    1    === length out
  where ((), ((pre, post), out)) = run (("", choice), "")
        choice                   = case abs n `mod` 3 of
                                     0 -> "null"
                                     1 -> "true"
                                     2 -> "false"
                                     _ -> undefined
        run                      = runState (J.writeFixed writeImp ())

prop_writeString :: Int -> Property
prop_writeString n' = forAll (genString n) checkResult
  where n             = abs n' `mod` 1000
        checkResult s = let ((), ((pre, post), out)) = run s
                            Just (MP.ObjectStr s') = parseMP (reverse out)
                         in drop 5 (reverse out) === stripQuotes s .&&.
                            pre                  === reverse s     .&&.
                            post                 === ""            .&&.
                            T.unpack s'          === stripQuotes s
        run         s = runState (J.writeString writeImp ()) (("", s), "")

prop_writeArray :: Int -> Property
prop_writeArray n' = forAll (genSizedArray n n) checkResult
  where n             = abs n' `mod` 1000
        checkResult s = let ((), ((pre, post), out)) = run s
                         in pre  === reverse s .&&.
                            post === ""        .&&.
                            case parseMP (reverse out) of
                              Just (MP.ObjectArray got) -> length got === n
                              Just x                    -> error (show x)
                              Nothing                   -> error "No parse"
        run         s = runState (J.writeArray writeImp ()) (("", s), "")

prop_writeMap :: Int -> Property
prop_writeMap n' = forAll (genObject n) checkResult
  where n             = abs n' `mod` 1000
        checkResult s = let ((), ((pre, post), out)) = run s
                            Just (MP.ObjectMap kvs)  = parseMP (reverse out)
                         in pre  === reverse s .&&.
                            post === ""        .&&.
                            forceMP (map fst kvs ++ map snd kvs)
        run s         = runState (J.writeMap writeImp ()) (("", s), "")

prop_writeValue :: Int -> Property
prop_writeValue n' = forAll (genValue n) canParse
  where n          = abs n' `mod` 1000
        canParse s = let ((), ((pre, post), out)) = run s
                         Just got                 = parseMP (reverse out)
                      in counterexample (show (("pre" , reverse pre),
                                               ("post", post       ),
                                               ("out" , out        ),
                                               ("got" , got        ))) $
                         pre  === reverse s .&&.
                         post === ""        .&&.
                         forceMP [got]
        run s        = runState (J.writeValue writeImp ()) (("", s), "")

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

prop_parseLength :: W.Word32 -> Property
prop_parseLength w = pre    === "" .&&.
                     post   === "" .&&.
                     parsed === w
  where ((), ((pre, post), out)) = runState (J.writeInt32 writeImp w)
                                            (("", ""), "")
        parsed                   = fromIntegral (parseLength (reverse out))

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
