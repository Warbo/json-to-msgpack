module Lengths where

import           Control.Monad.State.Lazy (get, put, runState, State)
import           Generators
import qualified JSONToMsgPack            as J
import           System.IO                (SeekMode(..))
import           Test.QuickCheck

-- | Represent state as a zipper, so we can wind it forward and backwards
type TestM = State (String, String)

readImp :: J.Imp () TestM
readImp = J.I {
    J.getchar   = getchar,
    J.lookahead = lookahead,
    J.seek      = seek,
    J.putchar   = error "Shouldn't putchar when reading",
    J.tell      = error "Reader's tell not implemented"
  }
  where getchar :: () -> TestM (Maybe Char)
        getchar () = do s <- get
                        case s of
                          (_, "")    -> pure Nothing
                          (xs, y:ys) -> do put (y:xs, ys)
                                           pure (Just y)

        lookahead :: () -> TestM (Maybe Char)
        lookahead () = do s <- get
                          case s of
                            (_, "" ) -> pure Nothing
                            (_, c:_) -> pure (Just c)

        seek :: () -> SeekMode -> Integer -> TestM ()
        seek () mode n = do (pre, post) <- get
                            put (case mode of
                                  AbsoluteSeek -> absStep n pre post
                                  RelativeSeek -> relStep n pre post
                                  SeekFromEnd  -> error "Shouldn't SeekFromEnd")

        absStep n pre post = let whole = reverse pre ++ post
                                 n'    = fromIntegral n
                              in (reverse (take n' whole), drop n' whole)

        relStep 0 pre post = (pre, post)
        relStep n pre post = case (n > 0, pre, post) of
                               (True , _   , c:cs) -> relStep (n-1) (c:pre) cs
                               (True , _   , _   ) -> error "Seeked too far"
                               (False, c:cs, _   ) -> relStep (n+1) cs (c:post)
                               (False, _   , _   ) -> error "Went back too far"

prop_chompSpaces :: Property
prop_chompSpaces = forAll genSpaces chomped
  where chomped s = let ((), (_, state)) = run ("", s)
                     in state === ""
        run       = runState (J.spaceEater readImp ())

prop_stringOfCorrectLength :: Int -> Property
prop_stringOfCorrectLength n' = forAll (genString n) lengthMatch
  where n             = abs n' `mod` 1000
        lengthMatch s = let (result, (_, state)) = run ("", s)
                         in result === n .&&. state === ""
        run           = runState (J.stringLength' readImp ())

prop_fixedLength :: Int -> Property
prop_fixedLength n = let (result, (pre, post)) = run ("", choice)
                      in result === length  choice .&&.
                         pre    === reverse choice .&&.
                         post   === ""
  where choice = case abs (n `mod` 3) of
                   0 -> "null"
                   1 -> "true"
                   2 -> "false"
                   _ -> undefined
        run = runState (J.fixedLength' readImp ())

prop_arrayLength :: Int -> Property
prop_arrayLength n' = forAll (genSizedArray n n) lengthMatches
  where n               = abs n' `mod` 1000
        lengthMatches s = let (result, (pre, post)) = run ("", s)
                           in result === n         .&&.
                              pre    === reverse s .&&.
                              post   === ""
        run             = runState (J.arrayLength' readImp ())

prop_objectLength :: Int -> Property
prop_objectLength n' = forAll (genSizedObject n n) lengthMatches
  where n               = abs n' `mod` 1000
        lengthMatches s = let (result, (pre, post)) = run ("", s)
                           in result === n         .&&.
                              pre    === reverse s .&&.
                              post   === ""
        run             = runState (J.objectLength' readImp ())
