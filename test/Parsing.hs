{-# LANGUAGE BangPatterns #-}
module Parsing where

import           Control.Monad.State.Lazy
import qualified JSONToMsgPack            as J
import           Test.QuickCheck

testImp :: J.Imp () (State String)
testImp = J.I {
    J.getchar   = getchar,
    J.lookahead = lookahead
  }
  where getchar :: () -> State String (Maybe Char)
        getchar () = do s <- get
                        case s of
                          ""   -> pure Nothing
                          c:cs -> do put cs
                                     pure (Just c)

        lookahead :: () -> State String (Maybe Char)
        lookahead () = do s <- get
                          case s of
                            ""   -> pure Nothing
                            c:_ -> pure (Just c)

prop_chompSpaces :: Property
prop_chompSpaces = forAll genSpaces chomped
  where chomped s = run s === ((), "")
        run       = runState (J.chompSpace' testImp ())

prop_stringOfCorrectLength :: Int -> Property
prop_stringOfCorrectLength n' = forAll (genString n) lengthMatch
  where n             = abs n' `mod` 1000
        lengthMatch s = run s === (n, "")
        run           = runState (J.stringLength' testImp ())

genSpaces :: Gen String
genSpaces = do n <- choose (1, 1000)
               replicateM n (elements " \t\n\r,:")

genString :: Int -> Gen String
genString n = do s <- go (abs n) ""
                 pure ("\"" ++ s ++ "\"")
  where go 0 !acc = pure acc
        go m !acc = do c <- arbitrary
                       go (m-1) (if c `elem` "\"\\"
                                    then '\\':c:acc
                                    else      c:acc)
