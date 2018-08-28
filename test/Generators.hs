{-# LANGUAGE BangPatterns #-}
module Generators where

import           Control.Monad   (replicateM)
import qualified Data.Char       as C
import           Data.List       (intercalate)
import           Test.QuickCheck

genSpaces :: Gen String
genSpaces = do n <- choose (1, 1000)
               replicateM n (elements " \t\n\r,:")

genString :: Int -> Gen String
genString n = do s <- go (abs n) ""
                 pure ("\"" ++ s ++ "\"")
  where go 0 !acc = pure acc
        go m !acc = do c <- arbitrary `suchThat` (\x -> C.isAscii x &&
                                                        not (C.isControl x))
                       go (m-1) (if c `elem` "\"\\"
                                    then '\\':c:acc
                                    else      c:acc)

wrapArray :: [String] -> String
wrapArray xs = "[" ++ intercalate ", " xs ++ "]"

genSizedArray :: Int -> Int -> Gen String
genSizedArray len fuel = do l  <- spreadFuel fuel
                            xs <- mapM genValue (take len l)
                            pure (wrapArray xs)

genArray :: Int -> Gen String
genArray = go [] . abs
  where go acc n | n < 2 = pure (wrapArray acc)
        go acc n         = do x <- choose (0, n - 1)
                              v <- genValue x
                              go (v:acc) (n - x)

wrapObject :: [(String, String)] -> String
wrapObject xs = "{" ++ intercalate ", " (map pair xs) ++ "}"
  where pair (k, v) = k ++ ": " ++ v

genObject :: Int -> Gen String
genObject = go [] . abs
  where go acc n | n < 2 = pure (wrapObject acc)
        go acc n         = do x <- choose (0, n - 1)
                              k <- genString (x + 1)  -- Need non-empty name
                              v <- genValue   x
                              go ((k, v):acc) (n - x)

genSizedObject :: Int -> Int -> Gen String
genSizedObject len fuel = do l  <- spreadFuel fuel
                             ks <- mapM genString (take len l)
                             vs <- mapM genValue  (take len l)
                             pure (wrapObject (zip ks vs))

genValue :: Int -> Gen String
genValue fuel = do c <- choose (1, 6)
                   case (c :: Int) of
                     1 -> pure "null"
                     2 -> pure "true"
                     3 -> pure "false"
                     4 -> genString (fuel - 1)
                     5 -> genArray  (fuel - 1)
                     6 -> genObject (fuel - 1)
                     _ -> undefined

spreadFuel :: Int -> Gen [Int]
spreadFuel fuel = do l <- go fuel
                     pure (l ++ repeat 0)
  where go 0 = pure []
        go n = do x  <- choose (0, abs n)
                  xs <- go (n - x)
                  pure (x:xs)
