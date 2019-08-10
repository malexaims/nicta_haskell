{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.

anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams s fp = (intersectBy equalIgnoringCase (permutations s)) <$> lines <$> readFile fp

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase (c1:.s1) (c2:.s2)
  | (toLower c1) == (toLower c2) = equalIgnoringCase s1 s2
  | otherwise = False
equalIgnoringCase Nil Nil = True
equalIgnoringCase Nil _ = False
equalIgnoringCase _ Nil = False
