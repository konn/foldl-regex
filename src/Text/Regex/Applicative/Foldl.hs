{-# LANGUAGE BangPatterns #-}
module Text.Regex.Applicative.Foldl
  ( exactMatch, matchAll, matchAllGreedy, matchAllNonGreedy,
    module Text.Regex.Applicative
  ) where
import           Control.Foldl                 (Fold (..))
import           Data.Foldable
import           Data.Maybe
import qualified Data.Sequence                 as Seq
import           Text.Regex.Applicative
import           Text.Regex.Applicative.Object

exactMatch :: RE c a -> Fold c (Maybe a)
exactMatch re = Fold (flip step) (compile re) (listToMaybe . results)

data P a b = P { getFst :: !a, getSnd :: !b }
  deriving (Read, Show, Eq, Ord)

-- | N.B. Unlike 'Text.Regex.Applicative.replace',
--   each empty string matches at most once; i.e.:
--
-- ghci> 'L.fold' (matchAll NonGreedy ('optional' $ 'sym' 'c')) "abccc"
-- >>> [Nothing,Nothing,Nothing,Just 'c',Nothing,Just 'c',Nothing,Just 'c',Nothing]
---
-- ghci> 'L.fold' (matchAll Greedy ('many' $ 'sym' 'c')) "abccc"
-- >>> ["", "", "ccc"]
matchAllNonGreedy :: RE c a -> Fold c [a]
matchAllNonGreedy re = Fold go (P iniMS obj0) (toList . getFst)
  where
    obj0 = compile re
    iniMS = maybe Seq.empty Seq.singleton $ listToMaybe $ results obj0
    go (P ms obj) !c
      | failed obj' = P (ms <> iniMS) obj0
      | not (null res) = P (ms <> Seq.singleton (head res) <> iniMS) obj0
      | otherwise = P ms obj'
      where
        obj' = step c obj
        res = results obj'

matchAllGreedy :: RE c a -> Fold c [a]
matchAllGreedy re = Fold go (P3 Seq.empty iniRes obj0) ext
  where
    ext (P3 ms prev _) = toList $ ms <> maybeSeq' prev
    obj0 = compile re
    iniRes = result' obj0
    go (P3 ms prev obj) !c
      | failed obj' =
        let obj'' = step c obj0
            ms' = ms <> maybeSeq' prev
            next | failed obj'' = obj0
                 | otherwise    = obj''
        in P3 ms' (result' next) next
      | otherwise = P3 ms (result' obj') obj'
      where
        !obj' = step c obj

maybeSeq' :: Maybe' a -> Seq.Seq a
maybeSeq' (Just' a) = Seq.singleton a
maybeSeq' Nothing'  = Seq.empty

result' :: ReObject s a -> Maybe' a
result' = toMaybe' . listToMaybe . results

data P3 a b c = P3 !a !b !c

data Maybe' a = Nothing' | Just' !a

toMaybe' :: Maybe a -> Maybe' a
toMaybe' (Just a) = Just' a
toMaybe' Nothing  = Nothing'

{-# INLINE matchAll #-}
matchAll :: Greediness -> RE c a -> Fold c [a]
matchAll Greedy    = matchAllGreedy
matchAll NonGreedy = matchAllNonGreedy
