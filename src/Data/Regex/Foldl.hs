{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies            #-}
{-# LANGUAGE ExistentialQuantification, GADTs, LambdaCase, NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeFamilies           #-}
{-# LANGUAGE TypeOperators                                                #-}
module Data.Regex.Foldl where
import           Control.Arrow
import           Control.DeepSeq
import qualified Control.Foldl        as L
import           Data.DFA
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Data.IntSet          as IS
import           Data.MemoTrie
import           Data.MonoTraversable
import qualified Data.Vector          as V
import           GHC.Generics         (Generic)

nubUnion
  :: [Int] -> [Int] -> [Int]
nubUnion = fmap IS.toList . (IS.union `on` IS.fromList)

unionTrans
  :: (Eq k, Hashable k, Eq k', Hashable k')
  => (k -> k' -> [Int])
  -> (k -> k' -> [Int])
  -> k -> k' -> [Int]
{-# INLINE unionTrans #-}
unionTrans f g k k' = f k k' `nubUnion` g k k'

data RE c
  = EPS
  | forall mono. (MonoFoldable mono, Element mono ~ c) => STR mono
  | SYM c
  | OPT (RE c)
  | REP (RE c)
  | RE c :<>: RE c
  | RE c :|: RE c

instance NFData c => NFData (RE c) where
  rnf EPS        = ()
  rnf (STR str)  = rnf $ otoList str
  rnf (SYM c)    = rnf c
  rnf (OPT re)   = rnf re
  rnf (REP re)   = rnf re
  rnf (l :<>: r) = rnf l `seq` rnf r
  rnf (l :|: r)  = rnf l `seq` rnf r

sumRENFA
  :: EpsilonNFA (REPath Int) c
  -> EpsilonNFA (REPath Int) c
  -> EpsilonNFA (REPath Int) c
sumRENFA l r =
  EpsilonNFA
    {nfa =
      FA { initial = Initial
         , accepts = \case
            Final -> True
            _ -> False
         , trans = \case
            L q -> fmap L . trans (nfa l) q
            R q -> fmap R . trans (nfa r) q
            _   -> mempty
        }
    , epsilonMoves =
        HM.union
          (HM.fromList
          $ map (L *** HS.map L)
          $ HM.toList
          $ epsilonMoves l)
          (HM.fromList
          $ map (R *** HS.map R)
          $ HM.toList
          $ epsilonMoves r)
    }

compileRE' :: (Hashable c, Eq c) => RE c -> EpsilonNFA (REPath Int) c
compileRE' EPS =
  let nfa = FA { initial = Initial
               , accepts = \case
                   Final   -> True
                   _       -> False
               , trans = mempty
               }
      epsilonMoves = HM.singleton Initial $ HS.singleton Final
  in EpsilonNFA{..}
compileRE' (SYM c) =
  let nfa = FA { initial = Initial
              , trans = \case
                  Initial -> \c' -> [Final | c == c']
                  _ -> mempty
              , accepts = \case
                  Final -> True
                  _     -> False
              }
      epsilonMoves = mempty
  in EpsilonNFA {..}
compileRE' (STR mono) =
  let s = L.purely ofoldlUnwrap L.vector mono
      trans Initial c
        | Just c == s V.!? 0 = [Q 1]
      trans (Q i) c
        | i == V.length s - 1 && c == V.last s = [Final]
        | Just c == s V.!? i = [Q $ i + 1]
      trans _ _ = []
      accepts = (== Final)
      initial = Initial
      nfa = FA{..}
      epsilonMoves = mempty
  in EpsilonNFA{..}
compileRE' (l :<>: r) =
  let fa0 = sumRENFA (compileRE' l) (compileRE' r)
      unRFinal = \case
        R Final -> Final
        q -> q
  in fa0
      { nfa =
          FA { trans = fmap (map unRFinal) .
                (\case
                    Initial -> trans (nfa fa0) $ L Initial
                    L Final -> trans (nfa fa0) $ R Initial
                    q -> trans (nfa fa0) q
                )
             , initial = Initial
             , accepts = \case {Final -> True; _ -> False}
             }
      }
compileRE' (OPT re) =
  let efa = invmapEpsilon L (\case { ~(L a) -> a})
          $ compileRE' re
  in addEpsilonMove
      Initial Final
    $ addEpsilonMove
      (L Final) Final
    $ addEpsilonMove
      Initial (L Initial)
      efa

compileRE' (l :|: r) =
  let fa0 = sumRENFA (compileRE' l) (compileRE' r)
  in addEpsilonMove
        (L Final) Final
      $ addEpsilonMove
        (R Final) Final
      $ addEpsilonMove
        Initial (L Initial)
      $ addEpsilonMove
        Initial (R Initial)
        fa0
compileRE' (REP re) =
  let fa = invmapEpsilon L (\case {L a -> a; q -> q}) $ compileRE' re
  in  addEpsilonMove  (L Final) (L Initial)
    $ addEpsilonMove Initial Final
    $ addEpsilonMove (L Final) Final
    $ addEpsilonMove Initial (L Initial)
      fa { nfa = (nfa fa) { initial = Initial, accepts = (== Final) }}

compileRE :: (Hashable c, Eq c) => RE c -> NFA (REPath Int) c
compileRE = removeEpsilonMoves . compileRE'

match :: (Hashable c, Eq c) => RE c -> L.Fold c Bool
match = isAccepted . determinise . compileRE

matchMemo :: (Hashable c, Eq c, HasTrie c) => RE c -> L.Fold c Bool
matchMemo = isAcceptedMemo . determinise . compileRE

matchNFA :: (Hashable c, Eq c) => RE c -> L.Fold c Bool
matchNFA = isAccepted . compileRE

matchNFAMemo :: (HasTrie c, Hashable c, Eq c) => RE c -> L.Fold c Bool
matchNFAMemo = isAcceptedMemo . compileRE

data REPath a
  = Q a
  | Initial
  | Final
  | L (REPath a)
  | R (REPath a)
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

instance HasTrie a => HasTrie (REPath a) where
  newtype REPath a :->: v =
    REPathTrie { runREPathTrie :: Reg (REPath a) :->: v }
  trie = trieGeneric REPathTrie
  untrie = untrieGeneric runREPathTrie
  enumerate = enumerateGeneric runREPathTrie

someFunc :: IO ()
someFunc = putStrLn "someFunc"
