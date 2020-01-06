{-# LANGUAGE BangPatterns, DerivingStrategies, FlexibleContexts, LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms, NamedFieldPuns, RankNTypes                #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeFamilies             #-}
{-# LANGUAGE TypeOperators                                                  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.DFA where
import           Control.Applicative
import qualified Control.Foldl                    as L
import           Control.Lens                     hiding (from, to)
import           Control.Monad.Trans.State.Strict
import           Data.Hashable
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import           Data.Maybe
import           Data.MemoTrie
import           Data.Monoid                      (Ap (..))

-- | Finite-automaton
--   with state @q@ and alphabets @c@
--   with @t@-many transition.
data Automaton t q c =
  FA  { trans   :: q -> c -> t q
      , accepts :: q -> Bool
      , initial :: q
      }

type DFA = Automaton Identity
type NFA = Automaton []

runAutomaton
  :: (Monad t, Eq q, Hashable q, Eq c, Hashable c)
  => Automaton t q c
  -> L.Fold c (t q)
runAutomaton FA{..} = L.Fold step (pure initial) id
  where
    step !qs !c = flip trans c =<< qs

runAutomatonMemo
  :: (Monad t, Eq q, Hashable q, Eq c, Hashable c, HasTrie (t q), HasTrie c)
  => Automaton t q c
  -> L.Fold c (t q)
runAutomatonMemo FA{..} = L.Fold (memo2 step) (pure initial) id
  where
    step !qs !c = flip trans c =<< qs

invmapState
  :: (Functor t, Eq q', Hashable q')
  => (q -> q')
  -> (q' -> q)
  -> Automaton t q  c
  -> Automaton t q' c
invmapState to from FA{..} =
  FA  { initial = to initial
      , trans = fmap (fmap to) . trans . from
      , accepts = accepts . from
      }

invmapEpsilon
  :: (Eq q', Hashable q')
  => (q -> q')
  -> (q' -> q)
  -> EpsilonNFA q  c
  -> EpsilonNFA q' c
invmapEpsilon to from EpsilonNFA{..} =
  EpsilonNFA
    { nfa = invmapState to from nfa
    , epsilonMoves =
        L.foldOver (ifolded.withIndex)
          (lmap (\(k, hs) -> (to k, HS.map to hs) )
          L.hashMap)
        epsilonMoves
    }
maybeToAlt
  :: Alternative t => Maybe a -> t a
maybeToAlt = maybe empty pure

isAccepted
  :: (Monad t, Eq q, Hashable q, Foldable t, Eq c, Hashable c)
  => Automaton t q c
  -> L.Fold c Bool
isAccepted = fmap <$> any . accepts <*> runAutomaton
{-# INLINE isAccepted #-}

instance HasTrie a => HasTrie (Identity a) where
  newtype Identity a :->: b = IdTrie { runIdTrie :: Reg (Identity a) :->: b }
  trie = trieGeneric IdTrie
  untrie = untrieGeneric runIdTrie
  enumerate = enumerateGeneric runIdTrie

isAcceptedMemo
  :: (Monad t, Eq q, HasTrie c, HasTrie (t q),
      Hashable q, Foldable t, Eq c, Hashable c
     )
  => Automaton t q c
  -> L.Fold c Bool
isAcceptedMemo = fmap <$> memo . (any . accepts) <*> runAutomatonMemo
{-# INLINE isAcceptedMemo #-}

data EpsilonNFA q c =
  EpsilonNFA
    { nfa          :: NFA q c
    , epsilonMoves :: HM.HashMap q (HS.HashSet q)
    }

epsilonClosure
  :: (Eq q, Hashable q)
  => EpsilonNFA q c -> q -> HS.HashSet q
epsilonClosure EpsilonNFA{..} =
  HS.fromList . closure (fromMaybe HS.empty . (`HM.lookup` epsilonMoves))

closure :: (Eq a, Hashable a, Foldable t) => (a -> t a) -> a -> [a]
closure f = flip evalState HS.empty . loop
  where
    loop a = do
      hset <- get
      if HS.member a hset
      then pure []
      else do
        modify $ HS.insert a
        (a :) <$> getAp (foldMap (Ap . loop) (f a))

addEpsilonMove
  :: (Eq c, Hashable c, Eq q, Hashable q)
  => q -> q -> EpsilonNFA q c -> EpsilonNFA q c
addEpsilonMove from to
  | from == to = id
  | otherwise = \EpsilonNFA{..} ->
      EpsilonNFA
        { epsilonMoves =
            HM.insertWith HS.union from (HS.singleton to)
            epsilonMoves
        , ..
        }

hoistFA
  :: (t q -> f q)
  -> Automaton t q c -> Automaton f q c
hoistFA nat FA{..} =
  FA { trans = fmap (fmap nat) trans
     , ..
     }

generalize
  :: Applicative t
  => DFA q c -> Automaton t q c
generalize fa =
  FA { accepts = accepts fa
     , initial = initial fa
     , trans = fmap (pure . runIdentity) <$> trans fa
     }

determinise
  :: forall t c q.
      (Foldable t, Monad t, Hashable q)
  => Automaton t q c -> DFA (t q) c
determinise nfa =
  FA { accepts = any (accepts nfa)
     , trans = \qs c -> Identity $ flip (trans nfa) c =<< qs
     , initial = pure $ initial nfa
     }

removeEpsilonMoves
  :: (Hashable q, Eq q) => EpsilonNFA q c -> NFA q c
removeEpsilonMoves ena@EpsilonNFA{..} =
  let trans' = fmap HS.toList
              . foldMap (fmap HS.fromList . trans nfa)
              . epsilonClosure ena
  in nfa { trans = trans'
         , accepts = any (accepts nfa) . epsilonClosure ena
         }
