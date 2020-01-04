{-# LANGUAGE BangPatterns, DerivingStrategies, LiberalTypeSynonyms #-}
{-# LANGUAGE NamedFieldPuns, RankNTypes, RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables                                   #-}
module Data.DFA where
import           Control.Applicative
import           Control.Arrow
import qualified Control.Foldl             as L
import           Control.Lens              hiding (from, to)
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Hashable
import qualified Data.HashMap.Lazy         as HM
import qualified Data.HashSet              as HS
import           Data.Maybe

-- | Finite-automaton
--   with state @q@ and alphabets @c@
--   with @t@-many transition.
data Automaton t q c =
  FA  { trans     :: q -> HM.HashMap c (t q)
      , accepteds :: HS.HashSet q
      , initial   :: q
      }

type DFA = Automaton Identity
type NFA = Automaton []

runAutomaton
  :: (Monad t, Alternative t, Eq q, Hashable q, Eq c, Hashable c)
  => Automaton t q c
  -> L.Fold c (t q)
runAutomaton FA{..} = L.Fold step (pure initial) id
  where
    step !qs !c = join . maybeToAlt . HM.lookup c . trans =<< qs

invmapState
  :: (Functor t, Eq q', Hashable q')
  => (q -> q')
  -> (q' -> q)
  -> Automaton t q  c
  -> Automaton t q' c
invmapState to from FA{..} =
  FA  { initial = to initial
      , trans = fmap (fmap to) . trans . from
      , accepteds = HS.map to accepteds
      }

maybeToAlt
  :: Alternative t => Maybe a -> t a
maybeToAlt = maybe empty pure

isAccepted
  :: (Monad t, Eq q, Hashable q, Foldable t, Eq c, Hashable c)
  => Automaton t q c
  -> L.Fold c Bool
isAccepted = (fmap <$> there . accepteds <*> runAutomaton)
  . hoistFA (MaybeT . fmap Just)
  where
    {-# INLINE there #-}
    there qs =
      any (maybe False (`HS.member` qs)) . runMaybeT
{-# INLINE isAccepted #-}

addEpsilonMove
  :: (Eq c, Hashable c, Alternative f, Eq q, Hashable q)
  => q -> q -> Automaton f q c -> Automaton f q c
addEpsilonMove from to fa@FA{..}
  | from == to = fa
  | otherwise =
      let trans' q
            | q == from = HM.unionWith (<|>) (trans q) (trans to)
            | otherwise = trans q
          fin | from `HS.member` accepteds = HS.insert from accepteds
              | otherwise = accepteds
      in FA{trans = trans', accepteds = fin, ..}

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
  FA { accepteds = accepteds fa
     , initial = initial fa
     , trans = fmap (pure . runIdentity) <$> trans fa
     }

determinise
  :: forall t c q.
      (Foldable t, Monad t, Hashable (t q))
  => Automaton t c q -> DFA c (t q)
determinise nfa =
  FA { accepteds = any (accepteds nfa)
     , trans = liftTq $ trans nfa
     , initial = pure $ initial nfa
     }
  where
    liftTq :: (q -> (HM.HashMap (Identity c) (t q)))
           -> t q -> (HM.HashMap (Identity c) (t (t q)))
    liftTq dic = _u

-- removeEpsilonMove
--   :: EpsilonAutomaton t c q -> Automaton Identity t c q
-- removeEpsilonMove = undefined
