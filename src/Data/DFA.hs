{-# LANGUAGE BangPatterns, DerivingStrategies, LiberalTypeSynonyms #-}
{-# LANGUAGE NamedFieldPuns, RankNTypes, RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables                                   #-}
module Data.DFA where
import           Control.Applicative
import qualified Control.Foldl       as L
import           Control.Lens        hiding (from, to)
import           Data.Hashable


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

maybeToAlt
  :: Alternative t => Maybe a -> t a
maybeToAlt = maybe empty pure

isAccepted
  :: (Monad t, Eq q, Hashable q, Foldable t, Eq c, Hashable c)
  => Automaton t q c
  -> L.Fold c Bool
isAccepted = fmap <$> any . accepts <*> runAutomaton
{-# INLINE isAccepted #-}

addEpsilonMove
  :: (Eq c, Hashable c, Alternative f, Eq q, Hashable q)
  => q -> q -> Automaton f q c -> Automaton f q c
addEpsilonMove from to
  | from == to = id
  | otherwise = addEpsilonMoveP (== from) to

addEpsilonMoveP
  :: (Eq c, Hashable c, Alternative f, Eq q, Hashable q)
  => (q -> Bool) -> q -> Automaton f q c -> Automaton f q c
addEpsilonMoveP isFrom to fa@FA{..}
  | isFrom to = fa
  | otherwise =
      let trans' q c
            | isFrom q = trans q c <|> trans to c
            | otherwise = trans q c
          fin | accepts to = \q -> isFrom q || accepts q
              | otherwise = accepts
      in FA{trans = trans', accepts = fin, ..}

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

-- removeEpsilonMove
--   :: EpsilonAutomaton t c q -> Automaton Identity t c q
-- removeEpsilonMove = undefined
