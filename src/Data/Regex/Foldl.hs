{-# LANGUAGE ExistentialQuantification, GADTs, NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables             #-}
module Data.Regex.Foldl where
import qualified Control.Foldl        as L
import           Data.DFA
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import qualified Data.IntSet          as IS
import           Data.MonoTraversable
import qualified Data.Vector          as V

nubUnion
  :: [Int] -> [Int] -> [Int]
nubUnion = fmap IS.toList . (IS.union `on` IS.fromList)

unionTrans
  :: (Eq k, Hashable k, Eq k', Hashable k')
  => HM.HashMap k (HM.HashMap k' [Int])
  -> HM.HashMap k (HM.HashMap k' [Int])
  -> HM.HashMap k (HM.HashMap k' [Int])
unionTrans = HM.unionWith (HM.unionWith nubUnion)

data RE c
  = EPS
  | forall mono. (MonoFoldable mono, Element mono ~ c) => SYM mono
  | OPT (RE c)
  | REP (RE c)
  | RE c :<>: RE c
  | NEG (RE c)
  | RE c :|: RE c

compileRE :: (Hashable c, Eq c) => RE c -> NFA Int c
compileRE EPS =
  FA { initial = 0
      , accepteds = (== 0)
      , trans = mempty
      }
compileRE (SYM mono) =
  let s = L.purely ofoldlUnwrap L.vector mono
      n = V.length s
      trans =
        HM.fromList
          [(i, HM.singleton (s V.! i) [i + 1])
          | i <- [0..n-1]]
      accepteds = (== n)
      initial = 0
  in FA{..}
compileRE (l :<>: r) =
  let lfa = invmapState (*2) (`div` 2) $ compileRE l
      rfa = invmapState (succ.(*2)) ((`div` 2).pred) $ compileRE r
      trans' = trans  lfa `unionTrans` trans rfa
      finals = filter (accepteds lfa) $ HM.keys $ trans lfa
      fa0 = FA { initial = initial lfa
               , accepteds = accepteds rfa
               , trans = trans'
               }
  in foldr (`addEpsilonMove` initial rfa) fa0 finals
compileRE (OPT re) =
  let FA{..} = compileRE re
  in FA {accepteds = \b -> accepteds b || b == initial
        , ..
        }

compileRE (l :|: r) =
  let lfa = invmapState ((*2).succ) (pred . (`div` 2)) $ compileRE l
      rfa = invmapState (succ.(*2)) ((`div` 2).pred) $ compileRE r
      trans' = trans lfa `unionTrans` trans rfa
  in addEpsilonMove
        0 (initial rfa)
      $ addEpsilonMove
        0 (initial lfa)
      $ FA { accepteds = \b -> accepteds lfa b || accepteds rfa b
           , initial = 0
           , trans = trans'
           }
compileRE (NEG re) =
  let FA{..} = compileRE re
  in FA { accepteds = not . accepteds
        , ..
        }
compileRE (REP re) =
  let fa = compileRE $ OPT re
      finals = filter (accepteds fa) $ HM.keys $ trans fa
  in foldr (`addEpsilonMove` initial fa) fa finals

match :: (Hashable c, Eq c) => RE c -> L.Fold c Bool
match = isAccepted . compileRE

someFunc :: IO ()
someFunc = putStrLn "someFunc"
