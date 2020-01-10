{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, TypeApplications #-}
module Main where
import qualified Control.Foldl                as L
import           Data.MonoTraversable
import           Data.Regex.Foldl
import qualified Data.Text                    as T
import           Gauge                        hiding (match)
import qualified Text.Regex.Applicative.Foldl as Ap

default (String)
re' :: Ap.RE Char String
re' = (<>) <$> Ap.many (Ap.sym 'a') <*> (pure <$> Ap.sym 'c')

main :: IO ()
main = defaultMain
  [ env (return (REP (SYM 'c') :<>: SYM 'a')) $
      \ re ->
        bgroup "c*a"
        [ bgroup "success"
          [ env (return $ replicate 100 'c' ++ "a") $
            \ input ->
            bgroup "String/100"
            [ bench "match" $ nf (L.fold $ match re) input
            , bench "matchMemo" $ nf (L.fold $ matchMemo re) input
            , bench "matchNFA" $ nf (L.fold $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.fold $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.fold $ Ap.exactMatch re') input            ]
          , env (return $ replicate 1000 'c' ++ "a") $
            \ input ->
            bgroup "String/1000"
            [ bench "match" $ nf (L.fold $ match re) input
            , bench "matchMemo" $ nf (L.fold $ matchMemo re) input
            , bench "matchNFA" $ nf (L.fold $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.fold $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.fold $ Ap.exactMatch re') input            ]
          , env (return $ replicate 10000 'c' ++ "a") $
            \ input ->
            bgroup "String/10000"
            [ bench "match" $ nf (L.fold $ match re) input
            , bench "matchMemo" $ nf (L.fold $ matchMemo re) input
            , bench "matchNFA" $ nf (L.fold $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.fold $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.fold $ Ap.exactMatch re') input            ]
          , env (return $ T.replicate 100 "c" <> "a") $
            \ input ->
            bgroup "Text/100"
            [ bench "match" $ nf (L.purely ofoldlUnwrap $ match re) input
            , bench "matchMemo" $ nf (L.purely ofoldlUnwrap $ matchMemo re) input
            , bench "matchNFA" $ nf (L.purely ofoldlUnwrap $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.purely ofoldlUnwrap $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.purely ofoldlUnwrap $ Ap.exactMatch re') input
            ]
          , env (return $ T.replicate 1000 "c" <> "a") $
            \ input ->
            bgroup "Text/1000"
            [ bench "match" $ nf (L.purely ofoldlUnwrap $ match re) input
            , bench "matchMemo" $ nf (L.purely ofoldlUnwrap $ matchMemo re) input
            , bench "matchNFA" $ nf (L.purely ofoldlUnwrap $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.purely ofoldlUnwrap $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.purely ofoldlUnwrap $ Ap.exactMatch re') input
            ]
          , env (return $ T.replicate 10000 "c" <> "a") $
            \ input ->
            bgroup "Text/10000"
            [ bench "match" $ nf (L.purely ofoldlUnwrap $ match re) input
            , bench "matchMemo" $ nf (L.purely ofoldlUnwrap $ matchMemo re) input
            , bench "matchNFA" $ nf (L.purely ofoldlUnwrap $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.purely ofoldlUnwrap $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.purely ofoldlUnwrap $ Ap.exactMatch re') input
            ]
          ]
      , bgroup "fail"
          [ env (return $ replicate 100 'c' ++ "z") $
            \ input ->
            bgroup "String/100"
            [ bench "match" $ nf (L.fold $ match re) input
            , bench "matchMemo" $ nf (L.fold $ matchMemo re) input
            , bench "matchNFA" $ nf (L.fold $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.fold $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.fold $ Ap.exactMatch re') input            ]
          , env (return $ replicate 1000 'c' ++ "z") $
            \ input ->
            bgroup "String/1000"
            [ bench "match" $ nf (L.fold $ match re) input
            , bench "matchMemo" $ nf (L.fold $ matchMemo re) input
            , bench "matchNFA" $ nf (L.fold $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.fold $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.fold $ Ap.exactMatch re') input            ]
          , env (return $ replicate 10000 'c' ++ "z") $
            \ input ->
            bgroup "String/10000"
            [ bench "match" $ nf (L.fold $ match re) input
            , bench "matchMemo" $ nf (L.fold $ matchMemo re) input
            , bench "matchNFA" $ nf (L.fold $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.fold $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.fold $ Ap.exactMatch re') input            ]
          , env (return $ T.replicate 100 "c" <> "z") $
            \ input ->
            bgroup "Text/100"
            [ bench "match" $ nf (L.purely ofoldlUnwrap $ match re) input
            , bench "matchMemo" $ nf (L.purely ofoldlUnwrap $ matchMemo re) input
            , bench "matchNFA" $ nf (L.purely ofoldlUnwrap $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.purely ofoldlUnwrap $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.purely ofoldlUnwrap $ Ap.exactMatch re') input
            ]
          , env (return $ T.replicate 1000 "c" <> "z") $
            \ input ->
            bgroup "Text/1000"
            [ bench "match" $ nf (L.purely ofoldlUnwrap $ match re) input
            , bench "matchMemo" $ nf (L.purely ofoldlUnwrap $ matchMemo re) input
            , bench "matchNFA" $ nf (L.purely ofoldlUnwrap $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.purely ofoldlUnwrap $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.purely ofoldlUnwrap $ Ap.exactMatch re') input
            ]
          , env (return $ T.replicate 10000 "c" <> "z") $
            \ input ->
            bgroup "Text/10000"
            [ bench "match" $ nf (L.purely ofoldlUnwrap $ match re) input
            , bench "matchMemo" $ nf (L.purely ofoldlUnwrap $ matchMemo re) input
            , bench "matchNFA" $ nf (L.purely ofoldlUnwrap $ matchNFA re) input
            , bench "matchNFAMemo" $ nf (L.purely ofoldlUnwrap $ matchNFAMemo re) input
            , bench "Ap.exactMatch @Maybe" $ whnf (L.purely ofoldlUnwrap $ Ap.exactMatch re') input
            ]
          ]
      ]
  ]
