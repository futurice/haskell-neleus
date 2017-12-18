{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Eldapo where

import Prelude ()
import Prelude.Compat

import Data.Foldable      (for_)
import Data.List.NonEmpty (NonEmpty (..))
import Neleus
import Network.Arcola     (Connection, connLazyRead, connLazySendAll, run)

import qualified Data.ByteString.Base16.Lazy as LBS16
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBS8
import qualified Data.Set                    as Set
import qualified Eldapo.Types                as T

defaultMain :: IO ()
defaultMain = do
    putStrLn "Start"
    run 9999 $ \conn -> do
        putStrLn "Connected"
        lbs <- connLazyRead conn
        loop conn 1 lbs
        LBS.putStr (LBS16.encode lbs)
        putChar '\n'
  where
    loop :: Connection -> Integer -> LBS.ByteString -> IO ()
    loop conn i lbs
        | LBS.null lbs = putStrLn "DONE"
        | otherwise = case decode stepDER schema lbs of
            Left err        -> do
                putStrLn "ERROR"
                putStrLn err
            Right (x, lbs') -> do
                ys <- interactLDAP i x
                for_ ys $ \y -> do
                    let e = encode schema y
                    putStrLn $ "<<< " ++ LBS8.unpack (LBS16.encode e)
                    connLazySendAll conn e
                loop conn (succ i) lbs'

interactLDAP :: Integer -> T.LDAPMessage -> IO (NonEmpty T.LDAPMessage)
interactLDAP i x@(T.LDAPMessage _ op _) = do
    print x
    case op of
        Z (I br) -> do
            y <- handleBindRequest br
            return $ return $ T.LDAPMessage i (S (Z (I y))) Nothing
        S (S (S (Z (I sr)))) -> do
            ys <- handleSearchRequest sr
            return $ snoc (fmap wrapEntry ys) (searchResDone i)
        -- rest: return something :)
        _        -> return $ return $ searchResDone i
  where
    handleBindRequest :: T.BindRequest -> IO T.BindResponse
    handleBindRequest _ = return $ T.BindResponse
        { T.brCode             = T.RCSuccess
        , T.brMatchedDN        = T.LDAPDN ""
        , T.brErrorMessage     = T.LDAPString ""
        , T.brReferral         = Nothing
        , T.brServerSaslsCreds = Nothing
        }

    handleSearchRequest :: T.SearchRequest -> IO [T.SearchResultEntry]
    handleSearchRequest _ = pure
        [ T.SearchResultEntry (T.LDAPDN "dn=root")
            [ T.PartialAttribute "cn" (Set.singleton "toor")
            ]
        ]

    wrapEntry :: T.SearchResultEntry -> T.LDAPMessage
    wrapEntry e = T.LDAPMessage i (S . S . S . S . Z . I $ e) Nothing

snoc :: [a] -> a -> NonEmpty a
snoc []       a = a :| []
snoc (x : xs) a = x :| (xs ++ [a])

searchResDone :: Integer -> T.LDAPMessage
searchResDone i = T.LDAPMessage i (S . S . S . S . S . Z . I $ msg) Nothing where
    msg = T.SearchResultDone
        { T.srdCode         = T.RCSuccess
        , T.srdMatchedDN    = T.LDAPDN ""
        , T.srdErrorMessage = T.LDAPString ""
        , T.srdReferral     = Nothing
        }
