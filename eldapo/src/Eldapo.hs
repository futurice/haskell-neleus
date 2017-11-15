{-# LANGUAGE DeriveGeneric #-}
module Eldapo where

import GHC.Generics   (Generic)
import Neleus
import Network.Arcola (Connection, connLazyRead, connLazySendAll, run)

import qualified Data.ByteString.Lazy as LBS

import qualified Data.ByteString.Base16.Lazy as LBS16

import qualified Eldapo.Types as T

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
        | LBS.null lbs = print "DONE"
        | otherwise = case decode stepDER schema lbs of
            Left err        -> do
                print "ERROR"
                print err
            Right (x, lbs') -> do
                print (x :: T.LDAPMessage Req)
                connLazySendAll conn (encode (searchResDone i))
                loop conn (succ i) lbs'

data Req
    = Bind T.BindRequest
    | Unbind T.UnbindRequest
    | Search T.SearchRequest
    | Unknown ASN1Value
  deriving (Show, Generic)

instance ASN1 Req where
    schema = choice $
        option "bindRequest" :*
        option "unbindRequest" :*
        option "searchRequest" :*
        option "unknown" :*
        Nil
