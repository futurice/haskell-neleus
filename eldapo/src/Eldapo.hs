{-# LANGUAGE DeriveGeneric #-}
module Eldapo where

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
                print (x :: T.LDAPMessage)
                connLazySendAll conn (encode (searchResDone i))
                loop conn (succ i) lbs'
