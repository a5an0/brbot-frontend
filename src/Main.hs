{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Http.Server

import           Database.SQLite.Simple
import Data.ByteString
import Data.String
import Data.String.Utils
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

data AwayField = AwayField
                 { uid :: String
                 , reason ::String
                 } deriving (Show)

instance FromRow AwayField where
  fromRow = AwayField <$> field <*> field

instance ToRow AwayField where
  toRow (AwayField uid_ reason_) = toRow (uid_, reason_)


main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = ifTop slashHandler

slashHandler :: Snap ()
slashHandler = do
  command <- liftM unpackParam $ getParam "command"
  userId <- liftM unpackParam $ getParam "user_id" 
  reasonTxt <- liftM unpackParam $ getParam "text"
  case command of
    "/brb" -> markUserAway userId reasonTxt
    "/back" -> markUserBack userId
    _ -> writeBS "Unrecognised Command"
    

unpackParam :: Data.String.IsString t => Maybe t -> t
unpackParam (Just x) = x
unpackParam Nothing = ""

markUserAway :: ByteString -> ByteString -> Snap()
markUserAway userId reasonTxt = do
  conn <- liftIO $ open "/tmp/brbot.sqlite3"
  liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS away (uid STRING PRIMARY KEY, reason TEXT)"
  liftIO $ execute conn "INSERT INTO away (uid, reason) VALUES (?,?)" (AwayField (replace "\"" "" $ show userId) (show reasonTxt))
  liftIO $ close conn
  writeBS "See you later"

markUserBack :: ByteString -> Snap()
markUserBack userId = do
  conn <- liftIO $ open "/tmp/brbot.sqlite3"
  liftIO $ execute conn "DELETE FROM away WHERE uid=(?)" (Only (replace "\"" "" $ show userId))
  liftIO $ close conn
  writeBS "Welcome back!"

