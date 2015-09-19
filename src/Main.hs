{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Http.Server

import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import Data.ByteString
import Data.String.Utils
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
  command <- getParam "command" >>= \x -> case x of
    Just c -> return c
    Nothing -> return ""
  userId <- getParam "user_id" >>= \x -> case x of
    Just u -> return u
    Nothing -> return ""
  reasonTxt <- getParam "text" >>= \x -> case x of
    Just r -> return r
    Nothing -> return ""
    
  case command of
    "/brb" -> markUserAway userId reasonTxt
    "/back" -> markUserBack userId
    _ -> writeBS "Unrecognised Command"
    
    -- maybe (writeBS "must specify command")
    --       writeBS command

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

