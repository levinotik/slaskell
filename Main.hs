{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.Trans.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (pack)
import Data.Aeson
import Control.Applicative (empty)
import qualified Network.Wreq as W
import Network.Wreq (defaults)
import Control.Lens
import System.Environment (lookupEnv)
import System.Exit (die)

data RequestInvite = RequestInvite {
      name :: Text
    , lastname :: Text
    , email :: Text
    , message :: Text
    } deriving Show

instance FromJSON RequestInvite where
    parseJSON (Object v) = RequestInvite <$>
                           v .: "name" <*>
                           v .: "lastname" <*>
                           v .: "email" <*>
                           v .: "message"
    parseJSON _          = empty

main :: IO ()
main = do
  slackToken <- lookupEnv "SLACK_TOKEN"
  case slackToken of
    Just token -> scottyMain token
    _          -> die "env var slack_token is not defined"


scottyMain :: String -> IO ()
scottyMain token = scotty 3000 $ do
    get "/" $ do
      indexHtml <- lift $ pack <$> readFile "index.html"
      html indexHtml
    post "/request-invite" $ do
      inviteRequest <- jsonData :: ActionM RequestInvite
      let opts = defaults & W.param "token" .~ [T.pack token] & W.param "channel" .~ ["invites"] & W.param "text" .~ [T.pack $ show inviteRequest]
      r <- lift (W.getWith opts "https://slack.com/api/chat.postMessage")
      text (pack $ show inviteRequest)