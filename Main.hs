{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import           Network.Wreq (defaults)
import qualified Network.Wreq as W
import           System.Environment (lookupEnv)
import           System.Exit (die)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Email.Validate (EmailAddress, validate)
import           Web.Scotty

type Token = Text

data Response
  = Notified
  | InvalidInput Text
  | ErrorOccurred Text
  deriving Show

main :: IO ()
main = do
  slackToken <- lookupEnv "SLACK_TOKEN"
  case slackToken of
    Just token -> scottyMain (T.pack token)
    _          -> die "env var SLACK_TOKEN is not defined"


scottyMain :: Token -> IO ()
scottyMain token = scotty 3000 $ do

  get "/health" $ text "ok"
  get "/" $ file "index.html"
  get "/style.css" $ file "style.css"

  post "/request-invite" $ do
    emailAddress <- param "email"
    case validateEmail emailAddress of
      Left e -> do
        html (renderHtml (resultHtml emailAddress (InvalidInput (T.pack e))))
      Right _ -> do
        result <- lift (sendMessage token emailAddress)
        html (renderHtml (resultHtml emailAddress result))

validateEmail :: Text -> Either String EmailAddress
validateEmail emailAddress | T.length emailAddress > 512 = Left "email too long"
                           | otherwise = validate (encodeUtf8 emailAddress)

resultHtml :: Text -> Response -> H.Html
resultHtml emailAddress result = H.docTypeHtml $ do
  H.head $ do
    H.title title
    H.link ! A.href "style.css" ! A.rel "stylesheet"
  H.body $ do
    H.div ! A.class_ "splash" $ do
      H.h1 $ H.toHtml message
      H.p $ do
        H.toHtml ("Requested email: " :: Text)
        H.a ! A.href (H.toValue $ "mailto:" `T.append` emailAddress) $ H.toHtml emailAddress
      H.p $ do
        H.a ! A.href "/" $ "Request another invite?"

  where (title, message) = case result of
                  Notified        -> ("Green across the board",  "Alright, we'll get in touch soon!")
                  InvalidInput e  -> ("Oops!", "Malformed email: " `T.append` e)
                  ErrorOccurred e -> ("Oops!", "Error: " `T.append` e)

sendMessage :: Token -> Text -> IO Response
sendMessage token emailAddress = do
  let opts = defaults
           & W.param "channel" .~ ["#invites"]
           & W.param "token"   .~ [token]
           & W.param "text"    .~ [emailAddress]
  apiResponse <- W.getWith opts "https://slack.com/api/chat.postMessage"
  pure (parseResponse (apiResponse ^. W.responseBody))


parseResponse :: ByteString -> Response
parseResponse apiResponse =
  fromMaybe (ErrorOccurred "Unexpected error: Malformed JSON Response") $ do
    responseJson <- decode apiResponse :: Maybe Object
    flip parseMaybe responseJson $ \o -> do
      ok <- o .: "ok"
      if ok
        then pure Notified
        else o .: "error" >>= pure . ErrorOccurred
