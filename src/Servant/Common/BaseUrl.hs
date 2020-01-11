{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Common.BaseUrl (
  -- * types
    BaseUrl (..)
  , Scheme (..)
  -- * functions
  , baseUrlWidget
  , showBaseUrl

  -- * constraints
  , SupportsServantReflex
) where

import           Control.Monad                     (join)
import           Data.Aeson
import           Control.Monad.Fix                 (MonadFix)
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Generics
import           Language.Javascript.JSaddle.Monad (MonadJSM)
import           Reflex
import           Reflex.Dom.Core
import           Text.Read


type SupportsServantReflex t m = (Reflex t, TriggerEvent t m, PerformEvent t m, HasWebView (Performable m), MonadJSM (Performable m))

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Read, Eq, Ord, Generic)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseFullUrl Scheme Text Int Text
             | BasePath Text
  deriving (Ord, Read, Show, Generic)

instance Eq BaseUrl where
    BasePath s == BasePath s' = s == s'
    BaseFullUrl a b c path == BaseFullUrl a' b' c' path'
        = a == a' && b == b' && c == c' && s path == s path'
        where s x = if T.isPrefixOf "/" x then T.tail x else x
    _ == _ = False

instance FromJSON BaseUrl where
  parseJSON = withText "Servant.Reflex.BaseUrl" $ \t ->
    case parseBaseUrl t of
      Left msg -> fail (T.unpack msg)
      Right v  -> pure v

parseBaseUrl :: Text -> Either Text BaseUrl
parseBaseUrl str =
    case T.stripPrefix "http://" str of
      Just suffix -> parseBaseFullUrl Http suffix
      Nothing ->
        case T.stripPrefix "https://" str of
          Just suffix -> parseBaseFullUrl Https suffix
          Nothing     -> Right (BasePath str)

parseBaseFullUrl :: Scheme -> Text -> Either Text BaseUrl
parseBaseFullUrl scheme str =
    if T.null portAndPath
    then Left "port must be specified"
    else case port' of
      Nothing -> Left "port must be valid Int"
      Just p  -> Right $ BaseFullUrl scheme host p path
  where
    (host, portAndPath) = T.breakOn ":" str
    (port, path) = T.breakOn "/" (T.drop 1 portAndPath)
    port' = readMaybe (T.unpack port)

showBaseUrl :: BaseUrl -> Text
showBaseUrl (BasePath s) = s
showBaseUrl (BaseFullUrl urlscheme host port path) =
  schemeString <> "//" <> host <> (portString </> path)
    where
      a </> b = if "/" `T.isPrefixOf` b || T.null b then a <> b else a <> "/" <> b
      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"
      portString = case (urlscheme, port) of
        (Http, 80)   -> ""
        (Https, 443) -> ""
        _            -> ":" <> T.pack (show port)

baseUrlWidget :: forall t m .(SupportsServantReflex t m,
                              DomBuilderSpace m ~ GhcjsDomSpace,
                              MonadFix m,
                              PostBuild t m,
                              MonadHold t m,
                              DomBuilder t m)
              => m (Dynamic t BaseUrl)
baseUrlWidget = elClass "div" "base-url" $ do
  urlWidget <- dropdown (0 :: Int) (constDyn $ 0 =: "BasePath" <> 1 =: "BaseUrlFull") def
  let bUrlWidget = ffor (value urlWidget) $ \i -> case i of
        0 -> pathWidget
        1 -> fullUrlWidget
        _ -> error "Surprising value"
  join <$> widgetHold pathWidget (updated bUrlWidget)
  where pathWidget :: m (Dynamic t BaseUrl)
        pathWidget = do
          text "Url base path"
          t <- textInput (def {_textInputConfig_attributes =
                          constDyn ("placeholder" =: "/a/b")})
          return $ BasePath <$> value t
        fullUrlWidget :: m (Dynamic t BaseUrl)
        fullUrlWidget = do
          schm <- dropdown Https (constDyn $ Https =: "https" <> Http =: "http") def
          srv  <- textInput def {_textInputConfig_attributes = constDyn $ "placeholder" =: "example.com"}
          text ":"
          prt  <- textInput def { _textInputConfig_attributes = constDyn $ "placeholder" =: "80"}
          port :: Dynamic t Int <- holdDyn 80 (fmapMaybe (readMaybe . T.unpack) $ updated (value prt))
          path <- textInput def { _textInputConfig_attributes = constDyn $ "placeholder" =: "a/b" }
          return $ BaseFullUrl <$> value schm <*> value srv <*> port <*> value path
