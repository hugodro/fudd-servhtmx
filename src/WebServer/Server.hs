{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebServer.Server where

import Control.Concurrent.Async (concurrently_)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (runContT, ContT (..), Cont)
import Control.Monad.Except (ExceptT, MonadError, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Control.Exception.Safe (tryAny)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Int as DI
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.List.NonEmpty (NonEmpty (..))

import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), (.:), (.:?), eitherDecode)

import Network.Wai.Handler.Warp as Wrp
import Network.Wai.Parse (setMaxRequestKeyLength, defaultParseRequestBodyOptions)
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Servant.Errors (errorMw)
import qualified Network.WebSockets as WS

import GHC.Generics
import GHC.Stack (HasCallStack)
import Servant as Srv
import Servant.API.Generic
import Servant.API.ContentTypes (FormUrlEncoded)
import Servant.Auth.Server (Auth, AuthResult (..), BasicAuth, BasicAuthCfg, CookieSettings (CookieSettings, cookieIsSecure)
                  , IsSecure (NotSecure), FromBasicAuthData, JWT, JWTSettings, FromJWT (..), ToJWT (..), cookieIsSecure
                  , defaultCookieSettings, defaultJWTSettings )
import qualified Servant.Auth.Server as Sauth
import Servant.Multipart (defaultMultipartOptions, MultipartOptions (..), Tmp)
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.WebSocket (WebSocket)
import Web.FormUrlEncoded (FromForm (..))

import System.Posix.Signals as Sgnl

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as A
import qualified Text.Blaze.Html.Renderer.Utf8 as H

import WebServer.JWT (readJWK, generateKeyPairIO)
import WebServer.CorsPolicy (setCorsPolicy)
import Options.Runtime as Ropt

import Pages.Page_1 (demoPage, demoSearch, demoReply)
import qualified Pages.MockData as Mk
import qualified Network.WebSockets as Ws


listen :: Ropt.RunOptions -> IO ()
listen rtOpts = do
  let
    fakeContT = ContT $ bracket (fakeContFct "dummy.") fakeEndFct
  runContT fakeContT finalAction
  where
  finalAction dummy = do
    let shutdownCallback = putStrLn "@[finalAction] empty termination callback."
        settings = setupWai rtOpts.webServer.port shutdownCallback
    webHandling <- runAPI rtOpts
    Wrp.runSettings settings webHandling

  fakeContFct :: [a] -> IO Int
  fakeContFct l = return (length l)

  fakeEndFct :: Int -> IO ()
  fakeEndFct aNum = pure ()


setupWai :: Int -> IO () -> Settings
setupWai port shutdownCallback =
  Wrp.setPort port . Wrp.setGracefulShutdownTimeout (Just 5) . Wrp.setInstallShutdownHandler shutdownHandler
    . setBeforeMainLoop showBanner
    $ Wrp.defaultSettings
  where
    showBanner =
      putStrLn $ "@[setupWai] using port: " <> show port
    shutdownHandler closeSocket = do
      void $ installHandler Sgnl.sigTERM (Catch $ shutdownCallback >> closeSocket) Nothing
      void $ installHandler Sgnl.sigINT (Catch $ shutdownCallback >> closeSocket) Nothing
      void $ installHandler Sgnl.sigQUIT (Catch $ shutdownCallback >> closeSocket) Nothing
      void $ installHandler Sgnl.sigHUP (Catch $ shutdownCallback >> closeSocket) Nothing


data AppEnv = AppEnv {
    jwtSettings :: JWTSettings
  , rtOptions :: Ropt.RunOptions
  , mockProjects :: [ Mk.Project ]
  }

runAPI ::  Ropt.RunOptions -> IO Application
runAPI rtOpts = do
  myKey <- case rtOpts.jwkConfFile of
    Nothing ->
      generateKeyPairIO "/tmp/jwk.json"
    Just aPath ->
      readJWK aPath

  let
    cookieCfg = defaultCookieSettings { cookieIsSecure = NotSecure }
    jwtDefSettings  = Sauth.defaultJWTSettings myKey
    -- For file upload support, will be used later:
    multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Tmp)) { 
          generalOptions = setMaxRequestKeyLength 512 defaultParseRequestBodyOptions
      }

    runContext = cookieCfg :. jwtDefSettings :. fakeSessionValidation :. multipartOpts :. EmptyContext
    runCtxtProxy = Proxy :: Proxy '[CookieSettings, JWTSettings, BasicAuthCfg]

    middlewares = linkUp $ id :| case rtOpts.corsPolicy of
      Nothing -> [ logStdout, errorMw @JSON @'["message", "status" ] ]
      Just aPolicy -> [ logStdout, setCorsPolicy aPolicy, errorMw @JSON @'["message", "status" ] ]

    appEnv = AppEnv { jwtSettings = jwtDefSettings, rtOptions = rtOpts, mockProjects = Mk.projectList }
    server = hoistServerWithContext serverApiProxy runCtxtProxy (toHandler appEnv) serverApiT

  pure $ middlewares $ serveWithContext serverApiProxy runContext server
  where
    linkUp :: NonEmpty (a -> a) -> a -> a
    linkUp = foldr1 (.)


-- Route definitions:
type ServerApi = ToServantApi ServerRoutes


data ServerRoutes route = ServerRoutes {
    anonymous :: route :- ToServantApi AnonymousRoutes
    , authenticated :: route :- Auth '[JWT, Sauth.BasicAuth] SessionContext :> ToServantApi AuthenticatedRoutes
  }
  deriving (Generic)


newtype AuthenticatedRoutes route = AuthenticatedRoutes { 
    getPage :: route :- ToServantApi GetPageRoutes
  }
  deriving (Generic)

newtype SearchContent = SearchContent {
    search :: DT.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm)


data AnonymousRoutes route = AnonymousRoutes { 
    login :: route :- "inlogin" :> ReqBody '[JSON] LoginForm :> Post '[JSON] LoginResult
    , staticLink :: route :- "xstatic" :> Capture "path" String :> Get '[HTML] RawHtml
    , stream :: route :- "stream" :> WebSocket
    , search :: route :- "xsearch" :> ReqBody '[FormUrlEncoded] SearchContent :> Post '[HTML] RawHtml
    , homePage :: route :- Get '[HTML] RawHtml
  }
  deriving (Generic)


newtype GetPageRoutes route = GetPageRoutes { 
    getPage :: route :- "private" :> Capture "path" String :> Get '[HTML] RawHtml
  }
  deriving (Generic)


-- Handler definitions:
serverApiProxy :: Proxy ServerApi
serverApiProxy = Proxy


serverApiT :: ToServant ServerRoutes (AsServerT WebApp)
serverApiT =
  genericServerT $ ServerRoutes {
    anonymous = anonHandlers
    , authenticated = authHandlers
  }


authHandlers :: AuthResult SessionContext -> ToServant AuthenticatedRoutes (AsServerT WebApp)
authHandlers authResult =
  genericServerT $ AuthenticatedRoutes {
    getPage = getPageHandler authResult
  }


getPageHandler :: AuthResult SessionContext -> String -> WebApp RawHtml
getPageHandler authResult pageUrl =
  let
    comment = case authResult of
      Authenticated context ->
        DT.pack . show $ context.sessionID
      _ ->
        "Unauthorized."
  in
    pure . RawHtml . H.renderHtml $ testPage comment


testPage :: DT.Text -> H.Html
testPage aComment = H.docTypeHtml $ do
  H.head $ do
    H.title "TEST"
  H.body $ do
    H.h1 "Doing a test..."
    H.p $ do
      H.toHtml aComment

basicPage :: H.Html
basicPage = H.docTypeHtml $ do
  H.head $ do
    H.title "TEST"
  H.body $ do
    H.h1 "Doing a test..."
    H.p $ do
      H.toHtml ("Hello World!" :: String)

anonHandlers :: ToServant AnonymousRoutes (AsServerT WebApp)
anonHandlers =
  genericServerT $ AnonymousRoutes {
    login = loginHandler
    , staticLink = staticHandler
    , stream = streamHandler
    , homePage = homePageHandler
    , search = searchHandler
  }


loginHandler :: LoginForm -> WebApp LoginResult
loginHandler form@LoginForm {..} = do
  settings <- asks jwtSettings
  jwtRez <- liftIO $ Sauth.makeJWT (SessionContext 1) settings Nothing
  case jwtRez of
    Left err ->
      throwError . UnexpectedError . DT.pack $ show err
    Right jwtValue ->
      pure $ LoginResult {
          context = SessionContext 1
          , jwt = decodeUtf8 . LBS.toStrict $ jwtValue
        }


staticHandler :: String -> WebApp RawHtml
staticHandler pageUrl = do
  let
    tmpStr = encodeUtf8 . DT.pack $ pageUrl
  pageContent <- liftIO $ LBS.readFile ("xstatic/" <> pageUrl)
  pure . RawHtml $ pageContent


streamHandler :: MonadIO m => WS.Connection -> m ()
streamHandler conn = do
  liftIO $ WS.withPingThread conn 30 (pure ()) $ do
    -- liftIO $ WS.sendTextData conn ("<div id=\"notifications\" hx-swap-oob=\"beforeend\">Some message</div?" :: ByteString)
    handleClient
  where
    handleClient = do
      rezA <- tryAny $ forever receiveStream
      case rezA of
        Left err -> do
          liftIO . putStrLn $ "@[streamHandler] situation: " <> show err
          closeConnection
        Right _ -> do
          liftIO $ putStrLn "@[streamHandler] client disconnected."
          pure ()

    receiveStream = do
      rezA <- WS.receiveDataMessage conn
      case rezA of
        WS.Text msg decodedMsg ->
          let
            hxMsg = eitherDecode msg :: Either String HxWsMessage
          in
          case hxMsg of
            Left err -> do
              putStrLn $ "@[receiveStream] invalid HxWsMessage: " <> (DT.unpack . decodeUtf8 . LBS.toStrict) msg
              putStrLn $ "@[receiveStream] error: " <> show err
            Right hxMsg ->
              Ws.sendTextData conn $ H.renderHtml $ demoReply hxMsg.wsMessage
        WS.Binary msg ->
          putStrLn "@[receiveStream] received binary."
    
    closeConnection = do
      WS.sendClose conn ("Bye" :: ByteString)
      void $ WS.receiveDataMessage conn


data HxWsHeaders = HxWsHeaders {
    request :: DT.Text
    , trigger :: DT.Text
    , triggerName :: Maybe DT.Text
    , target :: DT.Text
    , currentURL :: DT.Text
  }
  deriving stock (Show, Generic)

instance FromJSON HxWsHeaders where
  parseJSON (Object obj) = HxWsHeaders <$>
    obj .: "HX-Request"
    <*> obj .: "HX-Trigger"
    <*> obj .:? "HX-Trigger-Name"
    <*> obj .: "HX-Target"
    <*> obj .: "HX-Current-URL"


data HxWsMessage = HxWsMessage {
    wsMessage :: DT.Text
    , headers :: HxWsHeaders
  }
  deriving (Show, Generic)


instance FromJSON HxWsMessage where
  parseJSON (Object obj) = HxWsMessage <$>
    obj .: "ws-message"
    <*> obj .: "HEADERS"


homePageHandler :: WebApp RawHtml
homePageHandler = do
  pure . RawHtml . H.renderHtml $ demoPage "Premier demo." []


searchHandler :: SearchContent -> WebApp RawHtml
searchHandler SearchContent {..} = do
  projects <- asks mockProjects
  pure . RawHtml . H.renderHtml $ demoSearch projects search


newtype WebApp a = WebApp { 
    runApp :: ReaderT AppEnv (ExceptT ServerApiError IO) a
  }
  deriving newtype (
    Functor, Applicative, Monad, MonadMask, MonadCatch, MonadThrow
    , MonadReader AppEnv, MonadIO, MonadError ServerApiError
  )


data LoginResult = LoginResult {
    context :: SessionContext
    , jwt :: DT.Text
  }
  deriving stock Generic
  deriving anyclass (ToJSON)

data LoginForm = LoginForm {
  username :: DT.Text
  , password :: DT.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


data HTML = HTML
newtype RawHtml = RawHtml { rawContent :: LBS.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = rawContent


newtype SessionContext = SessionContext {
    sessionID :: DI.Int32
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

instance ToJWT SessionContext
instance FromJWT SessionContext
type instance BasicAuthCfg = Srv.BasicAuthData -> IO (AuthResult SessionContext)
instance FromBasicAuthData SessionContext where
  fromBasicAuthData authData authCheckFun = authCheckFun authData


data ServerApiError
  = NotImplemented
  | UnexpectedError DT.Text
  | NotAuthorized DT.Text
  | Unaccessible
  | NotFound DT.Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


toServerError :: ServerApiError -> ServerError
toServerError err =
  case err of
    Unaccessible -> err401 { errBody = "Resource not accessible" }
    NotImplemented -> err500 { errBody = "NotImplemented" }
    UnexpectedError x -> err500 { errBody = LBS.fromStrict . encodeUtf8 $ x }
    NotAuthorized x -> err401 { errBody = LBS.fromStrict . encodeUtf8 $ x }
    NotFound x -> err404 { errBody = LBS.fromStrict . encodeUtf8 $ x }



-- | Natural transformations between 'App' and 'Handler' monads
toHandler :: AppEnv -> WebApp a -> Srv.Handler a
toHandler e =
  Handler . withExceptT toServerError . flip runReaderT e . runApp


fakeSessionValidation :: BasicAuthData -> IO (AuthResult SessionContext)
fakeSessionValidation _ =
  pure $ Authenticated $ SessionContext 1
