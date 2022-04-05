{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Network.App.CRM (

          Username
        , Password
        , Login
        , PageResponse (..)
        , Statics (..)
        , URI
        , Actions (..)
        , start, startPaths

) where

--------------------------------------------------------------------------------

-- Package: base.
import Data.Maybe
import qualified GHC.Generics as GG
-- Package: Cabal.
#ifdef UsePathsCache
import PathsCache_introspection (getDataFileContent)
#else
import Paths_introspection (getDataFileName)
#endif
-- Package: bytestring.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
-- Package: text.
import qualified Data.Text as Text
-- Package: time.
import qualified Data.Time as Time
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: introspection.
import qualified Network.HTTP.Cookie as Cookie
import qualified Network.HTTP.Request as Request
import qualified Network.HTTP.Response as Response
import qualified Network.HTTP.Server as Server
import qualified Network.HTTP.Session as Session
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Version as Version

--------------------------------------------------------------------------------

type Username = BS.ByteString

type Password = BS.ByteString

data Session = Session {
          sessionHost :: Maybe BS.ByteString
        , sessionPath :: Maybe BS.ByteString
        -- | The ssesion object creation time.
        , sessionCreationTime :: Time.UTCTime
        -- | The URI's request that created the ssesion.
        , sessionFirstRequestUri :: BS.ByteString
        -- | The user last request time.
        , sessionLastRequestTime :: Time.UTCTime
        , isUserLoggedIn ::Bool
} deriving (Show, Read, Eq)

type Login = Maybe (Username -> Password -> IO Bool)

data PageRequest = PageRequest {
          pageRequestName :: Text.Text
        , pageRequestParams :: Maybe Aeson.Value
} deriving (Show, Eq, GG.Generic)

instance Aeson.FromJSON PageRequest

data PageResponse = PageResponse {
          pageResponseView :: Text.Text
        , pageResponseController :: Maybe Text.Text
        , pageResponseParams :: Maybe Aeson.Value
} deriving (Show, Eq, GG.Generic)

instance Aeson.ToJSON PageResponse

type URI = BS.ByteString

data Statics = Statics {
        -- | Get HTML from a view name.
          responseOnView :: URI -> IO BS.ByteString
        -- | JavaScript.
        , responseOnController :: URI -> IO BS.ByteString
}

data Actions = Actions {
        -- | User trying to login.
          responseOnLogin :: Maybe (Username -> Password -> IO Bool)
        -- | A page.
        , responseOnPage :: Text.Text -> (Maybe Aeson.Value) -> IO PageResponse
        -- | Idempotent query for info (As the intended use of HTTP GET).
        , responseOnLookup :: URI -> (Maybe BS.ByteString) -> IO Aeson.Value
        -- TODO: Rename this action, avoid confusion with HTTP post.
        -- | An action that changes the server's info (As an HTTP POST).
        , responseOnPost :: URI -> (Maybe BS.ByteString) -> IO Aeson.Value
        -- | jTables.
        , responseOnTable :: URI -> (Maybe BS.ByteString) -> IO BS.ByteString
}

-- TODO:
data Permissions = Permissions {
          authorizePage :: Text.Text -> Username -> Bool
        , authorizeLookup :: URI -> (Maybe Aeson.Value) -> Username -> Bool
        , authorizePost :: URI -> (Maybe Aeson.Value) -> Username -> Bool
        , authorizeTable :: URI -> (Maybe Aeson.Value)-> Username -> Bool
}

--------------------------------------------------------------------------------

start :: Int -> Statics -> Actions -> IO ()
start port statics actions = startPaths port statics [("",actions)]

startPaths :: Int -> Statics -> [(BS.ByteString, Actions)] -> IO ()
startPaths port statics actions = do
        storage <- Session.newStorage "ssid"
        Server.start port $ \request -> case (Request.uri request) of
                -- Statics path prefix.
                uri | (BS.isPrefixOf "/_/s/" uri) -> do
                        -- Dont even check if this is a GET method, just answer.
                        body <- getBodyStatic statics request
                        response body
                _ -> do
                        -- Create the session cookie used by all paths.
                        (session, setCookie) <- getSession
                                request
                                storage
                                Nothing
                                (Just "/")
                        resp <- matchPaths
                                storage
                                session
                                actions
                                request
                        -- Append the session cookie as the last header to if by
                        -- mistake someone used the same name it is overwriten.
                        return (resp {
                                Response.headers = (Response.headers resp) {
                                        Response.setCookie = (Response.setCookie $ Response.headers resp) ++ setCookie
                                }
                        })

{-- TODO: startHosts
-- | Host name as typed on browser:
-- For example http://web.com is "web.com" and web.com:8080 is "web.com:8080"
startHosts :: Int -> Statics -> [( BS.ByteString, [(BS.ByteString, Actions)] )] -> IO ()
--}

--------------------------------------------------------------------------------

getSession :: Request.Request -> Session.Storage Session -> (Maybe Cookie.Domain) -> (Maybe Cookie.Path) -> IO (Session, [BS.ByteString])
getSession request storage host path = case (Request.cookie $ Request.headers request) of
        Nothing -> newSession request storage host path
        Just cookiesBs -> do
                maybeSession <- Session.getSession storage cookiesBs
                case maybeSession of
                        Nothing -> newSession request storage host path
                        Just session -> do
                                utcTime <- Time.getCurrentTime
                                Session.setSession
                                        storage
                                        (session {
                                                sessionLastRequestTime = utcTime
                                        })
                                        cookiesBs
                                return (session, [])

newSession :: Request.Request -> Session.Storage Session -> (Maybe Cookie.Domain) -> (Maybe Cookie.Path) -> IO (Session, [BS.ByteString])
newSession request storage host path = do
        utcTime <- Time.getCurrentTime
        let session = Session {
                  sessionHost = host
                , sessionPath = path
                , sessionCreationTime = utcTime
                , sessionFirstRequestUri = (Request.uri request)
                , sessionLastRequestTime = utcTime
                , isUserLoggedIn = False
        }
        let cookieParams = Cookie.emptyParams {
                  Cookie.domain = host
                , Cookie.path = path
                -- TODO: Cookie.secure = True,
                , Cookie.httpOnly = True
        }
        setCookieHeaderBs <- Session.newSession storage session cookieParams
        return (session, [setCookieHeaderBs])

--------------------------------------------------------------------------------

matchPaths :: Session.Storage Session -> Session -> [(BS.ByteString, Actions)] -> Request.Request -> IO Response.Response
matchPaths storage session ((path,actions):xss) request = do
        if BS.isPrefixOf path (Request.uri request)
                then do
                        let uri = BS.drop (BS.length path) (Request.uri request)
                        let request' = if uri == ""
                                then request {Request.uri = "/"}
                                else request {Request.uri = uri}
                        body <- getBodyDynamic storage session actions request'
                        response body
                else matchPaths storage session xss request
matchPaths _ _ [] request =
        return $ Response.Response {
                  Response.version = Version.HTTP_1_1
                , Response.status = Status.Status 404 "Not Found"
                , Response.headers = Response.emptyHeaders {
                        Response.connection = Just "close"
                }
                , Response.body = Just $ BS8.pack (show request)
        }

{--
startPaths :: Login -> [(BS.ByteString, BS.ByteString, Actions)] -> Session.Storage Session -> Request.Request -> IO Response.Response
startPaths actions storage request
        |
                -- Method GET with the statics path prefix.
                (Request.method request == Method.GET),
                (BS.isPrefixOf "/_/s/" (Request.uri request))
        = do
                body <- getBodyStatic statics request
                response body []
        |
                -- Method POST with the dynamics path prefix.
                (Request.method request == Method.POST),
                (BS.isPrefixOf "/_/d/" (Request.uri request))
        = do
                body <- getBodyDynamic actions storage request
                response body []
        |
                -- Not framework path.
                (not $ BS.isPrefixOf "/_/" (Request.uri request))
        = do
                body <- getBodyLogin "web/html/login.html"
                response body []
        | otherwise
        = error $ "Unknown request: " ++ (show request)
--}

--------------------------------------------------------------------------------

response :: (Maybe BS.ByteString) -> IO Response.Response
response body = do
        let (contentEncoding, contentLength) = case body of
                Nothing -> (Nothing, Nothing)
                (Just body) ->
                        (
                                  Just "identity"
                                , Just $ BS8.pack $ show $ BS.length body
                        )
        return $ Response.Response {
                  Response.version = Version.HTTP_1_1
                , Response.status = Status.Status 200 "OK"
                , Response.headers = Response.emptyHeaders {
                          Response.setCookie = []
                        -- Safari on iOS is very aggresive, without this
                        -- medium length responses raise an error.
                        , Response.connection = Just "close"
-- TODO: Is this necessary or I can just close the connection ?
-- Look at the HTTP 1.1 standard.
--                        , Response.contentEncoding = contentEncoding
--                        , Response.contentLength = contentLength
                }
                , Response.body = body
        }

--------------------------------------------------------------------------------

getBodyStatic :: Statics -> Request.Request -> IO (Maybe BS.ByteString)
getBodyStatic (Statics doView doController) request
        |
                (BS.isPrefixOf "/_/s/view/" (Request.uri request))
        = do
                let viewName = BS.drop
                        (BS.length "/_/s/view/")
                        (Request.uri request)
                view <- doView viewName
                return (Just view)
        |
                (BS.isPrefixOf "/_/s/controller/" (Request.uri request))
        = do
                let controllerName = BS.drop
                        (BS.length "/_/s/controller/")
                        (Request.uri request)
                controllerCode <- doController controllerName
                return (Just $ controllerAppend controllerName controllerCode)
        |
                (BS.isPrefixOf "/_/s/html/" (Request.uri request))
        = do
                body <- getStaticBody (Request.uri request)
                return (Just body)
        | otherwise
        = error $ "Unknown static object requested: " ++ (show request)

getBodyDynamic :: Session.Storage Session -> Session -> Actions -> Request.Request -> IO (Maybe BS.ByteString)
getBodyDynamic storage session actions@(Actions Nothing _ _ _ _) request
        |
                -- Not framework path, not logged in, GET.
                (not $ BS.isPrefixOf "/_/" (Request.uri request)),
                (not $ isUserLoggedIn session),
                (Request.method request == Method.GET)
        = do
                body <- getBodyFromData "web/html/index.html"
                return (Just body)
        | otherwise
        = getBodyDynamic' (session {isUserLoggedIn = True}) actions request
getBodyDynamic storage session actions@(Actions (Just onLogin) doPage doLookup doPost doTable) request
        |
                -- Not framework path, not logged in, GET.
                (not $ BS.isPrefixOf "/_/" (Request.uri request)),
                (not $ isUserLoggedIn session),
                (Request.method request == Method.GET)
        = do
                body <- getBodyFromData "web/html/login.html"
                return (Just body)
        |
                -- Not framework path, not logged in, POST.
                (not $ BS.isPrefixOf "/_/" (Request.uri request)),
                (not $ isUserLoggedIn session),
                (Request.method request == Method.POST)
        = do
                passedLogIn <- onLogin "user" "pass"
                if (passedLogIn)
                then do
                        Session.setSession
                                storage
                                (session {isUserLoggedIn = True})
                                (fromJust $ Request.cookie $ Request.headers request)
                        body <- getBodyFromData "web/html/index.html"
                        return (Just body)
                else do
                        body <- getBodyFromData "web/html/login.html"
                        return (Just body)
        | otherwise
        = getBodyDynamic' session actions request

getBodyDynamic' :: Session -> Actions -> Request.Request -> IO (Maybe BS.ByteString)
getBodyDynamic' session (Actions _ doPage doLookup doPost doTable) request
        |
                -- Not framework path, logged in, GET.
                (not $ BS.isPrefixOf "/_/" (Request.uri request)),
                (isUserLoggedIn session),
                (Request.method request == Method.GET)
        = do
                body <- getBodyFromData "web/html/index.html"
                return (Just body)
        |
                -- Not framework path, logged in, POST.
                (not $ BS.isPrefixOf "/_/" (Request.uri request)),
                (isUserLoggedIn session),
                (Request.method request == Method.POST)
        = do
                body <- getBodyFromData "web/html/index.html"
                return (Just body)
        |
                (Request.uri request == "/_/d/page"),
                (isUserLoggedIn session),
                (Request.method request == Method.POST)
        = do
                case (Request.body request) of
                        Nothing -> error ""
                        Just body -> do
                                let eitherRequest = (Aeson.eitherDecodeStrict body :: (Either String PageRequest))
                                case eitherRequest of
                                        Left err -> return $ Just $ BS8.pack err
                                        Right (PageRequest pageName maybeParams) -> do
                                                pageResponse <- doPage pageName maybeParams
                                                return $ Just $ BSL.toStrict $ Aeson.encode $ pageResponse
        |
                (BS.isPrefixOf "/_/d/lookup/" (Request.uri request)),
                (isUserLoggedIn session),
                (Request.method request == Method.POST)
        = do
                aeson <- doLookup
                        (BS.drop (BS.length "/_/d/lookup/") (Request.uri request))
                        (Request.body request)
                return $ Just $ BSL.toStrict $ Aeson.encode aeson
        |
                (BS.isPrefixOf "/_/d/post/" (Request.uri request)),
                (isUserLoggedIn session),
                (Request.method request == Method.POST)
        = do
                aeson <- doPost
                        (BS.drop (BS.length "/_/d/post/") (Request.uri request))
                        (Request.body request)
                return $ Just $ BSL.toStrict $ Aeson.encode aeson
        |
                (BS.isPrefixOf "/_/d/table/" (Request.uri request)),
                (isUserLoggedIn session),
                (Request.method request == Method.POST)
        = do
                table <- doTable
                        (BS.drop (BS.length "/_/d/table/") (Request.uri request))
                        (Request.body request)
                return (Just table)
        | otherwise
        = error $ "Unknown dynamic object requested: " ++ (show request) ++ " " ++ (show session)

--------------------------------------------------------------------------------

controllerAppend :: BS.ByteString -> BS.ByteString -> BS.ByteString
controllerAppend controllerName controllerCode = BS.concat
        [
                "jsMVC.view.controller(",
                        "\"", controllerName, "\"",
                        ", ",
                        "function ( window, document, undefined, $, jQuery, _params ) {\n",
                                controllerCode,
                        "\n}\n",
                ");\n",
                "//# sourceMappingURL=/_/d/controller/",
                controllerName
        ]

--------------------------------------------------------------------------------

getStaticBody :: BS.ByteString -> IO BS.ByteString

-- /css:

-- -- Styles from bootstrap 3.2.0.
getStaticBody "/_/s/html/css/bootstrap.css" =
        getBodyFromData
                "web/css/bootstrap/3.3.6/bootstrap.min.css"
getStaticBody "/_/s/html/css/bootstrap-theme.css" =
        getBodyFromData
                "web/css/bootstrap/3.3.6/bootstrap-theme.min.css"
-- -- Styles from jquery-ui-1.11.2.
getStaticBody "/_/s/html/css/jquery-ui.css" =
        getBodyFromData
                "web/css/jquery-ui/1.11.2/jquery-ui.min.css"
getStaticBody "/_/s/html/css/jquery-ui.structure.css" =
        getBodyFromData
                "web/css/jquery-ui/1.11.2/jquery-ui.structure.min.css"
getStaticBody "/_/s/html/css/jquery-ui.theme.css" =
        getBodyFromData
                "web/css/jquery-ui/1.11.2/jquery-ui.theme.min.css"

-- /fonts:

-- -- Bootstrap Glyphicons.
getStaticBody "/_/s/html/fonts/glyphicons-halflings-regular.eot" =
        getBodyFromData
                "web/fonts/bootstrap/3.3.6/glyphicons-halflings-regular.eot"
getStaticBody "/_/s/html/fonts/glyphicons-halflings-regular.svg" =
        getBodyFromData
                "web/fonts/bootstrap/3.3.6/glyphicons-halflings-regular.svg"
getStaticBody "/_/s/html/fonts/glyphicons-halflings-regular.ttf" =
        getBodyFromData
                "web/fonts/bootstrap/3.3.6/glyphicons-halflings-regular.ttf"
getStaticBody "/_/s/html/fonts/glyphicons-halflings-regular.woff" =
        getBodyFromData
                "web/fonts/bootstrap/3.3.6/glyphicons-halflings-regular.woff"
getStaticBody "/_/s/html/fonts/glyphicons-halflings-regular.woff2" =
        getBodyFromData
                "web/fonts/bootstrap/3.3.6/glyphicons-halflings-regular.woff2"

-- /images:

-- Images from jquery-ui-1.11.2
getStaticBody "/_/s/html/css/images/ui-bg_diagonals-thick_18_b81900_40x40.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_diagonals-thick_18_b81900_40x40.png"
getStaticBody "/_/s/html/css/images/ui-bg_diagonals-thick_20_666666_40x40.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_diagonals-thick_20_666666_40x40.png"
getStaticBody "/_/s/html/css/images/ui-bg_flat_10_000000_40x100.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_flat_10_000000_40x100.png"
getStaticBody "/_/s/html/css/images/ui-bg_glass_65_ffffff_1x400.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_glass_65_ffffff_1x400.png"
getStaticBody "/_/s/html/css/images/ui-bg_glass_100_f6f6f6_1x400.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_glass_100_f6f6f6_1x400.png"
getStaticBody "/_/s/html/css/images/ui-bg_glass_100_fdf5ce_1x400.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_glass_100_fdf5ce_1x400.png"
getStaticBody "/_/s/html/css/images/ui-bg_gloss-wave_35_f6a828_500x100.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_gloss-wave_35_f6a828_500x100.png"
getStaticBody "/_/s/html/css/images/ui-bg_highlight-soft_75_ffe45c_1x100.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_highlight-soft_75_ffe45c_1x100.png"
getStaticBody "/_/s/html/css/images/ui-bg_highlight-soft_100_eeeeee_1x100.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-bg_highlight-soft_100_eeeeee_1x100.png"
getStaticBody "/_/s/html/css/images/ui-icons_228ef1_256x240.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-icons_228ef1_256x240.png"
getStaticBody "/_/s/html/css/images/ui-icons_222222_256x240.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-icons_222222_256x240.png"
getStaticBody "/_/s/html/css/images/ui-icons_ef8c08_256x240.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-icons_ef8c08_256x240.png"
getStaticBody "/_/s/html/css/images/ui-icons_ffd27a_256x240.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-icons_ffd27a_256x240.png"
getStaticBody "/_/s/html/css/images/ui-icons_ffffff_256x240.png" =
        getBodyFromData
                "web/images/jquery-ui/1.11.2/ui-icons_ffffff_256x240.png"
-- jsMVC images.
getStaticBody "/_/s/html/images/jsMVC/spinner.gif" =
        getBodyFromData
                "web/images/jsMVC/spinner.gif"

-- /js:

-- -- Bootstrap.
getStaticBody "/_/s/html/js/bootstrap.js" =
    getBodyFromData
                "web/js/bootstrap/3.3.6/bootstrap.min.js"

-- -- jQuery.
getStaticBody "/_/s/html/js/jquery.js" =
        getBodyFromData
                "web/js/jquery/2.1.1/jquery.min.js"

-- -- jQuery UI.
getStaticBody "/_/s/html/js/jquery-ui.js" =
        getBodyFromData
                "web/js/jquery-ui/1.11.2/jquery-ui.min.js"

-- -- My Javascripts libraries.
getStaticBody "/_/s/html/js/jInvoice.js" =
        getBodyFromData
                "web/js/jInvoice.js"
getStaticBody "/_/s/html/js/jTable.js" =
        getBodyFromData
                "web/js/jTable.js"
getStaticBody "/_/s/html/js/jsMVC.js" =
        getBodyFromData
                "web/js/jsMVC.js"
getStaticBody "/_/s/html/js/jSwipeCard.js" =
        getBodyFromData
                "web/js/jSwipeCard.js"
getStaticBody "/_/s/html/js/quirks.js" =
        getBodyFromData
                "web/js/quirks.js"

-- -- cldrjs.
getStaticBody "/_/s/html/js/cldrjs/cldr.js" =
        getBodyFromData
                "web/js/cldrjs/0.4.4/cldr.js"
getStaticBody "/_/s/html/js/cldrjs/event.js" =
        getBodyFromData
                "web/js/cldrjs/0.4.4/event.js"
getStaticBody "/_/s/html/js/cldrjs/supplemental.js" =
        getBodyFromData
                "web/js/cldrjs/0.4.4/supplemental.js"
getStaticBody "/_/s/html/js/cldrjs/unresolved.js" =
        getBodyFromData
                "web/js/cldrjs/0.4.4/unresolved.js"

-- -- Globalize.
getStaticBody "/_/s/html/js/globalize/currency.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/currency.js"
getStaticBody "/_/s/html/js/globalize/date.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/date.js"
getStaticBody "/_/s/html/js/globalize/globalize.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/globalize.js"
getStaticBody "/_/s/html/js/globalize/message.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/message.js"
getStaticBody "/_/s/html/js/globalize/number.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/number.js"
getStaticBody "/_/s/html/js/globalize/plural.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/plural.js"
getStaticBody "/_/s/html/js/globalize/relative-time.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/relative-time.js"
getStaticBody "/_/s/html/js/globalize/unit.js" =
        getBodyFromData
                "web/js/globalize/1.1.1/unit.js"

-- Unknown
getStaticBody bs = error $ "Unknown static html object requested:" ++ (show bs)

--------------------------------------------------------------------------------

getBodyFromData :: FilePath -> IO BS.ByteString
getBodyFromData fp = do
#ifdef UsePathsCache
        return (getDataFileContent fp)
#else
        realPath <- getDataFileName fp
        bodyBs <- BS.readFile realPath
        return bodyBs
#endif
