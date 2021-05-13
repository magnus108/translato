module Lib.Server
    ( application
    , docs
    )
where

import           Blaze.ByteString.Builder       ( toByteString )
import           Web.Cookie
import           Lib.App.Error                  ( throwError
                                                , AppErrorType(..)
                                                , WithError(..)
                                                )
import           Servant.API.Generic            ( toServant )
import           Servant.Server                 ( Application
                                                , Server
                                                , Handler
                                                , hoistServer
                                                , serve
                                                )

import           Lib.App                        ( AppEnv
                                                , App
                                                , Env(..)
                                                , runApp
                                                )

import           Lib.Server.Types               ( AuthCookie(..)
                                                , Permission(..)
                                                , adminPermissions
                                                )

import           Data.Aeson

import           Servant.Auth.Server            ( JWTSettings
                                                , CookieSettings
                                                )
import           Servant.Auth.Docs
import qualified Servant.Docs                  as Docs

import           Servant.API                   as Web
                                                ( (:>)
                                                , Capture
                                                , Get
                                                , Header
                                                , Header'
                                                , JSON
                                                , PostNoContent(..)
                                                , NoContent(NoContent)
                                                , Post
                                                , QueryParam
                                                , QueryParam'
                                                , ReqBody
                                                , (:<|>)
                                                )
import           Servant                 hiding ( throwError
                                                , ServerError
                                                )
import           Servant.Auth.Server
import           Servant.Server.Generic         ( genericServerT )

import           Servant.API.Generic           as Web
                                                ( (:-)
                                                , toServant
                                                , genericApi
                                                , ToServant
                                                )

import           Lib.Server.Types               ( AppServer
                                                , ToApi
                                                , Permission
                                                , ProtectAPI
                                                )
import           Lib.Server.Protected.AccessKey


import           Network.Wai.Middleware.Cors
import           Data.UUID.Typed

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as LT
import qualified Text.Markdown                 as Markdown

import           Servant.HTML.Blaze
import           Text.Blaze                    as HTML
import           Text.Blaze.Html               as HTML

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    -- not save
    liftIO $ runApp env app

-- does not match layercake
siteServer :: Site AppServer
siteServer = Site { publicSite    = genericServerT publicServer
                  , protectedSite = genericServerT protectedServer
                  }


newtype Username
  = Username
      { usernameText :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

data LoginForm
  = LoginForm
      { loginFormUsername :: Username
      , loginFormPassword :: Text
      }
        deriving (Show, Eq, Ord)
        deriving Generic
        deriving anyclass (FromJSON, ToJSON)

instance Docs.ToSample LoginForm
instance Docs.ToSample Username


publicServer :: PublicSite AppServer
publicServer =
    PublicSite { postLogin = servePostLogin, getDocs = serveGetDocs }

newtype GetDocsResponse
  = GetDocsResponse
      { unGetDocsResponse :: HTML.Html
      }
        deriving Generic

instance Docs.ToSample GetDocsResponse where
    toSamples Proxy =
        Docs.singleSample $ GetDocsResponse "Documentation (In HTML)."
instance ToMarkup GetDocsResponse where
    toMarkup (GetDocsResponse html) = toMarkup html

serveGetDocs :: App GetDocsResponse
serveGetDocs = do
    pure $ htmlResponse

htmlResponse :: GetDocsResponse
htmlResponse =
    GetDocsResponse
        $ Markdown.markdown Markdown.defaultMarkdownSettings
              { Markdown.msXssProtect = False
              }
        $ LT.fromStrict
        $ docs2

docs2 :: Text
docs2 =
    T.unlines
        . map
              (\t -> if T.isPrefixOf "```" (T.stripStart t)
                  then T.stripStart t
                  else t
              )
        . T.lines
        . T.pack
        $ Docs.markdown
        $ docs'





servePostLogin
    :: LoginForm -> App (Headers '[Header "Set-Cookie" Text] NoContent)
servePostLogin LoginForm {..} = do
    let perms = adminPermissions
        uid   = 1
        --Find ud af hvordan User virker??
    userIdentifier <- liftIO nextRandomUUID
    traceShowM "lol"
    setLoggedIn uid userIdentifier perms
  where
    setLoggedIn uid userIdentifier perms = do
        let cookie =
                AuthCookie { userUUID = userIdentifier, permissions = perms }
        Env {..} <- ask -- this is wrong
        mCookie  <- liftIO $ makeSessionCookie cookieSettings jwtSettings cookie
        case mCookie of
            Nothing        -> throwError $ ServerError "servantErr"
            Just setCookie -> do
                --now <- liftIO getCurrentTime
                --runDb $ update uid [UserLastLogin =. Just now]
                return $ addHeader
                    (decodeUtf8
                        ((toByteString . renderSetCookie) (setCookie { setCookieHttpOnly = False })
                        )
                    )
                    NoContent
        {-
  me <- runDb $ getBy $ UniqueUsername loginFormUsername
  case me of
    Nothing -> throwError err401
    Just (Entity uid user) ->
      if validatePassword (userHashedPassword user) loginFormPassword
        then do
          admins <- asks envAdmins
          let perms =
                if userUsername user `elem` admins
                  then adminPermissions
                  else userPermissions
          setLoggedIn uid user perms
        else do
          aks <- runDb $ selectList [] [Asc AccessKeyCreatedTimestamp]
          let mli =
                flip map aks $ \(Entity _ AccessKey {..}) -> do
                  submittedKey <- parseAccessKeySecretText loginFormPassword
                  if validatePassword accessKeyHashedKey (accessKeySecretText submittedKey)
                    then Just accessKeyPermissions
                    else Nothing
          case msum mli of
            Nothing -> throwError err401
            Just perms -> setLoggedIn uid user perms
  where
    setLoggedIn uid user perms = do
      let cookie =
            AuthCookie {authCookieUserUUID = userIdentifier user, authCookiePermissions = perms}
      IntrayServerEnv {..} <- ask
      mCookie <- liftIO $ makeSessionCookieBS envCookieSettings envJWTSettings cookie
      case mCookie of
        Nothing -> throwError err401
        Just setCookie -> do
          now <- liftIO getCurrentTime
          runDb $ update uid [UserLastLogin =. Just now]
          return $ addHeader (decodeUtf8 setCookie) NoContent
          -}

-- does not match layercake
protectedServer :: ProtectedSite AppServer
protectedServer = ProtectedSite
    { protectedAccessKeySite = genericServerT protectedAccessKeyServer
    , getPermissions         = withAuthResultAndPermission ReadSomthing
                                                           serveGetPermissions
    }

protectedAccessKeyServer :: ProtectedAccessKeySite AppServer
protectedAccessKeyServer = ProtectedAccessKeySite
    { postAddAccessKey = withAuthResultAndPermission PermitAdd
                                                     servePostAddAccessKey
    , getAccessKey     = withAuthResultAndPermission PermitAdd serveGetAccessKey
    , getAccessKeys = withAuthResultAndPermission PermitAdd serveGetAccessKeys
    , deleteAccessKey  = withAuthResultAndPermission PermitAdd
                                                     serveDeleteAccessKey
    }

serveGetPermissions :: AuthCookie -> App [Permission]
serveGetPermissions AuthCookie {..} = pure permissions

serveDeleteAccessKey :: AuthCookie -> AccessKeyUUID -> App NoContent
serveDeleteAccessKey AuthCookie {..} uuid = do
    -- RUN IN IO, read file ith array
    -- delete item
    -- write file
    -- STRICT!
    pure NoContent

serveGetAccessKey :: AuthCookie -> AccessKeyUUID -> App AccessKeyInfo
serveGetAccessKey AuthCookie {..} uuid = do
    undefined
  --mac <- runDb $ getBy $ UniqueAccessKeyIdentifier uuid
  --case mac of
   -- Nothing -> throwAll err404 {errBody = "AccessKey not found."}
  --  Just (Entity _ ak) -> pure $ makeAccessKeyInfo ak

serveGetAccessKeys :: AuthCookie -> App [AccessKeyInfo]
serveGetAccessKeys AuthCookie {..} = do
    undefined
--  aks <- runDb $ selectList [AccessKeyUser ==. authCookieUserUUID] []
 -- pure $ map (makeAccessKeyInfo . entityVal) aks

-- Shoud use withPattern
servePostAddAccessKey :: AuthCookie -> AddAccessKey -> App AccessKeyCreated
servePostAddAccessKey AuthCookie {..} AddAccessKey {..} = undefined




withAuthResult
    :: WithError m => (AuthCookie -> m a) -> (AuthResult AuthCookie -> m a)
withAuthResult func ar = case ar of
    Authenticated ac -> func ac
    _                -> throwError $ ServerError "servantErr"

withAuthResultAndPermission
    :: WithError m
    => Permission
    -> (AuthCookie -> m a)
    -> (AuthResult AuthCookie -> m a)
withAuthResultAndPermission p func =
    withAuthResult (\ac -> withPermission (permissions ac) p =<< (func ac))

withPermission :: WithError m => [Permission] -> Permission -> a -> m a
withPermission ps p func = do
    traceShowM "fuck"
    if elem p ps then return func else throwError $ ServerError "servantErr"


server :: AppEnv -> Server SiteApi
server env = hoistServerWithContext siteAPI
                                    (Proxy :: Proxy SiteContext)
                                    (runAppAsHandler env)
                                    (genericServerT siteServer)

type SiteContext = '[CookieSettings, JWTSettings]

docs :: Docs.API
docs = Docs.docs siteAPI


docs' :: Docs.API
docs' = Docs.docs publicSiteAPI


application :: AppEnv -> Application
application env = addPolicy
    $ serveWithContext siteAPI (siteContext env) (server env)
  where
    addPolicy = cors (const $ Just policy)
    policy    = simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        , corsMethods        = ["GET", "POST", "HEAD", "DELETE"]
        }


siteContext :: AppEnv -> Context SiteContext
siteContext Env {..} = cookieSettings :. jwtSettings :. EmptyContext

-------------------------------------------------------------------------------
siteAPI :: Proxy SiteApi
siteAPI = genericApi $ (Proxy :: Proxy Site)

publicSiteAPI :: Proxy PublicAPI
publicSiteAPI = genericApi $ (Proxy :: Proxy PublicSite)


type SiteApi = ToApi Site

data Site route = Site
      { publicSite :: !(route :- PublicAPI)
      , protectedSite :: !(route :- ProtectedAPI)
      }
  deriving (Generic)

type ProtectedAPI = ToApi ProtectedSite

data PublicSite route
    = PublicSite { postLogin :: !(route :- PostLogin)
                 , getDocs :: !(route :- GetDocs)
                 }
  deriving (Generic)

type GetDocs = Get '[HTML] GetDocsResponse

type PublicAPI = ToApi PublicSite

data ProtectedSite route
  = ProtectedSite
      { protectedAccessKeySite :: !(route :- "access-key" :> ToApi ProtectedAccessKeySite)
      , getPermissions :: !(route :- GetPermissions)
      }
  deriving (Generic)

type GetPermissions = ProtectAPI :> "permissions" :> Get '[JSON] [Permission]

type PostLogin
    = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
   --- PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
