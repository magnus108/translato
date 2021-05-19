module Lib.Server
    ( application
    , LoginForm
    , docs
    )
where

import Lib.Data.Permission
import           Lib.Api
import           Lib.Api.Types
import           Lib.Client.Types
import           Lib.Data.Photographer          ( Photographers )

import           System.IO.Error
import           Control.Exception       hiding ( Handler )
import qualified Utils.ListZipper              as ListZipper

import           Blaze.ByteString.Builder       ( toByteString )
import           Web.Cookie
import           Lib.App.Error                  ( throwError
                                                , AppErrorType(..)
                                                , WithError(..)
                                                )

import           Lib.App.Error                  ( throwError
                                                , WithError
                                                )
import           Servant.API.Generic            ( toServant )
import           Servant.Server                 ( Application
                                                , Server
                                                , Handler
                                                , hoistServer
                                                , serve
                                                )

import qualified Lib.App                       as App
import           Lib.App                        ( AppEnv
                                                , Has(..)
                                                , AppError(..)
                                                , App
                                                , Env(..)
                                                , runApp
                                                , AppServer
                                                )


import           Data.Aeson

import           Servant.Auth.Server            ( JWTSettings
                                                , CookieSettings
                                                )
import           Servant.Auth.Docs
import qualified Servant.Docs                  as Docs

import           Lib.Config                     ( readJSONFileStrict )
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




serveGetDocs :: App GetDocsResponse
serveGetDocs = do
    pure $ App.htmlResponse


publicServer :: PublicSite AppServer
publicServer =
    PublicSite { postLogin = servePostLogin, getDocs = serveGetDocs }

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
                        ((toByteString . renderSetCookie)
                            (setCookie { setCookieSecure   = False
                                       , setCookieHttpOnly = False
                                       }
                            )
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
    , photographers          = withAuthResultAndPermission ReadSomthing
                                                           serveGetPhotographers
    , getPermissions         = withAuthResultAndPermission ReadSomthing
                                                           serveGetPermissions
    }

protectedAccessKeyServer :: ProtectedAccessKeySite AppServer
protectedAccessKeyServer = ProtectedAccessKeySite
    { postAddAccessKey = withAuthResultAndPermission PermitAdd
                                                     servePostAddAccessKey
    , getAccessKey = withAuthResultAndPermission PermitAdd serveGetAccessKey
    , getAccessKeys    = withAuthResultAndPermission PermitAdd
                                                     serveGetAccessKeys
    , deleteAccessKey  = withAuthResultAndPermission PermitAdd
                                                     serveDeleteAccessKey
    }

photographerServer :: PhotographerSite AppServer
photographerServer = PhotographerSite
    { getPhotographers = withAuthResultAndPermission ReadSomthing
                                                     serveGetPhotographers
    }

serveGetPhotographers :: AuthCookie -> App Photographers
serveGetPhotographers AuthCookie {..} = do
    photographers <- readPhotographers
    pure $ photographers


type WithPhotographers r m
    = (MonadReader r m, MonadIO m, Has App.MPhotographersFile r)


readPhotographers :: forall  r m . WithPhotographers r m => m Photographers
readPhotographers = do
    mPhotographersFile <-
        App.unMPhotographersFile <$> App.grab @App.MPhotographersFile
    photographersFile <- liftIO $ takeMVar mPhotographersFile
    content           <-
        liftIO
        $         (readJSONFileStrict photographersFile)
        `finally` (putMVar mPhotographersFile photographersFile)
    return content


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

