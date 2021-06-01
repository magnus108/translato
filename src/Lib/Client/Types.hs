module Lib.Client.Types where

import           Web.Cookie                     ( SetCookie(..))
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )

import           Relude.Extra                   ( firstF )
import           Control.Exception              ( catch
                                                , throwIO
                                                , try
                                                )

import           Servant.Auth.Client
import           Lib.Data.Photographer          ( Photographers )
import           Lib.Data.Tab          ( Tabs )
import           Lib.Data.Dump          ( Dump )
import           Graphics.UI.Threepenny.Core
import           Servant                 hiding ( throwError, Handler)
import qualified Servant.Client                as Servant

import           Lib.Client.Error               ( ClientAppError
                                                , throwError
                                                , ClientAppErrorType(..)
                                                , ClientAppException(..)
                                                )
import           GHC.Base                       ( failIO )

import qualified Control.Monad.Except          as E

import           Lib.Utils
import           Lib.Api.Types
import           Lib.Api hiding (PostDump, postDump, getDump, GetDump, PostTabs, postTabs, PostPhotographers, postPhotographers, GetPhotographers, getPhotographers, GetTabs, getTabs)


data ClientEnv (m :: Type -> Type) = ClientEnv
    { login :: Login
    , getPhotographers :: GetPhotographers

    , getTabs :: GetTabs
    , postTabs :: PostTabs

    , getDump :: GetDump
    , postDump :: PostDump

    , postPhotographers :: PostPhotographers

    , bToken :: BToken
    , hToken :: HToken

    , bPhotographers :: BPhotographers
    , hPhotographers :: HPhotographers

    , bTabs :: BTabs
    , hTabs :: HTabs

    , bDump :: BDump
    , hDump :: HDump
    }

newtype BToken = BToken { unBToken :: Behavior (Maybe SetCookie) }
newtype HToken = HToken { unHToken :: Handler (Maybe SetCookie) }

newtype BPhotographers = BPhotographers { unBPhotographers :: Behavior (Maybe Photographers) }
newtype HPhotographers = HPhotographers { unHPhotographers :: Handler (Maybe Photographers) }

newtype BTabs = BTabs { unBTabs :: Behavior (Maybe Tabs) }
newtype HTabs = HTabs { unHTabs :: Handler (Maybe Tabs) }

newtype BDump = BDump { unBDump :: Behavior (Maybe Dump) }
newtype HDump = HDump { unHDump :: Handler (Maybe Dump) }

newtype GetPhotographers = GetPhotographers { unGetPhotographers :: Token -> ClientApp Photographers }
newtype GetTabs = GetTabs { unGetTabs :: Token -> ClientApp Tabs }
newtype PostTabs = PostTabs { unPosTabs :: Token -> Tabs -> ClientApp NoContent }
newtype PostPhotographers = PostPhotographers { unPosPhotographers :: Token -> Photographers -> ClientApp NoContent }

newtype GetDump = GetDump { unGetDump :: Token -> ClientApp Dump }
newtype PostDump = PostDump { unPosDump :: Token -> Dump -> ClientApp NoContent }

newtype Login = Login { unLogin :: LoginForm -> ClientApp (Headers '[Header "Set-Cookie" Text] NoContent) }

instance Has Login              (ClientEnv m) where
    obtain = login

instance Has GetPhotographers              (ClientEnv m) where
    obtain = getPhotographers

instance Has GetDump              (ClientEnv m) where
    obtain = getDump

instance Has PostDump              (ClientEnv m) where
    obtain = postDump

instance Has GetTabs              (ClientEnv m) where
    obtain = getTabs

instance Has PostTabs              (ClientEnv m) where
    obtain = postTabs

instance Has PostPhotographers              (ClientEnv m) where
    obtain = postPhotographers

instance Has BToken              (ClientEnv m) where
    obtain = bToken

instance Has HToken              (ClientEnv m) where
    obtain = hToken

instance Has BPhotographers              (ClientEnv m) where
    obtain = bPhotographers

instance Has HPhotographers              (ClientEnv m) where
    obtain = hPhotographers


instance Has BTabs (ClientEnv m) where
    obtain = bTabs

instance Has HTabs              (ClientEnv m) where
    obtain = hTabs

instance Has BDump (ClientEnv m) where
    obtain = bDump

instance Has HDump              (ClientEnv m) where
    obtain = hDump

type ClientAppEnv = ClientEnv ClientApp


newtype ClientApp a = ClientApp
    { unClientApp :: ReaderT ClientAppEnv UI a
    } deriving newtype ( Functor
                        , Applicative
                        , Monad
                        , MonadIO
                        , MonadFail
                        , MonadFix
                        , MonadReader ClientAppEnv
                        )

instance MonadFail UI where
    fail msg = liftIO $ failIO msg

instance MonadUI ClientApp where
    liftUI m = ClientApp (ReaderT (const m))

instance E.MonadError ClientAppError ClientApp where
    throwError :: ClientAppError -> ClientApp a
    throwError = liftIO . throwIO . ClientAppException

    catchError :: ClientApp a -> (ClientAppError -> ClientApp a) -> ClientApp a
    catchError action handler = ClientApp $ ReaderT $ \env -> do
        undefined


--runClientAppAsUI :: ClientAppEnv -> ClientApp a -> UI (Either ClientAppError a)
--runClientAppAsUI env =  firstF unClientAppException . try . runClientApp env


runClientApp :: ClientAppEnv -> ClientApp a -> UI a
runClientApp env = usingReaderT env . unClientApp


runClientM :: Servant.ClientEnv -> Servant.ClientM a -> ClientApp a
runClientM cenv client = do
    e <- liftIO $ Servant.runClientM client cenv
    case e of
        Left  servantErr -> do
            traceShowM servantErr
            throwError $ ClientError "servantErr"
        Right a          -> pure a


clients :: Servant.ClientEnv -> IO ClientAppEnv
clients cenv = do
    let api = Servant.hoistClient siteAPI
                                  (runClientM cenv)
                                  (Servant.client siteAPI)
    let public           :<|> protected      = api
    let postLogin        :<|> docs           = public
    let (photographers :<|> tabs) :<|> dump :<|> getPermissions = protected

    let getDump' :<|> postDump' = dump
    let getTabs' :<|> postTabs' = tabs
    let getPhotographers' :<|> postPhotographers' = photographers

    let login = Login $ postLogin
    let getPhotographers = GetPhotographers getPhotographers'
    let getTabs = GetTabs getTabs'
    let postTabs = PostTabs postTabs'
    let postPhotographers = PostPhotographers postPhotographers'

    let getDump = GetDump getDump'
    let postDump = PostDump postDump'

    (dumpE, dumpH) <- newEvent
    bDump <- BDump <$> stepper Nothing dumpE

    (tabsE, tabsH) <- newEvent
    bTabs <- BTabs <$> stepper Nothing tabsE

    (tokenE, tokenH) <- newEvent
    bToken <- BToken <$> stepper Nothing tokenE

    (photographersE, photographersH) <- newEvent
    bPhotographers                   <- BPhotographers <$> stepper Nothing photographersE

    let hToken = HToken tokenH
    let hDump = HDump dumpH
    let hPhotographers = HPhotographers photographersH
    let hTabs = HTabs tabsH

    pure $ ClientEnv { .. }

mkClientAppEnv :: IO ClientAppEnv
mkClientAppEnv = do
    let serverPort = 8080
    let baseUrl' = Servant.BaseUrl Servant.Http "localhost" serverPort ""
    manager' <- newManager defaultManagerSettings
    let cenv = Servant.mkClientEnv manager' baseUrl'
    clients cenv
