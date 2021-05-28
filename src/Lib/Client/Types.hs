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
import           Lib.Api hiding (GetPhotographers, getPhotographers, GetTabs, getTabs)


data ClientEnv (m :: Type -> Type) = ClientEnv
    { login :: Login
    , getPhotographers :: GetPhotographers
    , getTabs :: GetTabs

    , bToken :: BToken
    , hToken :: HToken

    , bPhotographers :: BPhotographers
    , hPhotographers :: HPhotographers

    , bTabs :: BTabs
    , hTabs :: HTabs
    }

newtype BToken = BToken { unBToken :: Behavior (Maybe SetCookie) }
newtype HToken = HToken { unHToken :: Handler (Maybe SetCookie) }

newtype BPhotographers = BPhotographers { unBPhotographers :: Behavior (Maybe Photographers) }
newtype HPhotographers = HPhotographers { unHPhotographers :: Handler (Maybe Photographers) }

newtype BTabs = BTabs { unBTabs :: Behavior (Maybe Tabs) }
newtype HTabs = HTabs { unHTabs :: Handler (Maybe Tabs) }

newtype GetPhotographers = GetPhotographers { unGetPhotographers :: Token -> ClientApp Photographers }
newtype GetTabs = GetTabs { unGetTabs :: Token -> ClientApp Tabs }

newtype Login = Login { unLogin :: LoginForm -> ClientApp (Headers '[Header "Set-Cookie" Text] NoContent) }

instance Has Login              (ClientEnv m) where
    obtain = login

instance Has GetPhotographers              (ClientEnv m) where
    obtain = getPhotographers

instance Has GetTabs              (ClientEnv m) where
    obtain = getTabs

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

type ClientAppEnv = ClientEnv ClientApp


newtype ClientApp a = ClientApp
    { unClientApp :: ReaderT ClientAppEnv UI a
    } deriving newtype ( Functor
                        , Applicative
                        , Monad
                        , MonadIO
                        , MonadFail
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
        Left  servantErr -> throwError $ ClientError "servantErr"
        Right a          -> pure a


clients :: Servant.ClientEnv -> IO ClientAppEnv
clients cenv = do
    let api = Servant.hoistClient siteAPI
                                  (runClientM cenv)
                                  (Servant.client siteAPI)
    let public           :<|> protected      = api
    let postLogin        :<|> docs           = public
    let getPhotographers' :<|> getTabs' :<|> getPermissions = protected

    let login = Login $ postLogin
    let getPhotographers = GetPhotographers getPhotographers'
    let getTabs = GetTabs getTabs'

    (tabsE, tabsH) <- newEvent
    bTabs <- BTabs <$> stepper Nothing tabsE

    (tokenE, tokenH) <- newEvent
    bToken' <- stepper Nothing tokenE

    (photographersE, photographersH) <- newEvent
    bPhotographers                   <- BPhotographers <$> stepper Nothing photographersE

    let bToken = BToken bToken'
    let hToken = HToken tokenH

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
