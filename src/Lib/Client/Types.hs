module Lib.Client.Types where

import qualified Foreign.JavaScript            as JS
import           Web.Cookie                     ( SetCookie(..) )
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
import           Lib.Data.Tab                   ( Tabs )
import           Lib.Data.Camera                   ( Cameras )
import           Lib.Data.Dump                  ( Dump )
import           Lib.Data.Doneshooting          ( Doneshooting )
import           Lib.Data.Dagsdato              ( Dagsdato )
import           Lib.Data.DagsdatoBackup        ( DagsdatoBackup )
import           Graphics.UI.Threepenny.Core
import           Servant                 hiding ( throwError
                                                , Handler
                                                )
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
import           Lib.Api                 (siteAPI)


data ClientEnv (m :: Type -> Type) = ClientEnv
    { login :: Login
    , getPhotographers :: GetPhotographers

    , getTabs :: GetTabs
    , postTabs :: PostTabs

    , getCameras :: GetCameras
    , postCameras :: PostCameras

    , getDump :: GetDump
    , postDump :: PostDump

    , getDagsdato :: GetDagsdato
    , postDagsdato :: PostDagsdato

    , getDoneshooting :: GetDoneshooting
    , postDoneshooting :: PostDoneshooting


    , getDagsdatoBackup :: GetDagsdatoBackup
    , postDagsdatoBackup :: PostDagsdatoBackup

    , postPhotographers :: PostPhotographers

    , bToken :: BToken
    , hToken :: HToken

    , bPhotographers :: BPhotographers
    , hPhotographers :: HPhotographers

    , bTabs :: BTabs
    , hTabs :: HTabs

    , bCameras :: BCameras
    , hCameras :: HCameras

    , bDump :: BDump
    , hDump :: HDump

    , bDagsdato :: BDagsdato
    , hDagsdato :: HDagsdato

    , bDoneshooting :: BDoneshooting
    , hDoneshooting :: HDoneshooting

    , bDagsdatoBackup :: BDagsdatoBackup
    , hDagsdatoBackup :: HDagsdatoBackup

    , eDialog :: EDialog
    }

newtype EDialog = EDialog { unEDialog :: [String] -> JS.JSObject -> UI () }

newtype BToken = BToken { unBToken :: Behavior (Maybe SetCookie) }
newtype HToken = HToken { unHToken :: Handler (Maybe SetCookie) }

newtype BPhotographers = BPhotographers { unBPhotographers :: Behavior (Maybe Photographers) }
newtype HPhotographers = HPhotographers { unHPhotographers :: Handler (Maybe Photographers) }

newtype BTabs = BTabs { unBTabs :: Behavior (Maybe Tabs) }
newtype HTabs = HTabs { unHTabs :: Handler (Maybe Tabs) }

newtype BCameras = BCameras { unBCameras :: Behavior (Maybe Cameras) }
newtype HCameras = HCameras { unHCameras :: Handler (Maybe Cameras) }

newtype BDump = BDump { unBDump :: Behavior (Maybe Dump) }
newtype HDump = HDump { unHDump :: Handler (Maybe Dump) }

newtype BDagsdato = BDagsdato { unBDagsdato :: Behavior (Maybe Dagsdato) }
newtype HDagsdato = HDagsdato { unHDagsdato :: Handler (Maybe Dagsdato) }


newtype BDoneshooting = BDoneshooting { unBDoneshooting :: Behavior (Maybe Doneshooting) }
newtype HDoneshooting = HDoneshooting { unHDoneshooting :: Handler (Maybe Doneshooting) }

newtype BDagsdatoBackup = BDagsdatoBackup { unBDagsdatoBackup :: Behavior (Maybe DagsdatoBackup) }
newtype HDagsdatoBackup = HDagsdatoBackup { unHDagsdatoBackup :: Handler (Maybe DagsdatoBackup) }

newtype GetPhotographers = GetPhotographers { unGetPhotographers :: Token -> ClientApp Photographers }
newtype GetTabs = GetTabs { unGetTabs :: Token -> ClientApp Tabs }
newtype PostTabs = PostTabs { unPosTabs :: Token -> Tabs -> ClientApp NoContent }
newtype PostPhotographers = PostPhotographers { unPosPhotographers :: Token -> Photographers -> ClientApp NoContent }

newtype GetDump = GetDump { unGetDump :: Token -> ClientApp Dump }
newtype PostDump = PostDump { unPosDump :: Token -> Dump -> ClientApp NoContent }

newtype GetCameras = GetCameras { unGetCameras :: Token -> ClientApp Cameras }
newtype PostCameras = PostCameras { unPosCameras :: Token -> Cameras -> ClientApp NoContent }

newtype GetDagsdato = GetDagsdato { unGetDagsdato :: Token -> ClientApp Dagsdato }
newtype PostDagsdato = PostDagsdato { unPosDagsdato :: Token -> Dagsdato -> ClientApp NoContent }

newtype GetDoneshooting = GetDoneshooting { unGetDoneshooting :: Token -> ClientApp Doneshooting }
newtype PostDoneshooting = PostDoneshooting { unPostDoneshooting :: Token -> Doneshooting -> ClientApp NoContent }

newtype GetDagsdatoBackup = GetDagsdatoBackup { unGetDagsdatoBackup :: Token -> ClientApp DagsdatoBackup }
newtype PostDagsdatoBackup = PostDagsdatoBackup { unPosDagsdatoBackup :: Token -> DagsdatoBackup -> ClientApp NoContent }

newtype Login = Login { unLogin :: LoginForm -> ClientApp (Headers '[Header "Set-Cookie" Text] NoContent) }

instance Has EDialog              (ClientEnv m) where
    obtain = eDialog


instance Has Login              (ClientEnv m) where
    obtain = login

instance Has GetPhotographers              (ClientEnv m) where
    obtain = getPhotographers

instance Has GetDump              (ClientEnv m) where
    obtain = getDump

instance Has PostDump              (ClientEnv m) where
    obtain = postDump

instance Has GetCameras              (ClientEnv m) where
    obtain = getCameras

instance Has PostCameras             (ClientEnv m) where
    obtain = postCameras

instance Has GetDagsdato              (ClientEnv m) where
    obtain = getDagsdato

instance Has PostDagsdato            (ClientEnv m) where
    obtain = postDagsdato

instance Has GetDoneshooting              (ClientEnv m) where
    obtain = getDoneshooting

instance Has PostDoneshooting            (ClientEnv m) where
    obtain = postDoneshooting

instance Has GetDagsdatoBackup              (ClientEnv m) where
    obtain = getDagsdatoBackup

instance Has PostDagsdatoBackup            (ClientEnv m) where
    obtain = postDagsdatoBackup

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

instance Has HCameras              (ClientEnv m) where
    obtain = hCameras

instance Has BCameras              (ClientEnv m) where
    obtain = bCameras

instance Has BDump (ClientEnv m) where
    obtain = bDump

instance Has HDump              (ClientEnv m) where
    obtain = hDump

instance Has BDagsdato (ClientEnv m) where
    obtain = bDagsdato

instance Has HDagsdato              (ClientEnv m) where
    obtain = hDagsdato

instance Has BDoneshooting (ClientEnv m) where
    obtain = bDoneshooting

instance Has HDoneshooting              (ClientEnv m) where
    obtain = hDoneshooting

instance Has BDagsdatoBackup (ClientEnv m) where
    obtain = bDagsdatoBackup

instance Has HDagsdatoBackup              (ClientEnv m) where
    obtain = hDagsdatoBackup

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
        Left servantErr -> do
            traceShowM servantErr
            throwError $ ClientError "servantErr"
        Right a -> pure a


electronDialog :: [String] -> JS.JSObject -> JSFunction ()
electronDialog options callback = ffi
    "require('electron').remote.dialog.showOpenDialog({properties: %2}).then(result => %1(result.filePaths[0]))"
    callback
    options


clients :: Servant.ClientEnv -> IO ClientAppEnv
clients cenv = do
    let
        api = Servant.hoistClient siteAPI
                                  (runClientM cenv)
                                  (Servant.client siteAPI)
    let public :<|> protected = api
    let postLogin :<|> docs   = public
    let
        (photographers :<|> tabs :<|> dump) :<|> (dagsdato :<|> dagsdatoBackup) :<|> doneshooting :<|> cameras
            = protected

    let getDump' :<|> postDump'         = dump
    let getCameras' :<|> postCameras'         = cameras
    let getDagsdato' :<|> postDagsdato' = dagsdato
    let getDagsdatoBackup' :<|> postDagsdatoBackup' = dagsdatoBackup
    let getDoneshooting' :<|> postDoneshooting' = doneshooting
    let getTabs' :<|> postTabs'         = tabs
    let getPhotographers' :<|> postPhotographers' = photographers

    let login                           = Login $ postLogin
    let getPhotographers                = GetPhotographers getPhotographers'
    let getTabs                         = GetTabs getTabs'
    let postTabs                        = PostTabs postTabs'
    let postPhotographers = PostPhotographers postPhotographers'

    let getCameras                         = GetCameras getCameras'
    let postCameras = PostCameras postCameras'

    let getDump                         = GetDump getDump'
    let postDump                        = PostDump postDump'

    let getDagsdato                     = GetDagsdato getDagsdato'
    let postDagsdato                    = PostDagsdato postDagsdato'

    let getDoneshooting                 = GetDoneshooting getDoneshooting'
    let postDoneshooting                = PostDoneshooting postDoneshooting'

    let getDagsdatoBackup = GetDagsdatoBackup getDagsdatoBackup'
    let postDagsdatoBackup = PostDagsdatoBackup postDagsdatoBackup'

    (dumpE, dumpH)                     <- newEvent
    bDump                              <- BDump <$> stepper Nothing dumpE

    (dagsdatoE, dagsdatoH)             <- newEvent
    bDagsdato <- BDagsdato <$> stepper Nothing dagsdatoE

    (doneshootingE, doneshootingH)     <- newEvent
    bDoneshooting <- BDoneshooting <$> stepper Nothing doneshootingE

    (dagsdatoBackupE, dagsdatoBackupH) <- newEvent
    bDagsdatoBackup <- BDagsdatoBackup <$> stepper Nothing dagsdatoBackupE

    (tabsE, tabsH)                     <- newEvent
    bTabs                              <- BTabs <$> stepper Nothing tabsE

    (tokenE, tokenH)                   <- newEvent
    bToken                             <- BToken <$> stepper Nothing tokenE

    (photographersE, photographersH)   <- newEvent
    bPhotographers <- BPhotographers <$> stepper Nothing photographersE

    (camerasE, camerasH)   <- newEvent
    bCameras <- BCameras <$> stepper Nothing camerasE

    let hToken          = HToken tokenH
    let hDump           = HDump dumpH
    let hDagsdato       = HDagsdato dagsdatoH
    let hDagsdatoBackup = HDagsdatoBackup dagsdatoBackupH
    let hDoneshooting   = HDoneshooting doneshootingH
    let hPhotographers  = HPhotographers photographersH
    let hTabs           = HTabs tabsH
    let hCameras           = HCameras camerasH

    let eDialog = EDialog $ \xs cb -> runFunction $ electronDialog xs cb

    pure $ ClientEnv { .. }

mkClientAppEnv :: IO ClientAppEnv
mkClientAppEnv = do
    let serverPort = 8080
    let baseUrl' = Servant.BaseUrl Servant.Http "localhost" serverPort ""
    manager' <- newManager defaultManagerSettings
    let cenv = Servant.mkClientEnv manager' baseUrl'
    clients cenv
