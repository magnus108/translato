{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}

module Lib.Client where


import qualified Lib.Client.FilePicker         as FilePicker
import qualified Lib.Client.Select             as Select
import qualified Foreign.JavaScript            as JS
import qualified Relude.Unsafe                 as Unsafe
import           Servant.Auth.Client
import           Control.Conditional            ( (?<>) )
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Lib.Client.Types
import           Lib.Utils
import           Servant                 hiding ( Handler )

import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import           Web.Cookie                     ( parseSetCookie
                                                , setCookieName
                                                , SetCookie(..)
                                                , setCookieValue
                                                )

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )

import           Servant.Auth.Client
import           Lib.Data.Photographer          ( Photographers )
import           Graphics.UI.Threepenny.Core
import           Servant                 hiding ( throwError
                                                , Handler
                                                )
import qualified Servant.Client                as Servant

import           Lib.Client.Error               ( ClientAppError
                                                , throwError
                                                , ClientAppErrorType(..)
                                                )
import           GHC.Base                       ( failIO )

import qualified Control.Monad.Except          as E

import           Lib.Utils
import           Lib.Api.Types
import qualified Utils.ListZipper              as ListZipper
import qualified Control.Lens                  as Lens
import qualified Lib.Data.Photographer         as Photographer
import qualified Lib.Data.Tab                  as Tab
import qualified Lib.Data.Session              as Session
import qualified Lib.Data.Location             as Location
import qualified Lib.Data.Shooting             as Shooting
import qualified Lib.Data.Dump                 as Dump
import qualified Lib.Data.Camera               as Camera
import qualified Lib.Data.Doneshooting         as Doneshooting
import qualified Lib.Data.Dagsdato             as Dagsdato
import qualified Lib.Data.DagsdatoBackup       as DagsdatoBackup
import           Control.Comonad         hiding ( (<@) )
import           Lib.Client.Utils


mkTabs :: ClientApp (Element, Event (Maybe Tab.Tabs))
mkTabs = do
    (BTabs bTabs)            <- grab @BTabs

    (eSelection, hSelection) <- liftIO $ newEvent

    let errorDisplay = UI.string "no text"
    let showTab x = UI.string $ show x
    let display center tabs = do
            display <-
                UI.button
                #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                #+ [showTab (extract tabs)]

            UI.on UI.click display $ \_ -> do
                liftIO $ hSelection (Just (Tab.Tabs tabs))

            return display

    item <- liftUI $ UI.div #. "buttons has-addons" # sink
        items'
        (   fromMaybe [errorDisplay]
        <$> (fmap
                (ListZipper.toList . ListZipper.bextend display . Tab.unTabs)
            )
        <$> bTabs
        )

    return (item, eSelection)



mkPhotographersTab
    :: ClientApp (Element, Event (Maybe Photographer.Photographers))
mkPhotographersTab = mdo
    (BPhotographers bPhotographers) <- grab @BPhotographers
    let showPhotographer x = UI.string $ T.unpack $ Photographer.name x

    (item, eSelection) <- liftUI $ Select.mkSelect
        (fmap Photographer.unPhotographers <$> bPhotographers)
        showPhotographer

    return (item, (fmap Photographer.Photographers <$> eSelection))

mkCamerasTab :: ClientApp (Element, Event (Maybe Camera.Cameras))
mkCamerasTab = mdo
    (BCameras bCameras) <- grab @BCameras
    let showCamera x = UI.string $ show x

    (item, eSelection) <- liftUI
        $ Select.mkSelect (fmap Camera.unCameras <$> bCameras) showCamera

    return (item, (fmap Camera.Cameras <$> eSelection))


mkShootingsTab :: ClientApp (Element, Event (Maybe Shooting.Shootings))
mkShootingsTab = mdo
    (BShootings bShootings) <- grab @BShootings
    let showShooting x = UI.string $ show x

    (item, eSelection) <- liftUI $ Select.mkSelect
        (fmap Shooting.unShootings <$> bShootings)
        showShooting

    return (item, (fmap Shooting.Shootings <$> eSelection))

mkSessionsTab :: ClientApp (Element, Event (Maybe Session.Sessions))
mkSessionsTab = mdo
    (BSessions bSessions) <- grab @BSessions
    let showSession x = UI.string $ show x

    (item, eSelection) <- liftUI
        $ Select.mkSelect (fmap Session.unSessions <$> bSessions) showSession

    return (item, (fmap Session.Sessions <$> eSelection))


mkDumpTab :: ClientApp (Element, Event (Maybe Dump.Dump))
mkDumpTab = mdo
    (BDump bDump) <- grab @BDump
    eDialog       <- grab @EDialog
    let showIt x = UI.string x
    (item, eSelection) <- liftUI
        $ FilePicker.mkDirPicker eDialog (fmap Dump.unDump <$> bDump) showIt

    return (item, fmap Dump.Dump <$> eSelection)

mkDagsdatoTab :: ClientApp (Element, Event (Maybe Dagsdato.Dagsdato))
mkDagsdatoTab = mdo
    (BDagsdato bDagsdato) <- grab @BDagsdato
    eDialog               <- grab @EDialog
    let showIt x = UI.string x
    (item, eSelection) <- liftUI $ FilePicker.mkDirPicker
        eDialog
        (fmap Dagsdato.unDagsdato <$> bDagsdato)
        showIt

    return (item, fmap Dagsdato.Dagsdato <$> eSelection)

mkDagsdatoBackupTab
    :: ClientApp (Element, Event (Maybe DagsdatoBackup.DagsdatoBackup))
mkDagsdatoBackupTab = mdo
    (BDagsdatoBackup bDagsdatoBackup) <- grab @BDagsdatoBackup
    eDialog                           <- grab @EDialog
    let showIt x = UI.string x
    (item, eSelection) <- liftUI $ FilePicker.mkDirPicker
        eDialog
        (fmap DagsdatoBackup.unDagsdatoBackup <$> bDagsdatoBackup)
        showIt

    return (item, fmap DagsdatoBackup.DagsdatoBackup <$> eSelection)


mkDoneshootingTab
    :: ClientApp (Element, Event (Maybe Doneshooting.Doneshooting))
mkDoneshootingTab = mdo
    (BDoneshooting bDoneshooting) <- grab @BDoneshooting
    eDialog                       <- grab @EDialog
    let showIt x = UI.string x
    (item, eSelection) <- liftUI $ FilePicker.mkDirPicker
        eDialog
        (fmap Doneshooting.unDoneshooting <$> bDoneshooting)
        showIt

    return (item, fmap Doneshooting.Doneshooting <$> eSelection)

mkLocationTab :: ClientApp (Element, Event (Maybe Location.Location))
mkLocationTab = mdo
    (BLocation bLocation) <- grab @BLocation
    eDialog               <- grab @EDialog
    let showIt x = UI.string x
    (item, eSelection) <- liftUI $ FilePicker.mkFilePicker
        eDialog
        (fmap Location.unLocation <$> bLocation)
        showIt

    return (item, fmap Location.Location <$> eSelection)

mkContent :: ClientApp (Element, Event (Maybe Photographer.Photographers), Event (Maybe Location.Location))
mkContent = do
    (BTabs bTabs)                            <- grab @BTabs

    (photographersContent , ePhotographers ) <- mkPhotographersTab
    (dumpContent          , eDump          ) <- mkDumpTab
    (dagsdatoContent      , eDagsdato      ) <- mkDagsdatoTab
    (dagsdatoBackupContent, eDagsdatoBackup) <- mkDagsdatoBackupTab
    (doneshootingContent  , eDoneshooting  ) <- mkDoneshootingTab
    (camerasContent       , eCameras       ) <- mkCamerasTab
    (shootingsContent     , eShootings     ) <- mkShootingsTab
    (sessionsContent      , eSessions      ) <- mkSessionsTab
    (locationContent      , eLocation      ) <- mkLocationTab

    elseContent                              <- liftUI $ UI.string "fuck2"
    elseContent2                             <- liftUI $ UI.string "fuck2nodATA"

    let
        contents =
            fmap
                    (\xs ->
                        let focus = extract (Tab.unTabs xs)
                        in
                            case focus of
                                Tab.PhotographersTab -> [photographersContent]
                                Tab.DumpTab          -> [dumpContent]
                                Tab.DagsdatoTab      -> [dagsdatoContent]
                                Tab.DagsdatoBackupTab ->
                                    [dagsdatoBackupContent]
                                Tab.DoneshootingTab -> [doneshootingContent]
                                Tab.CamerasTab      -> [camerasContent]
                                Tab.ShootingsTab    -> [shootingsContent]
                                Tab.SessionsTab     -> [sessionsContent]
                                Tab.LocationTab     -> [locationContent]
                                _                   -> [elseContent]
                    )
                <$> bTabs

    content <- liftUI $ UI.div # sink
        children
        (fromMaybe [elseContent2] <$> contents)

    return (content, ePhotographers, eLocation)



setup :: Window -> ClientApp ()
setup win = do
    liftUI $ return win # set title "test"
    _                 <- initial

    (elemTabs, eTabs) <- mkTabs
    (elemContent, ePhotographers, eLocation) <- mkContent

    liftUI $ getBody win #+ [element elemTabs, UI.hr, element elemContent]

    onEvent' (filterJust eTabs) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostTabs postTabs) <- grab @PostTabs
        (HTabs    hTabs   ) <- grab @HTabs
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- postTabs (Token $ setCookieValue t) e
                liftIO $ hTabs (Just e)

    onEvent' (filterJust eLocation) $ \e -> do
        (BToken       bToken      ) <- grab @BToken
        (PostLocation postLocation) <- grab @PostLocation
        (HLocation    hLocation   ) <- grab @HLocation
        token                       <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- postLocation (Token $ setCookieValue t) e
                liftIO $ hLocation (Just e)

    onEvent' (filterJust ePhotographers) $ \e -> do
        (BToken            bToken           ) <- grab @BToken
        (PostPhotographers postPhotographers) <- grab @PostPhotographers
        (HPhotographers    hPhotographers   ) <- grab @HPhotographers
        token <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- postPhotographers (Token $ setCookieValue t) e
                liftIO $ hPhotographers (Just e)

    return ()


withToken :: (Token -> ClientApp a) -> ClientApp a
withToken func = do
    (BToken bToken) <- grab @BToken
    token           <- currentValue bToken
    case token of
        Nothing -> liftIO $ die "Missing token"
        Just t  -> func (Token $ setCookieValue t)


initial :: ClientApp ()
initial = do
    _ <- loginClient
    _ <- getPhotographersClient
    _ <- getTabsClient
    _ <- getDumpClient
    _ <- getDagsdatoClient
    _ <- getDagsdatoBackupClient
    _ <- getDoneshootingClient
    _ <- getCamerasClient
    _ <- getShootingsClient
    _ <- getSessionsClient
    _ <- getLocationClient
    _ <- getGradesClient
    return ()

getGradesClient :: ClientApp ()
getGradesClient = withToken $ \t -> do
    (GetGrades getGrades) <- grab @GetGrades
    (HGrades   hGrades  ) <- grab @HGrades
    res                       <- getGrades t
    case res of
        _ -> liftIO $ hGrades (Just res)

getLocationClient :: ClientApp ()
getLocationClient = withToken $ \t -> do
    (GetLocation getLocation) <- grab @GetLocation
    (HLocation   hLocation  ) <- grab @HLocation
    res                       <- getLocation t
    case res of
        _ -> liftIO $ hLocation (Just res)


getSessionsClient :: ClientApp ()
getSessionsClient = withToken $ \t -> do
    (GetSessions getSessions) <- grab @GetSessions
    (HSessions   hSessions  ) <- grab @HSessions
    res                       <- getSessions t
    case res of
        _ -> liftIO $ hSessions (Just res)


getShootingsClient :: ClientApp ()
getShootingsClient = withToken $ \t -> do
    (GetShootings getShootings) <- grab @GetShootings
    (HShootings   hShootings  ) <- grab @HShootings
    res                         <- getShootings t
    case res of
        _ -> liftIO $ hShootings (Just res)

getCamerasClient :: ClientApp ()
getCamerasClient = withToken $ \t -> do
    (GetCameras getCameras) <- grab @GetCameras
    (HCameras   hCameras  ) <- grab @HCameras
    res                     <- getCameras t
    case res of
        _ -> liftIO $ hCameras (Just res)

getDoneshootingClient :: ClientApp ()
getDoneshootingClient = withToken $ \t -> do
    (GetDoneshooting getDoneshooting) <- grab @GetDoneshooting
    (HDoneshooting   hDoneshooting  ) <- grab @HDoneshooting
    res                               <- getDoneshooting t
    case res of
        _ -> liftIO $ hDoneshooting (Just res)


getDagsdatoClient :: ClientApp ()
getDagsdatoClient = withToken $ \t -> do
    (GetDagsdato getDagsdato) <- grab @GetDagsdato
    (HDagsdato   hDagsdato  ) <- grab @HDagsdato
    res                       <- getDagsdato t
    case res of
        _ -> liftIO $ hDagsdato (Just res)

getDagsdatoBackupClient :: ClientApp ()
getDagsdatoBackupClient = withToken $ \t -> do
    (GetDagsdatoBackup getDagsdatoBackup) <- grab @GetDagsdatoBackup
    (HDagsdatoBackup   hDagsdatoBackup  ) <- grab @HDagsdatoBackup
    res <- getDagsdatoBackup t
    case res of
        _ -> liftIO $ hDagsdatoBackup (Just res)


getDumpClient :: ClientApp ()
getDumpClient = do
    withToken $ \t -> do
        (GetDump getDump) <- grab @GetDump
        (HDump   hDump  ) <- grab @HDump
        res               <- getDump t
        case res of
            _ -> liftIO $ hDump (Just res)


getTabsClient :: ClientApp ()
getTabsClient = do
    withToken $ \t -> do
        (GetTabs getTabs) <- grab @GetTabs
        (HTabs   hTabs  ) <- grab @HTabs
        res               <- getTabs t
        case res of
            _ -> liftIO $ hTabs (Just res)


getPhotographersClient :: ClientApp ()
getPhotographersClient = do
    withToken $ \t -> do
        (GetPhotographers getPhotographers) <- grab @GetPhotographers
        (HPhotographers   hPhotographers  ) <- grab @HPhotographers
        res <- getPhotographers t
        case res of
            _ -> liftIO $ hPhotographers (Just res)


loginClient :: ClientApp ()
loginClient = do
    (Login login                       ) <- grab @Login
    (Headers NoContent (HCons res HNil)) <- login LoginForm
    (HToken hToken                     ) <- grab @HToken
    case res of
        MissingHeader ->
            liftIO
                $ die
                      "The server responded but the response was missing the right session header."
        UndecodableHeader _ ->
            liftIO
                $ die
                      "The server responded but the response had an undecodable session header."
        Header setCookieText -> do
            let cookies = parseSetCookie . encodeUtf8 <$> T.lines setCookieText
                jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
            case jwtCookie of
                Nothing ->
                    liftIO
                        $ die
                              "No JWT-Cookie was found in the Set-Cookie session header."
                Just setCookie -> liftIO $ hToken (Just setCookie)
