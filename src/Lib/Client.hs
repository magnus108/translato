{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns  #-}

module Lib.Client where

import qualified Servant.Types.SourceT as SO
import           Servant.Types.SourceT          ( foreach
                                                , source
                                                )
import           Control.Exception              ( catch
                                                , throwIO
                                                , try
                                                )


import qualified Servant.Client.Streaming      as S

import Control.Concurrent
import Conduit
import Servant.Conduit

import Servant.Types.SourceT

import           Data.Generics.Labels
import           Data.Generics.Product.Fields
import           GHC.Generics
import           GHC.OverloadedLabels         (IsLabel (..))
import           Options.Generic 
import           Data.Generics.Labels


import qualified Lib.Api as API

import qualified Lib.Client.FilePicker         as FilePicker
import qualified Lib.Client.Select             as Select
import qualified Lib.Client.Control             as Control
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
import qualified Lib.Data.Grade                as Grade
import Lib.Data.Grade                
import qualified Lib.Data.Location             as Location
import qualified Lib.Data.Shooting             as Shooting
import qualified Lib.Data.Dump                 as Dump
import qualified Lib.Data.Camera               as Camera
import qualified Lib.Data.Doneshooting         as Doneshooting
import qualified Lib.Data.Photographee         as Photographee
import qualified Lib.Data.Dagsdato             as Dagsdato
import qualified Lib.Data.DagsdatoBackup       as DagsdatoBackup
import           Control.Comonad         hiding ( (<@)
                                                , (<@>)
                                                )
import           Lib.Client.Utils
import qualified Lib.Client.Text               as Text
import Control.Lens ((<>~), (.~))



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


mkLocationTab :: ClientApp (Element, Event (Maybe Location.Location), Event (Maybe Grade.Grades),Event (Maybe Grade.Grades),Event (Maybe Grade.Grades), Event (Maybe Grade.Grades), Event (Maybe Grade.Grades), Event (Maybe Grade.Grades))
mkLocationTab = mdo
    (BLocation bLocation) <- grab @BLocation
    eDialog               <- grab @EDialog
    let showIt x = UI.string x
    (location, eLocation) <- liftUI $ FilePicker.mkFilePicker
        eDialog
        (fmap Location.unLocation <$> bLocation)
        showIt

    (BGrades bGrades) <- grab @BGrades
    let showGradeIdentifier x = UI.string $ T.unpack $ Grade.identifier x

    (grades, eGradeSwitch) <- liftUI
        $ Select.mkSelect (fmap Grade.unGrades <$> bGrades) showGradeIdentifier


    modify <- liftUI $ Text.entry $ do
        grades <- bGrades
        return $ maybe
            ""
            (T.unpack . Grade.identifier . extract . Grade.unGrades)
            grades

    let eGradeIdentifier =
            liftOp
                    (\grades identifier -> Lens.set (#unGrades . ListZipper.zipperL . #identifier) (T.pack identifier) grades )
                <$> bGrades
                <@> (rumors (Text.userTE modify))


    (elemControl, elemControlMove, eInsert, eDelete, ePrev, eNext) <- Control.mkControls Grade.sempty (fmap Grade.unGrades <$> bGrades)

    let eGradeInsert = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust eInsert
    let eGradeDelete = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust eDelete
    let eGradePrev = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust ePrev
    let eGradeNext  = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust eNext

    item <- liftUI $ UI.div # set children
                                  [location, grades, Text.elementTE modify, elemControl, elemControlMove]

    return (item, fmap Location.Location <$> eLocation, fmap Grade.Grades <$> eGradeSwitch, eGradeIdentifier, eGradeInsert, eGradeDelete, eGradePrev, eGradeNext)

mkInsertPhotographeeTab :: ClientApp (Element
    , Event (Maybe Grade.Grades),Event (Maybe Grade.Grades),Event (Maybe Grade.Grades), Event (Maybe Grade.Grades), Event (Maybe Grade.Grades), Event (Maybe Grade.Grades)
    , Event (Maybe Grade.Grades),Event (Maybe Grade.Grades),Event (Maybe Grade.Grades), Event (Maybe Grade.Grades), Event (Maybe Grade.Grades), Event (Maybe Grade.Grades))
mkInsertPhotographeeTab = mdo
    (BGrades bGrades) <- grab @BGrades
    let showGradeIdentifier x = UI.string $ T.unpack $ Grade.identifier x

    (grades, eGradeSwitch) <- liftUI
        $ Select.mkSelect (fmap Grade.unGrades <$> bGrades) showGradeIdentifier

    modify <- liftUI $ Text.entry $ do
        grades <- bGrades
        return $ maybe
            ""
            (T.unpack . Grade.identifier . extract . Grade.unGrades)
            grades

    let eGradeIdentifier =
            liftOp
                    (\grades identifier -> Lens.set (#unGrades . ListZipper.zipperL . #identifier) (T.pack identifier) grades )
                <$> bGrades
                <@> (rumors (Text.userTE modify))


    (elemControl, elemControlMove, eInsert, eDelete, ePrev, eNext) <- Control.mkControls Grade.sempty (fmap Grade.unGrades <$> bGrades)

    let eGradeInsert = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust eInsert
    let eGradeDelete = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust eDelete
    let eGradePrev = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust ePrev
    let eGradeNext  = liftOp (\grades content -> Lens.set #unGrades content grades) <$> bGrades <@> filterJust eNext





    let showPhotographeeName x = UI.string $ T.unpack $ Photographee.name x

    (photographees, ePhotographeeSwitch) <- liftUI
        $ Select.mkSelect (fmap (Photographee.unPhotographees . Grade.photographees . extract . Grade.unGrades) <$> bGrades) showPhotographeeName


    modify2 <- liftUI $ Text.entry $ do
        grades <- bGrades
        return $ maybe
            ""
            (T.unpack . Photographee.name . extract . Photographee.unPhotographees . Grade.photographees . extract . Grade.unGrades)
            grades

    let ePhotographeeName =
            liftOp
                    (\grades name -> Lens.set (#unGrades . ListZipper.zipperL . #photographees . #unPhotographees . ListZipper.zipperL . #name) (T.pack name) grades )
                <$> bGrades
                <@> (rumors (Text.userTE modify2))


    (elemControl2, elemControlMove2, eInsert2, eDelete2, ePrev2, eNext2) <- Control.mkControls Photographee.sempty (fmap (Photographee.unPhotographees . Grade.photographees . extract . Grade.unGrades) <$> bGrades)

    let ePhotographeeInsert = liftOp (\grades content -> Lens.set (#unGrades . ListZipper.zipperL . #photographees . #unPhotographees ) content grades) <$> bGrades <@> filterJust eInsert2
    let ePhotographeeDelete = liftOp (\grades content -> Lens.set (#unGrades . ListZipper.zipperL . #photographees . #unPhotographees ) content grades) <$> bGrades <@> filterJust eDelete2
    let ePhotographeePrev = liftOp (\grades content -> Lens.set (#unGrades . ListZipper.zipperL . #photographees . #unPhotographees ) content grades) <$> bGrades <@> filterJust ePrev2
    let ePhotographeeNext  = liftOp (\grades content -> Lens.set (#unGrades . ListZipper.zipperL . #photographees . #unPhotographees) content grades) <$> bGrades <@> filterJust eNext2

    let ePhotographeeSwitch' = liftOp (\grades content -> Lens.set (#unGrades . ListZipper.zipperL . #photographees . #unPhotographees) content grades) <$> bGrades <@> filterJust ePhotographeeSwitch

    item <- liftUI $ UI.div # set children
                                  [ grades, Text.elementTE modify, elemControl, elemControlMove
                                  , photographees, Text.elementTE modify2, elemControl2, elemControlMove2
                                  ]

    return (item, 
            fmap Grade.Grades <$> eGradeSwitch, eGradeIdentifier, eGradeInsert, eGradeDelete, eGradePrev, eGradeNext,
            ePhotographeeSwitch', ePhotographeeName, ePhotographeeInsert, ePhotographeeDelete, ePhotographeePrev, ePhotographeeNext
           )



mkContent
    :: ClientApp
           ( Element
           , Event (Maybe Photographer.Photographers)
           , Event (Maybe Location.Location)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           , Event (Maybe Grade.Grades)
           )
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
    (locationContent      , eLocation, eGradesSwitch, eGradesIdentifier, eGradeInsert, eGradeDelete, eGradePrev, eGradeNext) <- mkLocationTab

    (insertPhotographeeContent, eGradesSwitch, eGradesIdentifier, eGradeInsert, eGradeDelete, eGradePrev, eGradeNext
            , ePhotographeeSwitch, ePhotographeeName, ePhotographeeInsert, ePhotographeeDelete, ePhotographeePrev, ePhotographeeNext
            ) <- mkInsertPhotographeeTab

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
                                Tab.InsertPhotographeeTab     -> [insertPhotographeeContent]
                                _                   -> [elseContent]
                    )
                <$> bTabs

    content <- liftUI $ UI.div # sink
        children
        (fromMaybe [elseContent2] <$> contents)

    return (content, ePhotographers, eLocation, eGradesSwitch, eGradesIdentifier, eGradeInsert, eGradeDelete, eGradePrev, eGradeNext
           ,ePhotographeeSwitch, ePhotographeeName, ePhotographeeInsert, ePhotographeeDelete, ePhotographeePrev, ePhotographeeNext
           )



setup :: Window -> ClientApp ()
setup win = do
    liftUI $ return win # set title "test"
    _                 <- initial

    (elemTabs, eTabs) <- mkTabs
    (elemContent, ePhotographers, eLocation, eGradesSwitch, eGradesIdentifier, eGradeInsert, eGradeDelete, eGradePrev, eGradeNext
            , ePhotographeeSwitch, ePhotographeeName, ePhotographeeInsert, ePhotographeeDelete, ePhotographeePrev, ePhotographeeNext) <- mkContent

    liftUI $ getBody win #+ [element elemTabs, UI.hr, element elemContent]
    
    onEvent' (filterJust ePhotographeePrev) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust ePhotographeeNext) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust ePhotographeeDelete) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust ePhotographeeInsert) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust ePhotographeeName) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust ePhotographeeSwitch) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)











    onEvent' (filterJust eGradePrev) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust eGradeNext) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust eGradeDelete) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust eGradeInsert) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust eGradesIdentifier) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)

    onEvent' (filterJust eGradesSwitch) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostGrades postGrades) <- grab @PostGrades
        (HGrades    hGrades ) <- grab @HGrades
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postGrades (Token $ setCookieValue t) e
                liftIO $ hGrades (Just e)


    onEvent' (filterJust eTabs) $ \e -> do
        (BToken   bToken  ) <- grab @BToken
        (PostTabs postTabs) <- grab @PostTabs
        (HTabs    hTabs   ) <- grab @HTabs
        token               <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postTabs (Token $ setCookieValue t) e
                liftIO $ hTabs (Just e)

    onEvent' (filterJust eLocation) $ \e -> do
        (BToken       bToken      ) <- grab @BToken
        (PostLocation postLocation) <- grab @PostLocation
        (HLocation    hLocation   ) <- grab @HLocation
        token                       <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postLocation (Token $ setCookieValue t) e
                liftIO $ hLocation (Just e)

    onEvent' (filterJust ePhotographers) $ \e -> do
        (BToken            bToken           ) <- grab @BToken
        (PostPhotographers postPhotographers) <- grab @PostPhotographers
        (HPhotographers    hPhotographers   ) <- grab @HPhotographers
        token <- currentValue bToken
        case token of
            Nothing -> liftIO $ die "Missing token"
            Just t  -> void $ do
                res <- runSingleClientOrErr $ postPhotographers (Token $ setCookieValue t) e
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

    traceShowM "loL"
    _ <- streamDumpClient
    traceShowM "loL2"

    return ()



runSingleClientOrErr :: S.ClientM a -> ClientApp a --(Maybe a)
runSingleClientOrErr func = do
    r <- runSingleClient func
    pure r


runSingleClient :: S.ClientM a -> ClientApp a
runSingleClient func = do
    (BaseUrl baseUrl) <- grab @BaseUrl
    liftIO $ do
        man <- newManager defaultManagerSettings
        let cenv = Servant.mkClientEnv man baseUrl
        S.withClientM func cenv $ \e ->
                    case e of
                        Left servantErr -> do
                            traceShowM "FUCKKER"
                            traceShowM servantErr
                            ---- NOT SAFE
                            throwIO servantErr
                        Right a -> pure a

runSingleClientStream func func2 = do
    (BaseUrl baseUrl) <- grab @BaseUrl
    liftIO $ forkIO $ do
        man <- newManager defaultManagerSettings
        let cenv = Servant.mkClientEnv man baseUrl
        S.withClientM func cenv $ \e ->
                    case e of
                        Left servantErr -> do
                            traceShowM "FUCKKER"
                            traceShowM servantErr
                            ---- NOT SAFE
                            throwIO servantErr
                        Right a -> do
                            traceShowM "was here"
                            func2 a





streamDumpClient :: ClientApp ()
streamDumpClient = withToken $ \t -> do
    (StreamDump streamDump) <- grab @StreamDump
    (GetDump getDump) <- grab @GetDump
    (HDump   hDump ) <- grab @HDump

    let go !acc SO.Stop        = return acc
        go !acc (SO.Error err) = liftIO (print err) >> return acc
        go !acc (SO.Skip s)    = go acc s
        go !acc (SO.Effect ms) = ms >>= go acc
        go !acc (SO.Yield _ s) = go (traceShow acc acc + 1) s

    gg <- runSingleClientStream streamDump $ \gg -> do
                                        SO.unSourceT gg (go (0 :: Int))
                                        return ()
    return ()


getGradesClient :: ClientApp ()
getGradesClient = withToken $ \t -> do
    (GetGrades getGrades) <- grab @GetGrades
    (HGrades   hGrades  ) <- grab @HGrades
    res                   <- runSingleClientOrErr $ getGrades t
    case res of
        _ -> liftIO $ hGrades (Just res)

getLocationClient :: ClientApp ()
getLocationClient = withToken $ \t -> do
    (GetLocation getLocation) <- grab @GetLocation
    (HLocation   hLocation  ) <- grab @HLocation
    res                       <- runSingleClientOrErr $ getLocation t
    case res of
        _ -> liftIO $ hLocation (Just res)


getSessionsClient :: ClientApp ()
getSessionsClient = withToken $ \t -> do
    (GetSessions getSessions) <- grab @GetSessions
    (HSessions   hSessions  ) <- grab @HSessions
    res                       <- runSingleClientOrErr $ getSessions t
    case res of
        _ -> liftIO $ hSessions (Just res)


getShootingsClient :: ClientApp ()
getShootingsClient = withToken $ \t -> do
    (GetShootings getShootings) <- grab @GetShootings
    (HShootings   hShootings  ) <- grab @HShootings
    res                         <- runSingleClientOrErr $ getShootings t
    case res of
        _ -> liftIO $ hShootings (Just res)

getCamerasClient :: ClientApp ()
getCamerasClient = withToken $ \t -> do
    (GetCameras getCameras) <- grab @GetCameras
    (HCameras   hCameras  ) <- grab @HCameras
    res                     <- runSingleClientOrErr $ getCameras t
    case res of
        _ -> liftIO $ hCameras (Just res)

getDoneshootingClient :: ClientApp ()
getDoneshootingClient = withToken $ \t -> do
    (GetDoneshooting getDoneshooting) <- grab @GetDoneshooting
    (HDoneshooting   hDoneshooting  ) <- grab @HDoneshooting
    res                               <- runSingleClientOrErr $ getDoneshooting t
    case res of
        _ -> liftIO $ hDoneshooting (Just res)


getDagsdatoClient :: ClientApp ()
getDagsdatoClient = withToken $ \t -> do
    (GetDagsdato getDagsdato) <- grab @GetDagsdato
    (HDagsdato   hDagsdato  ) <- grab @HDagsdato
    res                       <- runSingleClientOrErr $ getDagsdato t
    case res of
        _ -> liftIO $ hDagsdato (Just res)

getDagsdatoBackupClient :: ClientApp ()
getDagsdatoBackupClient = withToken $ \t -> do
    (GetDagsdatoBackup getDagsdatoBackup) <- grab @GetDagsdatoBackup
    (HDagsdatoBackup   hDagsdatoBackup  ) <- grab @HDagsdatoBackup
    res <- runSingleClientOrErr $ getDagsdatoBackup t
    case res of
        _ -> liftIO $ hDagsdatoBackup (Just res)


getDumpClient :: ClientApp ()
getDumpClient = do
    withToken $ \t -> do
        (GetDump getDump) <- grab @GetDump
        (HDump   hDump  ) <- grab @HDump
        res               <- runSingleClientOrErr $ getDump t
        case res of
            _ -> liftIO $ hDump (Just res)


getTabsClient :: ClientApp ()
getTabsClient = do
    withToken $ \t -> do
        (GetTabs getTabs) <- grab @GetTabs
        (HTabs   hTabs  ) <- grab @HTabs
        res               <- runSingleClientOrErr $ getTabs t
        case res of
            _ -> liftIO $ hTabs (Just res)


getPhotographersClient :: ClientApp ()
getPhotographersClient = do
    withToken $ \t -> do
        (GetPhotographers getPhotographers) <- grab @GetPhotographers
        (HPhotographers   hPhotographers  ) <- grab @HPhotographers
        res <- runSingleClientOrErr $ getPhotographers t
        case res of
            _ -> liftIO $ hPhotographers (Just res)


loginClient :: ClientApp ()
loginClient = do
    (Login login                       ) <- grab @Login
    (Headers NoContent (HCons res HNil)) <- runSingleClientOrErr $ login LoginForm
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
