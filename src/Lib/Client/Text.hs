{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Text
    ( entry
    , userText
    , TextEntry(..)
    )
where

import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI



data TextEntry = TextEntry
    { elementTE :: Element
    , userTE    :: Tidings String
    }

instance Widget TextEntry where
    getElement = elementTE

userText :: TextEntry -> Tidings String
userText = userTE

entry :: Behavior String -> UI TextEntry
entry bValue = do
    input    <- UI.input #. "input"


    bEditing <- stepper False $ and <$> unions
        [True <$ UI.focus input, False <$ UI.blur input]

    window <- askWindow

    liftIOLater $ runUI window $ do
        value' <- currentValue bValue
        void $ element input # set value value'

    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        editing <- liftIO $ currentValue bEditing
        when (not editing) $ void $ element input # set value s

    let elementTE = input
        userTE    = tidings bValue $ UI.valueChange input

    return TextEntry { .. }
