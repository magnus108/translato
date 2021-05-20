module Format where

import           Model
import           Safe                           ( atMay )

import           Graphics.UI.Threepenny.Core
import qualified Control.Comonad.Store as Store


lookup :: PrintfType a => String -> a
lookup key = format $ Store.peek (Position key) . Store.lower . unRun


title :: WriteAttr Window Translation
title = mkWriteAttr
    $ \s _ -> runFunction $ ffi "document.title = %1;" (unTranslation s)


presentation :: WriteAttr Element Translation
presentation = mkWriteAttr
    $ \s el -> runFunction $ ffi "$(%1).text(%2)" el (unTranslation s)


mkPresentation :: Behavior Run -> String -> UI Element
mkPresentation bRun key = mkElement "span" # sink presentation (lookup key <$> bRun)


-------------------------------------------------------------------------------
format' :: (Run -> Translation) -> [String] -> (Run -> Translation)
format' f args = f & \g x -> Translation $ go (unTranslation (g x))
  where
    at xs i = maybe " " id $ atMay xs i
    argument i = args `at` i

    go []               = []
    go ('%' : '%' : cs) = '%' : go cs
    go ('%' : c   : cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c : cs) = c : go cs


format :: PrintfType a => (Run -> Translation) -> a
format f = fancy (format' f)


class PrintfType a where
    fancy :: ([String] -> (Run -> Translation)) -> a

instance PrintfType (Run -> Translation) where
    fancy f = f []

instance (PrintfType r) => PrintfType (String -> r) where
    fancy f x = fancy $ \xs -> f (x : xs)
