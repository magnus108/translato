module Main where

import qualified Lib


import           Safe                           ( atMay )
import qualified Control.Lens                  as Lens
import           Options.Generic
import           Data.Aeson

import           Control.Comonad
import           Control.Comonad.Trans.Store
import           Control.Comonad.Trans.Env
import           Control.Comonad.Store.Class
import qualified Control.Comonad.Store.Class   as Store
import           Control.Comonad.Env.Class
import qualified Control.Comonad.Env.Class     as Env


import qualified Data.Map.Strict               as M


-------------------------------------------------------------------------------
data Config = Config { _port :: Int }
    deriving Show
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)
    deriving anyclass ParseRecord


-------------------------------------------------------------------------------

translations :: M.Map String String
translations = M.fromList [("lol", "fuck"), ("loller", "lollo")]

lookup :: Store String (Maybe String)
lookup = store (\translation -> M.lookup translation translations) "lol"

lookup3 :: M.Map String String -> String -> Store String (Maybe String)
lookup3 translations' = store (\translation -> M.lookup translation translations')

lookup2
    :: (ComonadStore String w, ComonadEnv Style w)
    => w (Maybe String)
    -> Maybe String
lookup2 = extract

lookup4
    :: (ComonadStore String w)
    => w (Maybe String)
    -> Maybe String
lookup4 w = Store.seek "loller" w & extract
-------------------------------------------------------------------------------

translations' :: EnvT Style (Store String) (Maybe String)
translations' = EnvT Normal lookup

-------------------------------------------------------------------------------
data Style = Translating | Normal -- Der er altid 1 i focus
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)


data Translations = Translations { _unTranslation :: EnvT Style (Store String) (Maybe String) }
    deriving Generic

instance ToJSON Translations where
    toJSON (Translations p) =
        object
            $ [ "style" .= style p
              , "position" .= position p
              , "map" .= translations
              ]

instance FromJSON Translations where
    parseJSON = withObject "translation" $ \o -> do
        style    <- o .: "style"
        position <- o .: "position"
        map <- o .: "map"
        return $ Translations $ EnvT style $ lookup3 map position

-------------------------------------------------------------------------------
position :: (ComonadStore String w) => w a -> String
position = Store.pos

style :: (ComonadEnv Style w) => w a -> Style
style = Env.ask
-------------------------------------------------------------------------------

main :: IO ()
main = do

    let gg   = translations'
    let enco = encode (Translations gg)
    let haha = decode (enco) :: Maybe Translations
    putStrLn (show (isJust haha))
    let ggg = lookup2 gg
    putStrLn (show ggg)
    putStrLn (show enco)

    Config (_port) <- getRecord "Run"
    Lib.run _port


-------------------------------------------------------------------------------
format' :: String -> [String] -> String
format' code args = go code
  where
    at xs i = maybe " " id $ atMay xs i
    argument i = args `at` i

    go []               = []
    go ('%' : '%' : cs) = '%' : go cs
    go ('%' : c   : cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c : cs) = c : go cs

format :: PrintfType a => String -> a
format x = fancy (format' x)

class PrintfType a where
    fancy :: ([String] -> String) -> a

instance PrintfType String where
    fancy f = f []

instance (PrintfType r) => PrintfType (String -> r) where
    fancy f x = fancy $ \xs -> f (x : xs)
-------------------------------------------------------------------------------
