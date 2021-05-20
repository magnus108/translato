module Lib.Server.Handler.GetDocs where

import           Servant.HTML.Blaze
import           Text.Blaze                    as HTML
import           Text.Blaze.Html               as HTML
import           Lib.Server.Types
import           Lib.Api.Types
import           Lib.Api

import qualified Servant.Docs                  as Docs
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as LT
import qualified Text.Markdown                 as Markdown

serveGetDocs :: ServerApp GetDocsResponse
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

docs' :: Docs.API
docs' = Docs.docs publicSiteAPI
