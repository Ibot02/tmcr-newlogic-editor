{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import TMCR.Logic.Descriptor
import TMCR.Logic.Common
import Miso
#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
#endif
import TMCR.IO (Directory(..), withPath, runInMemoryDir)
import TMCR.Module (ResourceSpecifier)
import TMCR.Logic.Merge (GameDef)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Polysemy as P
import Polysemy.Error (runError)
import Miso.String (toMisoString)
import Data.Text as T

#ifndef __GHCJS__
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp = id
#endif

main :: IO ()
main = runApp $ startApp App {..} where
    initialAction = ()
    model = defaultModel
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs  = []
    mountPoint = Nothing
    logLevel = Off


data Model = Model {
      _inputFiles :: Directory
    , _currentFile :: [FilePath]
    , _currentRoom :: Maybe String
    , _currentGameDef :: Maybe GameDef
    , _compilerOutput :: T.Text
    , _descriptorOverridesTruthy :: Map (DescriptorIdent Truthy) Oolean --todo keylogic
    , _descriptorOverridesCounty :: Map (DescriptorIdent County) (Nteger, Nteger) --value, value ool
    } deriving (Eq, Ord)

defaultModel :: Model
defaultModel = Model {..} where
    _inputFiles = Directory mempty
    _currentFile = []
    _currentRoom = Nothing
    _currentGameDef = Nothing
    _compilerOutput = ""
    _descriptorOverridesTruthy = mempty
    _descriptorOverridesCounty = mempty


updateModel _ model = return model
viewModel Model {..} = div_ [] [
      div_ [id_ "content-view"] [

      ],
      div_ [id_ "module-view"] [
          div_ [id_ "file-editor"] (viewEditor _currentFile _inputFiles _compilerOutput)
        , div_ [id_ "file-list"] (viewFiles _inputFiles)
        , div_ [id_ "import-files"] viewImport
      ]
    ]

viewEditor currentFile inputFiles compilerOutput = [
      div_ [id_ "tab-bar"] (fmap viewTab currentFile)
    , div_ [id_ "text-editor"] (viewTextEditor currentFile inputFiles compilerOutput)
    ]

viewTab name = div_ [class_ "tab"] [
      text (toMisoString name)
    , button_ [class_ "tab-close"] [text "x"]
    ]

viewTextEditor current files compilerOutput = [
      viewTextEditorTextArea current files
    , div_ [id_ "compiler-output"] [
            button_ [id_ "recompile", onClick undefined] [text "Exec"]
        , textarea_ [readonly_ True] [text compilerOutput]
        ]
    ]
viewTextEditorTextArea [] _ = textarea_ [readonly_ True] []
viewTextEditorTextArea (name:_) dir = case getFileContent name dir of
    Nothing -> textarea_ [readonly_ True] []
    Just content -> textarea_ [readonly_ False] [text content]

getFileContent filename dir = h $ P.run $ runError @() $ runInMemoryDir dir $ withPath filename (\_ content -> return content) where
    h (Left _) = Nothing
    h (Right []) = Nothing
    h (Right (x:y:_)) = Nothing
    h (Right (x:_)) = Just $ toMisoString x

viewFiles (Directory xs) = fmap h $ M.toList xs where
    h (name, Right _) = span_ [class_ "filename", onClick undefined] [text $ toMisoString name]
    h (name, Left dir) = details_ [class_ "subdir"] (summary_ [] [text $ toMisoString name] : viewFiles dir)

viewImport = []