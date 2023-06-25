{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module BatchUI where



import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Database.SQLite.Simple

import BatchDb

import Monomer
import qualified Monomer.Lens as L
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import BatchDb



data AppModel = AppModel {
     _clickCount :: Int
  , _batch :: [Batch]
} deriving (Eq)
data AppEvent
  = AppInit
  | AppIncrease

makeLenses 'AppModel

todoRow wenv model idx ba = hstack [
        spacer
      , label (_nodeId ba)
      , case _amount ba of 
        Just a -> label $ showt  a
        _ -> label "" 
      , label $ showt (_avoid ba)   -- (_nodeId ba)
    ]

buildUI wenv model = vstack [
    label "Hello world",
    spacer,
    vstack $ zipWith (todoRow wenv model) [0..] (model ^. batch),
    spacer,
    hstack [
      label $ "Click count: " <> showt (model ^. clickCount),
      button "+++" AppIncrease
    ]
  ]  

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]

startMonomer conn = do
  bx <- lookupBatches conn
  -- let model = AppModel 5 bx
  startApp (AppModel 5 bx) handleEvent buildUI config
  where
    config = [
        appWindowTitle "fgln"
      , appTheme darkTheme
      , appInitEvent AppInit
      , appRenderOnMainThread
      , appFontDef "Regular" "/home/o/o/Code/monomer-starter/assets/fonts/Roboto-Regular.ttf"
      ]

