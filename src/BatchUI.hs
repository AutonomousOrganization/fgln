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

import qualified Monomer.Lens as L
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import BatchDb
import Data.Text.Format.Numbers
import Lightning.Candidates hiding (_amount)
import Data.Aeson

data AppModel = AppModel {
    _outputs:: Int
  , _batch :: [Batch]
} deriving (Eq)
data AppEvent
  = AppInit
  | AppIncrease
  | AddNodes [Batch]
  | Pick Text
  | Skip Text
  | Deploy

makeLenses 'AppModel

todoRow ba = hstack [
        spacer
      , hstack [
            case _pick ba of 
                Nothing -> bullet `styleBasic` [textColor yellow]
                Just True -> bullet `styleBasic` [textColor green]
                Just False -> bullet `styleBasic` [textColor red]
          , label $ prettyI (Just ',') sats
          , spacer
          , label rowId
          , spacer
          , button "." (Pick rowId) `styleBasic` [bgColor green]
          , spacer
          , button "." (Skip rowId) `styleBasic` [bgColor red]
      ]
    ]
    where rowId = _nodeId ba
          sats = fromIntegral $ _amount ba
          bullet = label "  ###>  "

buildUI wenv model = scroll $ vstack [
      label "Batch Open UI" `styleBasic` [textSize 30, textCenter] 
    , button ("Confirm Open of " <> prettyI (Just ',') batchTotal) AppIncrease   
    , spacer
    , button "fetcha" (AddNodes [Batch "test" 23232323 Nothing, Batch "ahhhh" 333 (Just False), Batch "nodeid" 231231 (Just True) ])
    , spacer
    , button "fetch1" (Pick "nononononono")
    , vstack $ map todoRow selected
    , spacer 
    , label "excluded:" `styleBasic` [textColor lightSalmon, borderB 3 lightSalmon]
    , vstack $ map todoRow banned
    , label $ prettyI Nothing (model ^. outputs)
    ]  
  where selected = filter picked $ model ^. batch
        banned = filter (not <$> picked) $ model ^. batch    
        batchTotal = sum . map (fromIntegral . _amount) $ selected
        picked (Batch _ _ (Just False)) = False
        picked _ = True 
-- handleEvent
--   :: WidgetEnv AppModel AppEvent
--   -> WidgetNode AppModel AppEvent
--   -> AppModel
--   -> AppEvent
--   -> [AppEventResponse AppModel AppEvent]
handleEvent cli wenv node model evt = case evt of
  AppInit -> [
      -- Task $ do 
      -- -- (fromJSON -> Success d@(Destination _ _ :_))
      -- -- _ <- cli "candidates" Nothing Nothing -- (Just$ object []) (Just $ object []) 
      -- pure $ AddNodes [ Batch "test" 1231231 Nothing
      --                 , Batch "test2" 12345 (Just False)] -- AddNodes $ map (\(Destination n m)-> Batch n (fromIntegral m) Nothing) d 
      ]
  AppIncrease -> [Model (model & outputs +~ 1)]
  AddNodes b -> [Model $ model & (batch .~ (b <> model ^. batch))]
  Pick p -> [Task $ pure $ AddNodes [Batch "test" 1231231 (Just False) ]]
  Skip s -> []
  Deploy -> [] 

startMonomer conn cli = do
  -- bx <- lookupBatches conn
  -- let model = AppModel 5 broadcastDoubleX8#
  startApp (AppModel 5 []) (handleEvent cli) buildUI config
  where
    config = [
        appWindowTitle "fgln"
      , appTheme darkTheme
      , appInitEvent AppInit
      , appFontDef "Regular" "/home/o/o/Code/monomer-starter/assets/fonts/Roboto-Regular.ttf"
      ]

