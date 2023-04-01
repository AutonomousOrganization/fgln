
{-# LANGUAGE 
      DuplicateRecordFields
    , LambdaCase
    , DeriveGeneric
    , OverloadedStrings
    , BangPatterns
    , ViewPatterns
    #-}

module Graph (Gra, Vertex, Hop, createGraph, getNodeInt) where 

import Control.Plugin (InitMonad) 
import Control.Client
import Data.Lightning.Generic
import Data.Aeson
import Numeric 
import Data.Char
import GHC.Generics 
import Data.Text (Text) 
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type Gra = Gr Vertex Hop

createGraph :: InitMonad Gra
createGraph = do 
    Just (Res xn _) <- lightningCli (Command "listnodes" (Just nfilt) (object []))
    Just (Res xc _) <- lightningCli (Command "listchannels" (Just cfilt) (object []))
    case (fromJSON xn, fromJSON xc) :: (Result Nodes, Result Chans) of
        (Success nx, Success cx) -> pure $ mkGraph 
            (map toLNode (_nodes nx)) (map toLEdge' (channels cx))     
        _ -> pure empty
        where 
              toLNode :: Vertex -> (Node, Vertex)
              toLNode ni = ((getNodeInt.nodeid) ni , ni)
              
              toLEdge' :: Chan -> (Node, Node, Hop) 
              toLEdge' c = (
                      (getNodeInt. __source) c
                    , (getNodeInt. __destination) c
                    , toHop c
                    )
              nfilt = object ["nodes" .= [object ["nodeid" .= True] ] ]
              cfilt = object ["channels" .= [object [
                    "source" .= True
                  , "destination" .= True
                  , "short_channel_id" .= True
                  ]]]

              toHop a = Hop $ __short_channel_id a

data Vertex = Vertex {
    nodeid :: Text
    } deriving (Show, Eq, Generic) 
instance FromJSON Vertex where
    parseJSON = defaultParse
instance ToJSON Vertex 

data Hop = Hop {
    short_channel_id :: Text
    } deriving (Show, Eq, Generic)
instance FromJSON Hop where 
    parseJSON = defaultParse 
instance ToJSON Hop 

getNodeInt :: Text -> Node
getNodeInt s = case readHex.filter isHexDigit $ show s of
    ([]) -> 0
    (x:_)-> fst x

data Nodes = M {
    _nodes :: [Vertex]  
    } deriving Generic 
instance FromJSON Nodes where
    parseJSON = defaultParse
data Chans = CC {
      channels :: [Chan] 
    } deriving Generic
instance FromJSON Chans
data Chan = Chan {
      __source :: Text
    , __destination :: Text
    , __short_channel_id :: Text
    } deriving Generic
instance FromJSON Chan where 
    parseJSON = defaultParse
              
