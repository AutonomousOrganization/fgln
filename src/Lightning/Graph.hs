
{-# LANGUAGE 
      DuplicateRecordFields
    , LambdaCase
    , DeriveGeneric
    , OverloadedStrings
    , BangPatterns
    , ViewPatterns
    #-}

module Lightning.Graph (Gra, Cxt, Vertex(..), Hop(..), Fee (..), createGraph, getNodeInt) where 

import Lightning.Util
import Lightning.Generic 
import Data.Aeson
import Numeric 
import Data.Char
import GHC.Generics 
import Data.Text (Text) 
import Control.Applicative (liftA2) 
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Fmt
import Control.Monad.IO.Class

type Gra = Gr Vertex Hop
type Cxt = Context Vertex Hop
data Vertex = Vertex {
    nodeid :: Text
    } deriving (Show, Eq, Generic) 
instance FromJSON Vertex where
    parseJSON = defaultParse
instance ToJSON Vertex 

data Hop = Hop {
      short :: Text
    , fees :: Fee
    , msats :: Msat
    , direction :: Int
    , delay :: Int
    } deriving (Show, Eq, Generic)
instance FromJSON Hop where 
    parseJSON = defaultParse 
instance ToJSON Hop 

data Fee = Fee {
      base :: Msat
    , ppm ::  Int
    } deriving (Show, Eq, Generic) 
instance FromJSON Fee
instance ToJSON Fee where 
    toJSON (Fee b p)= toJSON $ ( "(" +| build b +| "," +| build p +| ") " :: Text ) 

-- createGraph :: InitMonad Gra
createGraph cli = do 
    xn <- liftIO $ cli "listnodes" Nothing nfilt
    xc <- liftIO $ cli "listchannels" Nothing cfilt
    case (fromJSON xn, fromJSON xc) :: (Result Nodes, Result Chans) of
        (Success nx, Success cx) -> pure $ mkGraph 
            (map toLNode (_nodes nx)) 
            (map toLEdge' (channels cx))     
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
              nfilt = Just $ object ["nodes" .= [object ["nodeid" .= True] ] ]
              cfilt = Just $ object ["channels" .= [object [
                    "source" .= True
                  , "destination" .= True
                  , "short_channel_id" .= True
                  , "base_fee_millisatoshi" .= True
                  , "fee_per_millionth" .= True
                  , "amount_msat" .= True
                  , "direction" .= True
                  , "delay" .= True
                  ]]]

              toHop a = Hop (__short_channel_id a)
                            (liftA2 Fee __base_fee_millisatoshi __fee_per_millionth a) 
                            (__amount_msat a)
                            (__direction a)
                            (__delay a)
    
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
    , __base_fee_millisatoshi :: Msat
    , __fee_per_millionth :: Int
    , __amount_msat :: Msat 
    , __direction :: Int 
    , __delay :: Int
        } deriving Generic
instance FromJSON Chan where 
    parseJSON = defaultParse
              
-- | Get the Node of the graph virtex from the nodeid. Result is unique but my overflow into negative value. 
getNodeInt :: Text -> Node
getNodeInt s = case readHex.filter isHexDigit $ show s of
    ([]) -> 0
    (x:_)-> fst x
