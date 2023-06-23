
{-# LANGUAGE 
      DuplicateRecordFields
    , LambdaCase
    , DeriveGeneric
    , OverloadedStrings
    , BangPatterns
    , ViewPatterns
    , ScopedTypeVariables
    #-} 

module Lightning.Candidates where 

import Lightning.Graph
import Lightning.Search
import Lightning.Util
import Lightning.Generic
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Query.BFS as B
import Control.Applicative (liftA2) 
import Data.Text (Text) 
import Data.Function
import Data.List 
import GHC.Generics
import Data.Aeson
import Control.Monad.State

data Destination = Destination {
    _id :: Text
  , _amount :: Sat
} deriving (Show, Eq, Generic) 
instance ToJSON Destination where 
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v
instance FromJSON Destination where 
    parseJSON = defaultParse

suggest :: Gra -> Node -> [Destination] 
suggest g n = 
       map toSuggest
     $ filter suggestable
     $ map (context g . fst)  
     $ sortBy (on (flip compare) snd) 
     $ B.level n g 
 
toSuggest :: Cxt -> Destination
toSuggest c@(_, _, Vertex i, _) = Destination i (suggestAmt c)

suggestAmt :: Cxt -> Sat 
suggestAmt (_, _ , _, oo) = (`div` 1000) . msats . fst . head . drop 5 $ sortBy (on (flip compare) (msats . fst)) oo 

suggestable :: Cxt -> Bool
suggestable (ii, _, _, oo) = 
    let countIn = length ii 
        countOut = length oo 
        totalIn = total ii 
        totalOut = total oo 
    in         countIn > minChan
            && countOut > minChan
            && totalIn > minSize 
            && totalOut > minSize 

total = sum . map (msats . fst)
minChan = 11
minSize = 10000000000

type Dests = ([Destination], Sat, [Destination])

collectD :: State Dests [Destination] 
collectD = do 
    (l, t, c) <- get
    case l of 
        [] -> pure c 
        x:lx -> 
            let newT = t - (_amount x)
            in if newT > 10000 
               then put (lx, newT, x:c) >> collectD
               else pure c 









