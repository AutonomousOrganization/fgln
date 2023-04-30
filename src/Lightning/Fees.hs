
module Lightning.Fees where

import Control.Plugin
import Lightning.Graph
import Data.Lightning
import Data.Graph.Inductive.Graph


--setNewFee :: ForwardEvent -> PluginApp a ()
--setNewFee = do 
--    undefined





inFee :: Gra -> Node -> Fee
inFee g n = avgFee (fees.snd) $ lpre g n 

outFee g n = avgFee (fees.snd) $ lsuc g n 

addFee (Fee b p) (Fee b' p') = Fee (b + b') (p + p') 

divFee 0 _ = Fee 0 0 
divFee y (Fee b p) = Fee (div b y) (div p y) 

avgFee :: (a->Fee) -> [a] -> Fee
avgFee f ax = divFee (length ax) 
    $ foldr addFee (Fee 0 0) 
    $ map f ax


in2Fee :: Gra -> Node -> Fee
in2Fee g n = avgFee id $ map (inFee g . fst) $ lpre g n


out2Fee :: Gra -> Node -> Fee
out2Fee g n = avgFee id $ map (outFee g . fst) $ lsuc g n 
