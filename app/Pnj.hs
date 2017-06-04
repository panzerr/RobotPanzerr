
module Pnj where
import qualified Trie as Trie
import System.Random
import Control.Monad

data Stat =S(String,Integer)

-- Maybe implement feats one day
data Feat = F (Stat -> Bool, Stat -> Stat)

d6 :: StdGen -> (Integer, StdGen)

d6 gen = randomR (1,6) gen 


sumDice :: (StdGen -> (Integer, StdGen)) -> (Integer,StdGen)  -> (Integer, StdGen)

sumDice f (rolled, newgen) =
  let (rolled2, newgen2) = f (newgen) in
  (rolled+rolled2,newgen2)

d6x3 :: StdGen -> (Integer, StdGen)

d6x3 gen = 
  sumDice d6 $ sumDice d6 $ d6 gen

stats = ["Charisme","Sagesse","Intelligence","Constitution","Dexterite","Force"]

genstats :: [String] -> StdGen -> ([Stat],StdGen)

genstats [] gen = ([],gen)

genstats (name:tail) gen =
  let (val, newgen) = d6x3 gen in
    let (valist, finalgen) = genstats tail newgen in
      (S(name,val):valist, finalgen)
      
readStats :: Stat -> String  -> String

readStats (S(name,value)) acc =  acc ++ " " ++ name ++ " " ++ (show value)

gygax :: StdGen -> (String, StdGen)

gygax gen = 
  let (statlist,newgen) = genstats stats gen in
    (foldr readStats "" statlist, newgen)
  
