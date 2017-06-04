module TextCommands where
import Data.Text
import System.Random
import qualified Pnj as Pnj

control :: StdGen -> Text -> (String,StdGen)

control gen txt = case () of
  () | isPrefixOf (pack "#gygax") txt -> Pnj.gygax gen
     | otherwise -> ((unpack txt) ++ " n'est pas une commande valide",gen)
  
