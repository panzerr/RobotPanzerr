module TextCommands where
import Data.Text
import System.Random
import qualified Pnj as Pnj
import qualified Elric as Elric

control :: StdGen -> Text -> (String,StdGen)

control gen txt = case () of
  () | isPrefixOf (pack "#gygax") txt -> Pnj.gygax gen
     | isPrefixOf (pack "#elric") txt -> Elric.elricgen gen
     | otherwise -> ((unpack txt) ++ " n'est pas une commande valide",gen)
