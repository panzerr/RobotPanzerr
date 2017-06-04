-- char trie lib

module Trie where
import qualified Data.Map.Lazy as Map


data Trie a =
  Node (Maybe a, Map.Map Char (Trie a))
  | EmptyT

auxf :: a -> a -> a

auxf new_val old_val = new_val

makeNode :: Maybe a -> Map.Map Char (Trie a) -> Trie a

makeNode value nextNodes = Node(value,nextNodes)

insert :: String -> a -> Trie a -> Trie a

insert [] value tr = makeNode (Just value) Map.empty


insert (keychar:next) value tr = case tr of
                                   EmptyT -> makeNode Nothing (Map.insert keychar (insert next value EmptyT) (Map.empty))
                                   Node ( invar, follow) ->
                                     case (Map.lookup keychar follow) of
                                       Nothing -> makeNode invar (Map.insert keychar (insert next value EmptyT) follow)
                                       Just(nextNode) -> makeNode invar (Map.insert keychar (insert next value nextNode) follow)

fetch :: String -> Trie a -> Maybe a

fetch [] tr = case tr of
  EmptyT -> Nothing
  Node (invar, _) -> invar

fetch (keychar:next) tr = case tr of
  EmptyT -> Nothing
  Node (_, follow) -> case (Map.lookup keychar follow) of
                      Nothing -> Nothing
                      Just(nextNode) -> fetch next nextNode

instance Show a => Show (Trie a) where
  show (Node(a,m)) = "(" ++ show a ++ " , " ++ show m ++ ")"
  show (EmptyT) = "null"


foldWithKeyAux :: String -> (String -> a -> b -> b ) -> b -> Trie a -> b

foldWithKeyAux keystring f acc EmptyT = acc

foldWithKeyAux keystring f acc (Node(val,follow)) =
  let aux keychar val2 acc2 = foldWithKeyAux (keychar:keystring) f acc2 val2 in
    let resp = Map.foldrWithKey aux acc follow in
      case val of
        Nothing -> resp
        Just( truc) -> f  (reverse keystring) truc resp
   
foldWithKey :: (String -> a -> b -> b ) -> b -> Trie a -> b

foldWithKey f acc t = foldWithKeyAux [] f acc t


testShow str var acc = str ++ " " ++ show var ++ " " ++ acc


                  
-- test = insert "lol" 42 EmptyT
