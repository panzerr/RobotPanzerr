module Elric where
import qualified Trie as Trie
import System.Random
import Control.Monad
import Pnj

data RollTableElem a = RT(Integer , Integer , a) -- min, max, something


data Echaracter = EC([String],Trie.Trie Integer,Trie.Trie Integer,Trie.Trie Integer) -- les stats/stats secondaires/comps du perso


d100 :: StdGen -> (Integer, StdGen)

d100 gen = randomR (0,99) gen 

d10 :: StdGen -> (Integer, StdGen)

d10 gen = randomR (1,10) gen 


d8 :: StdGen -> (Integer, StdGen)

d8 gen = randomR (1,8) gen 

d4 :: StdGen -> (Integer, StdGen)

d4 gen = randomR (1,4) gen 

d4x2 :: StdGen -> (Integer, StdGen)

d4x2 gen = 
  sumDice d4 $ d4 gen

invD :: (Integer, StdGen) -> (Integer, StdGen)

invD (val,gen) = (-val,gen)

addDiceStat :: (StdGen -> (Integer, StdGen)) -> String -> (Echaracter,StdGen) -> (Echaracter,StdGen) --add a dice to a stat

addDiceStat dice name ((EC(infos,stats,secondstats,skills)),gen) =
  let (roll,newgen) = dice gen in
    let rep = Trie.fetch name stats in
      case rep of
        Nothing -> (EC(infos,stats,secondstats,skills),newgen) --again this should not happen
        Just(prevval) -> (EC(infos,Trie.insert name (prevval+roll) stats,secondstats,skills),newgen) -- should probably make functions to change part of a character

addInfo :: String -> (Echaracter,StdGen) -> (Echaracter,StdGen)

addInfo name ((EC(infos,stats,secondstats,skills)),gen) =
  (EC(name:infos,stats,secondstats,skills),gen)

addFlatStat :: Integer -> String -> (Echaracter,StdGen) -> (Echaracter,StdGen)

addFlatStat flat name ((EC(infos,stats,secondstats,skills)),gen) =
    let rep = Trie.fetch name stats in
      case rep of
        Nothing -> (EC(infos,stats,secondstats,skills),gen) --again this should not happen
        Just(prevval) -> (EC(infos,Trie.insert name (prevval+flat) stats,secondstats,skills),gen) -- should probably make functions to change part of a character

  
addNat name ec = addInfo ("Nationalite: "++name) ec 

eid :: (Echaracter,StdGen) -> (Echaracter,StdGen)

eid val = val

-- specific function to decreace size if superior to a threshold
subTai :: Integer -> Integer -> (Echaracter,StdGen) -> (Echaracter,StdGen)

subTai cond val ((EC(infos,stats,secondstats,skills)),gen) =
    let rep = Trie.fetch "Taille" stats in
      case rep of
        Nothing -> (EC(infos,stats,secondstats,skills),gen) --again this should not happen
        Just(prevval) ->if (prevval >= cond) then
                          (EC(infos,Trie.insert "Taille" (prevval-val) stats,secondstats,skills),gen) -- should probably make functions to change part of a character
                        else
                          (EC(infos,stats,secondstats,skills),gen)
                          
enational = [
  RT(1,2, [ addNat "Melnibone", addDiceStat d10 "Intelligence", addDiceStat d6 "Pouvoir", addDiceStat d6 "Intelligence", addFlatStat 3 "Taille" ]),
  RT(3,5, [ addNat "Pan Tang", addDiceStat d8 "Intelligence", addDiceStat d8 "Pouvoir", addFlatStat 1 "Taille" ]),
  RT(6,8, [ addNat "Murrhyn", addDiceStat d6 "Intelligence", addDiceStat d6 "Pouvoir", addDiceStat d6 "Charisme", subTai 9 2 ]),
  RT(9,12, [ addNat "Dharijor", addDiceStat d4 "Constitution" ]),
  RT(13,16, [ addNat "Jharkor", addDiceStat (invD.d4) "Charisme", addDiceStat d4 "Dexterite" ]),
  RT(17,24, [ addNat "Shazaar", addDiceStat d6 "Constitution" ]),
  RT(25,29, [ addNat "Tarkesh" , addDiceStat d4 "Constitution" , subTai 10 1 ]),
  RT(30,37, [ addNat "Vilmir" ]),
  RT(38,44, [ addNat "Ilmiora" ,  addDiceStat d4 "Charisme" ]),
  RT(45,49, [ addNat "Nadsokor" , addDiceStat (invD.d6) "Constitution" , addDiceStat (invD.d6) "Charisme" ]),
  RT(50,56, [ addNat "Desert des Larmes" , addDiceStat d6 "Force" , addDiceStat d4 "Dexterite" , addDiceStat d6 "Constitution" , addDiceStat (invD.d4) "Charisme" , subTai 10 1  ]),
  RT(57,60, [ addNat "Eshmir" , addDiceStat d4 "Intelligence", addDiceStat d6 "Pouvoir" , subTai 10 2 ]),
  RT(61,67, [ addNat "Ile des Cites Pourpres" , addDiceStat d4 "Force" , addDiceStat d6 "Constitution" ]),
  RT(68,74, [ addNat "Argimiliar"  ]),
  RT(75,81, [ addNat "Pikarayd" , addDiceStat d4x2 "Force" , addFlatStat 1 "Taille" ]),
  RT(82,88, [ addNat "Lormyr" , addDiceStat (invD.d4x2) "Intelligence" , addFlatStat 2 "Taille"]),
  RT(89,95, [ addNat "Filkhar" , addDiceStat d4 "Dexterite" ]),
  RT(96,97, [ addNat "Oin" ]), --TODO
  RT(98,99, [ addNat "Yu" ]),
  RT(100,100, [ addNat "Org" ])
  ]
  
-- gets a value out of an integer roll
getOnTable :: Integer -> [RollTableElem a] -> a

getOnTable roll [] = undefined --should not happen

getOnTable roll (RT(min,max,val):next) =
  if min <= roll && max >= roll then val
  else getOnTable roll next

estats = ["Charisme","Dexterite","Pouvoir","Intelligence","Taille","Constitution","Force"]

insertStat :: Stat -> Trie.Trie Integer -> Trie.Trie Integer

insertStat (S(name,value)) tr = Trie.insert name value tr

egenstats :: StdGen -> (Echaracter, StdGen)

egenstats gen =
  let (statlist,newgen) = genstats estats gen in
    (EC([] ,foldr insertStat Trie.EmptyT statlist, Trie.EmptyT, Trie.EmptyT),newgen)


-- chain transformations

recETransform :: [((Echaracter, StdGen) -> (Echaracter, StdGen))] -> (Echaracter,StdGen) -> (Echaracter, StdGen)

recETransform [] ec = ec

recETransform (hd:tl) ec = recETransform tl (hd ec)



-- Rendering

format_aux :: String -> Integer -> String -> String

format_aux name val acc = acc ++ " | " ++ name ++ ": " ++ (show val)

format :: Trie.Trie Integer -> String

format tr =
  (Trie.foldWithKey format_aux "" tr) ++ " |\n"

genhead :: [String] -> String

genhead [] = "\n"

genhead (head:tail) = head ++ ( genhead tail )

instance Show Echaracter where
  show (EC(infos ,stats, stats2, comp ))  =
   (genhead infos)  ++ (format stats) ++ (format stats2) ++ (format comp)

-- temp solution
printec (EC(infos ,stats, stats2, comp ))  =
  (genhead infos)  ++ (format stats) ++ (format stats2) ++ (format comp)


--Main func

elricgen :: StdGen -> (String, StdGen)

elricgen gen =
  let (character,gen2) =  egenstats gen in
    let (natroll,gen3) = d100 gen2 in
      let nat = getOnTable natroll enational in
        let (charfinal, genfinal) = recETransform nat (character,gen3) in
          (printec charfinal, genfinal)
          
  
