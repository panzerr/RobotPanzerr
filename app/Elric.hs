module Elric where
import qualified Trie as Trie
import System.Random
import Control.Monad
import Pnj

data RollTableElem a = RT(Integer , Integer , a) -- min, max, something


data Echaracter = EC([String],Trie.Trie Integer,Trie.Trie Integer,Trie.Trie Integer) -- les stats/stats secondaires/comps du perso

-- some dices TODO : move to Dice.hs

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

--Useful things
diceLift :: (Echaracter -> Echaracter) -> (Echaracter , StdGen) -> (Echaracter , StdGen)

diceLift f (char,gen) = (f char, gen)


-- Modifying character with dice

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



condMod :: (Echaracter -> Bool) -> ( (Echaracter,StdGen) -> (Echaracter,StdGen) ) -> (Echaracter,StdGen) -> (Echaracter,StdGen)

condMod cond mod (char,gen) =
  if (cond char) then mod (char,gen)
  else (char,gen)

testStat :: (Integer -> Bool) -> String -> Echaracter -> Bool

testStat f name (EC(_,stats,_,_)) =
    let rep = Trie.fetch name stats in
      case rep of
        Nothing -> False --again this should not happen
        Just(prevval) -> f prevval

lol = condMod (testStat ((<=) 10) "Intelligence") $ addDiceStat (invD.d6) "Intelligence"


-- getting stats

getStats :: Echaracter -> Trie.Trie Integer

getStats (EC(_,stats,_,_)) = stats -- should have done that sooner

insertSec :: Echaracter -> String -> Integer -> Echaracter

insertSec (EC(infos,stats,secondstats,skills)) name val = (EC(infos,stats, Trie.insert name val secondstats,skills))

-- generate secondary stats


eMod :: Integer -> Integer

eMod n = if n <= 9 then n - 9 else
           if n >= 12 then n - 12 else 0

data Emodcalc =
  EA ( String)
  | ES ( String)


calcMod :: Echaracter  -> [Emodcalc] -> Integer

calcMod  char [] = 0

calcMod char ((EA(name)):tail) = 
    let rep = Trie.fetch name $ getStats char in
      case rep of
        Nothing -> 0 --again this should not happen
        Just(val) -> (calcMod char tail) + (eMod val)

calcMod char ((ES(name)):tail) = 
    let rep = Trie.fetch name $ getStats char in
      case rep of
        Nothing -> 0 --again this should not happen
        Just(val) -> (calcMod char tail) - (eMod val)

attaque = [EA("Force"), EA ("Intelligence") , EA("Pouvoir") , EA ("Dexterite")]
parade = [EA("Force"), ES ("Taille") , EA("Pouvoir") , EA ("Dexterite")]
agilite = parade
manipulation = attaque
perception = [EA("Pouvoir"), EA ("Intelligence")]
discretion = [ES("Taille"), EA ("Intelligence") , EA ("Dexterite")]
connaissance = [EA ("Intelligence"), EA ("Intelligence")]
communication = [ EA ("Intelligence") , EA("Pouvoir") , EA ("Charisme")]

secondStats = [("Attaque",attaque), ("Parade",parade) , ("Agilite",agilite) , ("Manipulation", manipulation) , ("Perception", perception), ("Discretion", discretion) , ("Connaissance",connaissance), ("Communication", communication) ]

calcSecStats :: Echaracter -> [(String,[Emodcalc])] -> Echaracter

calcSecStats char [] = char

calcSecStats char ((name,calc):nextStat) =
  calcSecStats ( insertSec char name (calcMod char calc) ) nextStat

secondaries :: (Echaracter , StdGen) -> (Echaracter , StdGen)

secondaries (char , gen ) = ((calcSecStats char secondStats), gen)
  
-- classes functions



-- nationalities table
          
enational = [
  RT(1,2, [ addNat "Melnibone", secondaries , addDiceStat d10 "Intelligence", addDiceStat d6 "Pouvoir", addDiceStat d6 "Intelligence", addFlatStat 3 "Taille" ]),
  RT(3,5, [ addNat "Pan Tang", secondaries , addDiceStat d8 "Intelligence", addDiceStat d8 "Pouvoir", addFlatStat 1 "Taille" ]),
  RT(6,8, [ addNat "Murrhyn", secondaries , addDiceStat d6 "Intelligence", addDiceStat d6 "Pouvoir", addDiceStat d6 "Charisme", subTai 9 2 ]),
  RT(9,12, [ addNat "Dharijor", secondaries , addDiceStat d4 "Constitution" ]),
  RT(13,16, [ addNat "Jharkor", secondaries , addDiceStat (invD.d4) "Charisme", addDiceStat d4 "Dexterite" ]),
  RT(17,24, [ addNat "Shazaar", secondaries , addDiceStat d6 "Constitution" ]),
  RT(25,29, [ addNat "Tarkesh" , secondaries , addDiceStat d4 "Constitution" , subTai 10 1 ]),
  RT(30,37, [ addNat "Vilmir" , secondaries ]),
  RT(38,44, [ addNat "Ilmiora" , secondaries , addDiceStat d4 "Charisme" ]),
  RT(45,49, [ addNat "Nadsokor" , secondaries , addDiceStat (invD.d6) "Constitution" , addDiceStat (invD.d6) "Charisme" ]),
  RT(50,56, [ addNat "Desert des Larmes" , secondaries , addDiceStat d6 "Force" , addDiceStat d4 "Dexterite" , addDiceStat d6 "Constitution" , addDiceStat (invD.d4) "Charisme" , subTai 10 1  ]),
  RT(57,60, [ addNat "Eshmir" , secondaries , addDiceStat d4 "Intelligence", addDiceStat d6 "Pouvoir" , subTai 10 2 ]),
  RT(61,67, [ addNat "Ile des Cites Pourpres" , secondaries , addDiceStat d4 "Force" , addDiceStat d6 "Constitution" ]),
  RT(68,74, [ addNat "Argimiliar" , secondaries   ]),
  RT(75,81, [ addNat "Pikarayd" , secondaries , addDiceStat d4x2 "Force" , addFlatStat 1 "Taille" ]),
  RT(82,88, [ addNat "Lormyr" , secondaries , addDiceStat (invD.d4x2) "Intelligence" , addFlatStat 2 "Taille"]),
  RT(89,95, [ addNat "Filkhar" , secondaries , addDiceStat d4 "Dexterite" ]),
  RT(96,97, [ addNat "Oin" , secondaries , condMod (testStat ((<=) 10) "Intelligence") $ addDiceStat (invD.d6) "Intelligence" , condMod (testStat ((<=) 10) "Dexterite") $ addDiceStat (invD.d6) "Dexterite", condMod (testStat ((<=) 10) "Pouvoir") $ addDiceStat (invD.d6) "Pouvoir" , addDiceStat d6 "Constitution" ]),
  RT(98,99, [ addNat "Yu" , secondaries , condMod (testStat ((<=) 10) "Intelligence") $ addDiceStat (invD.d6) "Intelligence" , condMod (testStat ((<=) 10) "Dexterite") $ addDiceStat (invD.d6) "Dexterite", condMod (testStat ((<=) 10) "Pouvoir") $ addDiceStat (invD.d6) "Pouvoir" , condMod (testStat ((<=) 10) "Charisme") $ addDiceStat (invD.d6) "Charisme" , addDiceStat d6 "Constitution", addDiceStat d6 "Force"]),
  RT(100,100, [ addNat "Org" , secondaries , condMod (testStat ((<=) 10) "Intelligence") $ addDiceStat (invD.d6) "Intelligence" , condMod (testStat ((<=) 10) "Dexterite") $ addDiceStat (invD.d6) "Dexterite", condMod (testStat ((<=) 10) "Pouvoir") $ addDiceStat (invD.d4x2) "Pouvoir" , condMod (testStat ((<=) 10) "Charisme") $ addDiceStat (invD.d6) "Charisme" , addDiceStat d8 "Constitution", addDiceStat d4 "Force" , subTai 10 2 ])
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


-- chain transformations note : does the first transfo first

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
          
  
