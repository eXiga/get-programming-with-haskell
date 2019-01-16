module Chapter4 where

import Data.List (sortBy)

type Name = (String, String)

type Names = [Name]

names :: Names
names =
  [ ("Ian", "Curtis")
  , ("Bernard", "Sumner")
  , ("Peter", "Hook")
  , ("Stephen", "Morris")
  , ("Jack", "Johnes")
  , ("Aaron", "Johnes")
  ]

namesComparator :: Name -> Name -> Ordering
namesComparator name1 name2 =
  if result == EQ
    then compare (fst name1) (fst name2)
    else result
  where
    result = compare (snd name1) (snd name2)

sortNames :: Names -> Names
sortNames = sortBy namesComparator

sfOffice :: Name -> String
sfOffice name =
  if snd name < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    nameText = fst name ++ " " ++ snd name

nyOffice :: Name -> String
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = fst name ++ " " ++ snd name

renoOffice :: Name -> String
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

washOffice :: Name -> String
washOffice name = nameText ++ " - PO Box 999 - Washington, DC 12345"
  where
    nameText = fst name ++ " " ++ snd name ++ " Esq."

getLocationFunction :: String -> Name -> String
getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> washOffice
    _ -> \name -> fst name ++ " " ++ snd name

addressLetter :: Name -> String -> String
addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location
