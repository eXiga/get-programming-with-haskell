module Chapter12 where

data Sex
  = Male
  | Female
  deriving (Show)

data RhType
  = Pos
  | Neg

instance Show RhType where
  show Pos = "+"
  show Neg = "-"

data ABOType
  = A
  | B
  | AB
  | O
  deriving (Show)

data BloodType =
  BloodType ABOType
            RhType

instance Show BloodType where
  show (BloodType abo rh) = show abo ++ show rh

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName

instance Show Name where
  show (Name f l) = f ++ " " ++ l
  show (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient
  { name :: Name
  , sex :: Sex
  , age :: Int
  , height :: Int
  , weight :: Int
  , bloodType :: BloodType
  }

instance Show Patient where
  show (Patient n a s h w b) =
    "**************" ++
    "\n" ++
    "Patient name: " ++
    show n ++
    "\n" ++
    "Sex: " ++
    show s ++
    "\n" ++
    "Age: " ++
    show a ++
    "\n" ++
    "Height: " ++
    show h ++
    "\n" ++
    "Weight: " ++
    show w ++ "\n" ++ "Blood type: " ++ show b ++ "\n" ++ "**************"

testPatient :: Patient
testPatient =
  Patient
  { name = Name "John" "Jack"
  , age = 43
  , sex = Male
  , height = 194
  , weight = 90
  , bloodType = BloodType O Neg
  }
