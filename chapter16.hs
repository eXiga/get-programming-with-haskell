module Lib where

newtype FirstName = FirstName
  { firstName :: String
  } deriving (Show)

newtype LastName = LastName
  { lastName :: String
  } deriving (Show)

newtype MiddleName = MiddleName
  { middleName :: String
  } deriving (Show)

newtype Initial = Initial
  { initial :: Char
  } deriving (Show)

newtype Phone = Phone
  { phoneNumber :: String
  } deriving (Show)

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName
  | TwoInitialsWithLastName Initial
                            Initial
                            LastName
  | FirstNameWithTwoInitials FirstName
                             Initial
                             Initial
  deriving (Show)

data Price
  = Price Double
  | Free

instance Show Price where
  show (Price p) = show p
  show Free = "Free product!"

newtype Author =
  Author Name
  deriving (Show)

data Artist
  = Person Name
  | Band String
  deriving (Show)

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  deriving (Show)

data Book = Book
  { author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: Int
  , bookPrice :: Price
  } deriving (Show)

data VinylRecord = VinylRecord
  { artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Price
  } deriving (Show)

data CollectibleToy = CollectibleToy
  { name :: String
  , toyDescription :: String
  , toyPrice :: Price
  } deriving (Show)

data Pamphlet = Pamphlet
  { title :: String
  , pamphletDescription :: String
  , phone :: Phone
  , pamphletPrice :: Price
  } deriving (Show)

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet
  deriving (Show)

price :: StoreItem -> Price
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = pamphletPrice pamphlet

-- test functions
createName :: Name
createName = Name FirstName {firstName = "John"} LastName {lastName = "Snow"}

createAuthor :: Creator
createAuthor = AuthorCreator $ Author createName

createBook :: StoreItem
createBook =
  BookItem
    Book
    { author = createAuthor
    , isbn = "ISBN"
    , bookTitle = "Title"
    , bookYear = 2019
    , bookPrice = Price 12
    }

createPamphlet :: StoreItem
createPamphlet =
  PamphletItem
    Pamphlet
    { title = "Pamphlet"
    , pamphletDescription = "L O L"
    , phone = Phone {phoneNumber = "+123"}
    , pamphletPrice = Free
    }

-- exercise 16.2
data RectangleDescriptor = RectangleDescriptor
  { rLength :: Double
  , rWidth :: Double
  }

newtype SquareDescriptor = SquareDescriptor
  { sSide :: Double
  }

newtype CircleDescriptor = CircleDescriptor
  { cRadius :: Double
  }

data Shape
  = Circle CircleDescriptor
  | Rectangle RectangleDescriptor
  | Square SquareDescriptor

perimeter :: Shape -> Double
perimeter (Circle c) = 2 * pi * cRadius c
perimeter (Rectangle r) = 2 * rLength r + 2 * rWidth r
perimeter (Square s) = 4 * sSide s

area :: Shape -> Double
area (Circle c) = pi * (cRadius c) ^ 2
area (Rectangle r) = rLength r * rWidth r
area (Square s) = sSide s ^ 2
