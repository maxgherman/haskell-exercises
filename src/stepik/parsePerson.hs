import Data.List (find)
import Data.Text ( splitOn, pack, unpack )
import qualified Data.Text as TextNull (null)
import qualified Data.List as ListNull (null)
import Text.Read


data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson input = case hasEmptyLine (splitLines input) of
    Left e -> Left e
    Right lines -> case hasPairs (splitPairs lines) of
        Left e -> Left e
        Right pairs -> case hasFields pairs of
            Left e -> Left e
            Right pairs@[[_, fl], [_, ln], [_, a]] -> case hasAge a of
                Left e -> Left e
                Right age -> Right Person { firstName = fl, lastName  = ln, age = age }
        
splitLines input = splitOn (pack "\n") (pack input)

hasEmptyLine lines = if any TextNull.null lines
    then Left ParsingError
    else Right lines

hasPairs xs = if all checkPair xs then Right xs else Left ParsingError
    where
        checkPair [_, _] = True
        checkPair _ = False

splitPairs = map (\x -> map unpack $ splitOn (pack " = ") x)

hasFields xs = result [
        find (checkPair "firstName") xs,
        find (checkPair "lastName") xs,
        find (checkPair "age") xs
    ]
    where
        result p@[Just fPair, Just lPair, Just aPair ] = Right [fPair, lPair, aPair]
        result _ = Left IncompleteDataError 
        checkPair field [x, y] = x == field && not (ListNull.null y)

hasAge x = case (readMaybe x :: Maybe Int) of
    Nothing -> Left $ IncorrectDataError x
    Just age -> Right age


-- wrong Parse | empty string
t0 = parsePerson ""
-- correct
t1 = parsePerson "firstName = John\nlastName = Connor\nage = 30"
-- correct | shiffled fields
t18 = parsePerson "lastName = Connor\nfirstName = John\nage = 30"
-- wrong Parse | no spaces around = in minor fields
t2 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
 -- wrong Parse | no spaces around = on the left in minor fields
t5 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde= "
-- wrong Parse | no spaces around = in major fields
t3 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30"
-- wrong Incorrect | age is non-numeric
t4 = parsePerson "firstName = John\nlastName = Connor\nage = as30"
-- wrong Parse | no spaces around = in major fields, missing major field
t6 = parsePerson "firstName=Barbarian\nlastName=Conn Or"
-- wrong Parse | no spaces around = in major fields, typo in major field
t7 = parsePerson "firstNameee = John Smith\nlastName = Connor\nage = 30\nasde=as11"
-- correct | excessive fields
t8 = parsePerson "firstName = John\nlastName = Connor\nfoo = bar\nage = 30"
-- wrong Incomplete | missing major field
t9 = parsePerson "firstName = Barbarian\nlastName = Conn Or"
-- wrong Parse | empty major value
t10 = parsePerson "firstName = John\nlastName = Connor\nage = "
-- wrong Parse | no spaces around = on the right in major field
t11 = parsePerson "firstName = John\nlastName = Connor\nage ="
-- wrong Parse | empty key, missing major field
t12 = parsePerson "firstName = John\nlastName = Connor\n = 30"
-- correct | spaces in major field value
t13 = parsePerson "firstName = Barbarian\nlastName = Conn On\nage = 30"
-- correct | = in major field value
t14 = parsePerson "firstName = John\nlastName = Con=nor\nage = 30"
-- wrong Parse | no spaces around =, missing value in minor field
t15 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd"

-- wrong Incomplete | major field key with whitespace, age is non-numeric
t17 = parsePerson " firstName = John\nlastName = Connor\nage = 2f8 "
