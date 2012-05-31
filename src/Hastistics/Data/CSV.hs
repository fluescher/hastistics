module Hastistics.Data.CSV where

import Hastistics
import Hastistics.Fields()
import Hastistics.Types
import Text.ParserCombinators.Parsec

{-| Represents file that consists of a list of rows -}
type CSV = [Record]

{-| Represents row that consists of a list of fields -}
type Record = [Field]

{-| Represents field that consists of a list of chars -}
type Field = String

{-| try reading records until eol appears -}
csv :: Parser CSV
csv = endBy record eol

{-| try reading fields separated by , -}
record :: Parser Record
record = sepBy field (char ',')

{-| try to either read a quoted field or characters until one of
 - the following characters appears: , or \n or \r -}
field :: Parser Field
field = quotedField <|> many (noneOf ",\n\r")

{-| read content of a quoted field
 - content consists of a various number of quotedChar
 - if no quote found at the end of the field, write error -}
quotedField :: Parser Field
quotedField = 
    do _ <- char '"'
       content <- many quotedChar
       _ <- char '"' <?> "quote at end of cell"
       return content

{-| read char as long as it's not \" and 
 - if it's \"\" return " -}
quotedChar :: Parser Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

{-| define end of line type, to match all possible types of line
 - feed characters. If none is found, writer error. Will result
 - in a parser error saying couldn't find "end of line"-}
eol :: GenParser Char st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

{-| call csv parser with given string and either return 
 - type CSV or ParseError -}
parseCSV :: String -> Either ParseError CSV
parseCSV input = parse csv "(unknown)" input

{-| method used to get only one specific type out of the parsing. -}
extract :: (Either ParseError CSV) -> CSV
extract (Left _) = error "Error on parsing"
extract (Right r) = r

{-| create a csvTable which is an instance of HSTable out of the
 - parsed CSV from above types-}
csvTable :: [ValueParser] -> String -> CSVTable
csvTable ps input = CSVTable hs rs
                    where hs = head parsed
                          rs = [toRow hs (convertRow [] ps row) | row <- tail parsed ]
                          parsed = extract (parseCSV input)

{-| convert the records into Lists of HSValues -}
convertRow :: [HSValue] -> [ValueParser] -> Record -> [HSValue]
convertRow r []     _                   = r
convertRow r _      []                  = r
convertRow vs (t:ts) (f:fs)             = (t f):(convertRow vs ts fs)

data CSVTable = CSVTable [Key] [HSRow]

instance HSTable CSVTable where
    headersOf (CSVTable hs _) = hs
    dataOf (CSVTable _ rs) = rs

instance Show CSVTable where
    show = showTable
