module Hastistics.Data.CSV where

import Hastistics
import Hastistics.Fields()
import Hastistics.Types
import Text.ParserCombinators.Parsec

type CSV = [Record]
type Record = [Field]
type Field = String

csv :: Parser CSV
csv = endBy record eol

record :: Parser Record
record = sepBy field (char ',')

field :: Parser Field
field = quotedField <|> many (noneOf ",\n\r")

quotedField :: Parser Field
quotedField = 
    do _ <- char '"'
       content <- many quotedChar
       _ <- char '"' <?> "quote at end of cell"
       return content

quotedChar :: Parser Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol :: GenParser Char st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError CSV
parseCSV input = parse csv "(unknown)" input

extract :: (Either ParseError CSV) -> CSV
extract (Left _) = error "Error on parsing"
extract (Right r) = r

csvTable :: [ValueParser] -> String -> CSVTable
csvTable ps input = CSVTable hs rs
                    where hs = head parsed
                          rs = [toRow hs (convertRow [] ps row) | row <- tail parsed ]
                          parsed = extract (parseCSV input)

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
