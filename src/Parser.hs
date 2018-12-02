module Parser
  ( parseDeBruijn
  , parseDeBruijnString
  ) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Foldable

import Lib


lambda, lparen, rparen, spaces :: Parser ()

-- '\' == lambda
lambda = char '\\' *> pure ()

lparen = char '(' *> pure ()

rparen = char ')' *> pure ()

spaces = skipMany space

word :: Parser String
word = many1 letter_ascii

free :: Parser Term
free = spaces *> (Free <$> word)

var :: Parser Term
var = spaces *> (mkVar . (\d -> d - 1) <$> decimal)

lam :: Parser Term
lam = spaces *> lambda *> (Lam <$> term)

app :: Parser Term
app = App <$> (spaces *> lparen *> spaces *> term <* spaces <* rparen) <*> (spaces *> term)

term :: Parser Term
term = choice [app, lam, var, free]

parseDeBruijn :: ByteString -> Either String Term
parseDeBruijn = parseOnly term

parseDeBruijnString :: String -> Either String Term
parseDeBruijnString = parseDeBruijn . pack
