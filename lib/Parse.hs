module Parse where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Numeric (readFloat, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = choice [parseString, parseNumber, parseChar, parseFloat, parseQuoted, parseListWrap, parseAtom]

readExpr :: String -> Either ParseError LispVal
readExpr = parse parseExpr "lisp"

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Float
  deriving Eq

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
  show (Atom s) = s
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Number s) = show s
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Char c) = "'" ++ [c] ++ "'"
  show (Float f) = show f

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (choice (try . parseEscape <$> "\"\\ntr") <|> noneOf "\"")
  char '"'
  return $ String x
  where
    escapeChars = [('n', '\n'), ('t', '\t'), ('r', '\r')]
    escape :: Char -> Char
    escape c = fromMaybe c (lookup c escapeChars)
    parseEscape :: Char -> Parser Char
    parseEscape c = escape <$> (char '\\' >> char c)

symbol :: Parser Char
symbol = oneOf "?!@$%^&#+-/*_~|:<>="

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let x = first : rest
  return $ case x of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom x

liftMaybe :: Maybe a -> Parser a
liftMaybe = maybe (unexpected "no parse") return

liftReadS :: ReadS a -> String -> Parser a
liftReadS reader =
  liftMaybe
    . (fst <$>) -- String -> Maybe a
    . find parsed
    . reader
  where
    parsed = null . snd

parseNumber :: Parser LispVal
parseNumber = choice (try <$> [parseDecimal, parseOct, parseHex])
  where
    parseDecimal :: Parser LispVal
    parseDecimal = Number . read <$> (optional (string "#d") >> many1 digit)
    parseOct :: Parser LispVal
    parseOct = Number <$> (string "#o" >> many1 octDigit >>= liftReadS readOct)
    parseHex :: Parser LispVal
    parseHex = Number <$> (string "#x" >> many1 hexDigit >>= liftReadS readHex)

parseChar :: Parser LispVal
parseChar = try (string "#\\" >> choice (try <$> [parseCharName, parseCharLit]))
  where
    endWithSep :: Parser ()
    endWithSep = lookAhead (choice [eof, char ' ' >> pure (), char '(' >> pure ()])
    parseCharLit :: Parser LispVal
    parseCharLit = Char <$> ((letter <|> symbol) <* lookAhead endWithSep)
    charNames = [("space", ' '), ("newline", '\n')]
    parseCharName :: Parser LispVal
    parseCharName =
      Char
        <$> ( choice (try . string . fst <$> charNames)
                >>= liftMaybe . flip lookup charNames
            )

parseFloat :: Parser LispVal -- partial implementation
parseFloat =
  Float
    <$> try
      ( string "#i" >> many1 (digit <|> char '.')
          >>= liftReadS readFloat
      )

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = List . (Atom "quote" :) . singleton <$> (char '\'' >> parseExpr)
  where
    singleton x = [x]

parseListWrap :: Parser LispVal
parseListWrap = char '(' >> (try parseList <|> parseDottedList) <* char ')'
