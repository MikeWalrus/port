module Lib
  ( parseVerilog,
    toTable,
    formatTable,
    OutputFormat (Latex, Md, Csv, Org),
  )
where

import Control.Applicative (liftA3)
import Control.Monad (liftM3)
import Data.Char
import Data.Functor
import Data.List (intercalate, intersperse)
import Text.Parsec.Token (GenTokenParser)
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language

parseFile p = parseVerilog <$> readFile p

parseVerilog = parse verilog "(unknown)"

verilogDef =
  LanguageDef
    { commentStart = "/*",
      commentEnd = "*/",
      commentLine = "//",
      nestedComments = False,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      opStart = oneOf "+-*/><[:]#",
      opLetter = oneOf "=+-",
      reservedNames =
        [ "module",
          "endmodule",
          "input",
          "output"
        ],
      reservedOpNames = ["[", ":", "#"],
      caseSensitive = False
    }

lexer = P.makeTokenParser verilogDef

parens = P.parens lexer

braces = P.braces lexer

brackets = P.brackets lexer

op = P.reservedOp lexer

identifier = P.identifier lexer

integer = P.integer lexer

reserved = P.reserved lexer

lexeme = P.lexeme lexer

semi = P.semi lexer

commaSep = P.commaSep lexer

comma = P.comma lexer

data Direction = In | Out
  deriving (Show)

newtype Width = Width (Maybe String)

instance Show Width where
  showsPrec d (Width (Just width)) = showString width
  showsPrec d (Width Nothing) = showString "1"

data Port = Port
  { name :: String,
    direction :: Direction,
    width :: Width
  }
  deriving (Show)

data Parameter = Parameter
  { parameterName :: String,
    value :: String
  }
  deriving (Show)

data VModule = VModule String [Parameter] [Port]
  deriving (Show)

verilog = many vModule

vModule =
  vModuleDecl <* manyTill anyToken (try $ reserved "endmodule")

vModuleDecl :: GenParser Char st VModule
vModuleDecl =
  liftM3
    VModule
    ( manyTill anyToken (try $ reserved "module")
        *> identifier
    )
    (parameters <|> return [])
    (parens (many portDecl) <* semi)

parameters = op "#" *> parens (commaSep parameter)

parameter = do
  reserved "parameter"
  name <- identifier
  op "="
  value <- lexeme $ concat <$> many (show <$> tree)
  return Parameter {parameterName = name, value = value}

data Tree = Leaf String | Node [Tree]

instance Show Tree where
  showsPrec d (Leaf x) = showString x
  showsPrec d (Node xs) = showString $ "(" ++ concatMap show xs ++ ")"

tree = leaf <|> node
  where
    leaf = Leaf <$> many1 (noneOf "(),\n")
    node = Node <$> parens (many tree)

portDirection = reserved "input" $> In <|> reserved "output" $> Out

portWidthNotOne = Just . surroundWithBrackets <$> brackets (lexeme $ many $ noneOf "]")
  where
    surroundWithBrackets xs = "[" ++ xs ++ "]"

portWidth = portWidthNotOne <|> return Nothing

portDecl = do
  dir <- portDirection
  try $ reserved "reg" <|> return ()
  width <- portWidth
  name <- sepEndBy identifier comma
  return Port {name = intercalate ", " name, direction = dir, width = Width width}

data Table = Table String [String] [[String]]
  deriving (Show)

toTable (VModule name parameters ports) =
  Table name ["Name", "Direction", "Width/Default Value"] $ map parameterToRow parameters ++ map portToRow ports

parameterToRow :: Parameter -> [String]
parameterToRow p = [parameterName p, "parameter", value p]

portToRow :: Port -> [String]
portToRow p = [name p, show $ direction p, show $ width p]

data OutputFormat = Org | Csv | Latex | Md
  deriving (Show)

tableFormat :: OutputFormat -> TableFormat
tableFormat Org =
  TableFormat
    { formatCaption = \s -> "#+CAPTION: " ++ s ++ "\n",
      lineBegin = "|",
      lineEnd = "|\n",
      headerSep = "|---|---|---|\n",
      fieldSep = " | ",
      footer = "\n"
    }
tableFormat Csv =
  TableFormat
    { formatCaption = const "",
      lineBegin = "",
      lineEnd = "\n",
      headerSep = "",
      fieldSep = ",",
      footer = "\n"
    }
tableFormat Latex =
  TableFormat
    { formatCaption = \s ->
        "\\begin{table}[H]\n"
          ++ "\\caption{"
          ++ s
          ++ "}\n"
          ++ "\\centering\n"
          ++ "\\begin{tabular}{lll}\n",
      lineBegin = "",
      lineEnd = "\\\\\n",
      headerSep = "\\hline\n",
      fieldSep = " & ",
      footer =
        "\\end{tablular}\n"
          ++ "\\end{table}\n"
    }
tableFormat Md = (tableFormat Org) {formatCaption = (++ "\n")}

data TableFormat = TableFormat
  { formatCaption :: String -> String,
    lineBegin :: String,
    lineEnd :: String,
    headerSep :: String,
    fieldSep :: String,
    footer :: String
  }

formatTable' :: TableFormat -> Table -> String
formatTable' f (Table caption header rows) =
  formatCaption f caption
    ++ concat (addSep (map (surround . intercalate (fieldSep f)) (header : rows)))
    ++ footer f
  where
    surround s = lineBegin f ++ s ++ lineEnd f
    addSep :: [String] -> [String]
    addSep (x : xs) = [x, headerSep f] ++ xs
    addSep _ = error "No header"

formatTable :: OutputFormat -> Table -> String
formatTable f = formatTable' $ tableFormat f