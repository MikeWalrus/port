module Main where

import Lib
import Options.Applicative

data Args = Args OutputFormat Input
  deriving (Show)

args = liftA2 Args outputFormat input

data Input
  = FileInput FilePath
  | Stdin
  deriving (Show)

input = fileInput <|> stdInput

fileInput :: Parser Input
fileInput = FileInput <$> (argument str (metavar "FILE" <> action "file"))

stdInput =
  flag'
    Stdin
    ( long "stdin"
        <> help "Read from stdin"
    )

outputFormat =
  flag' Org (long "org")
    <|> flag' Csv (long "csv")
    <|> flag' Latex (long "latex")
    <|> flag' Md (long "md")

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    ( fullDesc
        <> progDesc "Extract port definitions from Verilog."
        <> header "port"
    )

main :: IO ()
main = execParser opts >>= run

run (Args o i) = do
  s <- r i
  putStr $ display $ map (formatTable o . toTable) <$> parseVerilog s
  where
    r Stdin = getContents
    r (FileInput f) = readFile f

display (Left e) = show e
display (Right xs) = concat xs
