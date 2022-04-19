module Main where

import Lib
import Options.Applicative

data Args = Args Input OutputFormat
  deriving (Show)

args = liftA2 Args input outputFormat

data Input
  = FileInput FilePath
  | Stdin
  deriving (Show)

input = fileInput <|> stdInput

fileInput :: Parser Input
fileInput = FileInput <$> argument str (metavar "FILE")

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

run (Args i o) = do
  s <- r i
  putStr $ display $ map (formatTable o . toTable) <$> parseVerilog s
  where
    r Stdin = getContents
    r (FileInput f) = readFile f

display (Left e) = show e
display (Right xs) = concat xs
