module Main where

import AST
import Lexer
import Parser
import Options.Applicative qualified as O

data ProgOpt
    = CmdAst FilePath
    | CmdRender FilePath
    | CmdTranspile { outputfile :: FilePath, inputfile :: FilePath }
    deriving (Show)

filepathArg :: String -> O.Parser FilePath
filepathArg help = O.argument O.str
    (  O.metavar "FilePath"
    <> O.help help)

astcmdP :: O.Parser ProgOpt
astcmdP = CmdAst <$> filepathArg "input file to generate the AST from"

rendercmdP :: O.Parser ProgOpt
rendercmdP = CmdRender <$> filepathArg "input file to render the html"

transpilecmdP :: O.Parser ProgOpt
transpilecmdP = CmdTranspile
    <$> O.option O.str
        (  O.long "output"
        <> O.short 'o'
        <> O.metavar "FilePath"
        <> O.help "output file path (optional)"
        <> O.showDefault
        <> O.value "index.html")
    <*> filepathArg "input file path (required)"

subcmdP :: O.Parser ProgOpt
subcmdP = O.subparser (astcmd <> rendercmd <> transpilecmd)
  where
    mkcmd name cmd desc =
        O.command name (O.info cmd (O.progDesc desc))

    astcmd       = mkcmd "ast"       astcmdP       "Print the AST generated from input file."
    rendercmd    = mkcmd "render"    rendercmdP    "Render the html generated from input file."
    transpilecmd = mkcmd "transpile" transpilecmdP "Transpile input file to html."

version :: String
version = "riml v1.0.0.4"

versionflagP :: O.Parser (a -> a)
versionflagP = O.infoOption version
    (  O.long "version"
    <> O.short 'v'
    <> O.help "Show the version of this compiler."
    )

progoptsP :: O.Parser ProgOpt
progoptsP = subcmdP O.<**> (versionflagP <*> O.helper)

progInfo :: O.ParserInfo ProgOpt
progInfo = O.info progoptsP
    (  O.fullDesc
    <> O.progDesc "This is the compiler for riml. Implemented in Haskell."
    <> O.header   "the riml compiler"
    )

toAst :: String -> HtmlNode
toAst = parse . alexScanTokens

printAst :: FilePath -> IO ()
printAst fname =
    print =<< toAst <$> readFile fname

renderFile :: FilePath -> IO ()
renderFile fname =
    putStrLn =<< prettyRender . toAst <$> readFile fname

translateFile :: FilePath -> FilePath -> IO ()
translateFile fin fout =
    writeFile fout =<<
        ("<!DOCTYPE html>\n" ++) . prettyRender . toAst <$> readFile fin

main :: IO ()
main = O.execParser progInfo >>= \case
    CmdAst       fname    -> printAst fname
    CmdRender    fname    -> renderFile fname
    CmdTranspile fout fin -> translateFile fin fout

