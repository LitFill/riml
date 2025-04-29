module Main where

import AST
import Lexer
import Parser
import System.Environment (getArgs)

usage :: String
usage = unlines
    [ "Usage: riml [file|cmds]"
    , ""
    , "Commands:"
    , "    <file>         transpile the .riml file into .html file"
    , "    ast <file>     print AST"
    , "    render <file>  print the rendered HTML"
    , "    help           print this help"
    , "    version        print the version"
    ]

version :: String
version = "riml v1.0.0.3"

-- NOTE: Do I need a library for this, like optparse-applicative?
handleArgs :: [String] -> IO ()
handleArgs = \case
    ["version"]       -> putStrLn      version
    [fname]           -> translateFile fname
    ["ast",    fname] -> printAst      fname
    ["render", fname] -> renderFile    fname
    _                 -> putStrLn      usage

printAst :: FilePath -> IO ()
printAst fname = do
    input <- readFile fname
    let tokens = alexScanTokens input
    let ast = parse tokens
    print ast

renderFile :: FilePath -> IO ()
renderFile fname = do
    input <- readFile fname
    let tokens = alexScanTokens input
    let ast = parse tokens
    putStrLn $ prettyRender ast

translateFile :: FilePath -> IO ()
translateFile fname = do
    input <- readFile fname
    let tokens   = alexScanTokens input
    let ast      = parse tokens
    let rendered = prettyRender ast
    let outname  = renameFile fname
    writeFile outname "<!DOCTYPE html>\n"
    appendFile outname rendered

renameFile :: String -> String
renameFile =
    (++ "html")
    . reverse
    . dropWhile (/= '.')
    . reverse

main :: IO ()
main = getArgs >>= handleArgs
