module Main where

import Lexer
import Parser
import AST
import System.Environment (getArgs)

usage :: String
usage = unlines
    [ "Usage: riml [file|cmds]"
    , ""
    , "Commands:"
    , "    <no-command>  transpile the .riml file into .html file"
    , "    ast file      print AST"
    , "    render file   print the rendered HTML"
    , "    help          print this help"
    ]

-- NOTE: Do I need a library for this, like optparse-applicative?
handleArgs :: [String] -> IO ()
handleArgs = \case
    [fname] -> translateFile fname
    ["ast", fname] -> printAst fname
    ["render", fname] -> renderFile fname
    _ -> putStrLn usage

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
    putStrLn $ render ast

translateFile :: FilePath -> IO ()
translateFile fname = do
    input <- readFile fname
    let tokens = alexScanTokens input
    let ast = parse tokens
    let rendered = render ast
    -- TODO: output file name
    let outname = renameFile fname
    writeFile outname "<!DOCTYPE html>\n"
    appendFile outname rendered

renameFile :: String -> String
renameFile = (++ "html") . reverse . dropWhile (/= '.') . reverse

main :: IO ()
main = getArgs >>= handleArgs
