module Main where

import Lexer
import Parser
import AST
import Data.Map qualified as M
import System.Environment (getArgs)

page :: HtmlNode
page =
    Element "div" ["container", "card", "bg-red-300"] (Just "app") (M.fromList [("src", "static/img.png"), ("href", "/id/1002/")])
        [ TextNode "Hello From Riml!"
        , Element "p" ["main"] Nothing (M.singleton "data-day" "sunday") [TextNode "Sunday"]
        ]

usage :: String
usage = "Usage: impera file\n\nCommands:\n\tast file\tprint AST\n\trender file\tprint the rendered HTML\n\thelp\t\tprint this help"

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
    writeFile (fname ++ ".html") rendered

main :: IO ()
main = getArgs >>= handleArgs
