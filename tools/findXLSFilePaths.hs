{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Text.Show.Pretty
import System.IO
import Text.Parsec 
import Text.Parsec.String

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> readFile f >>= \c -> 
               case parse (manyTill (try xlsPath <|> (anyChar >> return "?")) eof) f c of
                    Left e -> print e
                    Right r -> mapM_ putStrLn $ filter ("?" /=) r
        _ -> putStrLn "input error"
    
    

xlsPath :: Parser String
xlsPath = drive >>= \d ->
          many (try directory) >>= \dirs ->
          file >>= \f ->
          return (d ++ concat dirs ++ f)
          
drive :: Parser String
drive = letter >>= \l ->
        string ":\\" >>= 
        return . (l:)
        
directory :: Parser String
directory = many1 validChar >>= \s ->
            string "\\" >>= 
            return . (s ++)
    
    
validChar :: Parser Char
validChar = letter <|> digit <|> oneOf " -!+_." <|> (char '\n' >> return ' ')
{-
file :: Parser String
file = many1 validChar >>= \f ->
       string "." >>
       (string "XLS" <|> string "xls") >>
       return (f ++ ".XLS")
       -}        
file :: Parser String
file = try extension 
    <|> (validChar >>= \v -> 
         file >>= \vs ->
         return (v:vs))
         
extension :: Parser String
extension = string "." >>
            (string "XLS" <|> string "xls") >>
            return ".XLS"

