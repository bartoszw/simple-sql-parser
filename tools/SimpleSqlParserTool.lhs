
Simple command line tool to experiment with simple-sql-parser

Commands:

parse: parse sql from file, stdin or from command line
lex: lex sql same
indent: parse then pretty print sql

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE OverloadedStrings #-}
> import System.Environment ( getArgs )
> import Control.Monad ( when, forM_ )
> import Data.Maybe ( isJust )
> import System.Exit ( exitFailure )
> import Data.List ( intercalate )
> import Text.Show.Pretty ( ppShow )
> --import Control.Applicative
> import qualified Data.ByteString.Lazy as BL
> import qualified Data.ByteString.Lazy.Char8 as CL


> import Language.SQL.SimpleSQL.Pretty ( prettyStatements )
> import Language.SQL.SimpleSQL.Parse   ( ParseError(peFormattedError), parseStatements )
> import Language.SQL.SimpleSQL.Lex ( ansi2011, lexSQL )
> import Language.SQL.SimpleSQL.Dialect ( sqlserver, relaxParsing )

> import Text.Parsec ( parse, sepBy, manyTill, try, (<|>), eof)
> import Text.Parsec.Char ( anyChar, noneOf, space, string, char)
> import Text.Parsec.ByteString.Lazy  (Parser)

  

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         [] -> do
>               showHelp $ Just "no command given"
>         (c:as) -> do
>              let cmd = lookup c commands
>              maybe (showHelp (Just "command not recognised"))
>                    (\(_,cmd') -> cmd' as)
>                    cmd

> commands :: [(String, (String,[String] -> IO ()))]
> commands =
>     [("help", helpCommand)
>     ,("parse", parseCommand)
>     ,("lex", lexCommand)
>     ,("indent", indentCommand)
>     ,("l", lexBOreport)
>     ,("read", readCommand)
>     ,("split", splitCommand)
>     ]

> showHelp :: Maybe String -> IO ()
> showHelp msg = do
>           maybe (return ()) (\e -> putStrLn $ "Error: " ++ e) msg
>           putStrLn "Usage:\n SimpleSqlParserTool command args"
>           forM_ commands $ \(c, (h,_)) -> do
>                putStrLn $ c ++ "\t" ++ h
>           when (isJust msg) $ exitFailure

> helpCommand :: (String,[String] -> IO ())
> helpCommand =
>     ("show help for this progam", \_ -> showHelp Nothing)

> getInput :: [String] -> IO (FilePath,String)
> getInput as =
>     case as of
>       ["-"] -> ("",) <$> getContents
>       ("-c":as') -> return ("", unwords as')
>       ("-x":filename:_) -> (filename,) . CL.unpack . CL.filter isChar <$> BL.readFile filename
>       [filename] -> (filename,) <$> readFile filename
>       _ -> showHelp (Just "arguments not recognised") >> error ""

> parseCommand :: (String,[String] -> IO ())
> parseCommand =
>   ("parse SQL from file/stdin/command line (use -c to parse from command line)"
>   ,\args -> do
>       (f,src) <- getInput args
>       either (error . peFormattedError)
>           (putStrLn . ppShow)
>           $ parseStatements (relaxParsing sqlserver) f Nothing src
>   )

> lexCommand :: (String,[String] -> IO ())
> lexCommand =
>   ("lex SQL from file/stdin/command line (use -c to parse from command line)"
>   ,\args -> do
>       (f,src) <- getInput args
>       either (error . peFormattedError)
>              (putStrLn . intercalate ",\n" . map show)
>              $ lexSQL (relaxParsing sqlserver) f Nothing src
>   )


> indentCommand :: (String,[String] -> IO ())
> indentCommand =
>   ("parse then pretty print SQL from file/stdin/command line (use -c to parse from command line)"
>   ,\args -> do
>       (f,src) <- getInput args
>       either (error . peFormattedError)
>           (putStrLn . prettyStatements sqlserver)
>           $ parseStatements (relaxParsing sqlserver) f Nothing src

>   )

> readCommand :: (String,[String] -> IO ())
> readCommand =
>   ("just read and strip non readible chars)"
>   ,\args -> case args of
>               (f:_) -> BL.readFile f >>= return . (CL.filter isChar) >>= BL.putStr       
>               _     -> showHelp (Just "arguments not recognised") >> error ""
>   )

> splitCommand :: (String,[String] -> IO ())
> splitCommand =
>   ("(split file by SELECTs)"
>   ,\args -> do
>       (f,src) <- getInput args
>       case parse (splitLine <* eof) f (CL.pack src) of
>            Left e -> print e
>            Right r -> mapM_ putStrLn r
>   )

> --   ,\args -> case args of
> --              (f:_) -> BL.readFile f >>= 
>  --                      \c -> case parse splitLine f c of
>  --                                 Left e -> print e
>  --                                 Right r -> (mapM_ CL.putStrLn) r
>  --             _     -> showHelp (Just "arguments not recognised") >> error ""

> splitLine :: Parser [String]
> splitLine = do _ <- manyTill anyChar (try (string "SELECT")) 
> --               l <- manyTill fourCases eof
>                l <- sepBy fourCases space
>                return ("SELECT\n" : l)
>              

> fourCases :: Parser String
> fourCases = (try (string "UNION" >> manyTill space (string "SELECT")) >> return "\nUNION\nSELECT")
>           <|> (try (string "IN" >> manyTill space (char '(') >> manyTill space (string "SELECT")) >> return "IN (\nSELECT")
>           <|> (try (string "SELECT") >> return "\n_coZaZbiegOkolicznosci_\nSELECT")
>      --     <|> (pure <$> anyChar)
>           <|> do x <- noneOf " \n" 
>                  xs <- fourCases
>                  return (x:xs)
>           <|> return []  

> example3 :: Parser String
> example3 = (try (string "UNION" >> manyTill anyChar (string "SELECT")) >> return "\nUNION\nSELECT")
>           <|> (try (string "IN" >> manyTill anyChar (char '(')) >> manyTill anyChar (try (string "SELECT")) >> return "IN (\nSELECT")
>           <|> (try (string "SELECT") >> return "\n-||\nSELECT")
>              <|> do x <- noneOf " \n" 
>                     xs <- example3
>                     return (x:xs)
>              <|> return []            

> lexBOreport :: (String,[String] -> IO ())
> lexBOreport =
>   ("lex BO report file/stdin/command line (use -x to strip non readible chars)"
>   ,\args -> do
>       (f,src) <- getInput args
>       either (error . peFormattedError)
>              (putStrLn . intercalate ",\n" . map show)
>              $ lexSQL (relaxParsing sqlserver) f Nothing src
>   )

> isChar :: Char -> Bool           
> isChar c = c >= toEnum 32 && c < toEnum 127 || c == toEnum 10