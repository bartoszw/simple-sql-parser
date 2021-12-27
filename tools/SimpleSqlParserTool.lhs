
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
> import qualified Data.ByteString.Internal as BL (w2c)


> import Language.SQL.SimpleSQL.Pretty ( prettyStatements )
> import Language.SQL.SimpleSQL.Parse   ( ParseError(peFormattedError), parseStatements )
> import Language.SQL.SimpleSQL.Lex ( lexSQL )
> import Language.SQL.SimpleSQL.Dialect ( sqlserver, relaxParsing )

> import Text.Parsec ( Parsec, parse, sepBy, manyTill, try, (<|>), eof, lookAhead )
> import Text.Parsec.Char ( anyChar, noneOf, space, string, char)
> --import Text.Parsec.ByteString.Lazy  (Parser)

> import System.FilePath (takeBaseName, takeExtension)
> import Data.Char (toUpper, toLower)
  
> type Parser = Parsec String () 
  
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
>     ,("parser", parseRelaxedCommand)
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
>       ("-":filename:_) -> (filename,) <$> getContents
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
>           $ parseStatements sqlserver f Nothing src
>   )


> parseRelaxedCommand :: (String,[String] -> IO ())
> parseRelaxedCommand =
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
>       case takeExtension f of
>           []        -> putStrLn $ viewHeader $ takeBaseName f
>           _ -> putStrLn $ viewHeader $ takeBaseName f ++ tail (takeExtension f)
>       either (error . peFormattedError)
>           (putStrLn . prettyStatements (relaxParsing sqlserver))
>           $ parseStatements (relaxParsing sqlserver) f Nothing src
>       putStrLn "\nGO"
>   )

> readCommand :: (String,[String] -> IO ())
> readCommand =
>   ("just read and strip non readible chars)"
>   ,\args -> case args of
>               (f:_) -> BL.readFile f >>= return . mapWin1250toUft8 . (CL.filter isChar) >>= BL.putStr       
>               _     -> showHelp (Just "arguments not recognised") >> error ""
>   )

> splitCommand :: (String,[String] -> IO ())
> splitCommand =
>   ("(split file by SELECTs)"
>   ,\args -> do
>       (f,src) <- getInput args
> --       case parse (splitLine <* eof) f (CL.pack src) of
>       case parse (splitLine <* eof) f src of
>            Left e -> print e
>            Right r -> mapM_ putStrLn r -- >> putStrLn "GO" 
>   )


> --   ,\args -> case args of
> --              (f:_) -> BL.readFile f >>= 
>  --                      \c -> case parse splitLine f c of
>  --                                 Left e -> print e
>  --                                 Right r -> (mapM_ CL.putStrLn) r
>  --             _     -> showHelp (Just "arguments not recognised") >> error ""

> splitLine :: Parser [String]
> splitLine = do _ <- manyTill anyChar (try (myString "SELECT")) 
> --               l <- manyTill fourCases eof
>                l <- sepBy fourCases space
>                return ("SELECT\n" : l)
>              

> fourCases :: Parser String
> fourCases = (try (myString "UNION" >> manyTill space (myString "SELECT")) >> return "\nUNION\nSELECT")
>           <|> (try (myString "IN" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "IN (\nSELECT")
>           <|> (try (myString "FROM" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "FROM (\nSELECT")
>           <|> (try (myString "EXISTS" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "EXISTS (\nSELECT")
>           <|> (try (myString "WHEN" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "WHEN (\nSELECT")
>           <|> (try (myString "THEN" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "THEN (\nSELECT")
>           <|> (try (myString "ELSE" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "ELSE (\nSELECT")
>           <|> (try (string "," >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return ", (\nSELECT")
>           <|> (try (string "=" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "= (\nSELECT")
>           <|> (try (string "<" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "< (\nSELECT")
>           <|> (try (string ">" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "> (\nSELECT")
>           <|> (try (string "/" >> manyTill space (char '(') >> manyTill space (myString "SELECT")) >> return "/ (\nSELECT")
>           <|> (try (myString "SELECT" >> lookAhead space) >> return "\n_coZaZbiegOkolicznosci_\nSELECT")
>           <|> (try (string "--") >>= \ds -> manyTill anyChar (lookAhead lineCommentEnd) >>= \s -> return (ds ++ s)) 
>           <|> do x <- noneOf " \n" 
>                  xs <- fourCases
>                  return (x:xs)
>           <|> return []  
>   where lineCommentEnd :: Parser Char
>         lineCommentEnd = char '\n' <|> (eof >> return  '\n')

> {- example3 :: Parser String
> example3 = (try (string "UNION" >> manyTill anyChar (string "SELECT")) >> return "\nUNION\nSELECT")
>           <|> (try (string "IN" >> manyTill anyChar (char '(')) >> manyTill anyChar (try (string "SELECT")) >> return "IN (\nSELECT")
>           <|> (try (string "SELECT") >> return "\n-||\nSELECT")
>              <|> do x <- noneOf " \n" 
>                     xs <- example3
>                     return (x:xs)
>              <|> return []            -}

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
> isChar c = c >= toEnum 32 && c < toEnum 127 
>            || fromEnum c `elem` [10,163,179,211,243,198,230,202,234,209,241,175,191,140,156,143,159,165,185]

> mapWin1250toUft8 :: BL.ByteString -> BL.ByteString
> mapWin1250toUft8 = BL.concatMap convWin2Utf
>   where convWin2Utf w | BL.w2c w == toEnum 165 = BL.pack $ toEnum 0xC4:[toEnum 0x84] -- Ą
>                       | BL.w2c w == toEnum 185 = BL.pack $ toEnum 0xC4:[toEnum 0x85] -- ą
>                       | BL.w2c w == toEnum 198 = BL.pack $ toEnum 0xC4:[toEnum 0x86] -- Ć
>                       | BL.w2c w == toEnum 230 = BL.pack $ toEnum 0xC4:[toEnum 0x87] -- ć
>                       | BL.w2c w == toEnum 202 = BL.pack $ toEnum 0xC4:[toEnum 0x98] -- Ę
>                       | BL.w2c w == toEnum 234 = BL.pack $ toEnum 0xC4:[toEnum 0x99] -- ę
>                       | BL.w2c w == toEnum 163 = BL.pack $ toEnum 0xC5:[toEnum 0x81] -- Ł
>                       | BL.w2c w == toEnum 179 = BL.pack $ toEnum 0xC5:[toEnum 0x82] -- ł
>                       | BL.w2c w == toEnum 209 = BL.pack $ toEnum 0xC5:[toEnum 0x83] -- Ń
>                       | BL.w2c w == toEnum 241 = BL.pack $ toEnum 0xC5:[toEnum 0x84] -- ń
>                       | BL.w2c w == toEnum 211 = BL.pack $ toEnum 0xC3:[toEnum 0x93] -- Ó
>                       | BL.w2c w == toEnum 243 = BL.pack $ toEnum 0xC3:[toEnum 0xB3] -- ó
>                       | BL.w2c w == toEnum 140 = BL.pack $ toEnum 0xC5:[toEnum 0x9A] -- Ś
>                       | BL.w2c w == toEnum 156 = BL.pack $ toEnum 0xC5:[toEnum 0x9B] -- ś
>                       | BL.w2c w == toEnum 143 = BL.pack $ toEnum 0xC5:[toEnum 0xB9] -- Ź
>                       | BL.w2c w == toEnum 159 = BL.pack $ toEnum 0xC5:[toEnum 0xBA] -- ź
>                       | BL.w2c w == toEnum 175 = BL.pack $ toEnum 0xC5:[toEnum 0xBB] -- Ż
>                       | BL.w2c w == toEnum 191 = BL.pack $ toEnum 0xC5:[toEnum 0xBC] -- ż
>                       | otherwise              = BL.singleton w


> viewHeader :: String -> String
> viewHeader f = "IF OBJECT_ID('dbo.MIS_SELECT_" ++ f ++ "', 'V') IS NOT NULL\n\
>               \   DROP VIEW dbo.MIS_SELECT_" ++ f ++ ";\n\
>               \GO\n\n\
>               \SET ANSI_NULLS ON\n\
>               \GO\n\n\
>               \SET QUOTED_IDENTIFIER ON\n\
>               \GO\n\n\
>               \CREATE VIEW dbo.MIS_SELECT_" ++ f ++ " AS\n"

> -- Match the lowercase or uppercase form of 'c'
> caseInsensitiveChar :: Char -> Parser Char
> caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

> -- Match the string 's', accepting either lowercase or uppercase form of each character 
> myString :: String -> Parser String
> myString s = try (mapM caseInsensitiveChar s)
