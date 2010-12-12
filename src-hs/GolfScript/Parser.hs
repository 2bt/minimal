module GolfScript.Parser (parseFile, parseString) where

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Char
import Control.Applicative hiding ((<|>), many)
import GolfScript.Value

parseFile :: FilePath -> IO [GolfValue]
parseFile f = right <$> (parseFromFile $ many golfCode) f

parseString :: String -> [GolfValue]
parseString = right . parse (many golfCode) "-"

right (Left e) = error $ show e
right (Right a) = a
        
golfCode = golfAssign <|>
           golfNumber <|>
           golfArray <|>
           golfBlock <|>
           golfString <|>
           golfRawString <|>
           golfComment <|>
           golfToken
golfAssign = do char ':'
                GolfToken token <- golfToken
                return $ GolfAssign token
golfNumber = GolfNumber <$> read <$> many1 digit
golfBlock = char '{' >>
            (GolfBlock <$> manyTill golfCode (char '}'))
golfArray = char '[' >>
            (GolfArray <$> manyTill golfCode (char ']'))
-- TODO: escapes
golfString = char '\'' >>
             (GolfString <$> manyTill anyChar (char '\''))
-- TODO: escapes
golfRawString = char '"' >>
                (GolfString <$> manyTill anyChar (char '"'))
golfComment = char '#' >>
              (GolfComment <$> manyTill anyChar (char '\n'))
golfToken = GolfToken <$> (word <|> ((:"") <$> noneOf "}]"))
  where word = do h <- letter <|> char '_'
                  t <- many $ alphaNum <|> char '_'
                  return $ h : t
