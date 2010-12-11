module GolfScript where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Applicative hiding ((<|>), many)

-- TODO: order
data GolfValue = GolfToken String
               | GolfString String
               | GolfNumber Integer
               | GolfBlock [GolfValue]
               | GolfComment String
               deriving (Eq, Ord, Show)

parseFile :: FilePath -> IO [GolfValue]
parseFile f = right <$> (parseFromFile $ many golfCode) f
  where right (Left e) = error $ show e
        right (Right a) = a

        golfCode = golfWord <|>
                   golfNumber <|>
                   golfBlock <|>
                   golfString <|>
                   golfRawString <|>
                   golfComment <|>
                   golfToken
        golfWord = do h <- letter <|> char '_'
                      t <- many $ alphaNum <|> char '_'
                      return $ GolfToken $ h : t
        golfNumber = GolfNumber <$> read <$> many1 digit
        golfBlock = char '{' >>
                    (GolfBlock <$> manyTill golfCode (char '}'))
        -- TODO: escapes
        golfString = char '\'' >>
                     (GolfString <$> manyTill anyChar (char '\''))
        -- TODO: escapes
        golfRawString = char '"' >>
                        (GolfString <$> manyTill anyChar (char '"'))
        golfComment = char '#' >>
                      (GolfComment <$> manyTill anyChar (char '\n'))
        golfToken = GolfToken <$> (:"") <$> noneOf "}"
        