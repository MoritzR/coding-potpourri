import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP
  ( ReadP,
    choice,
    get,
    many1,
    optional,
    pfail,
    readP_to_S,
    string,
  )

data Command = Look | Go Direction
  deriving (Show)

data Direction = North | East | South | West
  deriving (Show)

runParser :: String -> Maybe Command
runParser = listToMaybe . map fst . readP_to_S parser

parser :: ReadP Command
parser = choice [look, go]

look :: ReadP Command
look = do
  string "look"
  return Look

go :: ReadP Command
go = do
  string "go "
  optional toThe
  directionToGo <- direction
  return (Go directionToGo)

toThe :: ReadP ()
toThe = do
  optional (string "to ")
  optional (string "the ")

direction :: ReadP Direction
direction = do
  directionString <- many1 get
  case directionString of
    "north" -> return North
    "east" -> return East
    "south" -> return South
    "west" -> return West
    _ -> pfail
