module Bittenfeld (
  IRC_Sender(IRC_Server, IRC_User, IRC_Sender_Empty),
  IRC_Command(IRC_Command),
  IRC_InMessage(IRC_InMessage),
  parse_Sender, parse_Command, parse_InMessage
  ) where

import Control.Applicative
import Data.ByteString.Char8 (unpack, pack)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 (char8, endOfLine)
import Data.Attoparsec.ByteString (notWord8)
import qualified Data.Attoparsec.ByteString.Char8 as P8

-- -------------------------------------------------------------------------- --
--  IRC MESSAGE TYPES DRAFT                                                   --
-- -------------------------------------------------------------------------- --

data IRC_Sender = IRC_Server String
                | IRC_User String String String
                | IRC_Sender_Empty
  deriving (Show, Eq)

data IRC_Command = IRC_Command String [String]
  deriving (Show, Eq)

data IRC_InMessage = IRC_InMessage IRC_Sender IRC_Command
  deriving (Show, Eq)
--data IRC_OutMessage = IRC_Command
--  deriving (Show)

-- -------------------------------------------------------------------------- --
--  IRC PARSER DRAFT                                                          --
-- -------------------------------------------------------------------------- --

parse_Sender :: Parser IRC_Sender
parse_Sender =
      do P8.char8 ':'
         nick <- P8.takeWhile (/='!')
         P8.char8 '!'
         user <- P8.takeWhile (/='@')
         P8.char8 '@'
         host <- P8.takeWhile (/=' ')
         return $ IRC_User (unpack nick) (unpack user) (unpack host)
  <|> do P8.char8 ':'
         server <- P8.takeWhile1 (/=' ')
         return $ IRC_Server (unpack server)

parse_Parameter :: Parser String
parse_Parameter =
      do P8.char8 '"'
         param <- P8.takeWhile (/='"')
         P8.char8 '"'
         return $ unpack param
  <|> do P8.char8 ':'
         param <- P8.takeByteString
         return $ unpack param
  <|> do param <- P8.takeWhile1 (/=' ')
         return $ unpack param

parse_Command :: Parser IRC_Command
parse_Command =
      do command <- P8.takeWhile1 (/=' ')
         P8.char8 ' '
         parameters <- parse_Parameter `sepBy` (P8.char8 ' ')
         --parameters <- many1 parse_Parameter
         return $ IRC_Command (unpack command) parameters
  <|> do command <- P8.takeWhile1 (/=' ')
         return $ IRC_Command (unpack command) []

parse_InMessage :: Parser IRC_InMessage
parse_InMessage =
      do sender <- parse_Sender
         P8.char8 ' '
         command <- parse_Command
         return $ IRC_InMessage sender command
  <|> do command <- parse_Command
         return $ IRC_InMessage IRC_Sender_Empty command

