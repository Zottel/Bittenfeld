import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.ByteString.Char8
import Data.Attoparsec.ByteString as P

import Bittenfeld

-- parseFullString
parseFS :: Parser a -> ByteString -> Either String a
parseFS p = parseOnly (p <* endOfInput)

parseSpec p s r = do (parseFS p $ pack s) `shouldBe` (Right r)

main :: IO ()
main = hspec $ do
  describe "Bittenfeld" $ do
    it "Parses a simple command 1" $ do
      parseSpec
        parse_Command "PING 123"
        (IRC_Command "PING" ["123"])
    it "Parses a simple command 2" $ do
      parseSpec
        parse_Command "PRIVMSG #test :Hallo Welt"
        (IRC_Command "PRIVMSG" ["#test", "Hallo Welt"])

    it "Parse a Server" $ do
      parseSpec
        parse_Sender ":irc.z0ttel.net"
        (IRC_Server "irc.z0ttel.net")

    it "Parse a User" $ do
      parseSpec
        parse_Sender ":z0ttel!z0ttel@z0ttel.net"
        (IRC_User "z0ttel" "z0ttel" "z0ttel.net")

    it "Parse a full line." $ do
      parseSpec
        parse_InMessage ":z0ttel!z0ttel@z0ttel.net PRIVMSG #test :Hallo Welt!"
        (IRC_InMessage (IRC_User "z0ttel" "z0ttel" "z0ttel.net")
                       (IRC_Command "PRIVMSG" ["#test", "Hallo Welt!"]))

