import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.ByteString.Char8
import Data.Attoparsec.ByteString as P

import Bittenfeld

-- parseFullString
parseFS :: Parser a -> ByteString -> Either String a
parseFS p = parseOnly (p <* endOfInput)

shouldParse p s r = do (parseFS p $ pack s) `shouldBe` (Right r)

main :: IO ()
main = hspec $ do
  describe "Bittenfeld" $ do
    it "Parses a simple command 1" $ do
      shouldParse
        parse_Command "PING 123"
        (IRC_Command "PING" ["123"])

    it "Parses a simple command 2" $ do
      shouldParse
        parse_Command "PRIVMSG #test :Hallo Welt"
        (IRC_Command "PRIVMSG" ["#test", "Hallo Welt"])

    it "Parse a Server" $ do
      shouldParse
        parse_Sender ":irc.bittenfeld.test"
        (IRC_Server "irc.bittenfeld.test")

    it "Parse a Server 2" $ do
      shouldParse
        parse_Sender ":irc0.zrh.ch.swissirc.test"
        (IRC_Server "irc0.zrh.ch.swissirc.test")

    it "Parse a User" $ do
      shouldParse
        parse_Sender ":bittenfeld!bittenfeld@bittenfeld.test"
        (IRC_User "bittenfeld" "bittenfeld" "bittenfeld.test")

    it "Parse a full line" $ do
      shouldParse
        parse_InMessage ":bittenfeld!bittenfeld@bittenfeld.test PRIVMSG #test :Hallo Welt!"
        (IRC_InMessage (IRC_User "bittenfeld" "bittenfeld" "bittenfeld.test")
                       (IRC_Command "PRIVMSG" ["#test", "Hallo Welt!"]))

    it "Parse a full line 2" $ do
      shouldParse
        parse_InMessage ":irc.asdf.test 001 bittenfeld :Welcome to the derpy.test IRC Network bittenfeld!bittenfeld@derpytest.local.test"
        (IRC_InMessage (IRC_Server "irc.asdf.test")
                       (IRC_Command "001" ["bittenfeld", "Welcome to the derpy.test IRC Network bittenfeld!bittenfeld@derpytest.local.test"]))

