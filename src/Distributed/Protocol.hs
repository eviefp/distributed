module Distributed.Protocol
    ( Message (..)
    , NodeName (..)
    , encode
    , decode
    ) where

import Prelude

import           Control.Applicative
    (Alternative ((<|>)))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.String
    (IsString (fromString))
import           Data.Text
    (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E
import           Data.Void
    (Void)
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Byte as MB

newtype NodeName = NodeName String
   deriving newtype (Show)

data Message
    = Connect NodeName
    | Welcome NodeName
    | Message NodeName NodeName Text
    deriving stock (Show)

encode :: Message -> L.ByteString
encode = \case
    Connect (NodeName name) -> "Connect#" <> fromString name
    Welcome (NodeName name) -> "Welcome#" <> fromString name
    Message (NodeName from) (NodeName to) t ->
        "Message#"
            <> fromString from
            <> "#"
            <> fromString to
            <> "#"
            <> L.fromStrict (E.encodeUtf8 t)

decode :: B.ByteString -> Maybe Message
decode = M.parseMaybe decodeMessage

type Parser = M.Parsec Void B.ByteString

decodeMessage :: Parser Message
decodeMessage = parseConnect <|> parseWelcome <|> parseMessage

parseConnect :: Parser Message
parseConnect =
    Connect . NodeName
        <$> (MB.string "Connect#" *> parsePart)

parseWelcome :: Parser Message
parseWelcome =
    Connect . NodeName
        <$> (MB.string "Welcome#" *> parsePart)

parseMessage :: Parser Message
parseMessage =
    Message
        <$> fmap NodeName (MB.string "Message#" *> parsePart)
        <*> fmap NodeName (MB.string "#" *> parsePart)
        <*> fmap T.pack (MB.string "#" *> parsePart)

parsePart :: Parser String
parsePart = show . L.pack <$> M.many (M.noneOf [35])
