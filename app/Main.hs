{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens (preview)
import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (traceM)
import qualified GHC.Read as TF
import System.Environment (lookupEnv)
import qualified Text.Trifecta as TF

data EntryType
  = Added
  | Changed
  | Deprecated
  | Removed
  | Fixed
  | Security
  deriving (Show)

data ChangelogEntry = ChangelogEntry
  { cllType :: !EntryType,
    cllMessage :: !Text
  }
  deriving (Show)

newtype PRBody = PRBody
  { prbChangelog :: [ChangelogEntry]
  }
  deriving (Show)

stringToType :: String -> Maybe EntryType
stringToType "add" = Just Added
stringToType "added" = Just Added
stringToType "change" = Just Changed
stringToType "changed" = Just Changed
stringToType "deprecate" = Just Deprecated
stringToType "deprecated" = Just Deprecated
stringToType "remove" = Just Removed
stringToType "removed" = Just Removed
stringToType "fix" = Just Fixed
stringToType "fixed" = Just Fixed
stringToType "security" = Just Security
stringToType _ = Nothing

parseEntryType :: TF.Parser EntryType
parseEntryType = do
  str <- TF.many TF.letter
  case stringToType (toLower <$> str) of
    Just t -> return t
    Nothing -> TF.unexpected $ "Unknown entry type: " <> str

parseChangelogEntry :: TF.Parser ChangelogEntry
parseChangelogEntry = do
  TF.text " - "
  et <- parseEntryType
  TF.text ": "
  msg <- TF.many $ TF.notChar '\n'
  return $ ChangelogEntry et (T.pack msg)

emptyLine :: TF.Parser ()
emptyLine = TF.many (TF.oneOf [' ', '\t']) $> ()

parseChangelog :: TF.Parser [ChangelogEntry]
parseChangelog = catMaybes <$> TF.sepBy f TF.newline
  where
    f :: TF.Parser (Maybe ChangelogEntry)
    f =
      TF.choice
        [ TF.try $ Just <$> parseChangelogEntry,
          emptyLine $> Nothing
        ]

parseSectionTitle :: T.Text -> TF.Parser ()
parseSectionTitle title = do
  TF.char '#'
  TF.many $ TF.char ' '
  TF.text title
  return ()

parseBody :: TF.Parser PRBody
parseBody = do
  parseSectionTitle "Description"
  TF.manyTill TF.anyChar . TF.try $ do
    TF.newline
    parseSectionTitle "Changelog"
    TF.newline
  cl <- parseChangelog
  TF.eof
  return $ PRBody cl

prBody :: Value -> Maybe Text
prBody = preview $ key "pull_request" . key "body" . _String

main :: IO ()
main = do
  eventPath_ <- lookupEnv "GITHUB_EVENT_PATH"

  let eventPath = case eventPath_ of
        Just x -> x
        Nothing -> error "Environment variable GITHUB_EVENT_PATH is not set"
  json <- readFile eventPath
  let ast = case eitherDecode @Value $ BSL.pack json of
        Right x -> x
        Left err -> error "Failed to parse JSON: "
  let body = case prBody ast of
        Just x -> x
        Nothing -> error "Failed to get PR body from event"
  let prb = case TF.parseString parseBody mempty (T.unpack body) of
        TF.Success x -> x
        TF.Failure x -> error $ "Failed to parse PR body:\n" <> show (TF._errDoc x)

  changelog <- readFile "CHANGELOG.md"
  undefined
