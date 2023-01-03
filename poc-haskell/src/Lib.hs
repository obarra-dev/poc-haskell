{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


 --  cabal update
 --  cabal run haskell-poc
 --  cabal list Relude
 --  cabal build haskell-poc
 --  cabal init -p haskell-poc

module Lib 
where

import Data.Data
import Network.URI (URI, parseURI)
import Relude
    ( isPrefixOf,
      fromMaybe,
      intercalate,
      putStrLn,
      show,
      ToString(toString),
      ToText(toText),
      Text )

showSomething :: IO ()
showSomething =  Relude.putStrLn "Hello, Haskell from lib!!"

showExamples :: IO ()
showExamples = do 
    Relude.putStrLn "Hello, Haskell examples!!"
    let s = intercalate
         "/" 
         ["ddd", "dxxxx"]

    Relude.putStrLn s


feedbackSurveyCommentSuffix :: Int -> GithubCommentExperiment -> Text
feedbackSurveyCommentSuffix toolNoteId GithubCommentExperiment {..} =
  surveyQuestion <> "\n" <> toText (intercalate " - " (fmap toString options))
 where
  options :: [Text]
  options = mkOption mkUrl <$> zip [1 ..] surveyResponses

  mkOption :: (Int -> Text) -> (Int, Text) -> Text
  mkOption urlFor (optionValue, optionText) =
    "[ [" <> optionText <> "](" <> urlFor optionValue <> ") ]"

  mkUrl :: Int -> Text
  mkUrl rating =
    surveyUrl
      <> "?"
      <> "comment="
      <> Relude.show toolNoteId
      <> "&"
      <> "lift_comment_rating="
      <> Relude.show rating



maxLogSize :: Int
maxLogSize = 4096


toolNotePrCommentBody :: ToolNote -> Text
toolNotePrCommentBody TN{..} =
    "*" <> typeString <> ":*" <> spacing <> tnDesc
    where
        typeString
            | Just details <- tnDetailsUrl = "[" <> tnType <> "]" <> "(" <> getDetailsUrl details <> ")"
            | otherwise = tnType
        spacing
            | "\n" `isPrefixOf` toString tnDesc = ""
            | otherwise = "  "


-- | URL for GitHub urls for a specific line in a specific commit.
gitHubCommitAndLineUrl :: Repo -> Commit -> Int -> FilePath -> Maybe URI
gitHubCommitAndLineUrl repo (Commit commitHash) lineNumber filePath =
    parseURI $ intercalate
         "/"
         (fmap toString [
           "https://github.com"
         , owner
         , repoName
         , "blob"
         , commitHash
         , toText filePath
         ])
         <> "#L"
         <> Relude.show lineNumber
      where
        Repo _ (RepoSlug (Owner owner) (RepoName repoName)) = repo

getCommentStyle ::  CommentStyle
getCommentStyle  =
    fromMaybe Normal $ projectSettings >>= commentStyleXXX
    where 
        projectSettings = Just ProjSet{ setupScript = Just "gia", commentStyleXXX  = Just Minimal}

dayOfTheWeek :: Int -> String
dayOfTheWeek 0 = "Sunday"
dayOfTheWeek 1 = "Monday"
dayOfTheWeek 2 = "Tuesday"
dayOfTheWeek 3 = "Wednesday"
dayOfTheWeek 4 = "Thursday"
dayOfTheWeek 5 = "Friday"
dayOfTheWeek 6 = "Saturday"

dayOfTheWeekWithCase :: Int -> String
dayOfTheWeekWithCase i = case i of
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"

dayOfTheWeekWithLambdaCase :: Int -> String
dayOfTheWeekWithLambdaCase = \case
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"

eitherLeftTest :: Either String Int
eitherLeftTest = Left "Hello" 

eitherRightTest :: Either String Int
eitherRightTest = Right 4 


functionMonad :: [Int]
functionMonad = [1,2,3,4] >>= \ x -> [x - 1, x + 1] 

functionMonadDo :: [Int]
functionMonadDo = do
    x <- [1,2,3,4] 
    [x - 1, x + 1] 


--k >> f = k >>= \_ -> f
--Sequentially compose two actions, discarding any value produced by the first, like sequencing operators (such as the semicolon) in imperative languages.
functionMonadSequentially :: [Int]
functionMonadSequentially = [7,8]  >>  [1,2,3,4] 

functionMonadSequentiallyWith :: [Int]
functionMonadSequentiallyWith = [7,8]  >>=  \_ -> [1,2,3,4] 

functionMonadSequentiallyDo:: [Int]
functionMonadSequentiallyDo = do 
    [7,8] 
    [1,2,3,4] 

functionMonadArgumentsInterchanged :: [Int]
functionMonadArgumentsInterchanged =  (\ x -> [x - 1, x + 1]) =<< [4, 3, 2, 1]

-- map (+10) (Just 4) does not compile
mapTest :: [Int]
mapTest = map (+10) [4, 3, 2, 1]

fmapTest :: [Int]
fmapTest = fmap (+10) [4, 3, 2, 1]

fmapMaybeTest :: Maybe Int
fmapMaybeTest = fmap (+10) (Just 4)

fmapLambdaTest :: [Int]
fmapLambdaTest =  fmap (\ x -> x + 10) [4, 3, 2, 1]

fmapInfixTest:: [Int]
fmapInfixTest = (+10) <$> [4, 3, 2, 1]

aplicativeTest :: Maybe Int
aplicativeTest = Just (+10) <*> Just 4

functionComposition :: Int 
functionComposition =  a 5
    where 
        f = (+1)
        g = (+4)
        a = f . g



data GithubCommentExperiment = GithubCommentExperiment
    { surveyUrl :: Text
    , surveyQuestion :: Text
    , surveyResponses :: [Text]
    }
    deriving (Show)


data ToolNote = TN
    { tnType :: Text -- ^ Type as identified by analyzer
    , tnDesc :: Text -- ^ Human readable description, typically including line number and file but not type.
    , tnDetailsUrl :: Maybe DetailsUrl -- ^ A URL linking to any additional details
    } deriving (Eq, Ord, Show, Typeable, Data)
    
newtype DetailsUrl = DetailsUrl { getDetailsUrl :: Text }
    deriving (Eq,Ord,Show,Data)
    
-- This is unique only within a given 'RepoType' and 'Owner'
newtype RepoName = RepoName Text
    deriving (Eq, Ord, Show, Data)


-- | The owning entity (or namespace) under which a repository exists.
--
-- This is unique only within a given 'RepoType'.
newtype Owner = Owner Text
    deriving (Eq, Ord, Show)


data RepoSlug = RepoSlug
    { owner :: Owner
    , repoName :: RepoName
    }
    deriving (Show, Ord, Eq)

data RepoType = GitHub | GitLab | BitbucketCloud | UnknownRepoType
    deriving  (Eq, Ord, Show)

-- | World-unique repo identifier.
--
-- * Example 1: @Repo GitHub "tommd/libacvp"@
-- * Example 2: @Repo GitLab "tamba/tooling"@
data Repo = Repo
    { repoType :: RepoType
    , repoSlug :: RepoSlug
    }
    deriving (Show, Ord, Eq)

newtype Commit = Commit Text
    deriving (Eq, Ord, Show, Typeable)


data ProjectSettings = ProjSet
    { setupScript      :: Maybe Text
    , commentStyleXXX     :: Maybe CommentStyle
    } deriving (Eq)

data CommentStyle = Normal | Minimal | None
    deriving (Bounded, Enum, Eq, Show)