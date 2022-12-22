{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
    ( Commit(Commit),
      Repo(Repo, repoSlug, repoType),
      RepoType(GitHub),
      RepoSlug(RepoSlug, repoName, owner),
      Owner(Owner),
      RepoName(RepoName),
      DetailsUrl(DetailsUrl),
      ToolNote(TN, tnDetailsUrl, tnType, tnDesc),
      GithubCommentExperiment(GithubCommentExperiment, surveyResponses,
                              surveyUrl, surveyQuestion),
      showSomething,
      functionMonad,
      showExamples,
      feedbackSurveyCommentSuffix,
      toolNotePrCommentBody,
      gitHubCommitAndLineUrl,
      getCommentStyle, functionMonadSequentially, functionMonadSequentiallyDo, functionMonadDo, functionMonadArgumentsInterchanged, functionMonadSequentiallyWith, fmapInfixTest, fmapTest, fmapLambdaTest, fmapMaybeTest, mapTest )

    

main :: IO ()
main = do 
    putStrLn "Hello, Haskell from main!!"
    Lib.showSomething

    let repo = Repo { repoType = GitHub, repoSlug = RepoSlug { owner = Owner "obarra", repoName = RepoName "pythonrepo"}}
    putStrLn $ show repo

    let maybeUrl = gitHubCommitAndLineUrl repo (Commit "454gggg") 4 "/directory/path.ext" 
    putStrLn $ show maybeUrl

    let toolNote = TN { tnType = "omar", tnDesc = "myname", tnDetailsUrl = Just (DetailsUrl "someurl")}

    let d = toolNotePrCommentBody toolNote
    putStrLn $ show d

    let gce = GithubCommentExperiment{ surveyUrl = "someulr", surveyQuestion = "question?", surveyResponses = ["one", "two"]}
    let e = feedbackSurveyCommentSuffix 12 gce
    putStrLn $ show e

    putStrLn $ show getCommentStyle
    
    putStrLn $ show functionMonad

    putStrLn $ show functionMonadDo

    putStrLn $ show functionMonadArgumentsInterchanged

    putStrLn $ show functionMonadSequentially
    
    putStrLn $ show functionMonadSequentiallyWith
    
    putStrLn $ show functionMonadSequentiallyDo

    putStrLn $ show fmapInfixTest

    putStrLn $ show fmapTest

    putStrLn $ show fmapLambdaTest

    putStrLn $ show fmapMaybeTest

    putStrLn $ show mapTest

    Lib.showExamples




