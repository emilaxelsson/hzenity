{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Simple where

import Control.Monad
import System.Process

import Zenity

config = def {debug = True}

-- Ask for a name and display a greeting
greeting = do
  Just name <-
    zenity config {title = Just "Name entry"} $
    Entry $ def {text = Just "What's your name?"}
  zenity config $ Info def {text = Just $ "Greetings, " <> name <> "!"}

data SystemCommand
  = Ls
  | Pwd
  | Uname
  deriving (Eq, Show, Read)

-- Ask for a system command to be executed
--
-- `keyedList` allows associating each option with a value of some type. We can
-- then scrutinize the result by matching on the associated values.
systemOperation = do
  op <- keyedList config radio def
    "Select a command"
    [ (Ls, "List files")
    , (Pwd, "Show current directory")
    , (Uname, "OS information")
    ]
  case op of
    Just Ls -> do
      putStrLn "These are the files in the current directory:"
      void $ system "ls"
    Just Pwd -> do
      putStrLn "This is the current directory:"
      void $ system "pwd"
    Just Uname -> do
      putStrLn "This is the OS that's running:"
      void $ system "uname"
    Nothing -> return ()
