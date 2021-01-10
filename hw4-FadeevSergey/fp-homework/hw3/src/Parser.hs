module Parser
        ( FSCommand (..),
          parseInputCommand
        ) where

import Options.Applicative
                ( Parser
                , ParserInfo
                , metavar
                , argument
                , progDesc
                , prefs
                , info
                , command
                , many
                , str
                , idm
                , helper
                , subparser
                , execParserPure
                , getParseResult
                , showHelpOnEmpty
                , (<**>) )

data FSCommand
  = CommandHelp
  | CommandPwd
  | CommandLs
  | CommandCd String
  | CommandLsAll String
  | CommandRemoveDir String
  | CommandRemoveFile String
  | CommandMkdir String
  | CommandTouch String
  | CommandCat String
  | CommandWrite String [String]
  | CommandDirInfo String
  | CommandFileInfo String
  | CommandFind String
  deriving (Show, Eq)

parseInputCommand :: String -> Maybe FSCommand
parseInputCommand newCommand = getParseResult $ execParserPure (prefs showHelpOnEmpty) options $ words newCommand

options :: ParserInfo FSCommand
options = info (commandParser <**> helper) idm

commandParser :: Parser FSCommand
commandParser =
  subparser
    ( command "help" (info (pure CommandHelp) (progDesc "Print help"))
        <> command "pwd" (info (pure CommandPwd) (progDesc "Print the current directory"))
        <> command "ls" (info (pure CommandLs)   (progDesc "Print files from directory"))
        <> command
          "cd"
          ( info (CommandCd <$> argument str (metavar "DIRECTORY"))
                 (progDesc "Print all files and directories in the file tree")
          )
        <> command
          "rmd"
          ( info (CommandRemoveDir <$> argument str (metavar "DIRECTORY"))
                 (progDesc "Remove directory")
          )
        <> command
          "ls-all"
          ( info (CommandLsAll <$> argument str (metavar "DIRECTORY"))
                 (progDesc "Go to directory")
          )
        <> command
          "rmf"
          ( info (CommandRemoveFile <$> argument str (metavar "FILENAME"))
                 (progDesc "Remove file")
          )
        <> command
          "mkdir"
          ( info (CommandMkdir <$> argument str (metavar "DIRECTORY"))
                 (progDesc "Create directory")
          )
        <> command
          "touch"
          ( info (CommandTouch <$> argument str (metavar "FILENAME"))
                 (progDesc "Create file")
          )
        <> command
          "cat"
          ( info (CommandCat <$> argument str (metavar "FILENAME"))
                 (progDesc "Print file")
          )
        <> command
          "write"
          ( info
              ( CommandWrite <$> argument str (metavar "FILENAME")
                  <*> (many (argument str (metavar "CONTENT")))
              )
              (progDesc "Write text to file")
          )
        <> command
          "dinfo"
          ( info (CommandDirInfo <$> argument str (metavar "DIRECTORY"))
                 (progDesc "Info about directory")
          )
        <> command
          "finfo"
          ( info (CommandFileInfo <$> argument str (metavar "FILENAME"))
                 (progDesc "Info about file")
          )
        <> command
          "find"
          ( info (CommandFind <$> argument str (metavar "FILENAME"))
                 (progDesc "Find file in directory")
          )
    )