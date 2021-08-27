{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | A wrapper for <https://en.wikipedia.org/wiki/Zenity Zenity> dialog boxes
--
-- Zenity is accessed through system calls, so it needs to be installed on the
-- computer in order for this wrapper to work.
--
-- It is advised to turn on the following extensions when using this module:
--
-- > DuplicateRecordFields
-- > OverloadedStrings
--
-- Here is a simple example for how to use the library. It asks the user for a
-- name and displays a greeting:
--
-- @
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE OverloadedStrings \#-}
--
-- import "Data.Monoid"
-- import "Zenity"
--
-- greeting = do
--   Just name <-
--     `zenity` `def` {title = Just "Name entry"} $
--       `Entry` $ `def` {text = Just "What's your name?"}
--   `zenity` `def` $ `Info` `def` {text = Just $ "Greetings, " <> name <> "!"}
-- @
--
-- More examples can be found in the
-- <https://github.com/emilaxelsson/hzenity/tree/master/examples examples/>
-- directory.
module Zenity
  ( Text
  , Day
  , Default (..)

    -- * Zenity dialogs
  , Config (..)
  , CalendarFlags (..)
  , EntryFlags (..)
  , FileSelectionFlags (..)
  , InfoFlags (..)
  , ReturnedColumn (..)
  , ListFlags (..)
  , SelectionHeader (..)
  , ListType (..)
  , radio
  , check
  , Matrix (..)
  , Dialog (..)
  , zenity

    -- ** Extra dialogs
  , keyedList
  ) where

import Control.Monad (void, when)
import Data.Bool (bool)
import Data.Default (Default (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Data.Text as Text
import qualified System.Process.Text as Text
import System.Process (showCommandForUser)

#if MIN_VERSION_base(4,13,0)
#else
import Control.Monad.Fail (MonadFail)
import Data.Word (Word)
#endif



-- | Chop a list into chunks of size @n@
chunk :: Int -> [a] -> [[a]]
chunk n as
  | n >= 1 = go (length as) as
  | otherwise = error "chunk: chunk size must be >= 1"
  where
    go l bs
      | l <= n = [bs]
      | otherwise = take n bs : go (l - n) (drop n bs)



--------------------------------------------------------------------------------
-- * Zenity
--------------------------------------------------------------------------------

-- | General Zenity configuration
--
-- Use 'def' for default configuration.
data Config = Config
  { title :: Maybe Text -- ^ Dialog title
  , windowIcon :: Maybe FilePath
      -- ^ Window icon with the path to an image. Alternatively, one of the four
      -- stock icons can be used: @error@, @info@, @question@ or @warning@.
  , width :: Maybe Int -- ^ Dialog width
  , height :: Maybe Int -- ^ Dialog height
  , timeout :: Maybe Int -- ^ Dialog timeout in seconds
  , debug :: Bool -- ^ Print the system call to Zenity with flags
  }

instance Default Config where
  def = Config
    { title = Nothing
    , windowIcon = Nothing
    , width = Nothing
    , height = Nothing
    , timeout = Nothing
    , debug = False
    }

data CmdFlag where
  CmdFlag
    :: { _flagName :: String
       , _flagField :: Maybe a
       , _flagParam :: a -> String}
    -> CmdFlag

cmdFlag :: CmdFlag -> [String]
cmdFlag (CmdFlag flag fld mkParam) = maybe [] (\f -> [flag, mkParam f]) fld

configFlags :: Config -> [String]
configFlags Config {..} =
  concatMap
    cmdFlag
    [ CmdFlag "--title" title Text.unpack
    , CmdFlag "--window-icon" windowIcon id
    , CmdFlag "--width" width show
    , CmdFlag "--height" height show
    , CmdFlag "--timeout" timeout show
    ]

class CmdParam p where
  serializeParam :: p -> [String]

boolParam :: Bool -> String -> [String]
boolParam param flag = bool [] [flag] param

maybeParam :: Maybe a -> (a -> [String]) -> [String]
maybeParam = flip $ maybe []

-- | Flags for the 'Calendar' dialog
--
-- Use 'def' for default flags.
data CalendarFlags = CalendarFlags
  { text :: Maybe Text -- ^ Dialog text
  , year :: Maybe Word -- ^ Calendar year
  , month :: Maybe Word -- ^ Calendar month
  , day :: Maybe Word -- ^ Calendar day
  } deriving (Eq, Show)

instance Default CalendarFlags where
  def = CalendarFlags Nothing Nothing Nothing Nothing

instance CmdParam CalendarFlags where
  serializeParam CalendarFlags {..} = concat
    [ maybeParam text $ \t -> ["--text", Text.unpack t]
    , maybeParam year $ \y -> ["--year", show y]
    , maybeParam month $ \m -> ["--month", show m]
    , maybeParam day $ \d -> ["--day", show d]
    ]

-- | Flags for the 'Entry' dialog
--
-- Use 'def' for default flags.
data EntryFlags = EntryFlags
  { text :: Maybe Text -- ^ Dialog text
  , entryText :: Maybe Text -- ^ Entry text
  , hideText :: Bool -- ^ Hide the text entered by the user
  } deriving (Eq, Show)

instance Default EntryFlags where
  def = EntryFlags Nothing Nothing False

instance CmdParam EntryFlags where
  serializeParam EntryFlags {..} = concat
    [ maybeParam text $ \t -> ["--text", Text.unpack t]
    , maybeParam entryText $ \t -> ["--entry-text", Text.unpack t]
    , boolParam hideText "--hide-text"
    ]

-- | Flags for the 'FileSelection' dialog
--
-- Use 'def' for default flags.
data FileSelectionFlags = FileSelectionFlags
  { fileName :: Maybe FilePath
      -- ^ File or directory to be selected by default
  , directory :: Bool
      -- ^ Activate directory-only selection
  , save :: Bool -- ^ Save mode
  , confirmOverwrite :: Bool
      -- ^ Confirm file selection if file name already exists

  -- TODO , fileFilter :: ???
  } deriving (Eq, Show)

instance Default FileSelectionFlags where
  def = FileSelectionFlags Nothing False False False

instance CmdParam FileSelectionFlags where
  serializeParam FileSelectionFlags {..} = concat
    [ maybeParam fileName $ \f -> ["--filename", f]
    , boolParam directory "--directory"
    , boolParam save "--save"
    , boolParam confirmOverwrite "--confirm-overwrite"
    ]

-- | Flags for the 'Error', 'Info', 'Notification' and 'Warning' dialogs
--
-- Note: 'noWrap' and 'noMarkup' have no effect on 'Notification' dialogs.
--
-- Use 'def' for default flags.
data InfoFlags = InfoFlags
  { text :: Maybe Text -- ^ Dialog text
  , noWrap :: Bool -- ^ Do not enable text wrapping
  , noMarkup :: Bool -- ^ Do not enable pango markup
  } deriving (Eq, Show)

instance Default InfoFlags where
  def = InfoFlags Nothing False False

instance CmdParam InfoFlags where
  serializeParam InfoFlags {..} = concat
    [ maybeParam text $ \t -> ["--text", Text.unpack t]
    , boolParam noWrap "--no-wrap"
    , boolParam noMarkup "--no-markup"
    ]

-- | What column(s) to return in a 'List' dialog
--
-- The default value is @`Col` 1@.
--
-- When 'All' is specified, the columns will be separated by newline characters
-- (@\\n@) in the result.
data ReturnedColumn a
  = All -- ^ Return all columns
  | Col a -- ^ Return the specified column (starting from 1)
  deriving (Eq, Show, Functor)

-- | Flags for the 'List' dialog
--
-- Use 'def' for default flags.
data ListFlags = ListFlags
  { text :: Maybe Text -- ^ Dialog text
  , editable :: Bool -- ^ Allow changes to text
  , returnColumn :: ReturnedColumn Word -- ^ What column(s) to return
  , hideColumn :: Maybe Word -- ^ Hide a specific column
  , hideHeader :: Bool -- ^ Hide the column headers
  } deriving (Eq, Show)

instance Default ListFlags where
  def = ListFlags Nothing False (Col 1) Nothing False

instance CmdParam ListFlags where
  serializeParam ListFlags {..} = concat
    [ maybeParam text $ \t -> ["--text", Text.unpack t]
    , boolParam editable "--editable"
    , case returnColumn of
        All -> ["--print-column", "ALL"]
        Col c -> ["--print-column", show c]
    , maybeParam hideColumn $ \c -> ["--hide-column", show c]
    , boolParam hideHeader "--hide-header"
    ]

-- | Increase the 'returnColumn' option, to cater for the first column in
-- radio/check lists
shiftColumns :: ListFlags -> ListFlags
shiftColumns ListFlags {..} = ListFlags
  { returnColumn = fmap shift returnColumn
  , hideColumn = fmap shift hideColumn
  , ..
  }
  where
    shift 0 = 0
    shift c = c+1

-- | Header for the selection column in a radio or check list (can be empty)
newtype SelectionHeader = SelectionHeader {unSelectionHeader :: Text}
  deriving (Eq, Show, IsString)

-- | List dialog type
data ListType a where
  Single :: ListType (Maybe Text)
  Multi :: ListType [Text]
  Radio :: SelectionHeader -> ListType (Maybe Text)
  Check :: SelectionHeader -> ListType [Text]

deriving instance Eq (ListType a)

deriving instance Show (ListType a)

-- | A radio list type with no selection header
radio :: ListType (Maybe Text)
radio = Radio ""

-- | A check list type with no selection header
check :: ListType [Text]
check = Check ""

-- | The contents of a list dialog
--
-- When used in a dialog, the matrix will be transformed in the following ways:
--
-- * Make sure that the matrix is rectangular and has at least one column and
-- one row. Any headers or elements that are added will be empty strings.
--
-- * Any newline characters will be turned into space characters. (This is
-- because newline characters are used internally as separators when returning
-- multiple rows and/or columns.)
data Matrix = Matrix
  { headers :: [Text] -- ^ Column headers
  , rows :: [[Text]] -- ^ Rows
  } deriving (Eq, Show)

-- | Return the width of a 'Matrix' after applying 'fixMatrix' (so the smallest
-- possible value is 1)
matrixWidth :: Matrix -> Int
matrixWidth Matrix {..} = max (length headers) (maximum $ 1 : map length rows)

-- | Make sure that the matrix is rectangular and has at least one column and
-- one row. Any headers or elements that are added will be empty strings.
--
-- The main reason for requring a non-empty matrix is that Zenity gives the
-- result @"(null)"@ otherwise. Not using empty matrices avoids the need to
-- handle such results.
fixMatrix :: Matrix -> Matrix
fixMatrix mat@Matrix {..} = Matrix
  { headers = widen $ if null headers then [""] else headers
  , rows = map widen $ if null rows then [[]] else rows
  }
  where
    width = matrixWidth mat
    widen as = as ++ replicate n ""
      where
        n = width - length as

-- | Add a first column for 'Radio' or 'Check' lists
addSelectionColumn :: SelectionHeader -> Matrix -> Matrix
addSelectionColumn hdr mat = Matrix
  { headers = unSelectionHeader hdr : headers
  , rows = map ("" :) rows
  }
  where
    Matrix {..} = fixMatrix mat
      -- Need to apply `fixMatrix` before adding the column. It doesn't matter
      -- that `fixMatrix` will be applied again inside `matrixFlags`.

matrixFlags :: Matrix -> [String]
matrixFlags mat = concat
  [ concatMap (\hdr -> ["--column", Text.unpack hdr]) headers
  , concatMap (map (map convertNewline . Text.unpack)) rows
  ]
  where
    Matrix {..} = fixMatrix mat
    convertNewline '\n' = ' '
    convertNewline c = c

-- | Zenity commands
--
-- Things to be aware of:
--
-- * In the very unlikely case of a file name containing newline characters,
-- 'MultiFileSelection' will give an incorrect result. This is because it uses
-- @\\n@ to separate the files returned from Zenity.
data Dialog a where
  Calendar :: CalendarFlags -> Dialog (Maybe Day)
  Entry :: EntryFlags -> Dialog (Maybe Text)
  Error :: InfoFlags -> Dialog ()
  FileSelection :: FileSelectionFlags -> Dialog (Maybe FilePath)
  MultiFileSelection :: FileSelectionFlags -> Dialog [FilePath]
  Info :: InfoFlags -> Dialog ()
  List :: ListType a -> ListFlags -> Matrix -> Dialog a
  Notification :: InfoFlags -> Dialog ()
  Warning :: InfoFlags -> Dialog ()
  -- TODO:
  -- Progress
  -- Question
  -- TextInfo
  -- Scale
  -- ColorSelection
  -- Password
  -- Forms

-- | Call Zenity with the given flags and capture its 'stdout'
callZenity ::
     Config
  -> [String] -- ^ Additional command-line flags
  -> IO Text
callZenity cfg flags = do
  when (debug cfg) $ putStrLn $ showCommandForUser "zenity" flags'
  (\(_, o, _) -> o) <$> Text.readProcessWithExitCode "zenity" flags' ""
  where
    flags' = configFlags cfg ++ flags

parseResult :: Text -> Maybe Text
parseResult "" = Nothing
  -- If the user entered an empty string, "\n" will be returned
parseResult t = Just $ Text.dropWhileEnd (== '\n') t

dateFormat = "%Y-%m-%d"

readDay :: MonadFail m => Text -> m Day
readDay = parseTimeM False defaultTimeLocale dateFormat . Text.unpack

-- | If @`returnColumn` = All@, this function will treat the input list as a
-- concated list of elements from selected rows, and turn that into a list of
-- selected rows. Each row in the result is represented as a single 'Text' value
-- with elements separated by newline characters (@\\n@).
--
-- The 'Matrix' argument is just used to determine the number of returned
-- columns (the width of the matrix).
--
-- If @`returnColumn` = Col c@, the argument list is returned unchanged.
unconcat ::
     ListFlags
  -> Matrix
  -> [Text] -- ^ Concated elements from selected rows
  -> [Text] -- ^ Selected rows
unconcat ListFlags {..} mat
  | All <- returnColumn =
    map (Text.dropEnd 1 . Text.unlines) . chunk (matrixWidth mat)
  | otherwise = id

-- | Run a 'Dialog' action
zenity :: Config -> Dialog a -> IO a
zenity cfg (Calendar flags) =
  traverse readDay . parseResult =<<
  callZenity
    cfg
    ("--calendar" : ("--date-format=" ++ dateFormat) : serializeParam flags)
zenity cfg (Entry flags) =
  fmap parseResult $ callZenity cfg ("--entry" : serializeParam flags)
zenity cfg (Error flags) =
  void $ callZenity cfg $ "--error" : serializeParam flags
zenity cfg (FileSelection flags) =
  fmap (fmap Text.unpack . parseResult) $
  callZenity cfg $ "--file-selection" : serializeParam flags
zenity cfg (MultiFileSelection flags) =
  fmap (lines . Text.unpack) $
  callZenity cfg $
  "--file-selection" :
  "--multiple" : "--separator" : "\n" : serializeParam flags
zenity cfg (Info flags) =
  void $ callZenity cfg $ "--info" : serializeParam flags
zenity cfg (List Single flags mat) =
  fmap parseResult $
  callZenity cfg $
  "--list" : "--separator" : "\n" : serializeParam flags ++ matrixFlags mat
zenity cfg (List Multi flags mat) =
  fmap (unconcat flags mat . Text.lines) $
  callZenity cfg $
  "--list" :
  "--separator" : "\n" : "--multiple" : serializeParam flags ++ matrixFlags mat
zenity cfg (List (Radio h) flags mat) =
  fmap parseResult $
  callZenity cfg $
  "--list" :
  "--separator" :
  "\n" :
  "--radiolist" :
  serializeParam (shiftColumns flags) ++ matrixFlags (addSelectionColumn h mat)
zenity cfg (List (Check h) flags mat) =
  fmap (unconcat flags mat . Text.lines) $
  callZenity cfg $
  "--list" :
  "--separator" :
  "\n" :
  "--checklist" :
  serializeParam (shiftColumns flags) ++ matrixFlags (addSelectionColumn h mat)
zenity cfg (Notification flags) =
  void $ callZenity cfg $ "--notification" : serializeParam flags'
  where
    flags' = flags {noWrap = False, noMarkup = False}
zenity cfg (Warning flags) =
  void $ callZenity cfg $ "--warning" : serializeParam flags



----------------------------------------
-- ** Extra dialogs
----------------------------------------

-- | Make a list selection dialog that selects values from an association list
--
-- Each item is a pair of a value of type @a@ and a text. Only the text will be
-- shown in the dialog, but the value associated with the selected text will be
-- returned.
keyedList ::
     (Show a, Read a, Functor f)
  => Config
  -> ListType (f Text)
  -> ListFlags -- ^ @returnColumn@ and @hideColumn@ will be ignored
  -> Text -- ^ Column head (can be empty)
  -> [(a, Text)] -- ^ List to select from
  -> IO (f a)
keyedList cfg ltype flags hd as =
  fmap (fmap (read . Text.unpack)) $
  zenity cfg $
  List ltype flags {returnColumn = Col 1, hideColumn = Just 1} $
  Matrix
  {headers = ["", hd], rows = [[Text.pack (show a), txt] | (a, txt) <- as]}
