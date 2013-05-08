module Main where

import           Control.Applicative
import           Control.Concurrent.ParallelIO (stopGlobalPool, parallel)
import           Control.DeepSeq (NFData(..))
import           Control.Exception
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Data
import           Data.Foldable
import           Data.Function (on)
import           Data.Function.Pointless ((.:))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Format (format)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Traversable
import           Data.Typeable
import           Debug.Trace (trace)
import           Development.Shake
import           Filesystem
import           Filesystem.Path.CurrentOS
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           Options.Applicative
import           Prelude hiding (filter, FilePath)
import           Shelly hiding (find)
import           System.Environment
import           System.IO hiding (FilePath)
import           System.IO.Storage (withStore, putValue, getValue)
import           System.Locale
import           System.Log.Formatter (tfLogFormatter)
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           Text.Printf (printf)
import           Text.Regex.Posix ((=~))

default (Integer, Text)

programName :: String
programName = "foo"

data Options = Options
    { verbose :: Bool
    , debug   :: Bool
    , dryRun  :: Bool
    , jobs    :: Int } deriving (Data, Typeable, Show, Eq)

options :: Parser Options
options =
    Options
    <$> switch (short 'v' <> long "verbose" <> help "Enable verbose output")
    <*> switch (short 'D' <> long "debug"   <> help "Enable debug output")
    <*> switch (short 'n' <> long "dry-run" <> help "Make no actual changes")
    <*> option (short 'j' <> long "jobs" <>
                help "Use N capabilities" <> value 1)

main :: IO ()
main = execParser opts >>= mainWrapper doMain >> stopGlobalPool
  where opts = info (helper <*> options)
                    (fullDesc
                     <> progDesc ""
                     <> header "foo - short description of foo")

mainWrapper :: IO () -> Options -> IO ()
mainWrapper act opts = do
    -- Set the number of processors to use.  Requires linking with -threaded.
    GHC.Conc.setNumCapabilities (jobs opts)

    -- Configure the logging subsystem to have more attractive output.
    let level | debug opts   = DEBUG
              | verbose opts = INFO
              | otherwise    = NOTICE
    h <- (`setFormatter` tfLogFormatter "%H:%M:%S" "$time - [$prio] $msg")
         <$> streamHandler System.IO.stderr level
    removeAllHandlers
    updateGlobalLogger programName (setLevel level)
    updateGlobalLogger programName (addHandler h)

    -- Create an io-storage slot named "opts" for convenient access to
    -- command-line options from any IO action.  Use 'getOption' to retrieve.
    withStore "main" $ putValue "main" "opts" opts >> act

doMain :: IO ()
doMain = shelly $ silently $ do
    criticalL "The real work of foo goes here"

-------------------------------------------------------------------------------

-- matchText :: Text -> Text -> Bool
-- matchText = flip ((=~) `on` T.unpack)

getOption' :: Data a => (a -> b) -> IO b
getOption' option = do
  opts <- getValue "main" "opts"
  return $ fromJust $ option <$> opts

getOption :: Data a => (a -> b) -> Sh b
getOption = liftIO . getOption'

doRun :: (FilePath -> [Text] -> Sh a)
      -> (Text -> Sh ())
      -> Sh a -> Bool -> Bool -> FilePath -> [Text]
      -> Sh a
doRun f g retval doLog heedDry n xs = do
  when doLog $
    g $ format "{} {}" [toTextIgnore n, T.unwords xs]

  dry <- getOption dryRun
  if dry && heedDry
    then retval
    else f n xs

drun :: Bool -> FilePath -> [Text] -> Sh Text
drun = doRun run debugL (return "") True

drun_ :: Bool -> FilePath -> [Text] -> Sh ()
drun_ x = void .: doRun run_ debugL (return ()) True x

vrun :: FilePath -> [Text] -> Sh Text
vrun = doRun run infoL (return "") True True

vrun_ :: FilePath -> [Text] -> Sh ()
vrun_ = void .: doRun run_ infoL (return ()) True True

srun :: FilePath -> [Text] -> Sh Text
srun = doRun run infoL (return "") False True

srun_ :: FilePath -> [Text] -> Sh ()
srun_ = void .: doRun run_ infoL (return ()) False True

liftL :: (String -> String -> IO ()) -> Text -> Sh ()
liftL f = liftIO . f programName . T.unpack

debugL    :: Text -> Sh (); debugL    = liftL debugM
infoL     :: Text -> Sh (); infoL     = liftL infoM
noticeL   :: Text -> Sh (); noticeL   = liftL noticeM
warningL  :: Text -> Sh (); warningL  = liftL warningM
errorL    :: Text -> Sh (); errorL    = liftL errorM
criticalL :: Text -> Sh (); criticalL = liftL criticalM

-- Main.hs ends here
