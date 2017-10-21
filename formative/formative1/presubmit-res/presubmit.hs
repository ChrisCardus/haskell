
{-# LANGUAGE Safe #-}
{-# LANGUAGE ImplicitParams #-}

module Presubmit where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.Process
import Text.Printf

{-

  OVERVIEW OF SCRIPT

  This Haskell script checks if a student solution (e.g. Formative1.hs) is
  ready for submission.

  To do that, it

  - verifies that the file exists
  - makes a temporary directory 'presubmit-temp',
  - verifies that we are in a correct git repository
  - looks at all imports, and verifies that every import is on a whitelist,
  - copies Formative1.hs into presubmit-temp,
  - copies other auxiliary files into it (Lib.hs, Mould.hs)
  - compiles Lib.hs
  - compiles Formative1.hs,
  - compiles Mould.hs,
  - checks if presubmit.sh detected that we are in the lab,
  - prints a concluding verdict,
  - on success, removes (recursively) the temporary directory.

  Inside Mould.hs, the student solution is imported safely, and there is a
  type assertion against all the solutions made by the students. Furthermore,
  Mould.hs re-exports all these solutions with the expected type.

  If we know that Mould.hs compiles together with the student solution file,
  then when can test the student solution via Mould.hs and everything should
  work perfectly.

  This approach avoids a couple of problems. For instance:

  - It verifies that {-# LANGUAGE Safe #-} is set in
    the student file.
  - It verifies that no other files are required in the student solution.
  - It verifies that no non-base libraries are required in the student solution.
  - It verifies that students use data types from Lib, rather than copying the
    definitions. (This prevents the marking script from working.)
  - It verifies that students did not accidentally modify Lib.hs or Mould.hs.

  If we find we are in a correct git repository, then we take Lib.hs and
  Mould.hs from there, rather from the working directory. We take them from
  the last commit in branch origin/master, which should be our fp-learning
  repository. Additionally, we compare the versions in the working directory
  against the versions in our commit. If they differ, we remark that students
  might find problems, but note that this is not a problem for the presubmit
  check. We suggest steps to restore Lib.hs and Mould.hs to the original
  version.

-}


-- CONFIGURATION
--
-- This script sets up the "configuration" once at the beginning, and then the
-- configuration is passed to all functions using an implicit parameter.

data Config = Config {
    confVerbose :: Bool,
    -- For instance "Formative1"
    confStudentBase :: String,
    confInLab :: Bool
  }

verbose :: (?config::Config) => Bool
verbose = confVerbose ?config

studentFile :: (?config::Config) => String
studentFile = studentBase ++ ".hs"

studentTemplate :: (?config::Config) => String
studentTemplate = studentBase ++ "Template.hs"

studentBase :: (?config::Config) => String
studentBase = confStudentBase ?config

-- STATE
--
-- We have one global variable, "taint", for which we use a StateT monad
-- transformer around IO.

data Taint = Tainted | Untainted deriving (Eq, Show)

type StateIO a = StateT Taint IO a

markTainted :: StateIO ()
markTainted = put Tainted

-- Auxiliary files that must be copied too

auxiliaryFiles :: [String]
auxiliaryFiles = [
    "Lib.hs",
    "Mould.hs"
  ]

allowedGitOrigins = [
    "git@git.cs.bham.ac.uk:mhe/fp-learning-2017-2018.git",
    "git@git.cs.bham.ac.uk:mhe/fp-learning-2017-2018",
    "ssh://git@git.cs.bham.ac.uk/mhe/fp-learning-2017-2018.git",
    "ssh://git@git.cs.bham.ac.uk/mhe/fp-learning-2017-2018",
    "https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018.git",
    "https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018",
    "http://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018.git",
    "http://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018"
  ]

moduleWhitelist = [

    -- custom
    "Lib",

    -- package 'base'
    "Control.Applicative", "Control.Arrow", "Control.Category", "Control.Concurrent", "Control.Concurrent.Chan", "Control.Concurrent.MVar", "Control.Concurrent.QSem", "Control.Concurrent.QSemN", "Control.Exception", "Control.Exception.Base", "Control.Monad", "Control.Monad.Fail", "Control.Monad.Fix", "Control.Monad.IO.Class", "Control.Monad.Instances", "Control.Monad.ST", "Control.Monad.ST.Lazy", "Control.Monad.ST.Lazy.Safe", "Control.Monad.ST.Lazy.Unsafe", "Control.Monad.ST.Safe", "Control.Monad.ST.Strict", "Control.Monad.ST.Unsafe", "Control.Monad.Zip", "Data.Bifunctor", "Data.Bits", "Data.Bool", "Data.Char", "Data.Coerce", "Data.Complex", "Data.Data", "Data.Dynamic", "Data.Either", "Data.Eq", "Data.Fixed", "Data.Foldable", "Data.Function", "Data.Functor", "Data.Functor.Classes", "Data.Functor.Compose", "Data.Functor.Const", "Data.Functor.Identity", "Data.Functor.Product", "Data.Functor.Sum", "Data.IORef", "Data.Int", "Data.Ix", "Data.Kind", "Data.List", "Data.List.NonEmpty", "Data.Maybe", "Data.Monoid", "Data.Ord", "Data.Proxy", "Data.Ratio", "Data.STRef", "Data.STRef.Lazy", "Data.STRef.Strict", "Data.Semigroup", "Data.String", "Data.Traversable", "Data.Tuple", "Data.Type.Bool", "Data.Type.Coercion", "Data.Type.Equality", "Data.Typeable", "Data.Typeable.Internal", "Data.Unique", "Data.Version", "Data.Void", "Data.Word", "Debug.Trace", "Foreign", "Foreign.C", "Foreign.C.Error", "Foreign.C.String", "Foreign.C.Types", "Foreign.Concurrent", "Foreign.ForeignPtr", "Foreign.ForeignPtr.Safe", "Foreign.ForeignPtr.Unsafe", "Foreign.Marshal", "Foreign.Marshal.Alloc", "Foreign.Marshal.Array", "Foreign.Marshal.Error", "Foreign.Marshal.Pool", "Foreign.Marshal.Safe", "Foreign.Marshal.Unsafe", "Foreign.Marshal.Utils", "Foreign.Ptr", "Foreign.Safe", "Foreign.StablePtr", "Foreign.Storable", "Numeric", "Numeric.Natural", "Prelude", "System.CPUTime", "System.Console.GetOpt", "System.Environment", "System.Exit", "System.IO", "System.IO.Error", "System.IO.Unsafe", "System.Info", "System.Timeout", "Text.ParserCombinators.ReadP", "Text.ParserCombinators.ReadPrec", "Text.Printf", "Text.Read", "Text.Read.Lex", "Text.Show", "Text.Show.Functions", "Unsafe.Coerce",

    -- package 'mtl'
    "Control.Monad.Cont", "Control.Monad.Cont.Class", "Control.Monad.Error", "Control.Monad.Error.Class", "Control.Monad.Except", "Control.Monad.Identity", "Control.Monad.List", "Control.Monad.RWS", "Control.Monad.RWS.Class", "Control.Monad.RWS.Lazy", "Control.Monad.RWS.Strict", "Control.Monad.Reader", "Control.Monad.Reader.Class", "Control.Monad.State", "Control.Monad.State.Class", "Control.Monad.State.Lazy", "Control.Monad.State.Strict", "Control.Monad.Trans", "Control.Monad.Writer", "Control.Monad.Writer.Class", "Control.Monad.Writer.Lazy", "Control.Monad.Writer.Strict",

    -- package 'random'
    "System.Random",

    -- package 'deepseq'
    "Control.DeepSeq"
  ]

-- Abort with an error message.
abortMsg :: String -> IO ()
abortMsg msg = do
    die ("ERROR: " ++ msg)

abort :: IO ()
abort = exitWith (ExitFailure 1)


banner :: (?config::Config) => String
banner = unlines [
    "Presubmit test, v4.",
    "",
    "This script will look at your " ++ studentFile ++" to see whether we can",
    "mark it in the current form. is. It will either give you an error, or it",
    "will say that you are ready for submission. We expect that most people",
    "will already be ready for submission; this presubmit script is meant to",
    "catch out any things you might have done that we didn't foresee, that",
    "might cause it to be unmarkable.",
    "",
    "If it's unclear why you're getting an error, check the Facebook group, ask, or",
    "if you need to show your source code, send an email to Bram on bram@bram.xyz.",
    "",
    "Please always use the latest version of this script, which you can obtain",
    "by running 'git pull'.",

    ""
  ]

-- Run cmd with system, assert that it returned status 0. If not, abort the
-- presubmit with a failure.
--
-- In this presubmit, we only need to run static strings as commands, so
-- there is no danger of any funny business with the shell parsing a
-- generated command in a funny way. (viz. "command injection")
--
-- If `blame` is Intl, then this command is supposed to succeed regardless
-- of the student's actions. If `blame` is (Extl problem hint), then we
-- expect that the student can fix this problem themselves.

data Blame = Intl | Extl [String] [String]

assertCmd :: (?config::Config) => String -> Blame -> IO ()
assertCmd cmd blame = do
    when verbose $ do
        putStrLn $ "+ " ++ cmd

    exitCode <- system cmd
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure n -> do
            putStrLn ""
            printBlame blame cmd exitCode
            abort

printBlame Intl cmd n = do
    putStrLn $ "INTERNAL ERROR: subcommand '" ++ cmd ++ "' failed"
    putStrLn $ "with exit code "++ show n ++ ". We could not verify that your solution is"
    putStrLn $ "ready for submission. Please inform the teaching assistants or Martin and"
    putStrLn $ "we will get it fixed. If these people are not around, then you can send an"
    putStrLn $ "email or comment on Facebook. Thanks."

printBlame (Extl problem hint) cmd n = do
    putStrLn $ "The presubmit failed on that last command. We could not verify that your solution"
    putStrLn $ "is ready for submission."
    putStrLn $ ""
    when (problem /= []) $ do
        putStrLn $ "Problem:"
        putStrLn $ ""
        printIndentedLines "    " problem
        putStrLn $ ""
    when (hint /= []) $ do
        putStrLn $ "The following action may improve the situation:"
        putStrLn $ ""
        printIndentedLines "    " hint

step_prepare :: (?config::Config) => IO ()
step_prepare = do
    assertCmd ("test -e " ++ studentFile)
        (Extl
            [studentFile ++ " does not exist"]
            [printf "You should copy %s to %s and work in that." studentTemplate studentBase])
    assertCmd ("test -f " ++ studentFile)
        (Extl [studentFile ++ " not a regular file"] [])
    assertCmd "rm -rf presubmit-temp" Intl
    assertCmd "mkdir presubmit-temp" Intl
    assertCmd (printf "cp %s presubmit-temp/" studentFile) Intl

-- Check if we are currently in a Git repository that has the right URL for
-- the 'origin' remote.
--
-- If so, then we fetch all auxiliary files from Git.
--
-- If not, then we show a warning and we mark the global state as tainted.
step_git_purity :: (?config::Config) => IO Bool
step_git_purity = alsoPrintError $ do
    -- Test if we are in a Git repository
    exit1 <- system "git remote > /dev/null"
    if exit1 /= ExitSuccess
      -- Apparently not
    then notPure "The current directory is not a Git checkout."
    else do
        -- Do we have an 'origin' remote?
        (exit2, out2', err2) <- readCreateProcessWithExitCode (shell "git remote get-url origin") ""
        let out2 = strip out2'
        if exit2 /= ExitSuccess
        -- ...no
        then notPure $ "This Git repository was not cloned from the correct URL."
        -- Is it the correct URL?
        else if not (out2 `elem` allowedGitOrigins)
        -- ...no
        then notPure $ "This Git repository was not cloned from the correct URL. Found URL: " ++ show out2
        -- Does the current path contain the auxiliary files, in the current directory?
        else do
            -- For each auxiliary file, try to find it in the last commit in origin/master
            exits3 <- mapM
                (\aux -> system $ "git rev-parse --verify -q origin/master:./" ++ aux ++ " >/dev/null")
                auxiliaryFiles

            -- If so, then yes, we have a correct Git setup.
            if all (==ExitSuccess) exits3
            then return True
            else notPure $ "You don't seem to be in the original directory that contains " ++ studentTemplate ++ "."
  where
    notPure msg = do
        -- Exclamation mark because this will taint the result.
        putStrLn $ "! Warning: " ++ msg
        putStrLn $ "  Continuing, taking auxiliary files from current directory."
        return False
    alsoPrintError mainBody = do
        -- gitProper is the result of our function, and it is true if our git environment is proper.
        -- If it is not proper, then we suggest to make a fresh checkout.
        gitProper <- mainBody
        when (not gitProper) $ do
            putStrLn $ ""
            putStrLn $ "  Suggested fix: Make a fresh clone of the fp-learning repository, move your"
            putStrLn $ printf "  %s to there, and re-run the presubmit check there." studentFile
        return gitProper


-- Check modules used by studentFile against whitelist.
step_modules :: (?config::Config) => StateIO ()
step_modules = do
    contents <- lift $ readFile studentFile
    let modulesInFile = parseModules contents :: [String]
    let notOk = filter (\mod -> not $ elem mod moduleWhitelist) modulesInFile
    let plural a b | length notOk >= 2 = b
        plural a b | otherwise         = a
    if notOk == []
    then do
        lift.putStrLn $ printf "* All %d imports found in %s are in whitelist, good." (length modulesInFile) studentFile
    else do
        lift.putStrLn $ printf "! Error: found import%s that %s not in our whitelist:" (plural "" "s") (plural "is" "are")
        lift.putStrLn $ "    " ++ show notOk
        lift.putStrLn $ ""
        lift.putStrLn $ "  If these are basic modules, then you can ask Martin or the"
        lift.putStrLn $ "  teaching assistants if we will allow them."
        lift.putStrLn $ ""
        markTainted

-- Given a file's contents, make a decent effort to parse the module names
-- that are imported from that file.
parseModules :: String -> [String]
parseModules contents = do
    line1 <- lines contents
    let line2 = strip line1
    -- Filter lines that start "import ", and strip that off
    line3 <- maybeToList $ stripPrefix "import " line2
    let line4 = strip line3
    -- Strip off "safe ", but don't filter here
    let line5 = line4 `fromMaybe` stripPrefix "safe " line4
    let line6 = strip line5
    -- Strip off "qualified ", but don't filter here
    let line7 = line6 `fromMaybe` stripPrefix "qualified " line6
    let line8 = strip line7
    return $ takeWhile isModuleChar line8
  where
    isModuleChar c = isAlphaNum c || c == '.'

-- Copy auxiliary files in preparation of compiling/linking.
step_prepare_link :: (?config::Config) => Bool -> IO ()
step_prepare_link gitProper = mapM_ (step_prepare_link_file gitProper) auxiliaryFiles

step_prepare_link_file :: (?config::Config) => Bool -> String -> IO ()
step_prepare_link_file True fileName = do
    assertCmd (printf "git cat-file blob origin/master:./%s > presubmit-temp/%s" fileName fileName)
        Intl
    officialVersion <- readFile $ "presubmit-temp/" ++ fileName
    workingVersion <- readFile fileName
    when (norm officialVersion /= norm workingVersion) $ do
        warnVersionDiscrepancy
  where
    -- Normalise file contents by removing trailing whitespace and empty
    -- lines.
    norm = unlines . filter (/="") . map stripRightComm . lines
    warnVersionDiscrepancy = printIndentedLines "- " [
        printf "Warning: your version of %s seems to be different from the official" fileName,
        "version I found. This is no problem for the presubmit, but this might",
        "cause some discrepancies compared to when you test yourself.",
        "",
        "By running the command",
        printf "    git checkout origin/master -- %s" fileName,
        "you can restore it to the official version I found. Watch out: this",
        "immediately undoes all changes you may have made yourself."
      ]

step_prepare_link_file False fileName = do
    assertCmd (printf "test -f %s" fileName)
        (Extl
            [printf "File %s not found." fileName]
            ["Restore it to its original version, or make a fresh git clone."])
    assertCmd (printf "cp %s presubmit-temp/%s" fileName fileName)
        Intl

step_link :: (?config::Config) => StateIO ()
step_link = do
    taint <- get
    lift $ assertCmd ("cd presubmit-temp && rm -f *.hi *.o && ghc -XSafe Lib") (blameForLib taint)
    lift $ assertCmd ("cd presubmit-temp && rm -f *.hi *.o && ghc -XSafe " ++ studentBase)
        (Extl
            [printf "I put your %s in a new directory together with" studentFile,
             (intercalate "," auxiliaryFiles) ++ ", but when I",
             printf "then let GHC compile %s in safe mode, it fails." studentFile
             ]
            [] -- there is no fix we can suggest
            )
    lift $ assertCmd ("cd presubmit-temp && rm -f *.hi *.o && ghc -XSafe Mould")
        (Extl
            [printf "I put your %s in a new directory together with" studentFile,
             (intercalate "," auxiliaryFiles) ++ ", but when I",
             printf "then let GHC compile Mould.hs in safe mode, it fails."
             ]
            [printf "Check that in your %s the types of all questions are exactly" studentFile,
             "as set in the assignment description. If Lib.hs defines data types,",
             "check that you import those data types rather than copy the",
             "definition of the data type."
            ])
  where
    blameForLib Untainted = Intl
    blameForLib Tainted = Extl ["Could not compile the Lib.hs I found in your directory."] []

step_check_lab :: (?config::Config) => StateIO ()
step_check_lab = do
    when (not $ confInLab ?config) $ do
        lift $ printIndentedLines "! " [
            "Error: Presubmit script is not running in the lab. Please run",
            "this script in the lab. To do so, you can ssh into tinky-winky,",
            "then use the command ssh-lab to connect to a lab machine.",
            "You can copy files across with scp:",
            printf "    scp %s abc123@tinky-winky.cs.bham.ac.uk:work/" studentFile,
            "to copy your file to ~/work/  ."
          ]
        markTainted
step_result :: (?config::Config) => StateIO ()
step_result = do
    taint <- get
    case taint of
      Tainted -> do
        lift $ printIndentedLines "" [
            "",
            printf "Unfortunately, we could not verify that your %s is" studentFile,
            "ready for submission. Please check the messages above."
          ]
        lift abort
      Untainted -> do
        lift $ printIndentedLines "" [
            "",
            "Conclusion: Everything looks okay! You should be fine to",
            printf "submit your %s." studentFile
          ]

step_cleanup :: (?config::Config) => IO ()
step_cleanup = do
    assertCmd "rm -rf presubmit-temp" Intl


main :: IO ()
main = do
    -- Fetch environment variables
    sanity <- lookupEnv "PRESUBMIT"
    when (sanity == Nothing) $ do
        abortMsg "The presubmit script must be run by typing \"sh presubmit.sh\"."
    stuBase <- getEnv "PRESUBMIT_STUDENT_BASE"
    verb <- getEnv "PRESUBMIT_VERBOSE"
    inLab <- getEnv "PRESUBMIT_IN_LAB"

    -- Create config, run main'
    let ?config=Config {confVerbose=parseVerb verb, confStudentBase=stuBase,
        confInLab=parseInLab inLab}
    runStateT main' Untainted
    return ()
  where
    parseVerb "1" = True
    parseVerb "0" = False
    parseInLab "yes" = True
    parseInLab "no" = False

mainConvenience :: IO ()
mainConvenience = do
    let ?config=Config {confVerbose=True, confStudentBase="Formative1", confInLab=True}
    runStateT main' Untainted
    return ()

main' :: (?config::Config) => StateIO ()
main' = do
    lift $ putStrLn banner
    lift $ step_prepare
    gitProper <- lift $ step_git_purity
    when (not gitProper) $ do
        markTainted
    step_modules
    lift $ step_prepare_link gitProper
    step_link
    step_check_lab
    step_result
    lift $ step_cleanup



-- UTILITY CODE

-- Print each string in the string list on its own line, indented by (length
-- prefix) spaces. The first line is indented by prefix, rather than purely by
-- spaces.
printIndentedLines :: String -> [String] -> IO ()
printIndentedLines prefix lines = do
    putStrLn $ prefix ++ head lines
    let prefix2 = replicate (length prefix) ' '
    mapM_ (\line -> putStrLn $ prefix2 ++ line) (tail lines)

-- Strip leading and trailing spaces from a string.
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Strip trailing spaces and inline comments.
stripRightComm :: String -> String
stripRightComm = stripComment . reverse . dropWhile isSpace . reverse

stripComment "" = ""
stripComment xs | " --" `isPrefixOf` xs = ""
stripComment (x:xs) = x : stripComment xs
