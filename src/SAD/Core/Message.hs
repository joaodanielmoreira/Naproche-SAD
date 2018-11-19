{-
Authors: Makarius Wenzel (2018)

Formal output messages, with Prover IDE support.
-}

{-# LANGUAGE TupleSections #-}

module SAD.Core.Message (Kind (..), pideActive,
  Report, ReportText, reportsText, reportText, reports, report,
  output, error, outputMain, outputExport, outputForTheL,
  outputParser, outputReason, outputThesis, outputSimp,
  errorExport, errorParser,
  trim
) where

import Prelude hiding (error)
import qualified Prelude (error)
import System.Environment
import Control.Monad
import Data.Maybe
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8

import SAD.Core.SourcePos (SourcePos)
import qualified SAD.Core.SourcePos as SourcePos

import Isabelle.Library as Isabelle
import qualified Isabelle.Value as Value
import qualified Isabelle.Markup as Markup
import qualified Isabelle.XML as XML
import qualified Isabelle.YXML as YXML


{- message kind -}

data Kind =
  STATE | WRITELN | INFORMATION | TRACING | WARNING | LEGACY | ERROR

instance Show Kind where
  show WARNING = "Warning"
  show LEGACY = "Legacy feature"
  show ERROR = "Error"
  show _ = ""


{- PIDE context -}

data PIDE = PIDE {pideID :: String, pideFileName :: String}

pideContext :: IO (Maybe PIDE)
pideContext = do
  fileName <- fromMaybe "" <$> lookupEnv "NAPROCHE_FILE_NAME"
  pide <- lookupEnv "NAPROCHE_PIDE"
  case pide of
    Nothing -> return Nothing
    Just "" -> return Nothing
    Just id -> return (Just $ PIDE id fileName)

pideActive :: IO Bool
pideActive = isJust <$> pideContext


{- output as PIDE message -}

kindXML :: Kind -> String
kindXML STATE = Markup.stateN
kindXML WRITELN = Markup.writelnN
kindXML INFORMATION = Markup.informationN
kindXML TRACING = Markup.tracingN
kindXML WARNING = Markup.warningN
kindXML LEGACY = Markup.legacyN
kindXML ERROR = Markup.errorN

posProperties :: PIDE -> SourcePos -> [(String, String)]
posProperties PIDE{pideID = id, pideFileName = defaultFile} pos =
  (if null id then [] else [(Markup.idN, id)]) ++
  (if null file && null defaultFile then []
   else [(Markup.fileN, if null file then defaultFile else file)]) ++
  (if line <= 0 then [] else [(Markup.lineN, Value.print_int line)]) ++
  (if offset <= 0 then [] else [(Markup.offsetN, Value.print_int offset)]) ++
  (if endOffset <= 0 then [] else [(Markup.end_offsetN, Value.print_int endOffset)])
  where
    file = SourcePos.sourceFile pos
    line = SourcePos.sourceLine pos
    offset = SourcePos.sourceOffset pos
    endOffset = SourcePos.sourceEndOffset pos

xmlMessage :: PIDE -> String -> Kind -> SourcePos -> String -> XML.Tree
xmlMessage pide origin kind pos msg =
  XML.Elem ((kindXML kind, props), [XML.Text msg])
  where
    props0 = posProperties pide pos
    props = if null origin then props0 else ("origin", origin) : props0

pideMessage :: String -> String
pideMessage s = "\1" ++ Value.print_int len ++ "\n" ++ s
  where len = ByteString.length (UTF8.fromString s)
    
    
{- markup reports -}

type Report = (SourcePos, Markup.T)
type ReportText = (Report, String)

reportsText :: [ReportText] -> IO ()
reportsText args = do
  pide <- pideContext
  when (isJust pide && not (null args)) $ putStrLn $ pideMessage $ YXML.string_of $
    XML.Elem (Markup.report,
      map (\((pos, markup), txt) ->
        let
          markup' = Markup.properties (posProperties (fromJust pide) pos) markup
          body = if null txt then [] else [XML.Text txt]
        in XML.Elem (markup', body)) args)

reportText :: SourcePos -> Markup.T -> String -> IO ()
reportText pos markup txt = reportsText [((pos, markup), txt)]

reports :: [Report] -> IO ()
reports = reportsText . map (, "")

report :: SourcePos -> Markup.T -> IO ()
report pos markup = reports [(pos, markup)]


{- output -}

messageText :: Maybe PIDE -> String -> Kind -> SourcePos -> String -> String
messageText pide origin kind pos msg =
  if isJust pide then
    pideMessage $ YXML.string_of $ xmlMessage (fromJust pide) origin kind pos msg
  else
    (if null origin then "" else "[" ++ origin ++ "] ") ++
    (case show kind of "" -> "" ; s -> s ++ ": ") ++
    (case show pos of "" -> ""; s -> s ++ "\n") ++ msg

output :: String -> Kind -> SourcePos -> String -> IO ()
output origin kind pos msg = do
  pide <- pideContext
  putStrLn $ messageText pide origin kind pos msg

error :: String -> SourcePos -> String -> IO a
error origin pos msg = do
  pide <- pideContext
  errorWithoutStackTrace $ messageText pide origin ERROR pos msg


{- specific messages -}

outputMain, outputExport, outputForTheL, outputParser, outputReason, outputSimp
  :: Kind -> SourcePos -> String -> IO ()
outputMain = output "Main"
outputExport = output "Export"
outputForTheL = output "ForTheL"
outputParser = output "Parser"
outputReason = output "Reasoner"
outputSimp = output "Simplifier"

errorExport = error "Export"
errorParser = error "Parser"

outputThesis :: Kind -> SourcePos -> Int -> String -> IO ()
outputThesis kind pos indent msg =
  output "Thesis" kind pos $ replicate (3 * indent) ' ' ++ msg

trim :: String -> String
trim = Isabelle.trim_line
