-- | Dual purpose module: Listing pass for the Misosys EDAS-compatible assembler as well as a general
-- pretty-printer for 'AsmStmt' assembler statements.
module Z80.MisosysEDAS.AsmPrettyPrinter
  ( printAsmStmtsPretty
  , asmStmtsPretty
  , asmStmtPretty
  ) where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Z80.MisosysEDAS.Types

printAsmStmtsPretty :: [AsmStmt]
                    -> IO ()
printAsmStmtsPretty stmts = mapM_ TIO.putStrLn (asmStmtsPretty stmts)

asmStmtsPretty :: [AsmStmt]
               -> [T.Text]
asmStmtsPretty stmts = map asmStmtPretty stmts

-- Empty instruction with optional label and comment
asmStmtPretty (AsmStmt symLabel NoAsmOp comment _stmtAddr _bytes) =
  let tSymLabel = maybe T.empty (\lab -> T.append lab ":") symLabel
  in  T.append tSymLabel (emitComment (T.length tSymLabel) comment)

-- Pseudo operation with optional label and comment

emitComment _atCol Nothing = T.empty
emitComment atCol (Just (Comment col ctext)) =
  T.append (T.replicate (if atCol < col then col - atCol else 0) " ") ctext

