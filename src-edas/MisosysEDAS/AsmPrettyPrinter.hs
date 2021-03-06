-- | Dual purpose module: Listing pass for the Misosys EDAS-compatible assembler as well as a general
-- pretty-printer for 'AsmStmt' assembler statements.
module Z80.MisosysEDAS.AsmPrettyPrinter
  ( printAsmStmtsPretty
  , asmStmtsPretty
  , asmStmtPretty
  ) where

import Numeric
-- import Data.Maybe
import Data.Char
import Data.Word
import Data.Bits
-- import Data.List
-- import Text.Parsec.Pos
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Machine.Utils
import Z80.MisosysEDAS.Types

-- | Utility function to pretty-print EDAS assembly statements to stdout.
printAsmStmtsPretty :: [AsmStmt]
                    -> IO ()
printAsmStmtsPretty stmts = mapM_ TIO.putStrLn (asmStmtsPretty stmts)

-- | Beautify a list of assembler statements. Note that this is not necessarily "one-for-one" with the assembler
-- input; spacing may vary due to reformatting, case is different (lower case is preferred for operation names),
-- as two examples.
asmStmtsPretty :: [AsmStmt]
               -> [T.Text]
asmStmtsPretty = concatMap (asmStmtPretty (T.singleton ordinaryStmt))

-- | Assembler statement beautifier.
asmStmtPretty :: T.Text                             -- ^ Line prefix (usually a space, but conditional code adds more)
              -> AsmStmt                            -- ^ Statement to beautify
              -> [T.Text]

-- Empty statement
asmStmtPretty lprefix (AsmStmt _pos symLab NoAsmOp cmnt _stmtAddr _bytes) =
  let tSymLabel = maybe T.empty id symLab
      -- Try to be somewhat heuristic about label placement on empty lines
      fmtCmnt (Comment srcCol c) =
        let tCmnt = T.cons ';' c
        in  if srcCol == 1 then
              tCmnt
            else if T.length tSymLabel == 0 && srcCol < 35 then
              T.append (T.replicate lenStmtLabel " ") tCmnt
            else
              let lenSoFar = lenStatement - T.length tSymLabel
              in  T.append (T.replicate lenSoFar " ") tCmnt
  in  [T.append lprefix (T.append tSymLabel (maybe T.empty fmtCmnt cmnt))]

-- Pseudo operation
asmStmtPretty lprefix (AsmStmt _srcLine symLab (Pseudo pseudo) cmnt _stmtAddr _bytes) =
  let tSymLabel            = emitSymLabel (emitColon pseudo) symLab
      tPseudo              = emitPseudo pseudo
      -- Don't emit ':' for equates; it's visually distracting
      emitColon (Equate _) = False
      emitColon _otherwise = True
      lenSoFar             = T.length lprefix + T.length tSymLabel + T.length tPseudo
  in  [ T.concat [ lprefix
                 , tSymLabel
                 , tPseudo
                 , emitComment lenSoFar  cmnt
                 ]
      ]

-- Conditional assembly
asmStmtPretty lprefix (CondPass _srcPos _symLabel _passNo _comment _stmtsTrue _elseLabel _elseComment
                                _stmtsFalse _endifLabel _endifComment) = undefined

asmStmtPretty lprefix (CondCmp _srcPos symLab cmpOp lExp rExp _condF cmnt trueStmts elseLab elseCmnt
                               falseStmts endifLab endifCmnt cResult) =
  let ifOp = T.concat [ formatOpName (T.append "if" (T.toLower cmpOp))
                      , formatExpr False lExp
                      , ", "
                      , formatExpr False rExp
                      ]
  in  emitCondStatement lprefix symLab ifOp cmnt trueStmts elseLab elseCmnt falseStmts endifLab endifCmnt cResult

asmStmtPretty lprefix (CondCmpStr _srcPos symLab cmpOp lExp rExp _strcmpF cmnt trueStmts elseLab elseCmnt
                                  falseStmts endifLab endifCmnt cResult) =
  let ifOp = T.concat [ formatOpName (T.append "if" (T.toLower cmpOp))
                      , lExp
                      , ", "
                      , rExp
                      ]
  in  emitCondStatement lprefix symLab ifOp cmnt trueStmts elseLab elseCmnt falseStmts endifLab endifCmnt cResult

asmStmtPretty lprefix (CondAsmEval _srcPos symLab expr cmnt trueStmts elseLab elseCmnt falseStmts endifLab
                           endifCmnt cResult) =
  emitCondStatement lprefix symLab (opWithExpr "if" expr) cmnt trueStmts elseLab elseCmnt falseStmts endifLab endifCmnt cResult

emitCondStatement :: T.Text
                     -> Maybe EDASLabel
                     -> T.Text
                     -> Maybe Comment
                     -> [AsmStmt]
                     -> Maybe EDASLabel
                     -> Maybe Comment
                     -> [AsmStmt]
                     -> Maybe EDASLabel
                     -> Maybe Comment
                     -> Bool
                     -> [T.Text]
emitCondStatement lprefix symLab ifOp cmnt trueStmts elseLab elseCmnt falseStmts endifLab endifCmnt cResult =
  let lprefix'             = T.snoc (T.init lprefix) condStmt
      tSymLabel            = T.append lprefix' (emitSymLabel True symLab)
      tIfStmt              = T.concat [ tSymLabel
                                      , ifOp
                                      , emitComment (T.length tSymLabel + T.length ifOp) cmnt
                                      ]
      tTrueStmts           = concatMap (asmStmtPretty (condPrefix cResult)) trueStmts
      tElseLabel           = T.append lprefix' (emitSymLabel True elseLab)
      tOpElse              = formatOpName "else"
      tElseStmt            = if (not . null) falseStmts then
                              [ T.concat [ tElseLabel
                                         , tOpElse
                                         , emitComment (T.length tElseLabel + T.length tOpElse) elseCmnt
                                         ]
                              ]
                             else
                              []
      tElseStmts           = if (not . null) falseStmts then
                               concatMap (asmStmtPretty (condPrefix (not cResult))) falseStmts
                             else
                               []
      tEndIfLabel          = T.append lprefix' (emitSymLabel True endifLab)
      tOpEndif             = formatOpName "endif"
      tEndifStmt           = T.concat [ tEndIfLabel
                                      , tOpEndif
                                      , emitComment (T.length tEndIfLabel + T.length tOpEndif) endifCmnt
                                      ]
      condPrefix cond      = T.snoc (T.snoc (T.init lprefix) $ if cond then condUsed else condStmt) ordinaryStmt
  in  (tIfStmt : tTrueStmts) ++ tElseStmt ++ tElseStmts ++ [tEndifStmt]

-- | Ordinary statement line prefix
ordinaryStmt :: Char
ordinaryStmt = ' '

-- | Line prefix mark for conditional code
condStmt :: Char
condStmt = '|'

-- | Line prefix mark for conditional code for which code was generated
condUsed :: Char
condUsed = '+'

-- | Length of the statement label's output column
lenStmtLabel :: Int
lenStmtLabel = 16

-- | Total length of the statement label and operation columns
lenStatement :: Int
lenStatement = 60

-- | Emit a pseudo-operation
emitPseudo :: EDASPseudo
           -> T.Text
emitPseudo (Equate expr)         = opWithExpr "equ" expr
emitPseudo (Origin  org)         = opWithExpr "org" org

emitPseudo (DefB blist)          =
  let formatElt (DBStr str)   = T.cons '\'' (T.snoc str '\'')
      formatElt (DBExpr expr) = formatExpr True expr
  in  T.append (formatOpName "db")
               (T.intercalate ", " (map formatElt blist))

emitPseudo (DefC rept fill)      = T.concat [ formatOpName "dc", formatExpr False rept, ", ", formatExpr True fill ]
emitPseudo (DefS rept)           = opWithExpr "ds" rept

emitPseudo (DefW wlist)          =
  let formatElt (DWChars c1 c2) = T.concat [ "'", T.singleton c1, T.singleton c2, "'" ]
      formatElt (DWExpr expr)   = formatExpr False expr
  in  T.append (formatOpName "dw")
               (T.intercalate ", " (map formatElt wlist))

emitPseudo (DSym sym)            = T.append (formatOpName "dsym") sym
emitPseudo (DExp expr)           = opWithExpr "dx" expr
emitPseudo AsmDate               = "date"
emitPseudo AsmTime               = "time"
emitPseudo (DefL expr)           = opWithExpr "defl" expr
emitPseudo (EndAsm _srcloc expr) = T.append (formatOpName "end") (maybe T.empty (formatExpr False) expr)
emitPseudo (Entry _srcloc expr)  = opWithExpr "entry" expr
emitPseudo (LoadOrg lorg)        = opWithExpr "lorg" lorg

-- | Common case for formatting pseudo-operations: output the name followed by an expression
opWithExpr :: T.Text
           -> EDASExpr
           -> T.Text
opWithExpr opName expr = T.append (formatOpName opName) $ formatExpr False expr

-- | Emit a left-justified statement label.
emitSymLabel :: Bool                        -- ^ Append a ':'?
             -> Maybe EDASLabel             -- ^ The optional statement/symbol label
             -> T.Text
emitSymLabel appendColon symLab =
  let tLabel lab = T.justifyLeft lenStmtLabel ' ' (if appendColon then T.snoc lab ':' else lab)
  in  maybe (T.replicate lenStmtLabel " ") tLabel symLab

-- | Emit the comment
emitComment :: Int                          -- ^ Current output column
            -> Maybe Comment
            -> T.Text
emitComment _atCol Nothing                    = T.empty
emitComment atCol (Just (Comment _col ctext))  =
  T.append (T.replicate (if atCol < lenStatement then lenStatement - atCol - 1 else 0) " ") (T.cons ';' ctext)

-- | Shorthand for formatting an operation's name.
formatOpName :: T.Text
             -> T.Text
formatOpName = T.justifyLeft 8 ' '

formatExpr :: Bool                                  -- ^ Constants are 8-bit (True)?
           -> EDASExpr                              -- ^ Expression to format
           -> T.Text

formatExpr _const8 EmptyExpr       = T.empty

-- Constant values are signed 16-bit for decimal, octal and binary. Hex is always treated as unsigned.
formatExpr  False  (Const16 _ val base)  =
  -- 'showIntAsBase' will invoke error if handed a negative number. Consequently, special handling
  -- here with two's complement manipulations and signs
  let sign       = if val < 0 then T.singleton '-' else T.empty
      posVal     = if val < 0 then (val16 `xor` 0xffff) + 1 else val16
      zeroPrefix = if val16 > 0x9fff then T.singleton '0' else T.empty
      val16      = fromIntegral val :: Word16
  in  case base of
        'd'        -> T.append sign (T.pack $ showIntAtBase 10 intToDigit (abs val) "")
        'h'        -> T.append zeroPrefix (T.snoc ((T.toUpper . asHex) val) 'H')
        'o'        -> T.snoc (T.append sign (T.pack $ showIntAtBase 8 intToDigit posVal "")) 'O'
        'b'        -> T.snoc (T.append sign (T.pack $ showIntAtBase 2 intToDigit posVal "")) 'B'
        _otherwise -> error ("formatExpr: Unknown base: '" ++ (show base) ++ "'")

-- Constant values are signed 8-bit for decimal, octal and binary. Hex is always treated as unsigned.
formatExpr  True   (Const16 _ val base)  =
  -- 'showIntAsBase' will invoke error if handed a negative number. Consequently, special handling
  -- here with two's complement manipulations and signs
  let val8       = fromIntegral val :: Word8
      sign       = if val < 0 then T.singleton '-' else T.empty
      zeroPrefix = if val8 > 0x9f then T.singleton '0' else T.empty
      posVal     = if val < 0 then (val8 `xor` 0xff) + 1 else val8
  in  case base of
        'd'        -> T.append sign (T.pack $ showIntAtBase 10 intToDigit (abs val) "")
        'h'        -> T.append zeroPrefix (T.snoc ((T.toUpper . asHex) val8) 'H')
        'o'        -> T.snoc (T.append sign (T.pack $ showIntAtBase 8 intToDigit posVal "")) 'O'
        'b'        -> T.snoc (T.append sign (T.pack $ showIntAtBase 2 intToDigit posVal "")) 'B'
        _otherwise -> error ("formatExpr: Unknown base: '" ++ (show base) ++ "'")

formatExpr _const8 (Var   _ vname) = vname
formatExpr _const8 CurrentPC       = "$"
formatExpr _const8 (AsmChar c)     = T.singleton c
formatExpr _const8 (Add a b)       = T.concat [ formatExpr False a, "+", formatExpr False b ]
formatExpr _const8 (Sub a b)       = T.concat [ formatExpr False a, "-", formatExpr False b ]
formatExpr _const8 (Mul a b)       = T.concat [ formatExpr False a, "*", formatExpr False b ]
formatExpr _const8 (Div a b)       = T.concat [ formatExpr False a, "/", formatExpr False b ]
formatExpr _const8 (Mod a b)       = T.concat [ formatExpr False a, ".mod.", formatExpr False b ]
formatExpr _const8 (Shift v amt)   = T.concat [ formatExpr False v, "<", formatExpr False amt ]
formatExpr _const8 (LogAnd a b)    = T.concat [ formatExpr False a, "&", formatExpr False b ]
formatExpr _const8 (LogOr a b)     = T.concat [ formatExpr False a, "!", formatExpr False b ]
formatExpr _const8 (LogXor a b)    = T.concat [ formatExpr False a, ".xor.", formatExpr False b ]
formatExpr _const8 (LogNE  a b)    = T.concat [ formatExpr False a, ".ne.", formatExpr False b ]
formatExpr _const8 (LogEQ  a b)    = T.concat [ formatExpr False a, ".eq.", formatExpr False b ]
formatExpr _const8 (LogGE  a b)    = T.concat [ formatExpr False a, ".ge.", formatExpr False b ]
formatExpr _const8 (LogGT  a b)    = T.concat [ formatExpr False a, ".gt.", formatExpr False b ]
formatExpr _const8 (LogLE  a b)    = T.concat [ formatExpr False a, ".le.", formatExpr False b ]
formatExpr _const8 (LogLT  a b)    = T.concat [ formatExpr False a, ".lt.", formatExpr False b ]
formatExpr _const8 (ShiftL a b)    = T.concat [ formatExpr False a, ".shl.", formatExpr False b ]
formatExpr _const8 (ShiftR a b)    = T.concat [ formatExpr False a, ".shr.", formatExpr False b ]
formatExpr _const8 (OnesCpl  x)    = T.append ".not." (formatExpr False x)
formatExpr _const8 (HighByte x)    = T.append ".high." (formatExpr False x)
formatExpr _const8 (LowByte  x)    = T.append ".low." (formatExpr False x)
