-- | Types shared across the Misosys EDAS assembler
module Z80.MisosysEDAS.Types where

import Control.Lens
import Data.Word
import Data.Int
import Data.Time
import Data.Maybe
import Text.Parsec.Pos
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector.Unboxed (Vector)

import Z80.Processor
import Z80.InstructionSet

-- | Synonym for an assembler label
type EDASLabel = T.Text

-- | An assembler comment, preserving its column position in the input
data Comment = Comment Int T.Text
  deriving (Show)

-- | Assembler operation: a Z80 instruction or EDAS pseudo operation
data AsmOp where
  Insn   :: (AsmEvalCtx -> Either T.Text Z80instruction)
                                                -- Left: Warning or error message
                                                -- Right: An instruction constructor, may involve an expression
                                                -- and an evaluation context. 
         -> AsmOp
  Pseudo :: EDASPseudo                          -- A pseudo operation, e.g., equate, db, ds, etc.
         -> AsmOp
  deriving (Show)

instance Show (AsmEvalCtx -> Either T.Text Z80instruction) where
  show _ctxInsn = "<asm insn eval>"

-- | EDAS\' pseudo operations
data EDASPseudo where
  Equate  :: EDASExpr           -- Symbol constant equate
          -> EDASPseudo
  Origin  :: EDASExpr           -- Assembly origin (start) address
          -> EDASPseudo
  DefB    :: [DBValue]          -- Define bytes
          -> EDASPseudo
  DefC    :: EDASExpr           -- Define constant byte fill (repeat, const)
          -> EDASExpr
          -> EDASPseudo
  DefS    :: EDASExpr           -- Define space ('DefC' with 0 as the constant)
          -> EDASPseudo
  DefW    :: [DWValue]          -- Define little endian words
          -> EDASPseudo
  DSym    :: T.Text             -- Emit the symbol as a byte string
          -> EDASPseudo
  DExp    :: EDASExpr           -- Emit the 16-bit value of the expression
          -> EDASPseudo
  AsmDate :: EDASPseudo         -- Emit the current date as "MM/DD/YY" byte sequence
  AsmTime :: EDASPseudo         -- Emit the current time as "HH:MM:SS" byte sequence
  DefL    :: EDASExpr           -- Define label, i.e., something that can be reassigned, as opposed to equates and statement labels
          -> EDASPseudo
  EndAsm  :: SourcePos          -- End of Assembly source, with optional entry point
          -> Maybe EDASExpr
          -> EDASPseudo
  Entry   :: SourcePos          -- Explicit start address/entry point
          -> EDASExpr
          -> EDASPseudo
  deriving (Show)

-- | 'DefB' elements
data DBValue where
  DBStr  :: T.Text
         -> DBValue
  DBExpr :: EDASExpr
         -> DBValue
  deriving (Show)

-- | 'DefW' elements
data DWValue where
  DWChars :: Char
          -> Char
          -> DWValue
  DWExpr  :: EDASExpr
          -> DWValue
  deriving (Show)

-- | EDAS expression data constructors.
data EDASExpr where
  Const     :: SourcePos        -- 16-bit integer constant, truncated to 8 bits when needed
            -> Int16
            -> EDASExpr
  Var       :: SourcePos        -- Variable/symbolic label
            -> T.Text
            -> EDASExpr
  CurrentPC :: EDASExpr         -- Current program counter
  AsmChar   :: Char             -- Single character constant (GHC stores as 16 bits, but always truncated to 8 bits)
            -> EDASExpr
  Add       :: EDASExpr         -- 16-bit addition
            -> EDASExpr
            -> EDASExpr
  Sub       :: EDASExpr         -- 16-bit subtraction
            -> EDASExpr
            -> EDASExpr
  Mul       :: EDASExpr         -- 16-bit multiplication (overflow is ignored)
            -> EDASExpr
            -> EDASExpr
  Div       :: EDASExpr         -- 16-bit value,  8-bit divisor division
            -> EDASExpr
            -> EDASExpr
  Mod       :: EDASExpr         -- Modulus of division's value
            -> EDASExpr
            -> EDASExpr
  Shift     :: EDASExpr         -- Shift: negative -> shift right, positive -> shift left
            -> EDASExpr
            -> EDASExpr
  LogAnd    :: EDASExpr         -- Bitwise AND
            -> EDASExpr
            -> EDASExpr
  LogOr     :: EDASExpr         -- Bitwise OR
            -> EDASExpr
            -> EDASExpr
  LogXor    :: EDASExpr         -- Bitwise XOR
            -> EDASExpr
            -> EDASExpr
  LogNE     :: EDASExpr         -- Not equal (0/-1)
            -> EDASExpr
            -> EDASExpr
  LogEQ     :: EDASExpr         -- Equal (0/-1)
            -> EDASExpr
            -> EDASExpr
  LogGE     :: EDASExpr         -- Greater than or equal (0/-1)
            -> EDASExpr
            -> EDASExpr
  LogGT     :: EDASExpr         -- Greater than (0/-1)
            -> EDASExpr
            -> EDASExpr
  LogLE     :: EDASExpr         -- Less than or equal (0/-1)
            -> EDASExpr
            -> EDASExpr
  LogLT     :: EDASExpr         -- Less than (0/-1)
            -> EDASExpr
            -> EDASExpr
  ShiftL    :: EDASExpr         -- Shift left
            -> EDASExpr
            -> EDASExpr
  ShiftR    :: EDASExpr         -- Shift right
            -> EDASExpr
            -> EDASExpr
  OnesCpl   :: EDASExpr         -- One's complement
            -> EDASExpr
  HighByte  :: EDASExpr         -- High 8 bits of 16-bit value
            -> EDASExpr
  LowByte   :: EDASExpr         -- Low 8 bits of 16-bit value
            -> EDASExpr
  deriving (Show)

-- | Symbol types that are stored in the symbol table. Equates and statement labels cannot be modified once
-- set, whereas defined labels (via 'DEFL') may take on multiple values. Defined labels are stored as a list,
-- with the most recent value at the head.

data SymbolType where
  SymEquate :: Word16                       -- Equate
            -> SymbolType
  StmtLabel :: Word16                       -- Statement label
            -> SymbolType
  DefLabel  :: [Word16]
            -> SymbolType
  deriving (Show)

-- | Assembler evaluation context, when evaluating expressions.
data AsmEvalCtx where
  AsmEvalCtx ::
    { _symbolTab    :: Map T.Text SymbolType  -- The symbol table
    , _asmPC        :: Z80addr                -- Current assembler program counter
    , _dateTime     :: ZonedTime              -- Time at which the assembler pass started
    , _startAddr    :: Maybe Z80addr          -- Start address from 'END' or 'ENTRY' pseudo-operations
    , _endOfAsm     :: Bool                   -- 'END' pseudo-instruction encountered?
    , _warnings     :: [T.Text]               -- Accumulated warning messages
    } -> AsmEvalCtx
    deriving (Show)

makeLenses ''AsmEvalCtx

-- | Make an initialized (empty) assembler context in the 'IO' context
--
-- N.B.: The result is embedded in the 'IO' monad due to the need to call 'Data.Time.getZonedTime'. Yuck.
mkAsmEvalCtx :: IO AsmEvalCtx
mkAsmEvalCtx = getZonedTime
               >>= (\currentTime -> return ( AsmEvalCtx { _symbolTab = Map.empty
                                                        , _asmPC     = 0
                                                        , _dateTime  = currentTime
                                                        , _startAddr = Nothing
                                                        , _endOfAsm  = False
                                                        , _warnings  = []
                                                        }
                                           )
                   )

-- | Find a symbol or label in the 'AsmEvalCtx' by case-insensitive key, returning its value
findAsmSymbol :: AsmEvalCtx                   -- ^ Current assembler context
              -> T.Text                       -- ^ Symbol name
              -> Maybe Word16                 -- ^ Nothing, if not found, otherwise the 16-bit value
findAsmSymbol ctx sym =
  let getValue Nothing                   = Nothing
      getValue (Just (SymEquate val))    = Just val
      getValue (Just (StmtLabel val))    = Just val
      getValue (Just (DefLabel []))      = Nothing
      getValue (Just (DefLabel (val:_))) = Just val
  in  getValue (ctx ^. symbolTab & Map.lookup (T.toLower sym))

-- | Insert a new symbol into the context's equate table
insertEquate :: AsmEvalCtx
             -> T.Text
             -> Word16
             -> AsmEvalCtx
insertEquate ctx sym symval = symbolTab %~ (Map.insert (T.toLower sym) (SymEquate symval)) $ ctx

-- | Insert a new statement label into the context's statement/symbol label table
insertSymLabel :: AsmEvalCtx
               -> T.Text
               -> Word16
               -> AsmEvalCtx
insertSymLabel ctx lab labval = symbolTab %~ (Map.insert (T.toLower lab) (StmtLabel labval)) $ ctx

-- | Insert a new 'DefL' label into the context's defined label table
insertDefLabel :: AsmEvalCtx
               -> T.Text
               -> Word16
               -> AsmEvalCtx
insertDefLabel ctx lab labval = symbolTab .~ updatedSymtab $ ctx
  where
    (_ignored, updatedSymtab) = ctx ^. symbolTab & Map.insertLookupWithKey updateDef (T.toLower lab) (DefLabel [labval])
    updateDef _k (DefLabel newval) (DefLabel existing) = DefLabel (newval ++ existing)
    updateDef _k badNewVal         badOldVal           =
      error ("insertDefLabel assertion: new = " ++ (show badNewVal) ++ ", old = " ++ (show badOldVal))

-- | Determine if a symbol is present as an equate symbol
existsEquate :: AsmEvalCtx
             -> T.Text
             -> Bool
existsEquate ctx sym = case ctx ^. symbolTab & (Map.lookup (T.toLower sym)) of
                         (Just (SymEquate _)) -> True
                         _otherwise           -> False

-- | Determine if a symbol is present as an equate symbol
existsSymLabel :: AsmEvalCtx
               -> T.Text
               -> Bool
existsSymLabel ctx sym = case ctx ^. symbolTab  & (Map.lookup (T.toLower sym)) of
                           (Just (StmtLabel _)) -> True
                           _otherwise           -> False

-- | Determine if a symbol is present as an equate symbol
existsDefLabel :: AsmEvalCtx
               -> T.Text
               -> Bool
existsDefLabel ctx sym = case ctx ^. symbolTab  & (Map.lookup (T.toLower sym)) of
                           (Just (DefLabel _)) -> True
                           _otherwise          -> False

-- | Determine if a symbol is present as either an equate or statement label
existsAsmSymbol :: AsmEvalCtx
                -> T.Text
                -> Bool
existsAsmSymbol ctx sym = isJust (ctx ^. symbolTab  & (Map.lookup (T.toLower sym)))

-- | Data type constructors for EDAS data elements
data AsmStmt where
  -- Basic parsed assembler statement
  AsmStmt :: { _symLabel :: Maybe EDASLabel
             , _asmOp    :: Maybe AsmOp
             , _comment  :: Maybe Comment
             , _stmtAddr :: Word16                      -- Statement address, i.e., current program counter
             , _bytes    :: Vector Z80word              -- The bytes corresponding to this statement
             } -> AsmStmt
  -- TODO: something here for macro expansion
  deriving (Show)

-- Emit TH lens hair:
makeLenses ''AsmStmt
