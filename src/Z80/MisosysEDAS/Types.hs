-- | Types shared across the Misosys EDAS assembler
module Z80.MisosysEDAS.Types where

import Control.Lens
import Data.Word
import Data.Int
import Data.Time
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
  Const    :: SourcePos         -- 16-bit integer constant, truncated to 8 bits when needed
           -> Int16
           -> EDASExpr
  Var      :: SourcePos         -- Variable/symbolic label
           -> T.Text
           -> EDASExpr
  AsmChar  :: Char              -- Single character constant (GHC stores as 16 bits, but always truncated to 8 bits)
           -> EDASExpr
  Add      :: EDASExpr          -- 16-bit addition
           -> EDASExpr
           -> EDASExpr
  Sub      :: EDASExpr          -- 16-bit subtraction
           -> EDASExpr
           -> EDASExpr
  Mul      :: EDASExpr          -- 16-bit multiplication (overflow is ignored)
           -> EDASExpr
           -> EDASExpr
  Div      :: EDASExpr          -- 16-bit value,  8-bit divisor division
           -> EDASExpr
           -> EDASExpr
  Mod      :: EDASExpr          -- Modulus of division's value
           -> EDASExpr
           -> EDASExpr
  Shift    :: EDASExpr          -- Shift: negative -> shift right, positive -> shift left
           -> EDASExpr
           -> EDASExpr
  LogAnd   :: EDASExpr          -- Bitwise AND
           -> EDASExpr
           -> EDASExpr
  LogOr    :: EDASExpr          -- Bitwise OR
           -> EDASExpr
           -> EDASExpr
  LogXor   :: EDASExpr          -- Bitwise XOR
           -> EDASExpr
           -> EDASExpr
  LogNE    :: EDASExpr          -- Not equal (0/-1)
           -> EDASExpr
           -> EDASExpr
  LogEQ    :: EDASExpr          -- Equal (0/-1)
           -> EDASExpr
           -> EDASExpr
  LogGE    :: EDASExpr          -- Greater than or equal (0/-1)
           -> EDASExpr
           -> EDASExpr
  LogGT    :: EDASExpr          -- Greater than (0/-1)
           -> EDASExpr
           -> EDASExpr
  LogLE    :: EDASExpr          -- Less than or equal (0/-1)
           -> EDASExpr
           -> EDASExpr
  LogLT    :: EDASExpr          -- Less than (0/-1)
           -> EDASExpr
           -> EDASExpr
  ShiftL   :: EDASExpr          -- Shift left
           -> EDASExpr
           -> EDASExpr
  ShiftR   :: EDASExpr          -- Shift right
           -> EDASExpr
           -> EDASExpr
  OnesCpl  :: EDASExpr          -- One's complement
           -> EDASExpr
  HighByte :: EDASExpr          -- High 8 bits of 16-bit value
           -> EDASExpr
  LowByte  :: EDASExpr          -- Low 8 bits of 16-bit value
           -> EDASExpr
  deriving (Show)

-- | Assembler evaluation context, when evaluating expressions.
data AsmEvalCtx where
  AsmEvalCtx ::
    { _symbolTab :: Map T.Text Word16
    , _asmPC     :: Z80addr
    , _dateTime  :: ZonedTime
    } -> AsmEvalCtx

makeLenses ''AsmEvalCtx

-- | Find a symbol or label in the 'AsmEvalCtx' by case-insensitive key
findAsmSymbol :: AsmEvalCtx
              -> T.Text
              -> Maybe Word16
findAsmSymbol ctx sym = ctx ^. symbolTab & Map.lookup (T.toLower sym)

-- | Insert a new symbol into the context's symbol table
insertAsmSymbol :: AsmEvalCtx
                -> T.Text
                -> Word16
                -> AsmEvalCtx
insertAsmSymbol ctx sym symval = symbolTab %~ (Map.insert (T.toLower sym) symval) $ ctx

-- | Insert a new symbol into the context's symbol table
existsAsmSymbol :: AsmEvalCtx
                -> T.Text
                -> Bool
existsAsmSymbol ctx sym = ctx ^. symbolTab & (Map.member (T.toLower sym))

-- | Data type constructors for EDAS data elements
data AsmStmt where
  -- Basic parsed assembler statement
  AsmStmt :: { _symLabel :: Maybe EDASLabel
             , _asmOp    :: Maybe AsmOp
             , _comment  :: Maybe Comment
             , _stmtAddr :: Word16                      -- Statement address, i.e., current program counter
             , _bytes    :: Vector Z80word              -- The bytes corresponding to this statement
             } -> AsmStmt
  -- TODO: something here for macro definition
  -- TODO: something here for macro expansion
  deriving (Show)

-- Emit TH lens hair:
makeLenses ''AsmStmt
