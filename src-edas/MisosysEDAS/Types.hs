{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Types shared across the Misosys EDAS assembler
module Z80.MisosysEDAS.Types where

-- import Debug.Trace

import Control.Lens
import Data.Word
import Data.Int
import Data.Time
import Data.Maybe
import Text.Parsec
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
    , _lorgPC       :: Z80addr                -- Load origin program counter
    , _dateTime     :: ZonedTime              -- Time at which the assembler pass started
    , _startAddr    :: Maybe Z80addr          -- Start address from 'END' or 'ENTRY' pseudo-operations
    , _endOfAsm     :: Bool                   -- 'END' pseudo-instruction encountered?
    , _warnings     :: [T.Text]               -- Accumulated warning messages
    } -> AsmEvalCtx
    deriving (Show)

makeLenses ''AsmEvalCtx

-- | Shorthand for catching issues and problems detected while using 'AsmEvalCtx', e.g., duplicate symbols and
-- labels.
type IntermediateCtx = Either T.Text AsmEvalCtx

-- | Make an initialized (empty) assembler context in the 'IO' Monad.
--
-- N.B.: The result is embedded in the 'IO' monad due to the need to call 'Data.Time.getZonedTime'. Yuck.
mkAsmEvalCtx :: IO AsmEvalCtx
mkAsmEvalCtx = getZonedTime
               >>= (\currentTime -> return ( AsmEvalCtx { _symbolTab = Map.empty
                                                        , _asmPC     = 0
                                                        , _lorgPC    = 0
                                                        , _dateTime  = currentTime
                                                        , _startAddr = Nothing
                                                        , _endOfAsm  = False
                                                        , _warnings  = []
                                                        }
                                           )
                   )

-- | Assembler operation: a Z80 instruction or EDAS pseudo operation
data AsmOp where
  NoAsmOp  :: AsmOp                             -- Monoid 'mempty' element. Note that this eliminates the need to
                                                -- wrap 'AsmOp' in 'Data.Maybe' when there is no 'Insn' or 'Pseudo'.
  InsnEval :: (IntermediateCtx -> Either T.Text Z80instruction)
                                                -- Left: Warning or error message
                                                -- Right: An instruction constructor, may involve an expression
                                                -- and an evaluation context. 
          -> AsmOp
  Insn    :: Z80instruction
          -> AsmOp
  Pseudo  :: EDASPseudo                         -- A pseudo operation, e.g., equate, db, ds, etc.
          -> AsmOp
  AsmSeq  :: [AsmOp]                            -- An assembly sequence, mainly used to implement 'Monoid' properties
                                                -- Note: This is not used or referenced, other than in the 'Monoid'
                                                -- instance.
          -> AsmOp
  deriving (Show)

instance Show (IntermediateCtx -> Either T.Text Z80instruction) where
  show _ctxInsn = "<asm insn eval>"

-- | Generating lenses for 'AsmStmt' requires 'AsmOp' to have 'Data.Monoid.Monoid' properties. However,
-- no of these instance functions get invoked at runtime (at least right now...)
instance Monoid AsmOp where
  -- Easy case. This also eliminates the need to wrap 'AsmOp' within 'Data.Maybe' when there is no value.
  mempty  = NoAsmOp

  -- Easy cases.
  NoAsmOp       `mappend` x             = x
  x             `mappend` NoAsmOp       = x

  (AsmSeq aseq) `mappend` (AsmSeq bseq) = AsmSeq (aseq ++ bseq)
  (AsmSeq aseq) `mappend` x             = AsmSeq (aseq ++ [x])
  x             `mappend` (AsmSeq aseq) = AsmSeq (x:aseq)

  -- Create 'AsmSeq' sequences for all other 'mappend' cases:
  x             `mappend` y             = AsmSeq [x, y]

-- | EDAS\' pseudo operations
data EDASPseudo where
  Equate    :: EDASExpr           -- Symbol constant equate
            -> EDASPseudo
  Origin    :: EDASExpr           -- Assembly origin (start) address
            -> EDASPseudo
  DefB      :: [DBValue]          -- Define bytes
            -> EDASPseudo
  DefC      :: EDASExpr           -- Define constant byte fill (repeat, const)
            -> EDASExpr
            -> EDASPseudo
  DefS      :: EDASExpr           -- Define space ('DefC' with 0 as the constant)
            -> EDASPseudo
  DefW      :: [DWValue]          -- Define little endian words
            -> EDASPseudo
  DSym      :: T.Text             -- Emit the symbol as a byte string
            -> EDASPseudo
  DExp      :: EDASExpr           -- Emit the 16-bit value of the expression
            -> EDASPseudo
  AsmDate   :: EDASPseudo         -- Emit the current date as "MM/DD/YY" byte sequence
  AsmTime   :: EDASPseudo         -- Emit the current time as "HH:MM:SS" byte sequence
  DefL      :: EDASExpr           -- Define label, i.e., something that can be reassigned, as opposed to equates and statement labels
            -> EDASPseudo
  EndAsm    :: SourcePos          -- End of Assembly source, with optional entry point
            -> Maybe EDASExpr
            -> EDASPseudo
  Entry     :: SourcePos          -- Explicit start address/entry point
            -> EDASExpr
            -> EDASPseudo
  LoadOrg   :: EDASExpr           -- Sets the executable's load origin
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
  EmptyExpr :: EDASExpr         -- The null expression
  Const16   :: SourcePos        -- 16-bit integer constant, truncated to 8 bits when needed
            -> Int16            -- Constant's value
            -> Char             -- Constant's original base
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

-- | Generating lenses for 'AsmStmt' requires 'EDASExpr' to have 'Data.Monoid.Monoid' properties. However,
-- no of these instance functions get invoked at runtime (at least right now...)
instance Monoid EDASExpr where
  -- Easy case. This also eliminates the need to wrap 'EDASExpr' within 'Data.Maybe' when there is no value.
  mempty  = EmptyExpr

  -- 'mappend' is undefined
  _x `mappend` _y = error "EDASExpr mappend"

-- | Find a symbol or label in the 'AsmEvalCtx' by case-insensitive key, returning its value
findAsmSymbol :: T.Text                       -- ^ Symbol name
              -> AsmEvalCtx                   -- ^ Current assembler context
              -> Maybe Word16                 -- ^ Nothing, if not found, otherwise the 16-bit value
findAsmSymbol sym ctx =
  let getValue Nothing                   = Nothing
      getValue (Just (SymEquate val))    = Just val
      getValue (Just (StmtLabel val))    = Just val
      getValue (Just (DefLabel []))      = Nothing
      getValue (Just (DefLabel (val:_))) = Just val
  in  getValue (ctx ^. symbolTab & Map.lookup (T.toLower sym))

-- | Insert a new symbol into the context's equate table
insertEquate :: T.Text
             -> Word16
             -> AsmEvalCtx
             -> AsmEvalCtx
insertEquate sym symval ctx = symbolTab %~ (Map.insert (T.toLower sym) (SymEquate symval)) $ ctx

-- | Insert a new statement label into the context's statement/symbol label table
insertSymLabel :: T.Text
               -> Word16
               -> AsmEvalCtx
               -> AsmEvalCtx
insertSymLabel lab labval ctx = symbolTab %~ (Map.insert (T.toLower lab) (StmtLabel labval)) $ ctx

-- | Insert a new 'DefL' label into the context's defined label table
insertDefLabel :: T.Text
               -> Word16
               -> AsmEvalCtx
               -> AsmEvalCtx
insertDefLabel lab labval ctx = symbolTab .~ updatedSymtab $ ctx
  where
    (_ignored, updatedSymtab) = ctx ^. symbolTab & Map.insertLookupWithKey updateDef (T.toLower lab) (DefLabel [labval])
    updateDef _k (DefLabel newval) (DefLabel existing) = DefLabel (newval ++ existing)
    updateDef _k badNewVal         badOldVal           =
      error ("insertDefLabel assertion: new = " ++ (show badNewVal) ++ ", old = " ++ (show badOldVal))

-- | Determine if a symbol is present as an equate symbol
existsEquate :: T.Text
             -> AsmEvalCtx
             -> Bool
existsEquate sym ctx = case ctx ^. symbolTab & (Map.lookup (T.toLower sym)) of
                         (Just (SymEquate _)) -> True
                         _otherwise           -> False

-- | Determine if a symbol is present as an equate symbol
existsSymLabel :: T.Text
               -> AsmEvalCtx
               -> Bool
existsSymLabel sym ctx = case ctx ^. symbolTab  & (Map.lookup (T.toLower sym)) of
                           (Just (StmtLabel _)) -> True
                           _otherwise           -> False

-- | Determine if a symbol is present as an equate symbol
existsDefLabel :: T.Text
               -> AsmEvalCtx
               -> Bool
existsDefLabel sym ctx = case ctx ^. symbolTab  & (Map.lookup (T.toLower sym)) of
                           (Just (DefLabel _)) -> True
                           _otherwise          -> False

-- | Determine if a symbol is present as either an equate or statement label
existsAsmSymbol :: T.Text
                -> AsmEvalCtx
                -> Bool
existsAsmSymbol sym ctx = isJust (ctx ^. symbolTab  & (Map.lookup (T.toLower sym)))

-- | Data type constructors for EDAS assembler statements.
data AsmStmt where
  -- Basic parsed assembler statement
  AsmStmt     :: { _srcPos     :: SourcePos
                 , _symLabel   :: Maybe EDASLabel
                 , _asmOp      :: AsmOp
                 , _comment    :: Maybe Comment
                 , _stmtAddr   :: Word16            -- Statement address, i.e., current program counter
                 , _bytes      :: Vector Z80word    -- The bytes corresponding to this statement
                 } -> AsmStmt
  -- Conditional assembly depending on pass number (1 = symbol evaluation, 2 = listing, 3 = object code generation)
  CondPass    :: { _srcPos       :: SourcePos
                 , _symLabel     :: Maybe EDASLabel
                 , _passNo       :: Int
                 , _comment      :: Maybe Comment
                 , _stmtsTrue    :: [AsmStmt]
                 , _elseLabel    :: Maybe EDASLabel
                 , _elseComment  :: Maybe Comment
                 , _stmtsFalse   :: [AsmStmt]
                 , _endifLabel   :: Maybe EDASLabel
                 , _endifComment :: Maybe Comment
                 } -> AsmStmt
  -- Conditional assembly depending on EQ, LT, GT or NE comparison
  CondCmp     :: { _srcPos       :: SourcePos
                 , _symLabel     :: Maybe EDASLabel
                 , _cmpName      :: T.Text
                 , _leftExp      :: EDASExpr
                 , _rightExp     :: EDASExpr
                 , _condF        :: (IntermediateCtx -> Either T.Text Bool)
                 , _comment      :: Maybe Comment
                 , _stmtsTrue    :: [AsmStmt]
                 , _elseLabel    :: Maybe EDASLabel
                 , _elseComment  :: Maybe Comment
                 , _stmtsFalse   :: [AsmStmt]
                 , _endifLabel   :: Maybe EDASLabel
                 , _endifComment :: Maybe Comment
                 , _condResult   :: Bool
                 } -> AsmStmt
  -- Conditional assembly depending on EQ, LT, GT or NE string comparison
  CondCmpStr :: { _srcPos        :: SourcePos
                 , _symLabel     :: Maybe EDASLabel
                 , _cmpName      :: T.Text
                 , _leftStr      :: T.Text
                 , _rightStr     :: T.Text
                 , _strcmpF      :: (IntermediateCtx -> Either T.Text Bool)
                 , _comment      :: Maybe Comment
                 , _stmtsTrue    :: [AsmStmt]
                 , _elseLabel    :: Maybe EDASLabel
                 , _elseComment  :: Maybe Comment
                 , _stmtsFalse   :: [AsmStmt]
                 , _endifLabel   :: Maybe EDASLabel
                 , _endifComment :: Maybe Comment
                 , _condResult   :: Bool
                 } -> AsmStmt
  -- Conditional assembly depending on the value of an expression
  CondAsmEval :: { _srcPos     :: SourcePos
                 , _symLabel     :: Maybe EDASLabel
                 , _evalExp      :: EDASExpr
                 , _comment      :: Maybe Comment
                 , _stmtsTrue    :: [AsmStmt]
                 , _elseLabel    :: Maybe EDASLabel
                 , _elseComment  :: Maybe Comment
                 , _stmtsFalse   :: [AsmStmt]
                 , _endifLabel   :: Maybe EDASLabel
                 , _endifComment :: Maybe Comment
                 , _condResult   :: Bool
                 } -> AsmStmt
  -- TODO: something here for macro expansion

-- | 'Show' instance for assembler statements
instance Show AsmStmt where
  show (AsmStmt srcPos symLabel asmOp comment stmtAddr bytes) =
    T.unpack (T.concat [ "AsmStmt("
                       , T.intercalate ", " [ sourcePosText srcPos
                                            , textShow symLabel
                                            , textShow asmOp
                                            , textShow comment
                                            , textShow stmtAddr
                                            , textShow bytes
                                            ]
                       , ")"
                       ])

  show (CondPass srcPos symLabel passNo comment stmtsTrue elseLabel elseComment 
                 stmtsFalse endifLabel endifComment ) =
    T.unpack (T.concat [ "CondPass("
                       , T.intercalate ", " [ sourcePosText srcPos
                                            , textShow symLabel
                                            , textShow passNo
                                            , textShow comment
                                            , textShow stmtsTrue
                                            , textShow elseLabel
                                            , textShow elseComment
                                            , textShow stmtsFalse
                                            , textShow endifLabel
                                            , textShow endifComment
                                            ]
                       , ")"
                       ])

  show (CondCmp srcPos symLabel cmpName leftExp rightExp _condF comment stmtsTrue elseLabel elseComment 
                stmtsFalse endifLabel endifComment condResult) =
    T.unpack (T.concat [ "CondCmp("
                       , T.intercalate ", " [ sourcePosText srcPos
                                            , textShow symLabel
                                            , cmpName
                                            , textShow leftExp
                                            , textShow rightExp
                                            , textShow comment
                                            , textShow stmtsTrue
                                            , textShow elseLabel
                                            , textShow elseComment
                                            , textShow stmtsFalse
                                            , textShow endifLabel
                                            , textShow endifComment
                                            , textShow condResult
                                            ]
                       , ")"
                       ])

  -- Conditional assembly depending on EQ, LT, GT or NE string comparison
  show (CondCmpStr srcPos symLabel cmpName leftExp rightExp _strcmpF comment stmtsTrue elseLabel elseComment
                   stmtsFalse endifLabel endifComment condResult) =
    T.unpack (T.concat [ "CondCmpStr("
                       , T.intercalate ", " [ sourcePosText srcPos
                                            , textShow symLabel
                                            , cmpName
                                            , textShow leftExp
                                            , textShow rightExp
                                            , textShow comment
                                            , textShow stmtsTrue
                                            , textShow elseLabel
                                            , textShow elseComment
                                            , textShow stmtsFalse
                                            , textShow endifLabel
                                            , textShow endifComment
                                            , textShow condResult
                                            ]
                       , ")"
                       ])

  -- Conditional assembly depending on the value of an expression
  show (CondAsmEval srcPos symLabel evalExp comment stmtsTrue elseLabel elseComment stmtsFalse endifLabel endifComment
                    condResult) =
    T.unpack (T.concat [ "CondAsmEval("
                       , T.intercalate ", " [ sourcePosText srcPos
                                            , textShow symLabel
                                            , textShow evalExp
                                            , textShow comment
                                            , textShow stmtsTrue
                                            , textShow elseLabel
                                            , textShow elseComment
                                            , textShow stmtsFalse
                                            , textShow endifLabel
                                            , textShow endifComment
                                            , textShow condResult
                                            ]
                       , ")"
                       ])

instance Monoid Word16 where
  mempty = (0 :: Word16)
  mappend = (+)

-- | Shorthand (not exported) converter
textShow :: (Show thingType) =>
            thingType
         -> T.Text
textShow = T.pack . show

-- | Customized source position-to-Text converter
sourcePosText :: SourcePos
              -> T.Text
sourcePosText loc = let srcLine = sourceLine loc
                        srcCol  = sourceColumn loc
                        srcFile = sourceName loc
                    in  T.concat [ "@("
                                 , T.intercalate "/" [ (T.cons '"' (T.snoc (T.pack srcFile) '"'))
                                                     , textShow srcLine
                                                     , textShow srcCol
                                                     ]
                                 , ")"
                                 ]

-- Emit TH lens hair:
makeLenses ''AsmStmt

-- | Shorthand for the parser's type, leaving the "return" type free
type EDASParser retType = ParsecT T.Text AsmStmt Identity retType
