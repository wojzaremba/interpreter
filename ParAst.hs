{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module ParAst where
import AbsAst
import LexAst
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.6

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Ident) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Ident)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Double) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Double)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Integer) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Integer)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (String) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (String)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Type) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Type)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([Ident]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([Ident])
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([Func]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([Func])
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([Instr]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([Instr])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([Arg]) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([Arg])
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Arg) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Arg)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Exp]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Exp])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Prog) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Prog)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Func) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Func)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Instr) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Instr)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Exp) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Exp)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Exp) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Exp)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Exp) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Exp)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Exp) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Exp)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Exp) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Exp)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Exp) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Exp)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Exp) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Exp)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Exp) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Exp)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x52\x00\x14\x01\x00\x00\x14\x01\x00\x00\x0c\x01\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x01\xff\x00\x1d\x01\x1a\x01\x1b\x01\xe1\x00\x20\x00\x52\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\xe1\x00\x00\x00\xe4\x00\xa2\x00\xa0\x00\x72\x00\xf9\xff\x64\x00\x6e\x00\x00\x00\x56\x00\x20\x00\x6a\x00\x6a\x00\x6a\x00\x00\x00\x94\x00\x42\x00\x00\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x8c\x00\x00\x00\x6a\x00\x86\x00\x00\x00\x00\x00\x00\x00\x84\x00\xfe\xff\x83\x00\x82\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x6a\x00\x00\x00\x0b\x00\x7d\x00\x6a\x00\x6a\x00\x00\x00\x6f\x00\x7e\x00\x00\x00\x35\x00\x6a\x00\x65\x00\x53\x00\xf9\xff\xf9\xff\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x6e\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x67\x00\x00\x00\x4e\x00\x20\x00\x20\x00\x00\x00\x43\x00\x23\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x20\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x24\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x02\x00\xfd\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x01\xa5\x00\xc7\x01\xc3\x01\xbf\x01\x00\x00\x00\x00\x7b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x01\x00\x00\x00\x00\x5d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x00\x00\x00\x00\x00\xbb\x01\xa5\x01\x2e\x01\x4d\x00\x46\x00\xa1\x01\x9d\x01\xf0\x00\xda\x00\xc4\x00\xae\x00\x06\x01\x99\x01\x00\x00\x00\x00\x00\x00\x29\x01\x47\x01\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x3f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe7\x00\xd1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf3\xff\x00\x00\xfe\xff\x00\x00\xe6\xff\x00\x00\xf2\xff\xf9\xff\xf8\xff\xfa\xff\xf7\xff\xf1\xff\x00\x00\xed\xff\x00\x00\x00\x00\xec\xff\xed\xff\x00\x00\x00\x00\xea\xff\xc2\xff\xc0\xff\xbf\xff\xbc\xff\xf6\xff\xe5\xff\x00\x00\xda\xff\xd8\xff\xd6\xff\xd3\xff\xce\xff\xcb\xff\xc7\xff\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\xbd\xff\x00\x00\x00\x00\xbe\xff\x00\x00\xfd\xff\xfc\xff\xfb\xff\xeb\xff\x00\x00\x00\x00\xe0\xff\x00\x00\xc2\xff\xc6\xff\xc4\xff\xc5\xff\x00\x00\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\xf5\xff\x00\x00\xe9\xff\x00\x00\xdb\xff\x00\x00\xe8\xff\xe2\xff\xf6\xff\x00\x00\xd9\xff\xd7\xff\xd4\xff\xd5\xff\xcf\xff\xd0\xff\xd1\xff\xd2\xff\xcc\xff\xcd\xff\xc8\xff\xc9\xff\xca\xff\x00\x00\xc3\xff\xee\xff\xe4\xff\x00\x00\xe1\xff\x00\x00\x00\x00\x00\x00\xbb\xff\x00\x00\xf5\xff\xf4\xff\xe9\xff\xc1\xff\xe7\xff\xe3\xff\xde\xff\xdc\xff\x00\x00\xdd\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x00\x00\x05\x00\x06\x00\x00\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x08\x00\x09\x00\x01\x00\x08\x00\x09\x00\x11\x00\x12\x00\x04\x00\x07\x00\x03\x00\x16\x00\x17\x00\x18\x00\x07\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x03\x00\x01\x00\x05\x00\x17\x00\x18\x00\x04\x00\x00\x00\x06\x00\x1c\x00\x11\x00\x12\x00\x1f\x00\x0b\x00\x0c\x00\x11\x00\x12\x00\x19\x00\x00\x00\x26\x00\x16\x00\x17\x00\x18\x00\x05\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\x08\x00\x00\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x04\x00\x11\x00\x12\x00\x04\x00\x21\x00\x06\x00\x16\x00\x03\x00\x14\x00\x15\x00\x1a\x00\x0c\x00\x0b\x00\x0c\x00\x1e\x00\x14\x00\x15\x00\x21\x00\x22\x00\x23\x00\x24\x00\x11\x00\x12\x00\x17\x00\x18\x00\x04\x00\x16\x00\x03\x00\x1c\x00\x0a\x00\x1a\x00\x1f\x00\x1c\x00\x04\x00\x1e\x00\x11\x00\x12\x00\x21\x00\x22\x00\x23\x00\x24\x00\x11\x00\x12\x00\x0b\x00\x0c\x00\x01\x00\x16\x00\x13\x00\x14\x00\x15\x00\x1a\x00\x08\x00\x04\x00\x04\x00\x1e\x00\x03\x00\x06\x00\x21\x00\x22\x00\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x08\x00\x03\x00\x07\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0a\x00\x09\x00\x07\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x08\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x21\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x01\x00\x0a\x00\x04\x00\x02\x00\x21\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\x0a\x00\xff\xff\x21\x00\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x15\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x13\x00\x14\x00\x15\x00\xff\xff\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x24\x00\x0e\x00\x25\x00\xef\xff\x0e\x00\x43\x00\x44\x00\x45\x00\x46\x00\x30\x00\x10\x00\x54\x00\x0f\x00\x10\x00\x26\x00\x27\x00\x14\x00\x55\x00\x4e\x00\x28\x00\x08\x00\x09\x00\x4f\x00\x29\x00\x2a\x00\x0a\x00\x2b\x00\x2c\x00\x0b\x00\x2d\x00\x03\x00\x2e\x00\x2f\x00\x30\x00\x24\x00\x54\x00\x25\x00\x08\x00\x09\x00\x03\x00\x0c\x00\x04\x00\x0a\x00\x41\x00\x42\x00\x0b\x00\x05\x00\x06\x00\x26\x00\x27\x00\x76\x00\x6d\x00\xf2\xff\x28\x00\x08\x00\x09\x00\x6e\x00\x29\x00\x2a\x00\x0a\x00\x2b\x00\x2c\x00\x0b\x00\x2d\x00\x03\x00\x2e\x00\x2f\x00\x30\x00\x24\x00\x35\x00\x16\x00\x17\x00\x18\x00\x34\x00\x73\x00\x4b\x00\x35\x00\x16\x00\x17\x00\x18\x00\x4c\x00\x6a\x00\x26\x00\x27\x00\x03\x00\x03\x00\x0b\x00\x28\x00\x24\x00\x5d\x00\x22\x00\x29\x00\x06\x00\x47\x00\x48\x00\x2c\x00\x5e\x00\x22\x00\x03\x00\x2e\x00\x2f\x00\x30\x00\x26\x00\x27\x00\x08\x00\x09\x00\x6b\x00\x28\x00\x24\x00\x0a\x00\x49\x00\x29\x00\x0b\x00\x3d\x00\x71\x00\x2c\x00\x41\x00\x42\x00\x03\x00\x2e\x00\x2f\x00\x30\x00\x26\x00\x27\x00\x47\x00\x48\x00\x70\x00\x28\x00\x3e\x00\x3f\x00\x40\x00\x29\x00\x53\x00\x63\x00\x64\x00\x2c\x00\x4e\x00\x66\x00\x03\x00\x2e\x00\x2f\x00\x30\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x68\x00\x32\x00\x64\x00\x35\x00\x35\x00\x16\x00\x17\x00\x18\x00\x3a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x49\x00\x4a\x00\x39\x00\x6b\x00\x35\x00\x16\x00\x17\x00\x18\x00\x3a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x57\x00\x20\x00\x21\x00\x22\x00\x35\x00\x16\x00\x17\x00\x18\x00\x76\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x58\x00\x20\x00\x21\x00\x22\x00\x35\x00\x16\x00\x17\x00\x18\x00\x73\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x4b\x00\x59\x00\x21\x00\x22\x00\x35\x00\x16\x00\x17\x00\x18\x00\x74\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x03\x00\x5a\x00\x21\x00\x22\x00\x35\x00\x16\x00\x17\x00\x18\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x56\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x12\x00\x71\x00\x13\x00\x14\x00\x03\x00\x51\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x0e\x00\x35\x00\x16\x00\x17\x00\x18\x00\xff\xff\x50\x00\x00\x00\x03\x00\x00\x00\x51\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x5f\x00\x00\x00\x00\x00\x00\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x6c\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x4f\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x66\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x68\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x32\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x3b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x35\x00\x16\x00\x17\x00\x18\x00\x35\x00\x16\x00\x17\x00\x18\x00\x35\x00\x16\x00\x17\x00\x18\x00\x35\x00\x16\x00\x17\x00\x18\x00\x55\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x5b\x00\x21\x00\x22\x00\x00\x00\x5c\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x60\x00\x35\x00\x16\x00\x17\x00\x18\x00\x35\x00\x16\x00\x17\x00\x18\x00\x35\x00\x16\x00\x17\x00\x18\x00\x35\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x61\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 68) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68)
	]

happy_n_terms = 39 :: Int
happy_n_nonterms = 22 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn4
		 (Ident happy_var_1
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn5
		 ((read happy_var_1) :: Double
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn6
		 ((read happy_var_1) :: Integer
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn7
		 (happy_var_1
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  happyIn8
		 (Int
	)

happyReduce_6 = happySpecReduce_1  4# happyReduction_6
happyReduction_6 happy_x_1
	 =  happyIn8
		 (Bool
	)

happyReduce_7 = happySpecReduce_1  4# happyReduction_7
happyReduction_7 happy_x_1
	 =  happyIn8
		 (Double
	)

happyReduce_8 = happySpecReduce_1  4# happyReduction_8
happyReduction_8 happy_x_1
	 =  happyIn8
		 (Void
	)

happyReduce_9 = happySpecReduce_0  5# happyReduction_9
happyReduction_9  =  happyIn9
		 ([]
	)

happyReduce_10 = happySpecReduce_1  5# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((:[]) happy_var_1
	)}

happyReduce_11 = happySpecReduce_3  5# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_12 = happySpecReduce_0  6# happyReduction_12
happyReduction_12  =  happyIn10
		 ([]
	)

happyReduce_13 = happySpecReduce_1  6# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((:[]) happy_var_1
	)}

happyReduce_14 = happySpecReduce_2  6# happyReduction_14
happyReduction_14 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_15 = happySpecReduce_0  7# happyReduction_15
happyReduction_15  =  happyIn11
		 ([]
	)

happyReduce_16 = happySpecReduce_1  7# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ((:[]) happy_var_1
	)}

happyReduce_17 = happySpecReduce_2  7# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_18 = happySpecReduce_0  8# happyReduction_18
happyReduction_18  =  happyIn12
		 ([]
	)

happyReduce_19 = happySpecReduce_1  8# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ((:[]) happy_var_1
	)}

happyReduce_20 = happySpecReduce_3  8# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_21 = happySpecReduce_3  9# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (ArgDecl happy_var_1 happy_var_3
	)}}

happyReduce_22 = happySpecReduce_0  10# happyReduction_22
happyReduction_22  =  happyIn14
		 ([]
	)

happyReduce_23 = happySpecReduce_1  10# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ((:[]) happy_var_1
	)}

happyReduce_24 = happySpecReduce_3  10# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_25 = happySpecReduce_1  11# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (Entry happy_var_1
	)}

happyReduce_26 = happyReduce 6# 12# happyReduction_26
happyReduction_26 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOut17 happy_x_6 of { happy_var_6 -> 
	happyIn16
		 (FuncDecl happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_27 = happySpecReduce_3  13# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (IBlock happy_var_2
	)}

happyReduce_28 = happyReduce 5# 13# happyReduction_28
happyReduction_28 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 (IDeclSt happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_29 = happySpecReduce_3  13# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (IDecl happy_var_1 happy_var_2
	)}}

happyReduce_30 = happySpecReduce_3  13# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (IRet happy_var_2
	)}

happyReduce_31 = happySpecReduce_2  13# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  happyIn17
		 (IRetEmpty
	)

happyReduce_32 = happySpecReduce_2  13# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (IExp happy_var_1
	)}

happyReduce_33 = happyReduce 5# 13# happyReduction_33
happyReduction_33 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut17 happy_x_5 of { happy_var_5 -> 
	happyIn17
		 (IIf happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_34 = happyReduce 7# 13# happyReduction_34
happyReduction_34 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut17 happy_x_5 of { happy_var_5 -> 
	case happyOut17 happy_x_7 of { happy_var_7 -> 
	happyIn17
		 (IIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_35 = happyReduce 5# 13# happyReduction_35
happyReduction_35 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut17 happy_x_5 of { happy_var_5 -> 
	happyIn17
		 (IWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_36 = happySpecReduce_3  14# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (EVarSet happy_var_1 happy_var_3
	)}}

happyReduce_37 = happySpecReduce_1  14# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  15# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (EOr happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_1  15# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (happy_var_1
	)}

happyReduce_40 = happySpecReduce_3  16# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (EAnd happy_var_1 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_1  16# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (happy_var_1
	)}

happyReduce_42 = happySpecReduce_3  17# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (EEq happy_var_1 happy_var_3
	)}}

happyReduce_43 = happySpecReduce_3  17# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (ENEq happy_var_1 happy_var_3
	)}}

happyReduce_44 = happySpecReduce_1  17# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_45 = happySpecReduce_3  18# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (ESm happy_var_1 happy_var_3
	)}}

happyReduce_46 = happySpecReduce_3  18# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (EGr happy_var_1 happy_var_3
	)}}

happyReduce_47 = happySpecReduce_3  18# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (EESm happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_3  18# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (EEGr happy_var_1 happy_var_3
	)}}

happyReduce_49 = happySpecReduce_1  18# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_50 = happySpecReduce_3  19# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (EAdd happy_var_1 happy_var_3
	)}}

happyReduce_51 = happySpecReduce_3  19# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (ESub happy_var_1 happy_var_3
	)}}

happyReduce_52 = happySpecReduce_1  19# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_53 = happySpecReduce_3  20# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (EMul happy_var_1 happy_var_3
	)}}

happyReduce_54 = happySpecReduce_3  20# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (EDiv happy_var_1 happy_var_3
	)}}

happyReduce_55 = happySpecReduce_3  20# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (EMod happy_var_1 happy_var_3
	)}}

happyReduce_56 = happySpecReduce_1  20# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_57 = happySpecReduce_2  21# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (ENot happy_var_2
	)}

happyReduce_58 = happySpecReduce_2  21# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (EPlus happy_var_2
	)}

happyReduce_59 = happySpecReduce_2  21# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (EMinus happy_var_2
	)}

happyReduce_60 = happySpecReduce_3  21# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (happy_var_2
	)}

happyReduce_61 = happySpecReduce_1  21# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (EVar happy_var_1
	)}

happyReduce_62 = happyReduce 4# 21# happyReduction_62
happyReduction_62 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (ECall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_63 = happySpecReduce_1  21# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (EDouble happy_var_1
	)}

happyReduce_64 = happySpecReduce_1  21# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (EInt happy_var_1
	)}

happyReduce_65 = happySpecReduce_1  21# happyReduction_65
happyReduction_65 happy_x_1
	 =  happyIn25
		 (ETrue
	)

happyReduce_66 = happySpecReduce_1  21# happyReduction_66
happyReduction_66 happy_x_1
	 =  happyIn25
		 (EFalse
	)

happyReduce_67 = happySpecReduce_1  21# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (EStr happy_var_1
	)}

happyReduce_68 = happyReduce 4# 21# happyReduction_68
happyReduction_68 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (EToInt happy_var_4
	) `HappyStk` happyRest}

happyNewToken action sts stk [] =
	happyDoAction 38# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS ",") -> cont 1#;
	PT _ (TS ":") -> cont 2#;
	PT _ (TS "(") -> cont 3#;
	PT _ (TS ")") -> cont 4#;
	PT _ (TS "{") -> cont 5#;
	PT _ (TS "}") -> cont 6#;
	PT _ (TS "=") -> cont 7#;
	PT _ (TS ";") -> cont 8#;
	PT _ (TS "||") -> cont 9#;
	PT _ (TS "&&") -> cont 10#;
	PT _ (TS "==") -> cont 11#;
	PT _ (TS "!=") -> cont 12#;
	PT _ (TS "<") -> cont 13#;
	PT _ (TS ">") -> cont 14#;
	PT _ (TS "<=") -> cont 15#;
	PT _ (TS ">=") -> cont 16#;
	PT _ (TS "+") -> cont 17#;
	PT _ (TS "-") -> cont 18#;
	PT _ (TS "*") -> cont 19#;
	PT _ (TS "/") -> cont 20#;
	PT _ (TS "%") -> cont 21#;
	PT _ (TS "!") -> cont 22#;
	PT _ (TS "bool") -> cont 23#;
	PT _ (TS "double") -> cont 24#;
	PT _ (TS "else") -> cont 25#;
	PT _ (TS "false") -> cont 26#;
	PT _ (TS "if") -> cont 27#;
	PT _ (TS "int") -> cont 28#;
	PT _ (TS "return") -> cont 29#;
	PT _ (TS "true") -> cont 30#;
	PT _ (TS "void") -> cont 31#;
	PT _ (TS "while") -> cont 32#;
	PT _ (TV happy_dollar_dollar) -> cont 33#;
	PT _ (TD happy_dollar_dollar) -> cont 34#;
	PT _ (TI happy_dollar_dollar) -> cont 35#;
	PT _ (TL happy_dollar_dollar) -> cont 36#;
	_ -> cont 37#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut15 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ if null ts then [] else (" before " ++ unwords (map prToken (take 4 ts)))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.