{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module LLVM.Assembler.Grammar where
import Language.LBNF
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar

bnfc [lbnf|
   -- This is a new pragma. The rest of the grammar is original JL.
   antiquote "[" ":" ":]" ;

   --Define rules for non-keyword tokens. This is currently just a bunch
   --of hacks. They don't cover many valid forms of tokens, and they also
   --generate some invalid forms of tokens. The LLVM parser has custom
   --C++ code to lex these; custom C++ code for emitting them would be
   --convenient, but polygen doesn't support that.
   
   LOCALVALID.  LOCALVALID  ::= "%"  Integer ;
   GLOBALVALID. GLOBALVALID ::= "@"  Integer ;
   INTTYPE.     INTTYPE     ::= "i"  Integer ;
   GLOBALVAR.   GLOBALVAR   ::= "@"  Ident   ;
   
   ATString. ATString    ::= "@"  String ;
   LABELSTR. LABELSTR    ::= Ident ;
   rules    FPVAL       ::= Double | "0x" Integer ;
   
   rules OptNSW ::= "nsw" | ;
   rules OptNUW ::= "nuw"  ;
   rules OptNW  ::= OptNUW OptNSW | OptNSW OptNUW ;
   
   rules ArithmeticOps ::= OptNW "add"     | "fadd" | OptNW "sub" | "fsub" |
                           OptNW "mul"     | "fmul" | "udiv"      | 
                           OptExact "sdiv" | "fdiv" | "urem"      | "srem" | 
                           "frem" ;
   
   rules OptExact ::= "exact"  ;
   rules LogicalOps ::= "shl" | "lshr" | "ashr" | "and"  
                         "or" | "xor"  ;
   
   rules CastOps ::= "trunc"    | "zext"     | "sext"   | "fptrunc" | "fpext"   
                     "bitcast"  | "uitofp"   | "sitofp" | "fptoui"  | "fptosi"  
                     "inttoptr" | "ptrtoint";

   rules IPredicates ::= "eq"  | "ne"  | "slt" | "sgt" |
                         "sle" | "sge" | "ult" | "ugt" |
                         "ule" | "uge" ;
                                   
   rules FPredicates ::= "oeq" | "one" | "olt"  | "ogt"   |  
                         "ole" | "oge" | "ord"  | "uno"   | 
                         "ueq" | "une" | "ult"  | "ugt"   |  
                         "ule" | "uge" | "true" | "false" ;
                                   
   IntType. IntType ::= INTTYPE;
                                     
   rules FPType  ::= "float" | "double" | "ppc_fp128" | "fp128" | "x86_fp80";
   
   rules LocalName ::= "%"  Ident | String | "%"  String ;
                                          
   rules OptLocalName ::= LocalName         ;
   
   rules OptAddrSpace ::= "addrspace"  "("  Integer  ")"     ;                             
   
   rules OptLocalAssign ::= LocalName "="     ;       
   
   rules GlobalName ::= GLOBALVAR | ATString  ;
   
   rules OptGlobalAssign ::= GlobalAssign     ;      
   
   rules GlobalAssign ::= GlobalName "=" ;
   
   rules GVInternalLinkage ::= "internal"             | "weak"           | 
                                "weak_odr"            | "linkonce"       | 
                                "linkonce_odr"        | "appending"      | 
                                "dllexport"           | "common"         | 
                                "private"             | "linker_private" | 
                                "linker_private_weak" ; 
                                               
   rules GVExternalLinkage ::= "dllimport" | "extern_weak" | "external" ;

   rules GVVisibilityStyle ::=  | "default" | "hidden" | "protected" ;

   rules FunctionDeclareLinkage ::=  | "dllimport" | "extern_weak" ;

   rules FunctionDefineLinkage ::=             | "internal" | "linkonce" | 
                                   "linkonce_odr" | "weak"     | "weak_odr" | 
                                   "dllexport"    ;

   rules AliasLinkage ::=  | "weak" | "weak_odr" | "internal" ;

   rules OptCallingConv ::=               | "ccc"            | "fastcc" | 
                            "coldcc"         | "x86_stdcallcc"  | 
                            "x86_fastcallcc" | "cc" Integer  ;

   rules ParamAttr ::= "zeroext"   | "signext" | "inreg" | "sret" | "noalias" | 
                       "nocapture" | "byval"   | "nest"  | "align" Integer ;

   separator ParamAttr " ";
   

   rules RetAttr       ::= "inreg"
                 | "zeroext"
                 | "signext"
                 | "noalias"
                 ;

   separator RetAttr " ";                 

   rules FuncAttr ::= "noreturn"           | "nounwind"          | 
                      "inreg"              | "zeroext"           | 
                      "signext"            | "readnone"          |
                      "readonly"           | "inlinehint"        | 
                      "alignstack"         | "noinline"          | 
                      "alwaysinline"       | "optsize"           |
                      "ssp"                | "sspreq"            | 
                      "returnsepstwice"    | "nonlazybind"       | 
                      "sanitizeepsaddress" | "sanitizeepsthread" |
                      "sanitizeepsmemory" ;
   
   separator FuncAttr " ";                 
   
   rules OptGC ::= | "gc" String ;
                     
   rules OptAlign    ::= | "align" Integer ;
   rules OptCAlign   ::= |  "," "align" Integer ;
   
   rules SectionString ::= "section" String ;
   
   separator String ",";
   
   rules OptSection    ::= SectionString |  ;
   
   rules GlobalVarAttribute ::= SectionString | "align" Integer ;
   separator GlobalVarAttribute "," ;
   
   rules PrimType ::= INTTYPE | "float" | "double" | "ppc_fp128" | "fp128" | "x86_fp80"
             | "label" ;
             
   rules SymbolicValueRef ::= LOCALVALID
    | GLOBALVALID
    | LocalName
    | GlobalName ;
   
   rules Types ::= "opaque" | PrimType | Types OptAddrSpace  "*"
    | SymbolicValueRef
    | "\\"  Integer 
    | Types "("  ArgTypeListI  ")" [FuncAttr]
    | "void" "("  ArgTypeListI  ")" [FuncAttr]
    | "["  Integer "x" Types  "]"
    | "<"  Integer "x" Types  ">"
    | "{" [Types] "}"
    | "{"  "}"
    | "<"  "{" [Types] "}"  ">"
    | "<"  "{"  "}"  ">" ;
   
   separator Types "," ;
   
   rules ArgType ::= Types [ParamAttr] ;
   
   rules ResultTypes ::= Types | "void" ;
   
   separator ArgType "," ;
   
   rules ArgTypeListI ::= [ArgType] | [ArgType]  "," "..." | "..."  ;

   rules ConstVal::= Types "["  [ConstVal]  "]"
    | Types "["  "]"
    | Types "c"  String
    | Types "<"  [ConstVal]  ">"
    | Types "{" [ConstVal] "}"
    | Types "{"  "}"
    | Types "<"  "{" [ConstVal] "}"  ">"
    | Types "<"  "{"  "}"  ">"
    | Types "null"
    | Types "undef"
    | Types SymbolicValueRef
    | Types ConstExpr
    | Types "zeroinitializer"
    | Types Integer
    | Types Integer
    | Types Integer
    | Types Integer
    | Types "true"
    | Types "false"
    | Types FPVAL ;

   rules ConstExpr ::= CastOps "("  ConstVal "to" Types  ")"
    | "getelementptr" OptInBounds "("  ConstVal IndexList  ")"
    | "select" "("  ConstVal  "," ConstVal  "," ConstVal  ")"
    | ArithmeticOps "("  ConstVal  "," ConstVal  ")"
    | LogicalOps "("  ConstVal  "," ConstVal  ")"
    | "icmp" IPredicates "("  ConstVal  "," ConstVal  ")"
    | "fcmp" FPredicates "("  ConstVal  "," ConstVal  ")"
    | "extractelement" "("  ConstVal  "," ConstVal  ")"
    | "insertelement" "("  ConstVal  "," ConstVal  "," ConstVal  ")"
    | "shufflevector" "("  ConstVal  "," ConstVal  "," ConstVal  ")"
    | "extractvalue" "("  ConstVal  ConstantIndexList  ")"
    | "insertvalue" "("  ConstVal  "," ConstVal  ConstantIndexList  ")" ;

   -- TODO make a list
   separator ConstVal ",";
   
   rules GlobalType ::= "global" | "constant" ;

   rules ThreadLocal ::= "threadepslocal"  ;

   rules AliaseeRef ::= ResultTypes SymbolicValueRef
    | "bitcast" "("  AliaseeRef "to" Types  ")" ;
    
   rules OptInBounds  ::= "inbounds"  ;
   
   rules ResolvedVal ::= Types ValueRef ;
   
   rules IndexList ::= | IndexList  "," ResolvedVal ;
   
   rules ConstantIndexList ::= "," Integer | ConstantIndexList  "," Integer ;   
   
   rules ValueRef ::= SymbolicValueRef | ConstValueRef;
   
   rules ConstValueRef ::= Integer
    | Integer
    | FPVAL
    | "true"
    | "false"
    | "null"
    | "undef"
    | "zeroinitializer"
    | "<" [ConstVal] ">"
    | "[" [ConstVal] "]"
    | "["  "]"
    | "c"  String
    | "{" [ConstVal] "}"
    | "{"  "}"
    | "<"  "{" [ConstVal] "}"  ">"
    | "<"  "{"  "}"  ">"
    | ConstExpr
    | "asm" OptSideEffect String  "," String ;
   
   rules OptSideEffect ::= | "sideeffect" ;

   rules AsmBlock ::= String ;
   
   rules TargetDefinition ::= "triple"     "=" String
                      | "datalayout" "=" String ;

   rules Module ::= [Definition] ;
   
   rules Definition
     ::=  "define" Function
    | "declare" FunctionProto
    | "module" "asm" AsmBlock
    | OptLocalAssign "type" Types
    | OptGlobalAssign GVVisibilityStyle ThreadLocal OptAddrSpace GlobalType
      ConstVal [GlobalVarAttribute]
    | OptGlobalAssign GVInternalLinkage GVVisibilityStyle ThreadLocal OptAddrSpace
      GlobalType ConstVal [GlobalVarAttribute]
    | OptGlobalAssign GVExternalLinkage GVVisibilityStyle ThreadLocal OptAddrSpace
      GlobalType Types [GlobalVarAttribute]
    | OptGlobalAssign GVVisibilityStyle "alias" AliasLinkage AliaseeRef
    | "target" TargetDefinition
    | "deplibs" "=" LibrariesDefinition
      ;
   separator Definition " " ;

   rules LibrariesDefinition ::= "[" [String] "]" ;

   rules ArgListH ::= ArgListH  "," Types [ParamAttr] OptLocalName
                    |  Types [ParamAttr] OptLocalName ;

   rules ArgList ::= ArgListH | ArgListH  "," "..." | "..."  ;

   rules FunctionHeaderH ::= OptCallingConv [RetAttr] ResultTypes
                     GlobalName  "("  ArgList  ")"
                     [FuncAttr] OptSection OptAlign OptGC ;
   
   rules BEGIN ::= "begin" | "{";
   
   rules FunctionHeader ::=
     FunctionDefineLinkage GVVisibilityStyle FunctionHeaderH BEGIN ;

   rules END ::=  "end" | "}" ;

   rules Function ::= BasicBlockList END ;

   rules FunctionProto ::= FunctionDeclareLinkage GVVisibilityStyle FunctionHeaderH ;

   --TODO make a list
   rules ReturnedVal ::= ResolvedVal | ReturnedVal  "," ResolvedVal ;

   --TODO make a list
   rules BasicBlockList ::= BasicBlockList BasicBlock | FunctionHeader BasicBlock ;

   rules BasicBlock ::= InstructionList OptLocalAssign BBTerminatorInst ;

   --TODO make a list
   rules InstructionList ::= InstructionList Inst
    |  LABELSTR  ":\n" | ;

   rules BBTerminatorInst ::= 
      "ret" ReturnedVal
    | "ret" "void"
    | "br" "label" ValueRef
    | "br" INTTYPE ValueRef  "," "label" ValueRef  "," "label" ValueRef
    | "switch" IntType ValueRef  "," "label" ValueRef "[" JumpTable "]"
    | "switch" IntType ValueRef  "," "label" ValueRef "["  "]"
    | "invoke" OptCallingConv ResultTypes ValueRef  "("  ParamList  ")"
      [FuncAttr]
      "to" "label" ValueRef "unwind" "label" ValueRef
    | "unwind"
    | "unreachable"  ;

   rules JumpTable ::= JumpTable IntType ConstValueRef  "," "label" ValueRef
                     | IntType ConstValueRef  "," "label" ValueRef ;

   rules Inst ::=  "  "  OptLocalAssign InstVal  "\n";

   rules PHIList ::= Types "[" ValueRef  "," ValueRef "]"
    | PHIList  "," "[" ValueRef  "," ValueRef "]" ;

   rules ParamList ::= Types [ParamAttr] ValueRef [ParamAttr]
    | "label" [ParamAttr] ValueRef [ParamAttr]
    | ParamList  "," Types [ParamAttr] ValueRef [ParamAttr]
    | ParamList  "," "label" [ParamAttr] ValueRef [ParamAttr]
     ;

   rules OptTailCall ::= "tail" "call" | "call" ;

   rules InstVal ::=
      ArithmeticOps Types ValueRef  "," ValueRef
    | LogicalOps Types ValueRef  "," ValueRef
    | "icmp" IPredicates Types ValueRef  "," ValueRef
    | "fcmp" FPredicates Types ValueRef  "," ValueRef
    | CastOps ResolvedVal "to" Types
    | "select" ResolvedVal  "," ResolvedVal  "," ResolvedVal
    | "vaepsarg" ResolvedVal  "," Types
    | "extractelement" ResolvedVal  "," ResolvedVal
    | "insertelement" ResolvedVal  "," ResolvedVal  "," ResolvedVal
    | "shufflevector" ResolvedVal  "," ResolvedVal  "," ResolvedVal
    | "phi" PHIList
    | OptTailCall OptCallingConv ResultTypes ValueRef  "("  ParamList  ")"
      [FuncAttr]
    | MemoryInst ;

   rules OptVolatile ::= "volatile"  ;

   rules MemoryInst ::= "malloc" Types OptCAlign
    | "malloc" Types  "," INTTYPE ValueRef OptCAlign
    | "alloca" Types OptCAlign
    | "alloca" Types  "," INTTYPE ValueRef OptCAlign
    | "free" ResolvedVal
    | OptVolatile "load" Types ValueRef OptCAlign
    | OptVolatile "store" ResolvedVal  "," Types ValueRef OptCAlign
    | "getresult" Types ValueRef  "," Integer
    | "getelementptr" OptInBounds Types ValueRef IndexList
    | "extractvalue" Types ValueRef  ConstantIndexList 
    | "insertvalue" Types ValueRef  "," Types ValueRef  ConstantIndexList ;
   
   comment "/*" "*/" ;
   comment "//"      ;

   entrypoints ArithmeticOps, OptExact, LogicalOps, CastOps, IPredicates, FPredicates, 
      IntType, FPType, LocalName, OptLocalName, OptAddrSpace, OptLocalAssign,
      GlobalName, OptGlobalAssign, GlobalAssign, GVInternalLinkage,
      GVExternalLinkage, GVVisibilityStyle, FunctionDeclareLinkage,
      FunctionDefineLinkage, AliasLinkage, OptCallingConv, ParamAttr,
      RetAttr, FuncAttr, OptGC, OptAlign, OptCAlign, SectionString,
      OptSection, GlobalVarAttribute, PrimType,
      SymbolicValueRef, Types, ResultTypes,
      ArgTypeListI, ConstVal, ConstExpr, GlobalType, ThreadLocal,
      AliaseeRef, OptInBounds, ResolvedVal, IndexList, ConstantIndexList,
      ValueRef, ConstValueRef, OptSideEffect, AsmBlock, TargetDefinition,
      Module, Definition, LibrariesDefinition, ArgListH, ArgList, FunctionHeaderH,
      FunctionHeader, Function, FunctionProto, ReturnedVal, BasicBlockList,
      BasicBlock, InstructionList, BBTerminatorInst, JumpTable, Inst,
      PHIList, ParamList, OptTailCall, InstVal, OptVolatile, MemoryInst;   
  |]































