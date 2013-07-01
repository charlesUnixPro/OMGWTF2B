
      * yylex states

       01 startOfLine constant as 0.
       01 inToken constant as 2.
       01 inOperands constant as 3.
       01 atEnd constant as 4.

      * variableTable variable types

       01 vtTypeScalar constant as 0.
       01 vtTypeString constant as H"80".

      *  Operands

       01 opNCONST constant as H"0e".
       01 opSCONST constant as H"0f".
       01 opComma1 constant as H"12".
       01 opColon  constant as H"14".
       01 opSemi   constant as H"15".
       01 opEOL    constant as H"16".
       01 opTo     constant as H"19".
       01 opStep   constant as H"1a".
       01 opThen   constant as H"1b".
       01 opSharp  constant as H"1c".
       01 opNE     constant as H"1e".
       01 opEQ1    constant as H"22".
       01 opTimes  constant as H"24".
       01 opMinus1 constant as H"26".
       01 opClose  constant as H"2c".
       01 opPlus   constant as H"25".
       01 opDiv    constant as H"27".
       01 opOr     constant as H"29".
       01 opOpen1  constant as H"2b".
       01 opEQ2    constant as H"2d".
       01 opEQ3    constant as H"2e".
       01 opEQ4    constant as H"34".
       01 opMinus2 constant as H"36".
       01 opOpen2  constant as H"37".
       01 opOpen3  constant as H"3a".
       01 opOpen4  constant as H"3b".
       01 opComma2 constant as H"3c".
       01 opLen    constant as H"42".
       01 opPeek   constant as H"46".
       01 opRnd    constant as H"48".
       01 opInt    constant as H"50".

      *  tokens

       01 tokREM      constant as H"00".
       01 tokINPUT    constant as H"02".
       01 tokIF       constant as H"07".
       01 tokFOR      constant as H"08".
       01 tokNEXT     constant as H"09".
       01 tokGOTO     constant as H"0a".
       01 tokGO_TO    constant as H"0b".
       01 tokGOSUB    constant as H"0c".
       01 tokTRAP     constant as H"0d".
       01 tokDIM      constant as H"14".
       01 tokEND      constant as H"15".
       01 tokPOKE     constant as H"1f".
       01 tokPRINT    constant as H"20".
       01 tokRETURN   constant as H"24".
       01 tokPOP      constant as H"27".
       01 tokQMARK    constant as H"28".
       01 tokGRAPHICS constant as H"2b".
       01 tokPOSITION constant as H"2d".
       01 tokSETCOLOR constant as H"30".
      *  silent let
       01 tokLET      constant as H"36".

      *  Special tokens for our parser

      *  end of token
       01 tokEOT      constant as H"f1".
      *  end of file
       01 tokEOF      constant as H"00".
      *  alias for tokREM that is non-zero
       01 tokREM2     constant as H"f3".
       01 opVAR       constant as H"80".
       01 opSVAR      constant as H"81".

