{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{                                                            }
{          Copyright(c) 2002-2005, The Mercury Team          }
{                  Contact: info@mdolib.org                  }
{                                                            }
{           Based on the FreeIBComponents written            }
{          by  Gregory H. Deatz - gdeatz@hlmdd.com           }
{           and InterBase Express 4.3 created by             }
{                    Inprise Corporation.                    }
{                                                            }
{************************************************************}


{//$A+}        { Aligned records: On }
{//$B-}        { Short circuit boolean expressions: Off }
{//$G+}        { Imported data: On }
{//$H+}        { Huge Strings: On }
{//$J-}        { Modification of Typed Constants: Off }
{//$M+}        { Generate run-time type information: On }
{//$Q-}        { Overflow checks: Off }
{//$R-}        { Range checks: Off }
{//$T+}        { Typed address: On }
{//$U+}        { Pentim-safe FDIVs: On }
{//$X+}        { Extended syntax: On }
{//$Z1}        { Minimum Enumeration Size: 1 Byte }


{$IFDEF VER130}      // Delphi 5
  {$DEFINE MDO_DELPHI5}
  {$DEFINE MDO_DELPHI5_UP}
{$ENDIF}
{$IFDEF VER140}      // Delphi 6
  {$DEFINE MDO_DELPHI6}
  {$DEFINE MDO_DELPHI6_UP}
  {$DEFINE MDO_DELPHI5_UP}
{$ENDIF}
{$IFDEF VER150}      // Delphi 7
  {$DEFINE MDO_DELPHI7}
  {$DEFINE MDO_DELPHI7_UP}  
  {$DEFINE MDO_DELPHI6_UP}
  {$DEFINE MDO_DELPHI5_UP}
{$ENDIF}
{$IFDEF VER170}      // Delphi 2005
  {$DEFINE MDO_DELPHI2005}
  {$DEFINE MDO_DELPHI7_UP}
  {$DEFINE MDO_DELPHI6_UP}
  {$DEFINE MDO_DELPHI5_UP}
{$ENDIF}
{$IFDEF VER180}      // Delphi 2006
  {$DEFINE MDO_DELPHI2006}
  {$DEFINE MDO_DELPHI2005}
  {$DEFINE MDO_DELPHI7_UP}
  {$DEFINE MDO_DELPHI6_UP}
  {$DEFINE MDO_DELPHI5_UP}
{$ENDIF}

