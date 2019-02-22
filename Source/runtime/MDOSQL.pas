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

unit MDOSQL;

{$I ..\MDO.INC}

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, MDOHeader, MDOErrorCodes,
  MDOExternals, DB, MDO, MDODatabase, MDOUtils, MDOConst
  {$IFDEF MDO_DELPHI6_UP}
    , Variants
  {$ENDIF};

type
  TMDOSQL = class;
  TMDOXSQLDA = class;

  { TMDOXSQLVAR }
  TMDOXSQLVAR = class (TObject)
  private
    FIndex: Integer;
    FModified: Boolean;
    FName: string;
    FParent: TMDOXSQLDA;
    FSQL: TMDOSQL;
    FXSQLVAR: PXSQLVAR;
    function AdjustScale(Value: Int64; Scale: Integer): Double;
    function AdjustScaleToCurrency(Value: Int64; Scale: Integer): Currency;
    function AdjustScaleToInt64(Value: Int64; Scale: Integer): Int64;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Float;
    function GetAsInt64: Int64;
    function GetAsLong: Integer;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: Short;
    function GetAsString: string;
    function GetAsVariant: Variant;
    function GetAsXSQLVAR: PXSQLVAR;
    function GetIsNull: Boolean;
    function GetIsNullable: Boolean;
    function GetSize: Integer;
    function GetSQLType: Integer;
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsDouble(Value: Double);
    procedure SetAsFloat(Value: Float);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsLong(Value: Integer);
    procedure SetAsPointer(Value: Pointer);
    procedure SetAsQuad(Value: TISC_QUAD);
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: string);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsVariant(Value: Variant);
    procedure SetAsXSQLVAR(Value: PXSQLVAR);
    procedure SetIsNull(Value: Boolean);
    procedure SetIsNullable(Value: Boolean);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
  public
    constructor Create(Parent: TMDOXSQLDA; Query: TMDOSQL);
    procedure Assign(Source: TMDOXSQLVAR);
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: String);
    procedure SaveToStream(Stream: TStream);
    procedure Clear;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsFloat: Float read GetAsFloat write SetAsFloat;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: Integer read GetAsLong write SetAsLong;
    property AsLong: Long read GetAsLong write SetAsLong;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: string read GetAsString write SetAsString;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsXSQLVAR: PXSQLVAR read GetAsXSQLVAR write SetAsXSQLVAR;
    property Data: PXSQLVAR read FXSQLVAR write FXSQLVAR;
    property Index: Integer read FIndex;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
    property Modified: Boolean read FModified write FModified;
    property Name: string read FName;
    property Size: Integer read GetSize;
    property SQLType: Integer read GetSQLType;
    property Value: Variant read GetAsVariant write SetAsVariant;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  end;
  
  TMDOXSQLVARArray = Array of TMDOXSQLVAR;

  { TMDOXSQLDA }
  TMDOXSQLDA = class (TObject)
  protected
    FCount: Integer;
    FNames: TStrings;
    FSize: Integer;
    FSQL: TMDOSQL;
    FUniqueRelationName: string;
    FXSQLDA: PXSQLDA;
    FXSQLVARs: TMDOXSQLVARArray;
    function GetModified: Boolean;
    function GetNames: string;
    function GetRecordSize: Integer;
    function GetXSQLDA: PXSQLDA;
    function GetXSQLVAR(Idx: Integer): TMDOXSQLVAR;
    function GetXSQLVARByName(Idx: String): TMDOXSQLVAR;
    procedure Initialize;
    procedure SetCount(Value: Integer);
  public
    constructor Create(Query: TMDOSQL);
    destructor Destroy; override;
    procedure AddName(FieldName: String; Idx: Integer);
    function ByName(Idx: String): TMDOXSQLVAR;
    property AsXSQLDA: PXSQLDA read GetXSQLDA;
    property Count: Integer read FCount write SetCount;
    property Modified: Boolean read GetModified;
    property Names: string read GetNames;
    property RecordSize: Integer read GetRecordSize;
    property UniqueRelationName: string read FUniqueRelationName;
    property Vars[Idx: Integer]: TMDOXSQLVAR read GetXSQLVAR; default;
  end;
  
  { TMDOBatch }

  TMDOBatch = class (TObject)
  protected
    FColumns: TMDOXSQLDA;
    FFilename: string;
    FParams: TMDOXSQLDA;
  public
    procedure ReadyFile; virtual; abstract;
    property Columns: TMDOXSQLDA read FColumns;
    property Filename: string read FFilename write FFilename;
    property Params: TMDOXSQLDA read FParams;
  end;
  
  TMDOBatchInput = class (TMDOBatch)
  public
    function ReadParameters: Boolean; virtual; abstract;
  end;
  
  TMDOBatchOutput = class (TMDOBatch)
  public
    function WriteColumns: Boolean; virtual; abstract;
  end;
  

  { TMDOOutputDelimitedFile }
  TMDOOutputDelimitedFile = class (TMDOBatchOutput)
  protected
    FColDelimiter: string;
    FHandle: THandle;
    FOutputTitles: Boolean;
    FRowDelimiter: string;
  public
    destructor Destroy; override;
    procedure ReadyFile; override;
    function WriteColumns: Boolean; override;
    property ColDelimiter: string read FColDelimiter write FColDelimiter;
    property OutputTitles: Boolean read FOutputTitles write FOutputTitles;
    property RowDelimiter: string read FRowDelimiter write FRowDelimiter;
  end;
  
  { TMDOInputDelimitedFile }
  TMDOInputDelimitedFile = class (TMDOBatchInput)
  protected
    FColDelimiter: string;
    FEOF: Boolean;
    FFile: TFileStream;
    FLookAhead: Char;
    FReadBlanksAsNull: Boolean;
    FRowDelimiter: string;
    FSkipTitles: Boolean;
  public
    destructor Destroy; override;
    function GetColumn(var Col: string): Integer;
    function ReadParameters: Boolean; override;
    procedure ReadyFile; override;
    property ColDelimiter: string read FColDelimiter write FColDelimiter;
    property ReadBlanksAsNull: Boolean read FReadBlanksAsNull write 
            FReadBlanksAsNull;
    property RowDelimiter: string read FRowDelimiter write FRowDelimiter;
    property SkipTitles: Boolean read FSkipTitles write FSkipTitles;
  end;
  
  { TMDOOutputRawFile }
  TMDOOutputRawFile = class (TMDOBatchOutput)
  protected
    FHandle: THandle;
  public
    destructor Destroy; override;
    procedure ReadyFile; override;
    function WriteColumns: Boolean; override;
  end;
  
  { TMDOInputRawFile }
  TMDOInputRawFile = class (TMDOBatchInput)
  protected
    FHandle: THandle;
  public
    destructor Destroy; override;
    function ReadParameters: Boolean; override;
    procedure ReadyFile; override;
  end;
  
     { TMDOSQL }
  TMDOSQLTypes = (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator);

  TMDOSQL = class (TComponent)
  private
    FFBLoaded: Boolean;
  protected
    FBase: TMDOBase;
    FBof: Boolean;
    FCursor: string;
    FEof: Boolean;
    FGenerateParamNames: Boolean;
    FGoToFirstRecordOnExecute: Boolean;
    FHandle: TISC_STMT_HANDLE;
    FOnSQLChanging: TNotifyEvent;
    FOpen: Boolean;
    FParamCheck: Boolean;
    FPrepared: Boolean;
    FProcessedSQL: TStrings;
    FRecordCount: Integer;
    FSQL: TStrings;
    FSQLParams: TMDOXSQLDA;
    FSQLRecord: TMDOXSQLDA;
    FSQLType: TMDOSQLTypes;
    procedure BeforeTransactionEnd(Sender: TObject);
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
    function GetDatabase: TMDODatabase;
    function GetDBHandle: PISC_DB_HANDLE;
    function GetEof: Boolean;
    function GetFieldIndex(FieldName: String): Integer;
    function GetFields(const Idx: Integer): TMDOXSQLVAR;
    function GetPlan: string;
    function GetRecordCount: Integer;
    function GetRowsAffected: Integer;
    function GetSQLParams: TMDOXSQLDA;
    function GetTransaction: TMDOTransaction;
    function GetTRHandle: PISC_TR_HANDLE;
    procedure PreprocessSQL;
    procedure SetDatabase(Value: TMDODatabase);
    procedure SetSQL(Value: TStrings);
    procedure SetTransaction(Value: TMDOTransaction);
    procedure SQLChanging(Sender: TObject);
    function GetCursorName: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BatchInput(InputObject: TMDOBatchInput);
    procedure BatchOutput(OutputObject: TMDOBatchOutput);
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    procedure CheckClosed;
    procedure CheckOpen;
    procedure CheckValidStatement;
    procedure Close;
    function Current: TMDOXSQLDA;
    procedure ExecQuery;
    function FieldByName(FieldName: String): TMDOXSQLVAR;
    procedure FreeHandle;
    function GetUniqueRelationName: string;
    function Next: TMDOXSQLDA;
    function ParamByName(Idx: String): TMDOXSQLVAR;
    procedure Prepare;
    property Bof: Boolean read FBof;
    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
    property Eof: Boolean read GetEof;
    property FieldIndex[FieldName: String]: Integer read GetFieldIndex;
    property Fields[const Idx: Integer]: TMDOXSQLVAR read GetFields;
    property GenerateParamNames: Boolean read FGenerateParamNames write
            FGenerateParamNames;
    property Handle: TISC_STMT_HANDLE read FHandle;
    property Open: Boolean read FOpen;
    property Params: TMDOXSQLDA read GetSQLParams;
    property Plan: string read GetPlan;
    property Prepared: Boolean read FPrepared;
    property RecordCount: Integer read GetRecordCount;
    property RowsAffected: Integer read GetRowsAffected;
    property SQLType: TMDOSQLTypes read FSQLType;
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
    property UniqueRelationName: string read GetUniqueRelationName;

  published
    property Database: TMDODatabase read GetDatabase write SetDatabase;
    property GoToFirstRecordOnExecute: Boolean read FGoToFirstRecordOnExecute
            write FGoToFirstRecordOnExecute default True;
    property OnSQLChanging: TNotifyEvent read FOnSQLChanging write
            FOnSQLChanging;
    property ParamCheck: Boolean read FParamCheck write FParamCheck;
    property SQL: TStrings read FSQL write SetSQL;
    property Transaction: TMDOTransaction read GetTransaction write
            SetTransaction;
  end;
  
implementation

uses
  MDOIntf, MDOBlob, MDOSQLMonitor;

var
  SequentialNumber: Integer = 1;
  SequentialSerie: Integer = 1;

{ TMDOXSQLVAR }
{
********************************* TMDOXSQLVAR **********************************
}
constructor TMDOXSQLVAR.Create(Parent: TMDOXSQLDA; Query: TMDOSQL);
begin
  inherited Create;
  FParent := Parent;
  FSQL := Query;
end;

function TMDOXSQLVAR.AdjustScale(Value: Int64; Scale: Integer): Double;
var
  Scaling: Int64;
  i: Integer;
  Val: Double;
begin
  Scaling := 1; Val := Value;
  if Scale > 0 then
  begin
    for i := 1 to Scale do
      Scaling := Scaling * 10;
    result := Val * Scaling;
  end
  else
    if Scale < 0 then
    begin
      for i := -1 downto Scale do
        Scaling := Scaling * 10;
      result := Val / Scaling;
    end
    else
      result := Val;
end;

function TMDOXSQLVAR.AdjustScaleToCurrency(Value: Int64; Scale: Integer): 
        Currency;
var
  Scaling: Int64;
  i: Integer;
  FractionText, PadText, CurrText: string;
begin
  Result := 0;
  Scaling := 1;
  if Scale > 0 then
  begin
    for i := 1 to Scale do
      Scaling := Scaling * 10;
    result := Value * Scaling;
  end
  else
    if Scale < 0 then
    begin
      for i := -1 downto Scale do
        Scaling := Scaling * 10;
      FractionText := IntToStr(abs(Value mod Scaling));
      for i := Length(FractionText) to -Scale -1 do
        PadText := '0' + PadText;
      if Value < 0 then
        CurrText := '-' + IntToStr(Abs(Value div Scaling)) + DecimalSeparator + PadText + FractionText
      else
        CurrText := IntToStr(Abs(Value div Scaling)) + DecimalSeparator + PadText + FractionText;
      try
        result := StrToCurr(CurrText);
      except
        on E: Exception do
          MDOError(mdoeInvalidDataConversion, [nil]);
      end;
    end
    else
      result := Value;
end;

function TMDOXSQLVAR.AdjustScaleToInt64(Value: Int64; Scale: Integer): Int64;
var
  Scaling: Int64;
  i: Integer;
  Val: Int64;
begin
  Scaling := 1; Val := Value;
  if Scale > 0 then
  begin
    for i := 1 to Scale do Scaling := Scaling * 10;
    result := Val * Scaling;
  end else if Scale < 0 then
  begin
    for i := -1 downto Scale do Scaling := Scaling * 10;
    result := Val div Scaling;
  end else
    result := Val;
end;

procedure TMDOXSQLVAR.Assign(Source: TMDOXSQLVAR);
var
  szBuff: PChar;
  s_bhandle, d_bhandle: TISC_BLOB_HANDLE;
  bSourceBlob, bDestBlob: Boolean;
  iSegs, iMaxSeg, iSize: Long;
  iBlobType: Short;
begin
  szBuff := nil;
  bSourceBlob := True;
  bDestBlob := True;
  s_bhandle := nil;
  d_bhandle := nil;
  try
    if (Source.IsNull) then
    begin
      IsNull := True;
      exit;
    end
    else
      if (FXSQLVAR^.sqltype and (not 1) = SQL_ARRAY) or
         (Source.FXSQLVAR^.sqltype and (not 1) = SQL_ARRAY) then
        exit; { arrays not supported }
    if (FXSQLVAR^.sqltype and (not 1) <> SQL_BLOB) and
       (Source.FXSQLVAR^.sqltype and (not 1) <> SQL_BLOB) then
    begin
      AsXSQLVAR := Source.AsXSQLVAR;
      exit;
    end
    else
      if (Source.FXSQLVAR^.sqltype and (not 1) <> SQL_BLOB) then
      begin
        szBuff := nil;
        MDOAlloc(szBuff, 0, Source.FXSQLVAR^.sqllen);
        Move(Source.FXSQLVAR^.sqldata[0], szBuff[0], Source.FXSQLVAR^.sqllen);
        bSourceBlob := False;
        iSize := Source.FXSQLVAR^.sqllen;
      end
      else
        if (FXSQLVAR^.sqltype and (not 1) <> SQL_BLOB) then
          bDestBlob := False;
  
    if bSourceBlob then
    begin
      { read the blob }
      Source.FSQL.Call(isc_open_blob2(StatusVector, Source.FSQL.DBHandle,
        Source.FSQL.TRHandle, @s_bhandle, PISC_QUAD(Source.FXSQLVAR.sqldata),
        0, nil), True);
      try
        MDOBlob.GetBlobInfo(@s_bhandle, iSegs, iMaxSeg, iSize,
          iBlobType);
        szBuff := nil;
        MDOAlloc(szBuff, 0, iSize);
        MDOBlob.ReadBlob(@s_bhandle, szBuff, iSize);
      finally
        Source.FSQL.Call(isc_close_blob(StatusVector, @s_bhandle), True);
      end;
    end;
  
    if bDestBlob then
    begin
      { write the blob }
      FSQL.Call(isc_create_blob2(StatusVector, FSQL.DBHandle,
        FSQL.TRHandle, @d_bhandle, PISC_QUAD(FXSQLVAR.sqldata),
        0, nil), True);
      try
        MDOBlob.WriteBlob(@d_bhandle, szBuff, iSize);
      finally
        FSQL.Call(isc_close_blob(StatusVector, @d_bhandle), True);
      end;
    end
    else
    begin
      { just copy the buffer }
      FXSQLVAR.sqltype := SQL_TEXT;
      FXSQLVAR.sqllen := iSize;
      MDOAlloc(FXSQLVAR.sqldata, iSize, iSize);
      Move(szBuff[0], FXSQLVAR^.sqldata[0], iSize);
    end;
  finally
    FreeMem(szBuff);
  end;
end;

function TMDOXSQLVAR.GetAsCurrency: Currency;
begin
  result := 0;
  if FSQL.Database.SQLDialect < 3 then
    result := GetAsDouble
  else begin
    if not IsNull then
      case FXSQLVAR^.sqltype and (not 1) of
        SQL_TEXT, SQL_VARYING: begin
          try
            result := StrtoCurr(AsString);
          except
            on E: Exception do MDOError(mdoeInvalidDataConversion, [nil]);
          end;
        end;
        SQL_SHORT:
          result := AdjustScaleToCurrency(Int64(PShort(FXSQLVAR^.sqldata)^),
                                      FXSQLVAR^.sqlscale);
        SQL_LONG:
          result := AdjustScaleToCurrency(Int64(PLong(FXSQLVAR^.sqldata)^),
                                      FXSQLVAR^.sqlscale);
        SQL_INT64:
          result := AdjustScaleToCurrency(PInt64(FXSQLVAR^.sqldata)^,
                                      FXSQLVAR^.sqlscale);
        SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
          result := GetAsDouble;
        else
          MDOError(mdoeInvalidDataConversion, [nil]);
      end;
    end;
end;

function TMDOXSQLVAR.GetAsDateTime: TDateTime;
var
  tm_date: TCTimeStructure;
begin
  result := 0;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToDate(AsString);
        except
          on E: EConvertError do MDOError(mdoeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_TYPE_DATE: begin
        isc_decode_sql_date(PISC_DATE(FXSQLVAR^.sqldata), @tm_date);
        try
          result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
                               Word(tm_date.tm_mday));
        except
          on E: EConvertError do
          begin
            MDOError(mdoeInvalidDataConversion, [nil]);
          end;
        end;
      end;
      SQL_TYPE_TIME: begin
        isc_decode_sql_time(PISC_TIME(FXSQLVAR^.sqldata), @tm_date);
        try
          result := EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                               Word(tm_date.tm_sec), 0)
        except
          on E: EConvertError do
          begin
            MDOError(mdoeInvalidDataConversion, [nil]);
          end;
        end;
      end;
      SQL_TIMESTAMP: begin
        isc_decode_date(PISC_QUAD(FXSQLVAR^.sqldata), @tm_date);
        try
          result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
                              Word(tm_date.tm_mday));
          if result >= 0 then
            result := result + EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                                          Word(tm_date.tm_sec), 0)
          else
            result := result - EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                                          Word(tm_date.tm_sec), 0)
        except
          on E: EConvertError do
          begin
            MDOError(mdoeInvalidDataConversion, [nil]);
          end;
        end;
      end;
      else
        MDOError(mdoeInvalidDataConversion, [nil]);
    end;
end;

function TMDOXSQLVAR.GetAsDouble: Double;
begin
  result := 0;
  if not IsNull then
  begin
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToFloat(AsString);
        except
          on E: Exception do MDOError(mdoeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScale(Int64(PShort(FXSQLVAR^.sqldata)^),
                              FXSQLVAR^.sqlscale);
      SQL_LONG:
        result := AdjustScale(Int64(PLong(FXSQLVAR^.sqldata)^),
                              FXSQLVAR^.sqlscale);
      SQL_INT64:
        result := AdjustScale(PInt64(FXSQLVAR^.sqldata)^, FXSQLVAR^.sqlscale);
      SQL_FLOAT:
        result := PFloat(FXSQLVAR^.sqldata)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        result := PDouble(FXSQLVAR^.sqldata)^;
      else
        MDOError(mdoeInvalidDataConversion, [nil]);
    end;
    if  FXSQLVAR^.sqlscale <> 0 then
      result :=
        StrToFloat(FloatToStrF(result, fffixed, 15,
                  Abs(FXSQLVAR^.sqlscale) ));
  end;
end;

function TMDOXSQLVAR.GetAsFloat: Float;
begin
  result := 0;
  try
    result := AsDouble;
  except
    on E: EOverflow do
      MDOError(mdoeInvalidDataConversion, [nil]);
  end;
end;

function TMDOXSQLVAR.GetAsInt64: Int64;
begin
  result := 0;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt64(AsString);
        except
          on E: Exception do MDOError(mdoeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScaleToInt64(Int64(PShort(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale);
      SQL_LONG:
        result := AdjustScaleToInt64(Int64(PLong(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale);
      SQL_INT64:
        result := AdjustScaleToInt64(PInt64(FXSQLVAR^.sqldata)^,
                                    FXSQLVAR^.sqlscale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        MDOError(mdoeInvalidDataConversion, [nil]);
    end;
end;

function TMDOXSQLVAR.GetAsLong: Integer;
begin
  result := 0;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt(AsString);
        except
          on E: Exception do MDOError(mdoeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := Trunc(AdjustScale(Int64(PShort(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale));
      SQL_LONG:
        result := Trunc(AdjustScale(Int64(PLong(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale));
      SQL_INT64:
        result := Trunc(AdjustScale(PInt64(FXSQLVAR^.sqldata)^, FXSQLVAR^.sqlscale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        MDOError(mdoeInvalidDataConversion, [nil]);
    end;
end;

function TMDOXSQLVAR.GetAsPointer: Pointer;
begin
  if not IsNull then
    result := FXSQLVAR^.sqldata
  else
    result := nil;
end;

function TMDOXSQLVAR.GetAsQuad: TISC_QUAD;
begin
  result.gds_quad_high := 0;
  result.gds_quad_low := 0;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        result := PISC_QUAD(FXSQLVAR^.sqldata)^;
      else
        MDOError(mdoeInvalidDataConversion, [nil]);
    end;
end;

function TMDOXSQLVAR.GetAsShort: Short;
begin
  result := 0;
  try
    result := AsLong;
  except
    on E: Exception do MDOError(mdoeInvalidDataConversion, [nil]);
  end;
end;

function TMDOXSQLVAR.GetAsString: string;
var
  sz: PChar;
  str_len: Integer;
  ss: TStringStream;
begin
  result := '';
  { Check null, if so return a default string }
  if not IsNull then
    case FXSQLVar^.sqltype and (not 1) of
      SQL_ARRAY:
        result := '(Array)'; {do not localize}
      SQL_BLOB: begin
        ss := TStringStream.Create('');
        SaveToStream(ss);
        result := ss.DataString;
        ss.Free;
      end;
      SQL_TEXT, SQL_VARYING: begin
        sz := FXSQLVAR^.sqldata;
        if (FXSQLVar^.sqltype and (not 1) = SQL_TEXT) then
          str_len := FXSQLVar^.sqllen
        else begin
          str_len := isc_vax_integer(FXSQLVar^.sqldata, 2);
          Inc(sz, 2);
        end;
        SetString(result, sz, str_len);
        if ((FXSQLVar^.sqltype and (not 1)) = SQL_TEXT) then
          result := TrimRight(result);
      end;
      SQL_TYPE_DATE:
        case FSQL.Database.SQLDialect of
          1 : result := DateTimeToStr(AsDateTime);
          3 : result := DateToStr(AsDateTime);
        end;
      SQL_TYPE_TIME :
        result := TimeToStr(AsDateTime);
      SQL_TIMESTAMP:
        result := DateTimeToStr(AsDateTime);
      SQL_SHORT, SQL_LONG:
        if FXSQLVAR^.sqlscale = 0 then
          result := IntToStr(AsLong)
        else if FXSQLVAR^.sqlscale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_INT64:
        if FXSQLVAR^.sqlscale = 0 then
          result := IntToStr(AsInt64)
        else if FXSQLVAR^.sqlscale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := FloatToStr(AsDouble);
      else
        MDOError(mdoeInvalidDataConversion, [nil]);
    end;
end;

function TMDOXSQLVAR.GetAsVariant: Variant;
begin
  if IsNull then
    result := NULL
  { Check null, if so return a default string }
  else case FXSQLVar^.sqltype and (not 1) of
      SQL_ARRAY:
        result := '(Array)'; {do not localize}
      SQL_BLOB:
        result := '(Blob)'; {do not localize}
      SQL_TEXT, SQL_VARYING:
        result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if FXSQLVAR^.sqlscale = 0 then
          result := AsLong
        else if FXSQLVAR^.sqlscale >= (-4) then
          result := AsCurrency
        else
          result := AsDouble;
      SQL_INT64:
        if FXSQLVAR^.sqlscale = 0 then
          MDOError(mdoeInvalidDataConversion, [nil])
        else if FXSQLVAR^.sqlscale >= (-4) then
          result := AsCurrency
        else
          result := AsDouble;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := AsDouble;
      else
        MDOError(mdoeInvalidDataConversion, [nil]);
    end;
end;

function TMDOXSQLVAR.GetAsXSQLVAR: PXSQLVAR;
begin
  result := FXSQLVAR;
end;

function TMDOXSQLVAR.GetIsNull: Boolean;
begin
  result := IsNullable and (FXSQLVAR^.sqlind^ = -1);
end;

function TMDOXSQLVAR.GetIsNullable: Boolean;
begin
  result := (FXSQLVAR^.sqltype and 1 = 1);
end;

function TMDOXSQLVAR.GetSize: Integer;
begin
  result := FXSQLVAR^.sqllen;
end;

function TMDOXSQLVAR.GetSQLType: Integer;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;

procedure TMDOXSQLVAR.LoadFromFile(const FileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TMDOXSQLVAR.LoadFromStream(Stream: TStream);
var
  bs: TMDOBlobStream;
begin
  bs := TMDOBlobStream.Create;
  try
    bs.Mode := bmWrite;
    bs.Database := FSQL.Database;
    bs.Transaction := FSQL.Transaction;
    Stream.Seek(0, soFromBeginning);
    bs.LoadFromStream(Stream);
    bs.Finalize;
    AsQuad := bs.BlobID;
  finally
    bs.Free;
  end;
end;

procedure TMDOXSQLVAR.SaveToFile(const FileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TMDOXSQLVAR.SaveToStream(Stream: TStream);
var
  bs: TMDOBlobStream;
begin
  bs := TMDOBlobStream.Create;
  try
    bs.Mode := bmRead;
    bs.Database := FSQL.Database;
    bs.Transaction := FSQL.Transaction;
    bs.BlobID := AsQuad;
    bs.SaveToStream(Stream);
  finally
    bs.Free;
  end;
end;

procedure TMDOXSQLVAR.Clear;
begin
  IsNull := True;
end;

procedure TMDOXSQLVAR.SetAsCurrency(Value: Currency);
var
  xvar: TMDOXSQLVAR;
  i: Integer;
begin
  if FSQL.Database.SQLDialect < 3 then
    AsDouble := Value
  else
  begin
    if IsNullable then
      IsNull := False;
    for i := 0 to FParent.FCount - 1 do
      if FParent.FNames[i] = FName then
      begin
        xvar := FParent[i];
        xvar.FXSQLVAR^.sqltype := SQL_INT64 or (xvar.FXSQLVAR^.sqltype and 1);
        xvar.FXSQLVAR^.sqlscale := -4;
        xvar.FXSQLVAR^.sqllen := SizeOf(Int64);
        MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
        PCurrency(xvar.FXSQLVAR^.sqldata)^ := Value;
        xvar.FModified := True;
      end;
  end;
end;

procedure TMDOXSQLVAR.SetAsDate(Value: TDateTime);
var
  i: Integer;
  tm_date: TCTimeStructure;
  Yr, Mn, Dy: Word;
  xvar: TMDOXSQLVAR;
begin
  if FSQL.Database.SQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_TYPE_DATE or (xvar.FXSQLVAR^.sqltype and 1);
      DecodeDate(Value, Yr, Mn, Dy);
      with tm_date do
      begin
        tm_sec := 0;
        tm_min := 0;
        tm_hour := 0;
        tm_mday := Dy;
        tm_mon := Mn - 1;
        tm_year := Yr - 1900;
      end;
      xvar.FXSQLVAR^.sqllen := SizeOf(ISC_DATE);
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      isc_encode_sql_date(@tm_date, PISC_DATE(xvar.FXSQLVAR^.sqldata));
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsDateTime(Value: TDateTime);
var
  i: Integer;
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_TIMESTAMP or (xvar.FXSQLVAR^.sqltype and 1);
      DecodeDate(Value, Yr, Mn, Dy);
      DecodeTime(Value, Hr, Mt, S, Ms);
      with tm_date do
      begin
        tm_sec := S;
        tm_min := Mt;
        tm_hour := Hr;
        tm_mday := Dy;
        tm_mon := Mn - 1;
        tm_year := Yr - 1900;
      end;
      xvar.FXSQLVAR^.sqllen := SizeOf(TISC_QUAD);
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      isc_encode_date(@tm_date, PISC_QUAD(xvar.FXSQLVAR^.sqldata));
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsDouble(Value: Double);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_DOUBLE or (xvar.FXSQLVAR^.sqltype and 1);
      xvar.FXSQLVAR^.sqllen := SizeOf(Double);
      xvar.FXSQLVAR^.sqlscale := 0;
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      PDouble(xvar.FXSQLVAR^.sqldata)^ := Value;
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsFloat(Value: Float);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_FLOAT or (xvar.FXSQLVAR^.sqltype and 1);
      xvar.FXSQLVAR^.sqllen := SizeOf(Float);
      xvar.FXSQLVAR^.sqlscale := 0;
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      PSingle(xvar.FXSQLVAR^.sqldata)^ := Value;
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsInt64(Value: Int64);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_INT64 or (xvar.FXSQLVAR^.sqltype and 1);
      xvar.FXSQLVAR^.sqlscale := 0;
      xvar.FXSQLVAR^.sqllen := SizeOf(Int64);
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      PInt64(xvar.FXSQLVAR^.sqldata)^ := Value;
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsLong(Value: Integer);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_LONG or (xvar.FXSQLVAR^.sqltype and 1);
      xvar.FXSQLVAR^.sqllen := SizeOf(Long);
      xvar.FXSQLVAR^.sqlscale := 0;
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      PLong(xvar.FXSQLVAR^.sqldata)^ := Value;
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsPointer(Value: Pointer);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable and (Value = nil) then
    IsNull := True
  else begin
    IsNull := False;
    for i := 0 to FParent.FCount - 1 do
      if FParent.FNames[i] = FName then
      begin
        xvar := FParent[i];
        xvar.FXSQLVAR^.sqltype := SQL_TEXT or (FXSQLVAR^.sqltype and 1);
        Move(Value^, xvar.FXSQLVAR^.sqldata^, xvar.FXSQLVAR^.sqllen);
        xvar.FModified := True;
      end;
  end;
end;

procedure TMDOXSQLVAR.SetAsQuad(Value: TISC_QUAD);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      if (xvar.FXSQLVAR^.sqltype and (not 1) <> SQL_BLOB) and
         (xvar.FXSQLVAR^.sqltype and (not 1) <> SQL_ARRAY) then
        MDOError(mdoeInvalidDataConversion, [nil]);
      xvar.FXSQLVAR^.sqllen := SizeOf(TISC_QUAD);
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      PISC_QUAD(xvar.FXSQLVAR^.sqldata)^ := Value;
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsShort(Value: Short);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_SHORT or (xvar.FXSQLVAR^.sqltype and 1);
      xvar.FXSQLVAR^.sqllen := SizeOf(Short);
      xvar.FXSQLVAR^.sqlscale := 0;
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      PShort(xvar.FXSQLVAR^.sqldata)^ := Value;
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsString(Value: string);
var
  stype: Integer;
  ss: TStringStream;
  
  procedure SetStringValue;
  var
    i: Integer;
    xvar: TMDOXSQLVAR;
  begin
    for i := 0 to FParent.FCount - 1 do
      if FParent.FNames[i] = FName then
      begin
        xvar := FParent[i];
        if (xvar.FXSQLVAR^.sqlname = 'DB_KEY') or {do not localize}
           (xvar.FXSQLVAR^.sqlname = 'RDB$DB_KEY') then {do not localize}
          Move(Value[1], xvar.FXSQLVAR^.sqldata^, xvar.FXSQLVAR^.sqllen)
        else begin
          xvar.FXSQLVAR^.sqltype := SQL_TEXT or (FXSQLVAR^.sqltype and 1);
          xvar.FXSQLVAR^.sqllen := Length(Value);
          MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen + 1);
          if (Length(Value) > 0) then
            Move(Value[1], xvar.FXSQLVAR^.sqldata^, xvar.FXSQLVAR^.sqllen);
        end;
        xvar.FModified := True;
      end;
  end;
  
begin
  if IsNullable then
    IsNull := False;
  stype := FXSQLVAR^.sqltype and (not 1);
  if (stype = SQL_TEXT) or (stype = SQL_VARYING) then
    SetStringValue
  else begin
    if (stype = SQL_BLOB) then
    begin
      ss := TStringStream.Create(Value);
      try
        LoadFromStream(ss);
      finally
        ss.Free;
      end;
    end
    else if Value = '' then
      IsNull := True
    else if (stype = SQL_TIMESTAMP) or (stype = SQL_TYPE_DATE) or
      (stype = SQL_TYPE_TIME) then
      SetAsDateTime(StrToDateTime(Value))
    else
      SetStringValue;
  end;
end;

procedure TMDOXSQLVAR.SetAsTime(Value: TDateTime);
var
  i: Integer;
  tm_date: TCTimeStructure;
  Hr, Mt, S, Ms: Word;
  xvar: TMDOXSQLVAR;
begin
  if FSQL.Database.SQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_TYPE_TIME or (xvar.FXSQLVAR^.sqltype and 1);
      DecodeTime(Value, Hr, Mt, S, Ms);
      with tm_date do
      begin
        tm_sec := S;
        tm_min := Mt;
        tm_hour := Hr;
        tm_mday := 0;
        tm_mon := 0;
        tm_year := 0;
      end;
      xvar.FXSQLVAR^.sqllen := SizeOf(ISC_TIME);
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      isc_encode_sql_time(@tm_date, PISC_TIME(xvar.FXSQLVAR^.sqldata));
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetAsVariant(Value: Variant);
begin
  if VarIsNull(Value) then
    IsNull := True
  else case VarType(Value) of
    varEmpty, varNull:
      IsNull := True;
    varSmallint, varInteger, varByte:
      AsLong := Value;
    varSingle, varDouble:
      AsDouble := Value;
    varCurrency:
      AsCurrency := Value;
    varBoolean:
      if Value then
        AsLong := ISC_TRUE
      else
        AsLong := ISC_FALSE;
    varDate:
      AsDateTime := Value;
    varOleStr, varString:
      AsString := Value;
    varArray:
      MDOError(mdoeNotSupported, [nil]);
    varByRef, varDispatch, varError, varUnknown, varVariant:
      MDOError(mdoeNotPermitted, [nil]);
  end;
end;

procedure TMDOXSQLVAR.SetAsXSQLVAR(Value: PXSQLVAR);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
  sqlind: PShort;
  sqldata: PChar;
  local_sqllen: Integer;
begin
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      sqlind := xvar.FXSQLVAR^.sqlind;
      sqldata := xvar.FXSQLVAR^.sqldata;
      Move(Value^, xvar.FXSQLVAR^, SizeOf(TXSQLVAR));
      xvar.FXSQLVAR^.sqlind := sqlind;
      xvar.FXSQLVAR^.sqldata := sqldata;
      if (Value^.sqltype and 1 = 1) then
      begin
        if (xvar.FXSQLVAR^.sqlind = nil) then
          MDOAlloc(xvar.FXSQLVAR^.sqlind, 0, SizeOf(Short));
        xvar.FXSQLVAR^.sqlind^ := Value^.sqlind^;
      end
      else
        if (xvar.FXSQLVAR^.sqlind <> nil) then
          ReallocMem(xvar.FXSQLVAR^.sqlind, 0);
      if ((xvar.FXSQLVAR^.sqltype and (not 1)) = SQL_VARYING) then
        local_sqllen := xvar.FXSQLVAR^.sqllen + 2
      else
        local_sqllen := xvar.FXSQLVAR^.sqllen;
      FXSQLVAR^.sqlscale := Value^.sqlscale;
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, local_sqllen);
      Move(Value^.sqldata[0], xvar.FXSQLVAR^.sqldata[0], local_sqllen);
      xvar.FModified := True;
    end;
end;

procedure TMDOXSQLVAR.SetIsNull(Value: Boolean);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  if Value then
  begin
    if not IsNullable then
      IsNullable := True;
    for i := 0 to FParent.FCount - 1 do
      if FParent.FNames[i] = FName then
      begin
        xvar := FParent[i];
        if Assigned(xvar.FXSQLVAR^.sqlind) then
          xvar.FXSQLVAR^.sqlind^ := -1;
        xvar.FModified := True;
      end;
  end
  else
    if ((not Value) and IsNullable) then
    begin
      for i := 0 to FParent.FCount - 1 do
        if FParent.FNames[i] = FName then
        begin
          xvar := FParent[i];
          if Assigned(xvar.FXSQLVAR^.sqlind) then
            xvar.FXSQLVAR^.sqlind^ := 0;
          xvar.FModified := True;
        end;
    end;
end;

procedure TMDOXSQLVAR.SetIsNullable(Value: Boolean);
var
  i: Integer;
  xvar: TMDOXSQLVAR;
begin
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      if (Value <> IsNullable) then
      begin
        if Value then
        begin
          xvar.FXSQLVAR^.sqltype := xvar.FXSQLVAR^.sqltype or 1;
          MDOAlloc(xvar.FXSQLVAR^.sqlind, 0, SizeOf(Short));
        end
        else
        begin
          xvar.FXSQLVAR^.sqltype := xvar.FXSQLVAR^.sqltype and (not 1);
          ReallocMem(xvar.FXSQLVAR^.sqlind, 0);
        end;
      end;
    end;
end;


{ TMDOXSQLDA }
{
********************************** TMDOXSQLDA **********************************
}
constructor TMDOXSQLDA.Create(Query: TMDOSQL);
begin
  inherited Create;
  FSQL := Query;
  FNames := TStringList.Create;
  FSize := 0;
  FUniqueRelationName := '';
end;

destructor TMDOXSQLDA.Destroy;
var
  i: Integer;
begin
  FNames.Free;
  if FXSQLDA <> nil then
  begin
    for i := 0 to FSize - 1 do
    begin
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqldata);
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqlind);
      FXSQLVARs[i].Free ;
    end;
    FreeMem(FXSQLDA);
    FXSQLDA := nil;
    FXSQLVARs := nil;
  end;
  inherited Destroy;
end;

procedure TMDOXSQLDA.AddName(FieldName: String; Idx: Integer);
var
  fn: string;
begin
  fn := FormatIdentifierValue(FSQL.Database.SQLDialect, FieldName);
  while FNames.Count <= Idx do
    FNames.Add('');
  FNames[Idx] := fn;
  FXSQLVARs[Idx].FName := fn;
  FXSQLVARs[Idx].FIndex := Idx;
end;

function TMDOXSQLDA.ByName(Idx: String): TMDOXSQLVAR;
begin
  result := GetXSQLVARByName(Idx);
  if result = nil then
    MDOError(mdoeFieldNotFound, [Idx]);
end;

function TMDOXSQLDA.GetModified: Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to FCount - 1 do
    if FXSQLVARs[i].Modified then
    begin
      result := True;
      exit;
    end;
end;

function TMDOXSQLDA.GetNames: string;
begin
  result := FNames.Text;
end;

function TMDOXSQLDA.GetRecordSize: Integer;
begin
  result := SizeOf(TMDOXSQLDA) + XSQLDA_LENGTH(FSize);
end;

function TMDOXSQLDA.GetXSQLDA: PXSQLDA;
begin
  result := FXSQLDA;
end;

function TMDOXSQLDA.GetXSQLVAR(Idx: Integer): TMDOXSQLVAR;
begin
  if (Idx < 0) or (Idx >= FCount) then
    MDOError(mdoeXSQLDAIndexOutOfRange, [nil]);
  result := FXSQLVARs[Idx]
end;

function TMDOXSQLDA.GetXSQLVARByName(Idx: String): TMDOXSQLVAR;
var
  s: string;
  i, Cnt: Integer;
begin
  s := FormatIdentifierValue(FSQL.Database.SQLDialect, Idx);
  i := 0;
  Cnt := FNames.Count;
  while (i < Cnt) and (FNames[i] <> s) do Inc(i);
  if i = Cnt then
    result := nil
  else
    result := GetXSQLVAR(i);
end;

procedure TMDOXSQLDA.Initialize;
var
  i, j, j_len: Integer;
  NamesWereEmpty: Boolean;
  st: string;
  bUnique: Boolean;
begin
  bUnique := True;
  NamesWereEmpty := (FNames.Count = 0);
  if FXSQLDA <> nil then
  begin
    for i := 0 to FCount - 1 do
    begin
      with FXSQLVARs[i].Data^ do
      begin
        if bUnique and (String(relname) <> '') then
        begin
          if FUniqueRelationName = '' then
            FUniqueRelationName := String(relname)
          else
            if String(relname) <> FUniqueRelationName then
            begin
              FUniqueRelationName := '';
              bUnique := False;
            end;
        end;
        if NamesWereEmpty then
        begin
          st := String(aliasname);
          if st = '' then
          begin
            st := 'F_'; {do not localize}
            aliasname_length := 2;
            j := 1; j_len := 1;
            StrPCopy(aliasname, st + IntToStr(j));
          end
          else
          begin
            StrPCopy(aliasname, st);
            j := 0; j_len := 0;
          end;
          while GetXSQLVARByName(String(aliasname)) <> nil do
          begin
            Inc(j); j_len := Length(IntToStr(j));
            if j_len + aliasname_length > 31 then
              StrPCopy(aliasname,
                       Copy(st, 1, 31 - j_len) +
                       IntToStr(j))
            else
              StrPCopy(aliasname, st + IntToStr(j));
          end;
          Inc(aliasname_length, j_len);
          AddName(String(aliasname), i);
        end;
        case sqltype and (not 1) of
          SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
          SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
          SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT, SQL_BOOLEAN: begin
            if (sqllen = 0) then
              { Make sure you get a valid pointer anyway
               select '' from foo }
              MDOAlloc(sqldata, 0, 1)
            else
              MDOAlloc(sqldata, 0, sqllen)
          end;
          SQL_VARYING: begin
            MDOAlloc(sqldata, 0, sqllen + 2);
          end;
          else
            MDOError(mdoeUnknownSQLDataType, [sqltype and (not 1)])
        end;
        if (sqltype and 1 = 1) then
          MDOAlloc(sqlind, 0, SizeOf(Short))
        else
          if (sqlind <> nil) then
            ReallocMem(sqlind, 0);
      end;
    end;
  end;
end;

procedure TMDOXSQLDA.SetCount(Value: Integer);
var
  i, OldSize: Integer;
  p: PXSQLVAR;
begin
  FNames.Clear;
  FCount := Value;
  if FCount = 0 then
    FUniqueRelationName := ''
  else
  begin
    if FSize > 0 then
      OldSize := XSQLDA_LENGTH(FSize)
    else
      OldSize := 0;
    if FCount > FSize then
    begin
      MDOAlloc(FXSQLDA, OldSize, XSQLDA_LENGTH(FCount));
      SetLength(FXSQLVARs, FCount);
      FXSQLDA^.version := SQLDA_VERSION1;
      p := @FXSQLDA^.sqlvar[0];
      for i := 0 to FCount - 1 do
      begin
        if i >= FSize then
          FXSQLVARs[i] := TMDOXSQLVAR.Create(self, FSQL);
        FXSQLVARs[i].FXSQLVAR := p;
        p := Pointer(PChar(p) + sizeof(FXSQLDA^.sqlvar));
      end;
      FSize := FCount;
    end;
    if FSize > 0 then
    begin
      FXSQLDA^.sqln := Value;
      FXSQLDA^.sqld := Value;
    end;
  end;
end;

{ TMDOOutputDelimitedFile }

{
*************************** TMDOOutputDelimitedFile ****************************
}
destructor TMDOOutputDelimitedFile.Destroy;
begin
  if FHandle <> 0 then
  begin
    FlushFileBuffers(FHandle);
    CloseHandle(FHandle);
  end;
  inherited Destroy;
end;

procedure TMDOOutputDelimitedFile.ReadyFile;
var
  i: Integer;
  BytesWritten: DWORD;
  st: string;
begin
  if FColDelimiter = '' then
    FColDelimiter := TAB;
  if FRowDelimiter = '' then
    FRowDelimiter := CRLF;
  FHandle := CreateFile(PChar(Filename), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                        FILE_ATTRIBUTE_NORMAL, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := 0;
  if FOutputTitles then
  begin
    for i := 0 to Columns.Count - 1 do
      if i = 0 then
        st := string(Columns[i].Data^.aliasname)
      else
        st := st + FColDelimiter + string(Columns[i].Data^.aliasname);
    st := st + FRowDelimiter;
    WriteFile(FHandle, st[1], Length(st), BytesWritten, nil);
  end;
end;

function TMDOOutputDelimitedFile.WriteColumns: Boolean;
var
  i: Integer;
  BytesWritten: DWORD;
  st: string;
begin
  result := False;
  if FHandle <> 0 then
  begin
    st := '';
    for i := 0 to Columns.Count - 1 do
    begin
      if i > 0 then
        st := st + FColDelimiter;
      st := st + StripString(Columns[i].AsString, FColDelimiter + FRowDelimiter);
    end;
    st := st + FRowDelimiter;
    WriteFile(FHandle, st[1], Length(st), BytesWritten, nil);
    if BytesWritten = DWORD(Length(st)) then
      result := True;
  end
end;

 { TMDOInputDelimitedFile }

{
**************************** TMDOInputDelimitedFile ****************************
}
destructor TMDOInputDelimitedFile.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

function TMDOInputDelimitedFile.GetColumn(var Col: string): Integer;
var
  c: Char;
  BytesRead: Integer;
  
  procedure ReadInput;
  begin
    if FLookAhead <> NULL_TERMINATOR then
    begin
      c := FLookAhead;
      BytesRead := 1;
      FLookAhead := NULL_TERMINATOR;
    end else
      BytesRead := FFile.Read(c, 1);
  end;
  
  procedure CheckCRLF(Delimiter: string);
  begin
    if (c = CR) and (Pos(LF, Delimiter) > 0) then {mbcs ok}
    begin
      BytesRead := FFile.Read(c, 1);
      if (BytesRead = 1) and (c <> #10) then
        FLookAhead := c
    end;
  end;
  
begin
  Col := '';
  result := 0;
  ReadInput;
  while BytesRead <> 0 do
  begin
    if Pos(c, FColDelimiter) > 0 then {mbcs ok}
    begin
      CheckCRLF(FColDelimiter);
      result := 1;
      break;
    end else if Pos(c, FRowDelimiter) > 0 then {mbcs ok}
    begin
      CheckCRLF(FRowDelimiter);
      result := 2;
      break;
    end else
      Col := Col + c;
    ReadInput;
  end;
end;

function TMDOInputDelimitedFile.ReadParameters: Boolean;
var
  i, curcol: Integer;
  Col: string;
begin
  result := False;
  if not FEOF then
  begin
    curcol := 0;
    repeat
      i := GetColumn(Col);
      if (i = 0) then
        FEOF := True;
      if (curcol < Params.Count) then
      begin
        try
          if (Col = '') and
             (ReadBlanksAsNull) then
            Params[curcol].IsNull := True
          else
            Params[curcol].AsString := Col;
          Inc(curcol);
        except
          on E: Exception do
          begin
            if not (FEOF and (curcol = Params.Count)) then
              raise;
          end;
        end;
      end;
    until (FEOF) or (i = 2);
    result := ((FEOF) and (curcol = Params.Count)) or
              (not FEOF);
  end;
end;

procedure TMDOInputDelimitedFile.ReadyFile;
begin
  if FColDelimiter = '' then
    FColDelimiter := TAB;
  if FRowDelimiter = '' then
    FRowDelimiter := CRLF;
  FLookAhead := NULL_TERMINATOR;
  FEOF := False;
  if FFile <> nil then
    FFile.Free;
  FFile := TFileStream.Create(FFilename, fmOpenRead or fmShareDenyWrite);
  if FSkipTitles then
    ReadParameters;
end;

{ TMDOOutputRawFile }
{
****************************** TMDOOutputRawFile *******************************
}
destructor TMDOOutputRawFile.Destroy;
begin
  if FHandle <> 0 then
  begin
    FlushFileBuffers(FHandle);
    CloseHandle(FHandle);
  end;
  inherited Destroy;
end;

procedure TMDOOutputRawFile.ReadyFile;
begin
  FHandle := CreateFile(PChar(Filename), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                        FILE_ATTRIBUTE_NORMAL, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := 0;
end;

function TMDOOutputRawFile.WriteColumns: Boolean;
var
  i: Integer;
  BytesWritten: DWord;
begin
  result := False;
  if FHandle <> 0 then
  begin
    for i := 0 to Columns.Count - 1 do
    begin
      WriteFile(FHandle, Columns[i].Data^.sqldata^, Columns[i].Data^.sqllen,
                BytesWritten, nil);
      if BytesWritten <> DWORD(Columns[i].Data^.sqllen) then
        exit;
    end;
    result := True;
  end;
end;

{ TMDOInputRawFile }
{
******************************* TMDOInputRawFile *******************************
}
destructor TMDOInputRawFile.Destroy;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);
  inherited Destroy;
end;

function TMDOInputRawFile.ReadParameters: Boolean;
var
  i: Integer;
  BytesRead: DWord;
begin
  result := False;
  if FHandle <> 0 then
  begin
    for i := 0 to Params.Count - 1 do
    begin
      ReadFile(FHandle, Params[i].Data^.sqldata^, Params[i].Data^.sqllen,
               BytesRead, nil);
      if BytesRead <> DWORD(Params[i].Data^.sqllen) then
        exit;
    end;
    result := True;
  end;
end;

procedure TMDOInputRawFile.ReadyFile;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := CreateFile(PChar(Filename), GENERIC_READ, 0, nil, OPEN_EXISTING,
                        FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := 0;
end;

{ TMDOSQL }
{
*********************************** TMDOSQL ************************************
}
constructor TMDOSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFBLoaded := False;
  CheckFBLoaded;
  FFBLoaded := True;
  FGenerateParamNames := False;
  FGoToFirstRecordOnExecute := True;
  FBase := TMDOBase.Create(Self);
  FBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
  FBase.BeforeTransactionEnd := BeforeTransactionEnd;
  FBOF := False;
  FEOF := False;
  FPrepared := False;
  FRecordCount := 0;
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChanging := SQLChanging;
  FProcessedSQL := TStringList.Create;
  FHandle := nil;
  FSQLParams := TMDOXSQLDA.Create(self);
  FSQLRecord := TMDOXSQLDA.Create(self);
  FSQLType := SQLUnknown;
  FParamCheck := True;
  // Removed the code FCursor set name from the TMDOSQL.Create.
  // This is needed because if you create the MDOSQL dynamically, the name is
  // not assigned in the create and the cursor is only based on a 'random'
  // number. And, in some special situation, duplicate cursornames will appear
  //FCursor := Name + RandomString(8);
  if AOwner is TMDODatabase then
    Database := TMDODatabase(AOwner)
  else
    if AOwner is TMDOTransaction then
      Transaction := TMDOTransaction(AOwner);
end;

destructor TMDOSQL.Destroy;
begin
  if FFBLoaded then
  begin
    if (FOpen) then
      Close;
    if (FHandle <> nil) then
      FreeHandle;
    FSQL.Free;
    FProcessedSQL.Free;
    FBase.Free;
    FSQLParams.Free;
    FSQLRecord.Free;
  end;
  inherited Destroy;
end;

procedure TMDOSQL.BatchInput(InputObject: TMDOBatchInput);
begin
  if not Prepared then
    Prepare;
  InputObject.FParams := Self.FSQLParams;
  InputObject.ReadyFile;
  if FSQLType in [SQLInsert, SQLUpdate, SQLDelete, SQLExecProcedure] then
    while InputObject.ReadParameters do
      ExecQuery;
end;

procedure TMDOSQL.BatchOutput(OutputObject: TMDOBatchOutput);
begin
  CheckClosed;
  if not Prepared then
    Prepare;
  if (FSQLType in [SQLSelect, SQLSelectForUpdate]) then
  begin
    try
      ExecQuery;
      OutputObject.FColumns := Self.FSQLRecord;
      OutputObject.ReadyFile;
      if not FGoToFirstRecordOnExecute then
        Next;
      while (not Eof) and (OutputObject.WriteColumns) do
        Next;
    finally
      Close;
    end;
  end;
end;

procedure TMDOSQL.BeforeTransactionEnd(Sender: TObject);
begin
  if (FOpen) then
    Close;
end;

function TMDOSQL.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
var
  sqlcode: Long;
  MDOErrorCode: Long;
  local_buffer: array[0..MDOHugeLocalBufferLength - 1] of char;
  usr_msg: string;
  status_vector: PISC_STATUS;
  MDODataBaseErrorMessages: TMDODataBaseErrorMessages;
  path: String;
begin
   result := 0;
  if Transaction <> nil then
    result := Transaction.Call(ErrCode, RaiseError)
  else if RaiseError and (ErrCode > 0) then
     MDODatabaseError;
end;

procedure TMDOSQL.CheckClosed;
begin
  if FOpen then MDOError(mdoeSQLOpen, [nil]);
end;

procedure TMDOSQL.CheckOpen;
begin
  if not FOpen then MDOError(mdoeSQLClosed, [nil]);
end;

procedure TMDOSQL.CheckValidStatement;
begin
  FBase.CheckTransaction;
  if (FHandle = nil) then
    MDOError(mdoeInvalidStatementHandle, [nil]);
end;

procedure TMDOSQL.Close;
var
  isc_res: ISC_STATUS;
begin
  try
    if (FHandle <> nil) and (SQLType in [SQLSelect, SQLSelectForUpdate]) and FOpen then
    begin
      isc_res := Call(
                   isc_dsql_free_statement(StatusVector, @FHandle, DSQL_close),
                   False);
      if (StatusVector^ = 1) and (isc_res > 0) and
        not CheckStatusVector(
              [isc_bad_stmt_handle, isc_dsql_cursor_close_err]) then
        MDODatabaseError;
    end;
  finally
    FEOF := False;
    FBOF := False;
    FOpen := False;
    FRecordCount := 0;
  end;
end;

function TMDOSQL.Current: TMDOXSQLDA;
begin
  result := FSQLRecord;
end;

procedure TMDOSQL.DoBeforeDatabaseDisconnect(Sender: TObject);
begin
  if (FHandle <> nil) then
  begin
    Close;
    FreeHandle;
  end;
end;

procedure TMDOSQL.ExecQuery;
var
  fetch_res: ISC_STATUS;
begin
  CheckClosed;
  if not Prepared then
    Prepare;
  CheckValidStatement;
  if (FBase.Transaction.AutoCommit) and
    not (FBase.Transaction.InTransaction) then
  begin
    FBase.Transaction.StartTransaction;
  end;
  
  try
    case FSQLType of
      SQLSelect, SQLSelectForUpdate: begin
        Call(isc_dsql_execute2(StatusVector,
                              TRHandle,
                              @FHandle,
                              Database.SQLDialect,
                              FSQLParams.AsXSQLDA,
                              nil), True);
        Call(
          isc_dsql_set_cursor_name(StatusVector, @FHandle, PChar(FCursor), 0),
          True);
        FOpen := True;
        FBOF := True;
        FEOF := False;
        FRecordCount := 0;
        if FGoToFirstRecordOnExecute then
          Next;
      end;
      SQLExecProcedure: begin
        fetch_res := Call(isc_dsql_execute2(StatusVector,
                              TRHandle,
                              @FHandle,
                              Database.SQLDialect,
                              FSQLParams.AsXSQLDA,
                              FSQLRecord.AsXSQLDA), False);
        if (fetch_res <> 0) and (fetch_res <> isc_deadlock) then
        begin
           { Sometimes a prepared stored procedure appears to get
             off sync on the server ....This code is meant to try
             to work around the problem simply by "retrying". This
             need to be reproduced and fixed.
           }
          isc_dsql_prepare(StatusVector, TRHandle, @FHandle, 0,
                           PChar(FProcessedSQL.Text), 1, nil);
          Call(isc_dsql_execute2(StatusVector,
                              TRHandle,
                              @FHandle,
                              Database.SQLDialect,
                              FSQLParams.AsXSQLDA,
                              FSQLRecord.AsXSQLDA), True);
        end;
      end
      else
        Call(isc_dsql_execute(StatusVector,
                             TRHandle,
                             @FHandle,
                             Database.SQLDialect,
                             FSQLParams.AsXSQLDA), True)
    end;
  finally
    if (FBase.Transaction.AutoCommit) and
      not(SQLType in [SQLSelect, SQLSelectForUpdate]) then
      try
        FBase.Transaction.CommitRetaining;
      except
        FBase.Transaction.RollbackRetaining;
        raise;
      end;
  end;
  if not (csDesigning in ComponentState) then
    MonitorHook.SQLExecute(Self);
end;

function TMDOSQL.FieldByName(FieldName: String): TMDOXSQLVAR;
var
  i: Integer;
begin
  i := GetFieldIndex(FieldName);
  if (i < 0) then
    MDOError(mdoeFieldNotFound, [FieldName]);
  result := GetFields(i);
end;

procedure TMDOSQL.FreeHandle;
var
  isc_res: ISC_STATUS;
begin
  try
    { The following two lines merely set the SQLDA count
     variable FCount to 0, but do not deallocate
     That way the allocations can be reused for
     a new query sring in the same SQL instance }
    FSQLRecord.Count := 0;
    FSQLParams.Count := 0;
    if FHandle <> nil then
    begin
      isc_res :=
        Call(isc_dsql_free_statement(StatusVector, @FHandle, DSQL_drop), False);
      if (StatusVector^ = 1) and (isc_res > 0) and (isc_res <> isc_bad_stmt_handle) then
        MDODatabaseError;
    end;
  finally
    FPrepared := False;
    FHandle := nil;
  end;
end;

function TMDOSQL.GetDatabase: TMDODatabase;
begin
  result := FBase.Database;
end;

function TMDOSQL.GetDBHandle: PISC_DB_HANDLE;
begin
  result := FBase.DBHandle;
end;

function TMDOSQL.GetEof: Boolean;
begin
  result := FEOF or not FOpen;
end;

function TMDOSQL.GetFieldIndex(FieldName: String): Integer;
begin
  if (FSQLRecord.GetXSQLVarByName(FieldName) = nil) then
    result := -1
  else
    result := FSQLRecord.GetXSQLVarByName(FieldName).Index;
end;

function TMDOSQL.GetFields(const Idx: Integer): TMDOXSQLVAR;
begin
  if (Idx < 0) or (Idx >= FSQLRecord.Count) then
    MDOError(mdoeFieldNotFound, [IntToStr(Idx)]);
  result := FSQLRecord[Idx];
end;

function TMDOSQL.GetPlan: string;
var
  result_buffer: array[0..16384] of Char;
  result_length, i: Integer;
  info_request: Char;
begin
  if (not Prepared) or
     (not (FSQLType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else begin
    info_request := Char(isc_info_sql_get_plan);
    Call(isc_dsql_sql_info(StatusVector, @FHandle, 2, @info_request,
                           SizeOf(result_buffer), result_buffer), True);
    if (result_buffer[0] <> Char(isc_info_sql_get_plan)) then
      MDOError(mdoeUnknownError, [nil]);
    result_length := isc_vax_integer(@result_buffer[1], 2);
    SetString(result, nil, result_length);
    for i := 1 to result_length do
      result[i] := result_buffer[i + 2];
    result := Trim(result);
  end;
end;

function TMDOSQL.GetRecordCount: Integer;
begin
  result := FRecordCount;
end;

function TMDOSQL.GetRowsAffected: Integer;
var
  result_buffer: array[0..1048] of Char;
  info_request: Char;
begin
  if not Prepared then
    result := -1
  else begin
    info_request := Char(isc_info_sql_records);
    if isc_dsql_sql_info(StatusVector, @FHandle, 1, @info_request,
                         SizeOf(result_buffer), result_buffer) > 0 then
      MDODatabaseError;
    if (result_buffer[0] <> Char(isc_info_sql_records)) then
      result := -1
    else
    case SQLType of
    SQLUpdate:   Result := isc_vax_integer(@result_buffer[6], 4);
    SQLDelete:   Result := isc_vax_integer(@result_buffer[13], 4);
    SQLInsert:   Result := isc_vax_integer(@result_buffer[27], 4);
    else         Result := -1 ;
    end ;
  end;
end;

function TMDOSQL.GetSQLParams: TMDOXSQLDA;
begin
  if not Prepared then
    Prepare;
  result := FSQLParams;
end;

function TMDOSQL.GetTransaction: TMDOTransaction;
begin
  result := FBase.Transaction;
end;

function TMDOSQL.GetTRHandle: PISC_TR_HANDLE;
begin
  result := FBase.TRHandle;
end;

function TMDOSQL.GetUniqueRelationName: string;
begin
  if FPrepared and (SQLType in [SQLSelect, SQLSelectForUpdate]) then
    result := FSQLRecord.UniqueRelationName
  else
    result := '';
end;

function TMDOSQL.Next: TMDOXSQLDA;
var
  fetch_res: ISC_STATUS;
begin
  result := nil;
  if not FEOF then
  begin
    CheckOpen;
    { Go to the next record... }
    fetch_res :=
      Call(isc_dsql_fetch(StatusVector, @FHandle, Database.SQLDialect, FSQLRecord.AsXSQLDA), False);
    if (fetch_res = 100) or (CheckStatusVector([isc_dsql_cursor_err])) then
    begin
      FEOF := True;
    end else if (fetch_res > 0) then
    begin
      try
        MDODatabaseError;
      except
        Close;
        raise;
      end;
    end else begin
      Inc(FRecordCount);
      FBOF := False;
      result := FSQLRecord;
    end;
    if not (csDesigning in ComponentState) then
      MonitorHook.SQLFetch(Self);
  end;
end;

procedure TMDOSQL.Prepare;
var
  stmt_len: Integer;
  res_buffer: array[0..7] of Char;
  type_item: Char;
begin
  if FCursor = '' then
    FCursor := GetCursorName;
  CheckClosed;
  FBase.CheckDatabase;
  FBase.CheckTransaction;
  if FPrepared then
    exit;
  if (FSQL.Text = '') then
    MDOError(mdoeEmptyQuery, [nil]);
  if not ParamCheck then
    FProcessedSQL.Text := FSQL.Text
  else
    PreprocessSQL;
  if (FProcessedSQL.Text = '') then
    MDOError(mdoeEmptyQuery, [nil]);
  try
    Call(isc_dsql_alloc_statement2(StatusVector, DBHandle,
                                    @FHandle), True);
    Call(isc_dsql_prepare(StatusVector, TRHandle, @FHandle, 0,
               PChar(FProcessedSQL.Text), Database.SQLDialect, nil), True);
    { After preparing the statement, query the stmt type and possibly
      create a FSQLRecord "holder" }
    { Get the type of the statement }
    type_item := Char(isc_info_sql_stmt_type);
    Call(isc_dsql_sql_info(StatusVector, @FHandle, 1, @type_item,
                         SizeOf(res_buffer), res_buffer), True);
    if (res_buffer[0] <> Char(isc_info_sql_stmt_type)) then
      MDOError(mdoeUnknownError, [nil]);
    stmt_len := isc_vax_integer(@res_buffer[1], 2);
    FSQLType := TMDOSQLTypes(isc_vax_integer(@res_buffer[3], stmt_len));
    { Done getting the type }
    case FSQLType of
      SQLGetSegment,
      SQLPutSegment,
      SQLStartTransaction: begin
        FreeHandle;
        MDOError(mdoeNotPermitted, [nil]);
      end;
      SQLCommit,
      SQLRollback,
      SQLDDL, SQLSetGenerator,
      SQLInsert, SQLUpdate, SQLDelete, SQLSelect, SQLSelectForUpdate,
      SQLExecProcedure: begin
        { We already know how many inputs there are, so... }
        if (FSQLParams.FXSQLDA <> nil) and
           (Call(isc_dsql_describe_bind(StatusVector, @FHandle, Database.SQLDialect,
                                        FSQLParams.FXSQLDA), False) > 0) then
          MDODatabaseError;
        FSQLParams.Initialize;
        if FSQLType in [SQLSelect, SQLSelectForUpdate,
                        SQLExecProcedure] then
        begin
          { Allocate an initial output descriptor (with one column) }
          FSQLRecord.Count := 1;
          { Using isc_dsql_describe, get the right size for the columns... }
          Call(isc_dsql_describe(StatusVector, @FHandle, Database.SQLDialect, FSQLRecord.FXSQLDA), True);
          if FSQLRecord.FXSQLDA^.sqld > FSQLRecord.FXSQLDA^.sqln then
          begin
            FSQLRecord.Count := FSQLRecord.FXSQLDA^.sqld;
            Call(isc_dsql_describe(StatusVector, @FHandle, Database.SQLDialect, FSQLRecord.FXSQLDA), True);
          end else if FSQLRecord.FXSQLDA^.sqld = 0 then
            FSQLRecord.Count := 0;
          FSQLRecord.Initialize;
        end;
      end;
    end;
    FPrepared := True;
    if not (csDesigning in ComponentState) then
      MonitorHook.SQLPrepare(Self);
  except
    on E: Exception do
    begin
      if (FHandle <> nil) then
        FreeHandle;
      raise;
    end;
  end;
end;

procedure TMDOSQL.PreprocessSQL;
var
  cCurChar, cNextChar, cQuoteChar: Char;
  sSQL, sProcessedSQL, sParamName: string;
  i, iLenSQL, iSQLPos: Integer;
  iCurState, iCurParamState: Integer;
  iParamSuffix: Integer;
  slNames: TStrings;
  
  const
    DefaultState = 0;
    CommentState = 1;
    QuoteState = 2;
    ParamState = 3;
    ParamDefaultState = 0;
    ParamQuoteState = 1;
  
    procedure AddToProcessedSQL(cChar: Char);
    begin
      sProcessedSQL[iSQLPos] := cChar;
      Inc(iSQLPos);
    end;

  function WordChange(Texto, substring, trocarpor: string): string;
  var
    i: integer;

    function SeExiste(const substring, em: string): boolean;
    var
      i: integer;
      s1, s2: string;
    begin
      Result := false;
      s1 := UpperCase(substring);
      s2 := UpperCase(em);
      for i := 1 to Length(s2) do
        if Copy(s2, i, Length(s1)) = s1 then
        begin
          Result := true;
          Break;
        end;
    end;
  begin
    if SeExiste(substring, Texto) then
    begin
      i := 1;
      while i <= Length(Texto) do
      begin
        if UpperCase(Copy(Texto, i, Length(substring))) = UpperCase(substring) then
        begin
          Delete(Texto, i, Length(substring));
          Insert(trocarpor, Texto, i);
          i := 0;
        end;
        Result := Texto;
        Inc(i);
      end;
    end
    else
      Result := Texto;
  end;
  
begin
  slNames := TStringList.Create;
  try
    { Do some initializations of variables }
    iParamSuffix := 0;
    cQuoteChar := '''';
    sSQL := FSQL.Text;
    iLenSQL := Length(sSQL);
    SetString(sProcessedSQL, nil, iLenSQL + 1);
    i := 1;
    iSQLPos := 1;
    iCurState := DefaultState;
    iCurParamState := ParamDefaultState;
    { Now, traverse through the SQL string, character by character,
     picking out the parameters and formatting correctly for Firebird }
    while (i <= iLenSQL) do
    begin
      { Get the current token and a look-ahead }
      cCurChar := sSQL[i];
      if i = iLenSQL then
        cNextChar := #0
      else
        cNextChar := sSQL[i + 1];
      { Now act based on the current state }
      case iCurState of
        DefaultState: begin
          case cCurChar of
            '''', '"':
            begin
              cQuoteChar := cCurChar;
              iCurState := QuoteState;
            end;
            '?', ':':
            begin
              iCurState := ParamState;
              AddToProcessedSQL('?');
            end;
            '/':
            if (cNextChar = '*') then
            begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
              iCurState := CommentState;
            end;
          end;
        end;
        CommentState: begin
          if (cNextChar = #0) then
            MDOError(mdoeSQLParseError, [SEOFInComment])
          else if (cCurChar = '*') then
          begin
            if (cNextChar = '/') then
              iCurState := DefaultState;
          end;
        end;
        QuoteState: begin
          if cNextChar = #0 then
            MDOError(mdoeSQLParseError, [SEOFInString])
          else if (cCurChar = cQuoteChar) then
          begin
            if (cNextChar = cQuoteChar) then
            begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
            end else
              iCurState := DefaultState;
          end;
        end;
        ParamState:
        begin
          { collect the name of the parameter }
          if iCurParamState = ParamDefaultState then
          begin
            if cCurChar = '"' then
              iCurParamState := ParamQuoteState
            else if (cCurChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) then
                sParamName := sParamName + cCurChar
            else if FGenerateParamNames then
            begin
              sParamName := 'MDOParam' + IntToStr(iParamSuffix); {do not localize}
              Inc(iParamSuffix);
              iCurState := DefaultState;
              slNames.Add(sParamName);
              sParamName := '';
            end
            else
              MDOError(mdoeSQLParseError, [SParamNameExpected]);
          end
          else
          begin
            { determine if Quoted parameter name is finished }
            if cCurChar = '"' then
            begin
              Inc(i);
              slNames.Add(sParamName);
              SParamName := '';
              iCurParamState := ParamDefaultState;
              iCurState := DefaultState;
            end
            else
              sParamName := sParamName + cCurChar
          end;
          { determine if the unquoted parameter name is finished }
          if (iCurParamState <> ParamQuoteState) and
            (iCurState <> DefaultState) then
          begin
            if not (cNextChar in ['A'..'Z', 'a'..'z',
                                  '0'..'9', '_', '$']) then
            begin
              Inc(i);
              iCurState := DefaultState;
              slNames.Add(sParamName);
              sParamName := '';
            end;
          end;
        end;
      end;
      if iCurState <> ParamState then
        AddToProcessedSQL(sSQL[i]);
      Inc(i);
    end;
    AddToProcessedSQL(#0);
    FSQLParams.Count := slNames.Count;
    for i := 0 to slNames.Count - 1 do
      FSQLParams.AddName(slNames[i], i);
    FProcessedSQL.Text := WordChange(sProcessedSQL, '<>', 'IS DISTINCT FROM');
    FSQL.Text := WordChange(FSQL.Text, '<>', 'IS DISTINCT FROM');
  finally
    slNames.Free;
  end;
end;

procedure TMDOSQL.SetDatabase(Value: TMDODatabase);
begin
  FBase.Database := Value;
end;

procedure TMDOSQL.SetSQL(Value: TStrings);
begin
  if FSQL.Text <> Value.Text then
  begin
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;

procedure TMDOSQL.SetTransaction(Value: TMDOTransaction);
begin
  FBase.Transaction := Value;
end;

procedure TMDOSQL.SQLChanging(Sender: TObject);
begin
  if Assigned(OnSQLChanging) then
    OnSQLChanging(Self);
  if FHandle <> nil then FreeHandle;
end;

function TMDOSQL.ParamByName(Idx: String): TMDOXSQLVAR;
begin
  if not Prepared then
    Prepare;
  result := FSQLParams.ByName(Idx);
end;


{
 Preprocess SQL
 Using FSQL, process the typed SQL and put the process SQL
 in FProcessedSQL and parameter names in FSQLParams
}

function TMDOXSQLVAR.GetAsBoolean: Boolean;
begin
  result := False;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_BOOLEAN:
        result := Boolean(PChar(FXSQLVAR^.sqldata)^);
    else
      MDOError(mdoeInvalidDataConversion, [nil]);
    end;
end;

procedure TMDOXSQLVAR.SetAsBoolean(const Value: Boolean);
var
  xvar: TMDOXSQLVAR;
  i: Integer;
begin
  if IsNullable then
    IsNull := False;
  for i := 0 to FParent.FCount - 1 do
    if FParent.FNames[i] = FName then
    begin
      xvar := FParent[i];
      xvar.FXSQLVAR^.sqltype := SQL_BOOLEAN;
      xvar.FXSQLVAR^.sqlscale := 0;
      xvar.FXSQLVAR^.sqllen := SizeOf(Boolean);
      MDOAlloc(xvar.FXSQLVAR^.sqldata, 0, xvar.FXSQLVAR^.sqllen);
      PChar(xvar.FXSQLVAR^.sqldata)^ := Char(Value);
      xvar.FModified := True;
    end;
end;

function TMDOSQL.GetCursorName: String;
begin
  if Length(IntToStr(SequentialNumber)) > 8 then
  begin
    SequentialNumber := 1;
    Inc(SequentialSerie);
  end;
  Result := IntToStr(SequentialSerie) + IntToStr(SequentialNumber) + RandomString(8);
  Inc(SequentialNumber);
end;

end.
