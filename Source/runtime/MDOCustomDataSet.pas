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


unit MDOCustomDataSet;

{$I ..\MDO.INC}

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, StdVCL, MDOExternals, MDO,
  MDOHeader, MDODatabase, MDOSQL, Db, MDOUtils, MDOBlob
  {$IFDEF MDO_DELPHI6_UP}
    , Variants, FmtBcd
  {$ENDIF};

const
  BufferCacheSize = 1000;  { Allocate cache in this many record chunks}
  UniCache = 2;     { Uni-directional cache is 2 records big }

type
  TMDOCustomDataSet = class;

  TMDODataSet = class;

  TMDODataSetUpdateObject = class(TComponent)
  private
    FRefreshSQL: TStrings;
    procedure SetRefreshSQL(Value: TStrings);
  protected
    procedure Apply(UpdateKind: TUpdateKind); virtual; abstract;
    function GetDataSet: TMDOCustomDataSet; virtual; abstract;
    function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual; abstract;
    procedure SetDataSet(ADataSet: TMDOCustomDataSet); virtual; abstract;
    property DataSet: TMDOCustomDataSet read GetDataSet write SetDataSet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RefreshSQL: TStrings read FRefreshSQL write SetRefreshSQL;
  end;

  PDateTime = ^TDateTime;

  TBlobDataArray = array[0..0] of TMDOBlobStream;

  PBlobDataArray = ^TBlobDataArray;

  { TMDOCustomDataSet }
  TFieldData = record
    fdDataType: Short;
    fdDataScale: Short;
    fdNullable: Boolean;
    fdIsNull: Boolean;
    fdDataSize: Short;
    fdDataLength: Short;
    fdDataOfs: Integer;
  end;

  PFieldData = ^TFieldData;

  TCachedUpdateStatus = (cusUnmodified, cusModified, cusInserted, cusDeleted, cusUninserted);

  TFBDBKey = record
    DBKey: array[0..7] of Byte;
  end;

  PFBDBKey = ^TFBDBKey;

  TRecordData = record
    rdBookmarkFlag: TBookmarkFlag;
    rdFieldCount: Short;
    rdRecordNumber: Long;
    rdCachedUpdateStatus: TCachedUpdateStatus;
    rdUpdateStatus: TUpdateStatus;
    rdSavedOffset: DWORD;
    rdDBKey: TFBDBKey;
    rdFields: array[1..1] of TFieldData;
  end;

  PRecordData = ^TRecordData;

  { TMDOStringField allows us to have strings longer than 8196 }

  TMDOStringField = class(TStringField)
  public
    constructor create(AOwner: TComponent); override;
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue(var Value: string): Boolean;
    procedure SetAsString(const Value: string); override;
  end;
  
  { TMDOBCDField }
  {  Actually, there is no BCD involved in this type,
     instead it deals with currency types.
     In IB, this is an encapsulation of Numeric (x, y)
     where x < 18 and y <= 4.
     Note: y > 4 will default to Floats
  }
  TMDOBCDField = class(TBCDField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 8;
  end;

  TMDODataLink = class(TDetailDataLink)
  private
    FDataSet: TMDOCustomDataSet;
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataset; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADataSet: TMDOCustomDataSet);
    destructor Destroy; override;
  end;
  
  { TMDOCustomDataSet }
  TMDOUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApply, uaApplied);

  TMDOUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError; UpdateKind: TUpdateKind; var UpdateAction: TMDOUpdateAction) of object;

  TMDOUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind; var UpdateAction: TMDOUpdateAction) of object;

  TMDOUpdateRecordTypes = set of TCachedUpdateStatus;

  TMDOGenWhereApplay = (waNewRecord, waPost, waServer);

  TMDOGeneratorLink = class(TPersistent)
  private
    FDataSet: TMDOCustomDataSet;
    FField: string;
    FGenerator: string;
    FIncrementBy: Integer;
    FWhereApply: TMDOGenWhereApplay;
  public
    constructor Create(MDODataSet: TMDOCustomDataSet);
    procedure Assign(Source: TPersistent); override;
    function GetGenValue: Int64;
    function LinksOk: Boolean;
  published
    property Field: string read FField write FField;
    property Generator: string read FGenerator write FGenerator;
    property IncrementBy: Integer read FIncrementBy write FIncrementBy default 1;
    property WhereApply: TMDOGenWhereApplay read FWhereApply write FWhereApply default waNewRecord;
  end;

  TMDOCustomDataSet = class(TDataset)
  private
    FAfterDatabaseDisconnect: TNotifyEvent;
    FAfterTransactionEnd: TNotifyEvent;
    FBase: TMDOBase;
    FBeforeDatabaseDisconnect: TNotifyEvent;
    FBeforeTransactionEnd: TNotifyEvent;
    FBEnd: DWord;
    FBlobCacheOffset: Integer;
    FBlobStreamList: TList;
    FBPos: DWord;
    FBufferCache: PChar;
    FBufferChunks: Integer;
    FBufferChunkSize: Integer;
    FCachedUpdates: Boolean;
    FCacheSize: Integer;
    FCalcFieldsOffset: Integer;
    FCurrentRecord: Long;
    FDatabaseFree: TNotifyEvent;
    FDataLink: TMDODataLink;
    FDeletedRecords: Long;
    FDidActivate: Boolean;
    FFBLoaded: Boolean;
    FFilterBuffer: PChar;
    FForcedRefresh: Boolean;
    FGeneratorLink: TMDOGeneratorLink;
    FInternalPrepared: Boolean;
    FLoadDefaults: Boolean;
    FMappedFieldPosition: array of Integer;
    FModelBuffer: PChar;
    FNeedsRefresh: Boolean;
    FOBEnd: DWord;
    FOBPos: DWord;
    FOldBuffer: PChar;
    FOldBufferCache: PChar;
    FOldCacheSize: Integer;
    FOnUpdateError: TMDOUpdateErrorEvent;
    FOnUpdateRecord: TMDOUpdateRecordEvent;
    FOpen: Boolean;
    FParamCheck: Boolean;
    FQDelete: TMDOSQL;
    FQInsert: TMDOSQL;
    FQModify: TMDOSQL;
    FQRefresh: TMDOSQL;
    FQSelect: TMDOSQL;
    FRecordBufferSize: Integer;
    FRecordCount: Integer;
    FRecordSize: Integer;
    FTransactionFree: TNotifyEvent;
    FUniDirectional: Boolean;
    FUpdateMode: TUpdateMode;
    FUpdateObject: TMDODataSetUpdateObject;
    FUpdateRecordTypes: TMDOUpdateRecordTypes;
    FUpdatesPending: Boolean;
    FCount: Integer;
    FSimpleSelectSQL: TStrings;
    FGoToDBKey: Boolean;
    FInternalDatabase: TMDODatabase;
    FInternalTransaction: TMDOTransaction;
    FOriginalDatabase: TMDODatabase;
    function AdjustCurrentRecord(Buffer: Pointer; GetMode: TGetMode): TGetResult;
    function AdjustPosition(FCache: PChar; Offset: DWORD; Origin: Integer): Integer;
    procedure AdjustRecordOnInsert(Buffer: Pointer);
    function CanDelete: Boolean;
    function CanEdit: Boolean;
    function CanInsert: Boolean;
    function CanRefresh: Boolean;
    procedure CheckEditState;
    procedure ClearBlobCache;
    procedure CopyRecordBuffer(Source, Dest: Pointer);
    procedure DoAfterDatabaseDisconnect(Sender: TObject);
    procedure DoAfterTransactionEnd(Sender: TObject);
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
    procedure DoBeforeTransactionEnd(Sender: TObject);
    procedure DoDatabaseFree(Sender: TObject);
    procedure DoTransactionFree(Sender: TObject);
    procedure FetchCurrentRecordToBuffer(Qry: TMDOSQL; RecordNumber: Integer; Buffer: PChar);
    function GetDatabase: TMDODataBase;
    function GetDBHandle: PISC_DB_HANDLE;
    function GetDeleteSQL: TStrings;
    function GetInsertSQL: TStrings;
    function GetModifySQL: TStrings;
    function GetRefreshSQL: TStrings;
    function GetSelectSQL: TStrings;
    function GetSelectStmtHandle: TISC_STMT_HANDLE;
    function GetSQLParams: TMDOXSQLDA;
    function GetStatementType: TMDOSQLTypes;
    function GetTransaction: TMDOTransaction;
    function GetTRHandle: PISC_TR_HANDLE;
    procedure InternalDeleteRecord(Qry: TMDOSQL; Buff: Pointer); virtual;
    function InternalGetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual;
    function InternalLocate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; virtual;
    procedure InternalPostRecord(Qry: TMDOSQL; Buff: Pointer); virtual;
    procedure InternalRevertRecord(RecordNumber: Integer); virtual;
    function IsVisible(Buffer: PChar): Boolean;
    procedure ReadCache(FCache: PChar; Offset: DWORD; Origin: Integer; Buffer: PChar);
    procedure ReadRecordCache(RecordNumber: Integer; Buffer: PChar; ReadOldBuffer: Boolean);
    procedure RefreshParams;
    procedure SaveOldBuffer(Buffer: PChar);
    procedure SetBufferChunks(Value: Integer);
    procedure SetDatabase(Value: TMDODataBase);
    procedure SetDeleteSQL(Value: TStrings);
    procedure SetInsertSQL(Value: TStrings);
    procedure SetInternalSQLParams(Qry: TMDOSQL; Buffer: Pointer);
    procedure SetModifySQL(Value: TStrings);
    procedure SetRefreshSQL(Value: TStrings);
    procedure SetSelectSQL(Value: TStrings);
    procedure SetTransaction(Value: TMDOTransaction);
    procedure SetUniDirectional(Value: Boolean);
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetUpdateObject(Value: TMDODataSetUpdateObject);
    procedure SetUpdateRecordTypes(Value: TMDOUpdateRecordTypes);
    procedure SQLChanging(Sender: TObject); virtual;
    procedure WriteCache(FCache: PChar; Offset: DWORD; Origin: Integer; Buffer: PChar);
    procedure WriteRecordCache(RecordNumber: Integer; Buffer: PChar);
    function InternalGetRecordCount: Integer;
    procedure SetGoToDBKey(const Value: Boolean);
  protected
    FUpdateRecordCount: Boolean;
    //function GetNextRecords: Integer; override;
    procedure CursorTransactionErrorEnter(Query: TMDOSQL); virtual;
    procedure CursorTransactionErrorExit(Query: TMDOSQL); virtual;
    procedure SetFiltered(Value: Boolean); override;
    procedure ActivateConnection;
    function ActivateTransaction: Boolean;
    function AllocRecordBuffer: PChar; override;
    procedure CheckDatasetClosed;
    procedure CheckDatasetOpen;
    procedure CheckNotUniDirectional;
    procedure ClearCalcFields(Buffer: PChar); override;
    function ConstraintsStored: Boolean;
    procedure DeactivateTransaction;
    procedure Disconnect; virtual;
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoOnNewRecord; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function GetActiveBuf: PChar;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecNo: Integer; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Integer; override;
    function GetRecordSize: Word; override;
    procedure InitRecord(Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalBatchInput(InputObject: TMDOBatchInput); virtual;
    procedure InternalBatchOutput(OutputObject: TMDOBatchOutput); virtual;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalExecQuery; virtual;
    procedure InternalFirst; override;
    function InternalGetFieldData(Field: TField; Buffer: Pointer): Boolean; virtual;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalInsert; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalPrepare; virtual;
    procedure InternalRefresh; override;
    procedure InternalRefreshRow; virtual;
    procedure InternalSetFieldData(Field: TField; Buffer: Pointer); virtual;
    procedure InternalSetParamsFromCursor; virtual;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure InternalUnPrepare; virtual;
    function IsCursorOpen: Boolean; override;
    procedure PSEndTransaction(Commit: Boolean); override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; override;
    function PSGetQuoteChar: string; override;
    function PsGetTableName: string; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    procedure PSStartTransaction; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    procedure ReQuery;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); overload; override;
    procedure SetRecNo(Value: Integer); override;
    property AfterDatabaseDisconnect: TNotifyEvent read FAfterDatabaseDisconnect write FAfterDatabaseDisconnect;
    property AfterTransactionEnd: TNotifyEvent read FAfterTransactionEnd write FAfterTransactionEnd;
    property BeforeDatabaseDisconnect: TNotifyEvent read FBeforeDatabaseDisconnect write FBeforeDatabaseDisconnect;
    property BeforeTransactionEnd: TNotifyEvent read FBeforeTransactionEnd write FBeforeTransactionEnd;
    property BufferChunks: Integer read FBufferChunks write SetBufferChunks;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates;
    property DatabaseFree: TNotifyEvent read FDatabaseFree write FDatabaseFree;
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
    property GeneratorLink: TMDOGeneratorLink read FGeneratorLink write FGeneratorLink;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property InternalPrepared: Boolean read FInternalPrepared;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property Params: TMDOXSQLDA read GetSQLParams;
    property QDelete: TMDOSQL read FQDelete;
    property QInsert: TMDOSQL read FQInsert;
    property QModify: TMDOSQL read FQModify;
    property QRefresh: TMDOSQL read FQRefresh;
    property QSelect: TMDOSQL read FQSelect;
    property RefreshSQL: TStrings read GetRefreshSQL write SetRefreshSQL;
    property SelectSQL: TStrings read GetSelectSQL write SetSelectSQL;
    property SimpleSelectSQL: TStrings read FSimpleSelectSQL write FSimpleSelectSQL;
    property SelectStmtHandle: TISC_STMT_HANDLE read GetSelectStmtHandle;
    property SQLParams: TMDOXSQLDA read GetSQLParams;
    property StatementType: TMDOSQLTypes read GetStatementType;
    property TransactionFree: TNotifyEvent read FTransactionFree write FTransactionFree;
    property UniDirectional: Boolean read FUniDirectional write SetUniDirectional default False;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode default upWhereAll;
    property GoToDBKey: Boolean read FGoToDBKey write SetGoToDBKey;
    property DataLink: TMDODataLink read FDataLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyUpdates;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CachedUpdateStatus: TCachedUpdateStatus;
    procedure CancelUpdates;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure FetchAll;
    function GetCurrentRecord(Buffer: PChar): Boolean; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; overload; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function LocateNext(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    procedure Post; override;
    procedure RecordModified(Value: Boolean);
    procedure RevertRecord;
    procedure Undelete;
    function UpdateStatus: TUpdateStatus; override;
    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
    property LoadDefaults: Boolean read FLoadDefaults write FLoadDefaults;
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
    property UpdateObject: TMDODataSetUpdateObject read FUpdateObject write SetUpdateObject;
    property UpdateRecordTypes: TMDOUpdateRecordTypes read FUpdateRecordTypes write SetUpdateRecordTypes;
    property UpdatesPending: Boolean read FUpdatesPending;
  published
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property AutoCalcFields;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property Database: TMDODataBase read GetDatabase write SetDatabase;
    property ForcedRefresh: Boolean read FForcedRefresh write FForcedRefresh default False;
    property ObjectView default False;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property OnUpdateError: TMDOUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
    property OnUpdateRecord: TMDOUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
    property Transaction: TMDOTransaction read GetTransaction write SetTransaction;
  end;

  TMDODataSet = class(TMDOCustomDataSet)
  private
    function GetPrepared: Boolean;
  protected
    procedure InternalOpen; override;
    procedure SetFiltered(Value: Boolean); override;
  public
    function ParamByName(Idx: string): TMDOXSQLVAR;
    procedure BatchInput(InputObject: TMDOBatchInput);
    procedure BatchOutput(OutputObject: TMDOBatchOutput);
    procedure ExecSQL;
    procedure Prepare;
    procedure UnPrepare;
    property Params;
    property Prepared: Boolean read GetPrepared;
    property QDelete;
    property QInsert;
    property QModify;
    property QRefresh;
    property QSelect;
    property SelectStmtHandle;
    property StatementType;
  published
    property Active;
    property AfterCancel;
    property AfterClose;
    property AfterDatabaseDisconnect;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterScroll;
    property AfterTransactionEnd;
    property AutoCalcFields;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDatabaseDisconnect;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeScroll;
    property BeforeTransactionEnd;
    property BufferChunks;
    property CachedUpdates;
    property DatabaseFree;
    property DataSource read GetDataSource write SetDataSource;
    property DeleteSQL;
    property Filtered;
    property GeneratorLink;
    property InsertSQL;
    property LoadDefaults;
    property ModifySQL;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property ParamCheck;
    property RefreshSQL;
    property SelectSQL;
    property SimpleSelectSQL;
    property TransactionFree;
    property UniDirectional;
  end;

  { TMDODSBlobStream }
  TMDODSBlobStream = class(TStream)
  protected
    FBlobStream: TMDOBlobStream;
    FField: TField;
  public
    constructor Create(AField: TField; ABlobStream: TMDOBlobStream; Mode: TBlobStreamMode);
    function Read(var Buffer; Count: Longint): LongInt; override;
    function Seek(Offset: Longint; Origin: Word): LongInt; override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): LongInt; override;
  end;

const
  DefaultFieldClasses: array[TFieldType] of TFieldClass = (nil,                { ftUnknown }
    TMDOStringField,    { ftString }
    TSmallintField,     { ftSmallint }
    TIntegerField,      { ftInteger }
    TWordField,         { ftWord }
    TBooleanField,      { ftBoolean }
    TFloatField,        { ftFloat }
    TCurrencyField,     { ftCurrency }
    TMDOBCDField,       { ftBCD }
    TDateField,         { ftDate }
    TTimeField,         { ftTime }
    TDateTimeField,     { ftDateTime }
    TBytesField,        { ftBytes }
    TVarBytesField,     { ftVarBytes }
    TAutoIncField,      { ftAutoInc }
    TBlobField,         { ftBlob }
    TMemoField,         { ftMemo }
    TGraphicField,      { ftGraphic }
    TBlobField,         { ftFmtMemo }
    TBlobField,         { ftParadoxOle }
    TBlobField,         { ftDBaseOle }
    TBlobField,         { ftTypedBinary }
    nil,                { ftCursor }
    TStringField,       { ftFixedChar }
    nil,                {TWideStringField } { ftWideString }
    TLargeIntField,     { ftLargeInt }
    TADTField,          { ftADT }
    TArrayField,        { ftArray }
    TReferenceField,    { ftReference }
    TDataSetField,     { ftDataSet }
    TBlobField,         { ftOraBlob }
    TMemoField,         { ftOraClob }
    TVariantField,      { ftVariant }
    TInterfaceField,    { ftInterface }
    TIDispatchField,    { ftIDispatch }
    TGuidField          { ftGuid }
  {$IFDEF MDO_DELPHI6_UP}
    , TDateTimeField    { ftTimeStamp }
    , TMDOBCDField      { ftFMTBcd }
  {$ENDIF}
  {$IFDEF MDO_DELPHI2006}
    , nil //ftFixedWideChar
    , nil //ftWideMemo
    , nil //ftOraTimeStamp
    , nil //ftOraInterval)
  {$ENDIF}
    );

var
  CreateProviderProc: function(DataSet: TMDOCustomDataSet): IProvider = nil;

implementation

uses
  MDOIntf, MDOQuery, Math, StrUtils;

type
  TFieldNode = class(TObject)
  protected
    COMPUTED_BLR: Boolean;
    DEFAULT_VALUE: Boolean;
    FieldName: string;
    NextField: TFieldNode;
  end;

  TRelationNode = class(TObject)
  protected
    FieldNodes: TFieldNode;
    NextRelation: TRelationNode;
    RelationName: string;
  end;
  

{ TMDOStringField}

{
******************************* TMDOStringField ********************************
}
constructor TMDOStringField.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

class procedure TMDOStringField.CheckTypeSize(Value: Integer);
begin
  { don't check string size. all sizes valid }
end;

function TMDOStringField.GetAsString: string;
begin
  if not GetValue(Result) then
    Result := '';
end;

function TMDOStringField.GetAsVariant: Variant;
var
  S: string;
begin
  if GetValue(S) then
    Result := S
  else
    Result := Null;
end;

function TMDOStringField.GetValue(var Value: string): Boolean;
var
  Buffer: PChar;
begin
  Buffer := nil;
  MDOAlloc(Buffer, 0, Size + 1);
  try
    Result := GetData(Buffer);
    if Result then
    begin
      Value := string(Buffer);
      if Transliterate and (Value <> '') then
        DataSet.Translate(PChar(Value), PChar(Value), False);
    end
  finally
    FreeMem(Buffer);
  end;
end;

procedure TMDOStringField.SetAsString(const Value: string);
var
  Buffer: PChar;
begin
  Buffer := nil;
  MDOAlloc(Buffer, 0, Size + 1);
  try
    StrLCopy(Buffer, PChar(Value), Size);
    if Transliterate then
      DataSet.Translate(Buffer, Buffer, True);
    SetData(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;

{ TMDOBCDField }

{
********************************* TMDOBCDField *********************************
}
constructor TMDOBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBCD);
  Size := 8;
end;

class procedure TMDOBCDField.CheckTypeSize(Value: Integer);
begin
  { No need to check as the base type is currency, not BCD }
end;

function TMDOBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then
    Result := 0;
end;

function TMDOBCDField.GetAsString: string;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := CurrToStr(C)
  else
    Result := '';
end;

function TMDOBCDField.GetAsVariant: Variant;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := C
  else
    Result := Null;
end;

function TMDOBCDField.GetDataSize: Integer;
begin
  Result := 8;
end;

{ TMDODataLink }

{
********************************* TMDODataLink *********************************
}
constructor TMDODataLink.Create(ADataSet: TMDOCustomDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

destructor TMDODataLink.Destroy;
begin
  FDataSet.FDataLink := nil;
  inherited Destroy;
end;

procedure TMDODataLink.ActiveChanged;
begin
  if FDataSet.Active then
    FDataSet.RefreshParams;
end;

procedure TMDODataLink.CheckBrowseMode;
begin
  if FDataSet.Active then
    FDataSet.CheckBrowseMode;
end;

function TMDODataLink.GetDetailDataSet: TDataset;
begin
  Result := FDataSet;
end;

procedure TMDODataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FDataSet.Active then
    FDataSet.RefreshParams;
end;



{ TMDOCustomDataSet }

{
****************************** TMDOCustomDataSet *******************************
}
constructor TMDOCustomDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFBLoaded := False;
  CheckFBLoaded;
  FFBLoaded := True;
  FBase := TMDOBase.Create(Self);
  FCurrentRecord := -1;
  FDeletedRecords := 0;
  FCount := 0;
  FUniDirectional := False;
  FBufferChunks := BufferCacheSize;
  FBlobStreamList := TList.Create;
  FDataLink := TMDODataLink.Create(Self);
  FQDelete := TMDOSQL.Create(Self);
  FQDelete.OnSQLChanging := SQLChanging;
  FQDelete.GoToFirstRecordOnExecute := False;
  FQInsert := TMDOSQL.Create(Self);
  FQInsert.OnSQLChanging := SQLChanging;
  FQInsert.GoToFirstRecordOnExecute := False;
  FQRefresh := TMDOSQL.Create(Self);
  FQRefresh.OnSQLChanging := SQLChanging;
  FQRefresh.GoToFirstRecordOnExecute := False;
  FQSelect := TMDOSQL.Create(Self);
  FQSelect.OnSQLChanging := SQLChanging;
  FQSelect.GoToFirstRecordOnExecute := False;
  FQModify := TMDOSQL.Create(Self);
  FQModify.OnSQLChanging := SQLChanging;
  FQModify.GoToFirstRecordOnExecute := False;
  FSimpleSelectSQL := TStringList.Create;
  FUpdateRecordTypes := [cusUnmodified, cusModified, cusInserted];
  FParamCheck := True;
  FForcedRefresh := False;
  FUpdateRecordCount := True;
  {Bookmark Size is Integer for IBX}
  BookmarkSize := SizeOf(Integer);
  FBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
  FBase.AfterDatabaseDisconnect := DoAfterDatabaseDisconnect;
  FBase.OnDatabaseFree := DoDatabaseFree;
  FBase.BeforeTransactionEnd := DoBeforeTransactionEnd;
  FBase.AfterTransactionEnd := DoAfterTransactionEnd;
  FBase.OnTransactionFree := DoTransactionFree;
  if AOwner is TMDODatabase then
    Database := TMDODatabase(AOwner)
  else if AOwner is TMDOTransaction then
    Transaction := TMDOTransaction(AOwner);
  FGeneratorLink := TMDOGeneratorLink.Create(Self);
  FGoToDBKey := False;
end;

destructor TMDOCustomDataSet.Destroy;
begin
  if FFBLoaded then
  begin
    FreeAndNil(FDataLink);
    FreeAndNil(FBase);
    ClearBlobCache;
    FBlobStreamList.Free;
    FreeMem(FBufferCache);
    FBufferCache := nil;
    FreeMem(FOldBufferCache);
    FOldBufferCache := nil;
    FCacheSize := 0;
    FOldCacheSize := 0;
    FMappedFieldPosition := nil;
    FreeAndNil(FGeneratorLink);
    FDidActivate := false;
  end;
  inherited Destroy;
end;

procedure TMDOCustomDataSet.ActivateConnection;
begin
  if not Assigned(Database) then
    MDOError(mdoeDatabaseNotAssigned, [nil]);
  if not Assigned(Transaction) then
    MDOError(mdoeTransactionNotAssigned, [nil]);
  if not Database.Connected then
    Database.Open;

end;

function TMDOCustomDataSet.ActivateTransaction: Boolean;
begin
  Result := False;
  if not Assigned(Transaction) then
    MDOError(mdoeTransactionNotAssigned, [nil]);
  if not Transaction.Active then
  begin
    Result := True;
    Transaction.StartTransaction;
    FDidActivate := True;
  end;
end;

function TMDOCustomDataSet.AdjustCurrentRecord(Buffer: Pointer; GetMode: TGetMode): TGetResult;
begin
  while not IsVisible(Buffer) do
  begin
    if GetMode = gmPrior then
    begin
      Dec(FCurrentRecord);
      if FCurrentRecord = -1 then
      begin
        result := grBOF;
        exit;
      end;
      ReadRecordCache(FCurrentRecord, Buffer, False);
    end
    else
    begin
      Inc(FCurrentRecord);
      if (FCurrentRecord = FRecordCount) then
      begin
        if (not FQSelect.EOF) and (FQSelect.Next <> nil) then
        begin
          FetchCurrentRecordToBuffer(FQSelect, FCurrentRecord, Buffer);
          Inc(FRecordCount);
        end
        else
        begin
          result := grEOF;
          exit;
        end;
      end
      else
        ReadRecordCache(FCurrentRecord, Buffer, False);
    end;
  end;
  result := grOK;
end;

function TMDOCustomDataSet.AdjustPosition(FCache: PChar; Offset: DWORD; Origin: Integer): Integer;
var
  OldCacheSize: Integer;
begin
  if (FCache = FBufferCache) then
  begin
    case Origin of
      FILE_BEGIN:
        FBPos := Offset;
      FILE_CURRENT:
        FBPos := FBPos + Offset;
      FILE_END:
        FBPos := DWORD(FBEnd) + Offset;
    end;
    OldCacheSize := FCacheSize;
    while (FBPos >= DWORD(FCacheSize)) do
      Inc(FCacheSize, FBufferChunkSize);
    if FCacheSize > OldCacheSize then
      MDOAlloc(FBufferCache, FCacheSize, FCacheSize);
    result := FBPos;
  end
  else
  begin
    case Origin of
      FILE_BEGIN:
        FOBPos := Offset;
      FILE_CURRENT:
        FOBPos := FOBPos + Offset;
      FILE_END:
        FOBPos := DWORD(FOBEnd) + Offset;
    end;
    OldCacheSize := FOldCacheSize;
    while (FBPos >= DWORD(FOldCacheSize)) do
      Inc(FOldCacheSize, FBufferChunkSize);
    if FOldCacheSize > OldCacheSize then
      MDOAlloc(FOldBufferCache, FOldCacheSize, FOldCacheSize);
    result := FOBPos;
  end;
end;

procedure TMDOCustomDataSet.AdjustRecordOnInsert(Buffer: Pointer);
begin
  with PRecordData(Buffer)^ do
    if (State = dsInsert) and (not Modified) then
    begin
      rdRecordNumber := FRecordCount;
      FCurrentRecord := FRecordCount;
    end;
end;

function TMDOCustomDataSet.AllocRecordBuffer: PChar;
begin
  result := nil;
  MDOAlloc(result, FRecordBufferSize, FRecordBufferSize);
  Move(FModelBuffer^, result^, FRecordBufferSize);
end;

procedure TMDOCustomDataSet.ApplyUpdates;
var
  CurBookmark: string;
  Buffer: PRecordData;
  CurUpdateTypes: TMDOUpdateRecordTypes;
  UpdateAction: TMDOUpdateAction;
  UpdateKind: TUpdateKind;
  bRecordsSkipped: Boolean;

  procedure GetUpdateKind;
  begin
    case Buffer^.rdCachedUpdateStatus of
      cusModified:
        UpdateKind := ukModify;
      cusInserted:
        UpdateKind := ukInsert;
    else
      UpdateKind := ukDelete;
    end;
  end;

  procedure ResetBufferUpdateStatus;
  begin
    case Buffer^.rdCachedUpdateStatus of
      cusModified:
        begin
          PRecordData(Buffer)^.rdUpdateStatus := usUnmodified;
          PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
        end;
      cusInserted:
        begin
          PRecordData(Buffer)^.rdUpdateStatus := usUnmodified;
          PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
        end;
      cusDeleted:
        begin
          PRecordData(Buffer)^.rdUpdateStatus := usDeleted;
          PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
        end;
    end;
    WriteRecordCache(PRecordData(Buffer)^.rdRecordNumber, Pointer(Buffer));
  end;

  procedure UpdateUsingOnUpdateRecord;
  begin
    UpdateAction := uaFail;
    try
      FOnUpdateRecord(Self, UpdateKind, UpdateAction);
    except
      on E: Exception do
      begin
        if (E is EDatabaseError) and Assigned(FOnUpdateError) then
          FOnUpdateError(Self, EMDOError(E), UpdateKind, UpdateAction);
        if UpdateAction = uaFail then
          raise;
      end;
    end;
  end;

  procedure UpdateUsingUpdateObject;
  begin
    try
      FUpdateObject.Apply(UpdateKind);
      ResetBufferUpdateStatus;
    except
      on E: Exception do
        if (E is EDatabaseError) and Assigned(FOnUpdateError) then
          FOnUpdateError(Self, EMDOError(E), UpdateKind, UpdateAction);
    end;
  end;

  procedure UpdateUsingInternalquery;
  begin
    try
      case Buffer^.rdCachedUpdateStatus of
        cusModified:
          InternalPostRecord(FQModify, Buffer);
        cusInserted:
          InternalPostRecord(FQInsert, Buffer);
        cusDeleted:
          InternalDeleteRecord(FQDelete, Buffer);
      end;
    except
      on E: EMDOError do
      begin
        UpdateAction := uaFail;
        if Assigned(FOnUpdateError) then
          FOnUpdateError(Self, E, UpdateKind, UpdateAction);
        case UpdateAction of
          uaFail:
            raise;
          uaAbort:
            SysUtils.Abort;
          uaSkip:
            bRecordsSkipped := True;
        end;
      end;
    end;
  end;

begin
  if State in [dsEdit, dsInsert] then
    Post;
  FBase.CheckDatabase;
  FBase.CheckTransaction;
  DisableControls;
  CurBookmark := Bookmark;
  CurUpdateTypes := FUpdateRecordTypes;
  FUpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
  try
    First;
    bRecordsSkipped := False;
    while not EOF do
    begin
      Buffer := PRecordData(GetActiveBuf);
      GetUpdateKind;
      UpdateAction := uaApply;
      if Assigned(FUpdateObject) or Assigned(FOnUpdateRecord) then
      begin
        if (Assigned(FOnUpdateRecord)) then
          UpdateUsingOnUpdateRecord
        else if Assigned(FUpdateObject) then
          UpdateUsingUpdateObject;
        case UpdateAction of
          uaFail:
            MDOError(mdoeUserAbort, [nil]);
          uaAbort:
            SysUtils.Abort;
          uaApplied:
            ResetBufferUpdateStatus;
          uaSkip:
            bRecordsSkipped := True;
          uaRetry:
            Continue;
        end;
      end;
      if (not Assigned(FUpdateObject)) and (UpdateAction = UaApply) then
      begin
        UpdateUsingInternalquery;
        UpdateAction := uaApplied;
      end;
      Next;
    end;
    FUpdatesPending := bRecordsSkipped;
  finally
    FUpdateRecordTypes := CurUpdateTypes;
    Bookmark := CurBookmark;
    EnableControls;
  end;
end;

function TMDOCustomDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := false;
  if not Assigned(Bookmark) then
    exit;
  Result := PInteger(Bookmark)^ < FRecordCount;
end;

function TMDOCustomDataSet.CachedUpdateStatus: TCachedUpdateStatus;
begin
  if Active then
    result := PRecordData(GetActiveBuf)^.rdCachedUpdateStatus
  else
    result := cusUnmodified;
end;

procedure TMDOCustomDataSet.CancelUpdates;
var
  CurUpdateTypes: TMDOUpdateRecordTypes;
begin
  if State in [dsEdit, dsInsert] then
    Post;
  if FCachedUpdates and FUpdatesPending then
  begin
    DisableControls;
    CurUpdateTypes := UpdateRecordTypes;
    UpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
    try
      First;
      while not EOF do
      begin
        if UpdateStatus = usInserted then
          RevertRecord
        else
        begin
          RevertRecord;
          Next;
        end;
      end;
    finally
      UpdateRecordTypes := CurUpdateTypes;
      First;
      FUpdatesPending := False;
      EnableControls;
    end;
  end;
end;

function TMDOCustomDataSet.CanDelete: Boolean;
begin
  if (FQDelete.SQL.Text <> '') or (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukDelete).Text <> '')) then
    result := True
  else
    result := False;
end;

function TMDOCustomDataSet.CanEdit: Boolean;
var
  Buff: PRecordData;
begin
  Buff := PRecordData(GetActiveBuf);
  result := (FQModify.SQL.Text <> '') or (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukModify).Text <> '')) or ((Buff <> nil) and (Buff^.rdCachedUpdateStatus = cusInserted) and (FCachedUpdates));
end;

function TMDOCustomDataSet.CanInsert: Boolean;
begin
  result := (FQInsert.SQL.Text <> '') or (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukInsert).Text <> ''));
end;

function TMDOCustomDataSet.CanRefresh: Boolean;
begin
  result := (FQRefresh.SQL.Text <> '') or (Assigned(FUpdateObject) and (FUpdateObject.RefreshSQL.Text <> ''));
end;

procedure TMDOCustomDataSet.CheckDatasetClosed;
begin
  if FOpen then
    MDOError(mdoeDatasetOpen, [nil]);
end;

procedure TMDOCustomDataSet.CheckDatasetOpen;
begin
  if not FOpen then
    MDOError(mdoeDatasetClosed, [nil]);
end;

procedure TMDOCustomDataSet.CheckEditState;
begin
  case State of
    { Check all the wsEditMode types }
    dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter, dsNewValue, dsInternalCalc:
      begin
        if (State in [dsEdit]) and (not CanEdit) then
          MDOError(mdoeCannotUpdate, [nil]);
        if (State in [dsInsert]) and (not CanInsert) then
          MDOError(mdoeCannotInsert, [nil]);
      end;
  else
    MDOError(mdoeNotEditing, [])
  end;
end;

procedure TMDOCustomDataSet.CheckNotUniDirectional;
begin
  if UniDirectional then
    MDOError(mdoeDataSetUniDirectional, [nil]);
end;

procedure TMDOCustomDataSet.ClearBlobCache;
var
  i: Integer;
begin
  try
    for i := 0 to FBlobStreamList.Count - 1 do
    begin
      TMDOBlobStream(FBlobStreamList[i]).Free;
      FBlobStreamList[i] := nil;
    end;
    FBlobStreamList.Pack;
  except
  end;
end;

procedure TMDOCustomDataSet.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

function TMDOCustomDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  CMPLess = -1;
  CMPEql = 0;
  CMPGtr = 1;
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, CMPLess), (CMPGtr, CMPEql));
begin
  result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];

  if Result = 2 then
  begin
    if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := CMPLess
    else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := CMPGtr
    else
      Result := CMPEql;
  end;
end;

function TMDOCustomDataSet.ConstraintsStored: Boolean;
begin
  Result := Constraints.Count > 0;
end;

procedure TMDOCustomDataSet.CopyRecordBuffer(Source, Dest: Pointer);
begin
  Move(Source^, Dest^, FRecordBufferSize);
end;

function TMDOCustomDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  pb: PBlobDataArray;
  fs: TMDOBlobStream;
  Buff: PChar;
  bTr, bDB: Boolean;
begin
  Buff := GetActiveBuf;
  if Buff = nil then
  begin
    fs := TMDOBlobStream.Create;
    fs.Mode := bmReadWrite;
    FBlobStreamList.Add(Pointer(fs));
    result := TMDODSBlobStream.Create(Field, fs, Mode);
    exit;
  end;
  pb := PBlobDataArray(Buff + FBlobCacheOffset);
  if pb^[Field.Offset] = nil then
  begin
    AdjustRecordOnInsert(Buff);
    pb^[Field.Offset] := TMDOBlobStream.Create;
    fs := pb^[Field.Offset];
    FBlobStreamList.Add(Pointer(fs));
    fs.Mode := bmReadWrite;
    fs.Database := Database;
    fs.Transaction := Transaction;
    fs.BlobID := PISC_QUAD(@Buff[PRecordData(Buff)^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataOfs])^;
    if (CachedUpdates) then
    begin
      bTr := not Transaction.InTransaction;
      bDB := not Database.Connected;
      if bDB then
        Database.Open;
      if bTr then
        Transaction.StartTransaction;
      fs.Seek(0, soFromBeginning);
      if bTr then
        Transaction.Commit;
      if bDB then
        Database.Close;
    end;
    WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Pointer(Buff));
  end
  else
    fs := pb^[Field.Offset];
  result := TMDODSBlobStream.Create(Field, fs, Mode);
end;

procedure TMDOCustomDataSet.DeactivateTransaction;
var
  i: Integer;
begin
  if not Assigned(Transaction) then
    MDOError(mdoeTransactionNotAssigned, [nil]);
  with Transaction do
  begin
    for i := 0 to SQLObjectCount - 1 do
    begin
      if (SQLObjects[i] <> nil) and ((SQLObjects[i]).owner is TDataSet) then
      begin
        if TDataSet(SQLObjects[i].owner).Active then
        begin
          FDidActivate := False;
          exit;
        end;
      end;
    end;
  end;
  FInternalPrepared := False;
  if Transaction.InTransaction then
    Transaction.ApplyDefaultAction;
  FDidActivate := False;
end;

procedure TMDOCustomDataSet.Disconnect;
begin
  Close;
  InternalUnPrepare;
end;

procedure TMDOCustomDataSet.DoAfterDatabaseDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDatabaseDisconnect) then
    FAfterDatabaseDisconnect(Sender);
end;

procedure TMDOCustomDataSet.DoAfterTransactionEnd(Sender: TObject);
begin
  if Assigned(FAfterTransactionEnd) then
    FAfterTransactionEnd(Sender);
end;

procedure TMDOCustomDataSet.DoBeforeDatabaseDisconnect(Sender: TObject);
begin
  if Active then
    Active := False;
  FInternalPrepared := False;
  if Assigned(FBeforeDatabaseDisconnect) then
    FBeforeDatabaseDisconnect(Sender);
end;

procedure TMDOCustomDataSet.DoBeforeDelete;
var
  Buff: PRecordData;
begin
  if not CanDelete then
    MDOError(mdoeCannotDelete, [nil]);
  // Prepare method is done before its execution, allowed only for
  // users with Delete grants.
  if not Assigned(FUpdateObject) and not FQDelete.Prepared then
    FQDelete.Prepare;
  Buff := PRecordData(GetActiveBuf);
  if FCachedUpdates and (Buff^.rdCachedUpdateStatus in [cusUnmodified]) then
    SaveOldBuffer(PChar(Buff));
  inherited DoBeforeDelete;
end;

procedure TMDOCustomDataSet.DoBeforeEdit;
var
  Buff: PRecordData;
begin
  Buff := PRecordData(GetActiveBuf);
  if not (CanEdit or (FQModify.SQL.Count <> 0) or (FCachedUpdates and Assigned(FOnUpdateRecord))) then
    MDOError(mdoeCannotUpdate, [nil]);
  // Prepare method is done before its execution, allowed only for
  // users with Update grants.
  if not Assigned(FUpdateObject) and not FQModify.Prepared then
    FQModify.Prepare;
  if FCachedUpdates and (Buff^.rdCachedUpdateStatus in [cusUnmodified, cusInserted]) then
    SaveOldBuffer(PChar(Buff));
  CopyRecordBuffer(GetActiveBuf, FOldBuffer);
  inherited DoBeforeEdit;
end;

procedure TMDOCustomDataSet.DoBeforeInsert;
begin
  if not CanInsert then
    MDOError(mdoeCannotInsert, [nil]);
  // Prepare method is done before its execution, allowed only for
  // users with Insert grants.
  if not Assigned(FUpdateObject) and not FQInsert.Prepared then
    FQInsert.Prepare;
  inherited DoBeforeInsert;
end;

procedure TMDOCustomDataSet.DoBeforeTransactionEnd(Sender: TObject);
begin
  if Active then
    Active := False;
  if FQSelect <> nil then
    FQSelect.FreeHandle;
  if FQDelete <> nil then
    FQDelete.FreeHandle;
  if FQInsert <> nil then
    FQInsert.FreeHandle;
  if FQModify <> nil then
    FQModify.FreeHandle;
  if FQRefresh <> nil then
    FQRefresh.FreeHandle;
  if Assigned(FBeforeTransactionEnd) then
    FBeforeTransactionEnd(Sender);
end;

procedure TMDOCustomDataSet.DoDatabaseFree(Sender: TObject);
begin
  if Assigned(FDatabaseFree) then
    FDatabaseFree(Sender);
end;

procedure TMDOCustomDataSet.DoOnNewRecord;
const
  SQLDefault = ' SELECT RDB$DEFAULT_SOURCE AS DEFAULT_SOURCE                        ' + ' FROM RDB$FIELDS                                                    ' + ' WHERE (RDB$FIELD_NAME = (SELECT RDB$FIELD_SOURCE                   ' + '                          FROM RDB$RELATION_FIELDS                  ' + '                          WHERE (RDB$FIELD_NAME = :FIELD_NAME) AND  ' + '                                (RDB$RELATION_NAME = :TABLE_NAME))) ';
var
  i: integer;
  SQL: TMDOSQL;
  FieldAlias, FieldName, TableName: string;
  Field: TFieldDef;
  GeneratorField: TField;
  GeneratorValue: Int64;
begin
  inherited DoOnNewRecord;
  if FGeneratorLink.WhereApply = waNewRecord then
  begin
    if FindField(FGeneratorLink.FField) <> nil then
    begin
      GeneratorField := FindField(FGeneratorLink.FField);
      if (Assigned(GeneratorField)) then
      begin
        GeneratorValue := FGeneratorLink.getGenValue;
        if (GeneratorField is TLargeintField) then
        begin
          TLargeIntField(GeneratorField).AsLargeInt := GeneratorValue;
        end
        else
        begin
            //urgent check: what if generator is > 2^32 ??
          GeneratorField.AsString := IntToStr(GeneratorValue);
        end;
      end;

      if FLoadDefaults then
      begin
        SQL := TMDOSQL.Create(Self);
        try
          SQL.Transaction := Self.Transaction;
          SQL.Database := Self.Database;
          SQL.SQL.Clear;
          SQL.SQL.Text := SQLDefault;
          SQL.Prepare;

          for i := 0 to FQSelect.Current.Count - 1 do
          begin
            with FQSelect.Current[i].Data^ do
            begin
              SetString(TableName, relname, relname_length);
              SetString(FieldName, sqlname, sqlname_length);
              SetString(FieldAlias, aliasname, aliasname_length);

              SQL.Close;
              SQL.Params.ByName('FIELD_NAME').Value := FieldName;
              SQL.Params.ByName('TABLE_NAME').Value := TableName;
              try
                SQL.ExecQuery;
              except
                on E: Exception do
                begin
                  if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
                  begin
                    CursorTransactionErrorEnter(SQL);
                    SQL.ExecQuery;
                    CursorTransactionErrorExit(SQL);
                  end
                  else
                    raise Exception.Create(E.Message);
                end;
              end;

              Field := FieldDefs.Find(FieldAlias);

              if Field <> nil then
              begin
                if pos('''', SQL.FieldByName('DEFAULT_SOURCE').AsString) = 9 then
                  FieldByName(FieldAlias).Value := Copy(SQL.FieldByName('DEFAULT_SOURCE').AsString, 10, Length(SQL.FieldByName('DEFAULT_SOURCE').AsString) - 10)
                      //Field.Value := Copy(SQL.FieldByName('DEFAULT_SOURCE').AsString, 10, Length(SQL.FieldByName('DEFAULT_SOURCE').AsString) - 10)
                else if not SQL.FieldByName('DEFAULT_SOURCE').IsNull then
                begin
                  FieldByName(FieldAlias).Value := Copy(SQL.FieldByName('DEFAULT_SOURCE').AsString, 9, Length(SQL.FieldByName('DEFAULT_SOURCE').AsString) - 8);
                end;
              end;
            end;
          end;
        finally
          SQL.Free;
        end;
      end;
    end;
  end;
end;

procedure TMDOCustomDataSet.DoTransactionFree(Sender: TObject);
begin
  if Assigned(FTransactionFree) then
    FTransactionFree(Sender);
end;

procedure TMDOCustomDataSet.FetchAll;
var
  SetCursor: Boolean;
  CurBookmark: string;
begin
  SetCursor := (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault);
  if SetCursor then
    Screen.Cursor := crHourGlass;
  try
    if FQSelect.EOF or not FQSelect.Open then
      exit;
    DisableControls;
    try
      CurBookmark := Bookmark;
      Last;
      Bookmark := CurBookmark;
    finally
      EnableControls;
    end;
  finally
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOCustomDataSet.FetchCurrentRecordToBuffer(Qry: TMDOSQL; RecordNumber: Integer; Buffer: PChar);
var
  p: PRecordData;
  pbd: PBlobDataArray;
  i, j: Integer;
  LocalData: Pointer;
  LocalDate, LocalDouble: Double;
  LocalInt: Integer;
  LocalInt64: Int64;
  LocalBoolean: Boolean;
  LocalCurrency: Currency;
  FieldsLoaded: Integer;
begin
  p := PRecordData(Buffer);
  { Make sure blob cache is empty }
  pbd := PBlobDataArray(Buffer + FBlobCacheOffset);
  if RecordNumber > -1 then
    for i := 0 to BlobFieldCount - 1 do
      pbd^[i] := nil;
  { Get record information }
  p^.rdBookmarkFlag := bfCurrent;
  p^.rdFieldCount := Qry.Current.Count;
  p^.rdRecordNumber := RecordNumber;
  p^.rdUpdateStatus := usUnmodified;
  p^.rdCachedUpdateStatus := cusUnmodified;
  p^.rdSavedOffset := $FFFFFFFF;
  
  { Load up the fields }
  FieldsLoaded := FQSelect.Current.Count;
  j := 1;
  for i := 0 to Qry.Current.Count - 1 do
  begin
    if (Qry = FQSelect) then
      j := i + 1
    else
    begin
      if FieldsLoaded = 0 then
        break;
      j := FQSelect.FieldIndex[Qry.Current[i].Name] + 1;
      if j < 1 then
        continue
      else
        Dec(FieldsLoaded);
    end;
    with FQSelect.Current[j - 1].Data^ do
      if aliasname = 'MDO_INTERNAL_DBKEY' then {do not localize}
      begin
        if sqllen <= 8 then
          p^.rdDBKey := PFBDBKEY(Qry.Current[i].AsPointer)^;
        continue;
      end;
    if j > 0 then
      with p^ do
      begin
        rdFields[j].fdDataType := Qry.Current[i].Data^.sqltype and (not 1);
        rdFields[j].fdDataScale := Qry.Current[i].Data^.sqlscale;
        rdFields[j].fdNullable := (Qry.Current[i].Data^.sqltype and 1 = 1);
        rdFields[j].fdIsNull := (rdFields[j].fdNullable and (Qry.Current[i].Data^.sqlind^ = -1));
        LocalData := Qry.Current[i].Data^.sqldata;
        case rdFields[j].fdDataType of
          SQL_TIMESTAMP:
            begin
              rdFields[j].fdDataSize := SizeOf(TDateTime);
              if RecordNumber >= 0 then
                LocalDate := TimeStampToMSecs(DateTimeToTimeStamp(Qry.Current[i].AsDateTime));
              LocalData := PChar(@LocalDate);
            end;
          SQL_TYPE_DATE:
            begin
              rdFields[j].fdDataSize := SizeOf(TDateTime);
              if RecordNumber >= 0 then
                LocalInt := DateTimeToTimeStamp(Qry.Current[i].AsDateTime).Date;
              LocalData := PChar(@LocalInt);
            end;
          SQL_TYPE_TIME:
            begin
              rdFields[j].fdDataSize := SizeOf(TDateTime);
              if RecordNumber >= 0 then
                LocalInt := DateTimeToTimeStamp(Qry.Current[i].AsDateTime).Time;
              LocalData := PChar(@LocalInt);
            end;
          SQL_SHORT, SQL_LONG:
            begin
              if (rdFields[j].fdDataScale = 0) then
              begin
                rdFields[j].fdDataSize := SizeOf(Integer);
                if RecordNumber >= 0 then
                  LocalInt := Qry.Current[i].AsLong;
                LocalData := PChar(@LocalInt);
              end
              else if (rdFields[j].fdDataScale >= (-4)) then
              begin
                rdFields[j].fdDataSize := SizeOf(Currency);
                if RecordNumber >= 0 then
                  LocalCurrency := Qry.Current[i].AsCurrency;
                LocalData := PChar(@LocalCurrency);
              end
              else
              begin
                rdFields[j].fdDataSize := SizeOf(Double);
                if RecordNumber >= 0 then
                  LocalDouble := Qry.Current[i].AsDouble;
                LocalData := PChar(@LocalDouble);
              end;
            end;
          SQL_INT64:
            begin
              if (rdFields[j].fdDataScale = 0) then
              begin
                rdFields[j].fdDataSize := SizeOf(Int64);
                if RecordNumber >= 0 then
                  LocalInt64 := Qry.Current[i].AsInt64;
                LocalData := PChar(@LocalInt64);
              end
              else if (rdFields[j].fdDataScale >= (-4)) then
              begin
                rdFields[j].fdDataSize := SizeOf(Currency);
                if RecordNumber >= 0 then
                  LocalCurrency := Qry.Current[i].AsCurrency;
                LocalData := PChar(@LocalCurrency);
              end
              else
              begin
                rdFields[j].fdDataSize := SizeOf(Double);
                if RecordNumber >= 0 then
                  LocalDouble := Qry.Current[i].AsDouble;
                LocalData := PChar(@LocalDouble);
              end
            end;
          SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
            begin
              rdFields[j].fdDataSize := SizeOf(Double);
              if RecordNumber >= 0 then
                LocalDouble := Qry.Current[i].AsDouble;
              LocalData := PChar(@LocalDouble);
            end;
          SQL_VARYING:
            begin
              rdFields[j].fdDataSize := Qry.Current[i].Data^.sqllen;
              rdFields[j].fdDataLength := isc_vax_integer(Qry.Current[i].Data^.sqldata, 2);
              if RecordNumber >= 0 then
              begin
                if (rdFields[j].fdDataLength = 0) then
                  LocalData := nil
                else
                  LocalData := @Qry.Current[i].Data^.sqldata[2];
              end;
            end;
          SQL_BOOLEAN:
            begin
              rdFields[j].fdDataSize := SizeOf(Boolean);
              if RecordNumber >= 0 then
                LocalBoolean := Qry.Current[i].AsBoolean;
              LocalData := PChar(@LocalBoolean);
            end;
        else { SQL_TEXT, SQL_BLOB, SQL_ARRAY, SQL_QUAD }
          begin
            rdFields[j].fdDataSize := Qry.Current[i].Data^.sqllen;
            if (rdFields[j].fdDataType = SQL_TEXT) then
              rdFields[j].fdDataLength := rdFields[j].fdDataSize;
          end;
        end;
        if RecordNumber < 0 then
        begin
          rdFields[j].fdIsNull := True;
          rdFields[j].fdDataOfs := FRecordSize;
          Inc(FRecordSize, rdFields[j].fdDataSize);
        end
        else
        begin
          if rdFields[j].fdDataType = SQL_VARYING then
          begin
            if LocalData <> nil then
              Move(LocalData^, Buffer[rdFields[j].fdDataOfs], rdFields[j].fdDataLength)
          end
          else
            Move(LocalData^, Buffer[rdFields[j].fdDataOfs], rdFields[j].fdDataSize)
        end;
      end;
  end;
  WriteRecordCache(RecordNumber, PChar(p));
end;

procedure TMDOCustomDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

function TMDOCustomDataSet.GetActiveBuf: PChar;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        result := nil
      else
        result := ActiveBuffer;
    dsEdit, dsInsert:
      result := ActiveBuffer;
    dsCalcFields:
      result := CalcBuffer;
    dsFilter:
      result := FFilterBuffer;
    dsNewValue:
      result := ActiveBuffer;
    dsOldValue:
      if (PRecordData(ActiveBuffer)^.rdRecordNumber = PRecordData(FOldBuffer)^.rdRecordNumber) then
        result := FOldBuffer
      else
        result := ActiveBuffer;
  else
    if not FOpen then
      result := nil
    else
      result := ActiveBuffer;
  end;
end;

procedure TMDOCustomDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(PRecordData(Buffer)^.rdRecordNumber, Data^, BookmarkSize);
end;

function TMDOCustomDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  result := PRecordData(Buffer)^.rdBookmarkFlag;
end;

function TMDOCustomDataSet.GetCanModify: Boolean;
begin
  result := (FQInsert.SQL.Text <> '') or (FQModify.SQL.Text <> '') or (FQDelete.SQL.Text <> '') or (Assigned(FUpdateObject));
end;

function TMDOCustomDataSet.GetCurrentRecord(Buffer: PChar): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    ReadRecordCache(PRecordData(ActiveBuffer)^.rdRecordNumber, Buffer, False);
    result := True;
  end
  else
    result := False;
end;

function TMDOCustomDataSet.GetDatabase: TMDODataBase;
begin
  result := FBase.Database;
end;

function TMDOCustomDataSet.GetDataSource: TDataSource;
begin
  if FDataLink = nil then
    result := nil
  else
    result := FDataLink.DataSource;
end;

function TMDOCustomDataSet.GetDBHandle: PISC_DB_HANDLE;
begin
  result := FBase.DBHandle;
end;

function TMDOCustomDataSet.GetDeleteSQL: TStrings;
begin
  result := FQDelete.SQL;
end;

function TMDOCustomDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := DefaultFieldClasses[FieldType];
end;

function TMDOCustomDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
  result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

function TMDOCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  lTempCurr: System.Currency;
begin
  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    Result := InternalGetFieldData(Field, @lTempCurr);
    if Result then
      CurrToBCD(lTempCurr, TBCD(Buffer^), 32, Field.Size);
  end
  else
    Result := InternalGetFieldData(Field, Buffer);
end;

function TMDOCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean;
begin
  if (Field.DataType = ftBCD) and not NativeFormat then
    Result := InternalGetFieldData(Field, Buffer)
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

function TMDOCustomDataSet.GetInsertSQL: TStrings;
begin
  result := FQInsert.SQL;
end;

function TMDOCustomDataSet.GetModifySQL: TStrings;
begin
  result := FQModify.SQL;
end;

function TMDOCustomDataSet.GetRecNo: Integer;
begin
  if GetActiveBuf = nil then
    result := 0
  else
    result := PRecordData(GetActiveBuf)^.rdRecordNumber + 1;
end;

function TMDOCustomDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  Result := grOK;
  if Filtered and Assigned(OnFilterRecord) then
  begin
    Accept := False;
    SaveState := SetTempState(dsFilter);
    while not Accept do
    begin
      Result := InternalGetRecord(Buffer, GetMode, DoCheck);
      if Result <> grOK then
        break;
      FFilterBuffer := Buffer;
      try
        Accept := True;
        OnFilterRecord(Self, Accept);
        if not Accept and (GetMode = gmCurrent) then
          GetMode := gmPrior;
      except
  //        Application.HandleException(Self);
      end;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck);
end;

function TMDOCustomDataSet.GetRecordCount: Integer;
begin
  if FUpdateRecordCount then
  begin
    FCount := InternalGetRecordCount;
    FUpdateRecordCount := False;
  end;
  result := FCount - FDeletedRecords;
end;

function TMDOCustomDataSet.GetRecordSize: Word;
begin
  result := FRecordBufferSize;
end;

function TMDOCustomDataSet.GetRefreshSQL: TStrings;
begin
  result := FQRefresh.SQL;
end;

function TMDOCustomDataSet.GetSelectSQL: TStrings;
begin
  result := FQSelect.SQL;
end;

function TMDOCustomDataSet.GetSelectStmtHandle: TISC_STMT_HANDLE;
begin
  Result := FQSelect.Handle;
end;

function TMDOCustomDataSet.GetSQLParams: TMDOXSQLDA;
begin
  if not FInternalPrepared then
    InternalPrepare;
  result := FQSelect.Params;
end;

function TMDOCustomDataSet.GetStatementType: TMDOSQLTypes;
begin
  result := FQSelect.SQLType;
end;

function TMDOCustomDataSet.GetTransaction: TMDOTransaction;
begin
  if assigned(FBase) then
    result := FBase.Transaction
  else
    result := nil;
end;

function TMDOCustomDataSet.GetTRHandle: PISC_TR_HANDLE;
begin
  result := FBase.TRHandle;
end;

procedure TMDOCustomDataSet.InitRecord(Buffer: PChar);
begin
  inherited InitRecord(Buffer);
  with PRecordData(Buffer)^ do
  begin
    rdUpdateStatus := TUpdateStatus(usInserted);
    rdBookMarkFlag := bfInserted;
    rdRecordNumber := -1;
  end;
end;

procedure TMDOCustomDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  CheckEditState;
  begin
     { When adding records, we *always* append.
       Insertion is just too costly }
    AdjustRecordOnInsert(Buffer);
    with PRecordData(Buffer)^ do
    begin
      rdUpdateStatus := usInserted;
      rdCachedUpdateStatus := cusInserted;
    end;
    if not CachedUpdates then
      InternalPostRecord(FQInsert, Buffer)
    else
    begin
      WriteRecordCache(FCurrentRecord, Buffer);
      FUpdatesPending := True;
    end;
    Inc(FRecordCount);
    InternalSetToRecord(Buffer);
  end
end;

procedure TMDOCustomDataSet.InternalBatchInput(InputObject: TMDOBatchInput);
begin
  FQSelect.BatchInput(InputObject);
end;

procedure TMDOCustomDataSet.InternalBatchOutput(OutputObject: TMDOBatchOutput);
var
  Qry: TMDOSQL;
begin
  Qry := TMDOSQL.Create(Self);
  try
    Qry.Database := FBase.Database;
    Qry.Transaction := FBase.Transaction;
    Qry.SQL.Assign(FQSelect.SQL);
    Qry.BatchOutput(OutputObject);
  finally
    Qry.Free;
  end;
end;

procedure TMDOCustomDataSet.InternalCancel;
var
  Buff: PChar;
  CurRec: Integer;
begin
  inherited InternalCancel;
  Buff := GetActiveBuf;
  if Buff <> nil then
  begin
    CurRec := FCurrentRecord;
    AdjustRecordOnInsert(Buff);
    if (State = dsEdit) then
    begin
      CopyRecordBuffer(FOldBuffer, Buff);
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
    end
    else
    begin
      CopyRecordBuffer(FModelBuffer, Buff);
      PRecordData(Buff)^.rdUpdateStatus := usDeleted;
      PRecordData(Buff)^.rdCachedUpdateStatus := cusUnmodified;
      PRecordData(Buff)^.rdBookmarkFlag := bfEOF;
      FCurrentRecord := CurRec;
    end;
  end;
end;

procedure TMDOCustomDataSet.InternalClose;
begin
  if FDidActivate then
    DeactivateTransaction;
  FQSelect.Close;
  ClearBlobCache;
  FreeRecordBuffer(FModelBuffer);
  FreeRecordBuffer(FOldBuffer);
  FCurrentRecord := -1;
  FOpen := False;
  FRecordCount := 0;
  FDeletedRecords := 0;
  FRecordSize := 0;
  FBPos := 0;
  FCount := 0;
  FOBPos := 0;
  FCacheSize := 0;
  FOldCacheSize := 0;
  FBEnd := 0;
  FOBEnd := 0;
  FreeMem(FBufferCache);
  FBufferCache := nil;
  FreeMem(FOldBufferCache);
  FOldBufferCache := nil;
  BindFields(False);
  if DefaultFields then
    DestroyFields;
end;

procedure TMDOCustomDataSet.InternalDelete;
var
  Buff: PChar;
  SetCursor: Boolean;
begin
  SetCursor := (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault);
  if SetCursor then
    Screen.Cursor := crHourGlass;
  try
    Buff := GetActiveBuf;
    if CanDelete then
    begin
      if not CachedUpdates then
        InternalDeleteRecord(FQDelete, Buff)
      else
      begin
        with PRecordData(Buff)^ do
        begin
          if rdCachedUpdateStatus = cusInserted then
            rdCachedUpdateStatus := cusUninserted
          else
          begin
            rdUpdateStatus := usDeleted;
            rdCachedUpdateStatus := cusDeleted;
          end;
        end;
        WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
      end;
      Inc(FDeletedRecords);
      FUpdatesPending := True;
    end
    else
      MDOError(mdoeCannotDelete, [nil]);
  finally
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOCustomDataSet.InternalDeleteRecord(Qry: TMDOSQL; Buff: Pointer);
begin
  if (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukDelete).Text <> '')) then
    FUpdateObject.Apply(ukDelete)
  else
  begin
    SetInternalSQLParams(FQDelete, Buff);
    try
      FQDelete.ExecQuery;
    except
      on E: Exception do
      begin
        if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
        begin
          CursorTransactionErrorEnter(FQDelete);
          FQDelete.ExecQuery;
          CursorTransactionErrorExit(FQDelete);
        end
        else
          raise Exception.Create(E.Message);
      end;
    end;
  end;
  with PRecordData(Buff)^ do
  begin
    rdUpdateStatus := usDeleted;
    rdCachedUpdateStatus := cusUnmodified;
  end;
  WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
end;

procedure TMDOCustomDataSet.InternalExecQuery;
var
  DidActivate: Boolean;
  SetCursor: Boolean;
begin
  DidActivate := False;
  SetCursor := (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault);
  if SetCursor then
    Screen.Cursor := crHourGlass;
  try
    ActivateConnection;
    DidActivate := ActivateTransaction;
    if FQSelect.SQL.Text = '' then
      MDOError(mdoeEmptyQuery, [nil]);
    if not FInternalPrepared then
      InternalPrepare;
    if (FQSelect.SQLType in [SQLSelect, SQLSelectForUpdate]) then
    begin
      MDOError(mdoeIsASelectStatement, [nil]);
    end
    else
      try
        FQSelect.ExecQuery;
      except
        on E: Exception do
        begin
          if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
          begin
            CursorTransactionErrorEnter(FQSelect);
            FQSelect.ExecQuery;
            CursorTransactionErrorExit(FQSelect);
          end
          else
            raise Exception.Create(E.Message);
        end;
      end;
  finally
    if DidActivate then
      DeactivateTransaction;
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOCustomDataSet.InternalFirst;
begin
  FCurrentRecord := -1;
end;

function TMDOCustomDataSet.InternalGetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Buff, Data: PChar;
  CurrentRecord: PRecordData;
begin
  result := False;
  Buff := GetActiveBuf;
  if (Buff = nil) or (not IsVisible(Buff)) then
    exit;
  { The intention here is to stuff the buffer with the data for the
   referenced field for the current record }
  CurrentRecord := PRecordData(Buff);
  if (Field.FieldNo < 0) then
  begin
    Inc(Buff, FRecordSize + Field.Offset);
    result := Boolean(Buff[0]);
    if result and (Buffer <> nil) then
      Move(Buff[1], Buffer^, Field.DataSize);
  end
  else if (FMappedFieldPosition[Field.FieldNo - 1] > 0) and (FMappedFieldPosition[Field.FieldNo - 1] <= CurrentRecord^.rdFieldCount) then
  begin
    result := not CurrentRecord^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdIsNull;
    if result and (Buffer <> nil) then
      with CurrentRecord^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]] do
      begin
        Data := Buff + CurrentRecord^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataOfs;
        if fdDataType = SQL_BOOLEAN then
        begin
          if Boolean(Data^) then
            WordBool(Buffer^) := True
          else
            WordBool(Buffer^) := False;
        end
        else if (fdDataType = SQL_VARYING) or (fdDataType = SQL_TEXT) then
        begin
          Move(Data^, Buffer^, fdDataLength);
          PChar(Buffer)[fdDataLength] := #0;
        end
        else
          Move(Data^, Buffer^, Field.DataSize);
      end;
  end;
end;

function TMDOCustomDataSet.InternalGetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  result := grError;
  case GetMode of
    gmCurrent:
      begin
        if (FCurrentRecord >= 0) then
        begin
          if FCurrentRecord < FRecordCount then
            ReadRecordCache(FCurrentRecord, Buffer, False)
          else
          begin
            while (not FQSelect.EOF) and (FQSelect.Next <> nil) and (FCurrentRecord >= FRecordCount) do
            begin
              FetchCurrentRecordToBuffer(FQSelect, FRecordCount, Buffer);
              Inc(FRecordCount);
            end;
            FCurrentRecord := FRecordCount - 1;
            if (FCurrentRecord >= 0) then
              ReadRecordCache(FCurrentRecord, Buffer, False);
          end;
          result := grOk;
        end
        else
          result := grBOF;
      end;
    gmNext:
      begin
        result := grOk;
        if FCurrentRecord = FRecordCount then
          result := grEOF
        else if FCurrentRecord = FRecordCount - 1 then
        begin
          if (not FQSelect.EOF) then
          begin
            FQSelect.Next;
            Inc(FCurrentRecord);
          end;
          if (FQSelect.EOF) then
          begin
            result := grEOF;
          end
          else
          begin
            Inc(FRecordCount);
            FetchCurrentRecordToBuffer(FQSelect, FCurrentRecord, Buffer);
          end;
        end
        else if (FCurrentRecord < FRecordCount) then
        begin
          Inc(FCurrentRecord);
          ReadRecordCache(FCurrentRecord, Buffer, False);
        end;
      end;
  else { gmPrior }
    begin
      if (FCurrentRecord = 0) then
      begin
        Dec(FCurrentRecord);
        result := grBOF;
      end
      else if (FCurrentRecord > 0) and (FCurrentRecord <= FRecordCount) then
      begin
        Dec(FCurrentRecord);
        ReadRecordCache(FCurrentRecord, Buffer, False);
        result := grOk;
      end
      else if (FCurrentRecord = -1) then
        result := grBOF;
    end;
  end;
  if result = grOk then
    result := AdjustCurrentRecord(Buffer, GetMode);
  if result = grOk then
    with PRecordData(Buffer)^ do
    begin
      rdBookmarkFlag := bfCurrent;
      if not (State = dsCalcFields) and not GoToDBKey then
        GetCalcFields(Buffer);
    end
  else if (result = grEOF) then
  begin
    CopyRecordBuffer(FModelBuffer, Buffer);
    PRecordData(Buffer)^.rdBookmarkFlag := bfEOF;
  end
  else if (result = grBOF) then
  begin
    CopyRecordBuffer(FModelBuffer, Buffer);
    PRecordData(Buffer)^.rdBookmarkFlag := bfBOF;
  end
  else if (result = grError) then
  begin
    CopyRecordBuffer(FModelBuffer, Buffer);
    PRecordData(Buffer)^.rdBookmarkFlag := bfEOF;
  end;
  ;
end;

procedure TMDOCustomDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  FCurrentRecord := PInteger(Bookmark)^;
end;

procedure TMDOCustomDataSet.InternalHandleException;
begin
  Application.HandleException(Self)
end;

procedure TMDOCustomDataSet.InternalInitFieldDefs;
const
  DefaultSQL = 'Select F.RDB$COMPUTED_BLR, ' + {do not localize}
    'F.RDB$DEFAULT_VALUE, R.RDB$FIELD_NAME ' + {do not localize}
    'from RDB$RELATION_FIELDS R, RDB$FIELDS F ' + {do not localize}
    'where R.RDB$RELATION_NAME = :RELATION ' +  {do not localize}
    'and R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ' + {do not localize}
    'and ((not F.RDB$COMPUTED_BLR is NULL) or ' + {do not localize}
    '     (not F.RDB$DEFAULT_VALUE is NULL)) '; {do not localize}
var
  FieldType: TFieldType;
  FieldSize: Word;
  FieldNullable: Boolean;
  i, FieldPosition, FieldPrecision: Integer;
  FieldAliasName: string;
  RelationName, FieldName: string;
  Query: TMDOSQL;
  FieldIndex: Integer;
  FRelationNodes: TRelationNode;
  FieldFound: TField;

  function Add_Node(Relation, Field: string): TRelationNode;
  var
    FField: TFieldNode;
    Retry: Integer;
  begin
    if FRelationNodes.RelationName = '' then
      Result := FRelationNodes
    else
    begin
      Result := TRelationNode.Create;
      Result.NextRelation := FRelationNodes;
    end;
    Result.RelationName := Relation;
    FRelationNodes := Result;
    Query.Params[0].AsString := Relation;
    try
      Query.ExecQuery;
    except
      on E: Exception do
      begin
        if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
        begin
          CursorTransactionErrorEnter(Query);
          Query.ExecQuery;
          CursorTransactionErrorExit(Query);
        end
        else
          raise Exception.Create(E.Message);
      end;
    end;
    while not Query.Eof do
    begin
      FField := TFieldNode.Create;
      FField.FieldName := Query.Fields[2].AsString;
      FField.DEFAULT_VALUE := not Query.Fields[1].IsNull;
      FField.COMPUTED_BLR := not Query.Fields[0].IsNull;
      FField.NextField := Result.FieldNodes;
      Result.FieldNodes := FField;
      Query.Next;
    end;
    Query.Close;
  end;

  function Has_COMPUTED_BLR(Relation, Field: string): Boolean;
  var
    FRelation: TRelationNode;
    FField: TFieldNode;
  begin
    FRelation := FRelationNodes;
    while Assigned(FRelation) and (FRelation.RelationName <> Relation) do
      FRelation := FRelation.NextRelation;
    if not Assigned(FRelation) then
      FRelation := Add_Node(Relation, Field);
    Result := false;
    FField := FRelation.FieldNodes;
    while Assigned(FField) do
      if FField.FieldName = Field then
      begin
        Result := FField.COMPUTED_BLR;
        Exit;
      end
      else
        FField := FField.NextField;
  end;

  function Has_DEFAULT_VALUE(Relation, Field: string): Boolean;
  var
    FRelation: TRelationNode;
    FField: TFieldNode;
  begin
    FRelation := FRelationNodes;
    while Assigned(FRelation) and (FRelation.RelationName <> Relation) do
      FRelation := FRelation.NextRelation;
    if not Assigned(FRelation) then
      FRelation := Add_Node(Relation, Field);
    Result := false;
    FField := FRelation.FieldNodes;
    while Assigned(FField) do
      if FField.FieldName = Field then
      begin
        Result := FField.DEFAULT_VALUE;
        Exit;
      end
      else
        FField := FField.NextField;
  end;

  procedure FreeNodes;
  var
    FRelation: TRelationNode;
    FField: TFieldNode;
  begin
    while Assigned(FRelationNodes) do
    begin
      while Assigned(FRelationNodes.FieldNodes) do
      begin
        FField := FRelationNodes.FieldNodes.NextField;
        FRelationNodes.FieldNodes.Free;
        FRelationNodes.FieldNodes := FField;
      end;
      FRelation := FRelationNodes.NextRelation;
      FRelationNodes.Free;
      FRelationNodes := FRelation;
    end;
  end;

begin
  if not InternalPrepared then
  begin
    InternalPrepare;
    exit;
  end;
  FRelationNodes := TRelationNode.Create;
  FNeedsRefresh := False;
  Database.InternalTransaction.StartTransaction;
  Query := TMDOSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    FieldDefs.BeginUpdate;
    FieldDefs.Clear;
    FieldIndex := 0;
    if (Length(FMappedFieldPosition) < FQSelect.Current.Count) then
      SetLength(FMappedFieldPosition, FQSelect.Current.Count);
    Query.SQL.Text := DefaultSQL;
    Query.Prepare;
    for i := 0 to FQSelect.Current.Count - 1 do
      with FQSelect.Current[i].Data^ do
      begin
        { Get the field name }
        SetString(FieldAliasName, aliasname, aliasname_length);
        SetString(RelationName, relname, relname_length);
        SetString(FieldName, sqlname, sqlname_length);
        FieldSize := 0;
        FieldPrecision := 0;
        FieldNullable := FQSelect.Current[i].IsNullable;
        FieldFound := FindField(FieldName);
        if Assigned(FieldFound) then
          if (RelationName <> '') and (FieldName <> '') then
            FieldFound.Origin := QuoteIdentifier(FBase.Database.SQLDialect, RelationName) + '.' + QuoteIdentifier(FBase.Database.SQLDialect, FieldName);
        case sqltype and not 1 of
          { All VARCHAR's must be converted to strings before recording
           their values }
          SQL_VARYING, SQL_TEXT:
            begin
              FieldSize := sqllen;
              FieldType := ftString;
            end;
          { All Doubles/Floats should be cast to doubles }
          SQL_DOUBLE, SQL_FLOAT:
            FieldType := ftFloat;
          SQL_SHORT:
            begin
              if (sqlscale = 0) then
                FieldType := ftSmallInt
              else
              begin
                FieldType := ftBCD;
                FieldPrecision := 4;
                FieldSize := -sqlscale;
              end;
            end;
          SQL_LONG:
            begin
              if (sqlscale = 0) then
                FieldType := ftInteger
              else if (sqlscale >= (-4)) then
              begin
                FieldType := ftBCD;
                FieldPrecision := 9;
                FieldSize := -sqlscale;
              end
              else
                FieldType := ftFloat;
            end;
          SQL_INT64:
            begin
              if (sqlscale = 0) then
                FieldType := ftLargeInt
              else if (sqlscale >= (-4)) then
              begin
                FieldType := ftBCD;
                FieldPrecision := 18;
                FieldSize := -sqlscale;
              end
              else
                FieldType := ftFloat;
            end;
          SQL_TIMESTAMP:
            FieldType := ftDateTime;
          SQL_TYPE_TIME:
            FieldType := ftTime;
          SQL_TYPE_DATE:
            FieldType := ftDate;
          SQL_BLOB:
            begin
              FieldSize := sizeof(TISC_QUAD);
              if (sqlsubtype = 1) then
                FieldType := ftmemo
              else
                FieldType := ftBlob;
            end;
          SQL_ARRAY:
            begin
              FieldSize := sizeof(TISC_QUAD);
              FieldType := ftUnknown;
            end;
          SQL_BOOLEAN:
            begin
              FieldType := ftBoolean;
            end;
        else
          FieldType := ftUnknown;
        end;
        FieldPosition := i + 1;
        if (FieldType <> ftUnknown) and (FieldAliasName <> 'MDO_INTERNAL_DBKEY') then {do not localize}
        begin
          FMappedFieldPosition[FieldIndex] := FieldPosition;
          Inc(FieldIndex);
          with FieldDefs.AddFieldDef do
          begin
            Name := string(FieldAliasName);
            FieldNo := FieldPosition;
            DataType := FieldType;
            Size := FieldSize;
            Precision := FieldPrecision;
            Required := not FieldNullable;
            InternalCalcField := False;
            if (FieldName <> '') and (RelationName <> '') then
            begin
              if Has_COMPUTED_BLR(RelationName, FieldName) then
              begin
                Attributes := [faReadOnly];
                InternalCalcField := True;
                FNeedsRefresh := True;
              end
              else
              begin
                if Has_DEFAULT_VALUE(RelationName, FieldName) then
                begin
                  if not FieldNullable then
                    Attributes := [faRequired];
                end
                else
                  FNeedsRefresh := True;
              end;
            end;
          end;
        end;
      end;
  finally
    Query.free;
    FreeNodes;
    Database.InternalTransaction.Commit;
    FieldDefs.EndUpdate;
  end;
end;

procedure TMDOCustomDataSet.InternalInitRecord(Buffer: PChar);
begin
  CopyRecordBuffer(FModelBuffer, Buffer);
end;

procedure TMDOCustomDataSet.InternalInsert;
begin
  CursorPosChanged;
end;

procedure TMDOCustomDataSet.InternalLast;
var
  Buffer: PChar;
begin
  if (FQSelect.EOF) then
    FCurrentRecord := FRecordCount
  else
  begin
    Buffer := AllocRecordBuffer;
    try
      while FQSelect.Next <> nil do
      begin
        FetchCurrentRecordToBuffer(FQSelect, FRecordCount, Buffer);
        Inc(FRecordCount);
      end;
      FCurrentRecord := FRecordCount;
    finally
      FreeRecordBuffer(Buffer);
    end;
  end;
end;

function TMDOCustomDataSet.InternalLocate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  fl: TList;
  CurBookmark: string;
  fld, val: Variant;
  i, fld_cnt: Integer;
begin
  fl := TList.Create;
  try
    GetFieldList(fl, KeyFields);
    fld_cnt := fl.Count;
    CurBookmark := Bookmark;
    result := False;
    while ((not result) and (not EOF)) do
    begin
      i := 0;
      result := True;
      while (result and (i < fld_cnt)) do
      begin
        if fld_cnt > 1 then
          val := KeyValues[i]
        else if VarIsArray(KeyValues) then
          val := KeyValues[i]
        else
          val := KeyValues;

        fld := TField(fl[i]).Value;
        result := not (VarIsNull(val) xor VarIsNull(fld));
        if result and not VarIsNull(val) then
        begin
          try
            fld := VarAsType(fld, VarType(val));
          except
            on E: EVariantError do
              result := False;
          end;
          if Result then
            if TField(fl[i]).DataType = ftString then
            begin
              if (loCaseInsensitive in Options) then
              begin
                fld := AnsiUpperCase(fld);
                val := AnsiUpperCase(val);
              end;
              fld := TrimRight(fld);
              val := TrimRight(val);
              if (loPartialKey in Options) then
                result := result and (AnsiPos(val, fld) = 1)
              else
                result := result and (val = fld);
            end
            else
              result := result and (val = fld);
        end;
        Inc(i);
      end;
      if not result then
        Next;
    end;
    if not result then
      Bookmark := CurBookmark
    else
      CursorPosChanged;
  finally
    fl.Free;
  end;
end;

procedure TMDOCustomDataSet.InternalOpen;
var
  SetCursor: Boolean;

  function RecordDataLength(n: Integer): Long;
  begin
    result := SizeOf(TRecordData) + ((n - 1) * SizeOf(TFieldData));
  end;

begin
  SetCursor := (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault);
  if SetCursor then
    Screen.Cursor := crHourGlass;
  try
    ActivateConnection;
    ActivateTransaction;
    if FQSelect.SQL.Text = '' then
      MDOError(mdoeEmptyQuery, [nil]);
    if not FInternalPrepared then
      InternalPrepare;
    if (FQSelect.SQLType in [SQLSelect, SQLSelectForUpdate]) then
    begin
      if DefaultFields then
        CreateFields;
      BindFields(True);
      FCurrentRecord := -1;
      FUpdateRecordCount := True;
      try
        FQSelect.ExecQuery;
      except
        on E: Exception do
        begin
          if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
          begin
            CursorTransactionErrorEnter(FQSelect);
            FQSelect.ExecQuery;
            CursorTransactionErrorExit(FQSelect);
          end
          else
            raise Exception.Create(E.Message);
        end;
      end;
      FOpen := FQSelect.Open;
      if (GetRecordCount + FDeletedRecords > BufferChunks) then
        BufferChunks := GetRecordCount + FDeletedRecords;
      if BufferChunks <= 0 then
        BufferChunks := 2;
      { Initialize offsets, buffer sizes, etc...
        1. Initially FRecordSize is just the "RecordDataLength".
        2. Allocate a "model" buffer and do a dummy fetch
        3. After the dummy fetch, FRecordSize will be appropriately
           adjusted to reflect the additional "weight" of the field
           data.
        4. Set up the FCalcFieldsOffset, FBlobCacheOffset and FRecordBufferSize.
        5. Now, with the BufferSize available, allocate memory for chunks of records
        6. Re-allocate the model buffer, accounting for the new
           FRecordBufferSize.
        7. Finally, calls to AllocRecordBuffer will work!.
       }
      {Step 1}
      FRecordSize := RecordDataLength(FQSelect.Current.Count);
      {Step 2, 3}
      MDOAlloc(FModelBuffer, 0, FRecordSize);
      FetchCurrentRecordToBuffer(FQSelect, -1, FModelBuffer);
      {Step 4}
      FCalcFieldsOffset := FRecordSize;
      FBlobCacheOffset := FCalcFieldsOffset + CalcFieldsSize;
      FRecordBufferSize := (FBlobCacheOffset + (BlobFieldCount * SizeOf(TMDOBlobStream)));
      {Step 5}
      if UniDirectional then
        FBufferChunkSize := FRecordBufferSize * UniCache
      else
        FBufferChunkSize := FRecordBufferSize * BufferChunks;
      MDOAlloc(FBufferCache, FBufferChunkSize, FBufferChunkSize);
      if FCachedUpdates or (csReading in ComponentState) then
        MDOAlloc(FOldBufferCache, FBufferChunkSize, FBufferChunkSize);
      FBPos := 0;
      FOBPos := 0;
      FBEnd := 0;
      FOBEnd := 0;
      FCacheSize := FBufferChunkSize;
      FOldCacheSize := FBufferChunkSize;
      {Step 6}
      MDOAlloc(FModelBuffer, RecordDataLength(FQSelect.Current.Count), FRecordBufferSize);
      {Step 7}
      FOldBuffer := AllocRecordBuffer;
    end
    else
      try
        FQSelect.ExecQuery;
      except
        on E: Exception do
        begin
          if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
          begin
            CursorTransactionErrorEnter(FQSelect);
            FQSelect.ExecQuery;
            CursorTransactionErrorExit(FQSelect);
          end
          else
            raise Exception.Create(E.Message);
        end;
      end;
  finally
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOCustomDataSet.InternalPost;
var
  Qry: TMDOSQL;
  Buff: PChar;
  SetCursor: Boolean;
  bInserting: Boolean;
begin
  SetCursor := (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault);
  if SetCursor then
    Screen.Cursor := crHourGlass;
  try
    Buff := GetActiveBuf;
    CheckEditState;
    AdjustRecordOnInsert(Buff);
    if (State = dsInsert) then
    begin
      bInserting := True;
      Qry := FQInsert;
      PRecordData(Buff)^.rdUpdateStatus := usInserted;
      PRecordData(Buff)^.rdCachedUpdateStatus := cusInserted;
      WriteRecordCache(FRecordCount, Buff);
      FCurrentRecord := FRecordCount;
    end
    else
    begin
      bInserting := False;
      Qry := FQModify;
      if PRecordData(Buff)^.rdCachedUpdateStatus = cusUnmodified then
      begin
        PRecordData(Buff)^.rdUpdateStatus := usModified;
        PRecordData(Buff)^.rdCachedUpdateStatus := cusModified;
      end
      else if PRecordData(Buff)^.rdCachedUpdateStatus = cusUninserted then
      begin
        PRecordData(Buff)^.rdCachedUpdateStatus := cusInserted;
        Dec(FDeletedRecords);
      end;
    end;
    if (not CachedUpdates) then
      InternalPostRecord(Qry, Buff)
    else
    begin
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
      FUpdatesPending := True;
    end;
    if bInserting then
    begin
      Inc(FRecordCount);
      Inc(FCount);
    end;
  finally
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOCustomDataSet.InternalPostRecord(Qry: TMDOSQL; Buff: Pointer);
var
  i, j, k: Integer;
  pbd: PBlobDataArray;
begin
  pbd := PBlobDataArray(PChar(Buff) + FBlobCacheOffset);
  j := 0;
  for i := 0 to FieldCount - 1 do
    if Fields[i].IsBlob then
    begin
      k := FMappedFieldPosition[Fields[i].FieldNo - 1];
      if pbd^[j] <> nil then
      begin
        pbd^[j].Finalize;
        PISC_QUAD(PChar(Buff) + PRecordData(Buff)^.rdFields[k].fdDataOfs)^ := pbd^[j].BlobID;
        PRecordData(Buff)^.rdFields[k].fdIsNull := pbd^[j].Size = 0;
      end;
      Inc(j);
    end;
  if Assigned(FUpdateObject) then
  begin
    if (Qry = FQDelete) then
      FUpdateObject.Apply(ukDelete)
    else if (Qry = FQInsert) then
      FUpdateObject.Apply(ukInsert)
    else
      FUpdateObject.Apply(ukModify);
  end
  else
  begin
    SetInternalSQLParams(Qry, Buff);
    try
      Qry.ExecQuery;
    except
      on E: Exception do
      begin
        if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
        begin
          CursorTransactionErrorEnter(Qry);
          Qry.ExecQuery;
          CursorTransactionErrorExit(Qry);
        end
        else
          raise Exception.Create(E.Message);
      end;
    end;
  end;
  PRecordData(Buff)^.rdUpdateStatus := usUnmodified;
  PRecordData(Buff)^.rdCachedUpdateStatus := cusUnmodified;
  SetModified(False);
  WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
  if (FForcedRefresh or FNeedsRefresh) and CanRefresh then
    InternalRefreshRow;
end;

procedure TMDOCustomDataSet.InternalPrepare;
var
  SetCursor: Boolean;
  DidActivate: Boolean;
begin
  if FInternalPrepared then
    Exit;
  DidActivate := False;
  SetCursor := (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault);
  if SetCursor then
    Screen.Cursor := crHourGlass;
  try
    ActivateConnection;
    DidActivate := ActivateTransaction;
    FBase.CheckDatabase;
    FBase.CheckTransaction;
    if trim(FQSelect.SQL.Text) <> '' then
    begin
      if not FQSelect.Prepared then
      begin
        FQSelect.ParamCheck := ParamCheck;
        FQSelect.Prepare;
      end;

      try
        if (Trim(FQDelete.SQL.Text) <> '') and not FQDelete.Prepared then
          FQDelete.Prepare;
      except
        on E: Exception do
          if not (E is EMDOFirebirdPermissionError) then
            raise;
      end;

      try
        if (Trim(FQInsert.SQL.Text) <> '') and not FQInsert.Prepared then
          FQInsert.Prepare;
      except
        on E: Exception do
          if not (E is EMDOFirebirdPermissionError) then
            raise;
      end;

      try
        if (Trim(FQModify.SQL.Text) <> '') and not FQModify.Prepared then
          FQModify.Prepare;
      except
        on E: Exception do
          if not (E is EMDOFirebirdPermissionError) then
            raise;
      end;

      try
        if (Trim(FQRefresh.SQL.Text) <> '') and not FQRefresh.Prepared then
          FQRefresh.Prepare;
      except
        on E: Exception do
          if not (E is EMDOFirebirdPermissionError) then
            raise;
      end;

      FInternalPrepared := True;
      InternalInitFieldDefs;
    end
    else
      MDOError(mdoeEmptyQuery, [nil]);
  finally
    if DidActivate then
      DeactivateTransaction;
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOCustomDataSet.InternalRefresh;
begin
  inherited InternalRefresh;
  InternalRefreshRow;
end;

procedure TMDOCustomDataSet.InternalRefreshRow;
var
  Buff: PChar;
  SetCursor: Boolean;
  ofs: DWORD;
  Qry: TMDOSQL;
begin
  SetCursor := (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault);
  if SetCursor then
    Screen.Cursor := crHourGlass;
  try
    Buff := GetActiveBuf;
    if CanRefresh then
    begin
      if Buff <> nil then
      begin
        if (Assigned(FUpdateObject) and (FUpdateObject.RefreshSQL.Text <> '')) then
        begin
          Qry := TMDOSQL.Create(self);
          Qry.Database := Database;
          Qry.Transaction := Transaction;
          Qry.GoToFirstRecordOnExecute := False;
          Qry.SQL.Text := FUpdateObject.RefreshSQL.Text;
        end
        else
          Qry := FQRefresh;
        SetInternalSQLParams(Qry, Buff);
        try
          Qry.ExecQuery;
        except
          on E: Exception do
          begin
            if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
            begin
              CursorTransactionErrorEnter(Qry);
              Qry.ExecQuery;
              CursorTransactionErrorExit(Qry);
            end
            else
              raise Exception.Create(E.Message);
          end;
        end;
        try
          if (Qry.SQLType = SQLExecProcedure) or (Qry.Next <> nil) then
          begin
            ofs := PRecordData(Buff)^.rdSavedOffset;
            FetchCurrentRecordToBuffer(Qry, PRecordData(Buff)^.rdRecordNumber, Buff);
            if FCachedUpdates and (ofs <> $FFFFFFFF) then
            begin
              PRecordData(Buff)^.rdSavedOffset := ofs;
              WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
              SaveOldBuffer(Buff);
            end;
          end;
        finally
          Qry.Close;
        end;
        if Qry <> FQRefresh then
          Qry.Free;
      end
    end
    else
      MDOError(mdoeCannotRefresh, [nil]);
  finally
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOCustomDataSet.InternalRevertRecord(RecordNumber: Integer);
var
  NewBuffer, OldBuffer: PRecordData;
begin
  NewBuffer := nil;
  OldBuffer := nil;
  NewBuffer := PRecordData(AllocRecordBuffer);
  OldBuffer := PRecordData(AllocRecordBuffer);
  try
    ReadRecordCache(RecordNumber, PChar(NewBuffer), False);
    ReadRecordCache(RecordNumber, PChar(OldBuffer), True);
    case NewBuffer^.rdCachedUpdateStatus of
      cusInserted:
        begin
          NewBuffer^.rdCachedUpdateStatus := cusUninserted;
          Inc(FDeletedRecords);
        end;
      cusModified, cusDeleted:
        begin
          if (NewBuffer^.rdCachedUpdateStatus = cusDeleted) then
            Dec(FDeletedRecords);
          CopyRecordBuffer(OldBuffer, NewBuffer);
        end;
    end;

    if State in dsEditModes then
      Cancel;

    WriteRecordCache(RecordNumber, PChar(NewBuffer));

    if (NewBuffer^.rdCachedUpdateStatus = cusUninserted) then
      ReSync([]);
  finally
    FreeRecordBuffer(PChar(NewBuffer));
    FreeRecordBuffer(PChar(OldBuffer));
  end;
end;

procedure TMDOCustomDataSet.InternalSetFieldData(Field: TField; Buffer: Pointer);
var
  Buff, TmpBuff: PChar;
begin
  Buff := GetActiveBuf;
  if Field.FieldNo < 0 then
  begin
    TmpBuff := Buff + FRecordSize + Field.Offset;
    Boolean(TmpBuff[0]) := LongBool(Buffer);
    if Boolean(TmpBuff[0]) then
      Move(Buffer^, TmpBuff[1], Field.DataSize);
    WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
  end
  else
  begin
    CheckEditState;
    with PRecordData(Buff)^ do
    begin
      { If inserting, Adjust record position }
      AdjustRecordOnInsert(Buff);
      if (FMappedFieldPosition[Field.FieldNo - 1] > 0) and (FMappedFieldPosition[Field.FieldNo - 1] <= rdFieldCount) then
      begin
        Field.Validate(Buffer);
        if (Buffer = nil) or (Field is TMDOStringField) and (PChar(Buffer)[0] = #0) then
          rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdIsNull := True
        else
        begin
          Move(Buffer^, Buff[rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataOfs], rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataSize);
          if (rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataType = SQL_TEXT) or (rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataType = SQL_VARYING) then
            rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdDataLength := StrLen(PChar(Buffer));
          rdFields[FMappedFieldPosition[Field.FieldNo - 1]].fdIsNull := False;
          if rdUpdateStatus = usUnmodified then
          begin
            if CachedUpdates then
            begin
              FUpdatesPending := True;
              if State = dsInsert then
                rdCachedUpdateStatus := cusInserted
              else if State = dsEdit then
                rdCachedUpdateStatus := cusModified;
            end;

            if State = dsInsert then
              rdUpdateStatus := usInserted
            else
              rdUpdateStatus := usModified;
          end;
          WriteRecordCache(rdRecordNumber, Buff);
          SetModified(True);
        end;
      end;
    end;
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

procedure TMDOCustomDataSet.InternalSetParamsFromCursor;
var
  i: Integer;
  cur_param: TMDOXSQLVAR;
  cur_field: TField;
  s: TStream;
begin
  if FQSelect.SQL.Text = '' then
    MDOError(mdoeEmptyQuery, [nil]);
  if not FInternalPrepared then
    InternalPrepare;
  if (SQLParams.Count > 0) and (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
    for i := 0 to SQLParams.Count - 1 do
    begin
      cur_field := DataSource.DataSet.FindField(SQLParams[i].Name);
      cur_param := SQLParams[i];
      if (cur_field <> nil) then
      begin
        if (cur_field.IsNull) then
          cur_param.IsNull := True
        else
          case cur_field.DataType of
            ftString:
              cur_param.AsString := cur_field.AsString;
            ftSmallint, ftWord:
              cur_param.AsShort := cur_field.AsInteger;
            ftBoolean:
              cur_param.AsBoolean := cur_field.AsBoolean;
            ftInteger:
              cur_param.AsLong := cur_field.AsInteger;
            ftLargeInt:
              cur_param.AsInt64 := TLargeIntField(cur_field).AsLargeInt;
            ftFloat, ftCurrency:
              cur_param.AsDouble := cur_field.AsFloat;
            ftBCD:
              cur_param.AsCurrency := cur_field.AsCurrency;
            ftDate:
              cur_param.AsDate := cur_field.AsDateTime;
            ftTime:
              cur_param.AsTime := cur_field.AsDateTime;
            ftDateTime:
              cur_param.AsDateTime := cur_field.AsDateTime;
            ftBlob, ftMemo:
              begin
                s := nil;
                try
                  s := DataSource.DataSet.CreateBlobStream(cur_field, bmRead);
                  cur_param.LoadFromStream(s);
                finally
                  s.free;
                end;
              end;
          else
            MDOError(mdoeNotSupported, [nil]);
          end;
      end;
    end;
  end;
end;

procedure TMDOCustomDataSet.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@(PRecordData(Buffer)^.rdRecordNumber));
end;

procedure TMDOCustomDataSet.InternalUnPrepare;
begin
  if FInternalPrepared then
  begin
    CheckDatasetClosed;
    FieldDefs.Clear;
    FInternalPrepared := False;
  end;
end;

function TMDOCustomDataSet.IsCursorOpen: Boolean;
begin
  result := FOpen;
end;

function TMDOCustomDataSet.IsSequenced: Boolean;
begin
  Result := Assigned(FQSelect) and FQSelect.EOF;
end;

function TMDOCustomDataSet.IsVisible(Buffer: PChar): Boolean;
begin
  result := True;
  if not (State = dsOldValue) then
    result := (PRecordData(Buffer)^.rdCachedUpdateStatus in FUpdateRecordTypes) and (not ((PRecordData(Buffer)^.rdCachedUpdateStatus = cusUnmodified) and (PRecordData(Buffer)^.rdUpdateStatus = usDeleted)));
end;

function TMDOCustomDataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  CurBookmark: string;
begin
  DisableControls;
  try
    CurBookmark := Bookmark;
    First;
    result := InternalLocate(KeyFields, KeyValues, Options);
    if not result then
      Bookmark := CurBookmark;
  finally
    EnableControls;
  end;
end;

function TMDOCustomDataSet.LocateNext(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DisableControls;
  try
    result := InternalLocate(KeyFields, KeyValues, Options);
  finally
    EnableControls;
  end;
end;

function TMDOCustomDataSet.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
var
  fl: TList;
  CurBookmark: string;
begin
  DisableControls;
  fl := TList.Create;
  CurBookmark := Bookmark;
  try
    First;
    if InternalLocate(KeyFields, KeyValues, []) then
    begin
      if (ResultFields <> '') then
        result := FieldValues[ResultFields]
      else
        result := NULL;
    end
    else
      result := Null;
  finally
    Bookmark := CurBookmark;
    fl.Free;
    EnableControls;
  end;
end;

procedure TMDOCustomDataSet.Post;
var
  GeneratorField: Tfield;
  GeneratorValue: Int64;
begin
  if (State = dsInsert) then
  begin
    if FGeneratorLink.LinksOk then
    begin
      if (FGeneratorLink.FWhereApply = waServer) then
        FieldByName(FGeneratorLink.Field).Required := false
      else if (FGeneratorLink.FWhereApply = waPost) then
      begin
        GeneratorField := FindField(FGeneratorLink.FField);
        if (assigned(GeneratorField)) then
        begin
          GeneratorValue := FGeneratorLink.getGenValue;

          if (GeneratorField is TLargeintField) then
          begin
            TLargeIntField(GeneratorField).AsLargeInt := GeneratorValue;
          end
          else
          begin
            GeneratorField.AsString := IntToStr(GeneratorValue);
          end;
        end;
      end;
    end;
  end;
  inherited Post;
end;

procedure TMDOCustomDataSet.PSEndTransaction(Commit: Boolean);
begin
  if Commit then
    Transaction.Commit
  else
    Transaction.Rollback;
end;

function TMDOCustomDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer;
var
  FQuery: TMDOQuery;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TMDOQuery.Create(nil);
    with TMDOQuery(ResultSet^) do
    begin
      SQL.Text := ASQL;
      Params.Assign(AParams);
      Open;
      Result := RowsAffected;
    end;
  end
  else
  begin
    FQuery := TMDOQuery.Create(nil);
    try
      FQuery.Database := Database;
      FQuery.Transaction := Transaction;
      FQuery.GenerateParamNames := True;
      FQuery.SQL.Text := ASQL;
      FQuery.Params.Assign(AParams);
      FQuery.ExecSQL;
      Result := FQuery.RowsAffected;
    finally
      FQuery.Free;
    end;
  end;
end;

function TMDOCustomDataSet.PSGetQuoteChar: string;
begin
  if Database.SQLDialect = 3 then
    Result := '"'
  else
    Result := '';
end;

function TMDOCustomDataSet.PsGetTableName: string;
begin
  //  if not FInternalPrepared then
  //    InternalPrepare;
    { It is possible for the FQSelectSQL to be unprepared
      with FInternalPreprepared being true (see DoBeforeTransactionEnd).
      So check the Prepared of the SelectSQL instead }
  if not FQSelect.Prepared then
    FQSelect.Prepare;
  Result := FQSelect.UniqueRelationName;
end;

function TMDOCustomDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErr: Integer;
begin
  if Prev <> nil then
    PrevErr := Prev.ErrorCode
  else
    PrevErr := 0;
  if E is EMDOError then
    with EMDOError(E) do
      Result := EUpdateError.Create(E.Message, '', SQLCode, PrevErr, E)
  else
    Result := inherited PSGetUpdateException(E, Prev);
end;

function TMDOCustomDataSet.PSInTransaction: Boolean;
begin
  Result := Transaction.InTransaction;
end;

function TMDOCustomDataSet.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TMDOCustomDataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TMDOCustomDataSet.PSReset;
begin
  inherited PSReset;
  if Active then
  begin
    Close;
    Open;
  end;
end;

procedure TMDOCustomDataSet.PSStartTransaction;
begin
  ActivateConnection;
  Transaction.StartTransaction;
end;

function TMDOCustomDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
var
  UpdateAction: TMDOUpdateAction;
  SQL: string;
  Params: TParams;

  procedure AssignParams(DataSet: TDataSet; Params: TParams);
  var
    I: Integer;
    Old: Boolean;
    Param: TParam;
    PName: string;
    Field: TField;
    Value: Variant;
  begin
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      PName := Param.Name;
      Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0; {do not localize}
      if Old then
        System.Delete(PName, 1, 4);
      Field := DataSet.FindField(PName);
      if not Assigned(Field) then
        Continue;
      if Old then
        Param.AssignFieldValue(Field, Field.OldValue)
      else
      begin
        Value := Field.NewValue;
        if VarIsEmpty(Value) then
          Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end;
    end;
  end;

begin
  Result := False;
  if Assigned(OnUpdateRecord) then
  begin
    UpdateAction := uaFail;
    if Assigned(FOnUpdateRecord) then
    begin
      FOnUpdateRecord(Delta, UpdateKind, UpdateAction);
      Result := UpdateAction = uaApplied;
    end;
  end
  else if Assigned(FUpdateObject) then
  begin
    SQL := FUpdateObject.GetSQL(UpdateKind).Text;
    if SQL <> '' then
    begin
      Params := TParams.Create;
      try
        Params.ParseSQL(SQL, True);
        AssignParams(Delta, Params);
        if PSExecuteStatement(SQL, Params) = 0 then
          MDOError(mdoeNoRecordsAffected, [nil]);
        Result := True;
      finally
        Params.Free;
      end;
    end;
  end;
end;

procedure TMDOCustomDataSet.ReadCache(FCache: PChar; Offset: DWORD; Origin: Integer; Buffer: PChar);
var
  pCache: PChar;
  bOld: Boolean;
begin
  bOld := (FCache = FOldBufferCache);
  pCache := PChar(AdjustPosition(FCache, Offset, Origin));
  if not bOld then
    pCache := FBufferCache + Integer(pCache)
  else
    pCache := FOldBufferCache + Integer(pCache);
  Move(pCache^, Buffer^, DWORD(FRecordBufferSize));
  AdjustPosition(FCache, FRecordBufferSize, FILE_CURRENT);
end;

procedure TMDOCustomDataSet.ReadRecordCache(RecordNumber: Integer; Buffer: PChar; ReadOldBuffer: Boolean);
begin
  if FUniDirectional then
    RecordNumber := RecordNumber mod UniCache;
  if (ReadOldBuffer) then
  begin
    ReadRecordCache(RecordNumber, Buffer, False);
    if FCachedUpdates and (PRecordData(Buffer)^.rdSavedOffset <> $FFFFFFFF) then
      ReadCache(FOldBufferCache, PRecordData(Buffer)^.rdSavedOffset, FILE_BEGIN, Buffer)
    else if ReadOldBuffer and (PRecordData(FOldBuffer)^.rdRecordNumber = RecordNumber) then
      CopyRecordBuffer(FOldBuffer, Buffer)
  end
  else
    ReadCache(FBufferCache, RecordNumber * FRecordBufferSize, FILE_BEGIN, Buffer);
end;

procedure TMDOCustomDataSet.RecordModified(Value: Boolean);
begin
  SetModified(Value);
end;

procedure TMDOCustomDataSet.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and (DataSet.State <> dsSetKey) then
        begin
          Close;
          Open;
        end;
    end;
  finally
    EnableControls;
  end;
end;

procedure TMDOCustomDataSet.ReQuery;
begin
  FQSelect.Close;
  ClearBlobCache;
  FCurrentRecord := -1;
  FRecordCount := 0;
  FDeletedRecords := 0;
  FBPos := 0;
  FOBPos := 0;
  FBEnd := 0;
  FOBEnd := 0;
  FQSelect.Close;
  try
    FQSelect.ExecQuery;
  except
    on E: Exception do
    begin
      if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
      begin
        CursorTransactionErrorEnter(FQSelect);
        FQSelect.ExecQuery;
        CursorTransactionErrorExit(FQSelect);
      end
      else
        raise Exception.Create(E.Message);
    end;
  end;
  FOpen := FQSelect.Open;
  First;
end;

procedure TMDOCustomDataSet.RevertRecord;
var
  Buff: PRecordData;
begin
  if FCachedUpdates and FUpdatesPending then
  begin
    Buff := PRecordData(GetActiveBuf);
    InternalRevertRecord(Buff^.rdRecordNumber);
    ReadRecordCache(Buff^.rdRecordNumber, PChar(Buff), False);
    DataEvent(deRecordChange, 0);
  end;
end;

procedure TMDOCustomDataSet.SaveOldBuffer(Buffer: PChar);
var
  OldBuffer: Pointer;

  procedure CopyOldBuffer;
  begin
    CopyRecordBuffer(Buffer, OldBuffer);
    if BlobFieldCount > 0 then
      FillChar(PChar(OldBuffer)[FBlobCacheOffset], BlobFieldCount * SizeOf(TMDOBlobStream), 0);
  end;

begin
  if (Buffer <> nil) and (PRecordData(Buffer)^.rdRecordNumber >= 0) then
  begin
    OldBuffer := AllocRecordBuffer;
    try
      if (PRecordData(Buffer)^.rdSavedOffset = $FFFFFFFF) then
      begin
        PRecordData(Buffer)^.rdSavedOffset := AdjustPosition(FOldBufferCache, 0, FILE_END);
        CopyOldBuffer;
        WriteCache(FOldBufferCache, 0, FILE_CURRENT, OldBuffer);
        WriteCache(FBufferCache, PRecordData(Buffer)^.rdRecordNumber * FRecordBufferSize, FILE_BEGIN, Buffer);
      end
      else
      begin
        CopyOldBuffer;
        WriteCache(FOldBufferCache, PRecordData(Buffer)^.rdSavedOffset, FILE_BEGIN, OldBuffer);
      end;
    finally
      FreeRecordBuffer(PChar(OldBuffer));
    end;
  end;
end;

procedure TMDOCustomDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PRecordData(Buffer)^.rdRecordNumber := PInteger(Data)^;
end;

procedure TMDOCustomDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordData(Buffer)^.rdBookmarkFlag := Value;
end;

procedure TMDOCustomDataSet.SetBufferChunks(Value: Integer);
begin
  if (Value <= 0) then
    FBufferChunks := BufferCacheSize
  else
    FBufferChunks := Value;
end;

procedure TMDOCustomDataSet.SetCachedUpdates(Value: Boolean);
begin
  if not Value and FCachedUpdates then
    CancelUpdates;
  if (not (csReading in ComponentState)) and Value then
    CheckDatasetClosed;
  FCachedUpdates := Value;
end;

procedure TMDOCustomDataSet.SetDatabase(Value: TMDODataBase);
begin
  if (FBase.Database <> Value) then
  begin
    CheckDatasetClosed;
    FBase.Database := Value;
    FQDelete.Database := Value;
    FQInsert.Database := Value;
    FQRefresh.Database := Value;
    FQSelect.Database := Value;
    FQModify.Database := Value;
  end;
end;

procedure TMDOCustomDataSet.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    MDOError(mdoeCircularReference, [nil]);
  if FDataLink <> nil then
    FDataLink.DataSource := Value;
end;

procedure TMDOCustomDataSet.SetDeleteSQL(Value: TStrings);
begin
  if FQDelete.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQDelete.SQL.Assign(Value);
  end;
end;

procedure TMDOCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  lTempCurr: System.Currency;
begin
  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    BCDToCurr(TBCD(Buffer^), lTempCurr);
    InternalSetFieldData(Field, @lTempCurr);
  end
  else
    InternalSetFieldData(Field, Buffer);
end;

procedure TMDOCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
begin
  if (not NativeFormat) and (Field.DataType = ftBCD) then
    InternalSetfieldData(Field, Buffer)
  else
    inherited SetFieldData(Field, Buffer, NativeFormat);
end;

procedure TMDOCustomDataSet.SetInsertSQL(Value: TStrings);
begin
  if FQInsert.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQInsert.SQL.Assign(Value);
  end;
end;

procedure TMDOCustomDataSet.SetInternalSQLParams(Qry: TMDOSQL; Buffer: Pointer);
var
  i, j: Integer;
  cr, data: PChar;
  fn, st: string;
  OldBuffer: Pointer;
  ts: TTimeStamp;
begin
  if (Buffer = nil) then
    MDOError(mdoeBufferNotSet, [nil]);
  if (not FInternalPrepared) then
    InternalPrepare;
  OldBuffer := nil;
  try
    for i := 0 to Qry.Params.Count - 1 do
    begin
      fn := Qry.Params[i].Name;
      if (Pos('OLD_', fn) = 1) then {mbcs ok}
      begin
        fn := Copy(fn, 5, Length(fn));
        if not Assigned(OldBuffer) then
        begin
          OldBuffer := AllocRecordBuffer;
          ReadRecordCache(PRecordData(Buffer)^.rdRecordNumber, OldBuffer, True);
        end;
        cr := OldBuffer;
      end
      else if (Pos('NEW_', fn) = 1) then {mbcs ok}
      begin
        fn := Copy(fn, 5, Length(fn));
        cr := Buffer;
      end
      else
        cr := Buffer;
      j := FQSelect.FieldIndex[fn] + 1;
      if (j > 0) then
        with PRecordData(cr)^ do
        begin
          if Qry.Params[i].name = 'MDO_INTERNAL_DBKEY' then {do not localize}
          begin
            PFBDBKey(Qry.Params[i].AsPointer)^ := rdDBKey;
            continue;
          end;
          if rdFields[j].fdIsNull then
            Qry.Params[i].IsNull := True
          else
          begin
            Qry.Params[i].IsNull := False;
            data := cr + rdFields[j].fdDataOfs;
            case rdFields[j].fdDataType of
              SQL_TEXT, SQL_VARYING:
                begin
                  SetString(st, data, rdFields[j].fdDataLength);
                  Qry.Params[i].AsString := st;
                end;
              SQL_FLOAT, SQL_DOUBLE, SQL_D_FLOAT:
                Qry.Params[i].AsDouble := PDouble(data)^;
              SQL_BOOLEAN:
                Qry.Params[i].AsBoolean := WordBool(PChar(data)^);
              SQL_SHORT, SQL_LONG:
                begin
                  if rdFields[j].fdDataScale = 0 then
                    Qry.Params[i].AsLong := PLong(data)^
                  else if rdFields[j].fdDataScale >= (-4) then
                    Qry.Params[i].AsCurrency := PCurrency(data)^
                  else
                    Qry.Params[i].AsDouble := PDouble(data)^;
                end;
              SQL_INT64:
                begin
                  if rdFields[j].fdDataScale = 0 then
                    Qry.Params[i].AsInt64 := PInt64(data)^
                  else if rdFields[j].fdDataScale >= (-4) then
                    Qry.Params[i].AsCurrency := PCurrency(data)^
                  else
                    Qry.Params[i].AsDouble := PDouble(data)^;
                end;
              SQL_BLOB, SQL_ARRAY, SQL_QUAD:
                Qry.Params[i].AsQuad := PISC_QUAD(data)^;
              SQL_TYPE_DATE:
                begin
                  ts.Date := PInt(data)^;
                  ts.Time := 0;
                  Qry.Params[i].AsDate := TimeStampToDateTime(ts);
                end;
              SQL_TYPE_TIME:
                begin
                  ts.Date := 1;
                  ts.Time := PInt(data)^;
                  Qry.Params[i].AsTime := TimeStampToDateTime(ts);
                end;
              SQL_TIMESTAMP:
                Qry.Params[i].AsDateTime := TimeStampToDateTime(MSecsToTimeStamp(PDouble(data)^));
            end;
          end;
        end;
    end;
  finally
    if (OldBuffer <> nil) then
      FreeRecordBuffer(PChar(OldBuffer));
  end;
end;

procedure TMDOCustomDataSet.SetModifySQL(Value: TStrings);
begin
  if FQModify.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQModify.SQL.Assign(Value);
  end;
end;

procedure TMDOCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value < 1) then
    Value := 1
  else if Value > FRecordCount then
  begin
    InternalLast;
    Value := Min(FRecordCount, Value);
  end;
  if (Value <> RecNo) then
  begin
    DoBeforeScroll;
    FCurrentRecord := Value - 1;
    Resync([]);
    DoAfterScroll;
  end;
end;

procedure TMDOCustomDataSet.SetRefreshSQL(Value: TStrings);
begin
  if FQRefresh.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQRefresh.SQL.Assign(Value);
  end;
end;

procedure TMDOCustomDataSet.SetSelectSQL(Value: TStrings);
begin
  if FQSelect.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQSelect.SQL.Assign(Value);
  end;
end;

procedure TMDOCustomDataSet.SetTransaction(Value: TMDOTransaction);
begin
  if (FBase.Transaction <> Value) then
  begin
    CheckDatasetClosed;
    FBase.Transaction := Value;
    FQDelete.Transaction := Value;
    FQInsert.Transaction := Value;
    FQRefresh.Transaction := Value;
    FQSelect.Transaction := Value;
    FQModify.Transaction := Value;
  end;
end;

procedure TMDOCustomDataSet.SetUniDirectional(Value: Boolean);
begin
  CheckDatasetClosed;
  FUniDirectional := Value;
end;

procedure TMDOCustomDataSet.SetUpdateMode(const Value: TUpdateMode);
begin
  if not CanModify then
    MDOError(mdoeCannotUpdate, [nil])
  else
    FUpdateMode := Value;
end;

procedure TMDOCustomDataSet.SetUpdateObject(Value: TMDODataSetUpdateObject);
begin
  if Value <> FUpdateObject then
  begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := nil;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
    begin
      if Assigned(FUpdateObject.DataSet) and (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;

      QInsert.Close;
      QInsert.SQL.Assign(FUpdateObject.GetSQL(ukInsert));

      QModify.Close;
      QModify.SQL.Assign(FUpdateObject.GetSQL(ukModify));

      QDelete.Close;
      QDelete.SQL.Assign(FUpdateObject.GetSQL(ukDelete));

      QRefresh.Close;
      QRefresh.SQL.Assign(FUpdateObject.RefreshSQL);
    end;
  end;
end;

procedure TMDOCustomDataSet.SetUpdateRecordTypes(Value: TMDOUpdateRecordTypes);
begin
  FUpdateRecordTypes := Value;
  if Active then
    First;
end;

procedure TMDOCustomDataSet.SQLChanging(Sender: TObject);
begin
  if FOpen then
    InternalClose;
  if FInternalPrepared then
    InternalUnPrepare;
end;

procedure TMDOCustomDataSet.Undelete;
var
  Buff: PRecordData;
begin
  CheckActive;
  Buff := PRecordData(GetActiveBuf);
  with Buff^ do
  begin
    if rdCachedUpdateStatus = cusUninserted then
    begin
      rdCachedUpdateStatus := cusInserted;
      Dec(FDeletedRecords);
    end
    else if (rdUpdateStatus = usDeleted) and (rdCachedUpdateStatus = cusDeleted) then
    begin
      rdCachedUpdateStatus := cusUnmodified;
      rdUpdateStatus := usUnmodified;
      Dec(FDeletedRecords);
    end;
    WriteRecordCache(rdRecordNumber, PChar(Buff));
  end;
end;

function TMDOCustomDataSet.UpdateStatus: TUpdateStatus;
begin
  if Active then
    if GetActiveBuf <> nil then
      result := PRecordData(GetActiveBuf)^.rdUpdateStatus
    else
      result := usUnmodified
  else
    result := usUnmodified;
end;

procedure TMDOCustomDataSet.WriteCache(FCache: PChar; Offset: DWORD; Origin: Integer; Buffer: PChar);
var
  pCache: PChar;
  bOld: Boolean;
  dwEnd: DWORD;
begin
  bOld := (FCache = FOldBufferCache);
  pCache := PChar(AdjustPosition(FCache, Offset, Origin));
  if not bOld then
    pCache := FBufferCache + Integer(pCache)
  else
    pCache := FOldBufferCache + Integer(pCache);
  Move(Buffer^, pCache^, FRecordBufferSize);
  dwEnd := AdjustPosition(FCache, FRecordBufferSize, FILE_CURRENT);
  if not bOld then
  begin
    if (dwEnd > FBEnd) then
      FBEnd := dwEnd;
  end
  else
  begin
    if (dwEnd > FOBEnd) then
      FOBEnd := dwEnd;
  end;
end;

procedure TMDOCustomDataSet.WriteRecordCache(RecordNumber: Integer; Buffer: PChar);
begin
  if RecordNumber >= 0 then
  begin
    if FUniDirectional then
      RecordNumber := RecordNumber mod UniCache;
    WriteCache(FBufferCache, RecordNumber * FRecordBufferSize, FILE_BEGIN, Buffer)
  end;
end;

procedure TMDOCustomDataSet.SetFiltered(Value: Boolean);
begin
  if (Filtered <> Value) then
  begin
    inherited SetFiltered(Value);
    {if Filtered and Assigned(onFilterRecord) and not Eof then
    begin
      FFilter := 0;
      GetNextRecords;
    end;}
  end
  else
    inherited SetFiltered(Value);
end;

function TMDOCustomDataSet.InternalGetRecordCount: Integer;
var
  Qry: TMDOSQL;
  I: Integer;

  function GetComplement: string;
  var
    J, K: Integer;
    From, OrderBy, WithLock: string;
  begin
    Result := '';
    for J := 1 to Length(SimpleSelectSQL.Text) do
    begin
      if (From <> 'FROM') and (UpperCase(SimpleSelectSQL.Text[J]) = 'F') and (UpperCase(SimpleSelectSQL.Text[J + 1]) = 'R') and (UpperCase(SimpleSelectSQL.Text[J + 2]) = 'O') and (UpperCase(SimpleSelectSQL.Text[J + 3]) = 'M') then
      begin
        for K := J to J + 3 do
          if (Length(SimpleSelectSQL.Text) > K) then
            From := From + UpperCase(SimpleSelectSQL.Text[K]);
        if From <> 'FROM' then
          From := '';
      end;

      if From <> 'FROM' then
        Continue;

      if (WithLock <> 'WITH LOCK') and (UpperCase(SimpleSelectSQL.Text[J]) = 'W') and (UpperCase(SimpleSelectSQL.Text[J + 1]) = 'I') and (UpperCase(SimpleSelectSQL.Text[J + 2]) = 'T') and (UpperCase(SimpleSelectSQL.Text[J + 3]) = 'H') then
      begin
        for K := J to J + 8 do
          if (Length(SimpleSelectSQL.Text) > K) then
            WithLock := WithLock + UpperCase(SimpleSelectSQL.Text[K]);
        if WithLock <> 'WITH LOCK' then
          WithLock := '';
      end;

      if (WithLock = 'WITH LOCK') then
        Break;

      if (OrderBy <> 'ORDERBY') and (UpperCase(SimpleSelectSQL.Text[J]) = 'O') and (UpperCase(SimpleSelectSQL.Text[J + 1]) = 'R') and (UpperCase(SimpleSelectSQL.Text[J + 2]) = 'D') and (UpperCase(SimpleSelectSQL.Text[J + 3]) = 'E') and (UpperCase(SimpleSelectSQL.Text[J + 4]) = 'R') then
      begin
        K := J;
        OrderBy := SimpleSelectSQL.Text[K];
        Inc(K);
        while (Length(SimpleSelectSQL.Text) > K) and (Length(OrderBy) < 7) do
        try
          if (Length(SimpleSelectSQL.Text) > K) and (SimpleSelectSQL.Text[K] <> ' ') then
            OrderBy := OrderBy + UpperCase(SimpleSelectSQL.Text[K]);
          if (OrderBy = 'ORDERBY') then
            Continue;
        finally
          Inc(K);
        end;
        if OrderBy <> 'ORDERBY' then
          OrderBy := '';
      end;

      if (OrderBy = 'ORDERBY') then
        break;

      Result := Result + SimpleSelectSQL.Text[J];
    end;
  end;

  function GetSelectPart: String;
  var
    J, K: Integer;
    First, Skip, Distinct, All, Text: string;
    Space: Integer;
  begin
    Result := 'SELECT';
    {Text := '';
    for J := 0 to SimpleSelectSQL.Count - 1 do
      Text := Text + SimpleSelectSQL[J] + ' ';
    Text := StrUpper(PAnsiChar(StringReplace(Text, '  ', ' ', [rfReplaceAll])));

    if Pos('FIRST', Text) > 0 then
    begin
      First := ' FIRST ';
      K := Pos('FIRST', Text) + 6;
      while True do
      begin
        if Text[K] = ' ' then
          Break;
        First := First + Text[K];
        Inc(K);
      end;
    end;

    if Pos('SKIP', Text) > 0 then
    begin
      Skip := ' SKIP ';
      K := Pos('SKIP', Text) + 5;
      while True do
      begin
        if Text[K] = ' ' then
          Break;
        Skip := Skip + Text[K];
        Inc(K);
      end;
    end;

    if Pos('DISTINCT', Text) > 0 then
      Distinct := ' DISTINCT';

    if Pos('ALL', Text) > 0 then
      Distinct := ' ALL'; }

    Result := Result + First + Skip + Distinct + All + ' COUNT(*) AS Internal_Count';
  end;

begin
  Qry := TMDOSQL.Create(nil);
  Qry.Transaction := Transaction;
  if not Qry.Transaction.InTransaction then
    Qry.Transaction.StartTransaction;
  Qry.SQL.Clear;
  Qry.SQL.Add(GetSelectPart);
  if SimpleSelectSQL.Text = '' then
    SimpleSelectSQL.Assign(QSelect.SQL);
  Qry.SQL.Add(GetComplement);
  for I := 0 to Qry.Params.Count - 1 do
    Qry.Params[I].Value := FQSelect.Params.ByName(Qry.Params[I].Name).Value;
  Qry.Prepare;
  try
    Qry.ExecQuery;
  except
    on E: Exception do
    begin
      if SeExiste('already has a cursor', E.Message) and SeExiste('assigned', E.Message) then
      begin
        CursorTransactionErrorEnter(Qry);
        Qry.ExecQuery;
        CursorTransactionErrorExit(Qry);
      end
      else
        raise Exception.Create(E.Message);
    end;
  end;
  Result := Qry.Current[0].AsInt64;
  FreeAndNil(Qry);
end;

{function TMDOCustomDataSet.GetNextRecords: Integer;
begin
  Result := 0;
  while (FRecordCount < FCount) and GetNextRecord do
    Inc(Result);
end;}

{ Read the record from FQSelect.Current into the record buffer
  Then write the buffer to in memory cache }
{ A visible record is one that is not truly deleted,
  and it is also listed in the FUpdateRecordTypes set }



{ I can "undelete" uninserted records (make them "inserted" again).
  I can "undelete" cached deleted (the deletion hasn't yet occurred) }
{ GetRecNo and SetRecNo both operate off of 1-based indexes as
 opposed to 0-based indexes.
 This is because we want LastRecordNumber/RecordCount = 1 }




{ TMDODataSet IProviderSupport }

{
********************************* TMDODataSet **********************************
}
procedure TMDODataSet.BatchInput(InputObject: TMDOBatchInput);
begin
  InternalBatchInput(InputObject);
end;

procedure TMDODataSet.BatchOutput(OutputObject: TMDOBatchOutput);
begin
  InternalBatchOutput(OutputObject);
end;

procedure TMDODataSet.ExecSQL;
begin
  InternalExecQuery;
end;

function TMDODataSet.GetPrepared: Boolean;
begin
  Result := InternalPrepared;
end;

procedure TMDODataSet.InternalOpen;
begin
  ActivateConnection;
  ActivateTransaction;
  InternalSetParamsFromCursor;
  inherited InternalOpen;
end;

function TMDODataSet.ParamByName(Idx: string): TMDOXSQLVAR;
begin
  if not FInternalPrepared then
    InternalPrepare;
  result := FQSelect.ParamByName(Idx);
end;

procedure TMDODataSet.Prepare;
begin
  InternalPrepare;
end;

procedure TMDODataSet.SetFiltered(Value: Boolean);
begin
  if (Filtered <> Value) then
  begin
    inherited SetFiltered(Value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TMDODataSet.UnPrepare;
begin
  InternalUnPrepare;
end;

{
*************************** TMDODataSetUpdateObject ****************************
}
constructor TMDODataSetUpdateObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefreshSQL := TStringList.Create;
end;

destructor TMDODataSetUpdateObject.Destroy;
begin
  FRefreshSQL.Free;
  inherited Destroy;
end;

procedure TMDODataSetUpdateObject.SetRefreshSQL(Value: TStrings);
begin
  FRefreshSQL.Assign(Value);
end;

{ TMDODSBlobStream }
{
******************************* TMDODSBlobStream *******************************
}
constructor TMDODSBlobStream.Create(AField: TField; ABlobStream: TMDOBlobStream; Mode: TBlobStreamMode);
begin
  FField := AField;
  FBlobStream := ABlobStream;
  FBlobStream.Seek(0, soFromBeginning);
  if (Mode = bmWrite) then
    FBlobStream.Truncate;
end;

function TMDODSBlobStream.Read(var Buffer; Count: Longint): LongInt;
begin
  result := FBlobStream.Read(Buffer, Count);
end;

function TMDODSBlobStream.Seek(Offset: Longint; Origin: Word): LongInt;
begin
  result := FBlobStream.Seek(Offset, Origin);
end;

procedure TMDODSBlobStream.SetSize(NewSize: Longint);
begin
  FBlobStream.SetSize(NewSize);
end;

function TMDODSBlobStream.Write(const Buffer; Count: Longint): LongInt;
begin
  if not (FField.DataSet.State in [dsEdit, dsInsert]) then
    MDOError(mdoeNotEditing, [nil]);
  TMDOCustomDataSet(FField.DataSet).RecordModified(True);
  TBlobField(FField).Modified := true;
  result := FBlobStream.Write(Buffer, Count);
  TMDOCustomDataSet(FField.DataSet).DataEvent(deFieldChange, Longint(FField));
end;

{ TMDOGeneratorLink }

{
****************************** TMDOGeneratorLink *******************************
}
constructor TMDOGeneratorLink.Create(MDODataSet: TMDOCustomDataSet);
begin
  inherited Create;
  FIncrementBy := 1;
  FField := '';
  FGenerator := '';
  FWhereApply := waNewRecord;
  FDataSet := MDODataSet;
end;

procedure TMDOGeneratorLink.Assign(Source: TPersistent);
begin
  if (Source is TMDOGeneratorLink) then
  begin
    FField := TMDOGeneratorLink(Source).Field;
    FGenerator := TMDOGeneratorLink(Source).Generator;
    FIncrementBy := TMDOGeneratorLink(Source).IncrementBy;
    FWhereApply := TMDOGeneratorLink(Source).WhereApply;
  end
  else
    inherited Assign(Source);
end;

function TMDOGeneratorLink.GetGenValue: Int64;
var
  SQL: TMDOSQL;
begin
  SQL := TMDOSQL.Create(FDataSet);
  try
    SQL.Database := FDataSet.Database;
    SQL.Transaction := FDataSet.Transaction;
    SQL.SQL.Add(Format('SELECT GEN_ID(%s, %d) FROM RDB$DATABASE ', [FGenerator, FIncrementBy]));
    try
      SQL.ExecQuery;
    except
      on E: EMDOError do
        MDOError(mdoeGeneratorNotDefined, [FGenerator]);
    end;
    Result := SQL.Fields[00].AsInt64;
  finally
    SQL.Close;
    SQL.Free;
  end;
end;

function TMDOGeneratorLink.LinksOk: Boolean;
begin
  Result := (FGenerator <> '') and (FField <> '');
end;

procedure TMDOCustomDataSet.SetGoToDBKey(const Value: Boolean);
begin
  FGoToDBKey := Value;
end;

procedure TMDOCustomDataSet.CursorTransactionErrorEnter(Query: TMDOSQL);
begin
  {FInternalDatabase := TMDODatabase.Create(Self);
  FInternalDatabase.ClientLib := Database.ClientLib;
  FInternalDatabase.DatabaseName := Database.DatabaseName;
  FInternalDatabase.Params.Assign(Database.Params);
  FInternalDatabase.LoginPrompt := False;
  FInternalDatabase.Open;
  FInternalDatabase.DefaultTransaction := Transaction;
  FOriginalDatabase := Query.Database;
  Query.Transaction.CommitRetaining;
  Query.ResetCursor;
  Query.Database := FInternalDatabase;
  Query.Prepare;}
end;

procedure TMDOCustomDataSet.CursorTransactionErrorExit(Query: TMDOSQL);
begin
  {if Assigned(FInternalDatabase) then
  begin
    FInternalDatabase.RemoveTransactions;
    FInternalDatabase.Close;
    FreeAndNil(FInternalDatabase);
  end;
  Query.Database := FOriginalDatabase;}
end;

end.

