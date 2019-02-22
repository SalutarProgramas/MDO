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

unit MDODatabase;

{$I ..\MDO.INC}

interface

uses
  Windows, Dialogs, Controls, StdCtrls, SysUtils, Classes, Forms, ExtCtrls,
  MDOHeader, MDOExternals, DB, MDO, DBLogDlg;

const
  DPBPrefix = 'isc_dpb_';
  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach',
    'set_db_charset',
    'gsec_attach',
    'address_path'
  );

  TPBPrefix = 'isc_tpb_';
  TPBConstantNames: array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo',
    'lock_timeout'
  );

type
  TMDOClientLib = (clAutoDetect, clGDS32, clFBClient, clFBEmbed);

const
  MDOClientLibrary: array [TMDOClientLib] of string = (
  FBASE_DLL_AUTODETECT,
  FBASE_DLL_GDS32,
  FBASE_DLL_FBCLIENT,
  FBASE_DLL_FBEMBED
  );

type
  TMDODatabase = class;
  TMDOTransaction = class;
  TMDOBase = class;

  TMDODatabaseLoginEvent = procedure (Database: TMDODatabase; LoginParams: 
          TStrings) of object;
  TMDOChangeLibraryEvent = procedure (var MDOClientLib: TMDOClientLib; var
          AbortChange: Boolean) of object;
  TMDOFileName = type string;
  { TMDODatabase }
  TMDODataBase = class (TCustomConnection)
  private
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FAllowStreamedConnected: Boolean;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FCanTimeout: Boolean;
    FDBName: TMDOFileName;
    FDBParams: TStrings;
    FDBParamsChanged: Boolean;
    FDBSQLDialect: Integer;
    FDefaultTransaction: TMDOTransaction;
    FDPB: PChar;
    FDPBLength: Short;
    FFBLoaded: Boolean;
    FHandle: TISC_DB_HANDLE;
    FHandleIsShared: Boolean;
    FHiddenPassword: string;
    FInternalTransaction: TMDOTransaction;
    FMDOClientLib: TMDOClientLib;
    FOnClientLibChange: TMDOChangeLibraryEvent;
    FOnDialectDowngradeWarning: TNotifyEvent;
    FOnIdleTimer: TNotifyEvent;
    FOnLogin: TMDODatabaseLoginEvent;
    FSQLDialect: Integer;
    FSQLObjects: TList;
    FTimer: TTimer;
    FTraceFlags: TTraceFlags;
    FTransactions: TList;
    FUserNames: TStringList;
    function AddSQLObject(ds: TMDOBase): Integer;
    procedure DBParamsChange(Sender: TObject);
    procedure DBParamsChanging(Sender: TObject);
    procedure EnsureInactive;
    function GetDBParamByDPB(const Idx: Integer): string;
    function GetDBSQLDialect: Integer;
    function GetIdleTimer: Integer;
    function GetIsReadOnly: Boolean;
    function GetSQLDialect: Integer;
    function GetSQLObject(Index: Integer): TMDOBase;
    function GetSQLObjectCount: Integer;
    function GetTransaction(Index: Integer): TMDOTransaction;
    function GetTransactionCount: Integer;
    procedure InternalClose(Force: Boolean);
    function Login: Boolean;
    procedure RemoveSQLObject(Idx: Integer);
    procedure RemoveSQLObjects;
    procedure SetDatabaseName(const Value: TMDOFileName);
    procedure SetDBParamByDPB(const Idx: Integer; Value: string);
    procedure SetDBParams(Value: TStrings);
    procedure SetDefaultTransaction(Value: TMDOTransaction);
    procedure SetIdleTimer(Value: Integer);
    procedure SetMDOClientLib(const Value: TMDOClientLib);
    procedure SetSQLDialect(const Value: Integer);
    procedure TimeoutConnection(Sender: TObject);
    procedure ValidateClientSQLDialect;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); 
            override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddTransaction(TR: TMDOTransaction): Integer;
    procedure ApplyUpdates(const DataSets: array of TDataSet);
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    procedure CheckActive;
    procedure CheckDatabaseName;
    procedure CheckInactive;
    procedure CloseDataSets;
    procedure CreateDatabase;
    procedure DropDatabase;
    function FindDefaultTransaction: TMDOTransaction;
    function FindTransaction(TR: TMDOTransaction): Integer;
    procedure ForceClose;
    procedure GetFieldNames(const TableName: string; List: TStrings);
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function IndexOfDBConst(st: String): Integer;
    procedure RemoveTransaction(Idx: Integer);
    procedure RemoveTransactions;
    procedure SetHandle(Value: TISC_DB_HANDLE);
    function TestConnected: Boolean;
    property DBParamByDPB[const Idx: Integer]: string read GetDBParamByDPB 
            write SetDBParamByDPB;
    property Handle: TISC_DB_HANDLE read FHandle;
    property HandleIsShared: Boolean read FHandleIsShared;
    property InternalTransaction: TMDOTransaction read FInternalTransaction;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property SQLObjectCount: Integer read GetSQLObjectCount;
    property SQLObjects[Index: Integer]: TMDOBase read GetSQLObject;
    property TransactionCount: Integer read GetTransactionCount;
    property Transactions[Index: Integer]: TMDOTransaction read GetTransaction;
  published
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write 
            FAfterDisconnect;
    property AllowStreamedConnected: Boolean read FAllowStreamedConnected write
            FAllowStreamedConnected default true;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write 
            FBeforeConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write 
            FBeforeDisconnect;
    property ClientLib: TMDOClientLib read FMDOClientLib write SetMDOClientLib default clAutoDetect;
    property Connected;
    property DatabaseName: TMDOFileName read FDBName write SetDatabaseName;
    property DBSQLDialect: Integer read FDBSQLDialect;
    property DefaultTransaction: TMDOTransaction read FDefaultTransaction write 
            SetDefaultTransaction;
    property IdleTimer: Integer read GetIdleTimer write SetIdleTimer;
    property LoginPrompt default True;
    property OnClientLibChange: TMDOChangeLibraryEvent read FOnClientLibChange 
            write FOnClientLibChange;
    property OnDialectDowngradeWarning: TNotifyEvent read 
            FOnDialectDowngradeWarning write FOnDialectDowngradeWarning;
    property OnIdleTimer: TNotifyEvent read FOnIdleTimer write FOnIdleTimer;
    property OnLogin: TMDODatabaseLoginEvent read FOnLogin write FOnLogin;
    property Params: TStrings read FDBParams write SetDBParams;
    property SQLDialect: Integer read GetSQLDialect write SetSQLDialect;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
  end;
  
  { TMDOTransaction }

  TTransactionAction = (TARollback, TACommit, TARollbackRetaining, TACommitRetaining);

  TMDOTransaction = class (TComponent)
  private
    FExplicitEndTransaction: boolean;
    FAfterCommit: TNotifyEvent;
    FAfterCommitRetaining: TNotifyEvent;
    FAfterRollback: TNotifyEvent;
    FAfterRollBackRetaining: TNotifyEvent;
    FAutoCommit: Boolean;
    FBeforeCommit: TNotifyEvent;
    FBeforeCommitRetaining: TNotifyEvent;
    FBeforeRollback: TNotifyEvent;
    FBeforeRollBackRetaining: TNotifyEvent;
    FCanTimeout: Boolean;
    FDatabases: TList;
    FDefaultAction: TTransactionAction;
    FDefaultDatabase: TMDODatabase;
    FFBLoaded: Boolean;
    FHandle: TISC_TR_HANDLE;
    FHandleIsShared: Boolean;
    FOnEndTransaction: TNotifyEvent;
    FOnIdleTimer: TNotifyEvent;
    FOnStartTransaction: TNotifyEvent;
    FSQLObjects: TList;
    FStreamedActive: Boolean;
    FTimer: TTimer;
    FTPB: PChar;
    FTPBLength: Short;
    FTRParams: TStrings;
    FTRParamsChanged: Boolean;
    function AddSQLObject(ds: TMDOBase): Integer;
    procedure BeforeDatabaseDisconnect(DB: TMDODatabase);
    procedure EndTransaction(Action: TTransactionAction; Force: Boolean);
    procedure EnsureNotInTransaction;
    function GetDatabase(Index: Integer): TMDODatabase;
    function GetDatabaseCount: Integer;
    function GetIdleTimer: Integer;
    function GetInTransaction: Boolean;
    function GetSQLObject(Index: Integer): TMDOBase;
    function GetSQLObjectCount: Integer;
    procedure RemoveSQLObject(Idx: Integer);
    procedure RemoveSQLObjects;
    procedure SetActive(Value: Boolean);
    procedure SetDefaultAction(Value: TTransactionAction);
    procedure SetDefaultDatabase(Value: TMDODatabase);
    procedure SetIdleTimer(Value: Integer);
    procedure SetTRParams(Value: TStrings);
    procedure TimeoutTransaction(Sender: TObject);
    procedure TRParamsChange(Sender: TObject);
    procedure TRParamsChanging(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
            override;
    procedure SetHandle(Value: TISC_TR_HANDLE);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddDatabase(db: TMDODatabase): Integer;
    procedure ApplyDefaultAction;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    procedure CheckDatabasesInList;
    procedure CheckInTransaction;
    procedure CheckNotInTransaction;
    procedure Commit;
    procedure CommitRetaining;
    function FindDatabase(db: TMDODatabase): Integer;
    function FindDefaultDatabase: TMDODatabase;
    procedure RemoveDatabase(Idx: Integer);
    procedure RemoveDatabases;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure StartTransaction;
    property DatabaseCount: Integer read GetDatabaseCount;
    property Databases[Index: Integer]: TMDODatabase read GetDatabase;
    property Handle: TISC_TR_HANDLE read FHandle;
    property HandleIsShared: Boolean read FHandleIsShared;
    property InTransaction: Boolean read GetInTransaction;
    property SQLObjectCount: Integer read GetSQLObjectCount;
    property SQLObjects[Index: Integer]: TMDOBase read GetSQLObject;
    property TPB: PChar read FTPB;
    property TPBLength: Short read FTPBLength;
  published
    property Active: Boolean read GetInTransaction write SetActive;
    property AfterCommit: TNotifyEvent read FAfterCommit write FAfterCommit;
    property AfterCommitRetaining: TNotifyEvent read FAfterCommitRetaining 
            write FAfterCommitRetaining;
    property AfterRollback: TNotifyEvent read FAfterRollback write 
            FAfterRollback;
    property AfterRollBackRetaining: TNotifyEvent read FAfterRollBackRetaining 
            write FAfterRollBackRetaining;
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;
    property BeforeCommit: TNotifyEvent read FBeforeCommit write FBeforeCommit;
    property BeforeCommitRetaining: TNotifyEvent read FBeforeCommitRetaining 
            write FBeforeCommitRetaining;
    property BeforeRollback: TNotifyEvent read FBeforeRollback write 
            FBeforeRollback;
    property BeforeRollBackRetaining: TNotifyEvent read 
            FBeforeRollBackRetaining write FBeforeRollBackRetaining;
    property DefaultAction: TTransactionAction read FDefaultAction write 
            SetDefaultAction default taCommit;
    property DefaultDatabase: TMDODatabase read FDefaultDatabase write 
            SetDefaultDatabase;
    property IdleTimer: Integer read GetIdleTimer write SetIdleTimer default 0;
    property OnEndTransaction: TNotifyEvent read FOnEndTransaction write 
            FOnEndTransaction;
    property OnIdleTimer: TNotifyEvent read FOnIdleTimer write FOnIdleTimer;
    property OnStartTransaction: TNotifyEvent read FOnStartTransaction write 
            FOnStartTransaction;
    property Params: TStrings read FTRParams write SetTRParams;
  end;
  
  { TMDOBase }

  { Virtually all components in IB are "descendents" of TMDOBase.
    It is to more easily manage the database and transaction
    connections. }
  TMDOBase = class (TObject)
  protected
    FAfterDatabaseDisconnect: TNotifyEvent;
    FAfterTransactionEnd: TNotifyEvent;
    FBeforeDatabaseDisconnect: TNotifyEvent;
    FBeforeTransactionEnd: TNotifyEvent;
    FDatabase: TMDODatabase;
    FIndexInDatabase: Integer;
    FIndexInTransaction: Integer;
    FOnDatabaseFree: TNotifyEvent;
    FOnTransactionFree: TNotifyEvent;
    FOwner: TObject;
    FTransaction: TMDOTransaction;
    procedure DoAfterDatabaseDisconnect; virtual;
    procedure DoAfterTransactionEnd; virtual;
    procedure DoBeforeDatabaseDisconnect; virtual;
    procedure DoBeforeTransactionEnd; virtual;
    procedure DoDatabaseFree; virtual;
    procedure DoTransactionFree; virtual;
    function GetDBHandle: PISC_DB_HANDLE; virtual;
    function GetTRHandle: PISC_TR_HANDLE; virtual;
    procedure SetDatabase(Value: TMDODatabase); virtual;
    procedure SetTransaction(Value: TMDOTransaction); virtual;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure CheckDatabase; virtual;
    procedure CheckTransaction; virtual;
    property AfterDatabaseDisconnect: TNotifyEvent read 
            FAfterDatabaseDisconnect write FAfterDatabaseDisconnect;
    property AfterTransactionEnd: TNotifyEvent read FAfterTransactionEnd write 
            FAfterTransactionEnd;
    property BeforeDatabaseDisconnect: TNotifyEvent read 
            FBeforeDatabaseDisconnect write FBeforeDatabaseDisconnect;
    property BeforeTransactionEnd: TNotifyEvent read FBeforeTransactionEnd 
            write FBeforeTransactionEnd;
    property Database: TMDODatabase read FDatabase write SetDatabase;
    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
    property OnDatabaseFree: TNotifyEvent read FOnDatabaseFree write 
            FOnDatabaseFree;
    property OnTransactionFree: TNotifyEvent read FOnTransactionFree write 
            FOnTransactionFree;
    property Owner: TObject read FOwner;
    property Transaction: TMDOTransaction read FTransaction write 
            SetTransaction;
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
  end;
  
procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);


implementation

uses MDOIntf, MDOSQLMonitor, MDOCustomDataSet, MDODatabaseInfo, MDOSQL,
  MDOUtils, typInfo;

{ TMDODatabase }

{
********************************* TMDODataBase *********************************
}
constructor TMDODataBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFBLoaded := False;

  CheckFBLoaded;

  FFBLoaded := True;
  LoginPrompt := True;
  FSQLObjects := TList.Create;
  FTransactions := TList.Create;
  FDBName := '';
  FDBParams := TStringList.Create;
  FDBParamsChanged := True;
  TStringList(FDBParams).OnChange := DBParamsChange;
  TStringList(FDBParams).OnChanging := DBParamsChanging;
  FDPB := nil;
  FHandle := nil;
  FUserNames := nil;
  FInternalTransaction := TMDOTransaction.Create(self);
  FInternalTransaction.DefaultDatabase := Self;
  FInternalTransaction.Params.Add('write');
  FInternalTransaction.Params.Add('nowait');
  FInternalTransaction.Params.Add('read_committed');
  FInternalTransaction.Params.Add('rec_version');
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 0;
  FTimer.OnTimer := TimeoutConnection;
  FDBSQLDialect := 3;
  FSQLDialect := 3;
  FTraceFlags := [];
  FAllowStreamedConnected := true;
end;

destructor TMDODataBase.Destroy;
var
  i: Integer;
begin
  if FFBLoaded then
  begin
    IdleTimer := 0;
    if FHandle <> nil then
      ForceClose;
    for i := 0 to FSQLObjects.Count - 1 do
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoDatabaseFree;
    RemoveSQLObjects;
    RemoveTransactions;
    FInternalTransaction.Free;
    FreeMem(FDPB);
    FDPB := nil;
    FDBParams.Free;
    FSQLObjects.Free;
    FUserNames.Free;
    FTransactions.Free;
  end;
  inherited Destroy;
end;

function TMDODataBase.AddSQLObject(ds: TMDOBase): Integer;
begin
  result := 0;
  if (ds.Owner is TMDOCustomDataSet) then
      RegisterClient(TDataSet(ds.Owner));
  while (result < FSQLObjects.Count) and (FSQLObjects[result] <> nil) do
    Inc(result);
  if (result = FSQLObjects.Count) then
    FSQLObjects.Add(ds)
  else
    FSQLObjects[result] := ds;
end;

function TMDODataBase.AddTransaction(TR: TMDOTransaction): Integer;
begin
  result := FindTransaction(TR);
  if result <> -1 then
  begin
    result := -1;
    exit;
  end;
  result := 0;
  while (result < FTransactions.Count) and (FTransactions[result] <> nil) do
    Inc(result);
  if (result = FTransactions.Count) then
    FTransactions.Add(TR)
  else
    FTransactions[result] := TR;
end;

procedure TMDODataBase.ApplyUpdates(const DataSets: array of TDataSet);
var
  I: Integer;
  DS: TMDOCustomDataSet;
  TR: TMDOTransaction;
begin
  TR := nil;
  for I := 0 to High(DataSets) do
  begin
    DS := TMDOCustomDataSet(DataSets[I]);
    if DS.Database <> Self then
      MDOError(mdoeUpdateWrongDB, [nil]);
    if TR = nil then
      TR := DS.Transaction;
    if (DS.Transaction <> TR) or (TR = nil) then
      MDOError(mdoeUpdateWrongTR, [nil]);
  end;
  TR.CheckInTransaction;
  for I := 0 to High(DataSets) do
  begin
    DS := TMDOCustomDataSet(DataSets[I]);
    DS.ApplyUpdates;
  end;
  TR.CommitRetaining;
end;

function TMDODataBase.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): 
        ISC_STATUS;
begin
  result := ErrCode;
  FCanTimeout := False;
  if RaiseError and (ErrCode > 0) then
    MDODatabaseError;
end;

procedure TMDODataBase.CheckActive;
begin
  if StreamedConnected and (not Connected) then
    Loaded;
  if FHandle = nil then
    MDOError(mdoeDatabaseClosed, [nil]);
end;

procedure TMDODataBase.CheckDatabaseName;
begin
  if (FDBName = '') then
    MDOError(mdoeDatabaseNameMissing, [nil]);
end;

procedure TMDODataBase.CheckInactive;
begin
  if FHandle <> nil then
    MDOError(mdoeDatabaseOpen, [nil]);
end;

procedure TMDODataBase.CloseDataSets;
var
  i: Integer;
begin
  for i := 0 to DataSetCount - 1 do
    if (DataSets[i] <> nil) then
      DataSets[i].close;
end;

procedure TMDODataBase.CreateDatabase;
var
  tr_handle: TISC_TR_HANDLE;
begin
  CheckInactive;
  tr_handle := nil;
  Call(
    isc_dsql_execute_immediate(StatusVector, @FHandle, @tr_handle, 0,
                               PChar('CREATE DATABASE ''' + FDBName + ''' ' + {do not localize}
                               Params.Text), SQLDialect, nil),
    True);
end;

procedure TMDODataBase.DBParamsChange(Sender: TObject);
begin
  FDBParamsChanged := True;
end;

procedure TMDODataBase.DBParamsChanging(Sender: TObject);
begin
  EnsureInactive;
  CheckInactive;
end;

procedure TMDODataBase.DoConnect;
var
  DPB: string;
  TempDBParams: TStrings;
begin
  { Call BeforeConnect Event if assigned }
  if Assigned(FBeforeConnect) then
    FBeforeConnect(Self);
  CheckInactive;
  CheckDatabaseName;
  if (not LoginPrompt) and (FHiddenPassword <> '') then
  begin
    FHiddenPassword := '';
    FDBParamsChanged := True;
  end;
  { Use builtin login prompt if requested }
  if LoginPrompt and not Login then
    MDOError(mdoeOperationCancelled, [nil]);
  { Generate a new DPB if necessary }
  if (FDBParamsChanged) then
  begin
    FDBParamsChanged := False;
    if (not LoginPrompt) or (FHiddenPassword = '') then
      GenerateDPB(FDBParams, DPB, FDPBLength)
    else
    begin
      TempDBParams := TStringList.Create;
      try
       TempDBParams.Assign(FDBParams);
       TempDBParams.Add('password=' + FHiddenPassword);
       GenerateDPB(TempDBParams, DPB, FDPBLength);
      finally
       TempDBParams.Free;
      end;
    end;
    MDOAlloc(FDPB, 0, FDPBLength);
    Move(DPB[1], FDPB[0], FDPBLength);
  end;
  if Call(isc_attach_database(StatusVector, Length(FDBName),
                         PChar(FDBName), @FHandle,
                         FDPBLength, FDPB), False) > 0 then
  begin
    FHandle := nil;
    MDODatabaseError;
  end;
  FDBSQLDialect := GetDBSQLDialect;
  ValidateClientSQLDialect;
  if Assigned(FAfterConnect) then
    FAfterConnect(Self);
  if not (csDesigning in ComponentState) then
    MonitorHook.DBConnect(Self);
  if FDefaultTransaction <> nil then
    if FDefaultTransaction.AutoCommit then
      FDefaultTransaction.StartTransaction;
  
end;

procedure TMDODataBase.DoDisconnect;
begin
  if Connected then
    InternalClose(False);
  FDBSQLDialect := 3;
end;

procedure TMDODataBase.DropDatabase;
begin
  CheckActive;
  Call(isc_drop_database(StatusVector, @FHandle), True);
end;

procedure TMDODataBase.EnsureInactive;
begin
  if csDesigning in ComponentState then
  begin
    if FHandle <> nil then
      Close;
  end
end;

function TMDODataBase.FindDefaultTransaction: TMDOTransaction;
var
  i: Integer;
begin
  result := FDefaultTransaction;
  if result = nil then
  begin
    for i := 0 to FTransactions.Count - 1 do
      if (Transactions[i] <> nil) and
        (TMDOTransaction(Transactions[i]).DefaultDatabase = self) and
        (TMDOTransaction(Transactions[i]) <> FInternalTransaction) then
       begin
         result := TMDOTransaction(Transactions[i]);
         break;
       end;
  end;
end;

function TMDODataBase.FindTransaction(TR: TMDOTransaction): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to FTransactions.Count - 1 do
    if TR = Transactions[i] then
    begin
      result := i;
      break;
    end;
end;

procedure TMDODataBase.ForceClose;
begin
  if Connected then
    InternalClose(True);
end;

function TMDODataBase.GetConnected: Boolean;
begin
  result := FHandle <> nil;
end;

function TMDODataBase.GetDBParamByDPB(const Idx: Integer): string;
var
  ConstIdx, EqualsIdx: Integer;
begin
  if (Idx > 0) and (Idx <= isc_dpb_last_dpb_constant) then
  begin
    ConstIdx := IndexOfDBConst(DPBConstantNames[Idx]);
    if ConstIdx = -1 then
      result := ''
    else
    begin
      result := Params[ConstIdx];
      EqualsIdx := Pos('=', result); {mbcs ok}
      if EqualsIdx = 0 then
        result := ''
      else
        result := Copy(result, EqualsIdx + 1, Length(result));
    end;
  end
  else
    result := '';
end;

function TMDODataBase.GetDBSQLDialect: Integer;
var
  DatabaseInfo: TMDODatabaseInfo;
begin
  DatabaseInfo := TMDODatabaseInfo.Create(self);
  DatabaseInfo.Database := self;
  result := DatabaseInfo.DBSQLDialect;
  DatabaseInfo.Free;
end;


procedure TMDODataBase.GetFieldNames(const TableName: string; List: TStrings);
var
  Query: TMDOSQL;
begin
  if TableName = '' then
    MDOError(mdoeNoTableName, [nil]);
  if not Connected then
    Open;
  if not FInternalTransaction.Active then
    FInternalTransaction.StartTransaction;
  Query := TMDOSQL.Create(self);
  try
    Query.GoToFirstRecordOnExecute := False;
    Query.Database := Self;
    Query.Transaction := FInternalTransaction;
    Query.SQL.Text := 'Select R.RDB$FIELD_NAME ' + {do not localize}
      'from RDB$RELATION_FIELDS R, RDB$FIELDS F ' + {do not localize}
      'where Upper(R.RDB$RELATION_NAME) = ' + {do not localize}
      '''' +
      FormatIdentifierValue(SQLDialect, TableName) +
      ''' ' +
      'and R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+ {do not localize}
      'order by R.RDB$FIELD_POSITION ';
    Query.Prepare;
    Query.ExecQuery;
    with List do
    begin
      BeginUpdate;
      try
        Clear;
        while (not Query.EOF) and (Query.Next <> nil) do
          List.Add(TrimRight(Query.Current.ByName('RDB$FIELD_NAME').AsString)); {do not localize}
      finally
        EndUpdate;
      end;
    end;
  finally
    Query.free;
    FInternalTransaction.Commit;
  end;
end;


function TMDODataBase.GetIdleTimer: Integer;
begin
  result := FTimer.Interval;
end;

function TMDODataBase.GetIsReadOnly: Boolean;
var
  DatabaseInfo: TMDODatabaseInfo;
begin
  DatabaseInfo := TMDODatabaseInfo.Create(self);
  DatabaseInfo.Database := self;
  if (DatabaseInfo.ODSMajorVersion < 10) then
    result := false
  else
  begin
    if (DatabaseInfo.ReadOnly = 0) then
      result := false
    else
      result := true;
  end;
  DatabaseInfo.Free;
end;

function TMDODataBase.GetSQLDialect: Integer;
begin
  Result := FSQLDialect;
end;

function TMDODataBase.GetSQLObject(Index: Integer): TMDOBase;
begin
  result := FSQLObjects[Index];
end;

function TMDODataBase.GetSQLObjectCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
    Inc(result);
end;

procedure TMDODataBase.GetTableNames(List: TStrings; SystemTables: Boolean = 
        False);
var
  Query: TMDOSQL;
begin
  if not (csReading in ComponentState) then
  begin
    if not Connected then
      Open;
    if not FInternalTransaction.Active then
      FInternalTransaction.StartTransaction;
    Query := TMDOSQL.Create(self);
    try
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := Self;
      Query.Transaction := FInternalTransaction;
      if SystemTables then
        Query.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS' + {do not localize}
                          ' WHERE RDB$VIEW_BLR IS NULL' {do not localize}
      else
        Query.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS' + {do not localize}
                          ' WHERE RDB$VIEW_BLR IS NULL AND RDB$SYSTEM_FLAG = 0'; {do not localize}
  
      //query.sql.savetofile('C:\Delphi\Lib\Mercury\test\GetTableNames.txt');
  
      Query.Prepare;
      Query.ExecQuery;
      with List do
      begin
        BeginUpdate;
        try
          Clear;
          while (not Query.EOF) and (Query.Next <> nil) do
            List.Add(TrimRight(Query.Current[0].AsString));
        finally
          EndUpdate;
        end;
      end;
    finally
      Query.Free;
      FInternalTransaction.Commit;
    end;
  end;
end;

function TMDODataBase.GetTransaction(Index: Integer): TMDOTransaction;
begin
  result := FTransactions[Index];
end;

function TMDODataBase.GetTransactionCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FTransactions.Count - 1 do
    if FTransactions[i] <> nil then
      Inc(result);
end;

function TMDODataBase.IndexOfDBConst(st: String): Integer;
var
  i, pos_of_str: Integer;
begin
  result := -1;
  for i := 0 to Params.Count - 1 do
  begin
    pos_of_str := Pos(st, AnsiLowerCase(Params[i])); {mbcs ok}
    if (pos_of_str = 1) or (pos_of_str = Length(DPBPrefix) + 1) then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TMDODataBase.InternalClose(Force: Boolean);
var
  i: Integer;
begin
  CheckActive;
  
  { Call BeforeDisconnect Event if assigned }
  if (Assigned(FBeforeDisconnect)) and (not Force) then
    FBeforeDisconnect(Self);
  
  { Tell all connected transactions that we're disconnecting.
    This is so transactions can commit/rollback, accordingly }
  for i := 0 to FTransactions.Count - 1 do
  begin
    try
      if FTransactions[i] <> nil then
        Transactions[i].BeforeDatabaseDisconnect(Self);
    except
      if not Force then
        raise;
    end;
  end;
  for i := 0 to FSQLObjects.Count - 1 do
  begin
    try
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoBeforeDatabaseDisconnect;
    except
      if not Force then
        raise;
    end;
  end;
  
  if (not HandleIsShared) and
     (Call(isc_detach_database(StatusVector, @FHandle), False) > 0) and
     (not Force) then
    MDODatabaseError
  else
  begin
    FHandle := nil;
    FHandleIsShared := False;
  end;
  
  if not (csDesigning in ComponentState) then
    MonitorHook.DBDisconnect(Self);
  
  for i := 0 to FSQLObjects.Count - 1 do
    if FSQLObjects[i] <> nil then
      SQLObjects[i].DoAfterDatabaseDisconnect;
  
  { Call AfterDisconnect Event if assigned }
  if (Assigned(FAfterDisconnect)) and (not Force) then
    FAfterDisconnect(Self);
  
  
end;

procedure TMDODataBase.Loaded;
var
  i: Integer;
begin
  try
    if (not FAllowStreamedConnected) and
       (not (csDesigning in ComponentState)) then
    begin
      StreamedConnected := false;
      for i := 0 to FTransactions.Count - 1 do
        if  FTransactions[i] <> nil then
          with TMDOTransaction(FTransactions[i]) do
            FStreamedActive := False;
    end;
    if StreamedConnected and (not Connected) then
    begin
      inherited Loaded;
      for i := 0 to FTransactions.Count - 1 do
        if  FTransactions[i] <> nil then
        begin
          with TMDOTransaction(FTransactions[i]) do
            if not Active then
              if FStreamedActive and not InTransaction then
              begin
                StartTransaction;
                FStreamedActive := False;
              end;
        end;
      if (FDefaultTransaction <> nil) and
         (FDefaultTransaction.FStreamedActive) and
         (not FDefaultTransaction.InTransaction) then
        FDefaultTransaction.StartTransaction;
      StreamedConnected := False;
    end;
  except
    if csDesigning in ComponentState then
    {$IFDEF MDO_DELPHI5}
      if Assigned(Self) then
    {$ENDIF}
    {$IFDEF MDO_DELPHI6_UP}
      if Assigned(ApplicationHandleException) then
    {$ENDIF}
        Application.HandleException(Self)
    else
      raise;
  end;
end;

function TMDODataBase.Login: Boolean;
var
  IndexOfUser, IndexOfPassword: Integer;
  Username, Password, OldPassword: string;
  LoginParams: TStrings;
  
  procedure HidePassword;
  var
    I: Integer;
    IndexAt: Integer;
  begin
    IndexAt := 0;
    for I := 0 to Params.Count -1 do
      if Pos('password', LowerCase(Trim(Params.Names[i]))) = 1 then {mbcs ok}
      begin
        FHiddenPassword := Params.Values[Params.Names[i]];
        IndexAt := I;
        break;
      end;
    if IndexAt <> 0 then
      Params.Delete(IndexAt);
  end;
  
begin
  if Assigned(FOnLogin) then
  begin
    result := True;
    LoginParams := TStringList.Create;
    try
      LoginParams.Assign(Params);
      FOnLogin(Self, LoginParams);
      Params.Assign (LoginParams);
      HidePassword;
    finally
      LoginParams.Free;
    end;
  end
  else
  begin
    IndexOfUser := IndexOfDBConst(DPBConstantNames[isc_dpb_user_name]);
    if IndexOfUser <> -1 then
      Username := Copy(Params[IndexOfUser],
                                         Pos('=', Params[IndexOfUser]) + 1, {mbcs ok}
                                         Length(Params[IndexOfUser]));
    IndexOfPassword := IndexOfDBConst(DPBConstantNames[isc_dpb_password]);
    if IndexOfPassword <> -1 then
    begin
      Password := Copy(Params[IndexOfPassword],
                                         Pos('=', Params[IndexOfPassword]) + 1, {mbcs ok}
                                         Length(Params[IndexOfPassword]));
      OldPassword := password;
    end;
    result := LoginDialogEx(DatabaseName, Username, Password, False);
    if result then
    begin
      if IndexOfUser = -1 then
        Params.Add(DPBConstantNames[isc_dpb_user_name] + '=' + Username)
      else
        Params[IndexOfUser] := DPBConstantNames[isc_dpb_user_name] +
                                 '=' + Username;
      if (Password = OldPassword) then
        FHiddenPassword := ''
      else
      begin
        FHiddenPassword := Password;
        if OldPassword <> '' then
          HidePassword;
      end;
    end;
  end;
end;

procedure TMDODataBase.Notification(AComponent: TComponent; Operation: 
        TOperation);
var
  i: Integer;
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDefaultTransaction) then
  begin
    i := FindTransaction(FDefaultTransaction);
    if (i <> -1) then
      RemoveTransaction(i);
    FDefaultTransaction := nil;
  end;
end;

procedure TMDODataBase.RemoveSQLObject(Idx: Integer);
var
  ds: TMDOBase;
begin
  if (Idx >= 0) and (FSQLObjects[Idx] <> nil) then
  begin
    ds := SQLObjects[Idx];
    FSQLObjects[Idx] := nil;
    ds.Database := nil;
    if (ds.owner is TDataSet) then
      UnregisterClient(TDataSet(ds.Owner));
  end;
end;

procedure TMDODataBase.RemoveSQLObjects;
var
  i: Integer;
begin
  for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
  begin
    RemoveSQLObject(i);
    if (TMDOBase(FSQLObjects[i]).owner is TDataSet) then
      UnregisterClient(TDataSet(TMDOBase(FSQLObjects[i]).owner));
  end;
end;

procedure TMDODataBase.RemoveTransaction(Idx: Integer);
var
  TR: TMDOTransaction;
begin
  if ((Idx >= 0) and (FTransactions[Idx] <> nil)) then
  begin
    TR := Transactions[Idx];
    FTransactions[Idx] := nil;
    TR.RemoveDatabase(TR.FindDatabase(Self));
    if TR = FDefaultTransaction then
      FDefaultTransaction := nil;
  end;
end;

procedure TMDODataBase.RemoveTransactions;
var
  i: Integer;
begin
  for i := 0 to FTransactions.Count - 1 do if FTransactions[i] <> nil then
    RemoveTransaction(i);
end;

procedure TMDODataBase.SetDatabaseName(const Value: TMDOFileName);
begin
  if FDBName <> Value then
  begin
    EnsureInactive;
    CheckInactive;
    FDBName := Value;
  end;
end;

procedure TMDODataBase.SetDBParamByDPB(const Idx: Integer; Value: string);
var
  ConstIdx: Integer;
begin
  ConstIdx := IndexOfDBConst(DPBConstantNames[Idx]);
  if (Value = '') then
  begin
    if ConstIdx <> -1 then
      Params.Delete(ConstIdx);
  end
  else
  begin
    if (ConstIdx = -1) then
      Params.Add(DPBConstantNames[Idx] + '=' + Value)
    else
      Params[ConstIdx] := DPBConstantNames[Idx] + '=' + Value;
  end;
end;

procedure TMDODataBase.SetDBParams(Value: TStrings);
begin
  FDBParams.Assign(Value);
end;

procedure TMDODataBase.SetDefaultTransaction(Value: TMDOTransaction);
var
  i: Integer;
begin
  if (FDefaultTransaction <> nil) and (FDefaultTransaction <> Value) then
  begin
    i := FindTransaction(FDefaultTransaction);
    if (i <> -1) and (FDefaultTransaction.DefaultDatabase <> self) then
      RemoveTransaction(i);
  end;
  if (Value <> nil) and (FDefaultTransaction <> Value) then
  begin
    Value.AddDatabase(Self);
    AddTransaction(Value);
  end;
  FDefaultTransaction := Value;
end;

procedure TMDODataBase.SetHandle(Value: TISC_DB_HANDLE);
begin
  if HandleIsShared then
    Close
  else
    CheckInactive;
  FHandle := Value;
  FHandleIsShared := (Value <> nil);
end;

procedure TMDODataBase.SetIdleTimer(Value: Integer);
begin
  if Value < 0 then
    MDOError(mdoeTimeoutNegative, [nil])
  else if (Value = 0) then
  begin
    FTimer.Enabled := False;
    FTimer.Interval := 0;
  end
  else if (Value > 0) then
  begin
    FTimer.Interval := Value;
    if not (csDesigning in ComponentState) then
      FTimer.Enabled := True;
  end;
end;

procedure TMDODataBase.SetMDOClientLib(const Value: TMDOClientLib);
var
  bAbort: Boolean;
begin

  if (FMDOClientLib <> Value) then
  begin
    FMDOClientLib := Value;
    if (FBASE_CURRENT_DLL = '') or
       (
        (FBASE_CURRENT_DLL <> MDOClientLibrary[FMDOClientLib]) and
        (FMDOClientLib <> clAutodetect)
       ) then
    begin
      bAbort := False;
      if Assigned(FOnClientLibChange) then
        FOnClientLibChange(FMDOClientLib, bAbort);
        if not bAbort then
        begin
          if FFBLoaded then
          begin
            if Connected then
              InternalClose(True);
            FreeFBLibrary;
            FBASE_CURRENT_DLL := MDOClientLibrary[FMDOClientLib];
          end;
          CheckFBLoaded;
        end;
    end;

  end;

end;

procedure TMDODataBase.SetSQLDialect(const Value: Integer);
begin
  if (Value < 1) then
    MDOError(mdoeSQLDialectInvalid, [nil]);
  if ((FHandle = nil) or (Value <= FDBSQLDialect))  then
    FSQLDialect := Value
  else
    MDOError(mdoeSQLDialectInvalid, [nil]);
end;

function TMDODataBase.TestConnected: Boolean;
var
  DatabaseInfo: TMDODatabaseInfo;
begin
  result := Connected;
  if result then
  begin
    DatabaseInfo := TMDODatabaseInfo.Create(self);
    try
      DatabaseInfo.Database := self;
      { poke the server to see if connected }
      if DatabaseInfo.BaseLevel = 0 then ;
      DatabaseInfo.Free;
    except
      ForceClose;
      result := False;
      DatabaseInfo.Free;
    end;
  end;
end;

procedure TMDODataBase.TimeoutConnection(Sender: TObject);
begin
  if Connected then
  begin
    if FCanTimeout then
    begin
      ForceClose;
      if Assigned(FOnIdleTimer) then
        FOnIdleTimer(Self);
    end
    else
      FCanTimeout := True;
  end;
end;

procedure TMDODataBase.ValidateClientSQLDialect;
begin
  if (FDBSQLDialect < FSQLDialect) then
  begin
    FSQLDialect := FDBSQLDialect;
    if Assigned (FOnDialectDowngradeWarning) then
      FOnDialectDowngradeWarning(self);
  end;
end;


//:?: Responsável por obter o nome das tabelas... checar por erros de sintaxe
{ TMDOTransaction }

{
******************************* TMDOTransaction ********************************
}
constructor TMDOTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoCommit := False;
  FFBLoaded := False;
  CheckFBLoaded;
  FFBLoaded := True;
  FDatabases := TList.Create;
  FSQLObjects := TList.Create;
  FHandle := nil;
  FTPB := nil;
  FTPBLength := 0;
  FTRParams := TStringList.Create;
  FTRParamsChanged := True;
  TStringList(FTRParams).OnChange := TRParamsChange;
  TStringList(FTRParams).OnChanging := TRParamsChanging;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 0;
  FTimer.OnTimer := TimeoutTransaction;
  FDefaultAction := taCommit;
end;

destructor TMDOTransaction.Destroy;
var
  i: Integer;
begin
  if FFBLoaded then
  begin
    if InTransaction then
      EndTransaction(FDefaultAction, True);
    for i := 0 to FSQLObjects.Count - 1 do
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoTransactionFree;
    RemoveSQLObjects;
    RemoveDatabases;
    FreeMem(FTPB);
    FTPB := nil;
    FTRParams.Free;
    FSQLObjects.Free;
    FDatabases.Free;
  end;
  inherited Destroy;
end;

function TMDOTransaction.AddDatabase(db: TMDODatabase): Integer;
var
  i: Integer;
  NilFound: Boolean;
begin
  i := FindDatabase(db);
  if i <> -1 then
  begin
    result := i;
    exit;
  end;
  NilFound := False;
  i := 0;
  while (not NilFound) and (i < FDatabases.Count) do
  begin
    NilFound := (FDatabases[i] = nil);
    if (not NilFound) then
      Inc(i);
  end;
  if (NilFound) then
  begin
    FDatabases[i] := db;
    result := i;
  end
  else
  begin
    result := FDatabases.Count;
    FDatabases.Add(db);
  end;
end;

function TMDOTransaction.AddSQLObject(ds: TMDOBase): Integer;
begin
  result := 0;
  while (result < FSQLObjects.Count) and (FSQLObjects[result] <> nil) do
    Inc(result);
  if (result = FSQLObjects.Count) then
    FSQLObjects.Add(ds)
  else
    FSQLObjects[result] := ds;
end;

procedure TMDOTransaction.BeforeDatabaseDisconnect(DB: TMDODatabase);
begin
  if InTransaction then
    EndTransaction(FDefaultAction, True);
end;

function TMDOTransaction.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): 
        ISC_STATUS;
var
  i: Integer;
begin
  result := ErrCode;
  for i := 0 to FDatabases.Count - 1 do if FDatabases[i] <> nil then
    Databases[i].FCanTimeout := False;
  FCanTimeout := False;
  if RaiseError and (result > 0) then
    MDODatabaseError;
end;

procedure TMDOTransaction.CheckDatabasesInList;
begin
  if GetDatabaseCount = 0 then
    MDOError(mdoeNoDatabasesInTransaction, [nil]);
end;

procedure TMDOTransaction.CheckInTransaction;
begin
  if FStreamedActive and (not InTransaction) then
    Loaded;
  if not InTransaction then
    StartTransaction;
  if (FHandle = nil) then
    MDOError(mdoeNotInTransaction, [nil]);
end;

procedure TMDOTransaction.CheckNotInTransaction;
begin
  if (FHandle <> nil) then
    MDOError(mdoeInTransaction, [nil]);
end;

procedure TMDOTransaction.Commit;
begin
  FExplicitEndTransaction := true;
  EndTransaction(TACommit, False);
end;

procedure TMDOTransaction.CommitRetaining;
begin
  FExplicitEndTransaction := true;
  EndTransaction(TACommitRetaining, False);
end;

procedure TMDOTransaction.EndTransaction(Action: TTransactionAction; Force:
        Boolean);
var
  status: ISC_STATUS;
  i: Integer;
begin
  CheckInTransaction;
  case Action of
    TARollback, TACommit:
    begin
      if (HandleIsShared) and
         (Action <> FDefaultAction) and
         (not Force) then
        MDOError(mdoeCantEndSharedTransaction, [nil]);
  
      for i := 0 to FSQLObjects.Count - 1 do
        if FSQLObjects[i] <> nil then
          SQLObjects[i].DoBeforeTransactionEnd;
  
      if InTransaction then
      begin
        if HandleIsShared then
        begin
          FHandle := nil;
          FHandleIsShared := False;
          status := 0;
        end
        else
        begin
  
          if (Action = TARollback) then
          begin
            if (Assigned(FBeforeRollback) and (not Force)) then
              FBeforeRollback(Self);
            status := Call(isc_rollback_transaction(StatusVector, @FHandle), False);
            if (Assigned(FAfterRollback) and (not Force)) then
              FAfterRollback(Self);
          end
          else
          begin
            if (Assigned(FBeforeCommit)) and (not Force) then
              FBeforeCommit(Self);
            status := Call(isc_commit_transaction(StatusVector, @FHandle), False);
            if (Assigned(FAfterCommit)) and (not Force) then
              FAfterCommit(Self);
          end;
  
        end;
  
        if ((Force) and (status > 0)) then
          status := Call(isc_rollback_transaction(StatusVector, @FHandle), False);
        if Force then
          FHandle := nil
        else
          if (status > 0) then
            MDODatabaseError;
  
        for i := 0 to FSQLObjects.Count - 1 do
          if FSQLObjects[i] <> nil then
            SQLObjects[i].DoAfterTransactionEnd;
  
      end;
    end;
    TACommitRetaining:
    begin
      if (Assigned(FBeforeCommitRetaining)) and (not Force) then
        FBeforeCommitRetaining(Self);
      Call(isc_commit_retaining(StatusVector, @FHandle), True);
      if (Assigned(FAfterCommitRetaining)) and (not Force) then
        FAfterCommitRetaining(Self);
    end;
    TARollbackRetaining:
    begin
      if (Assigned(FBeforeRollbackRetaining)) and (not Force) then
        FBeforeRollbackRetaining(Self);
      Call(isc_rollback_retaining(StatusVector, @FHandle), True);
      if (Assigned(FAfterRollbackRetaining)) and (not Force) then
        FAfterRollbackRetaining(Self);
    end;
  end;
  if not (csDesigning in ComponentState) then
  begin
    case Action of
      TACommit:
        MonitorHook.TRCommit(Self);
      TARollback:
        MonitorHook.TRRollback(Self);
      TACommitRetaining:
        MonitorHook.TRCommitRetaining(Self);
      TARollbackRetaining:
        MonitorHook.TRRollbackRetaining(Self);
    end;
  end;
  
  if (Assigned(FOnEndTransaction)) and (not Force) and
    (Action in [TARollBack, TACommit]) then
    FOnEndTransaction(Self);

  FExplicitEndTransaction := False;

end;

procedure TMDOTransaction.EnsureNotInTransaction;
begin
  if csDesigning in ComponentState then
  begin
    if FHandle <> nil then
      Rollback;
  end;
end;

function TMDOTransaction.FindDatabase(db: TMDODatabase): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to FDatabases.Count - 1 do
    if db = TMDODatabase(FDatabases[i]) then
    begin
      result := i;
      break;
    end;
end;

function TMDOTransaction.FindDefaultDatabase: TMDODatabase;
var
  i: Integer;
begin
  result := FDefaultDatabase;
  if result = nil then
  begin
    for i := 0 to FDatabases.Count - 1 do
      if (TMDODatabase(FDatabases[i]) <> nil) and
        (TMDODatabase(FDatabases[i]).DefaultTransaction = self) then
      begin
        result := TMDODatabase(FDatabases[i]);
        break;
      end;
  end;
end;

function TMDOTransaction.GetDatabase(Index: Integer): TMDODatabase;
begin
  result := FDatabases[Index];
end;

function TMDOTransaction.GetDatabaseCount: Integer;
var
  i, Cnt: Integer;
begin
  result := 0;
  Cnt := FDatabases.Count - 1;
  for i := 0 to Cnt do if FDatabases[i] <> nil then
    Inc(result);
end;

function TMDOTransaction.GetIdleTimer: Integer;
begin
  result := FTimer.Interval;
end;

function TMDOTransaction.GetInTransaction: Boolean;
begin
  result := (FHandle <> nil);
end;

function TMDOTransaction.GetSQLObject(Index: Integer): TMDOBase;
begin
  result := FSQLObjects[Index];
end;

function TMDOTransaction.GetSQLObjectCount: Integer;
var
  i, Cnt: Integer;
begin
  result := 0;
  Cnt := FSQLObjects.Count - 1;
  for i := 0 to Cnt do if FSQLObjects[i] <> nil then
    Inc(result);
end;

procedure TMDOTransaction.Loaded;
begin
  inherited Loaded;
  {
  try
    if FStreamedActive and (not InTransaction) then
      StartTransaction;
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
  }
end;

procedure TMDOTransaction.Notification(AComponent: TComponent; Operation: 
        TOperation);
var
  i: Integer;
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDefaultDatabase) then
  begin
    i := FindDatabase(FDefaultDatabase);
    if (i <> -1) then
      RemoveDatabase(i);
    FDefaultDatabase := nil;
  end;
end;

procedure TMDOTransaction.RemoveDatabase(Idx: Integer);
var
  DB: TMDODatabase;
begin
  if ((Idx >= 0) and (FDatabases[Idx] <> nil)) then
  begin
    DB := Databases[Idx];
    FDatabases[Idx] := nil;
    DB.RemoveTransaction(DB.FindTransaction(Self));
    if DB = FDefaultDatabase then
      FDefaultDatabase := nil;
  end;
end;

procedure TMDOTransaction.RemoveDatabases;
var
  i: Integer;
begin
  for i := 0 to FDatabases.Count - 1 do if FDatabases[i] <> nil then
    RemoveDatabase(i);
end;

procedure TMDOTransaction.RemoveSQLObject(Idx: Integer);
var
  ds: TMDOBase;
begin
  if ((Idx >= 0) and (FSQLObjects[Idx] <> nil)) then
  begin
    ds := SQLObjects[Idx];
    FSQLObjects[Idx] := nil;
    ds.Transaction := nil;
  end;
end;

procedure TMDOTransaction.RemoveSQLObjects;
var
  i: Integer;
begin
  for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
    RemoveSQLObject(i);
end;

procedure TMDOTransaction.Rollback;
begin
  FExplicitEndTransaction := true;
  EndTransaction(TARollback, False);
end;

procedure TMDOTransaction.RollbackRetaining;
begin
  FExplicitEndTransaction := true;
  EndTransaction(TARollbackRetaining, False);
end;

procedure TMDOTransaction.SetActive(Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Value and not InTransaction then
      StartTransaction
    else
      if not Value and InTransaction then
        Rollback;
end;

procedure TMDOTransaction.SetDefaultAction(Value: TTransactionAction);
begin
  if (Value = taRollbackRetaining) and (GetFBClientVersion < 6) then
    MDOError(mdoeIB60feature, [nil]);
  FDefaultAction := Value;
end;

procedure TMDOTransaction.SetDefaultDatabase(Value: TMDODatabase);
var
  i: Integer;
begin
  if (FDefaultDatabase <> nil) and (FDefaultDatabase <> Value) then
  begin
    i := FDefaultDatabase.FindTransaction(self);
    if (i <> -1) then
      FDefaultDatabase.RemoveTransaction(i);
  end;
  if (Value <> nil) and (FDefaultDatabase <> Value) then
  begin
    Value.AddTransaction(Self);
    AddDatabase(Value);
    for i := 0 to FSQLObjects.Count - 1 do
      if (FSQLObjects[i] <> nil) and
         (TMDOBase(FSQLObjects[i]).Database = nil) then
        SetOrdProp(TMDOBase(FSQLObjects[i]).Owner, 'Database', Integer(Value));
  end;
  FDefaultDatabase := Value;
end;

procedure TMDOTransaction.SetHandle(Value: TISC_TR_HANDLE);
begin
  if (HandleIsShared) then
    EndTransaction(DefaultAction, True)
  else
    CheckNotInTransaction;
  FHandle := Value;
  FHandleIsShared := (Value <> nil);
end;

procedure TMDOTransaction.SetIdleTimer(Value: Integer);
begin
  if Value < 0 then
    MDOError(mdoeTimeoutNegative, [nil])
  else
    if (Value = 0) then
    begin
      FTimer.Enabled := False;
      FTimer.Interval := 0;
    end
    else
      if (Value > 0) then
      begin
        FTimer.Interval := Value;
        if not (csDesigning in ComponentState) then
          FTimer.Enabled := True;
      end;
end;

procedure TMDOTransaction.SetTRParams(Value: TStrings);
begin
  FTRParams.Assign(Value);
end;

procedure TMDOTransaction.StartTransaction;
var
  pteb: PISC_TEB_ARRAY;
  TPB: string;
  i: Integer;
begin
  { Check that we're not already in a transaction.
    Check that there is at least one database in the list. }
  CheckNotInTransaction;
  CheckDatabasesInList;
  for i := 0 to FDatabases.Count - 1 do
   if  FDatabases[i] <> nil then
   begin
     with TMDODatabase(FDatabases[i]) do
     if not Connected then
       if StreamedConnected then
       begin
         Open;
         StreamedConnected := False;
       end
       else
         MDOError(mdoeDatabaseClosed, [nil]);
   end;
  
  if FTRParamsChanged then
  begin
    FTRParamsChanged := False;
    GenerateTPB(FTRParams, TPB, FTPBLength);
    if FTPBLength > 0 then
    begin
      MDOAlloc(FTPB, 0, FTPBLength);
      Move(TPB[1], FTPB[0], FTPBLength);
    end;
  end;
  
  { Set up a database record for each database in the list. Ordinarily, each
    database could have its own set of transaction options; however, MDO makes
    a transaction have one set of transaction options.. Is this a limitation?
    Certainly. It this bad? I don't think so... }
  pteb := nil;
  MDOAlloc(pteb, 0, DatabaseCount * SizeOf(TISC_TEB));
  try
    for i := 0 to DatabaseCount - 1 do if Databases[i] <> nil then
    begin
      pteb^[i].db_handle := @(Databases[i].Handle);
      pteb^[i].tpb_length := FTPBLength;
      pteb^[i].tpb_address := FTPB;
    end;
    { Finally, start the transaction }
    if Call(isc_start_multiple(StatusVector, @FHandle,
      DatabaseCount, PISC_TEB(pteb)), False) > 0 then
    begin
      FHandle := nil;
      MDODatabaseError;
    end;
  
    if Assigned(FOnStartTransaction) then
      FOnStartTransaction(Self);
  
    if not (csDesigning in ComponentState) then
      MonitorHook.TRStart(Self);
  finally
    FreeMem(pteb);
  end;
end;

procedure TMDOTransaction.TimeoutTransaction(Sender: TObject);
begin
  if InTransaction then
  begin
    if FCanTimeout then
    begin
      EndTransaction(FDefaultAction, True);
      if Assigned(FOnIdleTimer) then
        FOnIdleTimer(Self);
    end
    else
      FCanTimeout := True;
  end;
end;

procedure TMDOTransaction.TRParamsChange(Sender: TObject);
begin
  FTRParamsChanged := True;
end;

procedure TMDOTransaction.TRParamsChanging(Sender: TObject);
begin
  EnsureNotInTransaction;
  CheckNotInTransaction;
end;



{ TMDOBase }
{
*********************************** TMDOBase ***********************************
}
constructor TMDOBase.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

destructor TMDOBase.Destroy;
begin
  SetDatabase(nil);
  SetTransaction(nil);
  inherited Destroy;
end;

procedure TMDOBase.CheckDatabase;
begin
  if (FDatabase = nil) then
    MDOError(mdoeDatabaseNotAssigned, [nil]);
  FDatabase.CheckActive;
end;

procedure TMDOBase.CheckTransaction;
begin
  if FTransaction = nil then
    MDOError(mdoeTransactionNotAssigned, [nil]);
  FTransaction.CheckInTransaction;
end;

procedure TMDOBase.DoAfterDatabaseDisconnect;
begin
  if Assigned(AfterDatabaseDisconnect) then
    AfterDatabaseDisconnect(Self);
end;

procedure TMDOBase.DoAfterTransactionEnd;
begin
  if Assigned(FAfterTransactionEnd) then
    FAfterTransactionEnd(Self);
end;

procedure TMDOBase.DoBeforeDatabaseDisconnect;
begin
  if Assigned(BeforeDatabaseDisconnect) then
    BeforeDatabaseDisconnect(Self);
end;

procedure TMDOBase.DoBeforeTransactionEnd;
begin
  if Assigned(BeforeTransactionEnd) then
    BeforeTransactionEnd(Self);
end;

procedure TMDOBase.DoDatabaseFree;
begin
  if Assigned(OnDatabaseFree) then
    OnDatabaseFree(Self);
  SetDatabase(nil);
  SetTransaction(nil);
end;

procedure TMDOBase.DoTransactionFree;
begin
  if Assigned(FOnTransactionFree) then
    FOnTransactionFree(Self);
  FTransaction := nil;
end;

function TMDOBase.GetDBHandle: PISC_DB_HANDLE;
begin
  CheckDatabase;
  result := @FDatabase.Handle;
end;

function TMDOBase.GetTRHandle: PISC_TR_HANDLE;
begin
  CheckTransaction;
  result := @FTransaction.Handle;
end;

procedure TMDOBase.SetDatabase(Value: TMDODatabase);
begin
  if (FDatabase <> nil) then
    FDatabase.RemoveSQLObject(FIndexInDatabase);
  FDatabase := Value;
  if (FDatabase <> nil) then
  begin
    FIndexInDatabase := FDatabase.AddSQLObject(Self);
    if (FTransaction = nil) then
      Transaction := FDatabase.FindDefaultTransaction;
  end;
end;

procedure TMDOBase.SetTransaction(Value: TMDOTransaction);
begin
  if (FTransaction <> nil) then
    FTransaction.RemoveSQLObject(FIndexInTransaction);
  FTransaction := Value;
  if (FTransaction <> nil) then
  begin
    FIndexInTransaction := FTransaction.AddSQLObject(Self);
    if (FDatabase = nil) then
      Database := FTransaction.FindDefaultDatabase;
  end;
end;

{ GenerateDPB -
  Given a string containing a textual representation
  of the database parameters, generate a database
  parameter buffer, and return it and its length
  in DPB and DPBLength, respectively. }

procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
var
  i, j, pval: Integer;
  DPBVal: UShort;
  ParamName, ParamValue: string;
begin
  { The DPB is initially empty, with the exception that
    the DPB version must be the first byte of the string. }
  DPBLength := 1;
  DPB := Char(isc_dpb_version1);

  {Iterate through the textual database parameters, constructing
   a DPB on-the-fly }
  for i := 0 to sl.Count - 1 do
  begin
    { Get the parameter's name and value from the list,
      and make sure that the name is all lowercase with
      no leading 'isc_dpb_' prefix
    }
    if (Trim(sl.Names[i]) = '') then
      continue;
    ParamName := LowerCase(sl.Names[i]); {mbcs ok}
    ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    if (Pos(DPBPrefix, ParamName) = 1) then {mbcs ok}
      Delete(ParamName, 1, Length(DPBPrefix));
     { We want to translate the parameter name to some Integer
       value. We do this by scanning through a list of known
       database parameter names (DPBConstantNames, defined above) }
    DPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_dpb_last_dpb_constant do
      if (ParamName = DPBConstantNames[j]) then
      begin
        DPBVal := j;
        break;
      end;
     {  A database parameter either contains a string value (case 1)
       or an Integer value (case 2)
       or no value at all (case 3)
       or an error needs to be generated (case else)  }
    case DPBVal of
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype,
      isc_dpb_sql_role_name, isc_dpb_sql_dialect:
      begin
        if DPBVal = isc_dpb_sql_dialect then
          ParamValue[1] := Char(Ord(ParamValue[1]) - 48);
        DPB := DPB +
               Char(DPBVal) +
               Char(Length(ParamValue)) +
               ParamValue;
        Inc(DPBLength, 2 + Length(ParamValue));
      end;
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 +
               Char(StrToInt(ParamValue));
        Inc(DPBLength, 3);
      end;
      isc_dpb_sweep:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 +
               Char(isc_dpb_records);
        Inc(DPBLength, 3);
      end;
      isc_dpb_sweep_interval:
      begin
        pval := StrToInt(ParamValue);
        DPB := DPB +
               Char(DPBVal) +
               #4 +
               PChar(@pval)[0] +
               PChar(@pval)[1] +
               PChar(@pval)[2] +
               PChar(@pval)[3];
        Inc(DPBLength, 6);
      end;
      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
      isc_dpb_quit_log:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 + #0;
        Inc(DPBLength, 3);
      end;
      else
      begin
        if (DPBVal > 0) and
           (DPBVal <= isc_dpb_last_dpb_constant) then
          MDOError(mdoeDPBConstantNotSupported, [DPBConstantNames[DPBVal]])
        else
          MDOError(mdoeDPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

{ GenerateTPB -
  Given a string containing a textual representation
  of the transaction parameters, generate a transaction
  parameter buffer, and return it and its length in
  TPB and TPBLength, respectively. }
procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);
var
  i, j, TPBVal, ParamLength: Integer;
  ParamName, ParamValue: string;
begin
  TPB := '';
  if (sl.Count = 0) then
    TPBLength := 0
  else
  begin
    TPBLength := sl.Count + 1;
    TPB := TPB + Char(isc_tpb_version3);
  end;
  for i := 0 to sl.Count - 1 do
  begin
    if (Trim(sl[i]) =  '') then
    begin
      Dec(TPBLength);
      Continue;
    end;
    if (Pos('=', sl[i]) = 0) then {mbcs ok}
      ParamName := LowerCase(sl[i]) {mbcs ok}
    else
    begin
      ParamName := LowerCase(sl.Names[i]); {mbcs ok}
      ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    end;
    if (Pos(TPBPrefix, ParamName) = 1) then {mbcs ok}
      Delete(ParamName, 1, Length(TPBPrefix));
    TPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_tpb_last_tpb_constant do
      if (ParamName = TPBConstantNames[j]) then
      begin
        TPBVal := j;
        break;
      end;
    { Now act on it }
    case TPBVal of
      isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
      isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
      isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
      isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version:
        TPB := TPB + Char(TPBVal);
      isc_tpb_lock_read, isc_tpb_lock_write:
      begin
        TPB := TPB + Char(TPBVal);
        { Now set the string parameter }
        ParamLength := Length(ParamValue);
        Inc(TPBLength, ParamLength + 1);
        TPB := TPB + Char(ParamLength) + ParamValue;
      end;
      else
      begin
        if (TPBVal > 0) and
           (TPBVal <= isc_tpb_last_tpb_constant) then
          MDOError(mdoeTPBConstantNotSupported, [TPBConstantNames[TPBVal]])
        else
          MDOError(mdoeTPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

procedure TMDOTransaction.ApplyDefaultAction;
begin
   if not FExplicitEndTransaction then
    case FDefaultAction of
      TARollback : EndTransaction(TARollBack, false);
      TACommit : EndTransaction(TACommit, false);
      TARollbackRetaining : EndTransaction(TARollbackRetaining, false);
      TACommitRetaining : EndTransaction(TACommitRetaining, false);
    end;
end;


end.





