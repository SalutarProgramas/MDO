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

{
  InterBase Express provides component interfaces to
  functions introduced in InterBase 6.0.  The Services
  components (TMDO*Service, TMDOServerProperties) and
  Install components (TMDOInstall, TMDOUninstall, TMDOSetup)
  function only if you have installed InterBase 6.0 or
  later software
}

unit MDOServices;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MDODialogs, MDOHeader, MDO, MDOExternals;

const
  DefaultBufferSize = 32000;

  SPBPrefix = 'isc_spb_';
  SPBConstantNames: array[1..isc_spb_last_spb_constant] of String = (
    'user_name',
    'sys_user_name',
    'sys_user_name_enc',
    'password',
    'password_enc',
    'command_line',
    'db_name',
    'verbose',
    'options',
    'connect_timeout',
    'dummy_packet_interval',
    'sql_role_name'
  );

  SPBConstantValues: array[1..isc_spb_last_spb_constant] of Integer = (
    isc_spb_user_name_mapped_to_server,
    isc_spb_sys_user_name_mapped_to_server,
    isc_spb_sys_user_name_enc_mapped_to_server,
    isc_spb_password_mapped_to_server,
    isc_spb_password_enc_mapped_to_server,
    isc_spb_command_line_mapped_to_server,
    isc_spb_dbname_mapped_to_server,
    isc_spb_verbose_mapped_to_server,
    isc_spb_options_mapped_to_server,
    isc_spb_connect_timeout_mapped_to_server,
    isc_spb_dummy_packet_interval_mapped_to_server,
    isc_spb_sql_role_name_mapped_to_server
  );

type
  TProtocol = (TCP, SPX, NamedPipe, Local);
  TOutputBufferOption = (ByLine, ByChunk);

  TMDOCustomService = class;

  TLoginEvent = procedure (Database: TMDOCustomService; LoginParams: TStrings) 
          of object;
  TMDOCustomService = class (TComponent)
  private
    FBufferSize: Integer;
    FHandle: TISC_SVC_HANDLE;
    FIBLoaded: Boolean;
    FLoginPrompt: Boolean;
    FOnAttach: TNotifyEvent;
    FOnLogin: TLoginEvent;
    FOutputBuffer: PChar;
    FOutputBufferOption: TOutputBufferOption;
    FParams: TStrings;
    FParamsChanged: Boolean;
    FProtocol: TProtocol;
    FQueryParams: string;
    FQuerySPB: PChar;
    FQuerySPBLength: Short;
    FServerName: string;
    FSPB: PChar;
    FSPBLength: Short;
    FStreamedActive: Boolean;
    FTraceFlags: TTraceFlags;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    procedure CheckServerName;
    procedure GenerateSPB(sl: TStrings; var SPB: String; var SPBLength: Short);
    function GetActive: Boolean;
    function GetServiceParamBySPB(const Idx: Integer): string;
    function IndexOfSPBConst(st: String): Integer;
    procedure ParamsChange(Sender: TObject);
    procedure ParamsChanging(Sender: TObject);
    function ParseInteger(var RunLen: Integer): Integer;
    function ParseString(var RunLen: Integer): string;
    procedure SetActive(const Value: Boolean);
    procedure SetBufferSize(const Value: Integer);
    procedure SetParams(const Value: TStrings);
    procedure SetProtocol(const Value: TProtocol);
    procedure SetServerName(const Value: string);
    procedure SetServiceParamBySPB(const Idx: Integer; const Value: string);
  protected
    procedure CheckActive;
    procedure CheckInactive;
    procedure InternalServiceQuery;
    procedure Loaded; override;
    function Login: Boolean;
    property BufferSize: Integer read FBufferSize write SetBufferSize default 
            DefaultBufferSize;
    property OutputBuffer: PChar read FOutputBuffer;
    property OutputBufferOption: TOutputBufferOption read FOutputBufferOption 
            write FOutputBufferOption;
    property ServiceQueryParams: string read FQueryParams write FQueryParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach;
    procedure Detach;
    property Handle: TISC_SVC_HANDLE read FHandle;
    property ServiceParamBySPB[const Idx: Integer]: string read 
            GetServiceParamBySPB write SetServiceParamBySPB;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default 
            True;
    property OnAttach: TNotifyEvent read FOnAttach write FOnAttach;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
    property Params: TStrings read FParams write SetParams;
    property Protocol: TProtocol read FProtocol write SetProtocol default Local;
    property ServerName: string read FServerName write SetServerName;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
  end;
  
  TDatabaseInfo = class (TObject)
  public
    DbName: array of string;
    NoOfAttachments: Integer;
    NoOfDatabases: Integer;
    constructor Create;
    destructor Destroy; override;
  end;
  
  TLicenseInfo = class (TObject)
  public
    Desc: array of string;
    Id: array of string;
    Key: array of string;
    LicensedUsers: Integer;
    constructor Create;
    destructor Destroy; override;
  end;
  
  TLicenseMaskInfo = class (TObject)
  public
    CapabilityMask: Integer;
    LicenseMask: Integer;
  end;
  
  TConfigFileData = class (TObject)
  public
    ConfigFileKey: array of integer;
    ConfigFileValue: array of integer;
    constructor Create;
    destructor Destroy; override;
  end;
  
  TConfigParams = class (TObject)
  public
    BaseLocation: string;
    ConfigFileData: TConfigFileData;
    ConfigFileParams: array of string;
    LockFileLocation: string;
    MessageFileLocation: string;
    SecurityDatabaseLocation: string;
    constructor Create;
    destructor Destroy; override;
  end;
  
  TVersionInfo = class (TObject)
    ServerImplementation: string;
    ServerVersion: string;
    ServiceVersion: Integer;
  end;
  
  TPropertyOption = (Database, License, LicenseMask, ConfigParameters, Version);
  TPropertyOptions = set of TPropertyOption;

  TMDOServerProperties = class (TMDOCustomService)
  private
    FConfigParams: TConfigParams;
    FDatabaseInfo: TDatabaseInfo;
    FLicenseInfo: TLicenseInfo;
    FLicenseMaskInfo: TLicenseMaskInfo;
    FOptions: TPropertyOptions;
    FVersionInfo: TVersionInfo;
    procedure ParseConfigFileData(var RunLen: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Fetch;
    procedure FetchConfigParams;
    procedure FetchDatabaseInfo;
    procedure FetchLicenseInfo;
    procedure FetchLicenseMaskInfo;
    procedure FetchVersionInfo;
    property ConfigParams: TConfigParams read FConfigParams;
    property DatabaseInfo: TDatabaseInfo read FDatabaseInfo;
    property LicenseInfo: TLicenseInfo read FLicenseInfo;
    property LicenseMaskInfo: TLicenseMaskInfo read FLicenseMaskInfo;
    property VersionInfo: TVersionInfo read FVersionInfo;
  published
    property Options: TPropertyOptions read FOptions write FOptions;
  end;
  
  TMDOControlService = class (TMDOCustomService)
  private
    FStartParams: string;
    FStartSPB: PChar;
    FStartSPBLength: Integer;
    function GetIsServiceRunning: Boolean;
  protected
    procedure InternalServiceStart;
    procedure ServiceStartAddParam(Value: Integer; param: Integer); overload;
    procedure ServiceStartAddParam(Value: string; param: Integer); overload;
    procedure SetServiceStartOptions; virtual;
    property ServiceStartParams: string read FStartParams write FStartParams;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ServiceStart; virtual;
    property IsServiceRunning: Boolean read GetIsServiceRunning;
  end;
  
  TMDOControlAndQueryService = class (TMDOControlService)
  private
    FAction: Integer;
    FEof: Boolean;
    procedure SetAction(Value: Integer);
  protected
    property Action: Integer read FAction write SetAction;
  public
    constructor create(AOwner: TComponent); override;
    function GetNextChunk: string;
    function GetNextLine: string;
    property Eof: Boolean read FEof;
  published
    property BufferSize;
  end;
  
  TShutdownMode = (Forced, DenyTransaction, DenyAttachment);

  TMDOConfigService = class (TMDOControlService)
  private
    FDatabaseName: string;
    procedure SetDatabaseName(const Value: string);
  public
    procedure ActivateShadow;
    procedure BringDatabaseOnline;
    procedure ServiceStart; override;
    procedure SetAsyncMode(Value: Boolean);
    procedure SetDBSqlDialect(Value: Integer);
    procedure SetPageBuffers(Value: Integer);
    procedure SetReadOnly(Value: Boolean);
    procedure SetReserveSpace(Value: Boolean);
    procedure SetSweepInterval(Value: Integer);
    procedure ShutdownDatabase(Options: TShutdownMode; Wait: Integer);
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
  end;
  
  TMDOLogService = class (TMDOControlAndQueryService)
  protected
    procedure SetServiceStartOptions; override;
  end;
  
  TStatOption = (DataPages, DbLog, HeaderPages, IndexPages, SystemRelations);
  TStatOptions = set of TStatOption;

  TMDOStatisticalService = class (TMDOControlAndQueryService)
  private
    FDatabaseName: string;
    FOptions: TStatOptions;
    procedure SetDatabaseName(const Value: string);
  protected
    procedure SetServiceStartOptions; override;
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Options: TStatOptions read FOptions write FOptions;
  end;
  

  TMDOBackupRestoreService = class (TMDOControlAndQueryService)
  private
    FVerbose: Boolean;
  published
    property Verbose: Boolean read FVerbose write FVerbose default False;
  end;
  
  TBackupOption = (IgnoreChecksums, IgnoreLimbo, MetadataOnly, NoGarbageCollection,
    OldMetadataDesc, NonTransportable, ConvertExtTables);
  TBackupOptions = set of TBackupOption;

  TMDOBackupService = class (TMDOBackupRestoreService)
  private
    FBackupFile: TStrings;
    FBlockingFactor: Integer;
    FDatabaseName: string;
    FOptions: TBackupOptions;
    procedure SetBackupFile(const Value: TStrings);
  protected
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BackupFile: TStrings read FBackupFile write SetBackupFile;
    property BlockingFactor: Integer read FBlockingFactor write FBlockingFactor;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property Options: TBackupOptions read FOptions write FOptions;
  end;
  
  TRestoreOption = (DeactivateIndexes, NoShadow, NoValidityCheck, OneRelationAtATime,
    Replace, CreateNewDB, UseAllSpace);

  TRestoreOptions = set of TRestoreOption;
  TMDORestoreService = class (TMDOBackupRestoreService)
  private
    FBackupFile: TStrings;
    FDatabaseName: TStrings;
    FOptions: TRestoreOptions;
    FPageBuffers: Integer;
    FPageSize: Integer;
    procedure SetBackupFile(const Value: TStrings);
    procedure SetDatabaseName(const Value: TStrings);
  protected
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BackupFile: TStrings read FBackupFile write SetBackupFile;
    property DatabaseName: TStrings read FDatabaseName write SetDatabaseName;
    property Options: TRestoreOptions read FOptions write FOptions default 
            [CreateNewDB];
    property PageBuffers: Integer read FPageBuffers write FPageBuffers;
    property PageSize: Integer read FPageSize write FPageSize;
  end;
  
  TValidateOption = (LimboTransactions, CheckDB, IgnoreChecksum, KillShadows, MendDB,
    SweepDB, ValidateDB, ValidateFull);
  TValidateOptions = set of TValidateOption;

  TTransactionGlobalAction = (CommitGlobal, RollbackGlobal, RecoverTwoPhaseGlobal,
                             NoGlobalAction);
  TTransactionState = (LimboState, CommitState, RollbackState, UnknownState);
  TTransactionAdvise = (CommitAdvise, RollbackAdvise, UnknownAdvise);
  TTransactionAction = (CommitAction, RollbackAction);

  TLimboTransactionInfo = class (TObject)
  public
    Action: TTransactionAction;
    Advise: TTransactionAdvise;
    HostSite: string;
    ID: Integer;
    MultiDatabase: Boolean;
    RemoteDatabasePath: string;
    RemoteSite: string;
    State: TTransactionState;
  end;
  
  TMDOValidationService = class (TMDOControlAndQueryService)
  private
    FDatabaseName: string;
    FGlobalAction: TTransactionGlobalAction;
    FLimboTransactionInfo: array of TLimboTransactionInfo;
    FOptions: TValidateOptions;
    function GetLimboTransactionInfo(Index: integer): TLimboTransactionInfo;
    function GetLimboTransactionInfoCount: Integer;
    procedure SetDatabaseName(const Value: string);
  protected
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FetchLimboTransactionInfo;
    procedure FixLimboTransactionErrors;
    property LimboTransactionInfo[Index: integer]: TLimboTransactionInfo read 
            GetLimboTransactionInfo;
    property LimboTransactionInfoCount: Integer read 
            GetLimboTransactionInfoCount;
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property GlobalAction: TTransactionGlobalAction read FGlobalAction write 
            FGlobalAction;
    property Options: TValidateOptions read FOptions write FOptions;
  end;
  
  TUserInfo = class (TObject)
  public
    FirstName: string;
    GroupID: Integer;
    LastName: string;
    MiddleName: string;
    UserID: Integer;
    UserName: string;
  end;
  
  TSecurityAction = (ActionAddUser, ActionDeleteUser, ActionModifyUser, ActionDisplayUser);
  TSecurityModifyParam = (ModifyFirstName, ModifyMiddleName, ModifyLastName, ModifyUserId,
                         ModifyGroupId, ModifyPassword);
  TSecurityModifyParams = set of TSecurityModifyParam;

  TMDOSecurityService = class (TMDOControlAndQueryService)
  private
    FFirstName: string;
    FGroupID: Integer;
    FLastName: string;
    FMiddleName: string;
    FModifyParams: TSecurityModifyParams;
    FPassword: string;
    FSecurityAction: TSecurityAction;
    FSQlRole: string;
    FUserID: Integer;
    FUserInfo: array of TUserInfo;
    FUserName: string;
    procedure ClearParams;
    procedure FetchUserInfo;
    function GetUserInfo(Index: Integer): TUserInfo;
    function GetUserInfoCount: Integer;
    procedure SetFirstName(Value: string);
    procedure SetGroupID(Value: Integer);
    procedure SetLastName(Value: string);
    procedure SetMiddleName(Value: string);
    procedure SetPassword(Value: string);
    procedure SetSecurityAction(Value: TSecurityAction);
    procedure SetUserID(Value: Integer);
  protected
    procedure Loaded; override;
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddUser;
    procedure DeleteUser;
    procedure DisplayUser(UserName: string);
    procedure DisplayUsers;
    procedure ModifyUser;
    property UserInfo[Index: Integer]: TUserInfo read GetUserInfo;
    property UserInfoCount: Integer read GetUserInfoCount;
  published
    property FirstName: string read FFirstName write SetFirstName;
    property GroupID: Integer read FGroupID write SetGroupID;
    property LastName: string read FLastName write SetLastName;
    property MiddleName: string read FMiddleName write SetMiddleName;
    property Password: string read FPassword write SetPassword;
    property SecurityAction: TSecurityAction read FSecurityAction write 
            SetSecurityAction;
    property SQlRole: string read FSQlRole write FSQlRole;
    property UserID: Integer read FUserID write SetUserID;
    property UserName: string read FUserName write FUserName;
  end;
  

implementation

uses
  MDOIntf, MDOSQLMonitor;

{ TMDOCustomService }

{
****************************** TMDOCustomService *******************************
}
constructor TMDOCustomService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckFBLoaded;
  FIBLoaded := True;
  FserverName := '';
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  FSPB := nil;
  FQuerySPB := nil;
  FBufferSize := DefaultBufferSize;
  FHandle := nil;
  FLoginPrompt := True;
  FTraceFlags := [];
  FOutputbuffer := nil;
end;

destructor TMDOCustomService.Destroy;
begin
  if FIBLoaded then
  begin
    if FHandle <> nil then
      Detach;
    FreeMem(FSPB);
    FSPB := nil;
    FParams.Free;
  end;
  ReallocMem(FOutputBuffer, 0);
  inherited Destroy;
end;

procedure TMDOCustomService.Attach;
var
  SPB: string;
  ConnectString: string;
begin
  CheckInactive;
  CheckServerName;
  
  if FLoginPrompt and not Login then
    MDOError(mdoeOperationCancelled, [nil]);
  
  { Generate a new SPB if necessary }
  if FParamsChanged then
  begin
    FParamsChanged := False;
    GenerateSPB(FParams, SPB, FSPBLength);
    MDOAlloc(FSPB, 0, FsPBLength);
    Move(SPB[1], FSPB[0], FSPBLength);
  end;
  case FProtocol of
    TCP: ConnectString := FServerName + ':service_mgr'; {do not localize}
    SPX: ConnectString := FServerName + '@service_mgr'; {do not localize}
    NamedPipe: ConnectString := '\\' + FServerName + '\service_mgr'; {do not localize}
    Local: ConnectString := 'service_mgr'; {do not localize}
  end;
  if call(isc_service_attach(StatusVector, Length(ConnectString),
                         PChar(ConnectString), @FHandle,
                         FSPBLength, FSPB), False) > 0 then
  begin
    FHandle := nil;
    MDODatabaseError;
  end;
  
  if Assigned(FOnAttach) then
    FOnAttach(Self);
  
  MonitorHook.ServiceAttach(Self);
end;

function TMDOCustomService.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): 
        ISC_STATUS;
begin
  result := ErrCode;
  if RaiseError and (ErrCode > 0) then
    MDODatabaseError;
end;

procedure TMDOCustomService.CheckActive;
begin
  if FStreamedActive and (not Active) then
    Loaded;
  if FHandle = nil then
    MDOError(mdoeServiceActive, [nil]);
end;

procedure TMDOCustomService.CheckInactive;
begin
  if FHandle <> nil then
    MDOError(mdoeServiceInActive, [nil]);
end;

procedure TMDOCustomService.CheckServerName;
begin
  if (FServerName = '') and (FProtocol <> Local) then
    MDOError(mdoeServerNameMissing, [nil]);
end;

procedure TMDOCustomService.Detach;
begin
  CheckActive;
  if (Call(isc_service_detach(StatusVector, @FHandle), False) > 0) then
  begin
    FHandle := nil;
    MDODatabaseError;
  end
  else
    FHandle := nil;
  MonitorHook.ServiceDetach(Self);
end;

procedure TMDOCustomService.GenerateSPB(sl: TStrings; var SPB: String; var 
        SPBLength: Short);
var
  i, j, SPBVal, SPBServerVal: UShort;
  param_name, param_value: string;
begin
  { The SPB is initially empty, with the exception that
   the SPB version must be the first byte of the string.
  }
  SPBLength := 2;
  SPB := Char(isc_spb_version);
  SPB := SPB + Char(isc_spb_current_version);
  { Iterate through the textual service parameters, constructing
   a SPB on-the-fly }
  for i := 0 to sl.Count - 1 do
  begin
   { Get the parameter's name and value from the list,
     and make sure that the name is all lowercase with
     no leading 'isc_spb_' prefix }
    if (Trim(sl.Names[i]) = '') then continue;
    param_name := LowerCase(sl.Names[i]); {mbcs ok}
    param_value := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    if (Pos(SPBPrefix, param_name) = 1) then {mbcs ok}
      Delete(param_name, 1, Length(SPBPrefix));
    { We want to translate the parameter name to some integer
      value. We do this by scanning through a list of known
      service parameter names (SPBConstantNames, defined above). }
    SPBVal := 0;
    SPBServerVal := 0;
    { Find the parameter }
    for j := 1 to isc_spb_last_spb_constant do
      if (param_name = SPBConstantNames[j]) then
      begin
        SPBVal := j;
        SPBServerVal := SPBConstantValues[j];
        break;
      end;
    case SPBVal of
      isc_spb_user_name, isc_spb_password:
      begin
        SPB := SPB +
               Char(SPBServerVal) +
               Char(Length(param_value)) +
               param_value;
        Inc(SPBLength, 2 + Length(param_value));
      end;
      else
      begin
        if (SPBVal > 0) and
           (SPBVal <= isc_dpb_last_dpb_constant) then
          MDOError(mdoeSPBConstantNotSupported,
                   [SPBConstantNames[SPBVal]])
        else
          MDOError(mdoeSPBConstantUnknown, [SPBVal]);
      end;
    end;
  end;
end;

function TMDOCustomService.GetActive: Boolean;
begin
  result := FHandle <> nil;
end;

function TMDOCustomService.GetServiceParamBySPB(const Idx: Integer): string;
var
  ConstIdx, EqualsIdx: Integer;
begin
  if (Idx > 0) and (Idx <= isc_spb_last_spb_constant) then
  begin
    ConstIdx := IndexOfSPBConst(SPBConstantNames[Idx]);
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

function TMDOCustomService.IndexOfSPBConst(st: String): Integer;
var
  i, pos_of_str: Integer;
begin
  result := -1;
  for i := 0 to Params.Count - 1 do
  begin
    pos_of_str := Pos(st, Params[i]); {mbcs ok}
    if (pos_of_str = 1) or (pos_of_str = Length(SPBPrefix) + 1) then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TMDOCustomService.InternalServiceQuery;
begin
  FQuerySPBLength := Length(FQueryParams);
  if FQuerySPBLength = 0 then
    MDOError(mdoeQueryParamsError, [nil]);
  MDOAlloc(FQuerySPB, 0, FQuerySPBLength);
  Move(FQueryParams[1], FQuerySPB[0], FQuerySPBLength);
  if (FOutputBuffer = nil) then
    MDOAlloc(FOutputBuffer, 0, FBufferSize);
  try
    if call(isc_service_query(StatusVector, @FHandle, nil, 0, nil,
                           FQuerySPBLength, FQuerySPB,
                           FBufferSize, FOutputBuffer), False) > 0 then
    begin
      FHandle := nil;
      MDODatabaseError;
    end;
  finally
    FreeMem(FQuerySPB);
    FQuerySPB := nil;
    FQuerySPBLength := 0;
    FQueryParams := '';
  end;
  MonitorHook.ServiceQuery(Self);
end;

procedure TMDOCustomService.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive and (not Active) then
      Attach;
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

function TMDOCustomService.Login: Boolean;
var
  IndexOfUser, IndexOfPassword: Integer;
  Username, Password: string;
  LoginParams: TStrings;
begin
  if Assigned(FOnLogin) then begin
    result := True;
    LoginParams := TStringList.Create;
    try
      LoginParams.Assign(Params);
      FOnLogin(Self, LoginParams);
      Params.Assign (LoginParams);
    finally
      LoginParams.Free;
    end;
  end
  else begin
    IndexOfUser := IndexOfSPBConst(SPBConstantNames[isc_spb_user_name]);
    if IndexOfUser <> -1 then
      Username := Copy(Params[IndexOfUser],
                                         Pos('=', Params[IndexOfUser]) + 1, {mbcs ok}
                                         Length(Params[IndexOfUser]));
    IndexOfPassword := IndexOfSPBConst(SPBConstantNames[isc_spb_password]);
    if IndexOfPassword <> -1 then
      Password := Copy(Params[IndexOfPassword],
                                         Pos('=', Params[IndexOfPassword]) + 1, {mbcs ok}
                                         Length(Params[IndexOfPassword]));
    result := ServerLoginDialog(serverName, Username, Password);
    if result then
    begin
      IndexOfPassword := IndexOfSPBConst(SPBConstantNames[isc_spb_password]);
      if IndexOfUser = -1 then
        Params.Add(SPBConstantNames[isc_spb_user_name] + '=' + Username)
      else
        Params[IndexOfUser] := SPBConstantNames[isc_spb_user_name] +
                                 '=' + Username;
      if IndexOfPassword = -1 then
        Params.Add(SPBConstantNames[isc_spb_password] + '=' + Password)
      else
        Params[IndexOfPassword] := SPBConstantNames[isc_spb_password] +
                                     '=' + Password;
    end;
  end;
end;

procedure TMDOCustomService.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TMDOCustomService.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;

function TMDOCustomService.ParseInteger(var RunLen: Integer): Integer;
begin
  result := isc_vax_integer(OutputBuffer + RunLen, 4);
  RunLen := RunLen + 4;
end;

function TMDOCustomService.ParseString(var RunLen: Integer): string;
var
  Len: UShort;
  tmp: Char;
begin
  Len := isc_vax_integer(OutputBuffer + RunLen, 2);
  RunLen := RunLen + 2;
  if (Len <> 0) then
  begin
    tmp := OutputBuffer[RunLen + Len];
    OutputBuffer[RunLen + Len] := #0;
    result := String(PChar(@OutputBuffer[RunLen]));
    OutputBuffer[RunLen + Len] := tmp;
    RunLen := RunLen + Len;
  end
  else
    result := '';
end;

procedure TMDOCustomService.SetActive(const Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Value <> Active then
      if Value then
        Attach
      else
        Detach;
end;

procedure TMDOCustomService.SetBufferSize(const Value: Integer);
begin
  if (FOutputBuffer <> nil) and (Value <> FBufferSize) then
    MDOAlloc(FOutputBuffer, 0, FBufferSize);
end;

procedure TMDOCustomService.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TMDOCustomService.SetProtocol(const Value: TProtocol);
begin
  if FProtocol <> Value then
  begin
    CheckInactive;
    FProtocol := Value;
    if (Value = Local) then
      FServerName := '';
  end;
end;

procedure TMDOCustomService.SetServerName(const Value: string);
begin
  if FServerName <> Value then
  begin
    CheckInactive;
    FServerName := Value;
  end;
end;

procedure TMDOCustomService.SetServiceParamBySPB(const Idx: Integer; const 
        Value: string);
var
  ConstIdx: Integer;
begin
  ConstIdx := IndexOfSPBConst(SPBConstantNames[Idx]);
  if (Value = '') then
  begin
    if ConstIdx <> -1 then
      Params.Delete(ConstIdx);
  end
  else
  begin
    if (ConstIdx = -1) then
      Params.Add(SPBConstantNames[Idx] + '=' + Value)
    else
      Params[ConstIdx] := SPBConstantNames[Idx] + '=' + Value;
  end;
end;

{
 * GenerateSPB -
 *  Given a string containing a textual representation
 *  of the Service parameters, generate a service
 *  parameter buffer, and return it and its length
 *  in SPB and SPBLength, respectively.
}
{ TMDOServerProperties }
{
***************************** TMDOServerProperties *****************************
}
constructor TMDOServerProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabaseInfo := TDatabaseInfo.Create;
  FLicenseInfo := TLicenseInfo.Create;
  FLicenseMaskInfo := TLicenseMaskInfo.Create;
  FVersionInfo := TVersionInfo.Create;
  FConfigParams := TConfigParams.Create;
end;

destructor TMDOServerProperties.Destroy;
begin
  FDatabaseInfo.Free;
  FLicenseInfo.Free;
  FLicenseMaskInfo.Free;
  FVersionInfo.Free;
  FConfigParams.Free;
  inherited Destroy;
end;

procedure TMDOServerProperties.Fetch;
begin
  if (Database in Options) then
    FetchDatabaseInfo;
  if (License in Options) then
    FetchLicenseInfo;
  if (LicenseMask in Options) then
    FetchLicenseMaskInfo;
  if (ConfigParameters in Options) then
    FetchConfigParams;
  if (Version in Options) then
    FetchVersionInfo;
end;

procedure TMDOServerProperties.FetchConfigParams;
var
  RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_get_config) +
                        Char(isc_info_svc_get_env) +
                        Char(isc_info_svc_get_env_lock) +
                        Char(isc_info_svc_get_env_msg) +
                        Char(isc_info_svc_user_dbpath);
  
  InternalServiceQuery;
  RunLen := 0;
  While (not (Integer(OutputBuffer[RunLen]) = isc_info_end)) do
  begin
    case Integer(OutputBuffer[RunLen]) of
      isc_info_svc_get_config:
      begin
        FConfigParams.ConfigFileData.ConfigFileKey := nil;
        FConfigParams.ConfigFileData.ConfigFileValue := nil;
        Inc (RunLen);
        while (not (Integer(OutputBuffer[RunLen]) = isc_info_flag_end)) do
          ParseConfigFileData (RunLen);
        if (Integer(OutputBuffer[RunLen]) = isc_info_flag_end) then
          Inc (RunLen);
      end;
  
      isc_info_svc_get_env:
      begin
        Inc (RunLen);
        FConfigParams.BaseLocation := ParseString(RunLen);
      end;
  
      isc_info_svc_get_env_lock:
      begin
        Inc (RunLen);
        FConfigParams.LockFileLocation := ParseString(RunLen);
      end;
  
      isc_info_svc_get_env_msg:
      begin
        Inc (RunLen);
        FConfigParams.MessageFileLocation := ParseString(RunLen);
      end;
  
      isc_info_svc_user_dbpath:
      begin
        Inc (RunLen);
        FConfigParams.SecurityDatabaseLocation := ParseString(RunLen);
      end;
      else
        MDOError(mdoeOutputParsingError, [nil]);
    end;
  end;
end;

procedure TMDOServerProperties.FetchDatabaseInfo;
var
  i, RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_svr_db_info);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_svr_db_info)) then
      MDOError(mdoeOutputParsingError, [nil]);
  RunLen := 1;
  if (OutputBuffer[RunLen] <> Char(isc_spb_num_att)) then
      MDOError(mdoeOutputParsingError, [nil]);
  Inc(RunLen);
  FDatabaseInfo.NoOfAttachments := ParseInteger(RunLen);
  if (OutputBuffer[RunLen] <> Char(isc_spb_num_db)) then
      MDOError(mdoeOutputParsingError, [nil]);
  Inc(RunLen);
  FDatabaseInfo.NoOfDatabases := ParseInteger(RunLen);
  FDatabaseInfo.DbName := nil;
  SetLength(FDatabaseInfo.DbName, FDatabaseInfo.NoOfDatabases);
  i := 0;
  while (OutputBuffer[RunLen] <> Char(isc_info_flag_end)) do
  begin
    if (OutputBuffer[RunLen] <> Char(SPBConstantValues[isc_spb_dbname])) then
      MDOError(mdoeOutputParsingError, [nil]);
    Inc(RunLen);
    FDatabaseInfo.DbName[i] := ParseString(RunLen);
    Inc (i);
  end;
end;

procedure TMDOServerProperties.FetchLicenseInfo;
var
  i, RunLen: Integer;
  done: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_get_license) +
                        Char(isc_info_svc_get_licensed_users);
  InternalServiceQuery;
  RunLen := 0;
  done := 0;
  i := 0;
  FLicenseInfo.key := nil;
  FLicenseInfo.id := nil;
  FLicenseInfo.desc := nil;
  
  While done < 2 do begin
    Inc(Done);
    Inc(RunLen);
    case Integer(OutputBuffer[RunLen-1]) of
      isc_info_svc_get_license:
      begin
        while (OutputBuffer[RunLen] <> Char(isc_info_flag_end)) do
        begin
          if (i >= Length(FLicenseInfo.key)) then
          begin
            SetLength(FLicenseInfo.key, i + 10);
            SetLength(FLicenseInfo.id, i + 10);
            SetLength(FLicenseInfo.desc, i + 10);
          end;
          if (OutputBuffer[RunLen] <> Char(isc_spb_lic_id)) then
              MDOError(mdoeOutputParsingError, [nil]);
          Inc(RunLen);
          FLicenseInfo.id[i] := ParseString(RunLen);
          if (OutputBuffer[RunLen] <> Char(isc_spb_lic_key)) then
              MDOError(mdoeOutputParsingError, [nil]);
          Inc(RunLen);
          FLicenseInfo.key[i] := ParseString(RunLen);
          if (OutputBuffer[RunLen] <> Char(7)) then
              MDOError(mdoeOutputParsingError, [nil]);
          Inc(RunLen);
          FLicenseInfo.desc[i] := ParseString(RunLen);
          Inc(i);
        end;
        Inc(RunLen);
        if (Length(FLicenseInfo.key) > i) then
        begin
          SetLength(FLicenseInfo.key, i);
          SetLength(FLicenseInfo.id, i);
          SetLength(FLicenseInfo.desc, i);
        end;
      end;
      isc_info_svc_get_licensed_users:
        FLicenseInfo.LicensedUsers := ParseInteger(RunLen);
      else
        MDOError(mdoeOutputParsingError, [nil]);
    end;
  end;
end;

procedure TMDOServerProperties.FetchLicenseMaskInfo;
var
  done, RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_get_license_mask) +
                        Char(isc_info_svc_capabilities);
  InternalServiceQuery;
  RunLen := 0;
  done := 0;
  While done <= 1 do
  begin
    Inc(done);
    Inc(RunLen);
    case Integer(OutputBuffer[RunLen-1]) of
      isc_info_svc_get_license_mask:
        FLicenseMaskInfo.LicenseMask := ParseInteger(RunLen);
      isc_info_svc_capabilities:
        FLicenseMaskInfo.CapabilityMask := ParseInteger(RunLen);
      else
        MDOError(mdoeOutputParsingError, [nil]);
    end;
  end;
end;

procedure TMDOServerProperties.FetchVersionInfo;
var
  RunLen: Integer;
  done: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_version) +
                        Char(isc_info_svc_server_version) +
                        Char(isc_info_svc_implementation);
  InternalServiceQuery;
  RunLen := 0;
  done := 0;
  
  While done <= 2 do
  begin
    Inc(done);
    Inc(RunLen);
    case Integer(OutputBuffer[RunLen-1]) of
      isc_info_svc_version:
        FVersionInfo.ServiceVersion := ParseInteger(RunLen);
      isc_info_svc_server_version:
        FVersionInfo.ServerVersion := ParseString(RunLen);
      isc_info_svc_implementation:
        FVersionInfo.ServerImplementation := ParseString(RunLen);
      else
        MDOError(mdoeOutputParsingError, [nil]);
    end;
  end;
end;

procedure TMDOServerProperties.ParseConfigFileData(var RunLen: Integer);
begin
  Inc(RunLen);
  with FConfigParams.ConfigFileData do
  begin
    SetLength (ConfigFileValue, Length(ConfigFileValue)+1);
    SetLength (ConfigFileKey, Length(ConfigFileKey)+1);
  
    ConfigFileKey[High(ConfigFileKey)] := Integer(OutputBuffer[RunLen-1]);
    ConfigFileValue[High(ConfigFileValue)] := ParseInteger(RunLen);
  end;
end;


{ TMDOControlService }
{
****************************** TMDOControlService ******************************
}
constructor TMDOControlService.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FStartParams := '';
  FStartSPB := nil;
  FStartSPBLength := 0;
end;

function TMDOControlService.GetIsServiceRunning: Boolean;
var
  RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_running);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_running)) then
    MDOError(mdoeOutputParsingError, [nil]);
  RunLen := 1;
  if (ParseInteger(RunLen) = 1) then
    result := True
  else
    result := False;
end;

procedure TMDOControlService.InternalServiceStart;
begin
  FStartSPBLength := Length(FStartParams);
  if FStartSPBLength = 0 then
    MDOError(mdoeStartParamsError, [nil]);
  MDOAlloc(FStartSPB, 0, FStartSPBLength);
  Move(FStartParams[1], FStartSPB[0], FstartSPBLength);
  try
    if call(isc_service_start(StatusVector, @FHandle, nil,
                           FStartSPBLength, FStartSPB), False) > 0 then
    begin
      FHandle := nil;
      MDODatabaseError;
    end;
  finally
    FreeMem(FStartSPB);
    FStartSPB := nil;
    FStartSPBLength := 0;
    FStartParams := '';
  end;
  MonitorHook.ServiceStart(Self);
end;

procedure TMDOControlService.ServiceStart;
begin
  CheckActive;
  SetServiceStartOptions;
  InternalServiceStart;
end;

procedure TMDOControlService.ServiceStartAddParam(Value: Integer; param: 
        Integer);
begin
  FStartParams  := FStartParams +
                   Char(Param) +
                   PChar(@Value)[0] +
                   PChar(@Value)[1] +
                   PChar(@Value)[2] +
                   PChar(@Value)[3];
end;

procedure TMDOControlService.ServiceStartAddParam(Value: string; param: 
        Integer);
var
  Len: UShort;
begin
  Len := Length(Value);
  if Len > 0 then
  begin
    FStartParams  := FStartParams +
                     Char(Param) +
                     PChar(@Len)[0] +
                     PChar(@Len)[1] +
                     Value;
  end;
end;

procedure TMDOControlService.SetServiceStartOptions;
begin
  
end;

{ TMDOConfigService }

{
****************************** TMDOConfigService *******************************
}
procedure TMDOConfigService.ActivateShadow;
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (isc_spb_prp_activate, SPBConstantValues[isc_spb_options]);
  InternalServiceStart;
end;

procedure TMDOConfigService.BringDatabaseOnline;
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (isc_spb_prp_db_online, SPBConstantValues[isc_spb_options]);
  InternalServiceStart;
end;

procedure TMDOConfigService.ServiceStart;
begin
  MDOError(mdoeUseSpecificProcedures, [nil]);
end;

procedure TMDOConfigService.SetAsyncMode(Value: Boolean);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartParams := ServiceStartParams +
                        Char(isc_spb_prp_write_mode);
  if Value then
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_wm_async)
  else
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_wm_sync);
  InternalServiceStart;
end;

procedure TMDOConfigService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TMDOConfigService.SetDBSqlDialect(Value: Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (Value, isc_spb_prp_set_sql_dialect);
  InternalServiceStart;
end;

procedure TMDOConfigService.SetPageBuffers(Value: Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (Value, isc_spb_prp_page_buffers);
  InternalServiceStart;
end;

procedure TMDOConfigService.SetReadOnly(Value: Boolean);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartParams := ServiceStartParams +
                         Char(isc_spb_prp_access_mode);
  if Value then
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_am_readonly)
  else
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_am_readwrite);
  InternalServiceStart;
end;

procedure TMDOConfigService.SetReserveSpace(Value: Boolean);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartParams := ServiceStartParams +
                        Char(isc_spb_prp_reserve_space);
  if Value then
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_res)
  else
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_res_use_full);
  InternalServiceStart;
end;

procedure TMDOConfigService.SetSweepInterval(Value: Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (Value, isc_spb_prp_sweep_interval);
  InternalServiceStart;
end;

procedure TMDOConfigService.ShutdownDatabase(Options: TShutdownMode; Wait: 
        Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  if (Options = Forced) then
    ServiceStartAddParam (Wait, isc_spb_prp_shutdown_db)
  else if (Options = DenyTransaction) then
    ServiceStartAddParam (Wait, isc_spb_prp_deny_new_transactions)
  else
    ServiceStartAddParam (Wait, isc_spb_prp_deny_new_attachments);
  InternalServiceStart;
end;

{ TMDOStatisticalService }

{
**************************** TMDOStatisticalService ****************************
}
procedure TMDOStatisticalService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TMDOStatisticalService.SetServiceStartOptions;
var
  param: Integer;
begin
  if FDatabaseName = '' then
    MDOError(mdoeStartParamsError, [nil]);
  param := 0;
  if (DataPages in Options) then
    param := param or isc_spb_sts_data_pages;
  if (DbLog in Options) then
    param := param or isc_spb_sts_db_log;
  if (HeaderPages in Options) then
    param := param or isc_spb_sts_hdr_pages;
  if (IndexPages in Options) then
    param := param or isc_spb_sts_idx_pages;
  if (SystemRelations in Options) then
    param := param or isc_spb_sts_sys_relations;
  Action := isc_action_svc_db_stats;
  ServiceStartParams  := Char(isc_action_svc_db_stats);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (param, SPBConstantValues[isc_spb_options]);
end;

{ TMDOBackupService }
{
****************************** TMDOBackupService *******************************
}
constructor TMDOBackupService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFile := TStringList.Create;
end;

destructor TMDOBackupService.Destroy;
begin
  FBackupFile.Free;
  inherited Destroy;
end;

procedure TMDOBackupService.SetBackupFile(const Value: TStrings);
begin
  FBackupFile.Assign(Value);
end;

procedure TMDOBackupService.SetServiceStartOptions;
var
  param, i: Integer;
  value: string;
begin
  if FDatabaseName = '' then
    MDOError(mdoeStartParamsError, [nil]);
  param := 0;
  if (IgnoreChecksums in Options) then
    param := param or isc_spb_bkp_ignore_checksums;
  if (IgnoreLimbo in Options) then
    param := param or isc_spb_bkp_ignore_limbo;
  if (MetadataOnly in Options) then
    param := param or isc_spb_bkp_metadata_only;
  if (NoGarbageCollection in Options) then
    param := param or isc_spb_bkp_no_garbage_collect;
  if (OldMetadataDesc in Options) then
    param := param or isc_spb_bkp_old_descriptions;
  if (NonTransportable in Options) then
    param := param or isc_spb_bkp_non_transportable;
  if (ConvertExtTables in Options) then
    param := param or isc_spb_bkp_convert;
  Action := isc_action_svc_backup;
  ServiceStartParams  := Char(isc_action_svc_backup);
  ServiceStartAddParam(FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam(param, SPBConstantValues[isc_spb_options]);
  if Verbose then
    ServiceStartParams := ServiceStartParams + Char(SPBConstantValues[isc_spb_verbose]);
  if FBlockingFactor > 0 then
    ServiceStartAddParam(FBlockingFactor, isc_spb_bkp_factor);
  for i := 0 to FBackupFile.Count - 1 do
  begin
    if (Trim(FBackupFile[i]) = '') then
      continue;
    if (Pos('=', FBackupFile[i]) <> 0) then
    begin {mbcs ok}
      ServiceStartAddParam(FBackupFile.Names[i], isc_spb_bkp_file);
      value := Copy(FBackupFile[i], Pos('=', FBackupFile[i]) + 1, Length(FBackupFile.Names[i])); {mbcs ok}
      param := StrToInt(value);
      ServiceStartAddParam(param, isc_spb_bkp_length);
    end
    else
      ServiceStartAddParam(FBackupFile[i], isc_spb_bkp_file);
  end;
end;

{ TMDORestoreService }

{
****************************** TMDORestoreService ******************************
}
constructor TMDORestoreService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabaseName := TStringList.Create;
  FBackupFile := TStringList.Create;
  Include (FOptions, CreateNewDB);
end;

destructor TMDORestoreService.Destroy;
begin
  FDatabaseName.Free;
  FBackupFile.Free;
  inherited Destroy;
end;

procedure TMDORestoreService.SetBackupFile(const Value: TStrings);
begin
  FBackupFile.Assign(Value);
end;

procedure TMDORestoreService.SetDatabaseName(const Value: TStrings);
begin
  FDatabaseName.Assign(Value);
end;

procedure TMDORestoreService.SetServiceStartOptions;
var
  param, i: Integer;
  value: string;
begin
  param := 0;
  if (DeactivateIndexes in Options) then
    param := param or isc_spb_res_deactivate_idx;
  if (NoShadow in Options) then
    param := param or isc_spb_res_no_shadow;
  if (NoValidityCheck in Options) then
    param := param or isc_spb_res_no_validity;
  if (OneRelationAtATime in Options) then
    param := param or isc_spb_res_one_at_a_time;
  if (Replace in Options) then
    param := param or isc_spb_res_replace;
  if (CreateNewDB in Options) then
    param := param or isc_spb_res_create;
  if (UseAllSpace in Options) then
    param := param or isc_spb_res_use_all_space;
  Action := isc_action_svc_restore;
  ServiceStartParams  := Char(isc_action_svc_restore);
  ServiceStartAddParam(param, SPBConstantValues[isc_spb_options]);
  if Verbose then ServiceStartParams := ServiceStartParams + Char(SPBConstantValues[isc_spb_verbose]);
  if FPageSize > 0 then
    ServiceStartAddParam(FPageSize, isc_spb_res_page_size);
  if FPageBuffers > 0 then
    ServiceStartAddParam(FPageBuffers, isc_spb_res_buffers);
  for i := 0 to FBackupFile.Count - 1 do
  begin
    if (Trim(FBackupFile[i]) = '') then continue;
    if (Pos('=', FBackupFile[i]) <> 0) then  {mbcs ok}
    begin
      ServiceStartAddParam(FBackupFile.Names[i], isc_spb_bkp_file);
      value := Copy(FBackupFile[i], Pos('=', FBackupFile[i]) + 1, Length(FBackupFile.Names[i])); {mbcs ok}
      param := StrToInt(value);
      ServiceStartAddParam(param, isc_spb_bkp_length);
    end
    else
      ServiceStartAddParam(FBackupFile[i], isc_spb_bkp_file);
  end;
  for i := 0 to FDatabaseName.Count - 1 do
  begin
    if (Trim(FDatabaseName[i]) = '') then continue;
    if (Pos('=', FDatabaseName[i]) <> 0) then {mbcs ok}
    begin
      ServiceStartAddParam(FDatabaseName.Names[i], SPBConstantValues[isc_spb_dbname]);
      value := Copy(FDatabaseName[i], Pos('=', FDatabaseName[i]) + 1, Length(FDatabaseName[i])); {mbcs ok}
      param := StrToInt(value);
      ServiceStartAddParam(param, isc_spb_res_length);
    end
    else
      ServiceStartAddParam(FDatabaseName[i], SPBConstantValues[isc_spb_dbname]);
  end;
end;

{ TMDOValidationService }
{
**************************** TMDOValidationService *****************************
}
constructor TMDOValidationService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMDOValidationService.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FLimboTransactionInfo) do
    FLimboTransactionInfo[i].Free;
  FLimboTransactionInfo := nil;
  inherited Destroy;
end;

procedure TMDOValidationService.FetchLimboTransactionInfo;
var
  i, RunLen: Integer;
  Value: Char;
begin
  ServiceQueryParams := Char(isc_info_svc_limbo_trans);
  InternalServiceQuery;
  RunLen := 0;
  if (OutputBuffer[RunLen] <> Char(isc_info_svc_limbo_trans)) then
    MDOError(mdoeOutputParsingError, [nil]);
  Inc(RunLen, 3);
  for i := 0 to High(FLimboTransactionInfo) do
    FLimboTransactionInfo[i].Free;
  FLimboTransactionInfo := nil;
  i := 0;
  while (OutputBuffer[RunLen] <> Char(isc_info_end)) do
  begin
    if (i >= Length(FLimboTransactionInfo)) then
      SetLength(FLimboTransactionInfo, i + 10);
    if FLimboTransactionInfo[i] = nil then
      FLimboTransactionInfo[i] := TLimboTransactionInfo.Create;
    with FLimboTransactionInfo[i] do
    begin
      if (OutputBuffer[RunLen] = Char(isc_spb_single_tra_id)) then
      begin
        Inc(RunLen);
        MultiDatabase := False;
        ID := ParseInteger(RunLen);
      end
      else
      begin
        Inc(RunLen);
        MultiDatabase := True;
        ID := ParseInteger(RunLen);
        HostSite := ParseString(RunLen);
        if (OutputBuffer[RunLen] <> Char(isc_spb_tra_state)) then
          MDOError(mdoeOutputParsingError, [nil]);
        Inc(RunLen);
        Value := OutputBuffer[RunLen];
        Inc(RunLen);
        if (Value = Char(isc_spb_tra_state_limbo)) then
          State := LimboState
        else
          if (Value = Char(isc_spb_tra_state_commit)) then
            State := CommitState
          else
            if (Value = Char(isc_spb_tra_state_rollback)) then
              State := RollbackState
            else
              State := UnknownState;
        RemoteSite := ParseString(RunLen);
        RemoteDatabasePath := ParseString(RunLen);
        Value := OutputBuffer[RunLen];
        Inc(RunLen);
        if (Value = Char(isc_spb_tra_advise_commit)) then
        begin
          Advise := CommitAdvise;
          Action:= CommitAction;
        end
        else
          if (Value = Char(isc_spb_tra_advise_rollback)) then
          begin
            Advise := RollbackAdvise;
            Action := RollbackAction;
          end
          else
          begin
            { if no advice commit as default }
            Advise := UnknownAdvise;
            Action:= CommitAction;
          end;
      end;
      Inc (i);
    end;
  end;
  if (i > 0) then
    SetLength(FLimboTransactionInfo, i+1);
end;

procedure TMDOValidationService.FixLimboTransactionErrors;
var
  i: Integer;
begin
  ServiceStartParams  := Char(isc_action_svc_repair);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  if (FGlobalAction = NoGlobalAction) then
  begin
    i := 0;
    while (FLimboTransactionInfo[i].ID <> 0) do
    begin
      if (FLimboTransactionInfo[i].Action = CommitAction) then
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_commit_trans)
      else
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_rollback_trans);
      Inc(i);
    end;
  end
  else
  begin
    i := 0;
    if (FGlobalAction = CommitGlobal) then
      while (FLimboTransactionInfo[i].ID <> 0) do
      begin
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_commit_trans);
        Inc(i);
      end
    else
      while (FLimboTransactionInfo[i].ID <> 0) do
      begin
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_rollback_trans);
        Inc(i);
      end;
  end;
  InternalServiceStart;
end;

function TMDOValidationService.GetLimboTransactionInfo(Index: integer): 
        TLimboTransactionInfo;
begin
  if index <= High(FLimboTransactionInfo) then
    result := FLimboTransactionInfo[index]
  else
    result := nil;
end;

function TMDOValidationService.GetLimboTransactionInfoCount: Integer;
begin
  Result := High(FLimboTransactionInfo);
end;

procedure TMDOValidationService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TMDOValidationService.SetServiceStartOptions;
var
  param: Integer;
begin
  Action := isc_action_svc_repair;
  if FDatabaseName = '' then
    MDOError(mdoeStartParamsError, [nil]);
  param := 0;
  if (SweepDB in Options) then
    param := param or isc_spb_rpr_sweep_db;
  if (ValidateDB in Options) then
    param := param or isc_spb_rpr_validate_db;
  ServiceStartParams  := Char(isc_action_svc_repair);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  if param > 0 then
    ServiceStartAddParam (param, SPBConstantValues[isc_spb_options]);
  param := 0;
  if (LimboTransactions in Options) then
    param := param or isc_spb_rpr_list_limbo_trans;
  if (CheckDB in Options) then
    param := param or isc_spb_rpr_check_db;
  if (IgnoreChecksum in Options) then
    param := param or isc_spb_rpr_ignore_checksum;
  if (KillShadows in Options) then
    param := param or isc_spb_rpr_kill_shadows;
  if (MendDB in Options) then
    param := param or isc_spb_rpr_mend_db;
  if (ValidateFull in Options) then
  begin
     param := param or isc_spb_rpr_full;
     if not (MendDB in Options) then
       param := param or isc_spb_rpr_validate_db;
  end;
  if param > 0 then
    ServiceStartAddParam (param, SPBConstantValues[isc_spb_options]);
end;

{ TMDOSecurityService }
{
***************************** TMDOSecurityService ******************************
}
constructor TMDOSecurityService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModifyParams := [];
end;

destructor TMDOSecurityService.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FUserInfo) do
    FUserInfo[i].Free;
  FUserInfo := nil;
  inherited Destroy;
end;

procedure TMDOSecurityService.AddUser;
begin
  SecurityAction := ActionAddUser;
  ServiceStart;
end;

procedure TMDOSecurityService.ClearParams;
begin
  FModifyParams := [];
  FFirstName := '';
  FMiddleName := '';
  FLastName := '';
  FGroupID := 0;
  FUserID := 0;
  FPassword := '';
end;

procedure TMDOSecurityService.DeleteUser;
begin
  SecurityAction := ActionDeleteUser;
  ServiceStart;
end;

procedure TMDOSecurityService.DisplayUser(UserName: string);
begin
  SecurityAction := ActionDisplayUser;
  ServiceStartParams  := Char(isc_action_svc_display_user);
  ServiceStartAddParam (UserName, isc_spb_sec_username);
  InternalServiceStart;
  FetchUserInfo;
end;

procedure TMDOSecurityService.DisplayUsers;
begin
  SecurityAction := ActionDisplayUser;
  ServiceStartParams  := Char(isc_action_svc_display_user);
  InternalServiceStart;
  FetchUserInfo;
end;

procedure TMDOSecurityService.FetchUserInfo;
var
  i, RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_get_users);
  InternalServiceQuery;
  RunLen := 0;
  if (OutputBuffer[RunLen] <> Char(isc_info_svc_get_users)) then
    MDOError(mdoeOutputParsingError, [nil]);
  Inc(RunLen);
  for i := 0 to High(FUserInfo) do
    FUserInfo[i].Free;
  FUserInfo := nil;
  i := 0;
  { Don't have any use for the combined length
   so increment past by 2 }
  Inc(RunLen, 2);
  while (OutputBuffer[RunLen] <> Char(isc_info_end)) do
  begin
    if (i >= Length(FUSerInfo)) then
      SetLength(FUserInfo, i + 10);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_username)) then
      MDOError(mdoeOutputParsingError, [nil]);
    Inc(RunLen);
    if FUserInfo[i] = nil then
      FUserInfo[i] := TUserInfo.Create;
    FUserInfo[i].UserName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_firstname)) then
      MDOError(mdoeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].FirstName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_middlename)) then
      MDOError(mdoeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].MiddleName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_lastname)) then
      MDOError(mdoeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].LastName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_userId)) then
      MDOError(mdoeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].UserId := ParseInteger(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_groupid)) then
      MDOError(mdoeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].GroupID := ParseInteger(RunLen);
    Inc (i);
  end;
  if (i > 0) then
    SetLength(FUserInfo, i+1);
end;

function TMDOSecurityService.GetUserInfo(Index: Integer): TUserInfo;
begin
  if Index <= High(FUSerInfo) then
    result := FUserInfo[Index]
  else
    result := nil;
end;

function TMDOSecurityService.GetUserInfoCount: Integer;
begin
  Result := High(FUSerInfo);
end;

procedure TMDOSecurityService.Loaded;
begin
  inherited Loaded;
  ClearParams;
end;

procedure TMDOSecurityService.ModifyUser;
begin
  SecurityAction := ActionModifyUser;
  ServiceStart;
end;

procedure TMDOSecurityService.SetFirstName(Value: string);
begin
  FFirstName := Value;
  Include (FModifyParams, ModifyFirstName);
end;

procedure TMDOSecurityService.SetGroupID(Value: Integer);
begin
  FGroupId := Value;
  Include (FModifyParams, ModifyGroupId);
end;

procedure TMDOSecurityService.SetLastName(Value: string);
begin
  FLastName := Value;
  Include (FModifyParams, ModifyLastName);
end;

procedure TMDOSecurityService.SetMiddleName(Value: string);
begin
  FMiddleName := Value;
  Include (FModifyParams, ModifyMiddleName);
end;

procedure TMDOSecurityService.SetPassword(Value: string);
begin
  FPassword := Value;
  Include (FModifyParams, ModifyPassword);
end;

procedure TMDOSecurityService.SetSecurityAction(Value: TSecurityAction);
begin
  FSecurityAction := Value;
  if Value = ActionDeleteUser then
    ClearParams;
end;

procedure TMDOSecurityService.SetServiceStartOptions;
var
  Len: UShort;
begin
  case FSecurityAction of
    ActionAddUser:
    begin
      Action := isc_action_svc_add_user;
      if ( Pos(' ', FUserName) > 0 ) then
        MDOError(mdoeStartParamsError, [nil]);
      Len := Length(FUserName);
      if (Len = 0) then
        MDOError(mdoeStartParamsError, [nil]);
      ServiceStartParams  := Char(isc_action_svc_add_user);
      ServiceStartAddParam (FSQLRole, SPBConstantValues[isc_spb_sql_role_name]);
      ServiceStartAddParam (FUserName, isc_spb_sec_username);
      ServiceStartAddParam (FUserID, isc_spb_sec_userid);
      ServiceStartAddParam (FGroupID, isc_spb_sec_groupid);
      ServiceStartAddParam (FPassword, isc_spb_sec_password);
      ServiceStartAddParam (FFirstName, isc_spb_sec_firstname);
      ServiceStartAddParam (FMiddleName, isc_spb_sec_middlename);
      ServiceStartAddParam (FLastName, isc_spb_sec_lastname);
    end;
    ActionDeleteUser:
    begin
      Action := isc_action_svc_delete_user;
      Len := Length(FUserName);
      if (Len = 0) then
        MDOError(mdoeStartParamsError, [nil]);
      ServiceStartParams  := Char(isc_action_svc_delete_user);
      ServiceStartAddParam (FSQLRole, SPBConstantValues[isc_spb_sql_role_name]);
      ServiceStartAddParam (FUserName, isc_spb_sec_username);
    end;
    ActionModifyUser:
    begin
      Action := isc_action_svc_modify_user;
      Len := Length(FUserName);
      if (Len = 0) then
        MDOError(mdoeStartParamsError, [nil]);
      ServiceStartParams  := Char(isc_action_svc_modify_user);
      ServiceStartAddParam (FSQLRole, SPBConstantValues[isc_spb_sql_role_name]);
      ServiceStartAddParam (FUserName, isc_spb_sec_username);
      if (ModifyUserId in FModifyParams) then
        ServiceStartAddParam (FUserID, isc_spb_sec_userid);
      if (ModifyGroupId in FModifyParams) then
        ServiceStartAddParam (FGroupID, isc_spb_sec_groupid);
      if (ModifyPassword in FModifyParams) then
        ServiceStartAddParam (FPassword, isc_spb_sec_password);
      if (ModifyFirstName in FModifyParams) then
        ServiceStartAddParam (FFirstName, isc_spb_sec_firstname);
      if (ModifyMiddleName in FModifyParams) then
        ServiceStartAddParam (FMiddleName, isc_spb_sec_middlename);
      if (ModifyLastName in FModifyParams) then
        ServiceStartAddParam (FLastName, isc_spb_sec_lastname);
    end;
  end;
  ClearParams;
end;

procedure TMDOSecurityService.SetUserID(Value: Integer);
begin
  FUserId := Value;
  Include (FModifyParams, ModifyUserId);
end;

{ TMDOUnStructuredService }
{
************************** TMDOControlAndQueryService **************************
}
constructor TMDOControlAndQueryService.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEof := False;
  FAction := 0;
end;

function TMDOControlAndQueryService.GetNextChunk: string;
var
  Length: Integer;
begin
  if (FEof = True) then
  begin
    result := '';
    exit;
  end;
  if (FAction = 0) then
    MDOError(mdoeQueryParamsError, [nil]);
  ServiceQueryParams := Char(isc_info_svc_to_eof);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_to_eof)) then
    MDOError(mdoeOutputParsingError, [nil]);
  Length := isc_vax_integer(OutputBuffer + 1, 2);
  if (OutputBuffer[3 + Length] = Char(isc_info_truncated)) then
    FEof := False
  else
    if (OutputBuffer[3 + Length] = Char(isc_info_end)) then
      FEof := True
    else
      MDOError(mdoeOutputParsingError, [nil]);
  OutputBuffer[3 + Length] := #0;
  result := String(PChar(@OutputBuffer[3]));
end;

function TMDOControlAndQueryService.GetNextLine: string;
var
  Length: Integer;
begin
  if (FEof = True) then
  begin
    result := '';
    exit;
  end;
  if (FAction = 0) then
    MDOError(mdoeQueryParamsError, [nil]);
  ServiceQueryParams := Char(isc_info_svc_line);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_line)) then
    MDOError(mdoeOutputParsingError, [nil]);
  Length := isc_vax_integer(OutputBuffer + 1, 2);
  if (OutputBuffer[3 + Length] <> Char(isc_info_end)) then
    MDOError(mdoeOutputParsingError, [nil]);
  if (length <> 0) then
    FEof := False
  else
  begin
    result := '';
    FEof := True;
    exit;
  end;
  OutputBuffer[3 + Length] := #0;
  result := String(PChar(@OutputBuffer[3]));
end;

procedure TMDOControlAndQueryService.SetAction(Value: Integer);
begin
  FEof := False;
  FAction := Value;
end;


{ TMDOLogService }

{
******************************** TMDOLogService ********************************
}
procedure TMDOLogService.SetServiceStartOptions;
begin
  Action := isc_action_svc_get_ib_log;
  ServiceStartParams  := Char(isc_action_svc_get_ib_log);
end;

{ TDatabaseInfo }

{
******************************** TDatabaseInfo *********************************
}
constructor TDatabaseInfo.Create;
begin
  DbName := nil;
end;

destructor TDatabaseInfo.Destroy;
begin
  DbName := nil;
  inherited Destroy;
end;

{ TLicenseInfo }

{
********************************* TLicenseInfo *********************************
}
constructor TLicenseInfo.Create;
begin
  Key := nil;
  Id := nil;
  Desc := nil;
end;

destructor TLicenseInfo.Destroy;
begin
  Key := nil;
  Id := nil;
  Desc := nil;
  inherited Destroy;
end;

{ TConfigFileData }

{
******************************* TConfigFileData ********************************
}
constructor TConfigFileData.Create;
begin
  ConfigFileValue := nil;
  ConfigFileKey := nil;
end;

destructor TConfigFileData.Destroy;
begin
  ConfigFileValue := nil;
  ConfigFileKey := nil;
  inherited Destroy;
end;

{ TConfigParams }

{
******************************** TConfigParams *********************************
}
constructor TConfigParams.Create;
begin
  ConfigFileData := TConfigFileData.Create;
  ConfigFileParams := nil;
end;

destructor TConfigParams.Destroy;
begin
  ConfigFileData.Free;
  ConfigFileParams := nil;
  inherited Destroy;
end;

end.
