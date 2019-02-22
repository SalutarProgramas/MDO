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

unit MDOInstall;

interface

uses TypInfo, SysUtils, Classes, MDO, MDOInstallHeader, MDOIntf, MDOConst;

type

  TIscError = MSG_NO;
  TMDOInstallerError = (ieSuccess,
                       ieDelphiException,
                       ieNoOptionsSet,
                       ieNoDestinationDirectory,
                       ieNosourceDirectory,
                       ieNoUninstallFile,
                       ieOptionNeedsClient,
                       ieOptionNeedsServer,
                       ieInvalidOption,
                       ieInvalidOnErrorResult,
                       ieInvalidOnStatusResult);

  TMainOption = (moServer,
                 moClient,
                 moConServer,
                 moGuiTools,
                 moDocumentation,
                 moDevelopment);


  TExamplesOption = (exDB, exAPI);
  TCmdOption = (cmDBMgmt, cmDBQuery, cmUsrMgmt);
  TConnectivityOption = (cnODBC, cnOLEDB, cnJDBC);

  TMainOptions = set of TMainOption;
  TExamplesOptions = set of TExamplesOption;
  TCmdOptions = set of TCmdOption;
  TConnectivityOptions = set of TConnectivityOption;

  TErrorResult = (erAbort, erContinue, erRetry);
  TStatusResult = (srAbort, srContinue);
  TWarningResult = (wrAbort, wrContinue);

  TMDOSetupOnStatus = function (Sender : TObject; StatusComment : string): 
          TStatusResult of object;
  TMDOSetupOnWarning = function (Sender :TObject; WarningCode: TIscError; 
          WarningMessage : string): TWarningResult of object;
  TMDOSetupOnError = function (Sender : TObject; IscCode : TIscError; 
          ErrorMessage, ErrorComment : string): TErrorResult of object;
  EIBInstall = class (Exception)
  private
    FInstallerError: TMDOInstallerError;
    FIscError: MSG_NO;
  public
    constructor Create(IscCode : MSG_NO; IscMessage : string); overload; 
            virtual;
    constructor Create(ECode  : TMDOInstallerError; EMessage : string); 
            overload; virtual;
    property InstallerError: TMDOInstallerError read FInstallerError;
    property InstallError: MSG_NO read FIscError;
  end;
  
  EIBInstallError = class (EIBInstall)
  end;
  
  EIBInstallerError = class (EIBInstall)
  end;
  
  TInstallOptions = class (TPersistent)
  private
    FCmdLineTools: TCmdOptions;
    FConnectivityClients: TConnectivityOptions;
    FExamples: TExamplesOptions;
    FMainComponents: TMainOptions;
  published
    property CmdLineTools: TCmdOptions read FCmdLineTools write FCmdLineTools;
    property ConnectivityClients: TConnectivityOptions read 
            FConnectivityClients write FConnectivityClients;
    property Examples: TExamplesOptions read FExamples write FExamples;
    property MainComponents: TMainOptions read FMainComponents write 
            FMainComponents;
  end;
  
  TMDOSetup = class (TComponent)
  private
    FErrorContext: Pointer;
    FIBInstallLoaded: Boolean;
    FMsgFilePath: string;
    FOnError: TMDOSetupOnError;
    FOnStatusChange: TMDOSetupOnStatus;
    FOnWarning: TMDOSetupOnWarning;
    FProgress: Integer;
    FRebootToComplete: Boolean;
    FStatusContext: Pointer;
    procedure SetMsgFilePath(const Value: string);
  protected
    procedure Call(IscCode: MSG_NO);
    function ErrorInternal(IscCode: MSG_NO; const ActionDescription: TEXT): 
            Integer;
    function GetInstallMessage(IscCode : MSG_NO): string;
    procedure IBInstallError(IscCode: MSG_NO);
    function StatusInternal(Status: Integer; const ActionDescription: TEXT): 
            Integer;
  public
    constructor Create(AOwner : TComponent); override;
    property ErrorContext: Pointer read FErrorContext write FErrorContext;
    property MsgFilePath: string read FMsgFilePath write SetMsgFilePath;
    property Progress: Integer read FProgress;
    property RebootToComplete: Boolean read FRebootToComplete;
    property StatusContext: Pointer read FStatusContext write FStatusContext;
  published
    property OnError: TMDOSetupOnError read FOnError write FOnError;
    property OnStatusChange: TMDOSetupOnStatus read FOnStatusChange write 
            FOnStatusChange;
    property OnWarning: TMDOSetupOnWarning read FOnWarning write FOnWarning;
  end;
  
  TMDOInstall = class (TMDOSetup)
  private
    FDestinationDir: string;
    FInstallOptions: TInstallOptions;
    FSourceDir: string;
    FSuggestedDestination: string;
    FUnInstallFile: string;
    procedure GetOptionProperty(InfoType : Integer; Option : TCmdOption; Buffer 
            : Pointer; BufferLen : Cardinal); overload;
    procedure GetOptionProperty(InfoType : Integer; Option : 
            TConnectivityOption; Buffer : Pointer; BufferLen : Cardinal); 
            overload;
    procedure GetOptionProperty(InfoType : Integer; Option : TExamplesOption; 
            Buffer : Pointer; BufferLen : Cardinal); overload;
    procedure GetOptionProperty(InfoType : Integer; Option : TMainOption; 
            Buffer : Pointer; BufferLen : Cardinal); overload;
    procedure InternalSetOptions(pHandle : POPTIONS_HANDLE);
    procedure SetDestination(const Value: string);
    procedure SetInstallOptions(const Value: TInstallOptions);
    procedure SetSource(const Value: string);
    procedure SuggestDestination;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetOptionDescription(Option : TCmdOption): string; overload;
    function GetOptionDescription(Option : TConnectivityOption): string; 
            overload;
    function GetOptionDescription(Option : TExamplesOption): string; overload;
    function GetOptionDescription(Option : TMainOption): string; overload;
    function GetOptionName(Option : TCmdOption): string; overload;
    function GetOptionName(Option : TConnectivityOption): string; overload;
    function GetOptionName(Option : TExamplesOption): string; overload;
    function GetOptionName(Option : TMainOption): string; overload;
    function GetOptionSpaceRequired(Option : TCmdOption): LongWord; overload;
    function GetOptionSpaceRequired(Option : TConnectivityOption): LongWord; 
            overload;
    function GetOptionSpaceRequired(Option : TExamplesOption): LongWord; 
            overload;
    function GetOptionSpaceRequired(Option : TMainOption): LongWord; overload;
    procedure InstallCheck;
    procedure InstallExecute;
    property SuggestedDestination: string read FSuggestedDestination;
    property UnInstallFile: string read FUnInstallFile;
  published
    property DestinationDirectory: string read FDestinationDir write 
            SetDestination;
    property InstallOptions: TInstallOptions read FInstallOptions write 
            SetInstallOptions;
    property SourceDirectory: string read FSourceDir write SetSource;
  end;
  
  TMDOUnInstall = class (TMDOSetup)
  private
    FUnInstallFile: string;
  public
    procedure UnInstallCheck;
    procedure UnInstallExecute;
    property UnInstallFile: string read FUnInstallFile write FUnInstallFile;
  end;
  
implementation

const
  IBInstallerMessages : array[TMDOInstallerError] of string = (
    SSuccess,
    SDelphiException,
    SNoOptionsSet,
    SNoDestinationDirectory,
    SNosourceDirectory,
    SNoUninstallFile,
    SOptionNeedsClient,
    SOptionNeedsServer,
    SInvalidOption,
    SInvalidOnErrorResult,
    SInvalidOnStatusResult
    );



procedure IBInstallerError(ECode: TMDOInstallerError; const Args: array of const);
begin
  raise EIBInstallerError.Create(ECode, Format(IBInstallerMessages[ECode], Args));
end;

function ErrorCallback(IscCode: MSG_NO; UserContext: Pointer; const ActionDescription:
                       TEXT): Integer; stdcall;
begin
  Result := TMDOSetup(UserContext).ErrorInternal(IscCode, ActionDescription);
end;

function StatusCallback(Status : Integer; UserContext: Pointer; const ActionDescription:
                       TEXT): Integer; stdcall;
begin
  Result := TMDOSetup(UserContext).StatusInternal(Status, ActionDescription);
end;

{ TMDOSetup }

{
********************************** TMDOSetup ***********************************
}
constructor TMDOSetup.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FIBInstallLoaded := False;
  CheckFBInstallLoaded;
  FIBInstallLoaded := True;
  FRebootToComplete := False;
  FProgress := 0;
end;

procedure TMDOSetup.Call(IscCode: MSG_NO);
begin
  if IscCode = isc_install_success then
    Exit;
  
  if IscCode < isc_install_success then
  begin
    if Assigned(FOnWarning) then
    begin
      if FOnWarning(self, IscCode, GetInstallMessage(IscCode)) = wrAbort then
        IBInstallError(IscCode);
      Exit;
    end
    else
      IBInstallError(IscCode);
  end;
  IBInstallError(IscCode);
end;

function TMDOSetup.ErrorInternal(IscCode: MSG_NO; const ActionDescription: 
        TEXT): Integer;
var
  ErrorComment: string;
begin
  if(ActionDescription <> nil) and (ActionDescription[0] <> #0) then
  begin
    SetLength(ErrorComment, StrLen(ActionDescription));
    StrCopy(PChar(ErrorComment), ActionDescription);
  end
  else
    ErrorComment := '';
  
  if(Isccode = isc_install_fp_copy_delayed) or
    (Isccode = isc_install_fp_delete_delayed) then
  begin
    FRebootToComplete := True;
    Result := isc_install_fp_continue;
    exit;
  end;
  
  if Assigned(FOnError) then
    case FOnError(self, IscCode, GetInstallMessage(IscCode), ErrorComment) of
      erAbort:
        Result := isc_install_fp_abort;
      erContinue:
        Result := isc_install_fp_continue;
      erRetry:
        Result := isc_install_fp_retry;
      else
        Result := isc_install_fp_abort;
    end
  else
    Result := isc_install_fp_abort;
end;

function TMDOSetup.GetInstallMessage(IscCode : MSG_NO): string;
var
  status: MSG_NO;
  IscMessage: string;
begin
  SetLength(IscMessage, ISC_INSTALL_MAX_MESSAGE_LEN * 2);
  status := isc_install_get_message(0, IscCode, PChar(IscMessage),
                                    ISC_INSTALL_MAX_MESSAGE_LEN * 2);
  
  if status <> isc_install_success then
    isc_install_get_message(0, status, PChar(IscMessage),
                            ISC_INSTALL_MAX_MESSAGE_LEN * 2);
  
  SetLength(IscMessage, StrLen(PChar(IscMessage)));
  result := IscMessage;
end;

procedure TMDOSetup.IBInstallError(IscCode: MSG_NO);
begin
  raise EIBInstallError.Create(IscCode, GetInstallMessage(IscCode));
end;

procedure TMDOSetup.SetMsgFilePath(const Value: string);
begin
  if FMsgFilePath <> Value then
  begin
    Call(isc_install_load_external_text(PChar(Value)));
    FMsgFilePath := Value;
  end;
end;

function TMDOSetup.StatusInternal(Status: Integer; const ActionDescription: 
        TEXT): Integer;
var
  StatusComment: string;
begin
  FProgress := Status;
  if(ActionDescription <> nil) and (ActionDescription[0] <> #0) then
  begin
    SetLength(StatusComment, StrLen(ActionDescription));
    StrCopy(PChar(StatusComment), ActionDescription);
  end
  else
   StatusComment := '';
  
  if Assigned(FOnStatusChange) then
    case  FOnStatusChange(self, StatusComment) of
      srAbort:
       Result := isc_install_fp_abort;
      srContinue:
       Result := isc_install_fp_continue;
      else
       Result := isc_install_fp_continue;
    end
   else
     Result := isc_install_fp_continue;
end;

{ TMDOInstall }

{
********************************* TMDOInstall **********************************
}
constructor TMDOInstall.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBInstallLoaded := False;
  CheckFBInstallLoaded;
  FIBInstallLoaded := True;
  FInstallOptions := TInstallOptions.Create;
  SuggestDestination;
end;

destructor TMDOInstall.Destroy;
begin
  if FIBInstallLoaded then
    FInstallOptions.Free;
  inherited Destroy;
end;

function TMDOInstall.GetOptionDescription(Option : TCmdOption): string;
var
  OptionDesc: string;
begin
  SetLength(OptionDesc, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opdescription, Option, PChar(OptionDesc),
                   ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionDesc, StrLen(PChar(OptionDesc)));
  Result := OptionDesc;
end;

function TMDOInstall.GetOptionDescription(Option : TConnectivityOption): string;
var
  OptionDesc: string;
begin
  SetLength(OptionDesc, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opdescription, Option, PChar(OptionDesc),
                   ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionDesc, StrLen(PChar(OptionDesc)));
  Result := OptionDesc;
end;

function TMDOInstall.GetOptionDescription(Option : TExamplesOption): string;
var
  OptionDesc: string;
begin
  SetLength(OptionDesc, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opdescription, Option, PChar(OptionDesc),
                   ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionDesc, StrLen(PChar(OptionDesc)));
  Result := OptionDesc;
end;

function TMDOInstall.GetOptionDescription(Option : TMainOption): string;
var
  OptionDesc: string;
begin
  SetLength(OptionDesc, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opdescription, Option, PChar(OptionDesc),
                   ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionDesc, StrLen(PChar(OptionDesc)));
  Result := OptionDesc;
end;

function TMDOInstall.GetOptionName(Option : TCmdOption): string;
var
  OptionName: string;
begin
  SetLength(OptionName, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opname, Option, PChar(OptionName),
                   ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionName, StrLen(PChar(OptionName)));
  Result := OptionName;
end;

function TMDOInstall.GetOptionName(Option : TConnectivityOption): string;
var
  OptionName: string;
begin
  SetLength(OptionName, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opname, Option, PChar(OptionName),
                  ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionName, StrLen(PChar(OptionName)));
  Result := OptionName;
end;

function TMDOInstall.GetOptionName(Option : TExamplesOption): string;
var
  OptionName: string;
begin
  SetLength(OptionName, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opname, Option, PChar(OptionName),
                   ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionName, StrLen(PChar(OptionName)));
  Result := OptionName;
end;

function TMDOInstall.GetOptionName(Option : TMainOption): string;
var
  OptionName: string;
begin
  SetLength(OptionName, ISC_INSTALL_MAX_MESSAGE_LEN);
  GetOptionProperty(isc_install_info_opname, Option, PChar(OptionName),
                   ISC_INSTALL_MAX_MESSAGE_LEN);
  SetLength(OptionName, StrLen(PChar(OptionName)));
  Result := OptionName;
end;

procedure TMDOInstall.GetOptionProperty(InfoType : Integer; Option : TCmdOption;
        Buffer : Pointer; BufferLen : Cardinal);
var
  IscOption: OPT;
begin
  case Option of
    cmDBMgmt:
      IscOption := IB_CMD_TOOLS_DB_MGMT;
        cmDBQuery:
      IscOption := IB_CMD_TOOLS_DB_QUERY;
        else
      IscOption := IB_CMD_TOOLS_USR_MGMT;
  end;
  Call(isc_install_get_info(InfoType, IscOption, Buffer, BufferLen));
end;

procedure TMDOInstall.GetOptionProperty(InfoType : Integer; Option : 
        TConnectivityOption; Buffer : Pointer; BufferLen : Cardinal);
var
  IscOption: OPT;
begin
  case Option of
    cnODBC:
     IscOption :=  IB_ODBC_CLIENT;
    cnOLEDB:
     IscOption := IB_OLEDB_CLIENT;
    else
     IscOption := IB_JDBC_CLIENT;
  end;
  Call(isc_install_get_info(InfoType, IscOption, Buffer, BufferLen));
end;

procedure TMDOInstall.GetOptionProperty(InfoType : Integer; Option : 
        TExamplesOption; Buffer : Pointer; BufferLen : Cardinal);
var
  IscOption: OPT;
begin
  case Option of
     exDB:
       IscOption := IB_EXAMPLE_DB;
     else
       IscOption := IB_EXAMPLE_API;
   end;
   Call(isc_install_get_info(InfoType, IscOption, Buffer, BufferLen));
end;

procedure TMDOInstall.GetOptionProperty(InfoType : Integer; Option : 
        TMainOption; Buffer : Pointer; BufferLen : Cardinal);
var
  IscOption: OPT;
begin
  case Option of
    moClient:
     IscOption := IB_CLIENT;
    moDevelopment:
     IscOption := IB_DEV;
    moServer:
     IscOption :=  IB_SERVER;
    moDocumentation:
     IscOption := IB_DOC;
    moGuiTools:
     IscOption := IB_GUI_TOOLS;
    else
     IscOption :=  IB_CONNECTIVITY_SERVER;
  end;
  Call(isc_install_get_info(InfoType, IscOption, Buffer, BufferLen));
end;

function TMDOInstall.GetOptionSpaceRequired(Option : TCmdOption): LongWord;
var
  OptionSpace: LongWord;
begin
  GetOptionProperty(isc_install_info_opspace, Option, @OptionSpace,
                   Cardinal(SizeOf(OptionSpace)));
  Result := OptionSpace;
end;

function TMDOInstall.GetOptionSpaceRequired(Option : TConnectivityOption): 
        LongWord;
var
  OptionSpace: LongWord;
begin
  GetOptionProperty(isc_install_info_opspace, Option, @OptionSpace,
                    Cardinal(SizeOf(OptionSpace)));
  Result := OptionSpace;
end;

function TMDOInstall.GetOptionSpaceRequired(Option : TExamplesOption): LongWord;
var
  OptionSpace: LongWord;
begin
  GetOptionProperty(isc_install_info_opspace, Option, @OptionSpace,
                    Cardinal(SizeOf(OptionSpace)));
  Result := OptionSpace;
end;

function TMDOInstall.GetOptionSpaceRequired(Option : TMainOption): LongWord;
var
  OptionSpace: LongWord;
begin
  GetOptionProperty(isc_install_info_opspace, Option, @OptionSpace,
                    Cardinal(SizeOf(OptionSpace)));
  Result := OptionSpace;
end;

procedure TMDOInstall.InstallCheck;
var
  Handle: OPTIONS_HANDLE;
  SrcDir, DestDir: PChar;
begin
  Handle := 0;
  InternalSetOptions(@Handle);
  
  if FSourceDir = '' then
    SrcDir := nil
  else
    SrcDir := PChar(FSourceDir);
  
  if FDestinationDir = '' then
    DestDir := nil
  else
    DestDir := PChar(FDestinationDir);
  
  try
    Call(isc_install_precheck(Handle, SrcDir, DestDir));
  finally
    isc_install_clear_options(@Handle);
  end;
end;

procedure TMDOInstall.InstallExecute;
var
  Handle: OPTIONS_HANDLE;
begin
  Handle := 0;
  InternalSetOptions(@Handle);
  
  if Handle = 0 then
    IBInstallerError(ieNoOptionsSet, []);
  
  try
    SetLength(FUninstallFile, ISC_INSTALL_MAX_PATH);
    Call(isc_install_execute(Handle, PChar(FSourceDir), PChar(FDestinationDir),
                            StatusCallback, Pointer(self), ErrorCallback,
                            Pointer(self), PChar(FUninstallFile)));
    SetLength(FUninstallFile, StrLen(PChar(FUninstallFile)));
  finally
    isc_install_clear_options(@Handle);
  end;
end;

procedure TMDOInstall.InternalSetOptions(pHandle : POPTIONS_HANDLE);
begin
  with FInstallOptions do
  begin
    if FMainComponents <> [] then
    begin
     if moClient in  FMainComponents then
       isc_install_set_option(pHandle, IB_CLIENT);
     if moDevelopment in FMainComponents then
       isc_install_set_option(pHandle, IB_DEV);
     if moServer in FMainComponents then
       isc_install_set_option(pHandle, IB_SERVER);
     if  moDocumentation in FMainComponents then
       isc_install_set_option(pHandle, IB_DOC);
     if moConServer in FMainComponents then
       isc_install_set_option(pHandle, IB_CONNECTIVITY_SERVER);
     if moGuiTools in FMainComponents then
       isc_install_set_option(pHandle, IB_GUI_TOOLS);
    end;
  
    if FExamples <> [] then
    begin
     if exDB in FExamples  then
       isc_install_set_option(pHandle, IB_EXAMPLE_DB);
     if exAPI in FExamples then
       isc_install_set_option(pHandle, IB_EXAMPLE_API);
    end;
  
    if FCmdLineTools  <> [] then
    begin
     if cmDBMgmt in FCmdLineTools then
        isc_install_set_option(pHandle, IB_CMD_TOOLS_DB_MGMT);
     if cmDBQuery in FCmdLineTools then
        isc_install_set_option(pHandle, IB_CMD_TOOLS_DB_QUERY);
     if cmUsrMgmt in FCmdLineTools then
        isc_install_set_option(pHandle, IB_CMD_TOOLS_USR_MGMT);
    end;
  
    if FConnectivityClients <> [] then
    begin
     if cnODBC in FConnectivityClients then
       isc_install_set_option(pHandle, IB_ODBC_CLIENT);
     if cnOLEDB in FConnectivityClients then
       isc_install_set_option(pHandle, IB_OLEDB_CLIENT);
     if  cnJDBC in FConnectivityClients then
        isc_install_set_option(pHandle, IB_JDBC_CLIENT);
    end;
  end;
end;

procedure TMDOInstall.SetDestination(const Value: string);
var
  IscCode: MSG_NO;
begin
  if Value <> '' then
  begin
    IscCode := isc_install_precheck(0, nil, PChar(Value));
    if(IscCode > isc_install_success) then
     IBInstallError(IscCode);
  end;
  FDestinationDir := Value;
end;

procedure TMDOInstall.SetInstallOptions(const Value: TInstallOptions);
begin
  if FInstallOptions <> Value then
    FInstallOptions.Assign(Value);
end;

procedure TMDOInstall.SetSource(const Value: string);
var
  IscCode: MSG_NO;
begin
  if Value <> '' then
  begin
    IscCode := isc_install_precheck(0, PChar(Value), nil);
    if(IscCode > isc_install_success) then
      IBInstallError(IscCode);
    end;
  FSourceDir := Value;
end;

procedure TMDOInstall.SuggestDestination;
begin
  SetLength(FSuggestedDestination, ISC_INSTALL_MAX_PATH);
  Call(isc_install_get_info(isc_install_info_destination, 0, PChar(FSuggestedDestination),
                           ISC_INSTALL_MAX_PATH));
  SetLength(FSuggestedDestination, StrLen(PChar(FSuggestedDestination)));
end;

{ TMDOUnInstall }

{
******************************** TMDOUnInstall *********************************
}
procedure TMDOUnInstall.UnInstallCheck;
begin
  if FUninstallFile = '' then
    IBInstallerError(ieNoUninstallFile, []);
  
  Call(isc_uninstall_precheck(PChar(FUninstallFile)));
end;

procedure TMDOUnInstall.UnInstallExecute;
begin
  if FUninstallFile = '' then
    IBInstallerError(ieNoUninstallFile, []);
  
  Call(isc_uninstall_execute(PChar(FUninstallFile), StatusCallback, Pointer(self),
                             ErrorCallback, Pointer(self)));
end;

{ EIBInstall }

{
********************************** EIBInstall **********************************
}
constructor EIBInstall.Create(IscCode : MSG_NO; IscMessage : string);
begin
  inherited Create(IscMessage);
  FIscError := IscCode;
  FInstallerError := ieSuccess;
end;

constructor EIBInstall.Create(ECode  : TMDOInstallerError; EMessage : string);
begin
  inherited Create(EMessage);
  FInstallerError := ECode;
  FIscError := isc_install_success;
end;

end.
