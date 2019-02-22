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

unit MDOSQLMonitor;

interface

{$I ..\MDO.inc}

uses
  SysUtils, Windows, Messages, Classes, Forms, Controls, Dialogs, StdCtrls,
  MDO, MDOUtils, MDOSQL, MDOCustomDataSet, MDODatabase, MDOServices, MDOConst;

const
  WM_MIN_IBSQL_MONITOR = WM_USER;
  WM_MAX_IBSQL_MONITOR = WM_USER + 512;
  WM_IBSQL_SQL_EVENT = WM_MIN_IBSQL_MONITOR + 1;

type

  TTraceObject = class (TObject)
    FDataType: TTraceFlag;
    FMsg: string;
    FTimeStamp: TDateTime;
  public
    constructor Create(Msg : String; DataType : TTraceFlag); overload;
    constructor Create(obj : TTraceObject); overload;
  end;
  

  TMDOCustomSQLMonitor = class;

  { TMDOSQLMonitor }
  TSQLEvent = procedure (EventText: String; EventTime : TDateTime) of object;
  TMDOCustomSQLMonitor = class (TComponent)
  private
    FEnabled: Boolean;
    FHWnd: HWND;
    FOnSQLEvent: TSQLEvent;
    FTraceFlags: TTraceFlags;
    procedure MonitorWndProc(var Message : TMessage);
    procedure SetEnabled(const Value: Boolean);
  protected
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property OnSQL: TSQLEvent read FOnSQLEvent write FOnSQLEvent;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release;
    property Handle: HWND read FHWnd write FHWnd;
  end;
  
  TMDOSQLMonitor = class (TMDOCustomSQLMonitor)
  published
    property Enabled;
    property OnSQL;
    property TraceFlags;
  end;

{$IFDEF MDO_DELPHI5}
  IIBSQLMonitorHook = interface (IUnknown)
{$ENDIF}
{$IFDEF MDO_DELPHI6_UP}
  IIBSQLMonitorHook = interface (IInterface)
{$ENDIF}
    ['{CF65434C-9B75-4298-BA7E-E6B85B3C769D}']
    procedure DBConnect(db: TMDODatabase);
    procedure DBDisconnect(db: TMDODatabase);
    function GetEnabled: Boolean;
    function GetMonitorCount: Integer;
    function GetTraceFlags: TTraceFlags;
    procedure RegisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TMDOCustomSQLMonitor);
    procedure SendMisc(Msg : String);
    procedure ServiceAttach(service: TMDOCustomService);
    procedure ServiceDetach(service: TMDOCustomService);
    procedure ServiceQuery(service: TMDOCustomService);
    procedure ServiceStart(service: TMDOCustomService);
    procedure SetEnabled(const Value: Boolean);
    procedure SetTraceFlags(const Value: TTraceFlags);
    procedure SQLExecute(qry: TMDOSQL);
    procedure SQLFetch(qry: TMDOSQL);
    procedure SQLPrepare(qry: TMDOSQL);
    procedure TRCommit(tr: TMDOTransaction);
    procedure TRCommitRetaining(tr: TMDOTransaction);
    procedure TRRollback(tr: TMDOTransaction);
    procedure TRRollbackRetaining(tr: TMDOTransaction);
    procedure TRStart(tr: TMDOTransaction);
    procedure UnregisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
  end;
  

function MonitorHook: IIBSQLMonitorHook;
procedure EnableMonitoring;
procedure DisableMonitoring;
function MonitoringEnabled: Boolean;

implementation

uses
  contnrs;

type

  { TMDOSQLMonitorHook }
  TMDOSQLMonitorHook = class (TInterfacedObject, IIBSQLMonitorHook)
  private
    FEnabled: Boolean;
    FEventsCreated: Boolean;
    FTraceFlags: TTraceFlags;
    procedure CreateEvents;
  protected
    procedure WriteSQLData(Text: String; DataType: TTraceFlag);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DBConnect(db: TMDODatabase); virtual;
    procedure DBDisconnect(db: TMDODatabase); virtual;
    function GetEnabled: Boolean;
    function GetMonitorCount: Integer;
    function GetTraceFlags: TTraceFlags;
    procedure RegisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TMDOCustomSQLMonitor);
    procedure SendMisc(Msg : String);
    procedure ServiceAttach(service: TMDOCustomService); virtual;
    procedure ServiceDetach(service: TMDOCustomService); virtual;
    procedure ServiceQuery(service: TMDOCustomService); virtual;
    procedure ServiceStart(service: TMDOCustomService); virtual;
    procedure SetEnabled(const Value: Boolean);
    procedure SetTraceFlags(const Value: TTraceFlags);
    procedure SQLExecute(qry: TMDOSQL); virtual;
    procedure SQLFetch(qry: TMDOSQL); virtual;
    procedure SQLPrepare(qry: TMDOSQL); virtual;
    procedure TRCommit(tr: TMDOTransaction); virtual;
    procedure TRCommitRetaining(tr: TMDOTransaction); virtual;
    procedure TRRollback(tr: TMDOTransaction); virtual;
    procedure TRRollbackRetaining(tr: TMDOTransaction); virtual;
    procedure TRStart(tr: TMDOTransaction); virtual;
    procedure UnregisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    property Enabled: Boolean read GetEnabled write SetEnabled default true;
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
  end;
  
  { There are two possible objects.  One is a trace message object.
    This object holds the flag of the trace type plus the message.
    The second object is a Release object.  It holds the handle that
    the CM_RELEASE message is to be queued to. }

{***********************
  TTraceObject = Class(TObject)
    FDataType : TTraceFlag;
    FMsg : String;
    FTimeStamp : TDateTime;
  public
    constructor Create(Msg : String; DataType : TTraceFlag); overload;
    constructor Create(obj : TTraceObject); overload;
  end;
******************}
  TReleaseObject = class (TObject)
    FHandle: THandle;
  public
    constructor Create(Handle : THandle);
  end;
  
  TWriterThread = class (TThread)
  private
    FMsgs: TObjectList;
    procedure RemoveFromList;
  protected
    procedure BeginWrite;
    procedure EndWrite;
    procedure Execute; override;
    procedure Lock;
    procedure Unlock;
    procedure WriteToBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReleaseMonitor(HWnd : THandle);
    procedure WriteSQLData(Msg : String; DataType : TTraceFlag);
  end;
  
  TReaderThread = class (TThread)
  private
    FMonitors: TObjectList;
    st: TTraceObject;
  protected
    procedure BeginRead;
    procedure EndRead;
    procedure Execute; override;
    procedure ReadSQLData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMonitor(Arg : TMDOCustomSQLMonitor);
    procedure RemoveMonitor(Arg : TMDOCustomSQLMonitor);
  end;
  
const
  MonitorHookNames: array[0..5] of String = (
    'IB.SQL.MONITOR.Mutex4_1',
    'IB.SQL.MONITOR.SharedMem4_1',
    'IB.SQL.MONITOR.WriteEvent4_1',
    'IB.SQL.MONITOR.WriteFinishedEvent4_1',
    'IB.SQL.MONITOR.ReadEvent4_1',
    'IB.SQL.MONITOR.ReadFinishedEvent4_1'
  );
  cMonitorHookSize = 1024;
  cMaxBufferSize = cMonitorHookSize - (4 * SizeOf(Integer)) - SizeOf(TDateTime);
  cDefaultTimeout = 500; { 1 seconds }

var
  FSharedBuffer,
  FWriteLock,
  FWriteEvent,
  FWriteFinishedEvent,
  FReadEvent,
  FReadFinishedEvent : THandle;
  FBuffer : PChar;
  FMonitorCount,
  FReaderCount,
  FTraceDataType,
  FBufferSize : PInteger;
  FTimeStamp : PDateTime;

  FWriterThread : TWriterThread;
  FReaderThread : TReaderThread;
  _MonitorHook: TMDOSQLMonitorHook;
  bDone: Boolean;
  CS : TRTLCriticalSection;

{ TMDOCustomSQLMonitor }

{
***************************** TMDOCustomSQLMonitor *****************************
}
constructor TMDOCustomSQLMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTraceFlags := [tfqPrepare .. tfMisc];
  FEnabled := true;
  if not (csDesigning in ComponentState) then
  begin
    {$IFDEF MDO_DELPHI6_UP}
      FHWnd := Classes.AllocateHWnd(MonitorWndProc);
    {$ELSE}
      FHWnd := AllocateHWnd(MonitorWndProc);
    {$ENDIF}
    MonitorHook.RegisterMonitor(self);
  end;
end;

destructor TMDOCustomSQLMonitor.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FEnabled then
      MonitorHook.UnregisterMonitor(self);
    {$IFDEF MDO_DELPHI6_UP}
      Classes.DeallocateHWnd(FHWnd);
    {$ELSE}
      DeallocateHwnd(FHWnd);
    {$ENDIF}
  end;
  inherited Destroy;
end;

procedure TMDOCustomSQLMonitor.MonitorWndProc(var Message : TMessage);
var
  st: TTraceObject;
begin
  case Message.Msg of
    WM_IBSQL_SQL_EVENT:
    begin
      st := TTraceObject(Message.LParam);
      if (Assigned(FOnSQLEvent)) and
         (st.FDataType in FTraceFlags) then
        FOnSQLEvent(st.FMsg, st.FTimeStamp);
      st.Free;
    end;
    CM_RELEASE :
      Free;
    else
      DefWindowProc(FHWnd, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

procedure TMDOCustomSQLMonitor.Release;
begin
  MonitorHook.ReleaseMonitor(self);
end;

procedure TMDOCustomSQLMonitor.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if not (csDesigning in ComponentState) then
      if FEnabled then
        Monitorhook.RegisterMonitor(self)
      else
        MonitorHook.UnregisterMonitor(self);
  end;
end;

{ TMDOSQLMonitorHook }

{
****************************** TMDOSQLMonitorHook ******************************
}
constructor TMDOSQLMonitorHook.Create;
begin
  inherited Create;
  FEventsCreated := false;
  FTraceFlags := [tfQPrepare..tfMisc];
  FEnabled := true;
end;

destructor TMDOSQLMonitorHook.Destroy;
begin
  if FEventsCreated then
  begin
    UnmapViewOfFile(FBuffer);
    CloseHandle(FSharedBuffer);
    CloseHandle(FWriteEvent);
    CloseHandle(FWriteFinishedEvent);
    CloseHandle(FReadEvent);
    CloseHandle(FReadFinishedEvent);
    CloseHandle(FWriteLock);
  end;
  inherited Destroy;
end;

procedure TMDOSQLMonitorHook.CreateEvents;
var
  Sa: TSecurityAttributes;
  Sd: TSecurityDescriptor;
  
  function OpenLocalEvent(Idx: Integer): THandle;
  begin
    result := OpenEvent(EVENT_ALL_ACCESS, true, PChar(MonitorHookNames[Idx]));
    if result = 0 then
      MDOError(mdoeCannotCreateSharedResource, [GetLastError]);
  end;
  
  function CreateLocalEvent(Idx: Integer; InitialState: Boolean): THandle;
  begin
    result := CreateEvent(@sa, true, InitialState, PChar(MonitorHookNames[Idx]));
    if result = 0 then
      MDOError(mdoeCannotCreateSharedResource, [GetLastError]);
  end;
  
begin
  { Setup Secureity so anyone can connect to the MMF/Mutex/Events.  This is
    needed when MDO is used in a Service. }
  
  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;
  
  FSharedBuffer := CreateFileMapping($FFFFFFFF, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1]));
  
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    FSharedBuffer := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1]));
    if (FSharedBuffer = 0) then
      MDOError(mdoeCannotCreateSharedResource, [GetLastError]);
    FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if FBuffer = nil then
      MDOError(mdoeCannotCreateSharedResource, [GetLastError]);
    FMonitorCount := PInteger(FBuffer + cMonitorHookSize - SizeOf(Integer));
    FReaderCount := PInteger(PChar(FMonitorCount) - SizeOf(Integer));
    FTraceDataType := PInteger(PChar(FReaderCount) - SizeOf(Integer));
    FTimeStamp := PDateTime(PChar(FTraceDataType) - SizeOf(TDateTime));
    FBufferSize := PInteger(PChar(FTimeStamp) - SizeOf(Integer));
    FWriteLock := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MonitorHookNames[0]));
    FWriteEvent := OpenLocalEvent(2);
    FWriteFinishedEvent := OpenLocalEvent(3);
    FReadEvent := OpenLocalEvent(4);
    FReadFinishedEvent := OpenLocalEvent(5);
  end
  else
  begin
    FWriteLock := CreateMutex(@sa, False, PChar(MonitorHookNames[0]));
    FWriteEvent := CreateLocalEvent(2, False);
    FWriteFinishedEvent := CreateLocalEvent(3, True);
    FReadEvent := CreateLocalEvent(4, False);
    FReadFinishedEvent := CreateLocalEvent(5, False);
  
    FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    FMonitorCount := PInteger(FBuffer + cMonitorHookSize - SizeOf(Integer));
    FReaderCount := PInteger(PChar(FMonitorCount) - SizeOf(Integer));
    FTraceDataType := PInteger(PChar(FReaderCount) - SizeOf(Integer));
    FTimeStamp := PDateTime(PChar(FTraceDataType) - SizeOf(TDateTime));
    FBufferSize := PInteger(PChar(FTimeStamp) - SizeOf(Integer));
    FMonitorCount^ := 0;
    FReaderCount^ := 0;
    FBufferSize^ := 0;
  end;
  
  { This should never evaluate to true, if it does
  there has been a hiccup somewhere. }
  
  if FMonitorCount^ < 0 then
    FMonitorCount^ := 0;
  if FReaderCount^ < 0 then
    FReaderCount^ := 0;
  FEventsCreated := true;
end;

procedure TMDOSQLMonitorHook.DBConnect(db: TMDODatabase);
var
  st: string;
begin
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Connect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

procedure TMDOSQLMonitorHook.DBDisconnect(db: TMDODatabase);
var
  st: string;
begin
  if (Self = nil) then exit;
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Disconnect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

function TMDOSQLMonitorHook.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMDOSQLMonitorHook.GetMonitorCount: Integer;
begin
  Result := FMonitorCount^;
end;

function TMDOSQLMonitorHook.GetTraceFlags: TTraceFlags;
begin
  Result := FTraceFlags;
end;

procedure TMDOSQLMonitorHook.RegisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
begin
  if not FEventsCreated then
    CreateEvents;
  if not Assigned(FReaderThread) then
    FReaderThread := TReaderThread.Create;
  FReaderThread.AddMonitor(SQLMonitor);
end;

procedure TMDOSQLMonitorHook.ReleaseMonitor(Arg : TMDOCustomSQLMonitor);
begin
  FWriterThread.ReleaseMonitor(Arg.FHWnd);
end;

procedure TMDOSQLMonitorHook.SendMisc(Msg : String);
begin
  if FEnabled then
  begin
    WriteSQLData(Msg, tfMisc);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceAttach(service: TMDOCustomService);
var
  st: string;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Attach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceDetach(service: TMDOCustomService);
var
  st: string;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Detach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceQuery(service: TMDOCustomService);
var
  st: string;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Query]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceStart(service: TMDOCustomService);
var
  st: string;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Start]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
    FEnabled := Value;
  if (not FEnabled) and (Assigned(FWriterThread)) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

procedure TMDOSQLMonitorHook.SetTraceFlags(const Value: TTraceFlags);
begin
  FTraceFlags := Value
end;

procedure TMDOSQLMonitorHook.SQLExecute(qry: TMDOSQL);
var
  st: string;
  i: Integer;
begin
  if FEnabled then
  begin
    if not ((tfQExecute in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags)) ) then
      Exit;
    if qry.Owner is TMDOCustomDataSet then
      st := TMDOCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Execute] ' + qry.SQL.Text; {do not localize}
    if qry.Params.Count > 0 then begin
      for i := 0 to qry.Params.Count - 1 do begin
        st := st + CRLF + '  ' + qry.Params[i].Name + ' = ';
        try
          if qry.Params[i].IsNull then
            st := st + '<NULL>'; {do not localize}
          st := st + qry.Params[i].AsString;
        except
          st := st + '<' + SCantPrintValue + '>';
        end;
      end;
    end;
    WriteSQLData(st, tfQExecute);
  end;
end;

procedure TMDOSQLMonitorHook.SQLFetch(qry: TMDOSQL);
var
  st: string;
begin
  if FEnabled then
  begin
    if not ((tfQFetch in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TMDOCustomDataSet then
      st := TMDOCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Fetch] ' + qry.SQL.Text; {do not localize}
    if (qry.EOF) then
      st := st + CRLF + '  ' + SEOFReached;
    WriteSQLData(st, tfQFetch);
  end;
end;

procedure TMDOSQLMonitorHook.SQLPrepare(qry: TMDOSQL);
var
  st: string;
begin
  if FEnabled then
  begin
    if not ((tfQPrepare in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TMDOCustomDataSet then
      st := TMDOCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Prepare] ' + qry.SQL.Text + CRLF; {do not localize}
    st := st + '  Plan: ' + qry.Plan; {do not localize}
    WriteSQLData(st, tfQPrepare);
  end;
end;

procedure TMDOSQLMonitorHook.TRCommit(tr: TMDOTransaction);
var
  st: string;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit (Hard commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRCommitRetaining(tr: TMDOTransaction);
var
  st: string;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit retaining (Soft commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRRollback(tr: TMDOTransaction);
var
  st: string;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRRollbackRetaining(tr: TMDOTransaction);
var
  st: string;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback retaining (Soft rollback)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRStart(tr: TMDOTransaction);
var
  st: string;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Start transaction]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.UnregisterMonitor(SQLMonitor : 
        TMDOCustomSQLMonitor);
var
  Created: Boolean;
begin
  FReaderThread.RemoveMonitor(SQLMonitor);
  if FReaderThread.FMonitors.Count = 0 then
  begin
    FReaderThread.Terminate;
  
    { There is a possibility of a reader thread, but no writer one.
      When in that situation, the reader needs to be released after
      the terminate is set.  To do that, create a Writer thread, send
      the release code (a string of ' ' and type tfMisc) and then free
      it up. }
  
    Created := false;
    if not Assigned(FWriterThread) then
    begin
      FWriterThread := TWriterThread.Create;
      Created := true;
    end;
    FWriterThread.WriteSQLData(' ', tfMisc);
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
    if Created then
    begin
      FWriterThread.Terminate;
      FWriterThread.WaitFor;
      FreeAndNil(FWriterThread);
    end;
  end;
end;

procedure TMDOSQLMonitorHook.WriteSQLData(Text: String; DataType: TTraceFlag);
begin
  if not FEventsCreated then
    CreateEvents;
  Text := CRLF + '[Application: ' + Application.Title + ']' + CRLF + Text; {do not localize}
  if not Assigned(FWriterThread) then
    FWriterThread := TWriterThread.Create;
  FWriterThread.WriteSQLData(Text, DataType);
end;

{ TWriterThread }

{
******************************** TWriterThread *********************************
}
constructor TWriterThread.Create;
begin
  inherited Create(true);
  FMsgs := TObjectList.Create(true);
  Resume;
end;

destructor TWriterThread.Destroy;
begin
  FMsgs.Free;
  inherited Destroy;
end;

procedure TWriterThread.BeginWrite;
begin
  Lock;
end;

procedure TWriterThread.EndWrite;
begin
  {
   * 1. Wait to end the write until all registered readers have
   *    started to wait for a write event
   * 2. Block all of those waiting for the write to finish.
   * 3. Block all of those waiting for all readers to finish.
   * 4. Unblock all readers waiting for a write event.
   * 5. Wait until all readers have finished reading.
   * 6. Now, block all those waiting for a write event.
   * 7. Unblock all readers waiting for a write to be finished.
   * 8. Unlock the mutex.
   }
  while WaitForSingleObject(FReadEvent, cDefaultTimeout) = WAIT_TIMEOUT do
  begin
    if FMonitorCount^ > 0 then
      InterlockedDecrement(FMonitorCount^);
    if (FReaderCount^ = FMonitorCount^ - 1) or (FMonitorCount^ = 0) then
      SetEvent(FReadEvent);
  end;
  ResetEvent(FWriteFinishedEvent);
  ResetEvent(FReadFinishedEvent);
  SetEvent(FWriteEvent); { Let all readers pass through. }
  while WaitForSingleObject(FReadFinishedEvent, cDefaultTimeout) = WAIT_TIMEOUT do
    if (FReaderCount^ = 0) or (InterlockedDecrement(FReaderCount^) = 0) then
      SetEvent(FReadFinishedEvent);
  ResetEvent(FWriteEvent);
  SetEvent(FWriteFinishedEvent);
  Unlock;
end;

procedure TWriterThread.Execute;
begin
  { Place thread code here }
  while ((not Terminated) and (not bDone)) or
        (FMsgs.Count <> 0) do
  begin
    { Any one listening? }
    if FMonitorCount^ = 0 then
    begin
      if FMsgs.Count <> 0 then
        Synchronize(RemoveFromList);
      Sleep(50);
    end
    else
      { Anything to process? }
      if FMsgs.Count <> 0 then
      begin
       { If the current queued message is a release release the object }
        if FMsgs.Items[0] is TReleaseObject then
          PostMessage(TReleaseObject(FMsgs.Items[0]).FHandle, CM_RELEASE, 0, 0)
        else
        { Otherwise write the TraceObject to the buffer }
        begin
          WriteToBuffer;
        end;
      end
      else
        Sleep(50);
  end;
end;

procedure TWriterThread.Lock;
begin
  WaitForSingleObject(FWriteLock, INFINITE);
end;

procedure TWriterThread.ReleaseMonitor(HWnd : THandle);
begin
  FMsgs.Add(TReleaseObject.Create(HWnd));
end;

procedure TWriterThread.RemoveFromList;
begin
  FMsgs.Remove(FMsgs[0]); { Pop the written item }
end;

procedure TWriterThread.Unlock;
begin
  ReleaseMutex(FWriteLock);
end;

procedure TWriterThread.WriteSQLData(Msg : String; DataType : TTraceFlag);
begin
  FMsgs.Add(TTraceObject.Create(Msg, DataType));
end;

procedure TWriterThread.WriteToBuffer;
var
  i, len: Integer;
  Text: string;
begin
  Lock;
  try
    { If there are no monitors throw out the message
      The alternative is to have messages queue up until a
      monitor is ready.}
  
    if FMonitorCount^ = 0 then
      Synchronize(RemoveFromList)
    else
    begin
      Text := TTraceObject(FMsgs[0]).FMsg;
      i := 1;
      len := Length(Text);
      while (len > 0) do begin
        BeginWrite;
        try
          FTraceDataType^ := Integer(TTraceObject(FMsgs[0]).FDataType);
          FTimeStamp^ := TTraceObject(FMsgs[0]).FTimeStamp;
          FBufferSize^ := Min(len, cMaxBufferSize);
          Move(Text[i], FBuffer[0], FBufferSize^);
          Inc(i, cMaxBufferSize);
          Dec(len, cMaxBufferSize);
        finally
          {Do this in the main thread so the main thread
          adds and deletes}
          Synchronize(RemoveFromList);
          EndWrite;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

{ TTraceObject }

{
********************************* TTraceObject *********************************
}
constructor TTraceObject.Create(Msg : String; DataType : TTraceFlag);
begin
  FMsg := Msg;
  FDataType := DataType;
  FTimeStamp := Now;
end;

constructor TTraceObject.Create(obj : TTraceObject);
begin
  FMsg := obj.FMsg;
  FDataType := obj.FDataType;
  FTimeStamp := obj.FTimeStamp;
end;

{ TReleaseObject }

{
******************************** TReleaseObject ********************************
}
constructor TReleaseObject.Create(Handle : THandle);
begin
  FHandle := Handle;
end;

{ ReaderThread }

{
******************************** TReaderThread *********************************
}
constructor TReaderThread.Create;
begin
  inherited Create(true);
  st := TTraceObject.Create('', tfMisc);
  FMonitors := TObjectList.Create(false);
  InterlockedIncrement(FMonitorCount^);
  Resume;
end;

destructor TReaderThread.Destroy;
begin
  if FMonitorCount^ > 0 then
    InterlockedDecrement(FMonitorCount^);
  FMonitors.Free;
  st.Free;
  inherited Destroy;
end;

procedure TReaderThread.AddMonitor(Arg : TMDOCustomSQLMonitor);
begin
  EnterCriticalSection(CS);
  if FMonitors.IndexOf(Arg) < 0 then
    FMonitors.Add(Arg);
  LeaveCriticalSection(CS);
end;

procedure TReaderThread.BeginRead;
begin
  {
   * 1. Wait for the "previous" write event to complete.
   * 2. Increment the number of readers.
   * 3. if the reader count is the number of interested readers, then
   *    inform the system that all readers are ready.
   * 4. Finally, wait for the FWriteEvent to signal.
   }
  WaitForSingleObject(FWriteFinishedEvent, INFINITE);
  InterlockedIncrement(FReaderCount^);
  if FReaderCount^ = FMonitorCount^ then
    SetEvent(FReadEvent);
  WaitForSingleObject(FWriteEvent, INFINITE);
end;

procedure TReaderThread.EndRead;
begin
  if InterlockedDecrement(FReaderCount^) = 0 then
  begin
    ResetEvent(FReadEvent);
    SetEvent(FReadFinishedEvent);
  end;
end;

procedure TReaderThread.Execute;
var
  i: Integer;
  FTemp: TTraceObject;
begin
  { Place thread code here }
  while (not Terminated) and (not bDone) do
  begin
    ReadSQLData;
    if (st.FMsg <> '') and
       not ((st.FMsg = ' ') and (st.FDataType = tfMisc)) then
    begin
      for i := 0 to FMonitors.Count - 1 do
      begin
        FTemp := TTraceObject.Create(st);
        PostMessage(TMDOCustomSQLMonitor(FMonitors[i]).Handle,
                    WM_IBSQL_SQL_EVENT,
                    0,
                    LPARAM(FTemp));
      end;
    end;
  end;
end;

procedure TReaderThread.ReadSQLData;
begin
  st.FMsg := '';
  BeginRead;
  if not bDone then
  try
    SetString(st.FMsg, FBuffer, FBufferSize^);
    st.FDataType := TTraceFlag(FTraceDataType^);
    st.FTimeStamp := TDateTime(FTimeStamp^);
  finally
    EndRead;
  end;
end;

procedure TReaderThread.RemoveMonitor(Arg : TMDOCustomSQLMonitor);
begin
  EnterCriticalSection(CS);
  FMonitors.Remove(Arg);
  LeaveCriticalSection(CS);
end;

{ Misc methods }

function MonitorHook: IIBSQLMonitorHook;
begin
  if (_MonitorHook = nil) and (not bDone) then
  begin
    EnterCriticalSection(CS);
    if (_MonitorHook = nil) and (not bDone) then
    begin
      _MonitorHook := TMDOSQLMonitorHook.Create;
      _MonitorHook._AddRef;
    end;
    LeaveCriticalSection(CS);
  end;
  result := _MonitorHook;
end;

procedure EnableMonitoring;
begin
  MonitorHook.Enabled := True;
end;

procedure DisableMonitoring;
begin
  MonitorHook.Enabled := False;
end;

function MonitoringEnabled: Boolean;
begin
  result := MonitorHook.Enabled;
end;

procedure CloseThreads;
begin
  if Assigned(FReaderThread) then
  begin
    FReaderThread.Terminate;
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
  end;
  if Assigned(FWriterThread) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

initialization
  InitializeCriticalSection(CS);
  _MonitorHook := nil;
  FWriterThread := nil;
  FReaderThread := nil;
  bDone := False;

finalization
  try
    { Write an empty string to force the reader to unlock during termination }
    bDone := True;
    if Assigned(FReaderThread) then
    begin
      if not Assigned(FWriterThread) then
        FWriterThread := TWriterThread.Create;
      FWriterThread.WriteSQLData(' ', tfMisc);
    end;
    CloseThreads;
    if Assigned(_MonitorHook) then
      _MonitorHook._Release;
  finally
    _MonitorHook := nil;
    DeleteCriticalSection(CS);
  end;
end.
