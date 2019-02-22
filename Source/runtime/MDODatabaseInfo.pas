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

unit MDODatabaseInfo;

interface

uses
  SysUtils, Classes, MDOHeader, MDOExternals, MDO, MDODatabase;

type

  TMDODatabaseInfo = class (TComponent)
  protected
    FBackoutCount: TStringList;
    FDatabase: TMDODatabase;
    FDeleteCount: TStringList;
    FExpungeCount: TStringList;
    FIBLoaded: Boolean;
    FInsertCount: TStringList;
    FPurgeCount: TStringList;
    FReadIdxCount: TStringList;
    FReadSeqCount: TStringList;
    FUpdateCount: TStringList;
    FUserNames: TStringList;
    function GetAllocation: Long;
    function GetBackoutCount: TStringList;
    function GetBaseLevel: Long;
    function GetCurrentMemory: Long;
    function GetDBFileName: string;
    function GetDBImplementationClass: Long;
    function GetDBImplementationNo: Long;
    function GetDBSiteName: string;
    function GetDBSQLDialect: Long;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetFetches: Long;
    function GetForcedWrites: Long;
    function GetInsertCount: TStringList;
    function GetMarks: Long;
    function GetMaxMemory: Long;
    function GetNoReserve: Long;
    function GetNumBuffers: Long;
    function GetODSMajorVersion: Long;
    function GetODSMinorVersion: Long;
    function GetOperationCounts(DBInfoCommand: Integer; FOperation: 
            TStringList): TStringList;
    function GetPageSize: Long;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadOnly: Long;
    function GetReads: Long;
    function GetReadSeqCount: TStringList;
    function GetStringDatabaseInfo(DatabaseInfoCommand: Integer): string;
    function GetSweepInterval: Long;
    function GetUpdateCount: TStringList;
    function GetUserNames: TStringList;
    function GetVersion: string;
    function GetWrites: Long;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    function GetLongDatabaseInfo(DatabaseInfoCommand: Integer): Long;
    property Allocation: Long read GetAllocation;
    property BackoutCount: TStringList read GetBackoutCount;
    property BaseLevel: Long read GetBaseLevel;
    property CurrentMemory: Long read GetCurrentMemory;
    property DBFileName: string read GetDBFileName;
    property DBImplementationClass: Long read GetDBImplementationClass;
    property DBImplementationNo: Long read GetDBImplementationNo;
    property DBSiteName: string read GetDBSiteName;
    property DBSQLDialect: Long read GetDBSQLDialect;
    property DeleteCount: TStringList read GetDeleteCount;
    property ExpungeCount: TStringList read GetExpungeCount;
    property Fetches: Long read GetFetches;
    property ForcedWrites: Long read GetForcedWrites;
    property InsertCount: TStringList read GetInsertCount;
    property Marks: Long read GetMarks;
    property MaxMemory: Long read GetMaxMemory;
    property NoReserve: Long read GetNoReserve;
    property NumBuffers: Long read GetNumBuffers;
    property ODSMajorVersion: Long read GetODSMajorVersion;
    property ODSMinorVersion: Long read GetODSMinorVersion;
    property PageSize: Long read GetPageSize;
    property PurgeCount: TStringList read GetPurgeCount;
    property ReadIdxCount: TStringList read GetReadIdxCount;
    property ReadOnly: Long read GetReadOnly;
    property Reads: Long read GetReads;
    property ReadSeqCount: TStringList read GetReadSeqCount;
    property SweepInterval: Long read GetSweepInterval;
    property UpdateCount: TStringList read GetUpdateCount;
    property UserNames: TStringList read GetUserNames;
    property Version: string read GetVersion;
    property Writes: Long read GetWrites;
  published
    property Database: TMDODatabase read FDatabase write FDatabase;
  end;
  
implementation

uses
  MDOIntf;

{ TMDODatabaseInfo }

{
******************************* TMDODatabaseInfo *******************************
}
constructor TMDODatabaseInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckFBLoaded;
  FIBLoaded := True;
  FUserNames := TStringList.Create;
  FBackoutCount                        := nil;
  FDeleteCount                         := nil;
  FExpungeCount                        := nil;
  FInsertCount                         := nil;
  FPurgeCount                          := nil;
  FReadIdxCount                        := nil;
  FReadSeqCount                        := nil;
  FUpdateCount                         := nil;
end;

destructor TMDODatabaseInfo.Destroy;
begin
  if FIBLoaded then
  begin
    FUserNames.Free;
    FBackoutCount.Free;
    FDeleteCount.Free;
    FExpungeCount.Free;
    FInsertCount.Free;
    FPurgeCount.Free;
    FReadIdxCount.Free;
    FReadSeqCount.Free;
    FUpdateCount.Free;
  end;
  inherited Destroy;
end;

function TMDODatabaseInfo.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): 
        ISC_STATUS;
begin
  result := ErrCode;
  if RaiseError and (ErrCode > 0) then
    MDODatabaseError;
end;

function TMDODatabaseInfo.GetAllocation: Long;
begin
  result := GetLongDatabaseInfo(isc_info_allocation);
end;

function TMDODatabaseInfo.GetBackoutCount: TStringList;
begin
  result := GetOperationCounts(isc_info_backout_count,FBackoutCount);
end;

function TMDODatabaseInfo.GetBaseLevel: Long;
var
  local_buffer: array[0..MDOLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_base_level);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         MDOLocalBufferLength, local_buffer), True);
  result := isc_vax_integer(@local_buffer[4], 1);
end;

function TMDODatabaseInfo.GetCurrentMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_current_memory);
end;

function TMDODatabaseInfo.GetDBFileName: string;
var
  local_buffer: array[0..MDOLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         MDOLocalBufferLength, local_buffer), True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  result := String(PChar(@local_buffer[5]));
end;

function TMDODatabaseInfo.GetDBImplementationClass: Long;
var
  local_buffer: array[0..MDOLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         MDOLocalBufferLength, local_buffer), True);
  result := isc_vax_integer(@local_buffer[4], 1);
end;

function TMDODatabaseInfo.GetDBImplementationNo: Long;
var
  local_buffer: array[0..MDOLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        MDOLocalBufferLength, local_buffer), True);
  result := isc_vax_integer(@local_buffer[3], 1);
end;

function TMDODatabaseInfo.GetDBSiteName: string;
var
  local_buffer: array[0..MDOBigLocalBufferLength - 1] of Char;
  p: PChar;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        MDOLocalBufferLength, local_buffer), True);
  p := @local_buffer[5 + Int(local_buffer[4])]; { DBSiteName Length }
  p := p + Int(p^) + 1;                         { End of DBSiteName }
  p^ := #0;                                     { Null it }
  result := String(PChar(@local_buffer[6 + Int(local_buffer[4])]));
end;

function TMDODatabaseInfo.GetDBSQLDialect: Long;
var
  local_buffer: array[0..MDOLocalBufferLength - 1] of Char;
  length: Integer;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_SQL_Dialect);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                       MDOLocalBufferLength, local_buffer), True);
  if (local_buffer[0] <> Char(isc_info_db_SQL_dialect)) then
    result := 1
  else begin
    length := isc_vax_integer(@local_buffer[1], 2);
    result := isc_vax_integer(@local_buffer[3], length);
  end;
end;

function TMDODatabaseInfo.GetDeleteCount: TStringList;
begin
  result := GetOperationCounts(isc_info_delete_count,FDeleteCount);
end;

function TMDODatabaseInfo.GetExpungeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_expunge_count,FExpungeCount);
end;

function TMDODatabaseInfo.GetFetches: Long;
begin
  result := GetLongDatabaseInfo(isc_info_fetches);
end;

function TMDODatabaseInfo.GetForcedWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_forced_writes);
end;

function TMDODatabaseInfo.GetInsertCount: TStringList;
begin
  result := GetOperationCounts(isc_info_insert_count,FInsertCount);
end;

function TMDODatabaseInfo.GetLongDatabaseInfo(DatabaseInfoCommand: Integer): 
        Long;
var
  local_buffer: array[0..MDOLocalBufferLength - 1] of Char;
  length: Integer;
  _DatabaseInfoCommand: Char;
begin
  _DatabaseInfoCommand := Char(DatabaseInfoCommand);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @_DatabaseInfoCommand,
                         MDOLocalBufferLength, local_buffer), True);
  length := isc_vax_integer(@local_buffer[1], 2);
  result := isc_vax_integer(@local_buffer[3], length);
end;

function TMDODatabaseInfo.GetMarks: Long;
begin
  result := GetLongDatabaseInfo(isc_info_marks);
end;

function TMDODatabaseInfo.GetMaxMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_max_memory);
end;

function TMDODatabaseInfo.GetNoReserve: Long;
begin
  result := GetLongDatabaseInfo(isc_info_no_reserve);
end;

function TMDODatabaseInfo.GetNumBuffers: Long;
begin
  result := GetLongDatabaseInfo(isc_info_num_buffers);
end;

function TMDODatabaseInfo.GetODSMajorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_version);
end;

function TMDODatabaseInfo.GetODSMinorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_minor_version);
end;

function TMDODatabaseInfo.GetOperationCounts(DBInfoCommand: Integer; 
        FOperation: TStringList): TStringList;
var
  local_buffer: array[0..MDOHugeLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  i, qtd_tables, id_table, qtd_operations: Integer;
begin
  if FOperation = nil then FOperation := TStringList.Create;
  result := FOperation;
  DatabaseInfoCommand := Char(DBInfoCommand);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         MDOHugeLocalBufferLength, local_buffer), True);
  FOperation.Clear;
  { 1. 1 byte specifying the item type requested (e.g., isc_info_insert_count).
    2. 2 bytes telling how many bytes compose the subsequent value pairs.
    3. A pair of values for each table in the database on wich the requested
      type of operation has occurred since the database was last attached.
    Each pair consists of:
    1. 2 bytes specifying the table ID.
    2. 4 bytes listing the number of operations (e.g., inserts) done on that table.
  }
  qtd_tables := trunc(isc_vax_integer(@local_buffer[1],2)/6);
  for i := 0 to qtd_tables - 1 do
  begin
    id_table := isc_vax_integer(@local_buffer[3+(i*6)],2);
    qtd_operations := isc_vax_integer(@local_buffer[5+(i*6)],4);
    FOperation.Add(IntToStr(id_table)+'='+IntToStr(qtd_operations));
  end;
end;

function TMDODatabaseInfo.GetPageSize: Long;
begin
  result := GetLongDatabaseInfo(isc_info_page_size);
end;

function TMDODatabaseInfo.GetPurgeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_purge_count,FPurgeCount);
end;

function TMDODatabaseInfo.GetReadIdxCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_idx_count,FReadIdxCount);
end;

function TMDODatabaseInfo.GetReadOnly: Long;
begin
  result := GetLongDatabaseInfo(isc_info_db_read_only);
end;

function TMDODatabaseInfo.GetReads: Long;
begin
  result := GetLongDatabaseInfo(isc_info_reads);
end;

function TMDODatabaseInfo.GetReadSeqCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_seq_count,FReadSeqCount);
end;

function TMDODatabaseInfo.GetStringDatabaseInfo(DatabaseInfoCommand: Integer): 
        string;
var
  local_buffer: array[0..MDOBigLocalBufferLength - 1] of Char;
  _DatabaseInfoCommand: Char;
begin
  _DatabaseInfoCommand := Char(DatabaseInfoCommand);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @_DatabaseInfoCommand,
                         MDOBigLocalBufferLength, local_buffer), True);
  local_buffer[4 + Int(local_buffer[3])] := #0;
  result := String(PChar(@local_buffer[4]));
end;

function TMDODatabaseInfo.GetSweepInterval: Long;
begin
  result := GetLongDatabaseInfo(isc_info_sweep_interval);
end;

function TMDODatabaseInfo.GetUpdateCount: TStringList;
begin
  result := GetOperationCounts(isc_info_update_count,FUpdateCount);
end;

function TMDODatabaseInfo.GetUserNames: TStringList;
var
  local_buffer: array[0..MDOHugeLocalBufferLength - 1] of Char;
  temp_buffer: array[0..MDOLocalBufferLength - 2] of Char;
  DatabaseInfoCommand: Char;
  i, user_length: Integer;
begin
  result := FUserNames;
  DatabaseInfoCommand := Char(isc_info_user_names);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        MDOHugeLocalBufferLength, local_buffer), True);
  FUserNames.Clear;
  i := 0;
  while local_buffer[i] = Char(isc_info_user_names) do
  begin
    Inc(i, 3); { skip "isc_info_user_names byte" & two unknown bytes of structure (see below) }
    user_length := Long(local_buffer[i]);
    Inc(i,1);
    Move(local_buffer[i], temp_buffer[0], user_length);
    Inc(i, user_length);
    temp_buffer[user_length] := #0;
    FUserNames.Add(String(temp_buffer));
  end;
end;

function TMDODatabaseInfo.GetVersion: string;
var
  local_buffer: array[0..MDOBigLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_version);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        MDOBigLocalBufferLength, local_buffer), True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  result := String(PChar(@local_buffer[5]));
end;

function TMDODatabaseInfo.GetWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_writes);
end;




end.
