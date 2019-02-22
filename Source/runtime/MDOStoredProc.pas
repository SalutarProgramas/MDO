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

unit MDOStoredProc;

interface

uses
  SysUtils, Classes, DB, MDO, MDODatabase, MDOCustomDataSet, MDOHeader,
  MDOSQL, MDOUtils;

{ TMDOStoredProc }
type

  TMDOStoredProc = class (TMDOCustomDataSet)
  private
    FIBLoaded: Boolean;
    FNameList: TStrings;
    FParams: TParams;
    FPrepared: Boolean;
    FProcName: string;
    FStmtHandle: TISC_STMT_HANDLE;
    procedure CreateParamDesc;
    procedure FetchDataIntoOutputParams;
    procedure FreeStatement;
    procedure GenerateSQL;
    function GetStoredProcedureNames: TStrings;
    procedure GetStoredProcedureNamesFromServer;
    procedure ReadParamData(Reader: TReader);
    procedure SetParams;
    procedure SetParamsFromCursor;
    procedure SetParamsList(Value: TParams);
    procedure WriteParamData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Disconnect; override;
    function GetParamsCount: Word;
    procedure InternalOpen; override;
    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    function PSGetTableName: string; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetPrepare(Value: Boolean);
    procedure SetPrepared(Value: Boolean);
    procedure SetProcName(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyParams(Value: TParams);
    procedure ExecProc;
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure UnPrepare;
    property ParamCount: Word read GetParamsCount;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property StmtHandle: TISC_STMT_HANDLE read FStmtHandle;
    property StoredProcedureNames: TStrings read GetStoredProcedureNames;
  published
    property AfterDatabaseDisconnect;
    property AfterTransactionEnd;
    property BeforeDatabaseDisconnect;
    property BeforeTransactionEnd;
    property DatabaseFree;
    property Filtered;
    property OnFilterRecord;
    property Params: TParams read FParams write SetParamsList;
    property StoredProcName: string read FProcName write SetProcName;
    property TransactionFree;
  end;
  
implementation

uses
  MDOIntf;

{ TMDOStoredProc }

{
******************************** TMDOStoredProc ********************************
}
constructor TMDOStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckFBLoaded;
  FIBLoaded := True;
  FParams := TParams.Create (self);
  FNameList := TStringList.Create;
end;

destructor TMDOStoredProc.Destroy;
begin
  if FIBLoaded then
  begin
    Destroying;
    Disconnect;
    FParams.Free;
    FNameList.Destroy;
  end;
  inherited Destroy;
end;

procedure TMDOStoredProc.CopyParams(Value: TParams);
begin
  if not Prepared and (FParams.Count = 0) then
  try
    Prepare;
    Value.Assign(FParams);
  finally
    UnPrepare;
  end else
    Value.Assign(FParams);
end;

procedure TMDOStoredProc.CreateParamDesc;
var
  i: Integer;
  DataType: TFieldType;
begin
  DataType := ftUnknown;
  for i := 0 to QSelect.Current.Count - 1 do begin
  case QSelect.Fields[i].SQLtype of
    SQL_TYPE_DATE: DataType := ftDate;
    SQL_TYPE_TIME: DataType := ftTime;
    SQL_TIMESTAMP: DataType := ftDateTime;
    SQL_SHORT:
      if ((QSelect.Fields[i].AsXSQLVar)^.sqlscale = 0) then
        DataType := ftSmallInt
      else
        DataType := ftBCD;
    SQL_LONG:
      if ((QSelect.Fields[i].AsXSQLVar)^.sqlscale = 0) then
        DataType := ftInteger
      else if ((QSelect.Fields[i].AsXSQLVar)^.sqlscale >= (-4)) then
        DataType := ftBCD
      else
        DataType := ftFloat;
    SQL_INT64:
      if ((QSelect.Fields[i].AsXSQLVar)^.sqlscale = 0) then
        DataType := ftLargeInt
      else if ((QSelect.Fields[i].AsXSQLVar)^.sqlscale >= (-4)) then
        DataType := ftBCD
      else
        DataType := ftFloat;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: DataType := ftFloat;
    SQL_TEXT: DataType := ftString;
    SQL_VARYING:
      if ((QSelect.Fields[i].AsXSQLVar)^.sqllen < 1024) then
        DataType := ftString
      else DataType := ftBlob;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD: DataType := ftBlob;
    end;
    FParams.CreateParam(DataType, Trim(QSelect.Fields[i].Name), ptOutput);
  end;
  
  DataType := ftUnknown;
  for i := 0 to QSelect.Params.Count - 1 do begin
  case QSelect.Params[i].SQLtype of
    SQL_TYPE_DATE: DataType := ftDate;
    SQL_TYPE_TIME: DataType := ftTime;
    SQL_TIMESTAMP: DataType := ftDateTime;
    SQL_SHORT:
      if ((QSelect.Params[i].AsXSQLVar)^.sqlscale = 0) then
        DataType := ftSmallInt
      else
        DataType := ftBCD;
    SQL_LONG:
      if ((QSelect.Params[i].AsXSQLVar)^.sqlscale = 0) then
        DataType := ftInteger
      else if ((QSelect.Params[i].AsXSQLVar)^.sqlscale >= (-4)) then
        DataType := ftBCD
      else DataType := ftFloat;
    SQL_INT64:
      if ((QSelect.Params[i].AsXSQLVar)^.sqlscale = 0) then
        DataType := ftLargeInt
      else if ((QSelect.Params[i].AsXSQLVar)^.sqlscale >= (-4)) then
        DataType := ftBCD
      else DataType := ftFloat;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: DataType := ftFloat;
    SQL_TEXT: DataType := ftString;
    SQL_VARYING:
      if ((QSelect.Params[i].AsXSQLVar)^.sqllen < 1024) then
        DataType := ftString
      else DataType := ftBlob;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD: DataType := ftBlob;
    end;
    FParams.CreateParam(DataType, Trim(QSelect.Params[i].Name), ptInput);
  end;
end;

procedure TMDOStoredProc.DefineProperties(Filer: TFiler);
  
  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TMDOStoredProc(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;
  
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;

procedure TMDOStoredProc.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TMDOStoredProc.ExecProc;
var
  DidActivate: Boolean;
begin
  CheckInActive;
  if StoredProcName = '' then
    MDOError(mdoeNoStoredProcName, [nil]);
  ActivateConnection;
  DidActivate := ActivateTransaction;
  try
    SetPrepared(True);
    if DataSource <> nil then SetParamsFromCursor;
    if FParams.Count > 0 then SetParams;
    InternalExecQuery;
    FetchDataIntoOutputParams;
  finally
    if DidActivate then
      DeactivateTransaction;
  end;
end;

procedure TMDOStoredProc.FetchDataIntoOutputParams;
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to FParams.Count - 1 do
    with Params[I] do
      if ParamType = ptOutput then begin
         Value := QSelect.Fields[j].Value;
         Inc(j);
      end;
end;

procedure TMDOStoredProc.FreeStatement;
begin
  InternalUnPrepare;
  FPrepared := False;
end;

procedure TMDOStoredProc.GenerateSQL;
var
  Query: TMDOSQL;
  input: string;
begin
  ActivateConnection;
  Database.InternalTransaction.StartTransaction;
  Query := TMDOSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    Query.SQL.Text := 'SELECT RDB$PARAMETER_NAME,  RDB$PARAMETER_TYPE ' + {do not localize}
                       'FROM RDB$PROCEDURE_PARAMETERS ' + {do not localize}
                       'WHERE RDB$PROCEDURE_NAME = ' + {do not localize}
                       '''' + FormatIdentifierValue(Database.SQLDialect, FProcName) + '''' +
                       ' ORDER BY RDB$PARAMETER_NUMBER'; {do not localize}
    Query.Prepare;
    Query.GoToFirstRecordOnExecute := False;
    Query.ExecQuery;
    while (not Query.EOF) and (Query.Next <> nil) do begin
      if (Query.Current.ByName('RDB$PARAMETER_TYPE').AsInteger = 0) then begin {do not localize}
        if (input <> '') then
          input := input + ', :' +
            FormatIdentifier(Database.SQLDialect, Query.Current.ByName('RDB$PARAMETER_NAME').AsString) else {do not localize}
          input := ':' +
            FormatIdentifier(Database.SQLDialect, Query.Current.ByName('RDB$PARAMETER_NAME').AsString); {do not localize}
      end
    end;
    SelectSQL.Text := 'Execute Procedure ' + {do not localize}
                FormatIdentifier(Database.SQLDialect, FProcName) + ' ' + input;
  finally
    Query.Free;
    Database.InternalTransaction.Commit;
  end;
end;

function TMDOStoredProc.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

function TMDOStoredProc.GetStoredProcedureNames: TStrings;
begin
  FNameList.clear;
  GetStoredProcedureNamesFromServer;
  Result := FNameList;
end;

procedure TMDOStoredProc.GetStoredProcedureNamesFromServer;
var
  Query: TMDOSQL;
begin
  if not (csReading in ComponentState) then begin
    ActivateConnection;
    Database.InternalTransaction.StartTransaction;
    Query := TMDOSQL.Create(self);
    try
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := DataBase;
      Query.Transaction := Database.InternalTransaction;
      Query.SQL.Text := 'Select RDB$PROCEDURE_NAME from RDB$PROCEDURES'; {do not localize}
      Query.Prepare;
      Query.ExecQuery;
      while (not Query.EOF) and (Query.Next <> nil) do
        FNameList.Add(TrimRight(Query.Current.ByName('RDB$PROCEDURE_NAME').AsString)); {do not localize}
    finally
      Query.Free;
      Database.InternalTransaction.Commit;
    end;
  end;
end;

procedure TMDOStoredProc.InternalOpen;
begin
  MDOError(mdoeIsAExecuteProcedure,[nil]);
end;

function TMDOStoredProc.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TMDOStoredProc.Prepare;
begin
  SetPrepared(True);
end;

procedure TMDOStoredProc.PSExecute;
begin
  ExecProc;
end;

function TMDOStoredProc.PSGetParams: TParams;
begin
  Result := Params;
end;

function TMDOStoredProc.PSGetTableName: string;
begin
  { ! }
end;

procedure TMDOStoredProc.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    StoredProcName := CommandText;
end;

procedure TMDOStoredProc.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
  Close;
end;

procedure TMDOStoredProc.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

procedure TMDOStoredProc.SetFiltered(Value: Boolean);
begin
  if(Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(value);
end;

procedure TMDOStoredProc.SetParams;
var
  i: Integer;
  j: Integer;
begin
  i := 0;
  for j := 0 to FParams.Count - 1 do
  begin
    if (Params[j].ParamType <> ptInput) then
      continue;
    if not Params[j].Bound then
      MDOError(mdoeRequiredParamNotSet, [nil]);
    if Params[j].IsNull then
      SQLParams[i].IsNull := True
    else begin
      SQLParams[i].IsNull := False;
      case Params[j].DataType of
        ftString:
          SQLParams[i].AsString := Params[j].AsString;
        ftSmallint, ftWord:
          SQLParams[i].AsShort := Params[j].AsSmallInt;
        ftBoolean :
          SQLParams[i].AsBoolean := Params[j].AsBoolean;
        ftInteger:
          SQLParams[i].AsLong := Params[j].AsInteger;
  {        ftLargeInt:
            SQLParams[i].AsInt64 := Params[j].AsLargeInt; }
        ftFloat, ftCurrency:
         SQLParams[i].AsDouble := Params[j].AsFloat;
        ftBCD:
          SQLParams[i].AsCurrency := Params[j].AsCurrency;
        ftDate:
          SQLParams[i].AsDate := Params[j].AsDateTime;
        ftTime:
          SQLParams[i].AsTime := Params[j].AsDateTime;
        ftDateTime:
          SQLParams[i].AsDateTime := Params[j].AsDateTime;
        ftBlob, ftMemo:
          SQLParams[i].AsString := Params[j].AsString;
        else
          MDOError(mdoeNotSupported, [nil]);
      end;
    end;
    Inc(i);
  end;
end;

procedure TMDOStoredProc.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if DataSource <> nil then
  begin
    DataSet := DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        with FParams[I] do
          if (not Bound) and
            ((ParamType = ptInput) or (ParamType =  ptInputOutput)) then
            AssignField(DataSet.FieldByName(Name));
    end;
  end;
end;

procedure TMDOStoredProc.SetParamsList(Value: TParams);
begin
  CheckInactive;
  if Prepared then
  begin
    SetPrepared(False);
    FParams.Assign(Value);
    SetPrepared(True);
  end else
    FParams.Assign(Value);
end;

procedure TMDOStoredProc.SetPrepare(Value: Boolean);
begin
  if Value then Prepare
  else UnPrepare;
end;

procedure TMDOStoredProc.SetPrepared(Value: Boolean);
begin
  if Prepared <> Value then
  begin
    if Value then
      try
        if SelectSQL.Text = '' then GenerateSQL;
        InternalPrepare;
        if FParams.Count = 0 then CreateParamDesc;
        FPrepared := True;
      except
        FreeStatement;
        raise;
      end
    else FreeStatement;
  end;
  
end;

procedure TMDOStoredProc.SetProcName(Value: string);
begin
  if not (csReading in ComponentState) then
  begin
    CheckInactive;
    if Value <> FProcName then
    begin
      FProcName := Value;
      FreeStatement;
      FParams.Clear;
      if (Value <> '') and
        (Database <> nil) then
        GenerateSQL;
    end;
  end else begin
    FProcName := Value;
  if (Value <> '') and
    (Database <> nil) then
    GenerateSQL;
  end;
end;

procedure TMDOStoredProc.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TMDOStoredProc.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{ TMDOStoredProc IProviderSupport }

end.
