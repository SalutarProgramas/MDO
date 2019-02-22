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

unit MDOQuery;

{$I ..\MDO.INC}

interface

uses Windows, SysUtils, Graphics, Classes, Controls, Db, StdVCL,
     MDOHeader, MDO, MDOCustomDataSet, MDOSQL;

type

{ TMDOQuery }

  TMDOQuery = class (TMDOCustomDataSet)
  private
    FCheckRowsAffected: Boolean;
    FGenerateParamNames: Boolean;
    FParams: TParams;
    FPrepared: Boolean;
    FRowsAffected: Integer;
    FSQL: TStrings;
    FText: string;
    FFilteredCount: Integer;
    function GetRowsAffected: Integer;
    function GetStmtHandle: TISC_STMT_HANDLE;
    procedure PrepareSQL(Value: PChar);
    procedure QueryChanged(Sender: TObject);
    procedure ReadParamData(Reader: TReader);
    procedure SetParams;
    procedure SetParamsFromCursor;
    procedure SetParamsList(Value: TParams);
    procedure SetPrepare(Value: Boolean);
    procedure SetPrepared(Value: Boolean);
    procedure SetQuery(Value: TStrings);
    procedure WriteParamData(Writer: TWriter);
  protected
    function GetRecordCount: Integer; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Disconnect; override;
    function GenerateQueryForLiveUpdate: Boolean;
    function GetParamsCount: Word;
    procedure InitFieldDefs; override;
    procedure InternalOpen; override;
    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    function PSGetTableName: string; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure InternalRefreshRow; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BatchInput(InputObject: TMDOBatchInput);
    procedure BatchOutput(OutputObject: TMDOBatchOutput);
    procedure ExecSQL;
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure UnPrepare;
    property GenerateParamNames: Boolean read FGenerateParamNames write 
            FGenerateParamNames;
    property ParamCount: Word read GetParamsCount;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property RowsAffected: Integer read GetRowsAffected;
    property StatementType;
    property StmtHandle: TISC_STMT_HANDLE read GetStmtHandle;
    property Text: string read FText;
  published
    property Active;
    property AfterDatabaseDisconnect;
    property AfterTransactionEnd;
    property BeforeDatabaseDisconnect;
    property BeforeTransactionEnd;
    property BufferChunks;
    property CachedUpdates;
    property Constraints stored ConstraintsStored;
    property DatabaseFree;
    property DataSource read GetDataSource write SetDataSource;
    property Filtered;
    property GeneratorLink;
    property LoadDefaults;
    property OnFilterRecord;
    property ParamCheck;
    property Params: TParams read FParams write SetParamsList stored False;
    property SQL: TStrings read FSQL write SetQuery;
    property TransactionFree;
    property UniDirectional default False;
    property UpdateObject;
  end;
  
implementation

{ TMDOQuery }

{
********************************** TMDOQuery ***********************************
}
constructor TMDOQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams := TParams.Create(Self);
  ParamCheck := True;
  FGenerateParamNames := False;
  FRowsAffected := -1;
  {FDefaultDataSource := TDataSource.Create(Self);
  FDefaultDataSource.DataSet := Self;
  FDefaultDataLink := TDataLink.Create;
  FDefaultDataLink.DataSource := FDefaultDataSource;}
end;

destructor TMDOQuery.Destroy;
begin
  Destroying;
  Disconnect;
  SQL.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TMDOQuery.BatchInput(InputObject: TMDOBatchInput);
begin
  InternalBatchInput(InputObject);
end;

procedure TMDOQuery.BatchOutput(OutputObject: TMDOBatchOutput);
begin
  InternalBatchOutput(OutputObject);
end;

procedure TMDOQuery.DefineProperties(Filer: TFiler);
  
  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TMDOQuery(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;
  
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;

procedure TMDOQuery.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TMDOQuery.ExecSQL;
var
  DidActivate: Boolean;
begin
  CheckInActive;
  if SQL.Count <= 0 then
  begin
    FCheckRowsAffected := False;
    MDOError(mdoeEmptySQLStatement, [nil]);
  end;
  ActivateConnection();
  DidActivate := ActivateTransaction;
  try
    SetPrepared(True);
    if DataSource <> nil then SetParamsFromCursor;
    if FParams.Count > 0 then SetParams;
    InternalExecQuery;
  finally
    if DidActivate then
      DeactivateTransaction;
    FCheckRowsAffected := True;
  end;
end;

function TMDOQuery.GenerateQueryForLiveUpdate: Boolean;
begin
  Result := False;
end;

procedure TMDOQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);
  
    function AddFieldToList(const FieldName: string; DataSet: TDataSet;
      List: TList): Boolean;
    var
      Field: TField;
    begin
      Field := DataSet.FindField(FieldName);
      if (Field <> nil) then
        List.Add(Field);
      Result := Field <> nil;
    end;
  
  var
    i: Integer;
  
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields);
end;

function TMDOQuery.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

function TMDOQuery.GetRowsAffected: Integer;
begin
  Result := -1;
  if Prepared then
   Result := QSelect.RowsAffected
end;

function TMDOQuery.GetStmtHandle: TISC_STMT_HANDLE;
begin
  Result := SelectStmtHandle;
end;

procedure TMDOQuery.InitFieldDefs;
begin
  inherited InitFieldDefs;
end;

procedure TMDOQuery.InternalOpen;
begin
  // BufferChunks := 10000;
  ActivateConnection();
  ActivateTransaction;
  QSelect.GenerateParamNames := FGenerateParamNames;
  SetPrepared(True);
  if DataSource <> nil then
    SetParamsFromCursor;
  SetParams;
  inherited InternalOpen;
end;

function TMDOQuery.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TMDOQuery.Prepare;
begin
  SetPrepared(True);
end;

procedure TMDOQuery.PrepareSQL(Value: PChar);
begin
  QSelect.GenerateParamNames := FGenerateParamNames;
  InternalPrepare;
end;

procedure TMDOQuery.PSExecute;
begin
  ExecSQL;
end;

function TMDOQuery.PSGetParams: TParams;
begin
  Result := Params;
end;

function TMDOQuery.PSGetTableName: string;
begin
  Result := inherited PSGetTableName;
end;

procedure TMDOQuery.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SQL.Text := CommandText;
end;

procedure TMDOQuery.PSSetParams(AParams: TParams);
begin
  if AParams.Count <> 0 then
    Params.Assign(AParams);
  Close;
end;

procedure TMDOQuery.QueryChanged(Sender: TObject);
var
  List: TParams;
begin
  if not (csReading in ComponentState) then
  begin
    Disconnect;
    if ParamCheck or (csDesigning in ComponentState) then
    begin
      List := TParams.Create(Self);
      try
        FText := List.ParseSQL(SQL.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end else
      FText := SQL.Text;
    DataEvent(dePropertyChange, 0);
  end else
    FText := FParams.ParseSQL(SQL.Text, False);
  SelectSQL.Assign(SQL);
  SimpleSelectSQL.Assign(SQL);
end;

procedure TMDOQuery.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TMDOQuery.SetFiltered(Value: Boolean);
begin
  if not Value then FFilteredCount := 0;
  if(Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
    begin
      Close;
      Open;
    end;
    if Value and Assigned(OnFilterRecord) then
    begin
      InternalFirst;
      while GetNextRecord do
      begin
        if FFilteredCount = 0 then
          FFilteredCount := 1;
        Inc(FFilteredCount);
      end;
    end;
  end
  else
    inherited SetFiltered(value);
end;

procedure TMDOQuery.SetParams;
var
  i: Integer;
  Buffer: Pointer;
begin
  for I := 0 to FParams.Count - 1 do
  begin
    if Params[i].IsNull then
      SQLParams[i].IsNull := True
    else begin
      SQLParams[i].IsNull := False;
      case Params[i].DataType of
        ftBytes:
        begin
          GetMem(Buffer,Params[i].GetDataSize);
          try
            Params[i].GetData(Buffer);
            SQLParams[i].AsPointer := Buffer;
          finally
            FreeMem(Buffer);
          end;
        end;
        ftString:
          SQLParams[i].AsString := Params[i].AsString;
        ftSmallint, ftWord:
          SQLParams[i].AsShort := Params[i].AsSmallInt;
        ftBoolean:
            SQLParams[i].AsBoolean := Params[i].AsBoolean;
        ftInteger:
          SQLParams[i].AsLong := Params[i].AsInteger;
        ftLargeInt:
        {$IFDEF MDO_DELPHI5}
          SQLParams[i].AsInt64 := StrToInt64(Params[i].Value);
        {$ENDIF}
        {$IFDEF MDO_DELPHI6_UP}
          SQLParams[i].AsInt64 := Params[i].Value;
        {$ENDIF}
        ftFloat:
         SQLParams[i].AsDouble := Params[i].AsFloat;
        ftBCD, ftCurrency:
          SQLParams[i].AsCurrency := Params[i].AsCurrency;
        ftDate:
          SQLParams[i].AsDate := Params[i].AsDateTime;
        ftTime:
          SQLParams[i].AsTime := Params[i].AsDateTime;
        ftDateTime:
          SQLParams[i].AsDateTime := Params[i].AsDateTime;
        ftBlob, ftMemo:
          SQLParams[i].AsString := Params[i].AsString;
        else
          MDOError(mdoeNotSupported, [nil]);
      end;
    end;
  end;
end;

procedure TMDOQuery.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
  
  procedure CheckRequiredParams;
  var
    I: Integer;
  begin
    for I := 0 to FParams.Count - 1 do
    with FParams[I] do
      if not Bound then
        MDOError(mdoeRequiredParamNotSet, [nil]);
  end;
  
begin
  if DataSource <> nil then
  begin
    DataSet := DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        with FParams[I] do
          if not Bound then
          begin
            AssignField(DataSet.FieldByName(Name));
            Bound := False;
          end;
    end
    else
      CheckRequiredParams;
  end
  else
    CheckRequiredParams;
end;

procedure TMDOQuery.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

procedure TMDOQuery.SetPrepare(Value: Boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TMDOQuery.SetPrepared(Value: Boolean);
begin
  CheckDatasetClosed;
  if Value <> Prepared then
  begin
    if Value then
    begin
      FRowsAffected := -1;
      FCheckRowsAffected := True;
      if Length(Text) > 1 then PrepareSQL(PChar(Text))
      else MDOError(mdoeEmptySQLStatement, [nil]);
    end
    else
    begin
      if FCheckRowsAffected then
        FRowsAffected := RowsAffected;
      InternalUnPrepare;
    end;
    FPrepared := Value;
  end;
end;

procedure TMDOQuery.SetQuery(Value: TStrings);
begin
  if SQL.Text <> Value.Text then
  begin
    Disconnect;
    SQL.BeginUpdate;
    try
      SQL.Assign(Value);
    finally
      SQL.EndUpdate;
    end;
  end;
end;

procedure TMDOQuery.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TMDOQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{ TMDOQuery IProviderSupport }

{procedure TMDOQuery.SetDataLinkBufferCount(Value: Integer);
begin
  if Assigned(FDefaultDataLink) then
    FDefaultDataLink.BufferCount := Value;
  inherited SetDataLinkBufferCount(Value);
end;}

function TMDOQuery.GetRecordCount: Integer;
begin
  if Filtered and Assigned(OnFilterRecord) then
    result := FFilteredCount
  else
    result := Inherited GetRecordCount;
end;

procedure TMDOQuery.InternalRefreshRow;
begin
  Close;
  Open;
end;

end.

