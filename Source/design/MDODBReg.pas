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

unit MDODBReg;

{ Compiler defines }
{$A+} {Aligned records: On }
{$B-} {Short circuit boolean expressions: Off }
{$G+} {Imported data: On }
{$H+} {Huge Strings: On }
{$J-} {Modification of Typed Constants: Off }
{$M+} {Generate run-time type information: On }
{$O+} {Optimization: On }
{$Q-} {Overflow checks: Off }
{$R-} {Range checks: Off }
{$T+} {Typed address: On }
{$U+} {Pentim-safe FDIVs: On }
{$W-} {Always generate stack frames: Off }
{$X+} {Extended syntax: On }
{$Z1} {Minimum Enumeration Size: 1 Byte }
{$I MDO.INC}

interface

uses Windows, SysUtils, Classes, Graphics, Dialogs, Controls, Forms, TypInfo,
  DB, DBReg, SQLEdit, ColnEdit, FldLinks, MDO, MDOTable, MDODatabase,
  MDOUpdateSQLEditor, MDOEventsEditor, MDOConst, MDOMisc, MDOScript
{$IFDEF MDO_DELPHI5}
  , DsgnIntf
{$ENDIF}
{$IFDEF MDO_DELPHI6_UP}
  , DesignEditors, DesignIntf
{$ENDIF};

type

  { TMDOFileNameProperty
    Property editor the DataBase Name property.  Brings up the Open dialog }

  TMDOFileNameProperty = class (TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;
  
  { TMDONameProperty
  }
  TMDONameProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;
  
  { TMDOStoredProcNameProperty
    Editor for the TMDOStoredProc.StoredProcName property.  Displays a drop-down list of all
    the StoredProcedures in the Database.}
  TMDOStoredProcNameProperty = class (TMDONameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  
  { TMDOTableNameProperty
    Editor for the TMDOTable.TableName property.  Displays a drop-down list of all
    the Tables in the Database.}
  TMDOTableNameProperty = class (TMDONameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  
  TDBStringProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  
  TMDOIndexFieldNamesProperty = class (TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;
  
  TMDOIndexNameProperty = class (TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;
  
  { TMDODatabaseEditor }

  TMDODatabaseEditor = class (TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
  { TMDOTransactionEditor }

  TMDOTransactionEditor = class (TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
  { TMDOQueryEditor }

  TMDOQueryEditor = class (TDataSetEditor)
  protected
    FGetFieldnamesProc: TGetFieldNamesProc;
    FGetTableNamesProc: TGetTableNamesProc;
  public
    procedure EditSQL;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
  { TMDOStoredProcEditor }

  TMDOStoredProcEditor = class (TDataSetEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
  { TMDODataSetEditor }

  TMDODataSetEditor = class (TDataSetEditor)
  protected
    FGetFieldnamesProc: TGetFieldNamesProc;
    FGetTableNamesProc: TGetTableNamesProc;
  public
    procedure EditSQL;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
  { TMDOUpdateSQLEditor }

  TMDOUpdateSQLEditor = class (TComponentEditor)
  public
    GetFieldNames: TGetFieldNamesProc;
    GetTableNames: TGetTableNamesProc;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
  TMDOStoredProcParamsProperty = class (TCollectionProperty)
  public
    procedure Edit; override;
  end;
  
  TMDOTableFieldLinkProperty = class (TFieldLinkProperty)
  private
    FTable: TMDOTable;
  protected
    function GetIndexFieldNames: string; override;
    function GetMasterFields: string; override;
    procedure SetIndexFieldNames(const Value: string); override;
    procedure SetMasterFields(const Value: string); override;
  public
    procedure Edit; override;
  end;
  
  { TSQLPropertyEditor }

  TSQLPropertyEditor = class (TClassProperty)
  protected
    FGetFieldnamesProc: TGetFieldNamesProc;
    FGetTableNamesProc: TGetTableNamesProc;
  public
    procedure EditSQL;
    function GetAttributes: TPropertyAttributes; override;
  end;
  
  { TMDOQuerySQLProperty }

  TMDOQuerySQLProperty = class (TSQLPropertyEditor)
  public
    procedure Edit; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;
  
  { TMDODatasetSQLProperty }

  TMDODatasetSQLProperty = class (TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;
  
  { TMDOSQLProperty }

  TMDOSQLProperty = class (TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;
  
  TMDOEventListProperty = class (TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;
  
  TMDOServiceEditor = class (TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
procedure Register;

implementation

uses MDOQuery, MDOStoredProc, MDOUpdateSQL, MDOCustomDataSet,
  MDOIntf, MDOSQL, MDOSQLMonitor, MDODatabaseInfo, MDOEvents,
  MDOServices, MDOInstall, MDODatabaseEdit, MDOTransactionEdit,
  MDOBatchMove, DBLogDlg, MDOExtract, MDOServiceEditor, MDOCSVDataExport,
  MDOHTMLDataExport;

procedure Register;

begin
  RegisterComponents(MDOPalette1,
    [TMDOTable,
    TMDOQuery,
      TMDOStoredProc,
      TMDODatabase,
      TMDOTransaction,
      TMDOUpdateSQL,
      TMDODataSet,
      TMDOSQL,
      TMDODatabaseInfo,
      TMDOSQLMonitor,
      TMDOEvents,
      TMDOScript
      ]);

  if (TryFBLoad) and (GetFBClientVersion >= 6) then
  begin
    RegisterComponents(MDOPalette2,
      [TMDOConfigService,
      TMDOBackupService,
        TMDORestoreService,
        TMDOValidationService,
        TMDOStatisticalService,
        TMDOLogService,
        TMDOSecurityService,
        TMDOServerProperties,
        TMDOInstall,
        TMDOUninstall
        ]);
  end;

  RegisterComponents(MDOPalette3,
    [TMDOMisc,
     TMDOExtract,
     TMDOCSVDataExport,
     TMDOHTMLDataExport
    ]);

  RegisterClasses([TMDOStringField, TMDOBCDField]);
  RegisterFields([TMDOStringField, TMDOBCDField]);
  RegisterPropertyEditor(TypeInfo(TMDOFileName), TMDODatabase, 'DatabaseName',
    TMDOFileNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(string), TMDOStoredProc, 'StoredProcName',
    TMDOStoredProcNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TParams), TMDOStoredProc, 'Params',
    TMDOStoredProcParamsProperty);
  RegisterPropertyEditor(TypeInfo(string), TMDOTable, 'TableName',
    TMDOTableNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(string), TMDOTable, 'IndexName',
    TMDOIndexNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(string), TMDOTable, 'IndexFieldNames',
    TMDOIndexFieldNamesProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(string), TMDOTable, 'MasterFields',
    TMDOTableFieldLinkProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TMDOQuery, 'SQL',
    TMDOQuerySQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TMDODataSet, 'SelectSQL',
    TMDODatasetSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TMDOSQL, 'SQL', TMDOSQLProperty);
    {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TMDOEvents, 'Events',
    TMDOEventListProperty); {do not localize}

  RegisterComponentEditor(TMDODatabase, TMDODatabaseEditor);
  RegisterComponentEditor(TMDOCustomService, TMDOServiceEditor);
  RegisterComponentEditor(TMDOTransaction, TMDOTransactionEditor);
  RegisterComponentEditor(TMDOUpdateSQL, TMDOUpdateSQLEditor);
  RegisterComponentEditor(TMDODataSet, TMDODataSetEditor);
  RegisterComponentEditor(TMDOQuery, TMDOQueryEditor);
  RegisterComponentEditor(TMDOStoredProc, TMDOStoredProcEditor);

end;

{ TMDOFileNameProperty }

{
***************************** TMDOFileNameProperty *****************************
}
procedure TMDOFileNameProperty.Edit;
begin
  with TOpenDialog.Create(Application) do
  try
    InitialDir := ExtractFilePath(GetStrValue);
    Filter := SDatabaseFilter;
    if Execute then
      SetStrValue(FileName);
  finally
    Free
  end;
end;

function TMDOFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TMDONameProperty }

{
******************************* TMDONameProperty *******************************
}
function TMDONameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

{ TMDOStoredProcNameProperty }

{
************************** TMDOStoredProcNameProperty **************************
}
procedure TMDOStoredProcNameProperty.GetValues(Proc: TGetStrProc);
var
  StoredProc: TMDOStoredProc;
  i: Integer;
begin
  StoredProc := GetComponent(0) as TMDOStoredProc;
  with StoredProc do
    for I := 0 to StoredProcedureNames.Count - 1 do
      Proc(StoredProcedureNames[i]);
end;

{ TMDOTableNameProperty }

{
**************************** TMDOTableNameProperty *****************************
}
procedure TMDOTableNameProperty.GetValues(Proc: TGetStrProc);
var
  TableName: TMDOTable;
  i: Integer;
begin
  TableName := GetComponent(0) as TMDOTable;
  with TableName do
    for I := 0 to TableNames.Count - 1 do
      Proc(TableNames[i]);
end;

{ TDBStringProperty }

{
****************************** TDBStringProperty *******************************
}
function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ Utility Functions }

function GetPropertyValue(Instance: TPersistent; const PropName: string):
  TPersistent;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
    Result := TObject(GetOrdProp(Instance, PropInfo)) as TPersistent;
end;

function GetIndexDefs(Component: TPersistent): TIndexDefs;
var
  DataSet: TDataSet;
begin
  DataSet := Component as TDataSet;
  Result := GetPropertyValue(DataSet, 'IndexDefs') as TIndexDefs;
    {do not localize}
  if Assigned(Result) then
  begin
    Result.Updated := False;
    Result.Update;
  end;
end;

{ TMDOIndexFieldNamesProperty }

{
************************* TMDOIndexFieldNamesProperty **************************
}
procedure TMDOIndexFieldNamesProperty.GetValueList(List: TStrings);
var
  I: Integer;
  IndexDefs: TIndexDefs;
begin
  IndexDefs := GetIndexDefs(GetComponent(0));
  for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      if (Options * [ixExpression, ixDescending] = []) and (Fields <> '') then
        List.Add(Fields);
end;

{ TMDOIndexNameProperty }

{
**************************** TMDOIndexNameProperty *****************************
}
procedure TMDOIndexNameProperty.GetValueList(List: TStrings);
begin
  GetIndexDefs(GetComponent(0)).GetItemNames(List);
end;

{ TSQLPropertyEditor }

{
****************************** TSQLPropertyEditor ******************************
}
procedure TSQLPropertyEditor.EditSQL;
var
  SQLText: string;
  SQL: TStrings;
begin
  SQL := TStringList.Create;
  try
    SQL.Assign(TStrings(GetOrdValue));
    SQLText := SQL.Text;
    if (SQLEdit.EditSQL(SQLText, FGetTableNamesProc, FGetFieldNamesProc)) and
      (SQL.Text <> SQLText) then
    begin
      SQL.Text := SQLText;
      SetOrdValue(LongInt(SQL));
    end;
  finally
    SQL.free;
  end;
end;

function TSQLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

{ TMDOQuerySQLProperty }

{
***************************** TMDOQuerySQLProperty *****************************
}
procedure TMDOQuerySQLProperty.Edit;
var
  Query: TMDOQuery;
begin
  Query := TMDOQuery(GetComponent(0));
  if Assigned(Query.Database) then
  begin
    FGetTableNamesProc := Query.Database.GetTableNames;
    FGetFieldNamesProc := Query.Database.GetFieldNames;
  end
  else
  begin
    FGetTableNamesProc := nil;
    FGetFieldNamesProc := nil;
  end;
  EditSQL;
end;

function TMDOQuerySQLProperty.QueryInterface(const IID: TGUID; out Obj): 
        HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

function TMDOQuerySQLProperty._AddRef: Integer;
begin
  Result := -1;
  // non-refcounted memorymanagement
end;

function TMDOQuerySQLProperty._Release: Integer;
begin
  Result := -1;
  // non-refcounted memorymanagement
end;

{ TMDODatasetSQLProperty }

{
**************************** TMDODatasetSQLProperty ****************************
}
procedure TMDODatasetSQLProperty.Edit;
var
  MDODataset: TMDODataset;
begin
  MDODataset := TMDODataset(GetComponent(0));
  if Assigned(MDODataSet.Database) then
  begin
    FGetTableNamesProc := MDODataset.Database.GetTableNames;
    FGetFieldNamesProc := MDODataset.Database.GetFieldNames;
  end
  else
  begin
    FGetTableNamesProc := nil;
    FGetFieldNamesProc := nil;
  end;
  EditSQL;
end;

{ TMDOSQLProperty }

{
******************************* TMDOSQLProperty ********************************
}
procedure TMDOSQLProperty.Edit;
var
  MDOSQL: TMDOSQL;
begin
  MDOSQL := TMDOSQL(GetComponent(0));
  if Assigned(MDOSQL.Database) then
  begin
    FGetTableNamesProc := MDOSQL.Database.GetTableNames;
    FGetFieldNamesProc := MDOSQL.Database.GetFieldNames;
  end
  else
  begin
    FGetTableNamesProc := nil;
    FGetFieldNamesProc := nil;
  end;
  EditSQL;
end;

{ TMDOUpdateSQLEditor }

{
***************************** TMDOUpdateSQLEditor ******************************
}
procedure TMDOUpdateSQLEditor.ExecuteVerb(Index: Integer);
var
  MDOUpdSQL: TMDOUpdateSQL;
begin
  MDOUpdSQL := TMDOUpdateSQL(Component);
  
  GetTableNames := MDOUpdSQL.DataSet.Database.GetTableNames;
  GetFieldNames := MDOUpdSQL.DataSet.Database.GetFieldNames;
  
  if EditMDOUpdateSQL(MDOUpdSQL, GetTableNames, GetFieldnames) then
    Designer.Modified;
  
end;

function TMDOUpdateSQLEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SMDOUpdateSQLEditor;
    2: Result := SMercuryVersion;
  end;
end;

function TMDOUpdateSQLEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TMDODataSetEditor }

{
****************************** TMDODataSetEditor *******************************
}
procedure TMDODataSetEditor.EditSQL;
var
  SQLText: string;
  SQL: TStrings;
begin
  SQL := TStringList.Create;
  try
    SQL.Assign(TMDODataset(Component).SelectSQL);
    SQLText := SQL.Text;
    if (SQLEdit.EditSQL(SQLText, FGetTableNamesProc, FGetFieldNamesProc)) and
      (SQL.Text <> SQLText) then
    begin
      SQL.Text := SQLText;
      TMDODataset(Component).SelectSQL.Assign(SQL);
      TMDODataset(Component).SimpleSelectSQL.Assign(SQL);
    end;
  finally
    SQL.free;
  end;
end;

procedure TMDODataSetEditor.ExecuteVerb(Index: Integer);
var
  MDODataset: TMDODataset;
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    MDODataset := TMDODataset(Component);
    if Assigned(MDODataSet.Database) then
    begin
      FGetTableNamesProc := MDODataset.Database.GetTableNames;
      FGetFieldNamesProc := MDODataset.Database.GetFieldNames;
    end
    else
    begin
      FGetTableNamesProc := nil;
      FGetFieldNamesProc := nil;
    end;
    case Index of
      0:
        if EditMDODataSet(MDODataset, FGetTableNamesProc, FGetFieldNamesProc) then
          Designer.Modified;
      1: (Component as TMDODataSet).ExecSQL;
      2: EditSQL;
    end;
  end;
end;

function TMDODataSetEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SMDODataSetEditor;
      1: Result := SExecute;
      2: Result := SEditSQL;
      3: Result := SMercuryVersion;
    end;
  end;
end;

function TMDODataSetEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 4;
end;

{ TMDOEventListProperty }

{
**************************** TMDOEventListProperty *****************************
}
procedure TMDOEventListProperty.Edit;
var
  Events: TStrings;
begin
  Events := TStringList.Create;
  try
    Events.Assign(TStrings(GetOrdValue));
    if EditAlerterEvents(Events) then
      SetOrdValue(longint(Events));
  finally
    Events.Free;
  end;
end;

function TMDOEventListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

{ TMDODatabaseEditor }

{
****************************** TMDODatabaseEditor ******************************
}
procedure TMDODatabaseEditor.ExecuteVerb(Index: Integer);
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: if EditMDODatabase(TMDODatabase(Component)) then
          Designer.Modified;
    end;
  end;
end;

function TMDODatabaseEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SMDODatabaseEditor;
      1: Result := SMercuryVersion;
    end;
  end;
end;

function TMDODatabaseEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TMDOTransactionEditor }

{
**************************** TMDOTransactionEditor *****************************
}
procedure TMDOTransactionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: if EditMDOTransaction(TMDOTransaction(Component)) then
        Designer.Modified;
  end;
end;

function TMDOTransactionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SMDOTransactionEditor;
    1: Result := SMercuryVersion;
  end;
end;

function TMDOTransactionEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TMDOQueryEditor }

{
******************************* TMDOQueryEditor ********************************
}
procedure TMDOQueryEditor.EditSQL;
var
  SQLText: string;
  SQL: TStrings;
begin
  SQL := TStringList.Create;
  try
    SQL.Assign(TMDOQuery(Component).SQL);
    SQLText := SQL.Text;
  
    if (SQLEdit.EditSQL(SQLText, FGetTableNamesProc, FGetFieldNamesProc)) and
      (SQL.Text <> SQLText) then
    begin
      SQL.Text := SQLText;
      TMDOQuery(Component).SQL.Assign(SQL);
    end;
  finally
    SQL.free;
  end;
end;

procedure TMDOQueryEditor.ExecuteVerb(Index: Integer);
var
  Query: TMDOQuery;
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index)
  else
  begin
    Query := Component as TMDOQuery;
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Query.ExecSQL;
      1:
        begin
          if Assigned(Query.Database) then
          begin
            FGetTableNamesProc := Query.Database.GetTableNames;
            FGetFieldNamesProc := Query.Database.GetFieldNames;
          end
          else
          begin
            FGetTableNamesProc := nil;
            FGetFieldNamesProc := nil;
          end;
          EditSQL;
        end;
    end;
  end;
end;

function TMDOQueryEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SExecute;
      1: Result := SEditSQL;
      2: Result := SMercuryVersion;
    end;
  end;
end;

function TMDOQueryEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 3;
end;

{ TMDOStoredProcEditor }

{
***************************** TMDOStoredProcEditor *****************************
}
procedure TMDOStoredProcEditor.ExecuteVerb(Index: Integer);
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    if Index = 0 then
      (Component as TMDOStoredProc).ExecProc;
  end;
end;

function TMDOStoredProcEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SExecute;
      1: Result := SMercuryVersion;
    end;
  end;
end;

function TMDOStoredProcEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TMDOStoredProcParamsProperty }

{
************************* TMDOStoredProcParamsProperty *************************
}
procedure TMDOStoredProcParamsProperty.Edit;
var
  StoredProc: TMDOStoredProc;
  Params: TParams;
begin
  StoredProc := (GetComponent(0) as TMDOStoredProc);
  Params := TParams.Create(nil);
  try
    StoredProc.CopyParams(Params);
  finally
    Params.Free;
  end;
  inherited Edit;
end;

{ TMDOTableFieldLinkProperty }

{
************************** TMDOTableFieldLinkProperty **************************
}
procedure TMDOTableFieldLinkProperty.Edit;
begin
  FTable := DataSet as TMDOTable;
  inherited Edit;
end;

function TMDOTableFieldLinkProperty.GetIndexFieldNames: string;
begin
  Result := FTable.IndexFieldNames;
end;

function TMDOTableFieldLinkProperty.GetMasterFields: string;
begin
  Result := FTable.MasterFields;
end;

procedure TMDOTableFieldLinkProperty.SetIndexFieldNames(const Value: string);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TMDOTableFieldLinkProperty.SetMasterFields(const Value: string);
begin
  FTable.MasterFields := Value;
end;

{ TMDOUServiceEditor }

{
****************************** TMDOServiceEditor *******************************
}
procedure TMDOServiceEditor.ExecuteVerb(Index: Integer);
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index)
  else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: if EditMDOService(TMDOCustomService(Component)) then
          Designer.Modified;
    end;
  end;
end;

function TMDOServiceEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'MDO Service Editor...';
    1: Result := 'Mercury Services';
  end;
end;

function TMDOServiceEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.

