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

unit MDOUpdateSQLEditor;

interface

uses Forms, DB, ExtCtrls, StdCtrls, Controls, ComCtrls, Classes, SysUtils,
  Windows, Menus, MDO, MDODatabase, MDOUpdateSQL, MDOCustomDataSet, MDOTable,
  MDOQuery, MDOConst, MDOUtils;

type

  TWaitMethod = procedure of object;
  TGetTableNamesProc = procedure (List: TStrings; SystemTables: Boolean) of 
          object;
  TGetFieldNamesProc = procedure (const TableName: string; List: TStrings) of 
          object;
  TMDOUpdateSQLEditForm = class (TForm)
    CancelButton: TButton;
    DefaultButton: TButton;
    FieldListPopup: TPopupMenu;
    FieldsPage: TTabSheet;
    gboSQLGeneration: TGroupBox;
    GenerateButton: TButton;
    GetTableFieldsButton: TButton;
    HelpButton: TButton;
    KeyFieldList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    miClearAll: TMenuItem;
    miSelectAll: TMenuItem;
    okButton: TButton;
    Panel1: TPanel;
    pcoUpdate: TPageControl;
    PrimaryKeyButton: TButton;
    QuoteFields: TCheckBox;
    SQLMemo: TMemo;
    SQLPage: TTabSheet;
    StatementType: TRadioGroup;
    UpdateFieldList: TListBox;
    UpdateTableName: TComboBox;
    IgnoreCalculedFields: TCheckBox;
    procedure DefaultButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure GetTableFieldsButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure miClearAllClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure pcoUpdateChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PrimaryKeyButtonClick(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure SQLMemoKeyPress(Sender: TObject; var Key: Char);
    procedure StatementTypeClick(Sender: TObject);
    procedure UpdateTableNameChange(Sender: TObject);
    procedure UpdateTableNameClick(Sender: TObject);
  private
    DataSet: TMDOCustomDataset;
    DataSetEditorFlag: Boolean;
    FDatasetDefaults: Boolean;
    FSettingsChanged: Boolean;
    FTempTable: TMDOTable;
    GetFieldNames: TGetFieldNamesProc;
    GetTableNames: TGetTableNamesProc;
    RefreshSQL: TStrings;
    SQLText: array[TUpdateKind] of TStrings;
    StmtIndex: Integer;
    UpdateSQL: TMDOUpdateSQL;
    function Edit: Boolean;
    procedure GenDeleteSQL(const TableName, QuoteChar: string; KeyFields, SQL:
            TStrings);
    procedure GenerateSQL;
    procedure GenInsertSQL(const TableName, QuoteChar: string; UpdateFields,
            SQL: TStrings);
    procedure GenModifySQL(const TableName, QuoteChar: string; KeyFields,
            UpdateFields, SQL: TStrings);
    procedure GenRefreshSQL(const TableName, QuoteChar: string; KeyFields, SQL:
            TStrings);
    procedure GenWhereClause(const TabAlias, QuoteChar: string; KeyFields, SQL:
            TStrings);
    procedure GetDataSetFieldNames;
    procedure GetTableFieldNames;
    function GetTableRef(const TabName, QuoteChar: string): string;
    procedure InitGenerateOptions;
    procedure InitUpdateTableNames;
    procedure SelectPrimaryKeyFields;
    procedure SetButtonStates;
    procedure SetDefaultSelections;
    procedure ShowWait(WaitMethod: TWaitMethod);
    procedure UpdateInternalSQLText;
    function TempTable: TMDOTable;
    procedure DeleteCalculedFields(const TableName: String; var List: TStrings);
  end;

  { TSQLParser }

  TSQLToken = (stSymbol, stAlias, stNumber, stComma, stEQ, stOther, stLParen,
    stRParen, stEnd);

  TSQLParser = class (TObject)
  private
    FSourcePtr: PChar;
    FSymbolQuoted: Boolean;
    FText: string;
    FToken: TSQLToken;
    FTokenPtr: PChar;
    FTokenString: string;
    function NextToken: TSQLToken;
    procedure Reset;
    function TokenSymbolIs(const S: string): Boolean;
  public
    constructor Create(const Text: string);
    procedure GetSelectTableNames(List: TStrings);
    procedure GetUpdateFields(List: TStrings);
    procedure GetUpdateTableName(var TableName: string);
    procedure GetWhereFields(List: TStrings);
  end;
  
function EditMDOUpdateSQL(AUpdateSQL: TMDOUpdateSQL; GetTableNamesProc:
  TGetTableNamesProc; GetFieldnamesProc: TGetFieldNamesProc): Boolean;

function EditMDODataSet(ADataSet: TMDODataSet; GetTableNamesProc:
  TGetTableNamesProc; GetFieldnamesProc: TGetFieldNamesProc): Boolean;

implementation

{$R *.DFM}

uses Dialogs, {LibHelp,} TypInfo;

{ Global Interface functions }

function EditMDOUpdateSQL(AUpdateSQL: TMDOUpdateSQL; GetTableNamesProc:
  TGetTableNamesProc; GetFieldnamesProc: TGetFieldNamesProc): Boolean;
begin
  with TMDOUpdateSQLEditForm.Create(Application) do
  try
    DataSetEditorFlag := False;
    UpdateSQL := AUpdateSQL;

    GetTableNames := GetTableNamesProc;
    GetFieldNames := GetFieldnamesProc;

    Result := Edit;
  finally
    Free;
  end;
end;

function EditMDODataSet(ADataSet: TMDODataSet; GetTableNamesProc:
  TGetTableNamesProc; GetFieldnamesProc: TGetFieldNamesProc): Boolean;
var
  TempUpdateSQL: TMDOUpdateSQL;
  TempQuery: TMDODataSet;
begin
  TempUpdateSQL := TMDOUpdateSQL.Create(ADataSet);
  TempQuery := TMDODataSet.Create(ADataSet);
  try
    with TempQuery do
    begin
      Name := Concat('MDOInternal', ADataSet.Name); {mbcs ok}
      Database := ADataSet.Database;
      Transaction := ADataSet.Transaction;
      SelectSQL.Assign(ADataSet.SelectSQL);
      SimpleSelectSQL.Assign(ADataSet.SimpleSelectSQL);
      UpdateObject := TempUpdateSQL;
      TempUpdateSQL.ModifySQL.Assign(ADataSet.ModifySQL);
      TempUpdateSQL.InsertSQL.Assign(ADataSet.InsertSQL);
      TempUpdateSQL.DeleteSQL.Assign(ADataSet.DeleteSQL);
      TempUpdateSQL.RefreshSQL.Assign(ADataSet.RefreshSQL);
    end;
    with TMDOUpdateSQLEditForm.Create(Application) do
    try
      GetTableNames := GetTableNamesProc;
      GetFieldNames := GetFieldnamesProc;
      DataSetEditorFlag := True;
      UpdateSQL := TempUpdateSQL;
      Result := Edit;
    finally
      Free;
    end;
    if Result then
    begin
      ADataSet.RefreshSQL.Assign(TempUpdateSQL.RefreshSQL);
      ADataSet.InsertSQL.Assign(TempUpdateSQL.InsertSQL);
      ADataSet.ModifySQL.Assign(TempUpdateSQL.ModifySQL);
      ADataSet.DeleteSQL.Assign(TempUpdateSQL.DeleteSQL);
    end;
  finally
    TempUpdateSQL.free;
    TempQuery.free;
  end;
end;

{ Utility Routines }

procedure GetSelectedItems(ListBox: TListBox; List: TStrings);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to ListBox.Items.Count - 1 do
    if ListBox.Selected[I] then
      List.Add(ListBox.Items[I]);
end;


procedure TMDOUpdateSQLEditForm.DeleteCalculedFields( Const TableName: String; Var List: TStrings );
var
  TempQuery : TMDOQuery;
  S : String;
  I : Integer;
begin
  S := 'SELECT ';
  S := S + '  R.RDB$FIELD_NAME    FIELD,';
  S := S + '  F.RDB$FIELD_NAME    FIELD_SOURCE,';
  S := S + '  R.RDB$RELATION_NAME TABLE ';
  S := S + 'FROM RDB$RELATION_FIELDS R, RDB$FIELDS F ';
  S := S + 'WHERE (R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME) AND';
  S := S + '      (R.RDB$RELATION_NAME = ' + ''''
         + FormatIdentifierValue( DataSet.Database.SQLDialect, QuoteIdentifier( DataSet.Database.SQLDialect, TableName ) ) + '''' + ') AND';
  S := S + '      ((R.RDB$UPDATE_FLAG = 0) OR (R.RDB$UPDATE_FLAG IS NULL)) AND';
  S := S + '      (F.RDB$COMPUTED_SOURCE IS NOT NULL)';
  TempQuery := TMDOQuery.Create( Application );
  with TempQuery, SQL do
    try
      Database := DataSet.Database;
      Close;
      SQL.Text := S;
      Prepare;
      Open;
      DisableControls;
      try
        while not eof do
        begin
          I := List.IndexOf( Trim( FieldByName( 'FIELD' ).AsString ) );
          if I >= 0 then
            List.Delete( I );
          Next;
        end;
      finally
        EnableControls;
      end;
    finally
      TempQuery.Free;
    end;
end;


function SetSelectedItems(ListBox: TListBox; List: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  ListBox.Items.BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count - 1 do
      if List.IndexOf(ListBox.Items[I]) > -1 then
      begin
        ListBox.Selected[I] := True;
        Inc(Result);
      end
      else
        ListBox.Selected[I] := False;
    if ListBox.Items.Count > 0 then
    begin
      ListBox.ItemIndex := 0;
      ListBox.TopIndex := 0;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure SelectAll(ListBox: TListBox);
var
  I: Integer;
begin
  ListBox.Items.BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count - 1 do
      ListBox.Selected[I] := True;

    if ListBox.Items.Count > 0 then
    begin
      ListBox.ItemIndex := 0;
      ListBox.TopIndex := 0;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure GetSQLTableNames(const SQL: string; List: TStrings);
begin
  with TSQLParser.Create(SQL) do
  try
    GetSelectTableNames(List);
  finally
    Free;
  end;
end;

procedure ParseUpdateSQL(const SQL: string; var TableName: string;
  UpdateFields: TStrings; WhereFields: TStrings);
begin
  with TSQLParser.Create(SQL) do
  try
    GetUpdateTableName(TableName);
    if Assigned(UpdateFields) then
    begin
      Reset;
      GetUpdateFields(UpdateFields);
    end;
    if Assigned(WhereFields) then
    begin
      Reset;
      GetWhereFields(WhereFields);
    end;
  finally
    Free;
  end;
end;

{ TSQLParser }

{
********************************** TSQLParser **********************************
}
constructor TSQLParser.Create(const Text: string);
begin
  FText := Text;
  FSourcePtr := PChar(Text);
  NextToken;
end;

procedure TSQLParser.GetSelectTableNames(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('SELECT') then { Do not localize }
    try
      while not TokenSymbolIs('FROM') do
        NextToken; { Do not localize }
      NextToken;
      while FToken = stSymbol do
      begin
        List.AddObject(FTokenString, Pointer(Integer(FSymbolQuoted)));
        if NextToken = stSymbol then
          NextToken;
        if FToken = stComma then
          NextToken
        else
          break;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TSQLParser.GetUpdateFields(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('UPDATE') then { Do not localize }
    try
      while not TokenSymbolIs('SET') do
        NextToken; { Do not localize }
      NextToken;
      while True do
      begin
        if FToken = stAlias then
          NextToken;
        if FToken <> stSymbol then
          Break;
        List.Add(FTokenString);
        if NextToken <> stEQ then
          Break;
        while NextToken <> stComma do
          if TokenSymbolIs('WHERE') then
            Exit; { Do not localize }
        NextToken;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TSQLParser.GetUpdateTableName(var TableName: string);
begin
  if TokenSymbolIs('UPDATE') and (NextToken = stSymbol) then { Do not localize }
    TableName := FTokenString
  else
    TableName := '';
end;

procedure TSQLParser.GetWhereFields(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('UPDATE') then { Do not localize }
    try
      while not TokenSymbolIs('WHERE') do
        NextToken; { Do not localize }
      NextToken;
      while True do
      begin
        while FToken in [stLParen, stAlias] do
          NextToken;
        if FToken <> stSymbol then
          Break;
        List.Add(FTokenString);
        if NextToken <> stEQ then
          Break;
        while true do
        begin
          NextToken;
          if FToken = stEnd then
            Exit;
          if TokenSymbolIs('AND') then
            Break; { Do not localize }
        end;
        NextToken;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

function TSQLParser.NextToken: TSQLToken;
var
  P, TokenStart: PChar;
  QuoteChar: Char;
  IsParam: Boolean;
  
  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;
  
begin
  if FToken = stEnd then
    SysUtils.Abort;
  FTokenString := '';
  FSymbolQuoted := False;
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do
    Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', '$', #127..#255:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '"', '$',
            #127..#255] do
            Inc(P);
        end
        else
        begin
          while TRUE do
          begin
            if (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '"', '$']) or
              IsKatakana(Byte(P^)) then
              Inc(P)
            else if P^ in LeadBytes then
              Inc(P, 2)
            else
              Break;
          end;
        end;
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := stSymbol;
      end;
    '''', '"':
      begin
        QuoteChar := P^;
        Inc(P);
        IsParam := P^ = ':';
        if IsParam then
          Inc(P);
        TokenStart := P;
        while not (P^ in [QuoteChar, #0]) do
          Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        Inc(P);
        Trim(FTokenString);
        FToken := stSymbol;
        FSymbolQuoted := True;
      end;
    '-', '0'..'9':
      begin
        TokenStart := P;
        Inc(P);
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do
          Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := stNumber;
      end;
    ',':
      begin
        Inc(P);
        FToken := stComma;
      end;
    '=':
      begin
        Inc(P);
        FToken := stEQ;
      end;
    '(':
      begin
        Inc(P);
        FToken := stLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := stRParen;
      end;
    #0:
      FToken := stEnd;
  else
    begin
      FToken := stOther;
      Inc(P);
    end;
  end;
  FSourcePtr := P;
  if (FToken = stSymbol) and
    (FTokenString[Length(FTokenString)] = '.') then
    FToken := stAlias;
  Result := FToken;
end;

procedure TSQLParser.Reset;
begin
  FSourcePtr := PChar(FText);
  FToken := stSymbol;
  NextToken;
end;

function TSQLParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = stSymbol) and (CompareText(FTokenString, S) = 0);
end;

{ TMDOUpdateSQLEditor }

{ Private Methods }

{
**************************** TMDOUpdateSQLEditForm *****************************
}
procedure TMDOUpdateSQLEditForm.DefaultButtonClick(Sender: TObject);
begin
  with UpdateTableName do
    begin
      if (Items.Count > 0) and (ItemIndex = -1) then
        ItemIndex := Items.IndexOf(Text);
    end;
  ShowWait(GetDataSetFieldNames);
  FDatasetDefaults := True;
  SetDefaultSelections;
  KeyfieldList.SetFocus;
  SetButtonStates;
end;

function TMDOUpdateSQLEditForm.Edit: Boolean;
var
  Index: TUpdateKind;
  DataSetName: string;
begin
  Result := False;
  if Assigned(UpdateSQL.DataSet) and (UpdateSQL.DataSet is TMDOCustomDataset) then
  begin
    DataSet := TMDOCustomDataset(UpdateSQL.DataSet);
    QuoteFields.Enabled := False;
    if Assigned(DataSet.Database) then
    begin
      FTempTable.Database := DataSet.Database;
      if DataSet.Database.SQLDialect < 3 then
        QuoteFields.Enabled := False
      else
        QuoteFields.Enabled := True;
    end;
    DataSetName := Format('%s%s%s', [DataSet.Owner.Name, DotSep, DataSet.Name]);
  end
  else
    DataSetName := SNoDataSet;
  if DataSetEditorFlag then
  begin
    DataSetName := Copy(DataSet.Name, Length('MDOInternal') + 1,
      Length(DataSet.Name)); {mbcs ok}
    Caption := Format('%s%s%s', [DataSet.Owner.owner.Name, DotSep,
      DataSetName]);
  end
  else
    Caption := Format('%s%s%s (%s)', [UpdateSQL.Owner.Name, DotSep,
      UpdateSQL.Name, DataSetName]);
  try
    for Index := Low(TUpdateKind) to High(TUpdateKind) do
    begin
      SQLText[Index] := TStringList.Create;
      SQLText[Index].Assign(UpdateSQL.SQL[Index]);
    end;
    RefreshSQL := TStringList.Create;
    RefreshSQL.Assign(UpdateSQL.RefreshSQL);
    StatementTypeClick(Self);
  
    InitUpdateTableNames;
  
    if (@GetTableNames <> nil) then
      GetTableNames(UpdateTableName.Items, false);
  
    ShowWait(InitGenerateOptions);
  
    pcoUpdate.ActivePage := pcoUpdate.Pages[0];
    if ShowModal = mrOk then
    begin
      for Index := low(TUpdateKind) to high(TUpdateKind) do
        UpdateSQL.SQL[Index] := SQLText[Index];
      UpdateSQL.RefreshSQL := RefreshSQL;
      Result := True;
    end;
  finally
    for Index := Low(TUpdateKind) to High(TUpdateKind) do
      SQLText[Index].Free;
    RefreshSQL.free;
  end;
end;

procedure TMDOUpdateSQLEditForm.FormCloseQuery(Sender: TObject; var CanClose: 
        Boolean);
begin
  if (ModalResult = mrOK) and FSettingsChanged then
    CanClose := MessageDlg(SSQLNotGenerated, mtConfirmation,
      mbYesNoCancel, 0) = mrYes;
end;

procedure TMDOUpdateSQLEditForm.FormCreate(Sender: TObject);
begin
//  HelpContext := hcDIBUpdateSQL;
  FTempTable := TMDOTable.Create(Application);
end;

procedure TMDOUpdateSQLEditForm.FormDestroy(Sender: TObject);
begin
  FTempTable.Free;
end;

procedure TMDOUpdateSQLEditForm.GenDeleteSQL(const TableName, QuoteChar: string;
        KeyFields, SQL: TStrings);
begin
  SQL.Clear;
  SQL.Add(Format('DELETE FROM %s%s%0:s', [QuoteChar, TableName]));
  { Do not localize }
  GenWhereClause(GetTableRef(TableName, QuoteChar), QuoteChar, KeyFields, SQL);
end;

procedure TMDOUpdateSQLEditForm.GenerateButtonClick(Sender: TObject);
begin
  GenerateSQL;
  FSettingsChanged := False;
end;

procedure TMDOUpdateSQLEditForm.GenerateSQL;
  
    function QuotedTableName(const BaseName: string): string;
    begin
      with UpdateTableName do
        if QuoteFields.Checked then
          Result := Format('"%s"', [BaseName])
        else
          Result := BaseName;
    end;
  
  var
    KeyFields: TStringList;
    UpdateFields: TStringList;
    QuoteChar, TableName: string;
  
begin
  if (KeyFieldList.SelCount = 0) or (UpdateFieldList.SelCount = 0) then
    raise Exception.CreateRes(@SSQLGenSelect);
  KeyFields := TStringList.Create;
  try
    GetSelectedItems(KeyFieldList, KeyFields);
    UpdateFields := TStringList.Create;
    try
      GetSelectedItems(UpdateFieldList, UpdateFields);
      TableName := QuotedTableName(UpdateTableName.Text);
      TableName := UpdateTableName.Text;
      if QuoteFields.Checked then
        QuoteChar := '"'
      else
        QuoteChar := '';
      GenDeleteSQL(TableName, QuoteChar, KeyFields, SQLText[ukDelete]);
      if IgnoreCalculedFields.Checked then
        DeleteCalculedFields(String(UpdateTableName.Text), TStrings(UpdateFields));
      GenInsertSQL(TableName, QuoteChar, UpdateFields, SQLText[ukInsert]);
      GenModifySQL(TableName, QuoteChar, KeyFields, UpdateFields,
        SQLText[ukModify]);
      GenRefreshSQL(TableName, QuoteChar, KeyFields, RefreshSQL);
      SQLMemo.Modified := False;
      StatementTypeClick(Self);
      pcoUpdate.SelectNextPage(True);
    finally
      UpdateFields.Free;
    end;
  finally
    KeyFields.Free;
  end;
end;

procedure TMDOUpdateSQLEditForm.GenInsertSQL(const TableName, QuoteChar: string;
        UpdateFields, SQL: TStrings);
  
  procedure GenFieldList(const TabName, ParamChar, QuoteChar: string);
  var
    L: string;
    I: integer;
    Comma: string;
  begin
    L := '  (';
    Comma := ', ';
    for I := 0 to UpdateFields.Count - 1 do
    begin
      if I = UpdateFields.Count - 1 then
        Comma := '';
      L := Format('%s%s%s%s%s%3:s%5:s',
        [L, TabName, ParamChar, QuoteChar, UpdateFields[I], Comma]);
      if (Length(L) > 70) and (I <> UpdateFields.Count - 1) then
      begin
        SQL.Add(L);
        L := '   ';
      end;
    end;
    SQL.Add(L + ')');
  end;
  
begin
  SQL.Clear;
  SQL.Add(Format('INSERT INTO %s%s%0:s', [QuoteChar, TableName]));
  { Do not localize }
  GenFieldList(GetTableRef(TableName, QuoteChar), '', QuoteChar);
  SQL.Add('VALUES'); { Do not localize }
  GenFieldList('', ':', QuoteChar);
end;

procedure TMDOUpdateSQLEditForm.GenModifySQL(const TableName, QuoteChar: string;
        KeyFields, UpdateFields, SQL: TStrings);
var
  I: Integer;
  Comma: string;
  TableRef: string;
begin
  SQL.Clear;
  SQL.Add(Format('UPDATE %s%s%0:s', [QuoteChar, TableName])); { Do not localize }
  SQL.Add('SET'); { Do not localize }
  Comma := ',';
  TableRef := GetTableRef(TableName, QuoteChar);
  for I := 0 to UpdateFields.Count - 1 do
  begin
    if I = UpdateFields.Count - 1 then
      Comma := '';
    SQL.Add(Format('  %s%s%s%1:s = :%1:s%2:s%1:s%3:s',
      [TableRef, QuoteChar, UpdateFields[I], Comma]));
  end;
  GenWhereClause(TableRef, QuoteChar, KeyFields, SQL);
end;

procedure TMDOUpdateSQLEditForm.GenRefreshSQL(const TableName, QuoteChar: 
        string; KeyFields, SQL: TStrings);
var
  I: Integer;
  Comma: string;
  TableRef: string;
  RefreshFieldList: TStrings;
  
  procedure GenRefreshWhereClause;
  var
    I: Integer;
    BindText: string;
    FieldName: string;
  begin
    RefreshSQL.Add('WHERE'); { Do not localize }
    for I := 0 to KeyFields.Count - 1 do
    begin
      FieldName := KeyFields[I];
      BindText := Format('  %s%s%s%1:s = :%1:s%2:s%1:s', { Do not localize }
        [TableRef, QuoteChar, FieldName]);
      if I < KeyFields.Count - 1 then
        BindText := Format('%s AND', [BindText]); { Do not localize }
      RefreshSQL.Add(BindText);
    end;
  end;
  
begin
  RefreshFieldList := TStringList.Create;
  try
    GetFieldNames(TempTable.TableName, RefreshFieldList);
    Comma := ',';
    TableRef := GetTableRef(TableName, QuoteChar);
    RefreshSQL.Clear;
    RefreshSQL.Add('SELECT ');
    if Dataset is TMDOTable then
      RefreshSQL.Add('  RDB$DB_KEY as MDO_INTERNAL_DBKEY, ');
    for I := 0 to RefreshFieldList.Count - 1 do
    begin
      if I = RefreshFieldList.Count - 1 then
        Comma := '';
      RefreshSQL.Add(Format('  %s%s%s%1:s%3:s',
        [TableRef, QuoteChar, RefreshFieldList[I], Comma]));
    end;
    RefreshSQL.Add(Format('FROM %s%s%0:s ', [QuoteChar, TableName]));
    GenRefreshWhereClause;
  finally
    RefreshFieldList.Free;
  end;
end;

procedure TMDOUpdateSQLEditForm.GenWhereClause(const TabAlias, QuoteChar: 
        string; KeyFields, SQL: TStrings);
var
  I: Integer;
  BindText: string;
  FieldName: string;
begin
  SQL.Add('WHERE'); { Do not localize }
  for I := 0 to KeyFields.Count - 1 do
  begin
    FieldName := KeyFields[I];
    BindText := Format('  %s%s%s%1:s = :%1:sOLD_%2:s%1:s', { Do not localize }
      [TabAlias, QuoteChar, FieldName]);
    if I < KeyFields.Count - 1 then
      BindText := Format('%s AND', [BindText]); { Do not localize }
    SQL.Add(BindText);
  end;
end;

procedure TMDOUpdateSQLEditForm.GetDataSetFieldNames;
begin
  if (@GetFieldNames <> nil) then
  begin
    if DataSetEditorFlag then
      begin
        GetFieldNames(UpdateTableName.Text,
          KeyFieldList.Items);
      end
    else
      GetFieldNames(UpdateTableName.Text, KeyFieldList.Items);
    UpdateFieldList.Items.Assign(KeyFieldList.Items);
  end;
end;

procedure TMDOUpdateSQLEditForm.GetTableFieldNames;
begin
  if (@GetFieldNames <> nil) then
    GetFieldNames(UpdateTableName.Text, KeyFieldList.Items);
  UpdateFieldList.Items.Assign(KeyFieldList.Items);
  FDatasetDefaults := False;
end;

procedure TMDOUpdateSQLEditForm.GetTableFieldsButtonClick(Sender: TObject);
begin
  ShowWait(GetTableFieldNames);
  SetDefaultSelections;
  SettingsChanged(Sender);
end;

function TMDOUpdateSQLEditForm.GetTableRef(const TabName, QuoteChar: string): 
        string;
begin
  if QuoteChar <> '' then
    Result := QuoteChar + TabName + QuoteChar + '.'
  else
    Result := '';
end;

procedure TMDOUpdateSQLEditForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TMDOUpdateSQLEditForm.InitGenerateOptions;
var
  UpdTabName: string;
  
  procedure InitFromDataSet;
  begin
    // If this is a Query with more than 1 table in the "from" clause then
    //  initialize the list of fields from the table rather than the dataset.
    if (UpdateTableName.Items.Count > 1) then
      GetTableFieldNames
    else
    begin
      GetDataSetFieldNames;
      FDatasetDefaults := True;
    end;
    SetDefaultSelections;
  end;
  
  procedure InitFromUpdateSQL;
  var
    UpdFields,
      WhFields: TStrings;
  begin
    UpdFields := TStringList.Create;
    try
      WhFields := TStringList.Create;
      try
        ParseUpdateSQL(SQLText[ukModify].Text, UpdTabName, UpdFields, WhFields);
        GetDataSetFieldNames;
        if SetSelectedItems(UpdateFieldList, UpdFields) < 1 then
          SelectAll(UpdateFieldList);
        if SetSelectedItems(KeyFieldList, WhFields) < 1 then
          SelectAll(KeyFieldList);
      finally
        WhFields.Free;
      end;
    finally
      UpdFields.Free;
    end;
  end;
  
begin
  // If there are existing update SQL statements, try to initialize the
  // dialog with the fields that correspond to them.
  if SQLText[ukModify].Count > 0 then
  begin
    ParseUpdateSQL(SQLText[ukModify].Text, UpdTabName, nil, nil);
    // If the table name from the update statement is not part of the
    // dataset, then initialize from the dataset instead.
    if (UpdateTableName.Items.Count > 0) and
      (UpdateTableName.Items.IndexOf(UpdTabName) > -1) then
    begin
      UpdateTableName.Text := UpdTabName;
      InitFromUpdateSQL;
    end
    else
    begin
      InitFromDataSet;
      UpdateTableName.Items.Add(UpdTabName);
    end;
  end
  else
    InitFromDataSet;
  SetButtonStates;
end;

procedure TMDOUpdateSQLEditForm.InitUpdateTableNames;
begin
  UpdateTableName.Clear;
  if Assigned(DataSet) then
  begin
    if DataSet is TMDOQuery then
    begin
      GetSQLTableNames(TMDOQuery(DataSet).SQL.Text, UpdateTableName.Items)
    end
    else if (DataSet is TMDOTable) and (TMDOTable(DataSet).TableName <> '') then
      UpdateTableName.Items.Add(TMDOTable(DataSet).TableName)
    else if (DataSet is TMDOCustomDataSet) then
    begin
      GetSQLTableNames(TMDODataSet(DataSet).SelectSQL.Text, UpdateTableName.Items)
    end;
  end;
  
  if UpdateTableName.Items.Count > 0 then
    UpdateTableName.ItemIndex := 0;
end;

procedure TMDOUpdateSQLEditForm.miClearAllClick(Sender: TObject);
var
  I: Integer;
begin
  with FieldListPopup.PopupComponent as TListBox do
  begin
    Items.BeginUpdate;
    try
      for I := 0 to Items.Count - 1 do
        Selected[I] := False;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TMDOUpdateSQLEditForm.miSelectAllClick(Sender: TObject);
begin
  SelectAll(FieldListPopup.PopupComponent as TListBox);
end;

procedure TMDOUpdateSQLEditForm.OkButtonClick(Sender: TObject);
begin
  UpdateInternalSQLText;
end;

procedure TMDOUpdateSQLEditForm.pcoUpdateChanging(Sender: TObject; var 
        AllowChange: Boolean);
begin
  if (pcoUpdate.ActivePage = pcoUpdate.Pages[0]) and
    not SQLPage.Enabled then
    AllowChange := False;
end;

procedure TMDOUpdateSQLEditForm.PrimaryKeyButtonClick(Sender: TObject);
begin
  ShowWait(SelectPrimaryKeyFields);
  SettingsChanged(Sender);
end;

procedure TMDOUpdateSQLEditForm.SelectPrimaryKeyFields;
var
  SepPos, I, Index: Integer;
  FName, FieldNames: string;
begin
  if KeyFieldList.Items.Count < 1 then
    Exit;
  with TempTable do
  begin
    IndexDefs.Update;
    for I := 0 to KeyFieldList.Items.Count - 1 do
      KeyFieldList.Selected[I] := False;
    for I := 0 to IndexDefs.Count - 1 do
      if ixPrimary in IndexDefs[I].Options then
      begin
        FieldNames := IndexDefs[I].Fields + ';';
        while Length(FieldNames) > 0 do
        begin
          SepPos := Pos(';', FieldNames);
          if SepPos < 1 then
            Break;
          FName := Copy(FieldNames, 1, SepPos - 1);
          System.Delete(FieldNames, 1, SepPos);
          Index := KeyFieldList.Items.IndexOf(FName);
          if Index > -1 then
            KeyFieldList.Selected[Index] := True;
        end;
        break;
      end;
  end;
end;

procedure TMDOUpdateSQLEditForm.SetButtonStates;
begin
  GetTableFieldsButton.Enabled := UpdateTableName.Text <> '';
  PrimaryKeyButton.Enabled := GetTableFieldsButton.Enabled and
    (KeyFieldList.Items.Count > 0);
  GenerateButton.Enabled := GetTableFieldsButton.Enabled and
    (UpdateFieldList.Items.Count > 0) and (KeyFieldList.Items.Count > 0);
  DefaultButton.Enabled := Assigned(DataSet) and not FDatasetDefaults;
end;

procedure TMDOUpdateSQLEditForm.SetDefaultSelections;
var
  DSFields: TStringList;
begin
  if FDatasetDefaults or not Assigned(DataSet) then
  begin
    SelectAll(UpdateFieldList);
    PrimaryKeyButton.Click;
  end
  else if (DataSet.FieldDefs.Count > 0) then
  begin
    DSFields := TStringList.Create;
    try
      GetFieldNames(UpdateTableName.Text, UpdateFieldList.Items);
      SetSelectedItems(KeyFieldList, DSFields);
      SetSelectedItems(UpdateFieldList, DSFields);
    finally
      DSFields.Free;
    end;
  end;
end;

procedure TMDOUpdateSQLEditForm.SettingsChanged(Sender: TObject);
begin
  FSettingsChanged := True;
  FDatasetDefaults := False;
  SetButtonStates;
end;

procedure TMDOUpdateSQLEditForm.ShowWait(WaitMethod: TWaitMethod);
var
  SetCursor: Boolean;
begin
  SetCursor := Screen.Cursor = crDefault;
  if SetCursor then
    Screen.Cursor := crHourGlass;
  Screen.Cursor := crHourGlass;
  try
    WaitMethod;
  finally
    if SetCursor and (Screen.Cursor = crHourGlass) then
      Screen.Cursor := crDefault;
  end;
end;

procedure TMDOUpdateSQLEditForm.SQLMemoKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TMDOUpdateSQLEditForm.StatementTypeClick(Sender: TObject);
begin
  UpdateInternalSQLText;
  StmtIndex := StatementType.ItemIndex;
  // Update visible SQLMemo to user
  if StatementType.ItemIndex > Integer(High(TUpdateKind)) then
    SQLMemo.Lines.Assign(RefreshSQL)
  else
    SQLMemo.Lines.Assign(SQLText[TUpdateKind(StmtIndex)]);
end;

function TMDOUpdateSQLEditForm.TempTable: TMDOTable;
begin
  if FTempTable.TableName <> UpdateTableName.Text then
  begin
    FTempTable.Close;
    FTempTable.TableName := UpdateTableName.Text;
  end;
  Result := FTempTable;
end;

procedure TMDOUpdateSQLEditForm.UpdateInternalSQLText;
begin
  if (SQLMemo.Modified) and (StmtIndex <= Integer(High(TUpdateKind))) then
    SQLText[TUpdateKind(StmtIndex)].Assign(SQLMemo.Lines)
  else if (SQLMemo.Modified) and (StmtIndex > Integer(High(TUpdateKind))) then
    RefreshSQL.Assign(SQLMemo.Lines);
end;

procedure TMDOUpdateSQLEditForm.UpdateTableNameChange(Sender: TObject);
begin
  SettingsChanged(Sender);
end;

procedure TMDOUpdateSQLEditForm.UpdateTableNameClick(Sender: TObject);
begin
  if not Visible then
    Exit;
  GetTableFieldsButtonClick(Sender);
end;

{ Event Handlers }

end.
