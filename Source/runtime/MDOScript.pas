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

unit MDOScript;

interface

uses
  SysUtils, Classes, MDODatabase, MDOCustomDataset, MDOSQL, MDODatabaseInfo,
  MDOQuery;

type

  TMDOScript = class;

  TMDOParseKind = (stmtDDL, stmtDML, stmtSET, stmtCONNECT, stmtDrop,
    stmtCREATE, stmtINPUT, stmtUNK, stmtEMPTY, stmtTERM, stmtERR,
    stmtCOMMIT, stmtROLLBACK, stmtReconnect);

  TMDOSQLParseError = procedure (Sender: TObject; Error: string; SQLText: 
          string; LineIndex: Integer) of object;
  TMDOSQLExecuteError = procedure (Sender: TObject; Error: string; SQLText: 
          string; LineIndex: Integer; var Ignore: Boolean) of object;
  TMDOSQLParseStmt = procedure (Sender: TObject; AKind: TMDOParseKind; SQLText: 
          string) of object;
  TMDOScriptParamCheck = procedure (Sender: TMDOScript; var Pause: Boolean) of 
          object;
  TMDOSQLParseBeforeStmt = procedure (Sender: TObject; AKind: TMDOParseKind;
          var DoStatement: boolean) of object;
  TMDOSQLParser = class (TComponent)
  private
    FFinished: Boolean;
    FInput: TStrings;
    FOnError: TMDOSQLParseError;
    FOnParse: TMDOSQLParseStmt;
    FOnStatement: TMDOSQLParseBeforeStmt;
    FPaused: Boolean;
    FScript: TStrings;
    FTerminator: string;
    FTokens: TStrings;
    FWork: string;
    ImportIndex: Integer;
    InInput: Boolean;
    LineIndex: Integer;
    ScriptIndex: Integer;
    function AppendNextLine: Boolean;
    function IsValidStatement: TMDOParseKind;
    procedure LoadInput;
    procedure RemoveComment;
    procedure SetPaused(const Value: Boolean);
    procedure SetScript(const Value: TStrings);
    function TokenizeNextLine: string;
  protected
    procedure DoOnError(Error: string; SQLText: string); virtual;
    procedure DoOnParse(AKind: TMDOParseKind; SQLText: string); virtual;
    procedure DoOnStatement(AKind: TMDOParseKind; var DoStatement: Boolean);
    procedure DoParser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Parse;
    property CurrentLine: Integer read LineIndex;
    property CurrentTokens: TStrings read FTokens;
  published
    property Finished: Boolean read FFinished;
    property OnError: TMDOSQLParseError read FOnError write FOnError;
    property OnParse: TMDOSQLParseStmt read FOnParse write FOnParse;
    property OnStatement: TMDOSQLParseBeforeStmt read FOnStatement write FOnStatement;
    property Paused: Boolean read FPaused write SetPaused;
    property Script: TStrings read FScript write SetScript;
    property Terminator: string read FTerminator write FTerminator;
  end;
  
  TMDOScriptStats = class (TObject)
  private
    FBuffers: Int64;
    FDatabase: TMDODatabase;
    FDeltaMem: Int64;
    FFetches: Int64;
    FInfoStats: TMDODatabaseInfo;
    FReadIdx: Int64;
    FReads: Int64;
    FSeqReads: Int64;
    FStartBuffers: Int64;
    FStartFetches: Int64;
    FStartingMem: Int64;
    FStartReadIdx: Int64;
    FStartReads: Int64;
    FStartSeqReads: Int64;
    FStartWrites: Int64;
    FWrites: Int64;
    function AddStringValues(list: TStrings): Int64;
    procedure SetDatabase(const Value: TMDODatabase);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Start;
    procedure Stop;
    property Buffers: Int64 read FBuffers;
    property Database: TMDODatabase read FDatabase write SetDatabase;
    property DeltaMem: Int64 read FDeltaMem;
    property Fetches: Int64 read FFetches;
    property ReadIdx: Int64 read FReadIdx;
    property Reads: Int64 read FReads;
    property SeqReads: Int64 read FSeqReads;
    property StartingMem: Int64 read FStartingMem;
    property Writes: Int64 read FWrites;
  end;
  
  TMDOScript = class (TComponent)
  private
    FAutoDDL: Boolean;
    FCharSet: string;
    FContinue: Boolean;
    FCurrentStmt: TMDOParseKind;
    FDatabase: TMDODatabase;
    FDataset: TMDODataset;
    FDDLQuery: TMDOSQL;
    FDDLTransaction: TMDOTransaction;
    FDMLQuery: TMDOSQL;
    FExecuting: Boolean;
    FOnError: TMDOSQLParseError;
    FOnExecuteError: TMDOSQLExecuteError;
    FOnParamCheck: TMDOScriptParamCheck;
    FOnParse: TMDOSQLParseStmt;
    FOnStatement: TMDOSQLParseBeforeStmt;
    FSQLDialect: Integer;
    FSQLParser: TMDOSQLParser;
    FStats: TMDOScriptStats;
    FStatsOn: Boolean;
    FTerminator: string;
    FTransaction: TMDOTransaction;
    FValidate: Boolean;
    FValidating: Boolean;
    function GetPaused: Boolean;
    function GetScript: TStrings;
    function GetSQLParams: TMDOXSQLDA;
    function GetTokens: TStrings;
    procedure SetDatabase(const Value: TMDODatabase);
    procedure SetPaused(const Value: Boolean);
    procedure SetScript(const Value: TStrings);
    procedure SetStatsOn(const Value: Boolean);
    procedure SetTerminator(const Value: string);
    procedure SetTransaction(const Value: TMDOTransaction);
    procedure SetupNewConnection;
    function StripQuote(const Text: string): string;
  protected
    procedure DoConnect(const SQLText: string); virtual;
    procedure DoCreate(const SQLText: string); virtual;
    procedure DoDDL(const Text: string); virtual;
    procedure DoDML(const Text: string); virtual;
    procedure DoReconnect; virtual;
    procedure DoSET(const Text: string); virtual;
    procedure DropDatabase(const SQLText: string); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); 
            override;
    procedure ParserError(Sender: TObject; Error, SQLText: string; LineIndex: 
            Integer);
    procedure ParserParse(Sender: TObject; AKind: TMDOParseKind; SQLText: 
            string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteScript;
    function ParamByName(Idx: string): TMDOXSQLVAR;
    function ValidateScript: Boolean;
    property CurrentTokens: TStrings read GetTokens;
    property Params: TMDOXSQLDA read GetSQLParams;
    property Paused: Boolean read GetPaused write SetPaused;
    property Stats: TMDOScriptStats read FStats;
    property Validating: Boolean read FValidating;
  published
    property AutoDDL: Boolean read FAutoDDL write FAutoDDL default true;
    property Database: TMDODatabase read FDatabase write SetDatabase;
    property Dataset: TMDODataset read FDataset write FDataset;
    property OnExecuteError: TMDOSQLExecuteError read FOnExecuteError write 
            FOnExecuteError;
    property OnParamCheck: TMDOScriptParamCheck read FOnParamCheck write 
            FOnParamCheck;
    property OnParse: TMDOSQLParseStmt read FOnParse write FOnParse;
    property OnParseError: TMDOSQLParseError read FOnError write FOnError;
    property OnStatement: TMDOSQLParseBeforeStmt read FOnStatement write
            FOnStatement;
    property Script: TStrings read GetScript write SetScript;
    property Statistics: Boolean read FStatsOn write SetStatsOn default true;
    property Terminator: string read FTerminator write SetTerminator;
    property Transaction: TMDOTransaction read FTransaction write 
            SetTransaction;
  end;
  
implementation

uses MDOUtils, MDO;

const
  QUOTE = '''';
  DBL_QUOTE = '"';

  { TMDOSQLParser }

{
******************************** TMDOSQLParser *********************************
}
constructor TMDOSQLParser.Create(AOwner: TComponent);
begin
  inherited;
  FScript := TStringList.Create;
  FTokens := TStringList.Create;
  FInput := TStringList.Create;
  ImportIndex := 0;
  FTerminator := ';'; {do not localize}
end;

destructor TMDOSQLParser.Destroy;
begin
  FScript.Free;
  FTokens.Free;
  FInput.Free;
  inherited;
end;

function TMDOSQLParser.AppendNextLine: Boolean;
var
  FStrings: TStrings;
  FIndex: ^Integer;
begin
  if (FInput.Count > ImportIndex) then
  begin
    InInput := true;
    FStrings := FInput;
    FIndex := @ImportIndex;
  end
  else
  begin
    InInput := false;
    FStrings := FScript;
    FIndex := @ScriptIndex;
  end;
  
  if FIndex^ = FStrings.Count then
    Result := false
  else
  begin
    Result := true;
    repeat
      FWork := FWork + CRLF + FStrings[FIndex^];
      Inc(FIndex^);
    until (FIndex^ = FStrings.Count) or
      (Trim(FWork) <> '');
  end;
end;

procedure TMDOSQLParser.DoOnError(Error: string; SQLText: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, Error, SQLText, LineIndex);
end;

procedure TMDOSQLParser.DoOnParse(AKind: TMDOParseKind; SQLText: string);
begin
  if Assigned(FOnParse) then
    FOnParse(Self, AKind, SQLText);
end;

procedure TMDOSQLParser.DoOnStatement(AKind: TMDOParseKind;
  var DoStatement: Boolean);
begin
  if Assigned(FOnStatement) then
    FOnStatement(Self, AKind, DoStatement);
end;

procedure TMDOSQLParser.DoParser;
var
  Stmt: TMDOParseKind;
  Statement: string;
  i: Integer;
begin
  while ((ScriptIndex < FScript.Count) or
    (Trim(FWork) <> '') or
    (ImportIndex < FInput.Count)) and
    not FPaused do
  begin
    Statement := TokenizeNextLine;
    Stmt := IsValidStatement;
    case Stmt of
      stmtERR:
        DoOnError('Invalid statement', Statement);
      stmtTERM:
        begin
          DoOnParse(Stmt, FTokens[2]);
          FTerminator := FTokens[2];
        end;
      stmtINPUT:
        try
          LoadInput;
        except
          on E: Exception do
            DoOnError(E.Message, Statement);
        end;
      stmtEMPTY:
        Continue;
      stmtSET:
        begin
          Statement := '';
          for i := 1 to FTokens.Count - 1 do
            Statement := Statement + FTokens[i] + ' ';
          Statement := TrimRight(Statement);
          DoOnParse(Stmt, Statement);
        end;
    else
      DoOnParse(stmt, Statement);
    end;
  end;
end;

function TMDOSQLParser.IsValidStatement: TMDOParseKind;
var
  Token, Token1: string;
begin
  if FTokens.Count = 0 then
  begin
    Result := stmtEmpty;
    Exit;
  end;
  Token := AnsiUpperCase(FTokens[0]);
  if Token = 'COMMIT' then {do not localize}
  begin
    Result := stmtCOMMIT;
    exit;
  end;
  if Token = 'ROLLBACK' then {do not localize}
  begin
    Result := stmtROLLBACK;
    Exit;
  end;
  if Token = 'RECONNECT' then
  begin
    Result := stmtReconnect;
    Exit;
  end;
  Token1 := AnsiUpperCase(FTokens[1]);
  if FTokens.Count < 2 then
  begin
    Result := stmtERR;
    Exit;
  end;
  if (Token = 'INSERT') or (Token = 'DELETE') or {do not localize}
  (Token = 'SELECT') or (Token = 'UPDATE') or {do not localize}
  (Token = 'EXECUTE') or {do not localize}
  ((Token = 'EXECUTE') and (Token1 = 'PROCEDURE')) then {do not localize}
    Result := stmtDML
  else if Token = 'INPUT' then {do not localize}
    Result := stmtINPUT
  else if Token = 'CONNECT' then {do not localize}
    Result := stmtCONNECT
  else if (Token = 'CREATE') and
    ((Token1 = 'DATABASE') or (Token1 = 'SCHEMA')) then {do not localize}
    Result := stmtCREATE
  else if (Token = 'DROP') and (Token1 = 'DATABASE') then {do not localize}
    Result := stmtDROP
  else if (Token = 'DECLARE') or (Token = 'CREATE') or (Token = 'ALTER') or
    {do not localize}
  (Token = 'GRANT') or (Token = 'REVOKE') or (Token = 'DROP') or
    {do not localize}
  ((Token = 'SET') and ((Token1 = 'GENERATOR'))) then {do not localize}
    Result := stmtDDL
  else if (Token = 'SET') then {do not localize}
  begin
    if (Token1 = 'TERM') then {do not localize}
      if FTokens.Count = 3 then
        Result := stmtTERM
      else
        Result := stmtERR
    else if (Token1 = 'SQL') then {do not localize}
      if (FTokens.Count = 4) and
        (AnsiUpperCase(FTokens[2]) = 'DIALECT') then {do not localize}
        Result := stmtSET
      else
        Result := stmtERR
    else if (Token1 = 'AUTODDL') or (Token1 = 'STATISTICS') or {do not localize}
    (Token1 = 'NAMES') then {do not localize}
      if FTokens.Count = 3 then
        Result := stmtSET
      else
        Result := stmtERR
    else
      Result := stmtERR;
  end
  else
    Result := stmtERR;
end;

procedure TMDOSQLParser.LoadInput;
var
  FileName: string;
begin
  FInput.Clear;
  ImportIndex := 0;
  FileName := FTokens[1];
  if FileName[1] in [QUOTE, DBL_QUOTE] then
    Delete(FileName, 1, 1);
  if FileName[Length(FileName)] in [QUOTE, DBL_QUOTE] then
    Delete(FileName, Length(FileName), 1);
  
  FInput.LoadFromFile(FileName);
end;

procedure TMDOSQLParser.Parse;
begin
  ScriptIndex := 0;
  ImportIndex := 0;
  FInput.Clear;
  FPaused := false;
  DoParser;
end;

procedure TMDOSQLParser.RemoveComment;
var
  Start, Ending: Integer;
begin
  FWork := TrimLeft(FWork);
  Start := AnsiPos('/*', FWork); {do not localize}
  while Start = 1 do
  begin
    Ending := AnsiPos('*/', FWork); {do not localize}
    while Ending < Start do
    begin
      if AppendNextLine = false then
        raise Exception.Create('Invalid Comment');
      Ending := AnsiPos('*/', FWork); {do not localize}
    end;
    Delete(FWork, Start, Ending - Start + 2);
    FWork := TrimLeft(FWork);
    if FWork = '' then
      AppendNextLine;
    FWork := TrimLeft(FWork);
    Start := AnsiPos('/*', FWork); {do not localize}
  end;
  FWork := TrimLeft(FWork);
end;

procedure TMDOSQLParser.SetPaused(const Value: Boolean);
begin
  if FPaused <> Value then
  begin
    FPaused := Value;
    if not FPaused then
      DoParser;
  end;
end;

procedure TMDOSQLParser.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
  FPaused := false;
  ScriptIndex := 0;
  ImportIndex := 0;
  FInput.Clear;
end;

function TMDOSQLParser.TokenizeNextLine: string;
var
  InQuote, InDouble, InComment, Done: Boolean;
  NextWord: string;
  Index: Integer;
  
  procedure ScanToken;
  var
    SDone: Boolean;
  begin
    NextWord := '';
    SDone := false;
    Index := 1;
    while (Index <= Length(FWork)) and (not SDone) do
    begin
      { Hit the terminator, but it is not embedded in a single or double quote
          or inside a comment }
      if ((not InQuote) and (not InDouble) and (not InComment)) and
        (CompareStr(FTerminator, Copy(FWork, Index, Length(FTerminator))) = 0)
          then
      begin
        Done := true;
        Result := Result + NextWord;
        Delete(FWork, 1, Length(NextWord) + Length(FTerminator));
        NextWord := Trim(AnsiUpperCase(NextWord));
        if NextWord <> '' then
          FTokens.Add(AnsiUpperCase(NextWord));
        Exit;
      end;
  
      { Are we entering or exiting an inline comment? }
      if (Index < Length(FWork)) and ((not Indouble) or (not InQuote)) and
        (FWork[Index] = '/') and (FWork[Index + 1] = '*') then {do not localize}
        InComment := true;
      if InComment and (Index <> 1) and
        (FWork[Index] = '/') and (FWork[Index - 1] = '*') then {do not localize}
        InComment := false;
  
      if not InComment then
        { Handle case when the character is a single quote or a double quote }
        case FWork[Index] of
          QUOTE:
            if not InDouble then
            begin
              if InQuote then
              begin
                InQuote := false;
                SDone := true;
              end
              else
                InQuote := true;
            end;
          DBL_QUOTE:
            if not InQuote then
            begin
              if InDouble then
              begin
                Indouble := false;
                SDone := true;
              end
              else
                InDouble := true;
            end;
          ' ': {do not localize}
            if (not InDouble) and (not InQuote) then
              SDone := true;
        end;
      NextWord := NextWord + FWork[Index];
      Inc(Index);
    end;
    { copy over the remaining non character or spaces until the next word }
    while (Index <= Length(FWork)) and (FWork[Index] <= #32) do
    begin
      NextWord := NextWord + FWork[Index];
      Inc(Index);
    end;
    Result := Result + NextWord;
    Delete(FWork, 1, Length(NextWord));
    NextWord := Trim(NextWord);
    if NextWord <> '' then
      FTokens.Add(NextWord);
  end;
  
begin
  FTokens.Clear;
  if Trim(FWork) = '' then
    AppendNextLine;
  if not InInput then
    LineIndex := ScriptIndex;
  try
    RemoveComment;
  except
    on E: Exception do
    begin
      DoOnError(E.Message, '');
      exit;
    end
  end;
  InQuote := false;
  InDouble := false;
  InComment := false;
  Done := false;
  Result := '';
  while not Done do
  begin
    { Check the work queue, if it is empty get the next line to process }
    if FWork = '' then
      if not AppendNextLine then
        exit;
    ScanToken;
  end;
end;

{ Note on TokenizeNextLine.  This is not intended to actually tokenize in
  terms of SQL tokens.  It has two goals.  First is to get the primary statement
  type in FTokens[0].  These are items like SELECT, UPDATE, CREATE, SET, IMPORT.
  The secondary function is to correctly find the end of a statement.  So if the
  terminator is ; and the statement is "SELECT 'FDR'';' from Table1;" while
  correct SQL tokenization is SELECT, 'FDR'';', FROM, Table1 but this is more
  than needed.  The Tokenizer will tokenize this as SELECT, 'FDR', ';', FROM,
  Table1.  We get that it is a SELECT statement and get the correct termination
  and whole statement in the case where the terminator is embedded inside
  a ' or ". }

{ TMDOScript }

{
********************************** TMDOScript **********************************
}
constructor TMDOScript.Create(AOwner: TComponent);
begin
  inherited;
  FSQLParser := TMDOSQLParser.Create(self);
  FSQLParser.OnError := ParserError;
  FSQLParser.OnParse := ParserParse;
  Terminator := ';'; {do not localize}
  FDDLTransaction := TMDOTransaction.Create(self);
  FDDLQuery := TMDOSQL.Create(self);
  FDDLQuery.ParamCheck := false;
  FAutoDDL := true;
  FStatsOn := true;
  FStats := TMDOScriptStats.Create;
  FStats.Database := FDatabase;
  FSQLDialect := 3;
end;

destructor TMDOScript.Destroy;
begin
  FStats.Free;
  inherited;
end;

procedure TMDOScript.DoConnect(const SQLText: string);
var
  i: Integer;
  Param: string;
begin
  SetupNewConnection;
  if Database.Connected then
    Database.Connected := false;
  Database.SQLDialect := FSQLDialect;
  Database.Params.Clear;
  Database.DatabaseName := StripQuote(FSQLParser.CurrentTokens[1]);
  i := 2;
  while i < FSQLParser.CurrentTokens.Count - 1 do
  begin
    if AnsiCompareText(FSQLParser.CurrentTokens[i], 'USER') = 0 then
      {do not localize}
      Param := 'user_name'; {do not localize}
    if AnsiCompareText(FSQLParser.CurrentTokens[i], 'PASSWORD') = 0 then
      {do not localize}
      Param := 'password'; {do not localize}
    if AnsiCompareText(FSQLParser.CurrentTokens[i], 'ROLE') = 0 then
      {do not localize}
      Param := 'user_role'; {do not localize}
    Database.Params.Add(Param + '=' + StripQuote(FSQLParser.CurrentTokens[i +
      1]));
    Inc(i, 2);
  end;
  if FCharSet <> '' then
    Database.Params.Add('lc_ctype=' + FCharSet); {do not localize}
  Database.Connected := true;
end;

procedure TMDOScript.DoCreate(const SQLText: string);
var
  i: Integer;
begin
  SetupNewConnection;
  FDatabase.DatabaseName := StripQuote(FSQLParser.CurrentTokens[2]);
  i := 3;
  while i < FSQLParser.CurrentTokens.Count - 1 do
  begin
    Database.Params.Add(FSQLParser.CurrentTokens[i] + ' ' +
      FSQLParser.CurrentTokens[i + 1]);
    Inc(i, 2);
  end;
  FDatabase.SQLDialect := FSQLDialect;
  FDatabase.CreateDatabase;
  if FStatsOn and Assigned(FDatabase) and FDatabase.Connected then
    FStats.Start;
end;

procedure TMDOScript.DoDDL(const Text: string);
begin
  if AutoDDL then
    FDDLQuery.Transaction := FDDLTransaction
  else
    FDDLQuery.Transaction := FTransaction;
  if not FDDLQuery.Transaction.InTransaction then
    FDDLQuery.Transaction.StartTransaction;
  
  FDDLQuery.SQL.Text := Text;
  FDDLQuery.ExecQuery;
  if AutoDDL then
    FDDLTransaction.Commit;
end;

procedure TMDOScript.DoDML(const Text: string);
var
  FPaused: Boolean;
begin
  FPaused := false;
  if Assigned(FDataSet) then
  begin
    if FDataSet.Active then
      FDataSet.Close;
    FDataSet.SelectSQL.Text := Text;
    FDataset.Prepare;
    if (FDataSet.Params.Count <> 0) and Assigned(FOnParamCheck) then
    begin
      FOnParamCheck(self, FPaused);
      if FPaused then
      begin
        FSQLParser.Paused := true;
        exit;
      end;
    end;
    if TMDOSQL(FDataset).SQLType = SQLSelect then
      FDataSet.Open
    else
      FDataset.ExecSQL;
  end
  else
  begin
    if FDMLQuery.Open then
      FDMLQuery.Close;
    FDMLQuery.SQL.Text := Text;
    if not FDMLQuery.Transaction.InTransaction then
      FDMLQuery.Transaction.StartTransaction;
    FDMLQuery.Prepare;
    if (FDMLQuery.Params.Count <> 0) and Assigned(FOnParamCheck) then
    begin
      FOnParamCheck(self, FPaused);
      if FPaused then
      begin
        FSQLParser.Paused := true;
        exit;
      end;
    end;
    FDMLQuery.ExecQuery;
  end;
end;

procedure TMDOScript.DoReconnect;
begin
  if Assigned(FDatabase) then
  begin
    FDatabase.Connected := false;
    FDatabase.Connected := true;
  end;
end;

procedure TMDOScript.DoSET(const Text: string);
begin
  if AnsiCompareText('AUTODDL', FSQLParser.CurrentTokens[1]) = 0 then
    {do not localize}
    FAutoDDL := FSQLParser.CurrentTokens[2] = 'ON' {do not localize}
  else if AnsiCompareText('STATISTICS', FSQLParser.CurrentTokens[1]) = 0 then
    {do not localize}
    Statistics := FSQLParser.CurrentTokens[2] = 'ON' {do not localize}
  else if (AnsiCompareText('SQL', FSQLParser.CurrentTokens[1]) = 0) and
    {do not localize}
  (AnsiCompareText('DIALECT', FSQLParser.CurrentTokens[2]) = 0) then
    {do not localize}
  begin
    FSQLDialect := StrToInt(FSQLParser.CurrentTokens[3]);
    if Database.SQLDialect <> FSQLDialect then
    begin
      if Database.Connected then
      begin
        Database.Close;
        Database.SQLDialect := FSQLDialect;
        Database.Open;
      end
      else
        Database.SQLDialect := FSQLDialect;
    end;
  end
  else if (AnsiCompareText('NAMES', FSQLParser.CurrentTokens[1]) = 0) then
    {do not localize}
    FCharSet := FSQLParser.CurrentTokens[2];
end;

procedure TMDOScript.DropDatabase(const SQLText: string);
begin
  FDatabase.DropDatabase;
end;

procedure TMDOScript.ExecuteScript;
begin
  FContinue := true;
  FExecuting := true;
  FCharSet := '';
  if not Assigned(FDataset) then
    FDMLQuery := TMDOSQL.Create(FDatabase);
  try
    FStats.Clear;
    if FStatsOn and Assigned(FDatabase) and FDatabase.Connected then
      FStats.Start;
    FSQLParser.Parse;
    if FStatsOn then
      FStats.Stop;
  finally
    FExecuting := false;
    if Assigned(FDMLQuery) then
      FreeAndNil(FDMLQuery);
  end;
end;

function TMDOScript.GetPaused: Boolean;
begin
  Result := FSQLParser.Paused;
end;

function TMDOScript.GetScript: TStrings;
begin
  Result := FSQLParser.Script;
end;

function TMDOScript.GetSQLParams: TMDOXSQLDA;
begin
  if Assigned(FDataset) then
    Result := FDataset.Params
  else
    Result := FDMLQuery.Params;
end;

function TMDOScript.GetTokens: TStrings;
begin
  Result := FSQLParser.CurrentTokens;
end;

procedure TMDOScript.Notification(AComponent: TComponent; Operation: 
        TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FDataset then
      FDataset := nil
    else if AComponent = FDatabase then
      FDatabase := nil
    else if AComponent = FTransaction then
      FTransaction := nil;
  end;
end;

function TMDOScript.ParamByName(Idx: string): TMDOXSQLVAR;
begin
  if Assigned(FDataset) then
    Result := TMDOXSQLVAR(TMDOQuery(FDataset).ParamByName(Idx))
  else
    Result := TMDOXSQLVAR(TMDOQuery(FDMLQuery).ParamByName(Idx));
end;

procedure TMDOScript.ParserError(Sender: TObject; Error, SQLText: string; 
        LineIndex: Integer);
begin
  if Assigned(FOnError) then
    FOnError(Self, Error, SQLText, LineIndex);
  FValidate := false;
  FSQLParser.Paused := true;
end;

procedure TMDOScript.ParserParse(Sender: TObject; AKind: TMDOParseKind;
        SQLText: string);
var
  DoStatement: Boolean;
begin
  try
    FCurrentStmt := AKind;
    DoStatement := true;
    if Assigned(FOnStatement) then
      FOnStatement(self, AKind, DoStatement);
    if not FValidating and DoStatement then
      case AKind of
        stmtDrop: DropDatabase(SQLText);
        stmtDDL: DoDDL(SQLText);
        stmtDML: DoDML(SQLText);
        stmtSET: DoSET(SQLText);
        stmtCONNECT: DoConnect(SQLText);
        stmtCREATE: DoCreate(SQLText);
        stmtTERM: FTerminator := Trim(SQLText);
        stmtCOMMIT:
          if FTransaction.InTransaction then
            FTransaction.Commit;
        stmtROLLBACK:
          if FTransaction.InTransaction then
            FTransaction.Rollback;
        stmtReconnect:
          DoReconnect;
      end;
    if Assigned(FOnParse) then
      FOnParse(self, AKind, SQLText);
  except
    on E: EMDOError do
    begin
      FContinue := false;
      FValidate := false;
      FSQLParser.Paused := true;
      if Assigned(FOnExecuteError) then
        FOnExecuteError(Self, E.Message, SQLText, FSQLParser.CurrentLine,
          FContinue)
      else
        raise;
      if FContinue then
        FSQLParser.Paused := false;
    end;
  end;
end;

procedure TMDOScript.SetDatabase(const Value: TMDODatabase);
begin
  if FDatabase <> Value then
  begin
    FDatabase := Value;
    FDDLQuery.Database := Value;
    FDDLTransaction.DefaultDatabase := Value;
    FStats.Database := Value;
    if Assigned(FDMLQuery) then
      FDMLQuery.Database := Value;
  end;
end;

procedure TMDOScript.SetPaused(const Value: Boolean);
begin
  if FSQLParser.Paused and (FCurrentStmt = stmtDML) then
    if Assigned(FDataSet) then
    begin
      if TMDOSQL(FDataset).SQLType = SQLSelect then
        FDataSet.Open
      else
        FDataset.ExecSQL;
    end
    else
    begin
      FDMLQuery.ExecQuery;
    end;
  FSQLParser.Paused := Value;
end;

procedure TMDOScript.SetScript(const Value: TStrings);
begin
  FSQLParser.Script.Assign(Value);
end;

procedure TMDOScript.SetStatsOn(const Value: Boolean);
begin
  if FStatsOn <> Value then
  begin
    FStatsOn := Value;
    if FExecuting then
    begin
      if FStatsOn then
        FStats.Start
      else
        FStats.Stop;
    end;
  end;
end;

procedure TMDOScript.SetTerminator(const Value: string);
begin
  if FTerminator <> Value then
  begin
    FTerminator := Value;
    FSQLParser.Terminator := Value;
  end;
end;

procedure TMDOScript.SetTransaction(const Value: TMDOTransaction);
begin
  FTransaction := Value;
  if Assigned(FDMLQuery) then
    FDMLQuery.Transaction := Value;
end;

procedure TMDOScript.SetupNewConnection;
begin
  FDDLTransaction.RemoveDatabase(FDDLTransaction.FindDatabase(FDatabase));
  if FDatabase.Owner = self then
    FDatabase.Free;
  Database := TMDODatabase.Create(self);
  if FTransaction.Owner = self then
    FTransaction.Free;
  FTransaction := TMDOTransaction.Create(self);
  FDatabase.DefaultTransaction := FTransaction;
  FTransaction.DefaultDatabase := FDatabase;
  FDDLTransaction.DefaultDatabase := FDatabase;
  FDDLQuery.Database := FDatabase;
  if Assigned(FDataset) then
  begin
    FDataset.Database := FDatabase;
    FDataset.Transaction := FTransaction;
  end;
end;

function TMDOScript.StripQuote(const Text: string): string;
begin
  Result := Text;
  if Result[1] in [Quote, DBL_QUOTE] then
  begin
    Delete(Result, 1, 1);
    Delete(Result, Length(Result), 1);
  end;
end;

function TMDOScript.ValidateScript: Boolean;
begin
  FValidating := true;
  FValidate := true;
  FSQLParser.Parse;
  Result := FValidate;
  FValidating := false;
end;

{ TMDOScriptStats }

{
******************************* TMDOScriptStats ********************************
}
constructor TMDOScriptStats.Create;
begin
  FInfoStats := TMDODatabaseInfo.Create(nil);
end;

destructor TMDOScriptStats.Destroy;
begin
  FInfoStats.Destroy;
  inherited;
end;

function TMDOScriptStats.AddStringValues(list: TStrings): Int64;
var
  i: Integer;
  index: Integer;
begin
  try
    Result := 0;
    for i := 0 to list.count - 1 do
    begin
      index := Pos('=', list.strings[i]); {do not localize}
      if index > 0 then
        Result := Result + StrToInt(Copy(list.strings[i], index + 1, 255));
    end;
  except
    Result := 0;
  end;
end;

procedure TMDOScriptStats.Clear;
begin
  FBuffers := 0;
  FReads := 0;
  FWrites := 0;
  FSeqReads := 0;
  FFetches := 0;
  FReadIdx := 0;
  FDeltaMem := 0;
end;

procedure TMDOScriptStats.SetDatabase(const Value: TMDODatabase);
begin
  FDatabase := Value;
  FInfoStats.Database := Value;
end;

procedure TMDOScriptStats.Start;
begin
  FStartBuffers := FInfoStats.NumBuffers;
  FStartReads := FInfoStats.Reads;
  FStartWrites := FInfoStats.Writes;
  FStartSeqReads := AddStringValues(FInfoStats.ReadSeqCount);
  FStartFetches := FInfoStats.Fetches;
  FStartReadIdx := AddStringValues(FInfoStats.ReadIdxCount);
  FStartingMem := FInfoStats.CurrentMemory;
end;

procedure TMDOScriptStats.Stop;
begin
  FBuffers := FInfoStats.NumBuffers - FStartBuffers + FBuffers;
  FReads := FInfoStats.Reads - FStartReads + FReads;
  FWrites := FInfoStats.Writes - FStartWrites + FWrites;
  FSeqReads := AddStringValues(FInfoStats.ReadSeqCount) - FStartSeqReads +
    FSeqReads;
  FReadIdx := AddStringValues(FInfoStats.ReadIdxCount) - FStartReadIdx +
    FReadIdx;
  FFetches := FInfoStats.Fetches - FStartFetches + FFetches;
  FDeltaMem := FInfoStats.CurrentMemory - FStartingMem + FDeltaMem;
end;

end.

