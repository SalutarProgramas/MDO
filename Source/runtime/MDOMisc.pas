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

  Este componente encapsula várias funções utilitárias para
  obtenção de informações como data, hora, nome do usuário,
  valor de generator, etc. Outras funções serão adicionadas
  no futuro.
  O objetivo é evitar que o programador tenha que manipular o
  MDOSQL diretamente para obter informações simples.
  --
  This component encapsulates several useful functions to
  catch information such as date, time, user name, generator
  value, etc. Others methods functions will be added.
  The goal is to prevent the programmer has that handle the
  MDOSQL component directly to catch simple informations.

  }

unit MDOMisc;

interface

uses
  Classes, MDODatabase, MDOSQL, MDO, SysUtils;

type
  TMDOMisc = class (TComponent)
  private
    FDatabase: TMDODatabase;
    FQuery: TMDOSQL;
    FTransaction: TMDOTransaction;
    procedure CheckDatabase;
    procedure CheckQuery;
    procedure CheckTransaction;
    function EncodeSelect(const FieldName, TableName, Filter: string): string;
    function GetDate: TDateTime;
    function GetDateTime: TDateTime;
    function GetTime: TDateTime;
    function GetUserName: string;
    procedure SetDatabase(Value: TMDODatabase);
    procedure SetTransaction(Value: TMDOTransaction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AsCurrency(const FieldName, TableName, Filter: string): Currency;
    function AsDate(const FieldName, TableName, Filter: string): TDateTime;
    function AsDateTime(const FieldName, TableName, Filter: string): TDateTime;
    function AsFloat(const FieldName, TableName, Filter: string): Double;
    function AsInt64(const FieldName, TableName, Filter: string): Int64;
    function AsInteger(const FieldName, TableName, Filter: string): Integer;
    function AsString(const FieldName, TableName, Filter: string): string;
    function AsTime(const FieldName, TableName, Filter: string): TDateTime;
    function AvgAsCurrency(const FieldName, TableName, Filter: string): 
            Currency;
    function AvgAsFloat(const FieldName, TableName, Filter: string): Double;
    function AvgAsInt64(const FieldName, TableName, Filter: string): Int64;
    function AvgAsInteger(const FieldName, TableName, Filter: string): Integer;
    function Count(const FieldName, TableName, Filter: string): Integer;
    function Execute(const SQL: string): Integer;
    function GetGenerator(const GenName: string; const Increment: integer): 
            Integer;
    function MaxAsCurrency(const FieldName, TableName, Filter: string): 
            Currency;
    function MaxAsDate(const FieldName, TableName, Filter: string): TDateTime;
    function MaxAsDateTime(const FieldName, TableName, Filter: string): 
            TDateTime;
    function MaxAsFloat(const FieldName, TableName, Filter: string): Double;
    function MaxAsInt64(const FieldName, TableName, Filter: string): Int64;
    function MaxAsInteger(const FieldName, TableName, Filter: string): Integer;
    function MaxAsString(const FieldName, TableName, Filter: string): string;
    function MaxAsTime(const FieldName, TableName, Filter: string): TDateTime;
    function MinAsCurrency(const FieldName, TableName, Filter: string): 
            Currency;
    function MinAsDate(const FieldName, TableName, Filter: string): TDateTime;
    function MinAsDateTime(const FieldName, TableName, Filter: string): 
            TDateTime;
    function MinAsFloat(const FieldName, TableName, Filter: string): Double;
    function MinAsInt64(const FieldName, TableName, Filter: string): Int64;
    function MinAsInteger(const FieldName, TableName, Filter: string): Integer;
    function MinAsString(const FieldName, TableName, Filter: string): string;
    function MinAsTime(const FieldName, TableName, Filter: string): TDateTime;
    procedure SetGenerator(const GenName: string; const Value: integer);
    function SumAsCurrency(const FieldName, TableName, Filter: string): 
            Currency;
    function SumAsFloat(const FieldName, TableName, Filter: string): Double;
    function SumAsInt64(const FieldName, TableName, Filter: string): Int64;
    function SumAsInteger(const FieldName, TableName, Filter: string): Integer;
    property Date: TDateTime read GetDate;
    property DateTime: TDateTime read GetDateTime;
    property Time: TDateTime read GetTime;
    property UserName: string read GetUserName;
  published
    property Database: TMDODatabase read FDatabase write SetDatabase;
    property Transaction: TMDOTransaction read FTransaction write 
            SetTransaction;
  end;
  
implementation

resourcestring

  { Do not localize }
  SDate     = 'CURRENT_DATE';
  SDateTime = 'CURRENT_TIMESTAMP';
  STime     = 'CURRENT_TIME';
  SUserName = 'CURRENT_USER';
  SDBTable  = 'RDB$DATABASE';

{ Private }

{
*********************************** TMDOMisc ***********************************
}
constructor TMDOMisc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuery := TMDOSQL.Create(nil);
end;

destructor TMDOMisc.Destroy;
begin
  FQuery.Free;
  inherited;
end;

function TMDOMisc.AsCurrency(const FieldName, TableName, Filter: string): 
        Currency;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsCurrency
    else
      Result := 0;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AsDate(const FieldName, TableName, Filter: string): TDateTime;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsDate
    else
      Result := 0;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AsDateTime(const FieldName, TableName, Filter: string): 
        TDateTime;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsDateTime
    else
      Result := 0;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AsFloat(const FieldName, TableName, Filter: string): Double;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsFloat
    else
      Result := 0;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AsInt64(const FieldName, TableName, Filter: string): Int64;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsInt64
    else
      Result := 0;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AsInteger(const FieldName, TableName, Filter: string): 
        Integer;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsInteger
    else
      Result := 0;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AsString(const FieldName, TableName, Filter: string): string;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsString
    else
      Result := '';
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AsTime(const FieldName, TableName, Filter: string): TDateTime;
begin
  CheckQuery;
  FQuery.SQL.Text := EncodeSelect(FieldName, TableName, Filter);
  FQuery.ExecQuery;
  try
    if not FQuery.EOF then
      Result := FQuery.Fields[0].AsTime
    else
      Result := 0;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.AvgAsCurrency(const FieldName, TableName, Filter: string): 
        Currency;
begin
  Result := AsCurrency('AVG(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.AvgAsFloat(const FieldName, TableName, Filter: string): 
        Double;
begin
  Result := AsFloat('AVG(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.AvgAsInt64(const FieldName, TableName, Filter: string): Int64;
begin
  Result := AsInt64('AVG(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.AvgAsInteger(const FieldName, TableName, Filter: string): 
        Integer;
begin
  Result := AsInteger('AVG(' + FieldName + ')', TableName, Filter);
end;

procedure TMDOMisc.CheckDatabase;
begin
  if FDatabase = nil then
    MDOError(mdoeDatabaseNotAssigned, [nil]);
end;

procedure TMDOMisc.CheckQuery;
begin
  CheckDatabase;
  CheckTransaction;
  FQuery.Database := FDatabase;
  FQuery.Transaction := FTransaction;
end;

procedure TMDOMisc.CheckTransaction;
begin
  if FTransaction = nil then
    MDOError(mdoeTransactionNotAssigned, [nil]);
end;

function TMDOMisc.Count(const FieldName, TableName, Filter: string): Integer;
var
  S: string;
begin
  if FieldName = '' then
    S := '*'
  else
    S := FieldName;
  
  Result := AsInteger('COUNT(' + S + ')', TableName, Filter);
end;

function TMDOMisc.EncodeSelect(const FieldName, TableName, Filter: string): 
        string;
begin
  Result := 'SELECT ' + FieldName + ' FROM ' + TableName;
  if Filter <> '' then
    Result := Result + ' WHERE ' + Filter;
end;

function TMDOMisc.Execute(const SQL: string): Integer;
begin
  CheckQuery;
  FQuery.SQL.Text := SQL;
  FQuery.ExecQuery;
  try
    Result := FQuery.RowsAffected;
  finally
    FQuery.Close;
  end;
end;

function TMDOMisc.GetDate: TDateTime;
begin
  Result := AsDate(SDate, SDBTable, '');
end;

function TMDOMisc.GetDateTime: TDateTime;
begin
  Result := AsDateTime(SDateTime, SDBTable, '');
end;

function TMDOMisc.GetGenerator(const GenName: string; const Increment: 
        integer): Integer;
begin
  Result := integer(AsInt64(
    'GEN_ID(' + GenName + ',' + IntToStr(Increment) + ')', SDBTable, ''));
end;

function TMDOMisc.GetTime: TDateTime;
begin
  Result := AsTime(STime, SDBTable, '');
end;

function TMDOMisc.GetUserName: string;
begin
  Result := AsString(SUserName, SDBTable, '');
end;

function TMDOMisc.MaxAsCurrency(const FieldName, TableName, Filter: string): 
        Currency;
begin
  Result := AsCurrency('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MaxAsDate(const FieldName, TableName, Filter: string): 
        TDateTime;
begin
  Result := AsDate('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MaxAsDateTime(const FieldName, TableName, Filter: string): 
        TDateTime;
begin
  Result := AsDateTime('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MaxAsFloat(const FieldName, TableName, Filter: string): 
        Double;
begin
  Result := AsFloat('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MaxAsInt64(const FieldName, TableName, Filter: string): Int64;
begin
  Result := AsInt64('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MaxAsInteger(const FieldName, TableName, Filter: string): 
        Integer;
begin
  Result := AsInteger('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MaxAsString(const FieldName, TableName, Filter: string): 
        string;
begin
  Result := AsString('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MaxAsTime(const FieldName, TableName, Filter: string): 
        TDateTime;
begin
  Result := AsTime('MAX(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsCurrency(const FieldName, TableName, Filter: string): 
        Currency;
begin
  Result := AsCurrency('MIN(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsDate(const FieldName, TableName, Filter: string): 
        TDateTime;
begin
  Result := AsDate('MIN(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsDateTime(const FieldName, TableName, Filter: string): 
        TDateTime;
begin
  Result := AsDateTime('MIN(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsFloat(const FieldName, TableName, Filter: string): 
        Double;
begin
  Result := AsFloat('MIN(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsInt64(const FieldName, TableName, Filter: string): Int64;
begin
  Result := AsInt64('MIN(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsInteger(const FieldName, TableName, Filter: string): 
        Integer;
begin
  Result := AsInteger('MIN(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsString(const FieldName, TableName, Filter: string): 
        string;
begin
  Result := AsString('MIN(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.MinAsTime(const FieldName, TableName, Filter: string): 
        TDateTime;
begin
  Result := AsTime('MIN(' + FieldName + ')', TableName, Filter);
end;

procedure TMDOMisc.SetDatabase(Value: TMDODatabase);
begin
  FDatabase := Value;
  if (FTransaction = nil) and (FDatabase <> nil) and
     (FDatabase.DefaultTransaction <> nil) then
    FTransaction := FDatabase.DefaultTransaction;
end;

procedure TMDOMisc.SetGenerator(const GenName: string; const Value: integer);
begin
  Execute('SET GENERATOR ' + GenName + ' TO ' + IntToStr(Value))
end;

procedure TMDOMisc.SetTransaction(Value: TMDOTransaction);
begin
  FTransaction := Value;
  if (FDatabase = nil) and (FTransaction <> nil) and
     (FTransaction.DefaultDatabase <> nil) then
    FDatabase := FTransaction.DefaultDatabase;
end;

function TMDOMisc.SumAsCurrency(const FieldName, TableName, Filter: string): 
        Currency;
begin
  Result := AsCurrency('SUM(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.SumAsFloat(const FieldName, TableName, Filter: string): 
        Double;
begin
  Result := AsFloat('SUM(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.SumAsInt64(const FieldName, TableName, Filter: string): Int64;
begin
  Result := AsInt64('SUM(' + FieldName + ')', TableName, Filter);
end;

function TMDOMisc.SumAsInteger(const FieldName, TableName, Filter: string): 
        Integer;
begin
  Result := AsInteger('SUM(' + FieldName + ')', TableName, Filter);
end;

{ Public }


end.
