unit fm_mdoEvent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, MDODatabase, MDOScript, MDOCustomDataSet,
  MDOUpdateSQL, MDOQuery, MDOTable;

type
  TForm2 = class(TForm)
    MDODatabase: TMDODatabase;
    btnTable: TButton;
    MDOTransaction: TMDOTransaction;
    MDOScript: TMDOScript;
    MDOTable: TMDOTable;
    MDOQuery: TMDOQuery;
    MDOUpdateSQL: TMDOUpdateSQL;
    MDODataSet: TMDODataSet;
    edtBefore: TEdit;
    edtAfter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MDOTableCODIGO: TIntegerField;
    MDOTableNOME: TMDOStringField;
    btnQuery: TButton;
    btnDataset: TButton;
    MDOQueryCODIGO: TIntegerField;
    MDOQueryNOME: TMDOStringField;
    procedure FormCreate(Sender: TObject);
    procedure btnTableClick(Sender: TObject);
    procedure MDOTableBeforePost(DataSet: TDataSet);
    procedure MDOTableAfterPost(DataSet: TDataSet);
    procedure btnQueryClick(Sender: TObject);
    procedure MDOQueryBeforePost(DataSet: TDataSet);
    procedure MDOQueryAfterPost(DataSet: TDataSet);
    procedure btnDatasetClick(Sender: TObject);
    procedure MDODataSetBeforePost(DataSet: TDataSet);
    procedure MDODataSetAfterPost(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
var
  s : String;
begin
  s := ParamStr(0) + '.fdb';

  // Cria um banco de dados novo.
  MDODatabase.Close;
  DeleteFile(s);
  MDOdatabase.DatabaseName := s;
  MDOdatabase.Params.Clear;
  MDOdatabase.Params.Add('user "SYSDBA" password "masterkey" page_size 4096');
  MDOdatabase.CreateDatabase;
  MDOdatabase.Close;
  MDOdatabase.Params.Clear;
  MDOdatabase.Params.Add('user_name=SYSDBA');
  MDOdatabase.Params.Add('password=masterkey');
  MDOdatabase.Open;

  // Cria uma tabela
  MDOScript.ExecuteScript;

  MDOTransaction.Commit;
  

end;

procedure TForm2.btnTableClick(Sender: TObject);
begin
  with MDOTable do
  begin
    Open;
    Insert;
    FieldByName('CODIGO').AsInteger := 1;
    FieldByName('NOME').AsString := 'Tabela';
    Post;
  end;
end;

procedure TForm2.MDOTableBeforePost(DataSet: TDataSet);
begin
  edtBefore.Text := 'Before Post de MDOTable';
end;

procedure TForm2.MDOTableAfterPost(DataSet: TDataSet);
begin
  edtAfter.Text := 'After Post de MDOTable';
end;

procedure TForm2.btnQueryClick(Sender: TObject);
begin
  with MDOQuery do
  begin
    Open;
    Insert;
    FieldByName('CODIGO').AsInteger := 2;
    FieldByName('NOME').AsString := 'Query';
    Post;
  end;
end;

procedure TForm2.MDOQueryBeforePost(DataSet: TDataSet);
begin
  edtBefore.Text := 'Before Post de MDOQuery';
end;

procedure TForm2.MDOQueryAfterPost(DataSet: TDataSet);
begin
  edtAfter.Text := 'After Post de MDOQuery';
end;

procedure TForm2.btnDatasetClick(Sender: TObject);
begin
  with MDODataSet do
  begin
    Open;
    Insert;
    FieldByName('CODIGO').AsInteger := 3;
    FieldByName('NOME').AsString := 'Dataset';
    Post;
  end;
end;

procedure TForm2.MDODataSetBeforePost(DataSet: TDataSet);
begin
  edtBefore.Text := 'Before Post de MDODataset';
end;

procedure TForm2.MDODataSetAfterPost(DataSet: TDataSet);
begin
  edtAfter.Text := 'After Post de MDODataset';
end;

end.
