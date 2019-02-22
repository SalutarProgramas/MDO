{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{         Aplicativo de cadastro de nomes e telefones        }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{                                                            }
{************************************************************}
unit uLocalizar;

interface

uses
  Forms, Db, StdCtrls, Grids, DBGrids, Buttons, Controls,
  Classes, MDOCustomDataSet;

type
  TfrmLocalizar = class(TForm)
    DataSource: TDataSource;
    MDODataSet: TMDODataSet;
    GroupBox1: TGroupBox;
    edtNome: TEdit;
    DBGrid1: TDBGrid;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnLocalizar: TButton;
    procedure edtNomeEnter(Sender: TObject);
    procedure edtNomeExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MDODataSetAfterOpen(DataSet: TDataSet);
    procedure btnLocalizarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Localizar(var Codigo: integer): boolean;

implementation

uses uMain;

{$R *.DFM}

function Localizar(var Codigo: integer): boolean;
begin
  with TfrmLocalizar.Create(nil) do
  try
    Result := ShowModal = mrOK;
    if Result then
      Codigo := MDODataSet.FieldByName('Codigo').AsInteger;
  finally
    Free;
  end;
end;

procedure TfrmLocalizar.edtNomeEnter(Sender: TObject);
begin
  btnLocalizar.Default := true;
  btnOk.Default := false;
end;

procedure TfrmLocalizar.edtNomeExit(Sender: TObject);
begin
  btnLocalizar.Default := false;
  btnOk.Default := true;
end;

procedure TfrmLocalizar.FormCreate(Sender: TObject);
begin
  MDODataSet.SelectSQL.Text :=
    'SELECT Codigo, Nome FROM Contato ' +
    'WHERE UPPER(Nome) CONTAINING UPPER(:Nome)';

  { Quero abrir o DataSet, mas não quero trazer nenhum registro
    do servidor. Para isto passo no parâmetro Nome uma string
    que certamente não será encontrada no banco de dados. }
  MDODataSet.Params.ByName('Nome').AsString := #13;
  MDODataSet.Open;
end;

procedure TfrmLocalizar.MDODataSetAfterOpen(DataSet: TDataSet);
begin
  btnOk.Enabled := not MDODataSet.IsEmpty;
  
  with MDODataSet do
  begin
    FieldByName('Codigo').Visible := false;
    FieldByName('Codigo').DisplayLabel := 'Code';
    FieldByName('Nome').DisplayLabel := 'Name';
  end;
end;

procedure TfrmLocalizar.btnLocalizarClick(Sender: TObject);
begin
  MDODataSet.Close;
  MDODataSet.Params.ByName('Nome').AsString := edtNome.Text;
  MDODataSet.Open;
  if not MDODataSet.IsEmpty then
    MDODataSet.FieldByName('Nome').FocusControl;
end;

end.
