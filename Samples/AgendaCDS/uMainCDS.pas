{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{         Aplicativo de cadastro de nomes e telefones        }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{                                                            }
{************************************************************}
unit uMainCDS;

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, DBCtrls, MDODatabase, Db, StdCtrls,
  Menus, Mask, MDOSQL, SysUtils, IniFiles, MDOCustomDataSet, MDOMisc,
  Grids, DBGrids, Provider, DBClient, MDOScript;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    DBNavigator1: TDBNavigator;
    MDODatabase: TMDODatabase;
    MDODataSet: TMDODataSet;
    edtCodigo: TDBEdit;
    DataSource: TDataSource;
    edtNome: TDBEdit;
    edtEndereco: TDBEdit;
    edtBairro: TDBEdit;
    edtCidade: TDBEdit;
    edtCEP: TDBEdit;
    edtUF: TDBEdit;
    edtFone: TDBEdit;
    edtFax: TDBEdit;
    edtCelular: TDBEdit;
    Label11: TLabel;
    edtEmail: TDBEdit;
    btnProcurar: TSpeedButton;
    btnFiltrar: TSpeedButton;
    btnSobre: TSpeedButton;
    MDOMisc1: TMDOMisc;
    DBGrid1: TDBGrid;
    MDOTransaction: TMDOTransaction;
    Label12: TLabel;
    ClientDataSet1: TClientDataSet;
    DataSetProvider1: TDataSetProvider;
    MainMenu1: TMainMenu;
    mniArq: TMenuItem;
    mniArqSair: TMenuItem;
    mniReg: TMenuItem;
    mniRegPrimeiro: TMenuItem;
    mniRegAnterior: TMenuItem;
    mniRegProximo: TMenuItem;
    mniRegUltimo: TMenuItem;
    N1: TMenuItem;
    mniRegNovo: TMenuItem;
    mniRegExcluir: TMenuItem;
    mniRegAlterar: TMenuItem;
    N2: TMenuItem;
    mniRegSalvar: TMenuItem;
    mniRegCancelar: TMenuItem;
    N3: TMenuItem;
    mniRegAtualizar: TMenuItem;
    mniFer: TMenuItem;
    mniFerLocalizar: TMenuItem;
    mniFerFiltrar: TMenuItem;
    mniAjuda: TMenuItem;
    mniAjudaSobre: TMenuItem;
    MDOScript1: TMDOScript;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure RegClick(Sender: TObject);
    procedure MDODataSetAfterPost(DataSet: TDataSet);
    procedure MDODataSetAfterDelete(DataSet: TDataSet);
    procedure MDODataSetNewRecord(DataSet: TDataSet);
    procedure MDODataSetBeforePost(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure mniArqSairClick(Sender: TObject);
    procedure MDODataSetBeforeDelete(DataSet: TDataSet);
    procedure mniFerLocalizarClick(Sender: TObject);
    procedure mniFerFiltrarClick(Sender: TObject);
    procedure mniAjudaSobreClick(Sender: TObject);
    procedure DataSourceStateChange(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure ClientDataSet1AfterPost(DataSet: TDataSet);
  private
    FSelect: string;
    FWhere: string;
    FOrderBy: string;
    procedure AbrirDataSet;
    procedure AtualizarMenuReg;
    procedure CriaBase(path: string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses uLocalizarCDS, uFiltrarCDS, uConstCDS;

{$R *.DFM}

procedure TfrmMain.RegClick(Sender: TObject);
begin
  with MDODataSet do
    case (Sender as TMenuItem).Tag of
      1: First;
      2: Prior;
      3: Next;
      4: Last;
      5: Insert;
      6: Delete;
      7: Edit;
      8: Post;
      9: Cancel;
      10: Refresh;
    end;
end;

procedure TfrmMain.AbrirDataSet;
var
  Comando: string;
begin
  { Este procedimento gera novamente a instrução
    SelectSQL a partir das strings contidas nas
    variáveis FSelect, FWhere e FOrderBy. }

  Comando := FSelect;

  if FWhere <> '' then
    Comando := Comando + ' WHERE ' + FWhere;

  if FOrderBy <> '' then
    Comando := Comando + ' ORDER BY ' + FOrderBy;

  if ClientDataSet1.Active then
    ClientDataSet1.Close;

  MDODataSet.SelectSQL.Text := Comando;
  ClientDataSet1.Open;
end;

procedure TfrmMain.AtualizarMenuReg;
var
  CanModify, Editing, UpEnable, DnEnable, IsEmpty: boolean;
begin
  CanModify := MDODataSet.Active and MDODataSet.CanModify;
  Editing := MDODataSet.State in [dsEdit, dsInsert];
  UpEnable := MDODataSet.Active and (not MDODataSet.BOF);
  DnEnable := MDODataSet.Active and (not MDODataSet.EOF);
  IsEmpty := MDODataSet.BOF and MDODataSet.EOF;

  mniRegPrimeiro.Enabled := UpEnable;
  mniRegAnterior.Enabled := UpEnable;
  mniRegProximo.Enabled := DnEnable;
  mniRegUltimo.Enabled := DnEnable;
  mniRegNovo.Enabled := CanModify;
  mniRegExcluir.Enabled := CanModify and (not IsEmpty);
  mniRegAlterar.Enabled := CanModify and (not Editing);
  mniRegSalvar.Enabled := CanModify and Editing;
  mniRegCancelar.Enabled := CanModify and Editing;
  mniRegAtualizar.Enabled := CanModify;
end;

procedure TfrmMain.MDODataSetAfterPost(DataSet: TDataSet);
begin
  try
    MDOTransaction.CommitRetaining;
  except
    MDOTransaction.Rollback;
    raise;
  end;
end;

procedure TfrmMain.MDODataSetAfterDelete(DataSet: TDataSet);
begin
  try
    MDOTransaction.CommitRetaining;
  except
    MDOTransaction.Rollback;
    raise;
  end;
end;

procedure TfrmMain.MDODataSetNewRecord(DataSet: TDataSet);
begin
//  MDODataSetNome.FocusControl;
end;

procedure TfrmMain.MDODataSetBeforePost(DataSet: TDataSet);
var
  Query: TMDOSQL;
begin
  if MDODataSet.FieldByName('CODIGO').IsNull then
  begin
    Query := TMDOSQL.Create(nil);
    try
      Query.Database := MDODatabase;
      Query.Transaction := MDOTransaction;
      Query.SQL.Text := 'SELECT MAX(Codigo) FROM Contato';
      Query.ExecQuery;
      try
        MDODataSet.FieldByName('CODIGO').AsInteger := Query.Fields[0].AsInteger + 1;
      finally
        Query.Close;
      end;
    finally
      Query.Free;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
const
  Secao = 'Banco de Dados';
var
  Ini: TIniFile;
  ArqIni, Caminho, Usuario, Senha: string;
begin
  ArqIni := ChangeFileExt(ParamStr(0), '.ini');

  Ini := TIniFile.Create(ArqIni);
  try
    Caminho := Ini.ReadString(Secao, 'Caminho', '');
    Usuario := Ini.ReadString(Secao, 'Usuario', 'SYSDBA');
    Senha := Ini.ReadString(Secao, 'Senha', '');

    if Caminho = '' then
      if InputQuery('Banco de Dados', 'Caminho', Caminho) then
        Ini.WriteString(Secao, 'Caminho', Caminho)
      else
      begin
        Application.Terminate;
        Exit;
      end;

  finally
    Ini.Free;
  end;

  if not FileExists(Caminho) then
    CriaBase(Caminho);


  MDODatabase.Close;
  MDODatabase.DatabaseName := Caminho;
  MDODatabase.Params.Values['user_name'] := Usuario;
  MDODatabase.Params.Values['password'] := Senha;
  MDODatabase.LoginPrompt := Senha = '';

  try
    ClientDataSet1.Open;
  except
    on E: Exception do
    begin
      MessageDlg(Format(SErroConexao, [E.Message, ArqIni]),
        mtError, [mbOk], 0);
      Application.Terminate;
      Exit;
    end;
  end;

  { Salva o comando SELECT para futuras
    alterações em run-time. }
  FSelect := MDODataSet.SelectSQL.Text;
  FWhere := 'Codigo IS NULL'; { Consulta vazia }
  FOrderBy := 'Nome';
  AbrirDataSet;
end;

procedure TfrmMain.mniArqSairClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.MDODataSetBeforeDelete(DataSet: TDataSet);
begin
  if MessageDlg('Excluir este registro?', mtConfirmation,
      [mbYes, mbNo], 0) <> mrYes then
    SysUtils.Abort;
end;

procedure TfrmMain.mniFerLocalizarClick(Sender: TObject);
var
  Codigo: integer;
begin
  if Localizar(Codigo) then
  begin
    FWhere := 'Codigo = ' + IntToStr(Codigo);
    AbrirDataSet;
  end;
end;

procedure TfrmMain.mniFerFiltrarClick(Sender: TObject);
var
  S: string;
begin
  if Filtrar(S) then
  begin
    FWhere := S;
    AbrirDataSet;
  end;
end;

procedure TfrmMain.mniAjudaSobreClick(Sender: TObject);
begin
  MessageDlg(SSobre,  mtInformation, [mbOk], 0);
end;

procedure TfrmMain.DataSourceStateChange(Sender: TObject);
begin
  AtualizarMenuReg;
end;

procedure TfrmMain.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  AtualizarMenuReg;
end;

procedure TfrmMain.ClientDataSet1AfterPost(DataSet: TDataSet);
begin
  ClientDataSet1.ApplyUpdates(-1);
end;

procedure TfrmMain.CriaBase(path: string);
begin

  { Cria a base de dados de exemplo }
  with MDODatabase do
  begin
    if FileExists(path) then
      if MessageDlg('O arquivo já existe, sobrescrever?', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        Connected := True;
        DropDatabase;
        Connected := False;
      end
      else
        Exit;

    DatabaseName := path;
    Params.Clear;
    Params.Add('user "SYSDBA" password "masterkey" page_size 4096');
    CreateDatabase; // Cria e conecta a base de dados
  end;

  { Cria-se a Stored Procedure responsável por "lançar" o evento }
  MDOScript1.ExecuteScript;

end;

end.
