{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{        Aplicativo de exemplo do uso Mestre/Detalhe         }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{                                                            }
{************************************************************}
unit fm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, MDOCustomDataSet, MDODataBase, Grids, DBGrids, StdCtrls, ExtCtrls,
  DBCtrls, MDOScript;

type
  TfrmMain = class(TForm)
    gbxMestre: TGroupBox;
    gbxDetalhe: TGroupBox;
    navMestre: TDBNavigator;
    navDetalhe: TDBNavigator;
    grdDetalhe: TDBGrid;
    grdMestre: TDBGrid;
    Label1: TLabel;
    MDODatabase1: TMDODatabase;
    MDOTransaction1: TMDOTransaction;
    MDOScript1: TMDOScript;
    MDODataSetMestre: TMDODataSet;
    MDODataSetDetalhe: TMDODataSet;
    DataSourceMestre: TDataSource;
    DataSourceDetalhe: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    function CriaBase: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

function TfrmMain.CriaBase: Boolean;
begin

  Result := False;

  { Cria a base de dados de exemplo }
  with MDODatabase1 do
  begin
    if FileExists('MestreDetalhe.gdb') then
      if MessageDlg('File exist, override?', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        Connected := True;
        DropDatabase;
        Connected := False;
      end
      else
        Exit;

    DatabaseName := 'MestreDetalhe.gdb';
    Params.Clear;
    Params.Add('user "SYSDBA" password "masterkey" page_size 4096');
    CreateDatabase; // Cria e conecta a base de dados
  end;

  MDOScript1.ExecuteScript;

  Result := True;

end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  { Cria a base de dados automaticamente, se nao existir }
  if not FileExists('MestreDetalhe.gdb') then
    if not CriaBase then
    begin
      raise Exception.Create('I do not can create base.');
      exit;
    end;
  { Conecta-se a base de dados }
  if not MDODatabase1.Connected then
    MDODatabase1.Open;

  MDODataSetMestre.Open;
  MDODataSetDetalhe.Open;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  MDODataSetMestre.Close;
  MDODataSetDetalhe.Close;
end;

end.
