{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{     Aplicativo de exemplo de uso de eventos no Firebird    }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{                                                            }
{************************************************************}
unit fm_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB, MDOCustomDataSet, MDOScript, MDODataBase,
  StdCtrls, ComCtrls, MDOEvents, MDOQuery, ExtCtrls, MDOStoredProc;

type
  TfrmMain = class(TForm)
    MDODatabase1: TMDODatabase;
    MDOTransaction1: TMDOTransaction;
    MDOScript1: TMDOScript;
    MDOEvents1: TMDOEvents;
    Memo1: TMemo;
    Button2: TButton;
    MDOStoredProc1: TMDOStoredProc;
    Label1: TLabel;
    mmoEventos: TMemo;
    Label2: TLabel;
    Button1: TButton;
    Button3: TButton;
    edtEvento: TEdit;
    Label3: TLabel;
    procedure MDOEvents1EventAlert(Sender: TObject; EventName: String;
      EventCount: Integer; var CancelAlerts: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function CriaBase: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

function TfrmMain.CriaBase: Boolean;
begin

  Result := False;

  { Cria a base de dados de exemplo }
  with MDODatabase1 do
  begin
    if FileExists('Eventos.gdb') then
      if MessageDlg('O arquivo já existe, sobrescrever?', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        Connected := True;
        DropDatabase;
        Connected := False;
      end
      else
        Exit;

    DatabaseName := 'Eventos.gdb';
    Params.Clear;
    Params.Add('user "SYSDBA" password "masterkey" page_size 4096');
    CreateDatabase; // Cria e conecta a base de dados
  end;

  { Cria-se a Stored Procedure responsável por "lançar" o evento }
  MDOScript1.ExecuteScript;

  Result := True;

end;


procedure TfrmMain.MDOEvents1EventAlert(Sender: TObject; EventName: String;
  EventCount: Integer; var CancelAlerts: Boolean);
begin
  Memo1.Lines.Add(EventName + ': ' + IntToStr( EventCount ));
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  { Eventos no Firebird trabalham juntos com o controle de transaçoes,
    logo, eventos postados nao sao notificados ate que ele seja "commitado".
    Se for "rolledback" entao nenhum evento será notificado. }
  MDOStoredProc1.Params[0].AsString := edtEvento.Text;
  MDOStoredProc1.ExecProc;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  MDOEvents1.UnRegisterEvents;
  MDOEvents1.Events := mmoEventos.Lines;
  MDOEvents1.RegisterEvents;
  edtEvento.Text := MDOEvents1.Events.Strings[0];
  ShowMessage(IntToStr(MDOEvents1.Events.Count) + ' eventos registrados.');
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  MDOEvents1.UnRegisterEvents;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  { Cria a base de dados automaticamente, se nao existir }
  if not FileExists('Eventos.gdb') then
    if not CriaBase then
    begin
      raise Exception.Create('Nao foi possível criar a base.');
      exit;
    end;
  { Conecta-se a base de dados }
  if not MDODatabase1.Connected then
    MDODatabase1.Open;

  { Registra os eventos que receberao notificacao }
  MDOEvents1.RegisterEvents;
  mmoEventos.Lines := MDOEvents1.Events;
  edtEvento.Text := MDOEvents1.Events.Strings[0];
end;

end.
