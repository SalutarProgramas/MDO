unit fm_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MDOServices;

type
  TfrmMain = class(TForm)
    Label12: TLabel;
    MDOBackupService1: TMDOBackupService;
    Button1: TButton;
    mmoLog: TMemo;
    edtDbFile: TEdit;
    edtBkFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  with MDOBackupService1 do
  begin
    mmoLog.Lines.Clear;
    mmoLog.Lines.Add('Start');
    DatabaseName := edtDbFile.Text;
    ServerName := '';
    Protocol := Local;
    Active := True;
    BackupFile.Add(edtBkFile.Text);
    try
      ServiceStart;
      while not Eof do
        mmoLog.Lines.Add(GetNextLine);
    finally
      Active := False;
      mmoLog.Lines.Add('Finish');
    end;
  end;
end;

end.
