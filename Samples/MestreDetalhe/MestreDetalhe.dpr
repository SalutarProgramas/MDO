program MestreDetalhe;

uses
  Forms,
  fm_main in 'fm_main.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
