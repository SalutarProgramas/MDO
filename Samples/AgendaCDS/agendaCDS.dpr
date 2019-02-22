program agendaCDS;

uses
  Forms,
  uConstCDS in 'uConstCDS.pas',
  uFiltrarCDS in 'uFiltrarCDS.pas' {frmFiltrar},
  uLocalizarCDS in 'uLocalizarCDS.pas' {frmLocalizar},
  uMainCDS in 'uMainCDS.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Agenda';
  Application.CreateForm(TfrmMain, frmMain);
  Application.ProcessMessages;
  if not Application.Terminated then
    Application.Run;
end.
