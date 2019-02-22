program Agenda;

uses
  Forms,
  uMain in 'uMain.pas' {frmMain},
  uLocalizar in 'uLocalizar.pas' {frmLocalizar},
  uFiltrar in 'uFiltrar.pas' {frmFiltrar},
  uConst in 'uConst.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Agenda';
  Application.CreateForm(TfrmMain, frmMain);
  Application.ProcessMessages;
  if not Application.Terminated then
    Application.Run;
end.
