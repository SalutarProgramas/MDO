program MDOIsql;

uses
  Forms,
  fm_main in 'fm_main.pas' {frmMain},
  fm_database in 'fm_database.pas' {frmDatabase};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
