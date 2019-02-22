program MDOEvents;

uses
  Forms,
  fm_mdoEvent in 'fm_mdoEvent.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
