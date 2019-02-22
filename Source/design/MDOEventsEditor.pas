{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{                                                            }
{          Copyright(c) 2002-2005, The Mercury Team          }
{                  Contact: info@mdolib.org                  }
{                                                            }
{           Based on the FreeIBComponents written            }
{          by  Gregory H. Deatz - gdeatz@hlmdd.com           }
{           and InterBase Express 4.3 created by             }
{                    Inprise Corporation.                    }
{                                                            }
{************************************************************}

unit MDOEventsEditor;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Grids, MDOEvents;

type
  TMDOEventsEditorForm = class (TForm)
    bCancel: TButton;
    bOK: TButton;
    cEvents: TStringGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    RequestedEvents: TLabel;
    procedure FormCreate(Sender: TObject);
  end;
  
function EditAlerterEvents(Events: TStrings): Boolean;

implementation

{$R *.DFM}

function EditAlerterEvents(Events: TStrings): Boolean;
var
  i: integer;
begin
  result := false;
  with TMDOEventsEditorForm.Create(Application) do
  begin
    try
      for i := 0 to Events.Count - 1 do
        cEvents.Cells[1, i] := Events[i];
      if ShowModal = idOk then
      begin
        result := true;
        Events.Clear;
        for i := 0 to MaxEvents - 1 do
          if length(cEvents.Cells[1, i]) <> 0 then
            Events.Add(cEvents.Cells[1, i]);
      end;
    finally
      Free;
    end;
  end;
end;

{
***************************** TMDOEventsEditorForm *****************************
}
procedure TMDOEventsEditorForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to MaxEvents do
    cEvents.Cells[0, i - 1] := IntToStr(i);
end;

end.
