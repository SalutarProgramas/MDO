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

unit MDODialogs;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, ExtCtrls, DBLogDlg;

function ServerLoginDialog(const AServerName: string;
  var AUserName, APassword: string): Boolean;

implementation

function ServerLoginDialog(const AServerName: string;
  var AUserName, APassword: string): Boolean;
begin
  with TLoginDialog.Create(nil) do
  try
    Caption := 'InterBase Server Login';
    Label3.Caption := 'Server Name: ';
    DatabaseName.Caption := AServerName;
    UserName.Text := AUserName;
    Result := False;
    if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := UserName.Text;
      APassword := Password.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

end.
