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
{
  Componente para exportação de dados
  --
  Component for data export
}
unit MDOCustomExport;

interface

uses Classes, MDOCustomDataSet, SysUtils, DB;

type
  TMDOCustomDataExport = class (TComponent)
  private
    FBoolAsInt: Boolean;
    FDataSet: TMDOCustomDataSet;
    FDefaultExt: string;
    FFileName: string;
    FOnEnd: TNotifyEvent;
    FOnStart: TNotifyEvent;
  public
    procedure ExportData;
    procedure InternalExport; virtual;
    property BoolAsInt: Boolean read FBoolAsInt write FBoolAsInt;
    property DataSet: TMDOCustomDataSet read FDataSet write FDataSet;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property FileName: string read FFileName write FFileName;
    property OnEnd: TNotifyEvent read FOnEnd write FOnEnd;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;
  
implementation

{ TMDOCustomDataExport }

{
***************************** TMDOCustomDataExport *****************************
}
procedure TMDOCustomDataExport.ExportData;
begin
  if Assigned(FOnStart) then FOnStart(Self);
  InternalExport;
  if Assigned(FOnEnd) then FOnEnd(Self);
end;

procedure TMDOCustomDataExport.InternalExport;
begin
  
end;

end.
