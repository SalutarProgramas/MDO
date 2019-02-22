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

{ @abstract(Componente para exportação de dados no padrão HTML)
  @author(Júlio Cesar)
  @created(Indefinido)
  @lastmod(10 Dec 2003) }
unit MDOHTMLDataExport;

interface

uses Classes, SysUtils, DB, MDOCustomExport;

type
  THTMLHeaderOptions = (hoPhisicalName, hoDisplayName, hoNone);

  TMDOHTMLDataExport = class (TMDOCustomDataExport)
  private
    FHeaderOption: THTMLHeaderOptions;
    FStyleSheet: string;
    FTitle: string;
  public
    procedure InternalExport; override;
  published
    property BoolAsInt;
    property DataSet;
    property DefaultExt;
    property FileName;
    property HeaderOption: THTMLHeaderOptions read FHeaderOption write 
            FHeaderOption;
    property StyleSheet: string read FStyleSheet write FStyleSheet;
    property Title: string read FTitle write FTitle;
  end;
  
implementation

{ TMDOHTMLDataExport }

{
****************************** TMDOHTMLDataExport ******************************
}
procedure TMDOHTMLDataExport.InternalExport;
var
  sl: TStringList;
  Field: Integer;
  s: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('<HTML><HEAD>');
    if (Trim(FStyleSheet) <> '') then
      sl.Add(Format('<link rel="stylesheet" href="%s" type="text/css">',
        [FStyleSheet]));
    sl.Add(Format('<TITLE>%s</TITLE>', [Title]));
    sl.Add('<META http-equiv=Content-Type content="text/html; charset=iso-8859-1">');
    sl.Add('<META content="MDOHTMLDataExport Component" name=GENERATOR></HEAD>');
    sl.Add('<BODY><TABLE cellSpacing=0 cellPadding=1 width="100%" border=1>');
  
    if (FHeaderOption <> hoNone) then
    begin
      s := '<TR>';
      for Field := 0 to (DataSet.FieldCount - 1) do
      begin
        case FHeaderOption of
          hoPhisicalName:
            s := s + Format('<TD>%s</TD>', [DataSet.Fields[Field].FieldName]);
          hoDisplayName:
            s := s + Format('<TD>%s</TD>', [DataSet.Fields[Field].DisplayName]);
        end;
      end;
      s := s + '</TR>';
      sl.Add(s);
    end;
  
    DataSet.DisableControls;
    DataSet.First;
    while not DataSet.Eof do
    begin
      s := '<TR>';
      for Field := 0 to (DataSet.FieldCount - 1) do
      begin
        s := s + '<TD>';
        if DataSet.Fields[Field].IsNull then
          s := s + 'Null'
        else
        begin
          case DataSet.Fields[Field].DataType of
            ftWideString, ftString:
              s := s + DataSet.Fields[Field].AsString;
            ftBytes, ftAutoInc, ftSmallint, ftInteger, ftWord, ftLargeint: s
              := s + IntToStr(DataSet.Fields[Field].Value);
            ftBoolean:
              begin
                if not DataSet.Fields[Field].IsNull then
                begin
                  if BoolAsInt then
                    s := s + IntToStr(Longint(DataSet.Fields[Field].Value))
                  else
                  begin
                    if DataSet.Fields[Field].AsBoolean then
                      s := s + 'True'
                    else
                      s := s + 'False';
                  end;
                end;
              end;
            ftFloat, ftCurrency, ftBCD:
              s := s + FloatToStr(DataSet.Fields[Field].Value);
            ftDate:
              s := s + DateToStr(DataSet.Fields[Field].Value);
            ftTime:
              s := s + TimeToStr(DataSet.Fields[Field].Value);
            ftDateTime:
              s := s + DateTimeToStr(DataSet.Fields[Field].Value);
{$IFDEF MDO_DELPHI6_UP}
            ftFMTBcd:
              s := s + FloatToStr(DataSet.Fields[Field].Value);
            ftTimeStamp:
              s := s + DateTimeToStr(DataSet.Fields[Field].Value);
{$ENDIF}
          else
            s := s + '';
          end;
          if (Field <= (DataSet.FieldCount - 1)) then
            s := s + '</TD>';
          if (Field = (DataSet.FieldCount - 1)) then
            s := s + '</TR>';
        end;
      end;
      sl.Add(s);
      DataSet.Next;
    end;
    sl.Add('</TABLE></BODY></HTML>');
    sl.SaveToFile(FileName);
  finally
    sl.Free;
    DataSet.EnableControls;
  end;
end;

end.

