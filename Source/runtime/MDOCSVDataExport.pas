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
  Componente para exportação de dados no padrão CSV
  --
  Component for data export as CSV
}

unit MDOCSVDataExport;

interface

uses Classes, SysUtils, DB, MDOCustomExport;

type
  TCSVHeaderOptions = (hoPhisicalName, hoDisplayName, hoNone);

  TMDOCSVDataExport = class (TMDOCustomDataExport)
  private
    FHeaderOption: TCSVHeaderOptions;
    FSeparator: string;
  public
    procedure InternalExport; override;
  published
    property BoolAsInt;
    property DataSet;
    property DefaultExt;
    property FileName;
    property HeaderOption: TCSVHeaderOptions read FHeaderOption write 
            FHeaderOption;
    property Separator: string read FSeparator write FSeparator;
  end;

implementation

uses MDOConst;

{ TMDOCSVDataExport }

{
****************************** TMDOCSVDataExport *******************************
}
procedure TMDOCSVDataExport.InternalExport;
var
  ls: TStringList;
  Field: Integer;
  s: string;
begin
  if DataSet = nil then
    raise
      Exception.Create(SCSVExportNoDataset)
  else
  begin
    if (Trim(FileName) = '') then
      raise
        Exception.Create(SCSVExportNoFileName)
    else
    begin
      ls := TStringList.Create;
      try
        for Field := 0 to (DataSet.FieldCount - 1) do
        begin
          case FHeaderOption of
            hoPhisicalName: s := s + DataSet.Fields[Field].FieldName;
            hoDisplayName: s := s + DataSet.Fields[Field].DisplayName;
            hoNone: Break;
          end;
          if Field < (DataSet.FieldCount - 1) then
            s := s + FSeparator;
        end;
        DataSet.DisableControls;
        DataSet.First;
        while not DataSet.Eof do
        begin
          s := '';
          for Field := 0 to (DataSet.FieldCount - 1) do
          begin
            if not DataSet.Fields[Field].IsNull then
            begin
              case DataSet.Fields[Field].DataType of
                ftWideString, ftString: s := s + DataSet.Fields[Field].AsString;
                          ftBytes, ftAutoInc, ftSmallint, ftInteger, ftWord, ftLargeint: s := s + IntToStr(DataSet.Fields[Field].Value);
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
            end;
            if Field < (DataSet.FieldCount - 1) then
              s := s + FSeparator;
          end;
          ls.Add(s);
          DataSet.Next;
        end;
        ls.SaveToFile(FileName);
      finally
        ls.Free;
        DataSet.EnableControls;
      end;
    end;
  end;
end;

end.

