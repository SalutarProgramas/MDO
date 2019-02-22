{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{         Aplicativo de cadastro de nomes e telefones        }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{                                                            }
{************************************************************}
unit uFiltrarCDS;

interface

uses
  Graphics, Forms, StdCtrls, Buttons, Classes, Controls, SysUtils;

type
  TfrmFiltrar = class(TForm)
    radTodos: TRadioButton;
    radNome: TRadioButton;
    edtNome: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure radTodosClick(Sender: TObject);
    procedure radNomeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Filtrar(var Filtro: string): boolean;

implementation

uses uConstCDS;

{$R *.DFM}

function Filtrar(var Filtro: string): boolean;
begin
  with TfrmFiltrar.Create(nil) do
  try
    Result := ShowModal = mrOk;
    if Result then
    begin
      if radTodos.Checked then
        Filtro := ''
      else
        Filtro := 'UPPER(Nome) CONTAINING UPPER(' +
          QuotedStr(edtNome.Text) + ')';
    end;
  finally
    Free;
  end;
end;

procedure EnableEdit(Edit: TEdit; const Enabled: boolean);
begin
  Edit.ReadOnly := not Enabled;
  Edit.TabStop := Enabled;
  if Enabled then
    Edit.Color := clWindow
  else
    Edit.Color := clBtnFace;
end;

procedure TfrmFiltrar.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    if radNome.Checked and (edtNome.Text = '') then
    begin
      edtNome.SetFocus;
      raise Exception.Create(SNomeInvalido);
    end;
  end;
end;

procedure TfrmFiltrar.radTodosClick(Sender: TObject);
begin
  EnableEdit(edtNome, false);
end;

procedure TfrmFiltrar.radNomeClick(Sender: TObject);
begin
  EnableEdit(edtNome, true);
  edtNome.SetFocus;
end;

procedure TfrmFiltrar.FormCreate(Sender: TObject);
begin
  EnableEdit(edtNome, false);
end;

end.
