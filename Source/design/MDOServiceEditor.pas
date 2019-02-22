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

unit MDOServiceEditor;

interface

{$I MDO.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MDOServices, MDOConst
  {$IFDEF MDO_DELPHI6_UP}
    , Variants
  {$ENDIF};

type
  TMDOServiceEditorForm = class (TForm)
    bBrowse: TButton;
    bCancel: TButton;
    bOk: TButton;
    cbActive: TCheckBox;
    cbLoginPrompt: TCheckBox;
    cbProtocol: TComboBox;
    cbServer: TComboBox;
    eDataBase: TEdit;
    ePwd: TEdit;
    eServer: TEdit;
    eUsr: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lProtocol: TLabel;
    lServer: TLabel;
    mSettings: TMemo;
    procedure bBrowseClick(Sender: TObject);
    procedure cbServerChange(Sender: TObject);
    procedure ePwdChange(Sender: TObject);
    procedure eUsrChange(Sender: TObject);
  private
    Service: TMDOCustomService;
    procedure AddParam(Name, Value: string);
    procedure DeleteParam(Name: string);
    function Edit: Boolean;
    function GetParam(Name: string): string;
  end;
  
var
  MDOServiceEditorForm: TMDOServiceEditorForm;

function EditMDOService(AService: TMDOCustomService): Boolean;

implementation

uses TypInfo;

{$R *.dfm}

function EditMDOService(AService: TMDOCustomService): Boolean;
begin
  with TMDOServiceEditorForm.Create(Application) do
    try
      Service := AService;
      Result := Edit;
    finally
      Free;
    end;
end;

{
**************************** TMDOServiceEditorForm *****************************
}
procedure TMDOServiceEditorForm.AddParam(Name, Value: string);
var
  i: Integer;
  found: Boolean;
begin
  found := False;
  if Trim(Value) <> '' then
    begin
      for i := 0 to mSettings.Lines.Count - 1 do
        begin
          if (Pos(Name, LowerCase(mSettings.Lines.Names[i])) = 1) then
            begin
              mSettings.Lines.Values[mSettings.Lines.Names[i]] := Value;
              found := True;
              break;
            end;
        end;
      if not found then mSettings.Lines.Add(Name + '=' + Value);
    end
  else DeleteParam(Name);
end;

procedure TMDOServiceEditorForm.bBrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
    begin
      Filter := SDatabaseFilter;
      if Execute then
        eDataBase.Text := FileName;
    end;
end;

procedure TMDOServiceEditorForm.cbServerChange(Sender: TObject);
begin
  lServer.Enabled := cbServer.ItemIndex = 1;
  eServer.Enabled := cbServer.ItemIndex = 1;
  
  lProtocol.Enabled := cbServer.ItemIndex = 1;
  cbProtocol.Enabled := cbServer.ItemIndex = 1;
end;

procedure TMDOServiceEditorForm.DeleteParam(Name: string);
var
  i: Integer;
begin
  for i := 0 to mSettings.Lines.Count - 1 do
    begin
      if (Pos(Name, LowerCase(mSettings.Lines.Names[i])) = 1) then
        begin
          mSettings.Lines.Delete(i);
          break;
        end;
    end;
end;

function TMDOServiceEditorForm.Edit: Boolean;
var
  PropInfo: PPropInfo;
begin
  mSettings.Lines := Service.Params;
  cbLoginPrompt.Checked := Service.LoginPrompt;
  eUsr.Text := GetParam('user_name');
  ePwd.Text := GetParam('password');
  
  PropInfo := GetPropInfo(Service, 'DatabaseName', [tkString]);
  
  eDataBase.Enabled := (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString);
  bBrowse.Enabled := (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString);
  
  
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString) then
    eDataBase.Text := VarToStr(GetPropValue(Service, 'DatabaseName'));
  
  mSettings.Lines := Service.Params;
  
  case Service.Protocol of
    Local: cbServer.ItemIndex := 0;
    TCP: cbProtocol.ItemIndex := 0;
    SPX: cbProtocol.ItemIndex := 1;
    NamedPipe: cbProtocol.ItemIndex := 2;
  end;
  
  eServer.Text := Service.ServerName;
  cbActive.Checked := Service.Active;
  
  Result := False;
  if ShowModal = mrOk then
    begin
      if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString) then
        SetPropValue(Service, 'DatabaseName', eDataBase.Text);
  
      if cbServer.ItemIndex = 0 then Service.Protocol := Local
      else
        begin
          case cbProtocol.ItemIndex of
            0: Service.Protocol := TCP;
            1: Service.Protocol := SPX;
            2: Service.Protocol := NamedPipe;
          end;
        end;
  
  
      if cbProtocol.ItemIndex = 0 then Service.ServerName := 'localhost'
      else Service.ServerName := eServer.Text;
  
      Service.Params := mSettings.Lines;
      Service.LoginPrompt := cbLoginPrompt.Checked;
      Service.Active := cbActive.Checked;
  
      Result := True;
    end;
end;

procedure TMDOServiceEditorForm.ePwdChange(Sender: TObject);
begin
  AddParam('password', ePwd.Text);
end;

procedure TMDOServiceEditorForm.eUsrChange(Sender: TObject);
begin
  AddParam('user_name', eUsr.Text);
end;

function TMDOServiceEditorForm.GetParam(Name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to mSettings.Lines.Count - 1 do
    begin
      if (Pos(Name, LowerCase(mSettings.Lines.Names[i])) = 1) then
        begin
          Result := mSettings.Lines.Values[mSettings.Lines.Names[i]];
          break;
        end;
    end;
end;

end.

