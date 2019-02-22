{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{              Contact: henrique@delphi-br.org               }
{                                                            }
{           Based on the FreeIBComponents written            }
{          by  Gregory H. Deatz - gdeatz@hlmdd.com           }
{           and InterBase Express 4.3 created by             }
{                    Inprise Corporation.                    }
{                                                            }
{************************************************************}

{ @abstract(Editor de propriedades da base de dados.)
  @author(Henrique Meira [henrique@delphi-br.org])
  @created(Indefinido)
  @lastmod(02 Feb 2003) }

unit MDODatabaseEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, MDODatabase, MDO, MDOConst, ComCtrls;

type
  TMDODatabaseEditForm = class(TForm)
    GroupBox2: TGroupBox;
    LocalRbtn: TRadioButton;
    RemoteRbtn: TRadioButton;
    ServerName: TEdit;
    Protocol: TComboBox;
    Label8: TLabel;
    Label7: TLabel;
    Browse: TButton;
    DatabaseName: TEdit;
    Label9: TLabel;
    GroupBox3: TGroupBox;
    UserName: TEdit;
    Password: TEdit;
    SQLRole: TEdit;
    CharacterSet: TComboBox;
    LoginPrompt: TCheckBox;
    DatabaseParams: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Test: TButton;
    procedure sClick(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure LocalRbtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure UserNameChange(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure SQLRoleChange(Sender: TObject);
    procedure CharacterSetChange(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure TestClick(Sender: TObject);
  private
    { Private declarations }
    Database: TMDODatabase;
    function Edit: Boolean;
    function GetParam(Name: string): string;
    procedure AddParam(Name, Value: string);
    procedure DeleteParam(Name: string);
  public
    { Public declarations }
  end;

var
  MDODatabaseEditForm: TMDODatabaseEditForm;

function EdiTMDODatabase(ADatabase: TMDODatabase): Boolean;

implementation

{$R *.DFM}

uses LibHelp, TypInfo;

function EdiTMDODatabase(ADatabase: TMDODatabase): Boolean;
begin
  with TMDODatabaseEditForm.Create(Application) do
  try
    Database := ADatabase;
    Result := Edit;
  finally
    Free;
  end;
end;

function TMDODatabaseEditForm.GetParam(Name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to DatabaseParams.Lines.Count - 1 do
  begin
    if (Pos(Name, LowerCase(DatabaseParams.Lines.Names[i])) = 1) then {mbcs ok}
    begin
      Result := DatabaseParams.Lines.Values[DatabaseParams.Lines.Names[i]];
      break;
    end;
  end;
end;

procedure TMDODatabaseEditForm.AddParam(Name, Value: string);
var
  i: Integer;
  found: boolean;
begin
  found := False;
  if Trim(Value) <> '' then
  begin
    for i := 0 to DatabaseParams.Lines.Count - 1 do
    begin
      if (Pos(Name, LowerCase(DatabaseParams.Lines.Names[i])) = 1) then {mbcs ok}
      begin
        DatabaseParams.Lines.Values[DatabaseParams.Lines.Names[i]] := Value;
        found := True;
        break;
      end;
    end;
    if not found then
      DatabaseParams.Lines.Add(Name + '=' + Value);
  end
  else
    DeleteParam(Name);
end;

procedure TMDODatabaseEditForm.DeleteParam(Name: string);
var
  i: Integer;
begin
  for i := 0 to DatabaseParams.Lines.Count - 1 do
  begin
    if (Pos(Name, LowerCase(DatabaseParams.Lines.Names[i])) = 1) then {mbcs ok}
    begin
      DatabaseParams.Lines.Delete(i);
      break;
    end;
  end;
end;

function TMDODatabaseEditForm.Edit: Boolean;
var
  st: string;

  procedure DecomposeDatabaseName;
  var
    Idx1, Idx2: Integer;
    st: string;
  begin
    if Pos('\\', Database.DatabaseName) <> 0 then {do not localize}
    begin
      LocalRBtn.Checked := False;
      RemoteRbtn.Checked := True;
      Protocol.ItemIndex := 1;
      st := copy(Database.DatabaseName, 3, Length(Database.DatabaseName));
      Idx1 := Pos('\', st); {do not localize}
      if Idx1 = 0 then
        MDOError(mdoeUnknownError, [nil])
      else
      begin
        ServerName.Text := Copy(st, 1, Idx1 - 1);
        DatabaseName.Text := Copy(st, Idx1 + 1, Length(st));
      end;
    end
    else
    begin
      Idx1 := Pos(':', Database.DatabaseName); {do not localize}
      if (Idx1 = 0) or (Idx1 = 2) then
      begin
        DatabaseName.Text := Database.DatabaseName;
      end
      else
      begin
        LocalRBtn.Checked := False;
        RemoteRbtn.Checked := True;
        Idx2 := Pos('@', Database.DatabaseName); {do not localize}
        if Idx2 = 0 then
        begin
          Protocol.ItemIndex := 0;
          ServerName.Text := copy(Database.DatabaseName, 1, Idx1 - 1);
          DatabaseName.Text := copy(Database.DatabaseName, Idx1 + 1,
            Length(Database.DatabaseName));
        end
        else
        begin
          Protocol.ItemIndex := 2;
          ServerName.Text := copy(Database.DatabaseName, 1, Idx2 - 1);
          DatabaseName.Text := copy(Database.DatabaseName, Idx2 + 1,
            Length(Database.DatabaseName));
        end;
      end;
    end;
  end;
begin
  DecomposeDatabaseName;
  DatabaseParams.Lines := Database.Params;
  LoginPrompt.Checked := Database.LoginPrompt;
  UserName.Text := GetParam('user_name');
  Password.Text := GetParam('password');
  SQLRole.Text := GetParam('sql_role');
  st := GetParam('lc_ctype');
  if (st <> '') then
    CharacterSet.ItemIndex := CharacterSet.Items.IndexOf(st);
  Result := False;
  if ShowModal = mrOk then
  begin
    Database.DatabaseName := DatabaseName.Text;
    if LocalRbtn.Checked then
      DatabaseName.Text := Database.DatabaseName
    else
      case Protocol.ItemIndex of
        0: Database.DatabaseName := Format('%s:%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
        1: Database.DatabaseName := Format('\\%s\%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
        2: Database.DatabaseName := Format('%s@%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
      end;
    Database.Params := DatabaseParams.Lines;
    Database.LoginPrompt := LoginPrompt.Checked;
    Result := True;
  end;
end;

procedure TMDODatabaseEditForm.sClick(Sender: TObject);
begin
  Browse.Enabled := False;
  Label7.Enabled := True;
  Label8.Enabled := True;
  Protocol.Enabled := True;
  ServerName.Enabled := True;
end;

procedure TMDODatabaseEditForm.BrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(Application) do
  try
    InitialDir := ExtractFilePath(DatabaseName.Text);
    Filter := SDatabaseFilter;
    if Execute then
      DatabaseName.Text := FileName;
  finally
    Free
  end;
end;

procedure TMDODatabaseEditForm.LocalRbtnClick(Sender: TObject);
begin
  Browse.Enabled := LocalRbtn.Checked;
  Label7.Enabled := RemoteRbtn.Checked;
  Label8.Enabled := RemoteRbtn.Checked;
  ServerName.Enabled := RemoteRbtn.Checked;
  Protocol.Enabled := RemoteRbtn.Checked;
end;

procedure TMDODatabaseEditForm.FormCreate(Sender: TObject);
begin
  HelpContext := hcDIBDataBaseEdit;
end;

procedure TMDODatabaseEditForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TMDODatabaseEditForm.UserNameChange(Sender: TObject);
begin
  AddParam('user_name', UserName.Text);
end;

procedure TMDODatabaseEditForm.PasswordChange(Sender: TObject);
begin
  AddParam('password', Password.Text);
end;

procedure TMDODatabaseEditForm.SQLRoleChange(Sender: TObject);
begin
  AddParam('sql_role_name', SQLRole.Text);
end;

procedure TMDODatabaseEditForm.CharacterSetChange(Sender: TObject);
begin
  if (CharacterSet.Text <> 'None') then {do not localize}
    AddParam('lc_ctype', CharacterSet.Text)
  else
    DeleteParam('lc_ctype');
end;

procedure TMDODatabaseEditForm.btnTestClick(Sender: TObject);
var
  tempDB: TMDODatabase;
begin
  Test.Enabled := false;
  tempDB := TMDODatabase.Create(nil);
  try
    if LocalRbtn.Checked then
      tempDB.DatabaseName := DatabaseName.Text
    else
      case Protocol.ItemIndex of
        0: tempDB.DatabaseName := Format('%s:%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
        1: tempDB.DatabaseName := Format('\\%s\%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
        2: tempDB.DatabaseName := Format('%s@%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
      end;
    tempDB.Params.Assign(DatabaseParams.Lines);
    tempDB.LoginPrompt := LoginPrompt.Checked;
    tempDB.Connected := true;
    ShowMessage('Successful Connection');
  finally
    tempDB.Free;
    Test.Enabled := true;
  end;
end;

procedure TMDODatabaseEditForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if Database.Connected then
  begin
    if MessageDlg(SDisconnectDatabase, mtConfirmation,
      mbOkCancel, 0) <> mrOk then
      Exit;
    Database.Close;
  end;
  ModalResult := mrOk;
end;

procedure TMDODatabaseEditForm.TestClick(Sender: TObject);
var
  tempDB: TMDODatabase;
begin
  Test.Enabled := false;
  tempDB := TMDODatabase.Create(nil);
  try
    if LocalRbtn.Checked then
      tempDB.DatabaseName := DatabaseName.Text
    else
      case Protocol.ItemIndex of
        0: tempDB.DatabaseName := Format('%s:%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
        1: tempDB.DatabaseName := Format('\\%s\%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
        2: tempDB.DatabaseName := Format('%s@%s', [ServerName.Text,
          DatabaseName.Text]); {do not localize}
      end;
    tempDB.Params.Assign(DatabaseParams.Lines);
    tempDB.LoginPrompt := LoginPrompt.Checked;
    tempDB.Connected := true;
    ShowMessage('Successful Connection');
  finally
    tempDB.Free;
    Test.Enabled := true;
  end;
end;

end.
