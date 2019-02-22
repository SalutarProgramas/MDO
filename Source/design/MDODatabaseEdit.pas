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
  Editor de propriedades da base de dados
  --
  Database property editor
}

unit MDODatabaseEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, MDODatabase, MDO, MDOConst, ComCtrls;

type
  TMDODatabaseEditForm = class (TForm)
    Browse: TButton;
    CancelBtn: TButton;
    CharacterSet: TComboBox;
    DatabaseName: TEdit;
    DatabaseParams: TMemo;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    HelpBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LocalRbtn: TRadioButton;
    LoginPrompt: TCheckBox;
    OkBtn: TButton;
    Password: TEdit;
    Protocol: TComboBox;
    RemoteRbtn: TRadioButton;
    ServerName: TEdit;
    SQLRole: TEdit;
    Test: TButton;
    UserName: TEdit;
    Label1: TLabel;
    ClientLibrary: TComboBox;
    procedure BrowseClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure CharacterSetChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure LocalRbtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure sClick(Sender: TObject);
    procedure SQLRoleChange(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure UserNameChange(Sender: TObject);
  private
    Database: TMDODatabase;
    procedure AddParam(Name, Value: string);
    procedure DeleteParam(Name: string);
    function Edit: Boolean;
    function GetParam(Name: string): string;
  end;
  
var
  MDODatabaseEditForm: TMDODatabaseEditForm;

function EdiTMDODatabase(ADatabase: TMDODatabase): Boolean;

implementation

{$R *.DFM}

uses TypInfo;

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

{
***************************** TMDODatabaseEditForm *****************************
}
procedure TMDODatabaseEditForm.AddParam(Name, Value: string);
var
  i: Integer;
  found: Boolean;
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

procedure TMDODatabaseEditForm.CharacterSetChange(Sender: TObject);
begin
  if (CharacterSet.Text <> 'None') then {do not localize}
    AddParam('lc_ctype', CharacterSet.Text)
  else
    DeleteParam('lc_ctype');
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
  case Database.ClientLib of
    clAutoDetect: ClientLibrary.ItemIndex := 0;
    clFBClient: ClientLibrary.ItemIndex := 1;
    clGDS32: ClientLibrary.ItemIndex := 2;
    clFBEmbed: ClientLibrary.ItemIndex := 3;
  end;
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
    case ClientLibrary.ItemIndex of
      0: Database.ClientLib := clAutoDetect;
      1: Database.ClientLib := clFBClient;
      2: Database.ClientLib := clGDS32;
      3: Database.ClientLib := clFBEmbed;
    end;
    Result := True;
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

procedure TMDODatabaseEditForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TMDODatabaseEditForm.LocalRbtnClick(Sender: TObject);
begin
  Browse.Enabled := LocalRbtn.Checked;
  Label7.Enabled := RemoteRbtn.Checked;
  Label8.Enabled := RemoteRbtn.Checked;
  ServerName.Enabled := RemoteRbtn.Checked;
  Protocol.Enabled := RemoteRbtn.Checked;
  if Protocol.ItemIndex < 0 then Protocol.ItemIndex := 0;
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

procedure TMDODatabaseEditForm.PasswordChange(Sender: TObject);
begin
  AddParam('password', Password.Text);
end;

procedure TMDODatabaseEditForm.sClick(Sender: TObject);
begin
  Browse.Enabled := False;
  Label7.Enabled := True;
  Label8.Enabled := True;
  Protocol.Enabled := True;
  ServerName.Enabled := True;
end;

procedure TMDODatabaseEditForm.SQLRoleChange(Sender: TObject);
begin
  AddParam('sql_role_name', SQLRole.Text);
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
    if ClientLibrary.ItemIndex = 0 then
      tempDB.ClientLib := clFBClient
    else
      tempDB.ClientLib := clGDS32;
    tempDB.Connected := true;
    ShowMessage('Successful Connection');
  finally
    tempDB.Free;
    Test.Enabled := true;
  end;
end;

procedure TMDODatabaseEditForm.UserNameChange(Sender: TObject);
begin
  AddParam('user_name', UserName.Text);
end;

end.
