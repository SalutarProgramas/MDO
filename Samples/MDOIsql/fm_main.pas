{************************************************************}
{                                                            }
{                            MDOIsql                         }
{                        Ferramenta SQL                      }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{              Contact: henrique@delphi-br.org               }
{                                                            }
{************************************************************}

unit fm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, DBGrids, Db, StdCtrls, MDOSQLMonitor, MDOdatabase, ActnList,
  Menus, ComCtrls, MDOQuery, MDO, ToolWin, AppEvnts, MDOCustomDataSet, MDOSQL,
  MDOHeader;

type
  TfrmMain = class(TForm)
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    actConectar: TAction;
    actCriarDB: TAction;
    actDropDB: TAction;
    actDesconectar: TAction;
    actDescForce: TAction;
    actUsuarios: TAction;
    actErros: TAction;
    actTraParametros: TAction;
    actCommit: TAction;
    actCommitRetaining: TAction;
    actRollback: TAction;
    actDstExecutar: TAction;
    actDstPlan: TAction;
    actSalvarResultado: TAction;
    MDOdatabase: TMDOdatabase;
    MDOTransaction: TMDOTransaction;
    MDOdataSet: TMDOdataSet;
    DataSource: TDataSource;
    Splitter1: TSplitter;
    OpenDialog: TOpenDialog;
    Database1: TMenuItem;
    Transaction1: TMenuItem;
    SQL1: TMenuItem;
    Conectar1: TMenuItem;
    Desconectar1: TMenuItem;
    Forardesconexo1: TMenuItem;
    Criar1: TMenuItem;
    Drop1: TMenuItem;
    Commit1: TMenuItem;
    Commitretaining1: TMenuItem;
    Rollback1: TMenuItem;
    Parmetros1: TMenuItem;
    Exibirerros1: TMenuItem;
    Usurios1: TMenuItem;
    Executar1: TMenuItem;
    Exibirplan1: TMenuItem;
    Salvarresultado1: TMenuItem;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ApplicationEvents: TApplicationEvents;
    MDOSQLMonitor1: TMDOSQLMonitor;
    pnlBottom: TPanel;
    pnlResult: TPanel;
    DBGrid: TDBGrid;
    pnlEditor: TPanel;
    Splitter3: TSplitter;
    actScrNext: TAction;
    actScrPrior: TAction;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    actScrNew: TAction;
    ToolButton11: TToolButton;
    Arquivo1: TMenuItem;
    Sair1: TMenuItem;
    actArqSair: TAction;
    Panel1: TPanel;
    mmoMonitor: TMemo;
    mmoErro: TMemo;
    Splitter2: TSplitter;
    mmoSQL: TMemo;
    Label1: TLabel;
    procedure actConectarExecute(Sender: TObject);
    procedure actCriarDBExecute(Sender: TObject);
    procedure actDropDBExecute(Sender: TObject);
    procedure actDesconectarExecute(Sender: TObject);
    procedure actDescForceExecute(Sender: TObject);
    procedure actUsuariosExecute(Sender: TObject);
    procedure actErrosExecute(Sender: TObject);
    procedure actDstExecutarExecute(Sender: TObject);
    procedure actDstPlanExecute(Sender: TObject);
    procedure actSalvarResultadoExecute(Sender: TObject);
    procedure actTraParametrosExecute(Sender: TObject);
    procedure actCommitExecute(Sender: TObject);
    procedure actCommitRetainingExecute(Sender: TObject);
    procedure actRollbackExecute(Sender: TObject);
    procedure MDOSQLMonitorSQL(EventText: string);
    procedure MDODatabaseAfterConnect(Sender: TObject);
    procedure MDODatabaseAfterdisconnect(Sender: TObject);
    procedure MDOTransactionStartTransaction(Sender: TObject);
    procedure MDOTransactionEndTransaction(Sender: TObject);
    procedure MDOTransactionAfterCommit(Sender: TObject);
    procedure MDOTransactionAfterRollback(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure MDOSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
    procedure MDODatabaseLogin(Database: TMDODatabase;
      LoginParams: TStrings);
    procedure FormCreate(Sender: TObject);
    procedure actScrNextExecute(Sender: TObject);
    procedure actScrPriorExecute(Sender: TObject);
    procedure actScrNewExecute(Sender: TObject);
    procedure actArqSairExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MDODatabaseClientLibChange(var MDOClientLib: TMDOClientLib;
      var AbortChange: Boolean);
  private
    { Private declarations }
    LastErrorMessage: string;
    LastErrorCode: Integer;
    ScriptList: array of TStrings;
    ScriptIndex: integer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses fm_database;

{$R *.DFM}

procedure TfrmMain.actConectarExecute(Sender: TObject);
begin

  actDesconectarExecute(Sender);

  EdiDatabase(MDOdatabase);
  MDOdatabase.Connected := true;
  StatusBar.Panels[3].Text := MDOdatabase.DatabaseName;

end;

procedure TfrmMain.actCriarDBExecute(Sender: TObject);
begin
{
  actDesconectarExecute(Sender);

  frmDatabase := TfrmDatabase.Create(Application);
  try
    if frmDatabase.ShowModal = mrOk then
    begin
      MDOdatabase.DatabaseName := frmDatabase.cbxDatabase.Items[frmDatabase.cbxDatabase.ItemIndex];
      MDOdatabase.Params.Clear;
      MDOdatabase.Params.Add('user "' + frmDatabase.edtUser.Text +
        '" password "' + frmDatabase.edtPass.Text +
        '" page_size 4096');
      MDOdatabase.CreateDatabase;
      MDOdatabase.Params.Clear;
      MDOdatabase.Params.Add('user_name=' + frmDatabase.edtUser.Text);
      MDOdatabase.Params.Add('password=' + frmDatabase.edtPass.Text);

//      MDOdatabase.UserName := frmDatabase.edtUser.Text;
//      MDOdatabase.Password := frmDatabase.edtPass.Text;
      if FileExists(MDOdatabase.DatabaseName) then
      begin
      //frmDatabase.OpenDialog.FileName := MDOdatabase.DatabaseName;
      // MDOdatabase.Connected := true;
        StatusBar.Panels[3].Text := frmDatabase.cbxDatabase.Items[frmDatabase.cbxDatabase.ItemIndex]
      end;


    end;
  finally
    frmDatabase.Release;
  end;
}
end;

procedure TfrmMain.actDropDBExecute(Sender: TObject);
begin

  if MDOdatabase.Connected and
    (MessageDlg('Are you sure?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    MDOdatabase.DropDatabase;
  end;

end;

procedure TfrmMain.actDesconectarExecute(Sender: TObject);
begin

  if MDOdatabase.Connected then
  begin
    if MDOdataSet.Active then
      MDOdataSet.Active := False;
    if MDOTransaction.InTransaction then
      if MessageDlg('commit?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
        then
        MDOTransaction.Commit
      else
        MDOTransaction.Rollback;

    MDOdatabase.Connected := False;
    StatusBar.Panels[3].Text := '';
  end;

end;

procedure TfrmMain.actDescForceExecute(Sender: TObject);
begin
  if MDOdatabase.Connected then
    MDOdatabase.ForceClose;
end;

procedure TfrmMain.actUsuariosExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actErrosExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actDstExecutarExecute(Sender: TObject);
var
  InputObject: TMDOBatchInput;
  i: Integer;
  bAllParamsAreBlobs: Boolean;
const
  uni: array[Boolean] of string = ('Bi-directional', 'Uni-directional');
begin

  mmoErro.Clear;
  
  MDOdatabase.CheckActive;
  if not MDOTransaction.InTransaction then
    MDOTransaction.StartTransaction;

  if MDOdataSet.Active then
    MDOdataSet.Active := False;

  MDOdataSet.SelectSQL := mmoSQL.Lines;

  MDOdataSet.QSelect.ParamCheck := True;

  try
    MDOdataSet.Prepare;
  except
    on E: EMDOError do
    begin
      if EMDOError(E).SQLCode = -104 then
      begin
        MDOdataSet.QSelect.ParamCheck := False;
        MDOdataSet.Prepare;
      end
      else
        raise;
    end;
  end;
  if (MDOdataSet.Params.Count > 0) and
    (MDOdataSet.StatementType in [SQLInsert,
    SQLUpdate,
      SQLDelete,
      SQLExecProcedure]) then
  begin

    { if the parameters are all blobs, then try to load them from files
      otherwise expect a batch load. }

    bAllParamsAreBlobs := True;

    for i := 0 to MDOdataSet.Params.Count - 1 do
      if MDOdataSet.Params[i].SQLType <> SQL_BLOB then
        bAllParamsAreBlobs := False;

    if bAllParamsAreBlobs then
    begin
      OpenDialog.FilterIndex := 5;
      OpenDialog.DefaultExt := '';
      OpenDialog.InitialDir := '.\';

      for i := 0 to MDOdataSet.Params.Count - 1 do
      begin
        OpenDialog.Title := MDOdataSet.Params[i].Name;
        if OpenDialog.Execute then
          MDOdataSet.Params[i].LoadFromFile(OpenDialog.FileName)
        else
          exit;

        MDOdataSet.Active := True;
        mmoSQL.Lines.Text := '';
      end;
    end
    else
    begin
      OpenDialog.Title := 'Batch execute from file';
      OpenDialog.FilterIndex := 1;
      OpenDialog.DefaultExt := '';
      if OpenDialog.Execute then
      begin
        InputObject := nil;
        try
          if OpenDialog.FilterIndex = 3 then
            InputObject := TMDOInputRawFile.Create
          else
          begin
            InputObject := TMDOInputDelimitedFile.Create;

            case OpenDialog.FilterIndex of
              1:
                begin
                  TMDOInputDelimitedFile(InputObject).ColDelimiter := #9;
                  TMDOInputDelimitedFile(InputObject).RowDelimiter := #13#10;
                end;

              2:
                begin
                  TMDOInputDelimitedFile(InputObject).ColDelimiter := '|';
                  TMDOInputDelimitedFile(InputObject).RowDelimiter := '~';
                end;
            end;

            TMDOInputDelimitedFile(InputObject).
              ReadBlanksAsNull := True;
          end;
          InputObject.Filename := OpenDialog.Filename;
          MDOdataSet.BatchInput(InputObject);
          mmoSQL.Lines.Text := '';
        finally
          InputObject.Free;
        end;
      end;
    end
  end
  else
  begin
    MDOdataSet.Active := True;

    inc(ScriptIndex);
    if ScriptIndex > 14 then
      ScriptIndex := 14;
    ScriptList[ScriptIndex].Text := mmoSQL.Lines.Text;
    mmoSQL.Lines.Text := '';
  end;




end;

procedure TfrmMain.actDstPlanExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actSalvarResultadoExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actTraParametrosExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actCommitExecute(Sender: TObject);
begin
  if MDOTransaction.InTransaction then
    MDOTransaction.Commit;
end;

procedure TfrmMain.actCommitRetainingExecute(Sender: TObject);
begin
  if MDOTransaction.InTransaction then
    MDOTransaction.CommitRetaining;
end;

procedure TfrmMain.actRollbackExecute(Sender: TObject);
begin
  if MDOTransaction.InTransaction then
    MDOTransaction.Rollback;
end;

procedure TfrmMain.MDOSQLMonitorSQL(EventText: string);
begin
  mmoMonitor.Lines.Add(EventText);
end;

procedure TfrmMain.MDODatabaseAfterConnect(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Connected';
end;

procedure TfrmMain.MDODatabaseAfterdisconnect(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Disconnected';
end;

procedure TfrmMain.MDOTransactionStartTransaction(Sender: TObject);
begin
  StatusBar.Panels[1].Text := 'Start transaction';
end;

procedure TfrmMain.MDOTransactionEndTransaction(Sender: TObject);
begin
  StatusBar.Panels[1].Text := 'End transaction';
end;

procedure TfrmMain.MDOTransactionAfterCommit(Sender: TObject);
begin
  StatusBar.Panels[2].Text := 'Commit';
end;

procedure TfrmMain.MDOTransactionAfterRollback(Sender: TObject);
begin
  StatusBar.Panels[2].Text := 'Rollback';
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    actDesconectarExecute(Sender);
  except
    actDescForceExecute(Sender);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  if MDOdatabase.Connected then
    MDOdatabase.ForceClose;
  for i := 0 to 14 do
    ScriptList[i].Free;    
end;

procedure TfrmMain.ApplicationEventsException(Sender: TObject;
  E: Exception);
begin
  Beep;
  mmoErro.Lines.Add(#13 + #10 + E.Message);
  LastErrorMessage := E.Message;
  if (E is EMDOError) then
    LastErrorCode := EMDOError(E).SQLCode
  else
    LastErrorCode := 0;
  {
  if (E is EMDOClientError) and
     (TMDOClientError(EMDOError(E).SQLCode) = feConnectionTimeout) then
    StatusBar.Panels[0].Text := '';
  }
end;

procedure TfrmMain.MDOSQLMonitor1SQL(EventText: String;
  EventTime: TDateTime);
begin
  mmoMonitor.Lines.Add('Event at: ' + DateTimeToStr(EventTime));
  mmoMonitor.Lines.Add(EventText);
end;

procedure TfrmMain.MDODatabaseLogin(Database: TMDODatabase;
  LoginParams: TStrings);
begin
  Showmessage('login...');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  SetLength(ScriptList, 15);
  for i := 0 to 14 do
    ScriptList[i] := TstringList.Create;
  ScriptIndex := -1;
  MDOdatabase.DatabaseName := '';
end;

procedure TfrmMain.actScrNextExecute(Sender: TObject);
begin
  inc(ScriptIndex);
  if ScriptList[ScriptIndex].Text = '' then
  begin
    Dec(ScriptIndex);
    exit;
  end;
  if ScriptIndex > 14 then
    ScriptIndex := 14;
  mmoSQL.Lines.Text := ScriptList[ScriptIndex].Text;
end;

procedure TfrmMain.actScrPriorExecute(Sender: TObject);
begin
  Dec(ScriptIndex);
  if ScriptIndex < 0 then
    ScriptIndex := 0;
  mmoSQL.Lines.Text := ScriptList[ScriptIndex].Text;
end;

procedure TfrmMain.actScrNewExecute(Sender: TObject);
begin
  mmoSQL.Lines.Clear;
end;

procedure TfrmMain.actArqSairExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  MDOdatabase.ClientLib := clFBClient;
end;

procedure TfrmMain.MDODatabaseClientLibChange(
  var MDOClientLib: TMDOClientLib; var AbortChange: Boolean);
begin
  if MDOClientLib = clGDS32 then
    AbortChange := true;
end;

end.

