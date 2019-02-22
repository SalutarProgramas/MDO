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

unit MDOTransactionEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MDODatabase, MDO, ExtCtrls, MDOConst;

type
  TMDOTransactionEditForm = class (TForm)
    Cancelbtn: TButton;
    GroupBox1: TGroupBox;
    HelpBtn: TButton;
    OKBtn: TButton;
    Panel1: TPanel;
    rbReadCommitted: TRadioButton;
    rbReadOnlyTableStability: TRadioButton;
    rbReadWriteTableStability: TRadioButton;
    rbSnapShot: TRadioButton;
    TransactionParams: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure rbReadCommittedClick(Sender: TObject);
    procedure rbReadOnlyTableStabilityClick(Sender: TObject);
    procedure rbReadWriteTableStabilityClick(Sender: TObject);
    procedure rbSnapShotClick(Sender: TObject);
    procedure TransactionParamsClick(Sender: TObject);
    procedure TransactionParamsExit(Sender: TObject);
  private
    Transaction: TMDOTransaction;
    procedure ClearParamSelection;
    function Edit: Boolean;
    procedure ParseParams;
  end;

var
  MDOTransactionEditForm: TMDOTransactionEditForm;

function EdiTMDOtransaction(Atransaction: TMDOtransaction): Boolean;

implementation

{$R *.DFM}

uses
  LibHelp;

type
  TTransactionParam = (concurrency, read_committed, rec_version, nowait,
    consistency, read, write);
  TTransactionParams = set of TTransactionParam;


function EdiTMDOtransaction(ATransaction: TMDOtransaction): Boolean;
begin
  with TMDOtransactionEditForm.Create(Application) do
  try
    Transaction := ATransaction;
    Result := Edit;
  finally
    Free;
  end;
end;

{
*************************** TMDOTransactionEditForm ****************************
}
procedure TMDOTransactionEditForm.ClearParamSelection;
begin
  rbSnapShot.Checked := False;
  rbReadCommitted.Checked := False;
  rbReadOnlyTableStability.Checked := False;
  rbReadWriteTableStability.Checked := False;
end;

function TMDOTransactionEditForm.Edit: Boolean;
begin
  TransactionParams.Lines := Transaction.Params;
  ParseParams;
  Result := False;
  if ShowModal = mrOk then
  begin
    Transaction.Params := TransactionParams.Lines;
    Result := True;
  end;
end;

procedure TMDOTransactionEditForm.FormCreate(Sender: TObject);
begin
  HelpContext := hcDIBTransactionEdit;
end;

procedure TMDOTransactionEditForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TMDOTransactionEditForm.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if Transaction.Active then
  begin
    if MessageDlg(SCommitTransaction, mtConfirmation,
      mbOkCancel, 0) <> mrOk then
      Exit;
    Transaction.Rollback;
  end;
  ModalResult := mrOk;
end;

procedure TMDOTransactionEditForm.ParseParams;
var
  I: Integer;
  st: string;
  Value: TTransactionParams;
begin
  Value := [];
  for I := 0 to TransactionParams.Lines.Count - 1 do
  begin
    st := LowerCase(Trim(TransactionParams.Lines[I]));
    if st = '' then
      continue;
    if st = 'concurrency' then
      Include(Value, concurrency)
    else if st = 'read_committed' then
      Include(Value, read_committed)
    else if st = 'rec_version' then
      Include(Value, rec_version)
    else if st = 'nowait' then
      Include(Value, nowait)
    else if st = 'read' then
      Include(Value, read)
    else if st = 'write' then
      Include(Value, write)
    else if st = 'consistency' then
      Include(Value, consistency)
    else
    begin
      Value := [];
      break;
    end;
  end;
  ClearParamSelection;
  if Value = [concurrency, nowait] then
    rbSnapShot.Checked := True
  else if Value = [read_committed, rec_version, nowait] then
    rbReadCommitted.Checked := True
  else if Value = [read, consistency] then
    rbReadOnlyTableStability.Checked := True
  else if Value = [write, consistency] then
    rbReadWriteTableStability.Checked := True;
end;

procedure TMDOTransactionEditForm.rbReadCommittedClick(Sender: TObject);
begin
  TransactionParams.clear;
  TransactionParams.Lines.Add('read_committed'); { do not localize }
  TransactionParams.Lines.Add('rec_version'); { do not localize }
  TransactionParams.Lines.Add('nowait'); { do not localize }
end;

procedure TMDOTransactionEditForm.rbReadOnlyTableStabilityClick(Sender: 
        TObject);
begin
  TransactionParams.clear;
  TransactionParams.Lines.Add('read'); { do not localize }
  TransactionParams.Lines.Add('consistency'); { do not localize }
end;

procedure TMDOTransactionEditForm.rbReadWriteTableStabilityClick(Sender: 
        TObject);
begin
  TransactionParams.clear;
  TransactionParams.Lines.Add('write'); { do not localize }
  TransactionParams.Lines.Add('consistency'); { do not localize }
end;

procedure TMDOTransactionEditForm.rbSnapShotClick(Sender: TObject);
begin
  TransactionParams.clear;
  TransactionParams.Lines.Add('concurrency'); { do not localize }
  TransactionParams.Lines.Add('nowait'); { do not localize }
end;

procedure TMDOTransactionEditForm.TransactionParamsClick(Sender: TObject);
begin
  ClearParamSelection;
end;

procedure TMDOTransactionEditForm.TransactionParamsExit(Sender: TObject);
begin
  ParseParams;
end;

end.
