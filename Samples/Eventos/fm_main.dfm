object frmMain: TfrmMain
  Left = 210
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Eventos'
  ClientHeight = 305
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 292
    Width = 420
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Caption = 'Mercury Database Objects'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 80
    Height = 13
    Caption = 'Events (Max 15):'
  end
  object Label3: TLabel
    Left = 192
    Top = 16
    Width = 120
    Height = 13
    Caption = 'Posted event notification:'
  end
  object Memo1: TMemo
    Left = 192
    Top = 32
    Width = 217
    Height = 209
    TabOrder = 0
  end
  object Button2: TButton
    Left = 312
    Top = 248
    Width = 89
    Height = 25
    Caption = 'Launch event'
    TabOrder = 1
    OnClick = Button2Click
  end
  object mmoEventos: TMemo
    Left = 8
    Top = 32
    Width = 169
    Height = 209
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      'TESTE-01'
      'TESTE-02'
      'TESTE-03'
      'TEST-04'
      'TEST-05'
      'TEST-06'
      'TEST-07'
      'TEST-08'
      'TEST-09'
      'TEST-10'
      'TEST-11'
      'TEST-12'
      'TEST-13'
      'TEST-14'
      'TEST-15')
    ParentFont = False
    TabOrder = 2
  end
  object Button1: TButton
    Left = 16
    Top = 248
    Width = 75
    Height = 25
    Caption = '&Registry'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 96
    Top = 248
    Width = 75
    Height = 25
    Caption = '&Unregistry'
    TabOrder = 4
    OnClick = Button3Click
  end
  object edtEvento: TEdit
    Left = 192
    Top = 248
    Width = 105
    Height = 21
    TabOrder = 5
    Text = 'TEST'
  end
  object MDODatabase1: TMDODatabase
    DatabaseName = 'Eventos.gdb'
    DefaultTransaction = MDOTransaction1
    IdleTimer = 0
    LoginPrompt = False
    Params.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    SQLDialect = 3
    TraceFlags = []
    Left = 256
    Top = 104
  end
  object MDOTransaction1: TMDOTransaction
    Active = False
    AutoCommit = True
    DefaultDatabase = MDODatabase1
    Left = 288
    Top = 104
  end
  object MDOScript1: TMDOScript
    Database = MDODatabase1
    Script.Strings = (
      'SET TERM ^;'
      'CREATE PROCEDURE TESTE_EVENTO(MSG VARCHAR(20))'
      'AS'
      'BEGIN'
      '  POST_EVENT :MSG;'
      'END'
      '^'
      'SET TERM ;^')
    Terminator = ';'
    Transaction = MDOTransaction1
    Left = 320
    Top = 104
  end
  object MDOEvents1: TMDOEvents
    Database = MDODatabase1
    Events.Strings = (
      'TESTE')
    OnEventAlert = MDOEvents1EventAlert
    Registered = False
    Left = 384
    Top = 104
  end
  object MDOStoredProc1: TMDOStoredProc
    Database = MDODatabase1
    Transaction = MDOTransaction1
    Params = <
      item
        DataType = ftString
        Name = 'MSG'
        ParamType = ptInput
      end>
    StoredProcName = 'TESTE_EVENTO'
    Left = 352
    Top = 104
    ParamData = <
      item
        DataType = ftString
        Name = 'MSG'
        ParamType = ptInput
      end>
  end
end
