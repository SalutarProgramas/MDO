object frmMain: TfrmMain
  Left = 291
  Top = 110
  Width = 305
  Height = 324
  Caption = 'Backup Service'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 0
    Top = 284
    Width = 297
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
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 65
    Height = 13
    Caption = 'Database file:'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 53
    Height = 13
    Caption = 'Backup file'
  end
  object Button1: TButton
    Left = 112
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Backup'
    TabOrder = 0
    OnClick = Button1Click
  end
  object mmoLog: TMemo
    Left = 16
    Top = 104
    Width = 265
    Height = 129
    TabOrder = 1
  end
  object edtDbFile: TEdit
    Left = 16
    Top = 24
    Width = 265
    Height = 21
    TabOrder = 2
  end
  object edtBkFile: TEdit
    Left = 16
    Top = 64
    Width = 265
    Height = 21
    TabOrder = 3
  end
  object MDOBackupService1: TMDOBackupService
    LoginPrompt = False
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    Protocol = TCP
    ServerName = 'localhost'
    TraceFlags = []
    BlockingFactor = 0
    Options = []
    Left = 16
    Top = 240
  end
end
