object frmLocalizar: TfrmLocalizar
  Left = 193
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 343
  ClientWidth = 328
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
  object GroupBox1: TGroupBox
    Left = 15
    Top = 15
    Width = 300
    Height = 50
    Caption = 'Search names contain'
    TabOrder = 0
    object edtNome: TEdit
      Left = 10
      Top = 20
      Width = 200
      Height = 21
      TabOrder = 0
      OnEnter = edtNomeEnter
      OnExit = edtNomeExit
    end
    object btnLocalizar: TButton
      Left = 220
      Top = 20
      Width = 70
      Height = 21
      Caption = '&Search'
      TabOrder = 1
      OnClick = btnLocalizarClick
    end
  end
  object DBGrid1: TDBGrid
    Left = 15
    Top = 80
    Width = 300
    Height = 200
    DataSource = DataSource
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit]
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object btnOk: TBitBtn
    Left = 155
    Top = 300
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 240
    Top = 300
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object DataSource: TDataSource
    DataSet = MDODataSet
    Left = 42
    Top = 156
  end
  object MDODataSet: TMDODataSet
    AfterOpen = MDODataSetAfterOpen
    Database = frmMain.MDODatabase
    Transaction = frmMain.MDOTransaction
    BufferChunks = 32
    CachedUpdates = False
    LoadDefaults = False
    Left = 44
    Top = 124
  end
end
