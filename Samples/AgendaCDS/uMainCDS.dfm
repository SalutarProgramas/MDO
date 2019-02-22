object frmMain: TfrmMain
  Left = 401
  Top = 135
  ActiveControl = edtNome
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Agenda CDS'
  ClientHeight = 386
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 428
    Height = 2
    Align = alTop
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 0
    Top = 32
    Width = 428
    Height = 2
    Align = alTop
    Shape = bsTopLine
  end
  object Label11: TLabel
    Left = 15
    Top = 250
    Width = 28
    Height = 13
    Caption = 'E-mail'
    FocusControl = edtEmail
  end
  object Label12: TLabel
    Left = 0
    Top = 373
    Width = 428
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
    Left = 15
    Top = 50
    Width = 25
    Height = 13
    Caption = 'Code'
    FocusControl = edtCodigo
  end
  object Label2: TLabel
    Left = 91
    Top = 50
    Width = 28
    Height = 13
    Caption = 'Name'
    FocusControl = edtNome
  end
  object Label3: TLabel
    Left = 15
    Top = 100
    Width = 38
    Height = 13
    Caption = 'Address'
    FocusControl = edtEndereco
  end
  object Label4: TLabel
    Left = 276
    Top = 100
    Width = 35
    Height = 13
    Caption = 'Quarter'
    FocusControl = edtBairro
  end
  object Label6: TLabel
    Left = 275
    Top = 150
    Width = 17
    Height = 13
    Caption = 'ZIP'
    FocusControl = edtCEP
  end
  object Label7: TLabel
    Left = 386
    Top = 150
    Width = 14
    Height = 13
    Caption = 'UF'
    FocusControl = edtUF
  end
  object Label5: TLabel
    Left = 15
    Top = 150
    Width = 17
    Height = 13
    Caption = 'City'
    FocusControl = edtCidade
  end
  object Label8: TLabel
    Left = 15
    Top = 200
    Width = 31
    Height = 13
    Caption = 'Phone'
    FocusControl = edtFone
  end
  object Label9: TLabel
    Left = 153
    Top = 200
    Width = 17
    Height = 13
    Caption = 'Fax'
    FocusControl = edtFax
  end
  object Label10: TLabel
    Left = 290
    Top = 200
    Width = 32
    Height = 13
    Caption = 'Celular'
    FocusControl = edtCelular
  end
  object Panel1: TPanel
    Left = 0
    Top = 2
    Width = 428
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnProcurar: TSpeedButton
      Left = 300
      Top = 0
      Width = 30
      Height = 30
      Hint = 'Localizar'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        D44DD00D00D00DDD4744D00D00D00DD47444DDDDDDDDDD47444DDDDDD0000474
        44DDDDD00777F8444DDDDD0877777F80DDDDDD07777777F0DDDDD07777777777
        0DDDD077777777770DDDD07FE77777770DDDD07FE77777770DDDDD0FFEE77770
        DDDDDD08FFF77780DDDDDDD00777700DDDDDDDDDD0000DDDDDDD}
      ParentShowHint = False
      ShowHint = True
      OnClick = mniFerLocalizarClick
    end
    object btnFiltrar: TSpeedButton
      Left = 330
      Top = 0
      Width = 30
      Height = 30
      Hint = 'Filtrar'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333FF3FF3333333333CC30003333333333773777333333333C33
        3000333FF33337F33777339933333C3333333377F33337F3333F339933333C33
        33003377333337F33377333333333C333300333F333337F33377339333333C33
        3333337FF3333733333F33993333C33333003377FF33733333773339933C3333
        330033377FF73F33337733339933C33333333FF377F373F3333F993399333C33
        330077F377F337F33377993399333C33330077FF773337F33377399993333C33
        33333777733337F333FF333333333C33300033333333373FF7773333333333CC
        3000333333333377377733333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = mniFerFiltrarClick
    end
    object btnSobre: TSpeedButton
      Left = 360
      Top = 0
      Width = 30
      Height = 30
      Hint = 'Sobre'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333F797F3333333333F737373FF333333BFB999BFB
        33333337737773773F3333BFBF797FBFB33333733337333373F33BFBFBFBFBFB
        FB3337F33333F33337F33FBFBFB9BFBFBF3337333337F333373FFBFBFBF97BFB
        FBF37F333337FF33337FBFBFBFB99FBFBFB37F3333377FF3337FFBFBFBFB99FB
        FBF37F33333377FF337FBFBF77BF799FBFB37F333FF3377F337FFBFB99FB799B
        FBF373F377F3377F33733FBF997F799FBF3337F377FFF77337F33BFBF99999FB
        FB33373F37777733373333BFBF999FBFB3333373FF77733F7333333BFBFBFBFB
        3333333773FFFF77333333333FBFBF3333333333377777333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = mniAjudaSobreClick
    end
    object DBNavigator1: TDBNavigator
      Left = 0
      Top = 0
      Width = 300
      Height = 30
      DataSource = DataSource
      Flat = True
      ParentShowHint = False
      ConfirmDelete = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object edtCodigo: TDBEdit
    Left = 15
    Top = 65
    Width = 65
    Height = 21
    TabStop = False
    DataField = 'CODIGO'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object edtNome: TDBEdit
    Left = 91
    Top = 65
    Width = 325
    Height = 21
    DataField = 'NOME'
    DataSource = DataSource
    TabOrder = 2
  end
  object edtEndereco: TDBEdit
    Left = 15
    Top = 115
    Width = 250
    Height = 21
    DataField = 'ENDERECO'
    DataSource = DataSource
    TabOrder = 3
  end
  object edtBairro: TDBEdit
    Left = 276
    Top = 115
    Width = 140
    Height = 21
    DataField = 'BAIRRO'
    DataSource = DataSource
    TabOrder = 4
  end
  object edtCidade: TDBEdit
    Left = 15
    Top = 165
    Width = 249
    Height = 21
    DataField = 'CIDADE'
    DataSource = DataSource
    TabOrder = 5
  end
  object edtCEP: TDBEdit
    Left = 275
    Top = 165
    Width = 100
    Height = 21
    DataField = 'CEP'
    DataSource = DataSource
    TabOrder = 6
  end
  object edtUF: TDBEdit
    Left = 386
    Top = 165
    Width = 30
    Height = 21
    DataField = 'UF'
    DataSource = DataSource
    TabOrder = 7
  end
  object edtFone: TDBEdit
    Left = 15
    Top = 215
    Width = 126
    Height = 21
    DataField = 'FONE'
    DataSource = DataSource
    TabOrder = 8
  end
  object edtFax: TDBEdit
    Left = 153
    Top = 215
    Width = 126
    Height = 21
    DataField = 'FAX'
    DataSource = DataSource
    TabOrder = 9
  end
  object edtCelular: TDBEdit
    Left = 290
    Top = 215
    Width = 126
    Height = 21
    DataField = 'CELULAR'
    DataSource = DataSource
    TabOrder = 10
  end
  object edtEmail: TDBEdit
    Left = 15
    Top = 265
    Width = 400
    Height = 21
    DataField = 'EMAIL'
    DataSource = DataSource
    TabOrder = 11
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 255
    Width = 428
    Height = 118
    Align = alBottom
    Ctl3D = True
    DataSource = DataSource
    ParentCtl3D = False
    TabOrder = 12
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'CODIGO'
        Title.Caption = 'Code'
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NOME'
        Title.Caption = 'Name'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FONE'
        Title.Caption = 'Phone'
        Width = 60
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Email'
        Width = 140
        Visible = True
      end>
  end
  object MDODatabase: TMDODatabase
    AllowStreamedConnected = False
    ClientLib = clFBClient
    DatabaseName = 'localhost:C:\desenv\projetos\mdo\Samples\Agenda\agenda.fb'
    DefaultTransaction = MDOTransaction
    IdleTimer = 0
    LoginPrompt = False
    Params.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    SQLDialect = 3
    TraceFlags = []
    Left = 199
    Top = 35
  end
  object MDODataSet: TMDODataSet
    AfterDelete = MDODataSetAfterDelete
    AfterPost = MDODataSetAfterPost
    BeforeDelete = MDODataSetBeforeDelete
    BeforePost = MDODataSetBeforePost
    Database = MDODatabase
    OnNewRecord = MDODataSetNewRecord
    Transaction = MDOTransaction
    BufferChunks = 32
    CachedUpdates = False
    DeleteSQL.Strings = (
      'DELETE FROM "CONTATO"'
      'WHERE'
      '  "CONTATO"."CODIGO" = :"OLD_CODIGO"')
    InsertSQL.Strings = (
      'INSERT INTO "CONTATO"'
      
        '  ("CONTATO"."CODIGO", "CONTATO"."NOME", "CONTATO"."ENDERECO", "' +
        'CONTATO"."BAIRRO", '
      
        '   "CONTATO"."CIDADE", "CONTATO"."CEP", "CONTATO"."UF", "CONTATO' +
        '"."FONE", '
      '   "CONTATO"."FAX", "CONTATO"."CELULAR", "CONTATO"."Email")'
      'VALUES'
      
        '  (:"CODIGO", :"NOME", :"ENDERECO", :"BAIRRO", :"CIDADE", :"CEP"' +
        ', :"UF", '
      '   :"FONE", :"FAX", :"CELULAR", :"Email")')
    LoadDefaults = False
    ModifySQL.Strings = (
      'UPDATE "CONTATO"'
      'SET'
      '  "CONTATO"."CODIGO" = :"CODIGO",'
      '  "CONTATO"."NOME" = :"NOME",'
      '  "CONTATO"."ENDERECO" = :"ENDERECO",'
      '  "CONTATO"."BAIRRO" = :"BAIRRO",'
      '  "CONTATO"."CIDADE" = :"CIDADE",'
      '  "CONTATO"."CEP" = :"CEP",'
      '  "CONTATO"."UF" = :"UF",'
      '  "CONTATO"."FONE" = :"FONE",'
      '  "CONTATO"."FAX" = :"FAX",'
      '  "CONTATO"."CELULAR" = :"CELULAR",'
      '  "CONTATO"."Email" = :"Email"'
      'WHERE'
      '  "CONTATO"."CODIGO" = :"OLD_CODIGO"')
    RefreshSQL.Strings = (
      'SELECT '
      '  "CONTATO"."CODIGO",'
      '  "CONTATO"."NOME",'
      '  "CONTATO"."ENDERECO",'
      '  "CONTATO"."BAIRRO",'
      '  "CONTATO"."CIDADE",'
      '  "CONTATO"."CEP",'
      '  "CONTATO"."UF",'
      '  "CONTATO"."FONE",'
      '  "CONTATO"."FAX",'
      '  "CONTATO"."CELULAR",'
      '  "CONTATO"."Email"'
      'FROM "CONTATO" '
      'WHERE'
      '  "CONTATO"."CODIGO" = :"CODIGO"')
    SelectSQL.Strings = (
      
        'select CODIGO, NOME, ENDERECO, BAIRRO, CIDADE, CEP, UF, FONE, FA' +
        'X, CELULAR, "Email" from CONTATO')
    Left = 255
    Top = 35
  end
  object DataSource: TDataSource
    AutoEdit = False
    DataSet = ClientDataSet1
    OnStateChange = DataSourceStateChange
    OnDataChange = DataSourceDataChange
    Left = 283
    Top = 35
  end
  object MDOMisc1: TMDOMisc
    Database = MDODatabase
    Transaction = MDOTransaction
    Left = 311
    Top = 35
  end
  object MDOTransaction: TMDOTransaction
    Active = False
    AutoCommit = False
    DefaultDatabase = MDODatabase
    Left = 227
    Top = 35
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider1'
    AfterPost = ClientDataSet1AfterPost
    Left = 176
    Top = 88
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = MDODataSet
    Options = [poAllowCommandText]
    Left = 208
    Top = 88
  end
  object MainMenu1: TMainMenu
    Left = 171
    Top = 35
    object mniArq: TMenuItem
      Caption = '&File'
      object mniArqSair: TMenuItem
        Caption = '&Exit'
        OnClick = mniArqSairClick
      end
    end
    object mniReg: TMenuItem
      Caption = '&Record'
      object mniRegPrimeiro: TMenuItem
        Tag = 1
        Caption = '&First'
        ShortCut = 16420
        OnClick = RegClick
      end
      object mniRegAnterior: TMenuItem
        Tag = 2
        Caption = '&Prior'
        ShortCut = 16417
        OnClick = RegClick
      end
      object mniRegProximo: TMenuItem
        Tag = 3
        Caption = '&Next'
        ShortCut = 16418
        OnClick = RegClick
      end
      object mniRegUltimo: TMenuItem
        Tag = 4
        Caption = '&Last'
        ShortCut = 16419
        OnClick = RegClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mniRegNovo: TMenuItem
        Tag = 5
        Caption = 'N&ew'
        ShortCut = 16429
        OnClick = RegClick
      end
      object mniRegExcluir: TMenuItem
        Tag = 6
        Caption = '&Delete'
        ShortCut = 16430
        OnClick = RegClick
      end
      object mniRegAlterar: TMenuItem
        Tag = 7
        Caption = '&Update'
        ShortCut = 16397
        OnClick = RegClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mniRegSalvar: TMenuItem
        Tag = 8
        Caption = '&Save'
        ShortCut = 16467
        OnClick = RegClick
      end
      object mniRegCancelar: TMenuItem
        Tag = 9
        Caption = '&Cancel'
        ShortCut = 16392
        OnClick = RegClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mniRegAtualizar: TMenuItem
        Tag = 10
        Caption = '&Refresh'
        ShortCut = 116
        OnClick = RegClick
      end
    end
    object mniFer: TMenuItem
      Caption = '&Tools'
      object mniFerLocalizar: TMenuItem
        Caption = '&Search'
        ShortCut = 16460
        OnClick = mniFerLocalizarClick
      end
      object mniFerFiltrar: TMenuItem
        Caption = '&Filter'
        ShortCut = 16454
        OnClick = mniFerFiltrarClick
      end
    end
    object mniAjuda: TMenuItem
      Caption = '&Help'
      object mniAjudaSobre: TMenuItem
        Caption = '&About'
        ShortCut = 112
        OnClick = mniAjudaSobreClick
      end
    end
  end
  object MDOScript1: TMDOScript
    Database = MDODatabase
    Script.Strings = (
      'CREATE TABLE "CONTATO" '
      '('
      '  "CODIGO"'#9'INTEGER NOT NULL,'
      '  "NOME"'#9'VARCHAR(40) NOT NULL,'
      '  "ENDERECO"'#9'VARCHAR(40),'
      '  "BAIRRO"'#9'VARCHAR(20),'
      '  "CIDADE"'#9'VARCHAR(20),'
      '  "CEP"'#9'VARCHAR(10),'
      '  "UF"'#9'CHAR(2),'
      '  "FONE"'#9'VARCHAR(20),'
      '  "FAX"'#9'VARCHAR(20),'
      '  "CELULAR"'#9'VARCHAR(20),'
      '  "Email"'#9'VARCHAR(40),'
      'CONSTRAINT "PK_TELEFONE" PRIMARY KEY ("CODIGO")'
      ');'
      '')
    Terminator = ';'
    Transaction = MDOTransaction
    Left = 344
    Top = 32
  end
end
