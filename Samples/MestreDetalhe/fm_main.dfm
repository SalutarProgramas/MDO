object frmMain: TfrmMain
  Left = 346
  Top = 117
  Width = 298
  Height = 396
  Caption = 'Mestre / Detalhe'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 356
    Width = 290
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
  object gbxMestre: TGroupBox
    Left = 0
    Top = 0
    Width = 290
    Height = 201
    Align = alTop
    Caption = 'Master'
    TabOrder = 0
    object navMestre: TDBNavigator
      Left = 25
      Top = 16
      Width = 240
      Height = 25
      DataSource = DataSourceMestre
      Flat = True
      TabOrder = 0
    end
    object grdMestre: TDBGrid
      Left = 2
      Top = 48
      Width = 286
      Height = 151
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      DataSource = DataSourceMestre
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object gbxDetalhe: TGroupBox
    Left = 0
    Top = 201
    Width = 290
    Height = 155
    Align = alClient
    Caption = 'Detail'
    TabOrder = 1
    object navDetalhe: TDBNavigator
      Left = 25
      Top = 16
      Width = 240
      Height = 25
      DataSource = DataSourceDetalhe
      Flat = True
      TabOrder = 0
    end
    object grdDetalhe: TDBGrid
      Left = 2
      Top = 48
      Width = 286
      Height = 105
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      DataSource = DataSourceDetalhe
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object MDODatabase1: TMDODatabase
    DatabaseName = '.\MESTREDETALHE.GDB'
    DefaultTransaction = MDOTransaction1
    IdleTimer = 0
    LoginPrompt = False
    Params.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    SQLDialect = 3
    TraceFlags = []
    Left = 64
    Top = 128
  end
  object MDOTransaction1: TMDOTransaction
    Active = False
    AutoCommit = True
    DefaultDatabase = MDODatabase1
    Left = 96
    Top = 128
  end
  object MDOScript1: TMDOScript
    Database = MDODatabase1
    Script.Strings = (
      'CREATE TABLE MESTRE('
      '  COD_MESTRE INTEGER NOT NULL PRIMARY KEY,'
      '  DES_MESTRE VARCHAR(10)'
      ');'
      ''
      'CREATE TABLE DETALHE('
      '  COD_DETALHE INTEGER NOT NULL PRIMARY KEY,'
      '  COD_MESTRE INTEGER NOT NULL,'
      '  DES_DETALHE VARCHAR(10)'
      ');'
      ''
      
        'ALTER TABLE DETALHE ADD CONSTRAINT FK_DETALHE FOREIGN KEY (COD_M' +
        'ESTRE) REFERENCES MESTRE (COD_MESTRE);'
      ''
      'CREATE GENERATOR GEN_MESTRE;'
      'CREATE GENERATOR GEN_DETALHE;'
      ''
      'SET GENERATOR GEN_MESTRE TO 3;'
      'SET GENERATOR GEN_DETALHE TO 9;'
      ''
      'INSERT INTO MESTRE(COD_MESTRE, DES_MESTRE)VALUES(1, '#39'Mestre A'#39');'
      'INSERT INTO MESTRE(COD_MESTRE, DES_MESTRE)VALUES(2, '#39'Mestre B'#39');'
      'INSERT INTO MESTRE(COD_MESTRE, DES_MESTRE)VALUES(3, '#39'Mestre C'#39');'
      ''
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '1, 1, '#39'Detalhe AA'#39');'
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '2, 1, '#39'Detalhe AB'#39');'
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '3, 1, '#39'Detalhe AC'#39');'
      ''
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '4, 2, '#39'Detalhe BA'#39');'
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '5, 2, '#39'Detalhe BB'#39');'
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '6, 2, '#39'Detalhe BC'#39');'
      ''
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '7, 3, '#39'Detalhe CA'#39');'
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '8, 3, '#39'Detalhe CB'#39');'
      
        'INSERT INTO DETALHE(COD_DETALHE, COD_MESTRE, DES_DETALHE)VALUES(' +
        '9, 3, '#39'Detalhe CC'#39');')
    Terminator = ';'
    Transaction = MDOTransaction1
    Left = 128
    Top = 128
  end
  object MDODataSetMestre: TMDODataSet
    Database = MDODatabase1
    Transaction = MDOTransaction1
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'DELETE FROM MESTRE'
      'WHERE'
      '  COD_MESTRE = :OLD_COD_MESTRE')
    GeneratorLink.Field = 'COD_MESTRE'
    GeneratorLink.Generator = 'GEN_MESTRE'
    InsertSQL.Strings = (
      'INSERT INTO MESTRE'
      '  (COD_MESTRE, DES_MESTRE)'
      'VALUES'
      '  (:COD_MESTRE, :DES_MESTRE)')
    LoadDefaults = False
    ModifySQL.Strings = (
      'UPDATE MESTRE'
      'SET'
      '  COD_MESTRE = :COD_MESTRE,'
      '  DES_MESTRE = :DES_MESTRE'
      'WHERE'
      '  COD_MESTRE = :OLD_COD_MESTRE')
    RefreshSQL.Strings = (
      'SELECT '
      '  COD_MESTRE,'
      '  DES_MESTRE'
      'FROM MESTRE '
      'WHERE'
      '  COD_MESTRE = :COD_MESTRE')
    SelectSQL.Strings = (
      'select COD_MESTRE, DES_MESTRE from MESTRE')
    Left = 88
    Top = 56
  end
  object MDODataSetDetalhe: TMDODataSet
    Database = MDODatabase1
    Transaction = MDOTransaction1
    BufferChunks = 1000
    CachedUpdates = False
    DataSource = DataSourceMestre
    DeleteSQL.Strings = (
      'DELETE FROM DETALHE'
      'WHERE'
      '  COD_DETALHE = :OLD_COD_DETALHE')
    GeneratorLink.Field = 'COD_DETALHE'
    GeneratorLink.Generator = 'GEN_DETALHE'
    InsertSQL.Strings = (
      'INSERT INTO DETALHE'
      '  (COD_DETALHE, COD_MESTRE, DES_DETALHE)'
      'VALUES'
      '  (:COD_DETALHE, :COD_MESTRE, :DES_DETALHE)')
    LoadDefaults = False
    ModifySQL.Strings = (
      'UPDATE DETALHE'
      'SET'
      '  COD_DETALHE = :COD_DETALHE,'
      '  COD_MESTRE = :COD_MESTRE,'
      '  DES_DETALHE = :DES_DETALHE'
      'WHERE'
      '  COD_DETALHE = :OLD_COD_DETALHE')
    RefreshSQL.Strings = (
      'SELECT '
      '  COD_DETALHE,'
      '  COD_MESTRE,'
      '  DES_DETALHE'
      'FROM DETALHE '
      'WHERE'
      '  COD_DETALHE = :COD_DETALHE')
    SelectSQL.Strings = (
      'select COD_DETALHE, COD_MESTRE, DES_DETALHE from DETALHE'
      'where COD_MESTRE = :COD_MESTRE')
    Left = 88
    Top = 256
  end
  object DataSourceMestre: TDataSource
    DataSet = MDODataSetMestre
    Left = 120
    Top = 56
  end
  object DataSourceDetalhe: TDataSource
    DataSet = MDODataSetDetalhe
    Left = 120
    Top = 256
  end
end
