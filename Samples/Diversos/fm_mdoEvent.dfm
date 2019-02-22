object Form2: TForm2
  Left = 438
  Top = 237
  Width = 469
  Height = 136
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 264
    Top = 8
    Width = 62
    Height = 13
    Caption = 'Event Before'
  end
  object Label2: TLabel
    Left = 264
    Top = 56
    Width = 53
    Height = 13
    Caption = 'Event After'
  end
  object btnTable: TButton
    Left = 40
    Top = 8
    Width = 193
    Height = 25
    Caption = 'Insert by MDOTable'
    TabOrder = 0
    OnClick = btnTableClick
  end
  object edtBefore: TEdit
    Left = 264
    Top = 24
    Width = 193
    Height = 21
    TabOrder = 1
    Text = 'edtBefore'
  end
  object edtAfter: TEdit
    Left = 264
    Top = 72
    Width = 193
    Height = 21
    TabOrder = 2
    Text = 'edtAfter'
  end
  object btnQuery: TButton
    Left = 40
    Top = 40
    Width = 193
    Height = 25
    Caption = 'Insert by MDOQuery'
    TabOrder = 3
    OnClick = btnQueryClick
  end
  object btnDataset: TButton
    Left = 40
    Top = 72
    Width = 193
    Height = 25
    Caption = 'Insert by MDODataset'
    TabOrder = 4
    OnClick = btnDatasetClick
  end
  object MDODatabase: TMDODatabase
    ClientLib = clFBClient
    Connected = True
    DatabaseName = 'MDOEVENTS.FDB'
    DefaultTransaction = MDOTransaction
    IdleTimer = 0
    LoginPrompt = False
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    SQLDialect = 3
    TraceFlags = []
    Left = 8
    Top = 8
  end
  object MDOTransaction: TMDOTransaction
    Active = True
    AutoCommit = True
    DefaultDatabase = MDODatabase
    Left = 8
    Top = 40
  end
  object MDOScript: TMDOScript
    Database = MDODatabase
    Script.Strings = (
      'CREATE TABLE NOMES(CODIGO INTEGER, NOME VARCHAR(200));')
    Terminator = ';'
    Transaction = MDOTransaction
    Left = 8
    Top = 72
  end
  object MDOTable: TMDOTable
    AfterPost = MDOTableAfterPost
    BeforePost = MDOTableBeforePost
    Database = MDODatabase
    Transaction = MDOTransaction
    BufferChunks = 1000
    CachedUpdates = False
    TableName = 'NOMES'
    Left = 240
    Top = 8
    object MDOTableCODIGO: TIntegerField
      FieldName = 'CODIGO'
    end
    object MDOTableNOME: TMDOStringField
      FieldName = 'NOME'
      Size = 200
    end
  end
  object MDOQuery: TMDOQuery
    AfterPost = MDOQueryAfterPost
    BeforePost = MDOQueryBeforePost
    Database = MDODatabase
    Transaction = MDOTransaction
    BufferChunks = 1000
    CachedUpdates = False
    LoadDefaults = False
    SQL.Strings = (
      'select CODIGO, NOME from NOMES')
    UpdateObject = MDOUpdateSQL
    Left = 240
    Top = 40
    object MDOQueryCODIGO: TIntegerField
      FieldName = 'CODIGO'
    end
    object MDOQueryNOME: TMDOStringField
      FieldName = 'NOME'
      Size = 200
    end
  end
  object MDOUpdateSQL: TMDOUpdateSQL
    RefreshSQL.Strings = (
      'SELECT '
      '  CODIGO,'
      '  NOME'
      'FROM NOMES '
      'WHERE'
      '  CODIGO = :CODIGO')
    DeleteSQL.Strings = (
      'DELETE FROM NOMES'
      'WHERE'
      '  CODIGO = :OLD_CODIGO')
    InsertSQL.Strings = (
      'INSERT INTO NOMES'
      '  (CODIGO, NOME)'
      'VALUES'
      '  (:CODIGO, :NOME)')
    ModifySQL.Strings = (
      'UPDATE NOMES'
      'SET'
      '  CODIGO = :CODIGO,'
      '  NOME = :NOME'
      'WHERE'
      '  CODIGO = :OLD_CODIGO')
    Left = 256
    Top = 40
  end
  object MDODataSet: TMDODataSet
    AfterPost = MDODataSetAfterPost
    BeforePost = MDODataSetBeforePost
    Database = MDODatabase
    Transaction = MDOTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'DELETE FROM NOMES'
      'WHERE'
      '  CODIGO = :OLD_CODIGO')
    InsertSQL.Strings = (
      'INSERT INTO NOMES'
      '  (CODIGO, NOME)'
      'VALUES'
      '  (:CODIGO, :NOME)')
    LoadDefaults = False
    ModifySQL.Strings = (
      'UPDATE NOMES'
      'SET'
      '  CODIGO = :CODIGO,'
      '  NOME = :NOME'
      'WHERE'
      '  CODIGO = :OLD_CODIGO')
    RefreshSQL.Strings = (
      'SELECT '
      '  CODIGO,'
      '  NOME'
      'FROM NOMES '
      'WHERE'
      '  CODIGO = :CODIGO')
    SelectSQL.Strings = (
      'select CODIGO, NOME from NOMES')
    Left = 240
    Top = 72
  end
end
