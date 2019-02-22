object frmMain: TfrmMain
  Left = 237
  Top = 156
  Width = 515
  Height = 446
  Caption = 'MDO Interactive SQL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 241
    Width = 507
    Height = 3
    Cursor = crVSplit
    Align = alTop
    Beveled = True
    Color = clBtnFace
    ParentColor = False
  end
  object Label1: TLabel
    Left = 0
    Top = 387
    Width = 507
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 368
    Width = 507
    Height = 19
    Panels = <
      item
        Width = 85
      end
      item
        Width = 120
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 507
    Height = 29
    BorderWidth = 1
    ButtonHeight = 19
    ButtonWidth = 66
    Caption = 'ToolBar1'
    Flat = True
    List = True
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 3
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = actConectar
      Caption = 'Connect'
    end
    object ToolButton2: TToolButton
      Left = 66
      Top = 0
      Action = actDesconectar
      Caption = 'Disconnect'
    end
    object ToolButton6: TToolButton
      Left = 132
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 140
      Top = 0
      Action = actCommit
    end
    object ToolButton4: TToolButton
      Left = 206
      Top = 0
      Action = actRollback
    end
    object ToolButton7: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 4
      Wrap = True
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 0
      Top = 27
      Action = actDstExecutar
    end
    object ToolButton8: TToolButton
      Left = 66
      Top = 27
      Width = 8
      Caption = 'ToolButton8'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton9: TToolButton
      Left = 74
      Top = 27
      Action = actScrPrior
    end
    object ToolButton10: TToolButton
      Left = 140
      Top = 27
      Action = actScrNext
    end
    object ToolButton11: TToolButton
      Left = 206
      Top = 27
      Action = actScrNew
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 244
    Width = 507
    Height = 124
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlBottom'
    TabOrder = 1
    object Splitter3: TSplitter
      Left = 319
      Top = 0
      Height = 124
      Align = alRight
      Beveled = True
      Color = clBtnFace
      ParentColor = False
    end
    object pnlResult: TPanel
      Left = 0
      Top = 0
      Width = 319
      Height = 124
      Align = alClient
      BevelInner = bvLowered
      BorderWidth = 3
      Caption = 'pnlResult'
      TabOrder = 0
      object DBGrid: TDBGrid
        Left = 5
        Top = 5
        Width = 309
        Height = 114
        Align = alClient
        BorderStyle = bsNone
        DataSource = DataSource
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object Panel1: TPanel
      Left = 322
      Top = 0
      Width = 185
      Height = 124
      Align = alRight
      BevelInner = bvLowered
      BorderWidth = 3
      Caption = 'Panel1'
      TabOrder = 1
      object Splitter2: TSplitter
        Left = 5
        Top = 21
        Width = 175
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Beveled = True
        Color = clBtnFace
        ParentColor = False
      end
      object mmoMonitor: TMemo
        Left = 5
        Top = 5
        Width = 175
        Height = 16
        Align = alClient
        BiDiMode = bdLeftToRight
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentBiDiMode = False
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object mmoErro: TMemo
        Left = 5
        Top = 24
        Width = 175
        Height = 95
        Align = alBottom
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object pnlEditor: TPanel
    Left = 0
    Top = 29
    Width = 507
    Height = 212
    Align = alTop
    BevelInner = bvLowered
    BorderWidth = 3
    TabOrder = 0
    object mmoSQL: TMemo
      Left = 5
      Top = 5
      Width = 497
      Height = 202
      Cursor = crIBeam
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object ActionList1: TActionList
    Left = 160
    Top = 32
    object actConectar: TAction
      Category = 'Database'
      Caption = '&Conectar'
      OnExecute = actConectarExecute
    end
    object actCriarDB: TAction
      Category = 'Database'
      Caption = '&Criar'
      OnExecute = actCriarDBExecute
    end
    object actDropDB: TAction
      Category = 'Database'
      Caption = '&Drop'
      OnExecute = actDropDBExecute
    end
    object actDesconectar: TAction
      Category = 'Database'
      Caption = '&Desconectar'
      OnExecute = actDesconectarExecute
    end
    object actDescForce: TAction
      Category = 'Database'
      Caption = '&For'#231'ar desconex'#227'o'
      OnExecute = actDescForceExecute
    end
    object actUsuarios: TAction
      Category = 'Database'
      Caption = '&Usu'#225'rios'
      OnExecute = actUsuariosExecute
    end
    object actErros: TAction
      Category = 'Database'
      Caption = '&Exibir erros'
      OnExecute = actErrosExecute
    end
    object actTraParametros: TAction
      Category = 'Transacao'
      Caption = '&Par'#226'metros'
      OnExecute = actTraParametrosExecute
    end
    object actCommit: TAction
      Category = 'Transacao'
      Caption = '&Commit'
      OnExecute = actCommitExecute
    end
    object actCommitRetaining: TAction
      Category = 'Transacao'
      Caption = 'C&ommit retaining'
      OnExecute = actCommitRetainingExecute
    end
    object actRollback: TAction
      Category = 'Transacao'
      Caption = '&Rollback'
      OnExecute = actRollbackExecute
    end
    object actDstExecutar: TAction
      Category = 'Dataset'
      Caption = '&Executar'
      ShortCut = 16397
      OnExecute = actDstExecutarExecute
    end
    object actDstPlan: TAction
      Category = 'Dataset'
      Caption = 'E&xibir plan'
      OnExecute = actDstPlanExecute
    end
    object actSalvarResultado: TAction
      Category = 'Dataset'
      Caption = '&Salvar resultado'
      OnExecute = actSalvarResultadoExecute
    end
    object actScrNext: TAction
      Category = 'Script'
      Caption = 'Next >>'
      OnExecute = actScrNextExecute
    end
    object actScrPrior: TAction
      Category = 'Script'
      Caption = '<< Prior'
      ShortCut = 16464
      OnExecute = actScrPriorExecute
    end
    object actScrNew: TAction
      Category = 'Script'
      Caption = 'New'
      OnExecute = actScrNewExecute
    end
    object actArqSair: TAction
      Category = 'Arquivo'
      Caption = 'Sair'
      OnExecute = actArqSairExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 128
    Top = 32
    object Arquivo1: TMenuItem
      Caption = 'File'
      object Sair1: TMenuItem
        Action = actArqSair
        Caption = 'Exit'
      end
    end
    object Database1: TMenuItem
      Caption = '&Database'
      object Conectar1: TMenuItem
        Action = actConectar
        Caption = 'Connect'
      end
      object Desconectar1: TMenuItem
        Action = actDesconectar
        Caption = 'Disconnect'
      end
      object Forardesconexo1: TMenuItem
        Action = actDescForce
        Caption = 'Forced disconnection'
      end
      object Criar1: TMenuItem
        Action = actCriarDB
        Caption = 'Create'
      end
      object Drop1: TMenuItem
        Action = actDropDB
      end
      object Exibirerros1: TMenuItem
        Action = actErros
        Caption = 'Show errors'
      end
      object Usurios1: TMenuItem
        Action = actUsuarios
        Caption = 'Users'
      end
    end
    object Transaction1: TMenuItem
      Caption = '&Transaction'
      object Commit1: TMenuItem
        Action = actCommit
      end
      object Commitretaining1: TMenuItem
        Action = actCommitRetaining
      end
      object Rollback1: TMenuItem
        Action = actRollback
      end
      object Parmetros1: TMenuItem
        Action = actTraParametros
        Caption = 'Parameters'
      end
    end
    object SQL1: TMenuItem
      Caption = '&SQL'
      object Executar1: TMenuItem
        Action = actDstExecutar
        Caption = 'Execute'
      end
      object Exibirplan1: TMenuItem
        Action = actDstPlan
        Caption = 'Show plan'
      end
      object Salvarresultado1: TMenuItem
        Action = actSalvarResultado
        Caption = 'Save result'
      end
    end
  end
  object MDODatabase: TMDODatabase
    AfterConnect = MDODatabaseAfterConnect
    AfterDisconnect = MDODatabaseAfterdisconnect
    ClientLib = clFBClient
    DatabaseName = '..\Agenda\Agenda.fb'
    DefaultTransaction = MDOTransaction
    IdleTimer = 0
    LoginPrompt = False
    OnClientLibChange = MDODatabaseClientLibChange
    OnLogin = MDODatabaseLogin
    Params.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    SQLDialect = 3
    TraceFlags = [tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect, tfTransact, tfBlob, tfService, tfMisc]
    Left = 96
    Top = 72
  end
  object MDOTransaction: TMDOTransaction
    Active = False
    AfterCommit = MDOTransactionAfterCommit
    AfterRollback = MDOTransactionAfterRollback
    AutoCommit = False
    DefaultDatabase = MDODatabase
    OnEndTransaction = MDOTransactionEndTransaction
    OnStartTransaction = MDOTransactionStartTransaction
    Left = 128
    Top = 72
  end
  object MDODataSet: TMDODataSet
    Database = MDODatabase
    Transaction = MDOTransaction
    BufferChunks = 32
    CachedUpdates = False
    LoadDefaults = False
    Left = 160
    Top = 72
  end
  object DataSource: TDataSource
    DataSet = MDODataSet
    Left = 224
    Top = 72
  end
  object OpenDialog: TOpenDialog
    Left = 192
    Top = 32
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    Left = 224
    Top = 32
  end
  object MDOSQLMonitor1: TMDOSQLMonitor
    OnSQL = MDOSQLMonitor1SQL
    TraceFlags = [tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect, tfTransact, tfBlob, tfService, tfMisc]
    Left = 192
    Top = 72
  end
end
