object frmDatabase: TfrmDatabase
  Left = 502
  Top = 104
  BorderStyle = bsDialog
  Caption = 'Editor de Database'
  ClientHeight = 406
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox3: TGroupBox
    Left = 8
    Top = 152
    Width = 361
    Height = 217
    Caption = 'Parametros do Database'
    TabOrder = 1
    object Label2: TLabel
      Left = 24
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Usuário:'
    end
    object Label3: TLabel
      Left = 24
      Top = 64
      Width = 34
      Height = 13
      Caption = 'Senha:'
    end
    object Label4: TLabel
      Left = 24
      Top = 104
      Width = 49
      Height = 13
      Caption = 'SQL Role:'
    end
    object Label5: TLabel
      Left = 24
      Top = 144
      Width = 66
      Height = 13
      Caption = 'Character set:'
    end
    object Label6: TLabel
      Left = 136
      Top = 24
      Width = 71
      Height = 13
      Caption = 'Configurações:'
    end
    object UserName: TEdit
      Left = 24
      Top = 40
      Width = 100
      Height = 21
      TabOrder = 0
      OnChange = UserNameChange
    end
    object Password: TEdit
      Left = 24
      Top = 80
      Width = 100
      Height = 21
      TabOrder = 1
      OnChange = PasswordChange
    end
    object SQLRole: TEdit
      Left = 24
      Top = 120
      Width = 100
      Height = 21
      TabOrder = 2
      OnChange = SQLRoleChange
    end
    object CharacterSet: TComboBox
      Left = 24
      Top = 160
      Width = 100
      Height = 21
      ItemHeight = 13
      TabOrder = 3
      Text = 'None'
      OnChange = CharacterSetChange
      Items.Strings = (
        'None'
        'ASCII'
        'BIG_5'
        'BYRL'
        'DOS437'
        'DOS850'
        'DOS852'
        'DOS857'
        'DOS860'
        'DOS861'
        'DOS863'
        'DOS865'
        'EUCJ_0208'
        'GB_2312'
        'ISO8859_1'
        'KSC_5601'
        'NEXT'
        'OCTETS'
        'SJIS_0208'
        'UNICODE_FSS'
        'WIN1250'
        'WIN1251'
        'WIN1252'
        'WIN1253'
        'WIN1254')
    end
    object LoginPrompt: TCheckBox
      Left = 24
      Top = 192
      Width = 97
      Height = 17
      Caption = 'LoginPrompt'
      TabOrder = 4
    end
    object DatabaseParams: TMemo
      Left = 136
      Top = 40
      Width = 201
      Height = 161
      TabOrder = 5
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 361
    Height = 137
    Caption = 'Conexão'
    TabOrder = 0
    object Label8: TLabel
      Left = 152
      Top = 44
      Width = 42
      Height = 13
      Caption = 'Protocol:'
      Enabled = False
    end
    object Label7: TLabel
      Left = 16
      Top = 44
      Width = 34
      Height = 13
      Caption = 'Server:'
      Enabled = False
    end
    object Label9: TLabel
      Left = 16
      Top = 88
      Width = 49
      Height = 13
      Caption = 'Database:'
    end
    object LocalRbtn: TRadioButton
      Left = 16
      Top = 24
      Width = 73
      Height = 17
      Caption = 'Local'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = LocalRbtnClick
    end
    object RemoteRbtn: TRadioButton
      Left = 104
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Remote'
      TabOrder = 1
      OnClick = LocalRbtnClick
    end
    object ServerName: TEdit
      Left = 16
      Top = 60
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 2
    end
    object Protocol: TComboBox
      Left = 152
      Top = 60
      Width = 97
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        'TCP'
        'NamedPiped'
        'SPX')
    end
    object Browse: TButton
      Left = 256
      Top = 60
      Width = 75
      Height = 25
      Caption = '&Browse'
      TabOrder = 4
      OnClick = BrowseClick
    end
    object DatabaseName: TEdit
      Left = 16
      Top = 104
      Width = 321
      Height = 21
      TabOrder = 5
    end
  end
  object OkBtn: TButton
    Left = 71
    Top = 376
    Width = 75
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 151
    Top = 376
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Test: TButton
    Left = 231
    Top = 376
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 4
    OnClick = TestClick
  end
end
