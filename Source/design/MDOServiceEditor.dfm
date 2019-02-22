object MDOServiceEditorForm: TMDOServiceEditorForm
  Left = 313
  Top = 114
  BorderStyle = bsDialog
  Caption = 'MDO Service Editor'
  ClientHeight = 336
  ClientWidth = 378
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
  object GroupBox1: TGroupBox
    Left = 5
    Top = 5
    Width = 366
    Height = 141
    Caption = '  Connection  '
    TabOrder = 0
    object lServer: TLabel
      Left = 15
      Top = 45
      Width = 31
      Height = 13
      Caption = '&Server'
      Enabled = False
      FocusControl = eServer
    end
    object lProtocol: TLabel
      Left = 150
      Top = 45
      Width = 39
      Height = 13
      Caption = '&Protocol'
      Enabled = False
      FocusControl = cbProtocol
    end
    object Label3: TLabel
      Left = 15
      Top = 90
      Width = 46
      Height = 13
      Caption = '&Database'
      FocusControl = eDataBase
    end
    object cbServer: TComboBox
      Left = 15
      Top = 15
      Width = 126
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbServerChange
      Items.Strings = (
        'Local'
        'Remote')
    end
    object cbProtocol: TComboBox
      Left = 150
      Top = 60
      Width = 126
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'TCP'
        'NamedPipe'
        'IPX')
    end
    object eServer: TEdit
      Left = 15
      Top = 60
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 2
    end
    object eDataBase: TEdit
      Left = 15
      Top = 105
      Width = 256
      Height = 21
      TabOrder = 3
    end
    object bBrowse: TButton
      Left = 280
      Top = 102
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 4
      OnClick = bBrowseClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 5
    Top = 150
    Width = 366
    Height = 151
    Caption = '  Database Parameters  '
    TabOrder = 1
    object Label4: TLabel
      Left = 10
      Top = 20
      Width = 53
      Height = 13
      Caption = '&User Name'
      FocusControl = eUsr
    end
    object Label5: TLabel
      Left = 10
      Top = 65
      Width = 46
      Height = 13
      Caption = 'Pass&word'
      FocusControl = ePwd
    end
    object Label6: TLabel
      Left = 175
      Top = 20
      Width = 38
      Height = 13
      Caption = '&Settings'
      FocusControl = mSettings
    end
    object eUsr: TEdit
      Left = 10
      Top = 35
      Width = 121
      Height = 21
      TabOrder = 0
      OnChange = eUsrChange
    end
    object ePwd: TEdit
      Left = 10
      Top = 80
      Width = 121
      Height = 21
      TabOrder = 1
      OnChange = ePwdChange
    end
    object mSettings: TMemo
      Left = 175
      Top = 35
      Width = 185
      Height = 106
      TabOrder = 2
    end
    object cbActive: TCheckBox
      Left = 10
      Top = 105
      Width = 97
      Height = 17
      Caption = '&Active'
      TabOrder = 3
    end
    object cbLoginPrompt: TCheckBox
      Left = 10
      Top = 125
      Width = 97
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 4
    end
  end
  object bOk: TButton
    Left = 114
    Top = 305
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
  end
  object bCancel: TButton
    Left = 189
    Top = 305
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
