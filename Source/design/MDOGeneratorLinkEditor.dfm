object MDOGeneratorLinkEditForm: TMDOGeneratorLinkEditForm
  Left = 236
  Top = 194
  BorderStyle = bsDialog
  Caption = 'MDOGeneratorLinkEditForm'
  ClientHeight = 137
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 13
    Width = 47
    Height = 13
    Alignment = taRightJustify
    Caption = '&Generator'
    FocusControl = cbxGenerators
  end
  object Label2: TLabel
    Left = 10
    Top = 41
    Width = 22
    Height = 13
    Alignment = taRightJustify
    Caption = '&Field'
    FocusControl = cbxFields
  end
  object Label3: TLabel
    Left = 10
    Top = 68
    Width = 62
    Height = 13
    Alignment = taRightJustify
    Caption = 'Increment By'
  end
  object btnOk: TButton
    Left = 129
    Top = 102
    Width = 75
    Height = 24
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 216
    Top = 102
    Width = 75
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbxGenerators: TComboBox
    Left = 90
    Top = 10
    Width = 170
    Height = 21
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    Text = 'cbxGenerators'
    Items.Strings = (
      '1'
      '2'
      '3')
  end
  object cbxFields: TComboBox
    Left = 90
    Top = 37
    Width = 170
    Height = 21
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    Text = 'cbxFields'
    Items.Strings = (
      'a'
      'b'
      'c')
  end
  object grpWhereApply: TRadioGroup
    Left = 272
    Top = 4
    Width = 136
    Height = 81
    Caption = '&Where apply'
    ItemIndex = 0
    Items.Strings = (
      'On New Record'
      'On Post'
      'On Server')
    TabOrder = 4
  end
  object edtIncrement: TEdit
    Left = 90
    Top = 64
    Width = 170
    Height = 21
    TabOrder = 5
    Text = '1'
  end
end
