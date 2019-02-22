object frmFiltrar: TfrmFiltrar
  Left = 193
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Filter'
  ClientHeight = 128
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object radTodos: TRadioButton
    Left = 15
    Top = 15
    Width = 250
    Height = 17
    Caption = 'All records'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = radTodosClick
  end
  object radNome: TRadioButton
    Left = 15
    Top = 35
    Width = 250
    Height = 17
    Caption = 'Names contain'
    TabOrder = 1
    OnClick = radNomeClick
  end
  object edtNome: TEdit
    Left = 15
    Top = 55
    Width = 250
    Height = 21
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 105
    Top = 90
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 190
    Top = 90
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkCancel
  end
end
