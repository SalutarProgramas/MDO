object MDOUpdateSQLEditForm: TMDOUpdateSQLEditForm
  Left = 216
  Top = 240
  BorderStyle = bsDialog
  Caption = 'Update SQL Editor'
  ClientHeight = 326
  ClientWidth = 543
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 281
    Width = 543
    Height = 45
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object okButton: TButton
      Left = 152
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object CancelButton: TButton
      Left = 240
      Top = 10
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 328
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpButtonClick
    end
  end
  object pcoUpdate: TPageControl
    Left = 0
    Top = 0
    Width = 543
    Height = 281
    ActivePage = FieldsPage
    Align = alTop
    MultiLine = True
    ScrollOpposite = True
    TabOrder = 0
    OnChanging = pcoUpdateChanging
    object FieldsPage: TTabSheet
      Caption = 'Options'
      DesignSize = (
        535
        253)
      object gboSQLGeneration: TGroupBox
        Left = 8
        Top = 8
        Width = 520
        Height = 236
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'SQL Generation'
        TabOrder = 0
        object Label1: TLabel
          Left = 15
          Top = 20
          Width = 59
          Height = 13
          Caption = 'Table name:'
        end
        object Label2: TLabel
          Left = 190
          Top = 20
          Width = 48
          Height = 13
          Caption = 'Key fields:'
        end
        object Label3: TLabel
          Left = 350
          Top = 20
          Width = 65
          Height = 13
          Caption = 'Update fields:'
        end
        object QuoteFields: TCheckBox
          Left = 15
          Top = 190
          Width = 97
          Height = 17
          Caption = 'Quote identifiers'
          TabOrder = 5
        end
        object GenerateButton: TButton
          Left = 15
          Top = 160
          Width = 150
          Height = 25
          Caption = 'Generate SQL'
          TabOrder = 4
          OnClick = GenerateButtonClick
        end
        object PrimaryKeyButton: TButton
          Left = 15
          Top = 130
          Width = 150
          Height = 25
          Caption = 'Select primary keys'
          TabOrder = 3
          OnClick = PrimaryKeyButtonClick
        end
        object DefaultButton: TButton
          Left = 15
          Top = 100
          Width = 150
          Height = 25
          Caption = 'Dataset defaults'
          TabOrder = 2
          OnClick = DefaultButtonClick
        end
        object GetTableFieldsButton: TButton
          Left = 15
          Top = 70
          Width = 150
          Height = 25
          Caption = 'Get table fields'
          TabOrder = 1
          OnClick = GetTableFieldsButtonClick
        end
        object UpdateTableName: TComboBox
          Left = 15
          Top = 40
          Width = 150
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          OnChange = UpdateTableNameChange
          OnClick = UpdateTableNameClick
        end
        object KeyFieldList: TListBox
          Left = 190
          Top = 40
          Width = 150
          Height = 165
          ItemHeight = 13
          MultiSelect = True
          PopupMenu = FieldListPopup
          TabOrder = 6
        end
        object UpdateFieldList: TListBox
          Left = 350
          Top = 40
          Width = 150
          Height = 165
          ItemHeight = 13
          MultiSelect = True
          PopupMenu = FieldListPopup
          TabOrder = 7
        end
        object IgnoreCalculedFields: TCheckBox
          Left = 15
          Top = 208
          Width = 121
          Height = 17
          Caption = 'Ignore calculed fields'
          TabOrder = 8
        end
      end
    end
    object SQLPage: TTabSheet
      Caption = 'Script'
      ImageIndex = 1
      DesignSize = (
        535
        253)
      object Label4: TLabel
        Left = 8
        Top = 65
        Width = 44
        Height = 13
        Caption = 'SQL text:'
      end
      object SQLMemo: TMemo
        Left = 8
        Top = 80
        Width = 522
        Height = 161
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 0
        OnKeyPress = SQLMemoKeyPress
      end
      object StatementType: TRadioGroup
        Left = 8
        Top = 8
        Width = 522
        Height = 52
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'StatementType'
        Columns = 4
        ItemIndex = 0
        Items.Strings = (
          'Modify'
          'Insert'
          'Delete'
          'Refresh')
        TabOrder = 1
        OnClick = StatementTypeClick
      end
    end
  end
  object FieldListPopup: TPopupMenu
    Left = 408
    Top = 288
    object miSelectAll: TMenuItem
      Caption = 'Select all'
      OnClick = miSelectAllClick
    end
    object miClearAll: TMenuItem
      Caption = 'Clear all'
      OnClick = miClearAllClick
    end
  end
end
