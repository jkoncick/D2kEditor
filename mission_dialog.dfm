object MissionDialog: TMissionDialog
  Left = 192
  Top = 111
  Width = 1024
  Height = 560
  Caption = 'Mission settings'
  Color = clBtnFace
  Constraints.MaxWidth = 1024
  Constraints.MinHeight = 560
  Constraints.MinWidth = 1024
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SettingsPanel: TPanel
    Left = 0
    Top = 0
    Width = 714
    Height = 526
    Align = alClient
    TabOrder = 0
    object PlayerSettingsPanel: TPanel
      Left = 1
      Top = 1
      Width = 712
      Height = 288
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblTechLevel: TLabel
        Left = 72
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Tech level'
      end
      object lblStartingMoney: TLabel
        Left = 136
        Top = 8
        Width = 71
        Height = 13
        Caption = 'Starting Money'
      end
      object lblAllocIndex: TLabel
        Left = 224
        Top = 8
        Width = 54
        Height = 13
        Caption = 'Alloc. index'
      end
      object lblSetToAll: TLabel
        Left = 8
        Top = 228
        Width = 44
        Height = 13
        Caption = 'Set to all:'
      end
      object lblTimeLimit: TLabel
        Left = 8
        Top = 260
        Width = 46
        Height = 13
        Caption = 'Time limit:'
      end
      object seTechLevelAll: TSpinEdit
        Left = 72
        Top = 224
        Width = 56
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = seTechLevelAllChange
      end
      object edStartingMoneyAll: TEdit
        Left = 136
        Top = 224
        Width = 80
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = edStartingMoneyAllChange
      end
      object btnAllocIndexReset: TButton
        Left = 224
        Top = 224
        Width = 56
        Height = 22
        Caption = 'Reset'
        TabOrder = 3
        OnClick = btnAllocIndexResetClick
      end
      object btnAllegianceReset: TButton
        Left = 288
        Top = 224
        Width = 105
        Height = 22
        Caption = 'Reset allegiance'
        TabOrder = 4
        OnClick = btnAllegianceResetClick
      end
      object edTimeLimit: TEdit
        Left = 72
        Top = 256
        Width = 97
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = time_limit_change
      end
      object cbSetBothSides: TCheckBox
        Left = 400
        Top = 228
        Width = 113
        Height = 17
        Caption = 'Auto-set both sides'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
    end
    object RulesAndStringsPanel: TPanel
      Left = 1
      Top = 289
      Width = 712
      Height = 236
      Align = alClient
      TabOrder = 1
    end
  end
  object AITabControl: TTabControl
    Left = 714
    Top = 0
    Width = 302
    Height = 526
    Align = alRight
    MultiLine = True
    TabOrder = 1
    TabWidth = 74
    OnChange = AITabControlChange
    object AIValueList: TValueListEditor
      Left = 4
      Top = 6
      Width = 294
      Height = 492
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      TitleCaptions.Strings = (
        'Property'
        'Value')
      OnStringsChange = AIValueListStringsChange
      ColWidths = (
        150
        138)
    end
    object AIOptionsPanel: TPanel
      Left = 4
      Top = 498
      Width = 294
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnImportAI: TButton
        Left = 0
        Top = 0
        Width = 65
        Height = 24
        Caption = 'Import AI'
        TabOrder = 0
      end
      object btnExportAI: TButton
        Left = 64
        Top = 0
        Width = 65
        Height = 24
        Caption = 'Export AI'
        TabOrder = 1
      end
      object btnCopyAI: TButton
        Left = 128
        Top = 0
        Width = 65
        Height = 24
        Caption = 'Copy AI to'
        TabOrder = 2
        OnClick = btnCopyAIClick
      end
      object cbCopyAITo: TComboBox
        Left = 192
        Top = 0
        Width = 104
        Height = 21
        ItemHeight = 13
        TabOrder = 3
      end
    end
  end
end
