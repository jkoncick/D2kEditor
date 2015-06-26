object MissionDialog: TMissionDialog
  Left = 192
  Top = 111
  Width = 1024
  Height = 600
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
    Height = 566
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PlayerSettingsPanel: TPanel
      Left = 0
      Top = 0
      Width = 714
      Height = 369
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
      object Bevel1: TBevel
        Left = 0
        Top = 280
        Width = 713
        Height = 10
        Shape = bsBottomLine
      end
      object lblMapName: TLabel
        Left = 288
        Top = 296
        Width = 53
        Height = 13
        Caption = 'Map name:'
      end
      object lblMapAuthor: TLabel
        Left = 288
        Top = 320
        Width = 57
        Height = 13
        Caption = 'Map author:'
      end
      object lblMapMusic: TLabel
        Left = 288
        Top = 344
        Width = 31
        Height = 13
        Caption = 'Music:'
      end
      object lblMapSideId: TLabel
        Left = 552
        Top = 296
        Width = 37
        Height = 13
        Caption = 'Play as:'
      end
      object lblMapMissionNumber: TLabel
        Left = 552
        Top = 320
        Width = 76
        Height = 13
        Caption = 'Mission number:'
      end
      object lblMapBriefing: TLabel
        Left = 552
        Top = 344
        Width = 112
        Height = 13
        Caption = 'Mission briefing (below):'
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
      object cbUseINI: TCheckBox
        Left = 8
        Top = 296
        Width = 257
        Height = 17
        Caption = 'Use .ini file for additional mission settings'
        TabOrder = 6
        OnClick = cbUseINIClick
      end
      object btnRefreshStrings: TButton
        Left = 144
        Top = 342
        Width = 129
        Height = 22
        Caption = 'Refresh strings in events'
        TabOrder = 7
        OnClick = btnRefreshStringsClick
      end
      object btnResetToDefaults: TButton
        Left = 8
        Top = 342
        Width = 129
        Height = 22
        Caption = 'Reset values to defaults'
        TabOrder = 8
        OnClick = btnResetToDefaultsClick
      end
      object edMapName: TEdit
        Left = 352
        Top = 294
        Width = 188
        Height = 21
        TabOrder = 9
      end
      object edMapAuthor: TEdit
        Left = 352
        Top = 318
        Width = 188
        Height = 21
        TabOrder = 10
      end
      object edMapMusic: TEdit
        Left = 352
        Top = 342
        Width = 188
        Height = 21
        TabOrder = 11
      end
      object cbMapSideId: TComboBox
        Left = 600
        Top = 294
        Width = 105
        Height = 21
        ItemHeight = 13
        TabOrder = 12
      end
      object seMapMissionNumber: TSpinEdit
        Left = 640
        Top = 318
        Width = 65
        Height = 22
        MaxValue = 9
        MinValue = 0
        TabOrder = 13
        Value = 0
      end
    end
    object RulesAndStringsPanel: TPanel
      Left = 0
      Top = 369
      Width = 714
      Height = 197
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object RuleValueList: TValueListEditor
        Left = 0
        Top = 0
        Width = 272
        Height = 197
        Align = alLeft
        DefaultColWidth = 180
        Enabled = False
        TabOrder = 0
        TitleCaptions.Strings = (
          'Rule'
          'Value')
        ColWidths = (
          180
          86)
      end
      object StringsPanel: TPanel
        Left = 272
        Top = 0
        Width = 442
        Height = 197
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object StringsSplitter: TSplitter
          Left = 0
          Top = 94
          Width = 442
          Height = 3
          Cursor = crVSplit
          Align = alBottom
        end
        object StringValueList: TValueListEditor
          Left = 0
          Top = 97
          Width = 442
          Height = 100
          Align = alBottom
          Constraints.MinHeight = 60
          DefaultColWidth = 50
          Enabled = False
          KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
          TabOrder = 0
          TitleCaptions.Strings = (
            'String ID'
            'Text')
          ColWidths = (
            50
            386)
        end
        object MapBriefing: TMemo
          Left = 0
          Top = 0
          Width = 442
          Height = 94
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
    end
  end
  object AITabControl: TTabControl
    Left = 714
    Top = 0
    Width = 302
    Height = 566
    Align = alRight
    TabOrder = 1
    TabWidth = 37
    OnChange = AITabControlChange
    object AIValueList: TValueListEditor
      Left = 4
      Top = 6
      Width = 294
      Height = 532
      Align = alClient
      DefaultColWidth = 180
      ScrollBars = ssVertical
      TabOrder = 0
      TitleCaptions.Strings = (
        'Property'
        'Value')
      OnStringsChange = AIValueListStringsChange
      ColWidths = (
        180
        108)
    end
    object AIOptionsPanel: TPanel
      Left = 4
      Top = 538
      Width = 294
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnImportAI: TButton
        Left = 0
        Top = 0
        Width = 73
        Height = 24
        Caption = 'Import AI'
        TabOrder = 0
      end
      object btnExportAI: TButton
        Left = 72
        Top = 0
        Width = 73
        Height = 24
        Caption = 'Export AI'
        TabOrder = 1
      end
      object btnCopyAI: TButton
        Left = 150
        Top = 0
        Width = 73
        Height = 24
        Caption = 'Copy AI'
        TabOrder = 2
      end
      object btnPasteAI: TButton
        Left = 222
        Top = 0
        Width = 73
        Height = 24
        Caption = 'Paste AI'
        TabOrder = 3
      end
    end
  end
end
