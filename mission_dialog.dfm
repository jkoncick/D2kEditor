object MissionDialog: TMissionDialog
  Left = 192
  Top = 80
  Width = 1024
  Height = 658
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
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object SettingsPanel: TPanel
    Left = 0
    Top = 0
    Width = 696
    Height = 624
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PlayerSettingsPanel: TPanel
      Left = 0
      Top = 0
      Width = 696
      Height = 417
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblTechLevel: TLabel
        Left = 80
        Top = 8
        Width = 38
        Height = 13
        Caption = 'Tech lvl'
      end
      object lblStartingMoney: TLabel
        Left = 132
        Top = 8
        Width = 32
        Height = 13
        Caption = 'Credits'
      end
      object lblAllocIndex: TLabel
        Left = 192
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
        Width = 697
        Height = 10
        Shape = bsBottomLine
      end
      object lblMapName: TLabel
        Left = 8
        Top = 320
        Width = 53
        Height = 13
        Caption = 'Map name:'
      end
      object lblMapAuthor: TLabel
        Left = 8
        Top = 344
        Width = 34
        Height = 13
        Caption = 'Author:'
      end
      object lblMapMusic: TLabel
        Left = 8
        Top = 368
        Width = 31
        Height = 13
        Caption = 'Music:'
      end
      object lblMapSideId: TLabel
        Left = 272
        Top = 320
        Width = 37
        Height = 13
        Caption = 'Play as:'
      end
      object lblMapMissionNumber: TLabel
        Left = 272
        Top = 344
        Width = 76
        Height = 13
        Caption = 'Mission number:'
      end
      object lblMapBriefing: TLabel
        Left = 272
        Top = 392
        Width = 112
        Height = 13
        Caption = 'Mission briefing (below):'
      end
      object lblTimeLimitHelp: TLabel
        Left = 176
        Top = 260
        Width = 197
        Height = 13
        Caption = '(-1 = no limit, 25 = second, 1500 = minute)'
      end
      object lblTilesetName: TLabel
        Left = 384
        Top = 260
        Width = 63
        Height = 13
        Caption = 'Tileset name:'
      end
      object lblTileatrName: TLabel
        Left = 544
        Top = 260
        Width = 47
        Height = 13
        Caption = 'Attributes:'
      end
      object lblTextUib: TLabel
        Left = 440
        Top = 392
        Width = 57
        Height = 13
        Caption = 'Text.uib file:'
      end
      object lblModsFolder: TLabel
        Left = 440
        Top = 320
        Width = 58
        Height = 13
        Caption = 'Mods folder:'
      end
      object lblColoursBin: TLabel
        Left = 440
        Top = 344
        Width = 71
        Height = 13
        Caption = 'Colours.bin file:'
      end
      object lblCampaignFolder: TLabel
        Left = 440
        Top = 296
        Width = 79
        Height = 13
        Caption = 'Campaign folder:'
      end
      object lblMapIntelId: TLabel
        Left = 272
        Top = 368
        Width = 37
        Height = 13
        Caption = 'Intel ID:'
      end
      object lblPlayersIni: TLabel
        Left = 440
        Top = 368
        Width = 66
        Height = 13
        Caption = 'Players.ini file:'
      end
      object seTechLevelAll: TSpinEdit
        Left = 80
        Top = 224
        Width = 49
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = seTechLevelAllChange
      end
      object edStartingMoneyAll: TEdit
        Left = 132
        Top = 224
        Width = 57
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = edStartingMoneyAllChange
      end
      object btnAllocIndexReset: TButton
        Left = 192
        Top = 224
        Width = 65
        Height = 22
        Caption = 'Reset'
        TabOrder = 3
        OnClick = btnAllocIndexResetClick
      end
      object btnAllegianceReset: TButton
        Left = 264
        Top = 224
        Width = 113
        Height = 22
        Caption = 'Reset allegiance'
        TabOrder = 4
        OnClick = btnAllegianceResetClick
      end
      object edTimeLimit: TEdit
        Left = 80
        Top = 256
        Width = 89
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = time_limit_change
      end
      object cbSetBothSides: TCheckBox
        Left = 384
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
        Left = 136
        Top = 390
        Width = 129
        Height = 22
        Caption = 'Refresh strings in events'
        Enabled = False
        TabOrder = 7
        OnClick = btnRefreshStringsClick
      end
      object btnResetToDefaults: TButton
        Left = 8
        Top = 390
        Width = 125
        Height = 22
        Caption = 'Reset values to defaults'
        Enabled = False
        TabOrder = 8
        OnClick = btnResetToDefaultsClick
      end
      object edMapName: TEdit
        Left = 64
        Top = 318
        Width = 201
        Height = 21
        Enabled = False
        TabOrder = 9
        OnChange = MissionIniPropertyChange
      end
      object edMapAuthor: TEdit
        Left = 64
        Top = 342
        Width = 201
        Height = 21
        Enabled = False
        TabOrder = 10
        OnChange = MissionIniPropertyChange
      end
      object cbMapSideId: TComboBox
        Left = 320
        Top = 318
        Width = 113
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 12
        OnChange = cbMapSideIdChange
      end
      object seMapMissionNumber: TSpinEdit
        Left = 360
        Top = 342
        Width = 73
        Height = 22
        Enabled = False
        MaxValue = 9
        MinValue = 0
        TabOrder = 13
        Value = 0
        OnChange = seMapMissionNumberChange
      end
      object cbMapMusic: TComboBox
        Left = 64
        Top = 366
        Width = 201
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 11
        OnChange = MissionIniPropertyChange
      end
      object edTilesetName: TEdit
        Left = 456
        Top = 256
        Width = 80
        Height = 21
        MaxLength = 199
        TabOrder = 14
        OnChange = edTilesetNameChange
      end
      object edTileatrName: TEdit
        Left = 600
        Top = 256
        Width = 80
        Height = 21
        MaxLength = 199
        TabOrder = 15
        OnChange = edTileatrNameChange
      end
      object cbTextUib: TComboBox
        Left = 520
        Top = 390
        Width = 161
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 16
        OnChange = cbTextUibChange
      end
      object cbModsFolder: TComboBox
        Left = 520
        Top = 318
        Width = 161
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 17
        OnChange = cbModsFolderChange
      end
      object cbColoursBin: TComboBox
        Left = 520
        Top = 342
        Width = 161
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 18
        OnChange = cbColoursBinChange
      end
      object cbCampaignFolder: TComboBox
        Left = 520
        Top = 294
        Width = 161
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 19
        OnChange = cbCampaignFolderChange
      end
      object edMapIntelId: TEdit
        Left = 320
        Top = 366
        Width = 113
        Height = 21
        Enabled = False
        TabOrder = 20
        OnChange = MissionIniPropertyChange
      end
      object cbPlayersIni: TComboBox
        Left = 520
        Top = 366
        Width = 161
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 21
        OnChange = cbPlayersIniChange
      end
    end
    object RulesAndStringsPanel: TPanel
      Left = 0
      Top = 417
      Width = 696
      Height = 207
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object RuleValueList: TValueListEditor
        Left = 0
        Top = 0
        Width = 272
        Height = 207
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
        Width = 424
        Height = 207
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object StringsSplitter: TSplitter
          Left = 0
          Top = 104
          Width = 424
          Height = 3
          Cursor = crVSplit
          Align = alBottom
        end
        object StringValueList: TValueListEditor
          Left = 0
          Top = 107
          Width = 424
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
            368)
        end
        object MapBriefing: TMemo
          Left = 0
          Top = 0
          Width = 424
          Height = 104
          Align = alClient
          Enabled = False
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
    end
  end
  object AITabControl: TTabControl
    Left = 696
    Top = 0
    Width = 320
    Height = 624
    Align = alRight
    TabOrder = 1
    TabWidth = 39
    OnChange = AITabControlChange
    object AIValueList: TValueListEditor
      Left = 4
      Top = 6
      Width = 312
      Height = 590
      Align = alClient
      DefaultColWidth = 224
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
      ScrollBars = ssVertical
      TabOrder = 0
      TitleCaptions.Strings = (
        'Property'
        'Value')
      OnSelectCell = AIValueListSelectCell
      OnStringsChange = AIValueListStringsChange
      ColWidths = (
        224
        82)
    end
    object AIOptionsPanel: TPanel
      Left = 4
      Top = 596
      Width = 312
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnImportAI: TButton
        Left = 64
        Top = 0
        Width = 65
        Height = 24
        Caption = 'Import AI'
        TabOrder = 0
        OnClick = btnImportAIClick
      end
      object btnExportAI: TButton
        Left = 0
        Top = 0
        Width = 65
        Height = 24
        Caption = 'Export AI'
        TabOrder = 1
        OnClick = btnExportAIClick
      end
      object btnCopyAI: TButton
        Left = 136
        Top = 0
        Width = 65
        Height = 24
        Caption = 'Copy AI'
        TabOrder = 2
        OnClick = btnCopyAIClick
      end
      object btnPasteAI: TButton
        Left = 200
        Top = 0
        Width = 65
        Height = 24
        Caption = 'Paste AI'
        TabOrder = 3
        OnClick = btnPasteAIClick
      end
      object cbDiffMode: TCheckBox
        Left = 272
        Top = 4
        Width = 41
        Height = 17
        Caption = 'Diff'
        TabOrder = 4
        OnClick = cbDiffModeClick
      end
      object pnSelectDefenceAreaFromMap: TPanel
        Left = 0
        Top = 0
        Width = 312
        Height = 24
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 5
        Visible = False
        object btnSelectDefenceAreaFromMap: TButton
          Left = 64
          Top = 0
          Width = 185
          Height = 25
          Caption = 'Select defence area # from map'
          TabOrder = 0
          OnClick = btnSelectDefenceAreaFromMapClick
        end
      end
    end
  end
  object ExportAIDialog: TSaveDialog
    DefaultExt = 'misai'
    Filter = 'Mission file AI segment (*.misai)|*.misai'
    Title = 'Export AI segment'
  end
  object ImportAIDialog: TOpenDialog
    DefaultExt = 'misai'
    Filter = 'Mission file AI segment (*.misai)|*.misai'
    Title = 'Import AI segment'
    Left = 32
  end
end
