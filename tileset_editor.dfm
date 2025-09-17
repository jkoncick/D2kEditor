object TilesetEditor: TTilesetEditor
  Left = 192
  Top = 12
  Width = 1112
  Height = 706
  Caption = 'Tileset Editor'
  Color = clBtnFace
  Constraints.MaxWidth = 1112
  Constraints.MinHeight = 706
  Constraints.MinWidth = 1112
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Tag = 1
    Left = 0
    Top = 0
    Width = 1104
    Height = 641
    ActivePage = PageImage
    Align = alClient
    TabOrder = 1
    OnChange = PageControlChange
    object PageImage: TTabSheet
      Caption = 'Image       '
      object lblNumberOfTiles: TLabel
        Left = 680
        Top = 282
        Width = 73
        Height = 13
        Caption = 'Number of tiles:'
      end
      object TilesetImage: TImage
        Left = 8
        Top = 2
        Width = 640
        Height = 608
        OnMouseDown = TilesetImageMouseDown
        OnMouseMove = TilesetImageMouseMove
        OnMouseUp = TilesetImageMouseUp
      end
      object sbCopySelectionTo: TSpeedButton
        Left = 680
        Top = 250
        Width = 137
        Height = 25
        Hint = 'Keyboard shortcut: hold Ctrl'
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'Copy selection to'
        ParentShowHint = False
        ShowHint = True
        OnClick = sbCopySelectionToClick
      end
      object sbSwapSelectionWith: TSpeedButton
        Left = 824
        Top = 250
        Width = 137
        Height = 25
        Hint = 'Keyboard shortcut: hold Shift'
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'Swap selection with'
        ParentShowHint = False
        ShowHint = True
        OnClick = sbSwapSelectionWithClick
      end
      object lblPageImageMouseActions: TLabel
        Left = 680
        Top = 312
        Width = 209
        Height = 57
        AutoSize = False
        Caption = 
          'Mouse actions:'#13'Left = Start selection / Apply operation'#13'Middle =' +
          ' Clear selection'
      end
      object lblTileimageModified: TLabel
        Left = 972
        Top = 192
        Width = 110
        Height = 13
        Caption = '(Tileset image modified)'
        Visible = False
      end
      object btnImportTilesetImage: TButton
        Left = 680
        Top = 186
        Width = 137
        Height = 25
        Caption = 'Import tileset image'
        TabOrder = 0
        OnClick = btnImportTilesetImageClick
      end
      object btnExportTilesetImage: TButton
        Left = 824
        Top = 186
        Width = 137
        Height = 25
        Caption = 'Export tileset image'
        TabOrder = 1
        OnClick = btnExportTilesetImageClick
      end
      object seNumberOfTiles: TSpinEdit
        Left = 760
        Top = 282
        Width = 57
        Height = 22
        Increment = 20
        MaxValue = 4000
        MinValue = 800
        TabOrder = 2
        Value = 800
      end
      object btnNumberOfTilesApply: TButton
        Left = 824
        Top = 282
        Width = 75
        Height = 22
        Caption = 'Apply'
        TabOrder = 3
        OnClick = btnNumberOfTilesApplyClick
      end
      object btnImportTilesetPortion: TButton
        Left = 680
        Top = 218
        Width = 137
        Height = 25
        Hint = 'Keyboard shortcut: I'
        Caption = 'Import tileset portion'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = btnImportTilesetPortionClick
      end
      object btnExportTilesetPortion: TButton
        Left = 824
        Top = 218
        Width = 137
        Height = 25
        Hint = 'Keyboard shortcut: E'
        Caption = 'Export tileset portion'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = btnExportTilesetPortionClick
      end
      object TilesetScrollBar: TScrollBar
        Left = 656
        Top = 2
        Width = 17
        Height = 608
        Kind = sbVertical
        Max = 39
        PageSize = 0
        TabOrder = 6
        OnChange = TilesetScrollBarChange
      end
      object btnEraseSelection: TButton
        Left = 968
        Top = 250
        Width = 121
        Height = 25
        Hint = 'Keyboard shortcut: Backspace'
        Caption = 'Erase selection'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = btnEraseSelectionClick
      end
      object gbBasicTilesetData: TGroupBox
        Left = 680
        Top = 0
        Width = 409
        Height = 121
        Caption = 'Basic tileset data'
        TabOrder = 8
        object lblTilesetFancyName: TLabel
          Left = 8
          Top = 16
          Width = 92
          Height = 13
          Caption = 'Tileset fancy name:'
        end
        object lblAuthorName: TLabel
          Left = 8
          Top = 40
          Width = 63
          Height = 13
          Caption = 'Author name:'
        end
        object lblDefaultPaintGroup: TLabel
          Left = 8
          Top = 88
          Width = 93
          Height = 13
          Caption = 'Default paint group:'
        end
        object edTilesetFancyName: TEdit
          Left = 104
          Top = 16
          Width = 193
          Height = 21
          MaxLength = 31
          TabOrder = 0
          OnChange = BasicTilesetDataChange
        end
        object edAuthorName: TEdit
          Left = 104
          Top = 40
          Width = 193
          Height = 21
          MaxLength = 31
          TabOrder = 1
          OnChange = BasicTilesetDataChange
        end
        object cbCustomMinimapColorsAllowed: TCheckBox
          Left = 8
          Top = 64
          Width = 193
          Height = 17
          Caption = 'Use custom minimap colors in game'
          TabOrder = 2
          OnClick = BasicTilesetDataChange
        end
        object cbxDefaultPaintGroup: TComboBox
          Left = 104
          Top = 88
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          OnChange = BasicTilesetDataChange
        end
      end
      object gbTilesetRules: TGroupBox
        Left = 680
        Top = 128
        Width = 409
        Height = 41
        Caption = 'Tileset Rules'
        TabOrder = 9
        object cbRuleDoNotDrawRockCraters: TCheckBox
          Left = 8
          Top = 16
          Width = 145
          Height = 17
          Caption = 'Do not draw rock craters'
          TabOrder = 0
          OnClick = BasicTilesetDataChange
        end
        object cbRuleDoNotDrawSandCraters: TCheckBox
          Left = 200
          Top = 16
          Width = 145
          Height = 17
          Caption = 'Do not draw sand craters'
          TabOrder = 1
          OnClick = BasicTilesetDataChange
        end
      end
    end
    object PageAttributes: TTabSheet
      Caption = 'Attributes  '
      ImageIndex = 1
      object lbTileAtrValue: TLabel
        Left = 680
        Top = 6
        Width = 87
        Height = 13
        Caption = 'Tile attribute value'
      end
      object lbTileAtrNotValue: TLabel
        Left = 988
        Top = 6
        Width = 20
        Height = 13
        Caption = 'Not:'
        Visible = False
      end
      object lbRule: TLabel
        Left = 892
        Top = 489
        Width = 25
        Height = 13
        Caption = 'Rule:'
      end
      object lblPageAttributesMouseActions: TLabel
        Left = 892
        Top = 512
        Width = 197
        Height = 65
        AutoSize = False
        Caption = 
          'Mouse actions:'#13'Left = Modify attributes'#13'Right = Select tile'#13'Midd' +
          'le = Unselect tile'#13'Hold Shift+Left to make a selection'
      end
      object lblExtraAttributeName: TLabel
        Left = 892
        Top = 176
        Width = 108
        Height = 13
        Caption = 'Custom attribute name:'
      end
      object rgFilterMode: TRadioGroup
        Left = 892
        Top = 328
        Width = 201
        Height = 153
        Caption = ' Filter mode '
        ItemIndex = 0
        Items.Strings = (
          'Mark all tiles'
          'Mark tiles of this type'
          'Filter tiles having all attributes'
          'Filter tiles having any of attributes'
          'Filter tiles not having attributes'
          'Filter tiles by rule...'
          'Mark nothing')
        TabOrder = 8
        OnClick = rgFilterModeClick
      end
      object edRule: TEdit
        Left = 920
        Top = 485
        Width = 172
        Height = 21
        ReadOnly = True
        TabOrder = 0
        OnChange = edRuleChange
      end
      object cbMarkSelection: TCheckBox
        Left = 892
        Top = 220
        Width = 109
        Height = 17
        Hint = 'Keyboard shortcut: M'
        Caption = 'Mark selected tile'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbOptionClick
      end
      object cbDrawOwnerSide: TCheckBox
        Left = 892
        Top = 260
        Width = 117
        Height = 17
        Caption = 'Draw owner side'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cbOptionClick
      end
      object cbDrawEditorAttributes: TCheckBox
        Left = 892
        Top = 240
        Width = 120
        Height = 17
        Hint = 'Keyboard shortcut: E'
        Caption = 'Draw editor attributes'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbOptionClick
      end
      object cbAnyOf: TCheckBox
        Left = 1024
        Top = 439
        Width = 65
        Height = 17
        Hint = 'Use "Filter tiles having any of attributes" mode'
        Caption = 'Any attr.'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = cbAnyOfClick
      end
      object btnTileAtrValueApply: TButton
        Left = 892
        Top = 2
        Width = 41
        Height = 21
        Caption = 'OK'
        TabOrder = 5
        OnClick = btnTileAtrValueApplyClick
      end
      object btnClearAttributes: TButton
        Left = 940
        Top = 2
        Width = 41
        Height = 21
        Hint = 'Keyboard shortcut: C'
        Caption = 'Clear'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = btnClearAttributesClick
      end
      object rgOperation: TRadioGroup
        Left = 892
        Top = 286
        Width = 201
        Height = 37
        Hint = 'Keyboard shortcut: S/A/R'
        Caption = ' Attribute Operation '
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Set'
          'Add'
          'Remove')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
      end
      object edTileAtrValue: TEdit
        Left = 776
        Top = 2
        Width = 81
        Height = 21
        MaxLength = 10
        TabOrder = 9
        Text = '0000000000'
      end
      object edTileAtrNotValue: TEdit
        Left = 1012
        Top = 2
        Width = 81
        Height = 21
        MaxLength = 10
        TabOrder = 10
        Text = '0000000000'
        Visible = False
      end
      object clbTileAtrListEditor: TCheckListBox
        Left = 909
        Top = 30
        Width = 184
        Height = 140
        OnClickCheck = clbTileAtrListClickCheck
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 17
        Items.Strings = (
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          '')
        ParentFont = False
        Style = lbOwnerDrawFixed
        TabOrder = 11
        OnClick = clbTileAtrListEditorClick
      end
      object clbTileAtrList: TCheckListBox
        Left = 680
        Top = 30
        Width = 204
        Height = 548
        OnClickCheck = clbTileAtrListClickCheck
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 17
        ParentFont = False
        Style = lbOwnerDrawFixed
        TabOrder = 12
      end
      object pnTileAtrColor: TPanel
        Left = 864
        Top = 2
        Width = 21
        Height = 21
        BevelOuter = bvNone
        Color = clBlack
        ParentBackground = False
        TabOrder = 13
      end
      object stSpeedModifier: TStaticText
        Left = 680
        Top = 596
        Width = 409
        Height = 13
        AutoSize = False
        TabOrder = 14
      end
      object stSideBitValues: TStaticText
        Left = 680
        Top = 580
        Width = 209
        Height = 17
        AutoSize = False
        TabOrder = 15
      end
      object edExtraAttributeName: TEdit
        Left = 892
        Top = 192
        Width = 193
        Height = 21
        Enabled = False
        MaxLength = 31
        TabOrder = 16
        OnChange = edExtraAttributeNameChange
      end
    end
    object PageHints: TTabSheet
      Caption = 'Hints         '
      ImageIndex = 2
      object lblPageHintsMouseActions: TLabel
        Left = 892
        Top = 32
        Width = 197
        Height = 65
        AutoSize = False
        Caption = 
          'Mouse actions:'#13'Left = Modify tile'#13'Right = Select tile'#13'Middle = U' +
          'nselect tile'#13'Hold Shift+Left to make a selection'
      end
      object lbTileHintText: TListBox
        Left = 680
        Top = 2
        Width = 204
        Height = 607
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbTileHintTextClick
      end
      object btnTileHintTextClear: TButton
        Left = 892
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 1
        OnClick = btnTileHintTextClearClick
      end
    end
    object PageRestrictions: TTabSheet
      Caption = 'Restrictions'
      ImageIndex = 7
      object lblPageRestrictionsMouseActions: TLabel
        Left = 680
        Top = 120
        Width = 197
        Height = 65
        AutoSize = False
        Caption = 
          'Mouse actions:'#13'Left = Modify restrictions'#13'Right = Clear restrict' +
          'ions'#13'Hold Ctrl to modify all four directions'
      end
      object clbRestrictions: TCheckListBox
        Left = 680
        Top = 2
        Width = 204
        Height = 73
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 17
        Items.Strings = (
          'Block sight'
          '(Unused 1)'
          '(Unused 2)'
          '(Unused 3)')
        ParentFont = False
        Style = lbOwnerDrawFixed
        TabOrder = 0
      end
      object rgRestrictionsOperation: TRadioGroup
        Left = 680
        Top = 78
        Width = 204
        Height = 37
        Hint = 'Keyboard shortcut: S/A/R'
        Caption = ' Operation '
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Set'
          'Add'
          'Remove')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
    end
    object PageColors: TTabSheet
      Caption = 'Colors       '
      ImageIndex = 3
      object lblMinimapColorRuleName: TLabel
        Left = 680
        Top = 496
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblMinimapColorRuleColor: TLabel
        Left = 680
        Top = 520
        Width = 74
        Height = 13
        Caption = 'Color hex code:'
      end
      object lblMinimapColorRuleRule: TLabel
        Left = 680
        Top = 544
        Width = 25
        Height = 13
        Caption = 'Rule:'
      end
      object sgMinimapColorRules: TStringGrid
        Tag = 1
        Left = 680
        Top = 2
        Width = 409
        Height = 453
        DefaultColWidth = 80
        DefaultRowHeight = 17
        RowCount = 25
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goThumbTracking]
        TabOrder = 0
        OnDrawCell = sgMinimapColorRulesDrawCell
        OnMouseWheelDown = sgMinimapColorRulesMouseWheelDown
        OnMouseWheelUp = sgMinimapColorRulesMouseWheelUp
        OnSelectCell = sgMinimapColorRulesSelectCell
        RowHeights = (
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17)
      end
      object btnMinimapColorRuleAdd: TButton
        Left = 680
        Top = 464
        Width = 75
        Height = 25
        Caption = 'Add new'
        TabOrder = 1
        OnClick = btnMinimapColorRuleAddClick
      end
      object btnMinimapColorRuleRemove: TButton
        Left = 760
        Top = 464
        Width = 75
        Height = 25
        Caption = 'Remove last'
        TabOrder = 2
        OnClick = btnMinimapColorRuleRemoveClick
      end
      object btnMinimapColorRuleMoveDown: TButton
        Left = 840
        Top = 464
        Width = 75
        Height = 25
        Caption = 'Move down'
        TabOrder = 3
        OnClick = btnMinimapColorRuleMoveDownClick
      end
      object btnMinimapColorRuleMoveUp: TButton
        Left = 920
        Top = 464
        Width = 75
        Height = 25
        Caption = 'Move up'
        TabOrder = 4
        OnClick = btnMinimapColorRuleMoveUpClick
      end
      object edMinimapColorRuleName: TEdit
        Left = 760
        Top = 496
        Width = 193
        Height = 21
        MaxLength = 31
        TabOrder = 5
      end
      object edMinimapColorRuleColor: TEdit
        Left = 760
        Top = 520
        Width = 73
        Height = 21
        MaxLength = 6
        TabOrder = 6
      end
      object edMinimapColorRuleRule: TEdit
        Left = 760
        Top = 544
        Width = 193
        Height = 21
        TabOrder = 7
      end
      object btnMinimapColorRuleApply: TButton
        Left = 680
        Top = 568
        Width = 75
        Height = 25
        Caption = 'Apply'
        TabOrder = 8
        OnClick = btnMinimapColorRuleApplyClick
      end
    end
    object PageFillArea: TTabSheet
      Caption = 'Fill Area     '
      ImageIndex = 4
      object lblFillAreaRuleName: TLabel
        Left = 680
        Top = 352
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblFillAreaRuleRule: TLabel
        Left = 680
        Top = 376
        Width = 25
        Height = 13
        Caption = 'Rule:'
      end
      object sgFillAreaRules: TStringGrid
        Left = 680
        Top = 2
        Width = 409
        Height = 309
        ColCount = 4
        DefaultColWidth = 100
        DefaultRowHeight = 17
        RowCount = 17
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
        TabOrder = 0
        OnDrawCell = sgFillAreaRulesDrawCell
        OnMouseWheelDown = sgFillAreaRulesMouseWheelDown
        OnMouseWheelUp = sgFillAreaRulesMouseWheelUp
        OnSelectCell = sgFillAreaRulesSelectCell
        RowHeights = (
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17
          17)
      end
      object btnFillAreaRuleAdd: TButton
        Left = 680
        Top = 320
        Width = 75
        Height = 25
        Caption = 'Add new'
        TabOrder = 1
        OnClick = btnFillAreaRuleAddClick
      end
      object btnFillAreaRuleRemove: TButton
        Left = 760
        Top = 320
        Width = 75
        Height = 25
        Caption = 'Remove last'
        TabOrder = 2
        OnClick = btnFillAreaRuleRemoveClick
      end
      object btnFillAreaRuleMoveDown: TButton
        Left = 840
        Top = 320
        Width = 75
        Height = 25
        Caption = 'Move down'
        TabOrder = 3
        OnClick = btnFillAreaRuleMoveDownClick
      end
      object btnFillAreaRuleMoveUp: TButton
        Left = 920
        Top = 320
        Width = 75
        Height = 25
        Caption = 'Move up'
        TabOrder = 4
        OnClick = btnFillAreaRuleMoveUpClick
      end
      object edFillAreaRuleName: TEdit
        Left = 760
        Top = 352
        Width = 193
        Height = 21
        MaxLength = 31
        TabOrder = 5
      end
      object edFillAreaRuleRule: TEdit
        Left = 760
        Top = 376
        Width = 193
        Height = 21
        TabOrder = 6
      end
      object btnFillAreaRuleApply: TButton
        Left = 680
        Top = 400
        Width = 75
        Height = 25
        Caption = 'Apply'
        TabOrder = 7
        OnClick = btnFillAreaRuleApplyClick
      end
    end
    object PagePaint: TTabSheet
      Caption = 'Paint         '
      ImageIndex = 6
      object lblPaintTileGroupButtonImage: TLabel
        Left = 680
        Top = 248
        Width = 65
        Height = 13
        Caption = 'Button image:'
      end
      object imgPaintTileGroupButtonImage: TImage
        Left = 752
        Top = 248
        Width = 32
        Height = 32
        ParentShowHint = False
        ShowHint = True
      end
      object lblPaintTileGroupName: TLabel
        Left = 680
        Top = 288
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblPaintTileGroupRestrictionRule: TLabel
        Left = 680
        Top = 312
        Width = 73
        Height = 13
        Caption = 'Restriction rule:'
      end
      object lblPaintTileGroupSmoothPresetGroup: TLabel
        Left = 680
        Top = 336
        Width = 124
        Height = 13
        Caption = 'Auto-smooth preset group:'
      end
      object lblPaintTileGroupSmoothPresets: TLabel
        Left = 680
        Top = 360
        Width = 99
        Height = 13
        Caption = 'Auto-smooth presets:'
      end
      object lblPaintTileGroupRandomMapName: TLabel
        Left = 680
        Top = 384
        Width = 82
        Height = 13
        Caption = 'Random tile map:'
      end
      object Label1: TLabel
        Left = 680
        Top = 440
        Width = 197
        Height = 57
        AutoSize = False
        Caption = 
          'Mouse actions:'#13'Left = Add tile'#13'Right = Remove tile'#13'Middle = Sele' +
          'ct button image tile'
      end
      object sgPaintTileGroups: TStringGrid
        Tag = 1
        Left = 680
        Top = 2
        Width = 409
        Height = 237
        ColCount = 6
        DefaultColWidth = 66
        DefaultRowHeight = 17
        RowCount = 13
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
        TabOrder = 0
        OnDrawCell = sgPaintTileGroupsDrawCell
        OnMouseWheelDown = sgPaintTileGroupsMouseWheelDown
        OnMouseWheelUp = sgPaintTileGroupsMouseWheelUp
        OnSelectCell = sgPaintTileGroupsSelectCell
      end
      object edPaintTileGroupName: TEdit
        Left = 760
        Top = 288
        Width = 193
        Height = 21
        MaxLength = 31
        TabOrder = 1
      end
      object edPaintTileGroupRestrictionRule: TEdit
        Left = 760
        Top = 312
        Width = 193
        Height = 21
        TabOrder = 2
      end
      object cbxPaintTileGroupSmoothPresetGroup: TComboBox
        Left = 808
        Top = 336
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
      end
      object edPaintTileGroupSmoothPresets: TEdit
        Left = 784
        Top = 360
        Width = 169
        Height = 21
        MaxLength = 20
        TabOrder = 4
      end
      object edPaintTileGroupRandomMapName: TEdit
        Left = 784
        Top = 384
        Width = 169
        Height = 21
        MaxLength = 31
        TabOrder = 5
      end
      object btnPaintTileGroupApply: TButton
        Left = 680
        Top = 408
        Width = 75
        Height = 25
        Caption = 'Apply'
        TabOrder = 6
        OnClick = btnPaintTileGroupApplyClick
      end
    end
    object PagePresets: TTabSheet
      Caption = 'Presets     '
      ImageIndex = 5
      object lblBlockPresetGroupName: TLabel
        Left = 680
        Top = 176
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblBlockPresetGroupPaintGroup: TLabel
        Left = 680
        Top = 200
        Width = 57
        Height = 13
        Caption = 'Paint group:'
      end
      object imgBlockPresetKeys: TImage
        Left = 680
        Top = 232
        Width = 321
        Height = 129
        OnMouseDown = imgBlockPresetKeysMouseDown
      end
      object sgBlockPresetGroups: TStringGrid
        Tag = 1
        Left = 680
        Top = 2
        Width = 409
        Height = 165
        ColCount = 4
        DefaultColWidth = 66
        DefaultRowHeight = 17
        RowCount = 9
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
        TabOrder = 0
        OnMouseWheelDown = sgBlockPresetGroupsMouseWheelDown
        OnMouseWheelUp = sgBlockPresetGroupsMouseWheelUp
        OnSelectCell = sgBlockPresetGroupsSelectCell
        RowHeights = (
          17
          17
          17
          17
          17
          17
          17
          17
          17)
      end
      object edBlockPresetGroupName: TEdit
        Left = 744
        Top = 176
        Width = 193
        Height = 21
        MaxLength = 31
        TabOrder = 1
      end
      object cbxBlockPresetGroupPaintGroup: TComboBox
        Left = 744
        Top = 200
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
      end
      object btnBlockPresetGroupApply: TButton
        Left = 944
        Top = 196
        Width = 75
        Height = 25
        Caption = 'Apply'
        TabOrder = 3
        OnClick = btnBlockPresetGroupApplyClick
      end
      object btnBlockPresetAddPreset: TButton
        Left = 1008
        Top = 336
        Width = 81
        Height = 25
        Caption = 'Add preset...'
        TabOrder = 4
        OnClick = btnBlockPresetAddPresetClick
      end
      object pnBlockPreset: TPanel
        Left = 680
        Top = 2
        Width = 409
        Height = 223
        TabOrder = 5
        Visible = False
        object imgBlockPreset: TImage
          Left = 16
          Top = 16
          Width = 128
          Height = 128
          Hint = 'Left button = select tile'#13'Right button = erase tile'
          ParentShowHint = False
          ShowHint = True
          OnMouseDown = imgBlockPresetMouseDown
        end
        object lblBlockPresetWidth: TLabel
          Left = 160
          Top = 16
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object lblBlockPresetHeight: TLabel
          Left = 160
          Top = 40
          Width = 34
          Height = 13
          Caption = 'Height:'
        end
        object lblBlockPresetHint: TLabel
          Left = 16
          Top = 160
          Width = 309
          Height = 26
          Caption = 
            'Click into tileset image to select a single tile.'#13'Hold shift and' +
            ' make a selection (up to 4*4) to select whole preset.'
        end
        object seBlockPresetWidth: TSpinEdit
          Left = 200
          Top = 16
          Width = 41
          Height = 22
          MaxValue = 4
          MinValue = 1
          TabOrder = 0
          Value = 1
          OnChange = seBlockPresetSizeChange
        end
        object seBlockPresetHeight: TSpinEdit
          Left = 200
          Top = 40
          Width = 41
          Height = 22
          MaxValue = 4
          MinValue = 1
          TabOrder = 1
          Value = 1
          OnChange = seBlockPresetSizeChange
        end
        object btnBlockPresetAdd: TButton
          Left = 160
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 2
          OnClick = btnBlockPresetAddClick
        end
        object btnBlockPresetClose: TButton
          Left = 240
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Close'
          TabOrder = 3
          OnClick = btnBlockPresetCloseClick
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 641
    Width = 1104
    Height = 19
    Panels = <
      item
        Text = 'X: 0  Y: 0'
        Width = 100
      end
      item
        Text = 'No tileset loaded'
        Width = 128
      end
      item
        Text = 'No Image loaded'
        Width = 292
      end
      item
        Text = 'No Attributes loaded'
        Width = 292
      end
      item
        Text = 'No INI Config loaded'
        Width = 292
      end>
  end
  object cbShowGrid: TCheckBox
    Left = 942
    Top = 0
    Width = 71
    Height = 17
    Hint = 'Keyboard shortcut: G'
    Caption = 'Show Grid'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = cbOptionClick
  end
  object cbAlwaysOnTop: TCheckBox
    Left = 1022
    Top = 0
    Width = 75
    Height = 17
    Caption = 'Stay on top'
    TabOrder = 3
    OnClick = cbAlwaysOnTopClick
  end
  object cbHideUnmarkedTiles: TCheckBox
    Left = 820
    Top = 0
    Width = 120
    Height = 17
    Hint = 'Keyboard shortcut: H'
    Caption = 'Hide unmarked tiles'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    Visible = False
    OnClick = cbOptionClick
  end
  object cbMarkSelectedItem: TCheckBox
    Left = 684
    Top = 0
    Width = 133
    Height = 17
    Hint = 'Keyboard shortcut: T'
    Caption = 'Mark only selected item'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    Visible = False
    OnClick = cbOptionClick
  end
  object MainMenu: TMainMenu
    Left = 520
    object Newtileset1: TMenuItem
      Caption = 'New tileset'
      OnClick = Newtileset1Click
    end
    object Opentileset1: TMenuItem
      Caption = 'Open tileset'
      OnClick = Opentileset1Click
    end
    object Applychanges1: TMenuItem
      Caption = 'Apply changes (Ctrl+A)'
      ShortCut = 16449
      OnClick = Applychanges1Click
    end
    object Savechanges1: TMenuItem
      Caption = 'Save changes (Ctrl+S)'
      ShortCut = 16467
      OnClick = Savechanges1Click
    end
    object Saveandtest1: TMenuItem
      Caption = 'Save and test'
      ShortCut = 119
      OnClick = Saveandtest1Click
    end
    object Reloadfiles1: TMenuItem
      Caption = 'Reload files (Ctrl+R)'
      ShortCut = 16466
      OnClick = Reloadfiles1Click
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Caption = 'Undo'
        Enabled = False
        ShortCut = 16474
        OnClick = Undo1Click
      end
      object Redo1: TMenuItem
        Caption = 'Redo'
        Enabled = False
        ShortCut = 16473
        OnClick = Redo1Click
      end
    end
  end
  object TilesetImageOpenDialog: TOpenDialog
    DefaultExt = 'png'
    Filter = 
      'Supported formats (*.bmp,png,R16,R8)|*.R16;*.R8;*.bmp;*.png|Dune' +
      '2000 R16 tileset image (*.R16)|*.R16|Dune2000 R8 tileset image (' +
      '*.R8)|*.R8|BMP image (*.bmp)|*.bmp|PNG image (*.png)|*.png'
    InitialDir = '.\tilesets'
    Title = 'Import tileset image'
    Left = 552
  end
  object TilesetImageSaveDialog: TSaveDialog
    DefaultExt = 'png'
    Filter = 
      'BMP Image (*.bmp)|*.bmp|PNG Image (*.png)|*.png|R16 Image (*.R16' +
      ')|*.R16'
    FilterIndex = 2
    Title = 'Export tileset image'
    Left = 584
  end
  object TilesetPortionOpenDialog: TOpenDialog
    DefaultExt = 'png'
    Filter = 
      'Supported formats (*.bmp,png)|*.bmp;*.png|BMP image (*.bmp)|*.bm' +
      'p|PNG image (*.png)|*.png'
    InitialDir = '.\tilesets'
    Title = 'Import tileset portion'
    Left = 616
  end
  object TilesetPortionSaveDialog: TSaveDialog
    DefaultExt = 'png'
    Filter = 'BMP Image (*.bmp)|*.bmp|PNG Image (*.png)|*.png'
    FilterIndex = 2
    Title = 'Export tileset portion'
    Left = 648
  end
end
