object TileAtrEditor: TTileAtrEditor
  Left = 192
  Top = 65
  Width = 1112
  Height = 664
  Caption = 'Tile Attributes Editor'
  Color = clBtnFace
  Constraints.MaxWidth = 1112
  Constraints.MinHeight = 664
  Constraints.MinWidth = 1112
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object TilesetImage: TImage
    Left = 8
    Top = 8
    Width = 640
    Height = 576
    OnMouseDown = TilesetImageMouseDown
    OnMouseMove = TilesetImageMouseMove
    OnMouseUp = TilesetImageMouseUp
  end
  object lbTileAtrValue: TLabel
    Left = 680
    Top = 12
    Width = 87
    Height = 13
    Caption = 'Tile attribute value'
  end
  object lbTileAtrNotValue: TLabel
    Left = 988
    Top = 12
    Width = 20
    Height = 13
    Caption = 'Not:'
    Visible = False
  end
  object lbRule: TLabel
    Left = 892
    Top = 448
    Width = 25
    Height = 13
    Caption = 'Rule:'
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 591
    Width = 1104
    Height = 19
    Panels = <
      item
        Text = 'X: 0  Y: 0'
        Width = 100
      end
      item
        Text = 'No tileset loaded'
        Width = 100
      end
      item
        Text = 'No Image loaded'
        Width = 304
      end
      item
        Text = 'No Attributes loaded'
        Width = 304
      end
      item
        Text = 'No INI Config loaded'
        Width = 304
      end>
  end
  object TilesetScrollBar: TScrollBar
    Left = 656
    Top = 8
    Width = 17
    Height = 576
    Kind = sbVertical
    Max = 39
    PageSize = 0
    TabOrder = 1
    OnChange = TilesetScrollBarChange
  end
  object TileAtrValue: TEdit
    Left = 776
    Top = 8
    Width = 81
    Height = 21
    MaxLength = 10
    TabOrder = 2
    Text = '0000000000'
  end
  object lbTileHintText: TListBox
    Left = 680
    Top = 38
    Width = 204
    Height = 548
    ItemHeight = 13
    TabOrder = 18
    Visible = False
  end
  object TileAtrList: TCheckListBox
    Left = 680
    Top = 38
    Width = 204
    Height = 548
    OnClickCheck = TileAtrListClickCheck
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 17
    Items.Strings = (
      'Building/Unit owner side (bit1)'
      'Building/Unit owner side (bit2)'
      'Building/Unit owner side (bit3)'
      'Occupied by Unit'
      'Occupied by Building'
      'Occupied by Infantry (middle)'
      'Occupied by Infantry (top-right)'
      'Occupied by Infantry (down-right)'
      'Occupied by Infantry (down-left)'
      'Occupied by Infantry (top-left)'
      'Wall'
      'Concrete'
      'Non-buildable'
      'Vehicles can pass'
      'Infantry can pass'
      'Buildings can be placed, Rock craters'
      'Sandworm can pass, Sand craters'
      'Concrete owner side (bit 1)'
      'Concrete owner side (bit 2)'
      'Concrete owner side (bit 3)'
      'Spice amount (bit 1)'
      'Spice amount (bit 2)'
      'Spice amount (bit 3)'
      'Unknown/Unused'
      'Unknown/Unused'
      'Unknown (side bit 1)'
      'Unknown (side bit 2)'
      'Unknown (side bit 3)'
      'Unknown/Unused'
      'Rock (wheeled +10% speed)'
      'Dunes (wheeled -50%, other -20% sp.)'
      'Rough Rock (all -50% speed)')
    ParentFont = False
    Style = lbOwnerDrawFixed
    TabOrder = 3
  end
  object btnTileAtrValueApply: TButton
    Left = 892
    Top = 8
    Width = 41
    Height = 21
    Caption = 'OK'
    TabOrder = 4
    OnClick = btnTileAtrValueApplyClick
  end
  object rgFilterMode: TRadioGroup
    Left = 892
    Top = 288
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
    TabOrder = 5
    OnClick = rgFilterModeClick
  end
  object rgOperation: TRadioGroup
    Left = 892
    Top = 248
    Width = 201
    Height = 37
    Caption = ' Attribute Operation '
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Set'
      'Add'
      'Remove')
    TabOrder = 6
  end
  object cbMultipleSelectMode: TCheckBox
    Left = 892
    Top = 182
    Width = 121
    Height = 17
    Caption = 'Multi-select mode'
    TabOrder = 7
  end
  object rgViewMode: TRadioGroup
    Left = 892
    Top = 470
    Width = 201
    Height = 116
    Caption = ' View mode  '
    ItemIndex = 0
    Items.Strings = (
      'Draw tile attributes'
      'Draw minimap colors'
      'Draw fill area groups'
      'Check block preset coverage'
      'Edit tile hint text')
    TabOrder = 8
    OnClick = rgViewModeClick
  end
  object btnConvertEditorAttributes: TButton
    Left = 1040
    Top = 224
    Width = 53
    Height = 21
    Hint = 'Convert editor attributes from older TILEATR*.BIN files'
    Caption = 'Convert'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    OnClick = btnConvertEditorAttributesClick
  end
  object cbShowGrid: TCheckBox
    Left = 1012
    Top = 182
    Width = 71
    Height = 17
    Caption = 'Show Grid'
    TabOrder = 10
    OnClick = cbOptionClick
  end
  object cbMarkSelection: TCheckBox
    Left = 892
    Top = 204
    Width = 109
    Height = 17
    Caption = 'Mark selected tile'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = cbOptionClick
  end
  object btnClearAttributes: TButton
    Left = 940
    Top = 8
    Width = 41
    Height = 21
    Hint = 'Clear selected attributes'
    Caption = 'Clear'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    OnClick = btnClearAttributesClick
  end
  object TileAtrListEditor: TCheckListBox
    Left = 892
    Top = 38
    Width = 201
    Height = 141
    OnClickCheck = TileAtrListClickCheck
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 17
    Items.Strings = (
      'Editor Area type 1'
      'Editor Area type 2'
      'Editor Area type 3'
      'Editor Area type 4'
      'Editor Area type 5'
      'Editor Area type 6'
      'Editor Area type 7'
      'Editor Area type 8')
    ParentFont = False
    Style = lbOwnerDrawFixed
    TabOrder = 13
  end
  object TileAtrColor: TPanel
    Left = 864
    Top = 8
    Width = 21
    Height = 21
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 14
    object TileAtrColorEditor: TPanel
      Left = 8
      Top = 8
      Width = 5
      Height = 5
      BevelOuter = bvNone
      Color = clBlack
      ParentBackground = False
      TabOrder = 0
    end
  end
  object TileAtrNotValue: TEdit
    Left = 1012
    Top = 8
    Width = 81
    Height = 21
    MaxLength = 10
    TabOrder = 15
    Text = '0000000000'
    Visible = False
  end
  object cbDrawEditorAttributes: TCheckBox
    Left = 892
    Top = 226
    Width = 125
    Height = 17
    Caption = 'Draw editor attributes'
    Checked = True
    State = cbChecked
    TabOrder = 16
    OnClick = cbOptionClick
  end
  object cbAlwaysOnTop: TCheckBox
    Left = 1012
    Top = 204
    Width = 75
    Height = 17
    Caption = 'Alw. on Top'
    TabOrder = 17
    OnClick = cbAlwaysOnTopClick
  end
  object edRule: TEdit
    Left = 920
    Top = 444
    Width = 172
    Height = 21
    ReadOnly = True
    TabOrder = 19
    OnChange = edRuleChange
  end
  object cbAnyOf: TCheckBox
    Left = 1024
    Top = 399
    Width = 65
    Height = 17
    Hint = 'Use "Filter tiles having any of attributes" mode'
    Caption = 'Any attr.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 20
    OnClick = cbAnyOfClick
  end
  object MainMenu: TMainMenu
    object File1: TMenuItem
      Caption = 'File'
      object OpenTileAtr1: TMenuItem
        Caption = 'Load Attributes'
        OnClick = OpenTileAtr1Click
      end
      object OpenTileset1: TMenuItem
        Caption = 'Load Image'
        OnClick = OpenTileset1Click
      end
      object OpenBoth1: TMenuItem
        Caption = 'Load Both'
        ShortCut = 16463
        OnClick = OpenBoth1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object ReloadTileatr1: TMenuItem
        Caption = 'Reload Attributes'
        ShortCut = 16466
        OnClick = ReloadTileatr1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SaveTileAtr1: TMenuItem
        Caption = 'Save Attributes'
        ShortCut = 16467
        OnClick = SaveTileAtr1Click
      end
      object Saveandtest1: TMenuItem
        Caption = 'Save and test'
        ShortCut = 119
        OnClick = Saveandtest1Click
      end
      object SaveTileAtras1: TMenuItem
        Caption = 'Save Attributes as'
        OnClick = SaveTileAtras1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Quickopen1: TMenuItem
      Caption = 'Open tileset'
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
    object Help1: TMenuItem
      Caption = 'Help'
      object KeyShortcuts1: TMenuItem
        Caption = 'Key Shortcuts'
        OnClick = KeyShortcuts1Click
      end
      object MouseActions1: TMenuItem
        Caption = 'Mouse Actions'
        OnClick = MouseActions1Click
      end
    end
  end
  object SaveTileAtrDialog: TSaveDialog
    DefaultExt = 'bin'
    Filter = 'Dune 2000 tileatr file (*.bin)|*.bin'
    Left = 96
  end
end
