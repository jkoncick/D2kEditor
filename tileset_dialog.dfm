object TilesetDialog: TTilesetDialog
  Left = 326
  Top = 66
  Width = 682
  Height = 642
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Tile block selection'
  Color = clBtnFace
  Constraints.MaxHeight = 1348
  Constraints.MaxWidth = 682
  Constraints.MinHeight = 578
  Constraints.MinWidth = 682
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TilesetImage: TImage
    Left = 8
    Top = 24
    Width = 640
    Height = 576
    OnMouseDown = TilesetImageMouseDown
    OnMouseMove = TilesetImageMouseMove
    OnMouseUp = TilesetImageMouseUp
  end
  object sbCustomSize: TSpeedButton
    Left = 168
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Down = True
    Caption = 'Custom'
  end
  object sbPreset11: TSpeedButton
    Tag = 1
    Left = 216
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '1 x 1'
    OnClick = SetBlockSize
  end
  object sbPreset22: TSpeedButton
    Tag = 2
    Left = 256
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '2 x 2'
    OnClick = SetBlockSize
  end
  object sbPreset33: TSpeedButton
    Tag = 3
    Left = 296
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '3 x 3'
    OnClick = SetBlockSize
  end
  object sbPreset44: TSpeedButton
    Tag = 4
    Left = 336
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '4 x 4'
    OnClick = SetBlockSize
  end
  object sbPreset21: TSpeedButton
    Tag = 5
    Left = 376
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '2 x 1'
    OnClick = SetBlockSize
  end
  object sbPreset12: TSpeedButton
    Tag = 6
    Left = 416
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '1 x 2'
    OnClick = SetBlockSize
  end
  object sbPreset32: TSpeedButton
    Tag = 7
    Left = 456
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '3 x 2'
    OnClick = SetBlockSize
  end
  object sbPreset23: TSpeedButton
    Tag = 8
    Left = 496
    Top = 3
    Width = 41
    Height = 19
    GroupIndex = 1
    Caption = '2 x 3'
    OnClick = SetBlockSize
  end
  object lbPresetSize: TLabel
    Left = 680
    Top = 52
    Width = 23
    Height = 13
    Caption = 'Size:'
    Visible = False
  end
  object lbPresetSizeX: TLabel
    Left = 760
    Top = 52
    Width = 7
    Height = 13
    Caption = 'X'
    Visible = False
  end
  object Bevel1: TBevel
    Left = 678
    Top = 78
    Width = 196
    Height = 196
    Shape = bsFrame
  end
  object PresetImage: TImage
    Left = 680
    Top = 80
    Width = 192
    Height = 192
    OnMouseDown = PresetImageMouseDown
  end
  object lbPresetCode: TLabel
    Left = 680
    Top = 284
    Width = 60
    Height = 13
    Caption = 'Preset code:'
  end
  object TilesetScroll: TScrollBar
    Left = 650
    Top = 24
    Width = 16
    Height = 576
    Kind = sbVertical
    LargeChange = 2
    Max = 39
    PageSize = 0
    TabOrder = 0
    OnChange = DrawTileset
  end
  object TilesetGrid: TCheckBox
    Left = 8
    Top = 4
    Width = 73
    Height = 17
    Caption = 'Show Grid'
    TabOrder = 1
    OnClick = DrawTileset
  end
  object TilesetMarkTiles: TCheckBox
    Left = 88
    Top = 4
    Width = 73
    Height = 17
    Caption = 'Mark tiles'
    TabOrder = 2
    OnClick = DrawTileset
  end
  object cbPresetHelper: TCheckBox
    Left = 552
    Top = 4
    Width = 105
    Height = 17
    Caption = 'Preset Helper >>'
    TabOrder = 3
    OnClick = cbPresetHelperClick
  end
  object rbContinuousTiles: TRadioButton
    Left = 680
    Top = 24
    Width = 113
    Height = 17
    Caption = 'Continuous tiles'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = PresetTypeChange
  end
  object rbCustomTiles: TRadioButton
    Left = 792
    Top = 24
    Width = 81
    Height = 17
    Caption = 'Custom tiles'
    TabOrder = 5
    OnClick = PresetTypeChange
  end
  object sePresetWidth: TSpinEdit
    Left = 712
    Top = 48
    Width = 41
    Height = 22
    MaxValue = 8
    MinValue = 1
    TabOrder = 6
    Value = 1
    Visible = False
    OnChange = SetCustomPresetSize
  end
  object sePresetHeight: TSpinEdit
    Left = 776
    Top = 48
    Width = 41
    Height = 22
    MaxValue = 8
    MinValue = 1
    TabOrder = 7
    Value = 1
    Visible = False
    OnChange = SetCustomPresetSize
  end
  object mmPresetDefinition: TMemo
    Left = 680
    Top = 304
    Width = 193
    Height = 89
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 8
  end
  object btnClearPreset: TButton
    Left = 824
    Top = 46
    Width = 49
    Height = 25
    Caption = 'Clear'
    TabOrder = 9
    Visible = False
    OnClick = ClearPreset
  end
  object btnCopyPresetCode: TButton
    Left = 776
    Top = 280
    Width = 97
    Height = 21
    Caption = 'Copy to clipboard'
    TabOrder = 10
    OnClick = btnCopyPresetCodeClick
  end
  object btnUsePreset: TButton
    Left = 680
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Use preset'
    TabOrder = 11
    OnClick = btnUsePresetClick
  end
end
