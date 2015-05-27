object TilesetDialog: TTilesetDialog
  Left = 326
  Top = 66
  BorderStyle = bsDialog
  Caption = 'Tile block selection'
  ClientHeight = 608
  ClientWidth = 674
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = DrawTileset
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
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '1 x 1'
    OnClick = SetBlockSize
  end
  object sbPreset22: TSpeedButton
    Tag = 2
    Left = 264
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '2 x 2'
    OnClick = SetBlockSize
  end
  object sbPreset33: TSpeedButton
    Tag = 3
    Left = 312
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '3 x 3'
    OnClick = SetBlockSize
  end
  object sbPreset44: TSpeedButton
    Tag = 4
    Left = 360
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '4 x 4'
    OnClick = SetBlockSize
  end
  object sbPreset21: TSpeedButton
    Tag = 5
    Left = 408
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '2 x 1'
    OnClick = SetBlockSize
  end
  object sbPreset12: TSpeedButton
    Tag = 6
    Left = 456
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '1 x 2'
    OnClick = SetBlockSize
  end
  object sbPreset32: TSpeedButton
    Tag = 7
    Left = 504
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '3 x 2'
    OnClick = SetBlockSize
  end
  object sbPreset23: TSpeedButton
    Tag = 8
    Left = 552
    Top = 3
    Width = 49
    Height = 19
    GroupIndex = 1
    Caption = '2 x 3'
    OnClick = SetBlockSize
  end
  object TilesetScroll: TScrollBar
    Left = 648
    Top = 24
    Width = 16
    Height = 576
    Kind = sbVertical
    Max = 22
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
end
