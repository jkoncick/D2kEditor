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
  object Block11: TButton
    Tag = 1
    Left = 164
    Top = 4
    Width = 49
    Height = 17
    Caption = '1 x 1'
    TabOrder = 2
    OnClick = SetBlockSize
  end
  object Block22: TButton
    Tag = 2
    Left = 212
    Top = 4
    Width = 49
    Height = 17
    Caption = '2 x 2'
    TabOrder = 3
    OnClick = SetBlockSize
  end
  object Block33: TButton
    Tag = 3
    Left = 260
    Top = 4
    Width = 49
    Height = 17
    Caption = '3 x 3'
    TabOrder = 4
    OnClick = SetBlockSize
  end
  object Block44: TButton
    Tag = 4
    Left = 308
    Top = 4
    Width = 49
    Height = 17
    Caption = '4 x 4'
    TabOrder = 5
    OnClick = SetBlockSize
  end
  object Block21: TButton
    Tag = 5
    Left = 356
    Top = 4
    Width = 49
    Height = 17
    Caption = '2 x 1'
    TabOrder = 6
    OnClick = SetBlockSize
  end
  object Block12: TButton
    Tag = 6
    Left = 404
    Top = 4
    Width = 49
    Height = 17
    Caption = '1 x 2'
    TabOrder = 7
    OnClick = SetBlockSize
  end
  object Block32: TButton
    Tag = 7
    Left = 452
    Top = 4
    Width = 49
    Height = 17
    Caption = '3 x 2'
    TabOrder = 8
    OnClick = SetBlockSize
  end
  object Block23: TButton
    Tag = 8
    Left = 500
    Top = 4
    Width = 49
    Height = 17
    Caption = '2 x 3'
    TabOrder = 9
    OnClick = SetBlockSize
  end
  object TilesetMarkTiles: TCheckBox
    Left = 88
    Top = 4
    Width = 73
    Height = 17
    Caption = 'Mark tiles'
    TabOrder = 10
    OnClick = DrawTileset
  end
end
