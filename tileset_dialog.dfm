object TilesetDialog: TTilesetDialog
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Tile block selection'
  ClientHeight = 544
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
    Height = 512
    OnDblClick = TilesetImageDblClick
    OnMouseDown = TilesetImageMouseDown
  end
  object TilesetScroll: TScrollBar
    Left = 648
    Top = 24
    Width = 16
    Height = 512
    Kind = sbVertical
    Max = 24
    PageSize = 0
    TabOrder = 0
    OnChange = DrawTileset
  end
  object TilesetGrid: TCheckBox
    Left = 8
    Top = 4
    Width = 97
    Height = 17
    Caption = 'Show Grid'
    TabOrder = 1
    OnClick = DrawTileset
  end
  object Block11: TButton
    Tag = 1
    Left = 92
    Top = 4
    Width = 49
    Height = 17
    Caption = '1 x 1'
    TabOrder = 2
    OnClick = SetBlockSize
  end
  object Block22: TButton
    Tag = 2
    Left = 140
    Top = 4
    Width = 49
    Height = 17
    Caption = '2 x 2'
    TabOrder = 3
    OnClick = SetBlockSize
  end
  object Block33: TButton
    Tag = 3
    Left = 188
    Top = 4
    Width = 49
    Height = 17
    Caption = '3 x 3'
    TabOrder = 4
    OnClick = SetBlockSize
  end
  object Block44: TButton
    Tag = 4
    Left = 236
    Top = 4
    Width = 49
    Height = 17
    Caption = '4 x 4'
    TabOrder = 5
    OnClick = SetBlockSize
  end
  object Block21: TButton
    Tag = 5
    Left = 284
    Top = 4
    Width = 49
    Height = 17
    Caption = '2 x 1'
    TabOrder = 6
    OnClick = SetBlockSize
  end
  object Block12: TButton
    Tag = 6
    Left = 332
    Top = 4
    Width = 49
    Height = 17
    Caption = '1 x 2'
    TabOrder = 7
    OnClick = SetBlockSize
  end
  object Block32: TButton
    Tag = 7
    Left = 380
    Top = 4
    Width = 49
    Height = 17
    Caption = '3 x 2'
    TabOrder = 8
    OnClick = SetBlockSize
  end
  object Block23: TButton
    Tag = 8
    Left = 428
    Top = 4
    Width = 49
    Height = 17
    Caption = '2 x 3'
    TabOrder = 9
    OnClick = SetBlockSize
  end
end
