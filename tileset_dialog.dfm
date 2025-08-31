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
end
