object BlockPresetDialog: TBlockPresetDialog
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Block preset selection'
  ClientHeight = 424
  ClientWidth = 960
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BlockPresetImage: TImage
    Left = 0
    Top = 40
    Width = 960
    Height = 384
    OnMouseDown = BlockPresetImageMouseDown
  end
end
