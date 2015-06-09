object EventDialog: TEventDialog
  Left = 192
  Top = 114
  Width = 1024
  Height = 620
  Caption = 'Event Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 348
    Width = 1016
    Height = 4
    Cursor = crVSplit
    Align = alBottom
  end
  object EventGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 1016
    Height = 348
    Align = alClient
    ColCount = 6
    DefaultRowHeight = 18
    RowCount = 65
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object LowerPanel: TPanel
    Left = 0
    Top = 352
    Width = 1016
    Height = 234
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 234
    TabOrder = 1
    object ConditionGrid: TStringGrid
      Left = 696
      Top = 0
      Width = 320
      Height = 234
      Align = alRight
      ColCount = 4
      DefaultRowHeight = 18
      RowCount = 49
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      TabOrder = 0
      RowHeights = (
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18)
    end
  end
end
