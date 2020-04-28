object MapStatsDialog: TMapStatsDialog
  Left = 192
  Top = 114
  Width = 752
  Height = 534
  Caption = 'Map statistics'
  Color = clBtnFace
  Constraints.MaxWidth = 752
  Constraints.MinHeight = 400
  Constraints.MinWidth = 752
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object StatsGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 744
    Height = 500
    Align = alClient
    ColCount = 10
    DefaultRowHeight = 18
    RowCount = 8
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goThumbTracking]
    TabOrder = 0
  end
end
