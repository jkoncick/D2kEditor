object MissionLauncher: TMissionLauncher
  Left = 137
  Top = 111
  BorderStyle = bsDialog
  Caption = 'Mission Launcher / Browser'
  ClientHeight = 574
  ClientWidth = 1192
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbMissionFileName: TLabel
    Left = 688
    Top = 546
    Width = 48
    Height = 13
    Caption = 'File name:'
  end
  object lbDifficultyLevel: TLabel
    Left = 952
    Top = 524
    Width = 43
    Height = 13
    Caption = 'Difficulty:'
  end
  object mMissionBriefing: TMemo
    Left = 688
    Top = 0
    Width = 505
    Height = 513
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object edMissionFileName: TEdit
    Left = 744
    Top = 544
    Width = 249
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object btnLaunchGame: TButton
    Left = 1104
    Top = 520
    Width = 83
    Height = 45
    Caption = 'Launch Game'
    TabOrder = 2
    OnClick = btnLaunchGameClick
  end
  object btnOpenMissionInEditor: TButton
    Left = 1000
    Top = 544
    Width = 97
    Height = 21
    Caption = 'Open in Editor'
    TabOrder = 3
    OnClick = btnOpenMissionInEditorClick
  end
  object cbDifficultyLevel: TComboBox
    Left = 1008
    Top = 520
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 4
    Text = 'Normal'
    Items.Strings = (
      'Easy'
      'Normal'
      'Hard')
  end
  object sgMissionList: TStringGrid
    Left = 0
    Top = 0
    Width = 681
    Height = 574
    Align = alLeft
    ColCount = 6
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goThumbTracking]
    TabOrder = 5
    OnDblClick = sgMissionListDblClick
    OnMouseWheelDown = sgMissionListMouseWheelDown
    OnMouseWheelUp = sgMissionListMouseWheelUp
    OnSelectCell = sgMissionListSelectCell
  end
end
