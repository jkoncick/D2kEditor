object MissionLauncher: TMissionLauncher
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Mission Launcher'
  ClientHeight = 567
  ClientWidth = 832
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbMissionName: TLabel
    Left = 248
    Top = 12
    Width = 69
    Height = 13
    Caption = 'Mission Name:'
  end
  object lbMissionAuthor: TLabel
    Left = 584
    Top = 12
    Width = 34
    Height = 13
    Caption = 'Author:'
  end
  object lbMissionNumber: TLabel
    Left = 736
    Top = 12
    Width = 48
    Height = 13
    Caption = 'Mission #:'
  end
  object lbMissionFileName: TLabel
    Left = 248
    Top = 508
    Width = 48
    Height = 13
    Caption = 'File name:'
  end
  object lbMissionList: TListBox
    Left = 0
    Top = 0
    Width = 241
    Height = 567
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbMissionListClick
    OnDblClick = btnOpenMissionInEditorClick
  end
  object mMissionBriefing: TMemo
    Left = 248
    Top = 40
    Width = 569
    Height = 457
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object edMissionName: TEdit
    Left = 320
    Top = 8
    Width = 257
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object edMissionAuthor: TEdit
    Left = 624
    Top = 8
    Width = 105
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object edMissionNumber: TEdit
    Left = 792
    Top = 8
    Width = 25
    Height = 21
    ReadOnly = True
    TabOrder = 4
  end
  object edMissionFileName: TEdit
    Left = 304
    Top = 504
    Width = 169
    Height = 21
    ReadOnly = True
    TabOrder = 5
  end
  object btnLaunchGame: TButton
    Left = 736
    Top = 504
    Width = 83
    Height = 49
    Caption = 'Launch Game'
    TabOrder = 6
    OnClick = btnLaunchGameClick
  end
  object btnOpenMissionInEditor: TButton
    Left = 360
    Top = 532
    Width = 115
    Height = 21
    Caption = 'Open in Editor'
    TabOrder = 7
    OnClick = btnOpenMissionInEditorClick
  end
end
