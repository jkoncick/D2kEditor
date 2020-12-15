object MissionLauncher: TMissionLauncher
  Left = 137
  Top = 111
  Width = 1200
  Height = 608
  Caption = 'Mission Browser / Launcher'
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 1024
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
  object sgMissionList: TStringGrid
    Left = 0
    Top = 0
    Width = 680
    Height = 574
    Align = alClient
    ColCount = 1
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goThumbTracking]
    PopupMenu = pmMissionList
    TabOrder = 0
    OnDblClick = sgMissionListDblClick
    OnMouseWheelDown = sgMissionListMouseWheelDown
    OnMouseWheelUp = sgMissionListMouseWheelUp
    OnSelectCell = sgMissionListSelectCell
  end
  object pnMissionDetails: TPanel
    Left = 680
    Top = 0
    Width = 512
    Height = 574
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object mMissionBriefing: TMemo
      Left = 0
      Top = 0
      Width = 512
      Height = 521
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object pnMissionDetailsControls: TPanel
      Left = 0
      Top = 521
      Width = 512
      Height = 53
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lbMissionFileName: TLabel
        Left = 4
        Top = 30
        Width = 48
        Height = 13
        Caption = 'File name:'
      end
      object lbDifficultyLevel: TLabel
        Left = 280
        Top = 8
        Width = 43
        Height = 13
        Caption = 'Difficulty:'
      end
      object btnLaunchGame: TButton
        Left = 424
        Top = 4
        Width = 83
        Height = 45
        Caption = 'Launch Game'
        TabOrder = 0
        OnClick = btnLaunchGameClick
      end
      object btnOpenMissionInEditor: TButton
        Left = 320
        Top = 28
        Width = 97
        Height = 21
        Caption = 'Open in Editor'
        TabOrder = 1
        OnClick = btnOpenMissionInEditorClick
      end
      object cbDifficultyLevel: TComboBox
        Left = 328
        Top = 4
        Width = 89
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 2
        Text = 'Normal'
        Items.Strings = (
          'Easy'
          'Normal'
          'Hard')
      end
      object edMissionFileName: TEdit
        Left = 56
        Top = 28
        Width = 257
        Height = 21
        ReadOnly = True
        TabOrder = 3
      end
    end
  end
  object pmMissionList: TPopupMenu
  end
end
