object EventDialog: TEventDialog
  Left = 234
  Top = 19
  Width = 1280
  Height = 720
  Caption = 'Events and Conditions'
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 1280
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
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 427
    Width = 1272
    Height = 4
    Cursor = crVSplit
    Align = alBottom
  end
  object BevelSizeHolder: TBevel
    Left = 0
    Top = 0
    Width = 1264
    Height = 50
  end
  object EventGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 1272
    Height = 427
    Align = alClient
    ColCount = 7
    DefaultRowHeight = 18
    RowCount = 65
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
    ParentFont = False
    PopupMenu = EventGridPopupMenu
    ScrollBars = ssVertical
    TabOrder = 0
    OnDrawCell = EventGridDrawCell
    OnKeyDown = EventGridKeyDown
    OnMouseDown = EventGridMouseDown
    OnMouseWheelDown = EventGridMouseWheelDown
    OnMouseWheelUp = EventGridMouseWheelUp
    OnSelectCell = EventGridSelectCell
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
  object LowerPanel: TPanel
    Left = 0
    Top = 431
    Width = 1272
    Height = 255
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 255
    TabOrder = 1
    object ConditionGrid: TStringGrid
      Left = 748
      Top = 0
      Width = 312
      Height = 255
      Align = alLeft
      ColCount = 4
      DefaultRowHeight = 18
      RowCount = 49
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goThumbTracking]
      PopupMenu = ConditionGridPopupMenu
      TabOrder = 0
      OnDblClick = ConditionGridDblClick
      OnKeyDown = ConditionGridKeyDown
      OnMouseDown = ConditionGridMouseDown
      OnMouseWheelDown = ConditionGridMouseWheelDown
      OnMouseWheelUp = ConditionGridMouseWheelUp
      OnSelectCell = ConditionGridSelectCell
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
    object EventPropertiesPanel: TPanel
      Left = 0
      Top = 0
      Width = 240
      Height = 255
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object lblEventProperties: TLabel
        Left = 4
        Top = 0
        Width = 77
        Height = 13
        Caption = 'Event properties'
      end
      object lblEventType: TLabel
        Left = 4
        Top = 28
        Width = 54
        Height = 13
        Caption = 'Event type:'
      end
      object lblEventNote: TLabel
        Left = 4
        Top = 176
        Width = 26
        Height = 13
        Caption = 'Note:'
      end
      object cbEventType: TComboBox
        Left = 64
        Top = 24
        Width = 169
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbEventTypeChange
        OnKeyDown = EventGridKeyDown
      end
      object epEventPlayer: TPanel
        Left = 0
        Top = 48
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 1
        object lblEventPlayer: TLabel
          Left = 4
          Top = 8
          Width = 32
          Height = 13
          Caption = 'Player:'
        end
        object cbEventPlayer: TComboBox
          Left = 64
          Top = 4
          Width = 120
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object epEventPosition: TPanel
        Left = 0
        Top = 64
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 2
        object lblEventPosition: TLabel
          Left = 4
          Top = 8
          Width = 40
          Height = 13
          Caption = 'Position:'
        end
        object seEventPositionX: TSpinEdit
          Left = 64
          Top = 4
          Width = 49
          Height = 22
          MaxValue = 127
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object seEventPositionY: TSpinEdit
          Left = 120
          Top = 4
          Width = 49
          Height = 22
          MaxValue = 127
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object btnEventPositionGotoMap: TButton
          Left = 176
          Top = 4
          Width = 59
          Height = 22
          Caption = 'Go to map'
          TabOrder = 2
          OnClick = btnEventPositionGotoMapClick
        end
      end
      object epDeployAction: TPanel
        Left = 0
        Top = 80
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 3
        object lblDeployAction: TLabel
          Left = 4
          Top = 8
          Width = 68
          Height = 13
          Caption = 'Deploy action:'
        end
        object cbDeployAction: TComboBox
          Left = 88
          Top = 4
          Width = 97
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object epAllegiance: TPanel
        Left = 0
        Top = 96
        Width = 240
        Height = 58
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        object lblAllegiance: TLabel
          Left = 4
          Top = 36
          Width = 52
          Height = 13
          Caption = 'Allegiance:'
        end
        object lblAllegianceTo: TLabel
          Left = 112
          Top = 8
          Width = 16
          Height = 13
          Caption = '-->'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbAllegianceSource: TComboBox
          Left = 4
          Top = 4
          Width = 101
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object cbAllegianceTarget: TComboBox
          Left = 132
          Top = 4
          Width = 101
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object cbAllegianceType: TComboBox
          Left = 64
          Top = 32
          Width = 97
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
      end
      object epSetFlag: TPanel
        Left = 0
        Top = 108
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 5
        object lblSetFlag: TLabel
          Left = 4
          Top = 8
          Width = 39
          Height = 13
          Caption = 'Set flag:'
        end
        object seFlagNumber: TSpinEdit
          Left = 64
          Top = 4
          Width = 49
          Height = 22
          MaxValue = 47
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = seFlagNumberChange
        end
        object rbFlagTrue: TRadioButton
          Left = 120
          Top = 6
          Width = 49
          Height = 17
          Caption = 'True'
          Checked = True
          TabOrder = 1
          TabStop = True
        end
        object rbFlagFalse: TRadioButton
          Left = 168
          Top = 6
          Width = 56
          Height = 17
          Caption = 'False'
          TabOrder = 2
        end
      end
      object epRadius: TPanel
        Left = 0
        Top = 124
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 6
        object lblRadius: TLabel
          Left = 4
          Top = 8
          Width = 36
          Height = 13
          Caption = 'Radius:'
        end
        object seRadius: TSpinEdit
          Left = 64
          Top = 4
          Width = 49
          Height = 22
          MaxValue = 127
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
      end
      object epEventValue: TPanel
        Left = 0
        Top = 140
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 7
        object lblEventValue: TLabel
          Left = 4
          Top = 8
          Width = 30
          Height = 13
          Caption = 'Value:'
        end
        object edEventValue: TEdit
          Left = 64
          Top = 4
          Width = 105
          Height = 21
          TabOrder = 0
          Text = '0'
        end
      end
      object epMessage: TPanel
        Left = 0
        Top = 220
        Width = 240
        Height = 62
        BevelOuter = bvNone
        TabOrder = 8
        object lblMessage: TLabel
          Left = 4
          Top = 8
          Width = 60
          Height = 13
          Caption = 'Message ID:'
        end
        object seMessageId: TSpinEdit
          Left = 72
          Top = 4
          Width = 65
          Height = 22
          MaxValue = 2047
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = seMessageIdChange
        end
        object edMessageText: TEdit
          Left = 4
          Top = 36
          Width = 232
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object btnCustomMsgText: TButton
          Left = 152
          Top = 4
          Width = 81
          Height = 22
          Caption = 'Custom text'
          TabOrder = 2
          OnClick = btnCustomMsgTextClick
        end
      end
      object btnApplyEventChanges: TBitBtn
        Left = 120
        Top = 152
        Width = 115
        Height = 25
        Caption = 'Apply changes'
        TabOrder = 9
        OnClick = btnApplyEventChangesClick
        Kind = bkOK
      end
      object edEventNote: TEdit
        Left = 4
        Top = 192
        Width = 232
        Height = 21
        TabOrder = 10
      end
      object epMusic: TPanel
        Left = 0
        Top = 132
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 11
        object lblMusic: TLabel
          Left = 4
          Top = 8
          Width = 31
          Height = 13
          Caption = 'Music:'
        end
        object cbMusicName: TComboBox
          Left = 64
          Top = 4
          Width = 145
          Height = 21
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object epSound: TPanel
        Left = 0
        Top = 140
        Width = 240
        Height = 30
        BevelOuter = bvNone
        TabOrder = 12
        object lblSound: TLabel
          Left = 4
          Top = 8
          Width = 34
          Height = 13
          Caption = 'Sound:'
        end
        object cbSoundName: TComboBox
          Left = 64
          Top = 4
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
    object EventUnitListPanel: TPanel
      Left = 240
      Top = 0
      Width = 288
      Height = 255
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object UnitSelectionList: TListBox
        Left = 0
        Top = 16
        Width = 128
        Height = 239
        Align = alLeft
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = UnitSelectionListDblClick
      end
      object EventUnitList: TListBox
        Left = 152
        Top = 16
        Width = 128
        Height = 239
        Align = alRight
        ItemHeight = 13
        TabOrder = 1
        OnDblClick = EventUnitListDblClick
      end
      object EventUnitListLabelPanel: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object lblUnitSelection: TLabel
          Left = 0
          Top = 0
          Width = 64
          Height = 13
          Caption = 'Unit selection'
        end
        object lblUnitList: TLabel
          Left = 152
          Top = 0
          Width = 65
          Height = 13
          Caption = 'Units in event'
        end
      end
      object EventUnitListPaddingPanel: TPanel
        Left = 280
        Top = 16
        Width = 8
        Height = 239
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 3
      end
      object btnAddUnit: TButton
        Left = 128
        Top = 24
        Width = 24
        Height = 49
        Hint = 'Add unit'
        Caption = '-->'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = btnAddUnitClick
      end
      object btnDeleteUnit: TButton
        Left = 128
        Top = 80
        Width = 24
        Height = 25
        Hint = 'Remove selected unit'
        Caption = '<--'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = btnDeleteUnitClick
      end
      object btnDeleteLastUnit: TButton
        Left = 128
        Top = 112
        Width = 24
        Height = 25
        Hint = 'Remove last unit'
        Caption = 'X'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = btnDeleteLastUnitClick
      end
      object btnDeleteAllUnits: TButton
        Left = 128
        Top = 144
        Width = 24
        Height = 25
        Hint = 'Remove all units'
        Caption = 'C'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = btnDeleteAllUnitsClick
      end
      object btnMoveUnitUp: TButton
        Left = 128
        Top = 192
        Width = 24
        Height = 25
        Hint = 'Move unit up'
        Caption = '^'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = btnMoveUnitUpClick
      end
      object btnMoveUnitDown: TButton
        Left = 128
        Top = 216
        Width = 24
        Height = 25
        Hint = 'Move unit down'
        Caption = 'v'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        OnClick = btnMoveUnitDownClick
      end
    end
    object EventConditionListPanel: TPanel
      Left = 528
      Top = 0
      Width = 220
      Height = 255
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      object EventConditionList: TCheckListBox
        Left = 0
        Top = 16
        Width = 196
        Height = 215
        OnClickCheck = EventConditionListClickCheck
        Align = alLeft
        ItemHeight = 16
        Style = lbOwnerDrawFixed
        TabOrder = 0
        OnDblClick = EventConditionListDblClick
      end
      object EventConditionListLabelPanel: TPanel
        Left = 0
        Top = 0
        Width = 220
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblEventConditions: TLabel
          Left = 0
          Top = 0
          Width = 90
          Height = 13
          Caption = 'Conditions in event'
        end
      end
      object btnAddCondition: TButton
        Left = 196
        Top = 56
        Width = 24
        Height = 33
        Hint = 'Add condition'
        Caption = '<--'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = btnAddConditionClick
      end
      object btnDeleteCondition: TButton
        Left = 196
        Top = 96
        Width = 24
        Height = 25
        Hint = 'Remove selected condition'
        Caption = '-->'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = btnDeleteConditionClick
      end
      object btnDeleteLastCondition: TButton
        Left = 196
        Top = 128
        Width = 24
        Height = 25
        Hint = 'Remove last condition'
        Caption = 'X'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = btnDeleteLastConditionClick
      end
      object btnDeleteAllConditions: TButton
        Left = 196
        Top = 160
        Width = 24
        Height = 25
        Hint = 'Remove all conditions'
        Caption = 'C'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = btnDeleteAllConditionsClick
      end
      object btnPlusCondition: TButton
        Left = 196
        Top = 16
        Width = 24
        Height = 33
        Hint = 'Create new condition and add into list'
        Caption = '+'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = btnPlusConditionClick
      end
      object EventConditionListButtonPanel: TPanel
        Left = 0
        Top = 231
        Width = 220
        Height = 24
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 7
        object btnEventConditionListCopy: TButton
          Left = 0
          Top = 0
          Width = 96
          Height = 24
          Caption = 'Copy list'
          TabOrder = 0
          OnClick = btnEventConditionListCopyClick
        end
        object btnEventConditionListPaste: TButton
          Left = 100
          Top = 0
          Width = 96
          Height = 25
          Caption = 'Paste list'
          TabOrder = 1
          OnClick = btnEventConditionListPasteClick
        end
      end
      object btnMoveConditionUp: TButton
        Left = 196
        Top = 192
        Width = 24
        Height = 25
        Hint = 'Move condition up'
        Caption = '^'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = btnMoveConditionUpClick
      end
      object btnMoveConditionDown: TButton
        Left = 196
        Top = 216
        Width = 24
        Height = 25
        Hint = 'Move condition down'
        Caption = 'v'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        OnClick = btnMoveConditionDownClick
      end
    end
    object ConditionPropertiesPanel: TPanel
      Left = 1060
      Top = 0
      Width = 208
      Height = 255
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 4
      object lblConditionProperties: TLabel
        Left = 4
        Top = 0
        Width = 93
        Height = 13
        Caption = 'Condition properties'
      end
      object lblConditionType: TLabel
        Left = 4
        Top = 28
        Width = 70
        Height = 13
        Caption = 'Condition type:'
      end
      object lblConditionNote: TLabel
        Left = 4
        Top = 176
        Width = 26
        Height = 13
        Caption = 'Note:'
      end
      object cbConditionType: TComboBox
        Left = 80
        Top = 24
        Width = 128
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbConditionTypeChange
        OnKeyDown = ConditionGridKeyDown
      end
      object cpConditionPlayer: TPanel
        Left = 0
        Top = 48
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 1
        object lblConditionPlayer: TLabel
          Left = 4
          Top = 8
          Width = 32
          Height = 13
          Caption = 'Player:'
        end
        object cbConditionPlayer: TComboBox
          Left = 48
          Top = 4
          Width = 120
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object cpConditionPosition: TPanel
        Left = 0
        Top = 64
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 2
        object lblConditionPosition: TLabel
          Left = 4
          Top = 8
          Width = 40
          Height = 13
          Caption = 'Position:'
        end
        object seConditionPositionX: TSpinEdit
          Left = 48
          Top = 4
          Width = 49
          Height = 22
          MaxValue = 127
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object seConditionPositionY: TSpinEdit
          Left = 104
          Top = 4
          Width = 49
          Height = 22
          MaxValue = 127
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object btnConditionPositionGotoMap: TButton
          Left = 156
          Top = 4
          Width = 52
          Height = 22
          Caption = 'To map'
          TabOrder = 2
          OnClick = btnConditionPositionGotoMapClick
        end
      end
      object cpBuildingType: TPanel
        Left = 0
        Top = 80
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 3
        object lblBuildingType: TLabel
          Left = 4
          Top = 8
          Width = 40
          Height = 13
          Caption = 'Building:'
        end
        object cbBuildingType: TComboBox
          Left = 48
          Top = 4
          Width = 160
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object cpUnitType: TPanel
        Left = 0
        Top = 96
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 4
        object lblUnitType: TLabel
          Left = 4
          Top = 8
          Width = 22
          Height = 13
          Caption = 'Unit:'
        end
        object cbUnitType: TComboBox
          Left = 48
          Top = 4
          Width = 160
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object cpTimer: TPanel
        Left = 0
        Top = 112
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 5
        object lblTimer: TLabel
          Left = 4
          Top = 8
          Width = 23
          Height = 13
          Caption = 'Time'
        end
        object cbTimerCompareFunc: TComboBox
          Left = 48
          Top = 4
          Width = 49
          Height = 21
          Hint = 'Comparison function'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
        end
        object edTimerTime: TEdit
          Left = 104
          Top = 4
          Width = 104
          Height = 21
          Hint = 'Time amount'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          Text = '0'
        end
      end
      object cpInterval: TPanel
        Left = 0
        Top = 128
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 6
        object lblInterval: TLabel
          Left = 4
          Top = 8
          Width = 38
          Height = 13
          Caption = 'Interval:'
        end
        object edStartDelay: TEdit
          Left = 48
          Top = 4
          Width = 72
          Height = 21
          Hint = 'Start delay'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '0'
        end
        object edInterval: TEdit
          Left = 128
          Top = 4
          Width = 72
          Height = 21
          Hint = 'Time between intervals'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = '0'
        end
      end
      object cpConditionValue: TPanel
        Left = 0
        Top = 144
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 7
        object lblConditionValue: TLabel
          Left = 4
          Top = 8
          Width = 30
          Height = 13
          Caption = 'Value:'
        end
        object edConditionValue: TEdit
          Left = 64
          Top = 4
          Width = 80
          Height = 21
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Text = '0'
        end
      end
      object cpCasualties: TPanel
        Left = 0
        Top = 144
        Width = 208
        Height = 30
        BevelOuter = bvNone
        TabOrder = 8
        object lblCasualtiesRatio: TLabel
          Left = 4
          Top = 8
          Width = 51
          Height = 13
          Caption = 'Proportion:'
        end
        object edCasualtiesRatio: TEdit
          Left = 64
          Top = 4
          Width = 80
          Height = 21
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Text = '0'
        end
      end
      object btnApplyConditionChanges: TBitBtn
        Left = 85
        Top = 152
        Width = 115
        Height = 25
        Caption = 'Apply changes'
        TabOrder = 9
        OnClick = btnApplyConditionChangesClick
        Kind = bkOK
      end
      object edConditionNote: TEdit
        Left = 4
        Top = 192
        Width = 196
        Height = 21
        TabOrder = 10
      end
      object cbMarkEventsHavingCondition: TCheckBox
        Left = 8
        Top = 224
        Width = 185
        Height = 17
        Caption = 'Mark events having this condition'
        TabOrder = 11
        OnClick = cbMarkEventsHavingConditionClick
      end
    end
  end
  object CreateEventsPanel: TPanel
    Left = 528
    Top = 96
    Width = 209
    Height = 177
    TabOrder = 2
    Visible = False
    object lblCreateEvents: TLabel
      Left = 24
      Top = 24
      Width = 128
      Height = 13
      Caption = 'Create Unit spawn event(s)'
    end
    object lblCreateEventsPlayer: TLabel
      Left = 24
      Top = 60
      Width = 32
      Height = 13
      Caption = 'Player:'
    end
    object lblCreateEventsCount: TLabel
      Left = 24
      Top = 92
      Width = 87
      Height = 13
      Caption = 'Number of events:'
    end
    object btnCreateEventsCancel: TBitBtn
      Left = 24
      Top = 128
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = btnCreateEventsCancelClick
      Kind = bkCancel
    end
    object btnCreateEventsOk: TBitBtn
      Left = 112
      Top = 128
      Width = 75
      Height = 25
      TabOrder = 1
      OnClick = btnCreateEventsOkClick
      Kind = bkOK
    end
    object cbCreateEventsPlayer: TComboBox
      Left = 64
      Top = 56
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = cbCreateEventsPlayerChange
    end
    object seCreateEventsNum: TSpinEdit
      Left = 128
      Top = 88
      Width = 57
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 3
      Value = 1
    end
    object cbCreateEventsAllocIndex: TCheckBox
      Left = 24
      Top = 90
      Width = 97
      Height = 17
      Caption = 'Use alloc. index:'
      TabOrder = 4
    end
  end
  object EventGridPopupMenu: TPopupMenu
    Left = 392
    object Addevent1: TMenuItem
      Caption = 'Add event'
      ShortCut = 45
      OnClick = Addevent1Click
    end
    object Insertevent1: TMenuItem
      Caption = 'Insert before'
      ShortCut = 16429
      OnClick = Insertevent1Click
    end
    object Duplicateevent1: TMenuItem
      Caption = 'Duplicate event'
      ShortCut = 8237
      OnClick = Duplicateevent1Click
    end
    object Deleteselectedevent1: TMenuItem
      Caption = 'Delete event'
      ShortCut = 46
      OnClick = Deleteselectedevent1Click
    end
    object Deletelastevent1: TMenuItem
      Caption = 'Delete last event'
      ShortCut = 16430
      OnClick = Deletelastevent1Click
    end
    object MoveUp1: TMenuItem
      Caption = 'Move Up'
      ShortCut = 33
      OnClick = MoveUp1Click
    end
    object MoveDown1: TMenuItem
      Caption = 'Move Down'
      ShortCut = 34
      OnClick = MoveDown1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Createevent1: TMenuItem
      Caption = 'Create events'
      object Unitspawn1: TMenuItem
        Caption = 'Unit spawn'
        ShortCut = 112
        OnClick = Unitspawn1Click
      end
      object Harvesterreplacement1: TMenuItem
        Caption = 'Harvester replacement'
        ShortCut = 113
        OnClick = Harvesterreplacement1Click
      end
      object Annihilatemessage1: TMenuItem
        Caption = 'Side annihilated message'
        ShortCut = 114
        OnClick = Annihilatemessage1Click
      end
    end
    object Createrunonceflag1: TMenuItem
      Caption = 'Add run-once flag'
      ShortCut = 115
      OnClick = Createrunonceflag1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Showkeyshortcuts1: TMenuItem
      Caption = 'Show key shortcuts'
      OnClick = Showkeyshortcuts1Click
    end
  end
  object ConditionGridPopupMenu: TPopupMenu
    Left = 424
    object Addcondition1: TMenuItem
      Caption = 'Add condition'
      ShortCut = 45
      OnClick = Addcondition1Click
    end
    object Duplicatecondition1: TMenuItem
      Caption = 'Duplicate condition'
      ShortCut = 8237
      OnClick = Duplicatecondition1Click
    end
    object Deleteselectedcondition1: TMenuItem
      Caption = 'Delete condition'
      ShortCut = 46
      OnClick = Deleteselectedcondition1Click
    end
    object Deletelastcondition1: TMenuItem
      Caption = 'Delete last condition'
      ShortCut = 16430
      OnClick = Deletelastcondition1Click
    end
    object MoveUp2: TMenuItem
      Caption = 'Move Up'
      ShortCut = 33
      OnClick = MoveUp2Click
    end
    object MoveDown2: TMenuItem
      Caption = 'Move Down'
      ShortCut = 34
      OnClick = MoveDown2Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Showkeyshortcuts2: TMenuItem
      Caption = 'Show key shortcuts'
      OnClick = Showkeyshortcuts2Click
    end
  end
end
