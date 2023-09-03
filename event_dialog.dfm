object EventDialog: TEventDialog
  Left = 190
  Top = 0
  Width = 1378
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 401
    Width = 1370
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    OnMoved = FormResize
  end
  object BevelSizeHolder: TBevel
    Left = 0
    Top = 0
    Width = 1264
    Height = 50
  end
  object UpperPanel: TPanel
    Left = 0
    Top = 0
    Width = 1370
    Height = 401
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object EventGrid: TStringGrid
      Left = 168
      Top = 0
      Width = 1202
      Height = 401
      Align = alClient
      ColCount = 8
      DefaultRowHeight = 18
      RowCount = 2
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
      OnMouseUp = EventGridMouseUp
      OnMouseWheelDown = EventGridMouseWheelDown
      OnMouseWheelUp = EventGridMouseWheelUp
      OnSelectCell = EventGridSelectCell
      RowHeights = (
        18
        18)
    end
    object lbEventTypeList: TListBox
      Left = 0
      Top = 0
      Width = 168
      Height = 401
      Align = alLeft
      ItemHeight = 13
      TabOrder = 1
      OnClick = lbEventTypeListClick
      OnDblClick = lbEventTypeListDblClick
    end
  end
  object LowerPanel: TPanel
    Left = 0
    Top = 405
    Width = 1370
    Height = 288
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 287
    TabOrder = 0
    object ConditionGrid: TStringGrid
      Left = 748
      Top = 0
      Width = 312
      Height = 288
      Align = alLeft
      ColCount = 4
      DefaultRowHeight = 18
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
      PopupMenu = ConditionGridPopupMenu
      TabOrder = 0
      OnDblClick = ConditionGridDblClick
      OnDrawCell = ConditionGridDrawCell
      OnKeyDown = ConditionGridKeyDown
      OnMouseDown = ConditionGridMouseDown
      OnMouseWheelDown = ConditionGridMouseWheelDown
      OnMouseWheelUp = ConditionGridMouseWheelUp
      OnSelectCell = ConditionGridSelectCell
      RowHeights = (
        18
        18)
    end
    object EventPropertiesPanel: TPanel
      Left = 0
      Top = 0
      Width = 240
      Height = 288
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object lblEventNumber: TLabel
        Left = 4
        Top = 0
        Width = 28
        Height = 13
        Caption = 'Event'
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
        Top = 244
        Width = 26
        Height = 13
        Caption = 'Note:'
      end
      object lblEventGameStructMember: TLabel
        Left = 4
        Top = 204
        Width = 42
        Height = 13
        Caption = 'Property:'
        Visible = False
      end
      object cbxEventType: TComboBox
        Left = 64
        Top = 24
        Width = 173
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbxEventTypeChange
        OnKeyDown = EventGridKeyDown
      end
      object edEventNote: TEdit
        Left = 4
        Top = 260
        Width = 232
        Height = 21
        TabOrder = 1
      end
      object cbEventAutoBlock: TCheckBox
        Left = 64
        Top = 0
        Width = 97
        Height = 17
        Caption = 'Auto-block'
        TabOrder = 2
        OnClick = EventFlagsClick
      end
      object cbEventBlocked: TCheckBox
        Left = 152
        Top = 0
        Width = 89
        Height = 17
        Caption = 'Blocked'
        TabOrder = 3
        OnClick = EventFlagsClick
      end
      object cbxEventGameStructMember: TComboBox
        Left = 48
        Top = 200
        Width = 185
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        Visible = False
        OnChange = cbxEventGameStructMemberChange
      end
    end
    object EventDataPanel: TPanel
      Left = 240
      Top = 0
      Width = 288
      Height = 288
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object edpFilter: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 288
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 6
        object lblEventFilterIndexVar: TLabel
          Left = 64
          Top = 0
          Width = 80
          Height = 13
          Caption = 'Object index var:'
          Visible = False
        end
        object btnEventFilterIndexToggle: TButton
          Left = 0
          Top = 0
          Width = 57
          Height = 22
          Caption = 'Index'
          TabOrder = 0
          OnClick = btnEventFilterIndexToggleClick
        end
        object pnEventFilterLimitSkip: TPanel
          Left = 64
          Top = 0
          Width = 224
          Height = 22
          BevelOuter = bvNone
          TabOrder = 1
          object lblEventFilterLimit: TLabel
            Left = 112
            Top = 0
            Width = 24
            Height = 13
            Caption = 'Limit:'
          end
          object lblEventFilterSkip: TLabel
            Left = 0
            Top = 0
            Width = 24
            Height = 13
            Caption = 'Skip:'
          end
          object seEventFilterLimit: TSpinEdit
            Left = 140
            Top = 0
            Width = 62
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = seEventFilterLimitChange
          end
          object seEventFilterSkip: TSpinEdit
            Left = 26
            Top = 0
            Width = 62
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 1
            Value = 0
            OnChange = seEventFilterSkipChange
          end
          object btnEventFilterSkipVarToggle: TButton
            Left = 88
            Top = 0
            Width = 20
            Height = 22
            Caption = 'V'
            TabOrder = 2
            OnClick = btnEventFilterSkipVarToggleClick
          end
          object btnEventFilterLimitVarToggle: TButton
            Left = 202
            Top = 0
            Width = 20
            Height = 22
            Caption = 'V'
            TabOrder = 3
            OnClick = btnEventFilterLimitVarToggleClick
          end
          object edEventFilterSkipVar: TEdit
            Left = 26
            Top = 0
            Width = 62
            Height = 21
            ReadOnly = True
            TabOrder = 4
            OnClick = edEventFilterSkipVarClick
          end
          object edEventFilterLimitVar: TEdit
            Left = 140
            Top = 0
            Width = 62
            Height = 21
            ReadOnly = True
            TabOrder = 5
            OnClick = edEventFilterLimitVarClick
          end
        end
        object pnEventFilterBody: TPanel
          Left = 0
          Top = 24
          Width = 288
          Height = 264
          BevelOuter = bvNone
          TabOrder = 2
        end
        object edEventFilterIndexVar: TEdit
          Left = 152
          Top = 0
          Width = 97
          Height = 21
          ReadOnly = True
          TabOrder = 3
          Visible = False
          OnClick = edEventFilterIndexVarClick
        end
      end
      object edpTileBlock: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 288
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 4
        object imgTileBlock: TImage
          Left = 16
          Top = 16
          Width = 32
          Height = 32
          OnMouseDown = imgTileBlockMouseDown
        end
      end
      object edpByteValues: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 97
        BevelOuter = bvNone
        TabOrder = 1
        object lblEventByteValues: TLabel
          Left = 0
          Top = 0
          Width = 55
          Height = 13
          Caption = 'Byte values'
        end
        object sgEventByteValues: TStringGrid
          Left = 0
          Top = 16
          Width = 283
          Height = 79
          ColCount = 8
          DefaultColWidth = 34
          DefaultRowHeight = 18
          FixedCols = 0
          RowCount = 4
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          OnSetEditText = sgEventByteValuesSetEditText
        end
      end
      object edpMusic: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 30
        BevelOuter = bvNone
        TabOrder = 3
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
          OnChange = cbMusicNameChange
        end
      end
      object edpTilePairs: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 225
        BevelOuter = bvNone
        TabOrder = 5
        object lblTilePairs: TLabel
          Left = 0
          Top = 0
          Width = 42
          Height = 13
          Caption = 'Tile pairs'
        end
        object imgTilePairs: TImage
          Left = 72
          Top = 17
          Width = 64
          Height = 192
        end
        object sgTilePairs: TStringGrid
          Left = 0
          Top = 16
          Width = 67
          Height = 195
          ColCount = 2
          DefaultColWidth = 31
          DefaultRowHeight = 31
          FixedCols = 0
          RowCount = 6
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          OnSetEditText = sgTilePairsSetEditText
        end
      end
      object edpMessage: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 273
        BevelOuter = bvNone
        TabOrder = 2
        object lblMessage: TLabel
          Left = 4
          Top = 8
          Width = 60
          Height = 13
          Caption = 'Message ID:'
        end
        object lblMessageVarDatatype: TLabel
          Left = 24
          Top = 64
          Width = 46
          Height = 13
          Caption = 'Data type'
        end
        object lblMessageVariable: TLabel
          Left = 140
          Top = 64
          Width = 38
          Height = 13
          Caption = 'Variable'
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
          Left = 0
          Top = 36
          Width = 284
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
      object edpValueList: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 288
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnEventValueListHeader: TPanel
          Left = 0
          Top = 0
          Width = 288
          Height = 16
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          object lblEventValueSelectionList: TLabel
            Left = 0
            Top = 0
            Width = 64
            Height = 13
            Caption = 'Unit selection'
          end
          object lblEventValueList: TLabel
            Left = 152
            Top = 0
            Width = 65
            Height = 13
            Caption = 'Units in event'
          end
        end
        object EventValueSelectionList: TListBox
          Left = 0
          Top = 16
          Width = 128
          Height = 272
          Align = alLeft
          ItemHeight = 13
          TabOrder = 0
          OnDblClick = btnAddValueClick
        end
        object pnEventValueListPadding: TPanel
          Left = 280
          Top = 16
          Width = 8
          Height = 272
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
        end
        object EventValueList: TListBox
          Left = 152
          Top = 16
          Width = 128
          Height = 272
          Align = alRight
          ItemHeight = 13
          TabOrder = 3
          OnDblClick = btnDeleteValueClick
        end
        object btnMoveValueUp: TButton
          Left = 128
          Top = 192
          Width = 24
          Height = 25
          Hint = 'Move up'
          Caption = '^'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = btnMoveValueUpClick
        end
        object btnMoveValueDown: TButton
          Left = 128
          Top = 216
          Width = 24
          Height = 25
          Hint = 'Move down'
          Caption = 'v'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = btnMoveValueDownClick
        end
        object btnDeleteValue: TButton
          Left = 128
          Top = 80
          Width = 24
          Height = 25
          Hint = 'Remove selected'
          Caption = '<--'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = btnDeleteValueClick
        end
        object btnDeleteLastValue: TButton
          Left = 128
          Top = 112
          Width = 24
          Height = 25
          Hint = 'Remove last'
          Caption = 'X'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = btnDeleteLastValueClick
        end
        object btnDeleteAllValues: TButton
          Left = 128
          Top = 144
          Width = 24
          Height = 25
          Hint = 'Remove all'
          Caption = 'C'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          OnClick = btnDeleteAllValuesClick
        end
        object btnAddValue: TButton
          Left = 128
          Top = 24
          Width = 24
          Height = 49
          Hint = 'Add'
          Caption = '-->'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          OnClick = btnAddValueClick
        end
        object pnEventValueListCoords: TPanel
          Left = 0
          Top = 24
          Width = 128
          Height = 97
          BevelOuter = bvNone
          TabOrder = 10
          object lblEventValueListXCoord: TLabel
            Left = 0
            Top = 32
            Width = 37
            Height = 13
            Caption = 'X coord'
          end
          object lblEventValueListYCoord: TLabel
            Left = 64
            Top = 32
            Width = 37
            Height = 13
            Caption = 'Y coord'
          end
          object seEventValueListXCoord1: TSpinEdit
            Left = 0
            Top = 48
            Width = 57
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
          object seEventValueListYCoord1: TSpinEdit
            Left = 64
            Top = 48
            Width = 57
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
          object seEventValueListXCoord2: TSpinEdit
            Left = 0
            Top = 72
            Width = 57
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 2
            Value = 0
          end
          object seEventValueListYCoord2: TSpinEdit
            Left = 64
            Top = 72
            Width = 57
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 3
            Value = 0
          end
          object btnEventValueListCoordsSelect: TButton
            Left = 0
            Top = 0
            Width = 89
            Height = 22
            Caption = 'Select from map'
            TabOrder = 4
            OnClick = btnEventValueListCoordsSelectClick
          end
        end
      end
      object edpCondExpr: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 241
        BevelOuter = bvNone
        TabOrder = 7
        object lblCondExprAndOr: TLabel
          Left = 4
          Top = 44
          Width = 35
          Height = 13
          Caption = 'And/Or'
        end
        object lblCondExprVariable: TLabel
          Left = 48
          Top = 32
          Width = 38
          Height = 13
          Caption = 'Variable'
        end
        object lblCondExprValue: TLabel
          Left = 176
          Top = 32
          Width = 27
          Height = 13
          Caption = 'Value'
        end
        object btnCondExprPlus: TButton
          Left = 48
          Top = 4
          Width = 41
          Height = 24
          Caption = '+'
          TabOrder = 0
          OnClick = btnCondExprPlusClick
        end
        object btnCondExprMinus: TButton
          Left = 96
          Top = 4
          Width = 41
          Height = 24
          Caption = '-'
          TabOrder = 1
          OnClick = btnCondExprMinusClick
        end
      end
    end
    object EventConditionListPanel: TPanel
      Left = 528
      Top = 0
      Width = 220
      Height = 288
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      object EventConditionList: TCheckListBox
        Left = 0
        Top = 16
        Width = 196
        Height = 248
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
        object rbEventConditionsAnd: TRadioButton
          Left = 104
          Top = 0
          Width = 49
          Height = 17
          Caption = 'AND'
          TabOrder = 0
          OnClick = EventFlagsClick
        end
        object rbEventConditionsOr: TRadioButton
          Left = 152
          Top = 0
          Width = 49
          Height = 17
          Caption = 'OR'
          TabOrder = 1
          OnClick = EventFlagsClick
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
        Top = 264
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
      Width = 220
      Height = 288
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
        Top = 244
        Width = 26
        Height = 13
        Caption = 'Note:'
      end
      object lblConditionGameStructMember: TLabel
        Left = 4
        Top = 204
        Width = 42
        Height = 13
        Caption = 'Property:'
        Visible = False
      end
      object cbxConditionType: TComboBox
        Left = 80
        Top = 24
        Width = 132
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbxConditionTypeChange
        OnKeyDown = ConditionGridKeyDown
      end
      object edConditionNote: TEdit
        Left = 4
        Top = 260
        Width = 196
        Height = 21
        TabOrder = 1
      end
      object cbxConditionGameStructMember: TComboBox
        Left = 48
        Top = 200
        Width = 165
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Visible = False
        OnChange = cbxConditionGameStructMemberChange
      end
    end
    object lbConditionTypeList: TListBox
      Left = 1280
      Top = 0
      Width = 140
      Height = 288
      Align = alLeft
      ItemHeight = 13
      TabOrder = 5
      OnClick = lbConditionTypeListClick
      OnDblClick = lbConditionTypeListDblClick
    end
  end
  object CreateEventsPanel: TPanel
    Left = 528
    Top = 96
    Width = 209
    Height = 177
    TabOrder = 1
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
  object pnEventExportMarker: TPanel
    Left = 144
    Top = 3
    Width = 465
    Height = 16
    BevelOuter = bvNone
    Color = clYellow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    Visible = False
    object lblEventExportMarker: TLabel
      Left = 0
      Top = 0
      Width = 461
      Height = 16
      Caption = 
        'Select a contiguous range of events to export. Press Esc to canc' +
        'el.'
    end
  end
  object pnConditionFilter: TPanel
    Left = 840
    Top = 96
    Width = 288
    Height = 288
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object lblConditionFilterAmount: TLabel
      Left = 0
      Top = 0
      Width = 39
      Height = 13
      Caption = 'Amount:'
    end
    object seConditionFilterAmount: TSpinEdit
      Left = 120
      Top = 0
      Width = 68
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = seConditionFilterAmountChange
    end
    object rbConditionFilterAmoutGtEq: TRadioButton
      Left = 48
      Top = 0
      Width = 33
      Height = 17
      Caption = '>='
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = seConditionFilterAmountChange
    end
    object rbConditionFilterAmoutEq: TRadioButton
      Left = 88
      Top = 0
      Width = 29
      Height = 17
      Caption = '='
      TabOrder = 2
      OnClick = seConditionFilterAmountChange
    end
    object pnConditionFilterBody: TPanel
      Left = 0
      Top = 24
      Width = 288
      Height = 264
      BevelOuter = bvNone
      TabOrder = 3
    end
    object btnConditionFilterAmountVarToggle: TButton
      Left = 188
      Top = 0
      Width = 20
      Height = 22
      Caption = 'V'
      TabOrder = 4
      OnClick = btnConditionFilterAmountVarToggleClick
    end
    object edConditionFilterAmountVar: TEdit
      Left = 120
      Top = 0
      Width = 68
      Height = 21
      ReadOnly = True
      TabOrder = 5
      OnClick = edConditionFilterAmountVarClick
    end
  end
  object SelectVariablePanel: TPanel
    Left = 168
    Top = 0
    Width = 185
    Height = 329
    TabOrder = 5
    Visible = False
    object lblSelectVariableList: TLabel
      Left = 8
      Top = 4
      Width = 73
      Height = 13
      Caption = 'Select variable:'
    end
    object lbSelectVariableList: TListBox
      Left = 8
      Top = 20
      Width = 169
      Height = 229
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbSelectVariableListClick
      OnDblClick = btnSelectVariableOkClick
    end
    object pnSelectVariableBottomPanel: TPanel
      Left = 1
      Top = 256
      Width = 183
      Height = 72
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lblSelectVariableName: TLabel
        Left = 8
        Top = 0
        Width = 107
        Height = 13
        Caption = 'Custom variable name:'
      end
      object edSelectVariableName: TEdit
        Left = 8
        Top = 16
        Width = 169
        Height = 21
        TabOrder = 0
        OnChange = edSelectVariableNameChange
      end
      object btnSelectVariableOk: TBitBtn
        Left = 100
        Top = 40
        Width = 75
        Height = 25
        TabOrder = 1
        OnClick = btnSelectVariableOkClick
        Kind = bkOK
      end
      object btnSelectVariableCancel: TBitBtn
        Left = 8
        Top = 40
        Width = 75
        Height = 25
        TabOrder = 2
        OnClick = btnSelectVariableCancelClick
        Kind = bkCancel
      end
    end
  end
  object EventGridPopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
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
    object Exportevents1: TMenuItem
      Caption = 'Export events'
      ShortCut = 16453
      OnClick = Exportevents1Click
    end
    object Importevents1: TMenuItem
      Caption = 'Import events'
      ShortCut = 16457
      OnClick = Importevents1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Markcounterpart1: TMenuItem
      AutoCheck = True
      Caption = 'Mark counterpart'
      Checked = True
      RadioItem = True
      ShortCut = 116
      OnClick = MarkEventsClick
    end
    object Markselcondition1: TMenuItem
      AutoCheck = True
      Caption = 'Mark sel. condition'
      RadioItem = True
      ShortCut = 117
      OnClick = MarkEventsClick
    end
    object Markseltype1: TMenuItem
      AutoCheck = True
      Caption = 'Mark sel. type'
      RadioItem = True
      ShortCut = 118
      OnClick = MarkEventsClick
    end
  end
  object ConditionGridPopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
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
    object N4: TMenuItem
      Caption = '-'
    end
    object Marknothing2: TMenuItem
      AutoCheck = True
      Caption = 'Mark nothing'
      Checked = True
      RadioItem = True
      OnClick = MarkConditionsClick
    end
    object Markseltype2: TMenuItem
      AutoCheck = True
      Caption = 'Mark sel. type'
      RadioItem = True
      OnClick = MarkConditionsClick
    end
  end
  object ExportEventsDialog: TSaveDialog
    DefaultExt = 'd2kevt'
    Filter = 'Dune2000 event export data (*.d2kevt)|*.d2kevt'
    Left = 456
  end
  object ImportEventsDialog: TOpenDialog
    DefaultExt = 'd2kevt'
    Filter = 'Dune2000 event export data (*.d2kevt)|*.d2kevt'
    Title = 'Import events'
    Left = 488
  end
end
