object SetDialog: TSetDialog
  Left = 365
  Top = 188
  BorderStyle = bsDialog
  Caption = 'SetDialog'
  ClientHeight = 138
  ClientWidth = 176
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
  object SetMapSize_Menu: TPanel
    Left = 0
    Top = 0
    Width = 176
    Height = 101
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SetMapSize_LbWidth: TLabel
      Left = 16
      Top = 16
      Width = 49
      Height = 13
      Caption = 'Map width'
    end
    object SetMapSize_LbHeight: TLabel
      Left = 16
      Top = 48
      Width = 53
      Height = 13
      Caption = 'Map height'
    end
    object SetMapSize_Width: TSpinEdit
      Left = 80
      Top = 16
      Width = 81
      Height = 22
      Increment = 2
      MaxValue = 128
      MinValue = 0
      TabOrder = 0
      Value = 32
    end
    object SetMapSize_Height: TSpinEdit
      Left = 80
      Top = 48
      Width = 81
      Height = 22
      Increment = 2
      MaxValue = 128
      MinValue = 0
      TabOrder = 1
      Value = 32
    end
  end
  object ShiftMap_Menu: TPanel
    Left = 0
    Top = 0
    Width = 176
    Height = 101
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object ShiftMap_LbNumTiles: TLabel
      Left = 16
      Top = 64
      Width = 55
      Height = 13
      Caption = 'Tiles count:'
    end
    object ShiftMap_RbUp: TRadioButton
      Tag = 2
      Left = 32
      Top = 8
      Width = 57
      Height = 17
      Caption = 'Up'
      TabOrder = 0
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_RbDown: TRadioButton
      Tag = 4
      Left = 32
      Top = 32
      Width = 57
      Height = 17
      Caption = 'Down'
      TabOrder = 1
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_RbLeft: TRadioButton
      Tag = 1
      Left = 96
      Top = 8
      Width = 57
      Height = 17
      Caption = 'Left'
      TabOrder = 2
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_RbRight: TRadioButton
      Tag = 3
      Left = 96
      Top = 32
      Width = 57
      Height = 17
      Caption = 'Right'
      TabOrder = 3
      OnClick = ShiftMap_SelectDirection
    end
    object ShiftMap_NumTiles: TSpinEdit
      Left = 80
      Top = 64
      Width = 73
      Height = 22
      MaxValue = 128
      MinValue = 0
      TabOrder = 4
      Value = 1
    end
  end
  object ChStrOwn_Menu: TPanel
    Left = 0
    Top = 0
    Width = 176
    Height = 101
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object ChStrOwn_LbPlayerFrom: TLabel
      Left = 8
      Top = 16
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object ChStrOwn_LbPlayerTo: TLabel
      Left = 8
      Top = 48
      Width = 13
      Height = 13
      Caption = 'To'
    end
    object ChStrOwn_PlayerFrom: TComboBox
      Left = 48
      Top = 16
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object ChStrOwn_PlayerTo: TComboBox
      Left = 48
      Top = 48
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object ChStrOwn_Swap: TCheckBox
      Left = 48
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Swap'
      TabOrder = 2
    end
  end
  object Tileset_Menu: TPanel
    Left = 0
    Top = 0
    Width = 176
    Height = 101
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Tileset_List: TListBox
      Left = 0
      Top = 0
      Width = 176
      Height = 101
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = BtnOKClick
    end
  end
  object pnButtons: TPanel
    Left = 0
    Top = 101
    Width = 176
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object BtnCancel: TButton
      Left = 94
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = BtnCancelClick
    end
    object BtnOK: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = BtnOKClick
    end
  end
end
