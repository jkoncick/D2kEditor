object TestMapDialog: TTestMapDialog
  Left = 409
  Top = 199
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Test Map Settings'
  ClientHeight = 273
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lblMySideID: TLabel
    Left = 24
    Top = 24
    Width = 34
    Height = 13
    Caption = 'Play as'
  end
  object lblMissionNumber: TLabel
    Left = 24
    Top = 56
    Width = 35
    Height = 13
    Caption = 'Mission'
  end
  object lblDifficultyLevel: TLabel
    Left = 24
    Top = 88
    Width = 40
    Height = 13
    Caption = 'Difficulty'
  end
  object lblSeed: TLabel
    Left = 24
    Top = 120
    Width = 25
    Height = 13
    Caption = 'Seed'
  end
  object lblTextUib: TLabel
    Left = 24
    Top = 152
    Width = 38
    Height = 13
    Caption = 'Text.uib'
  end
  object lblParameters: TLabel
    Left = 24
    Top = 184
    Width = 53
    Height = 13
    Caption = 'Parameters'
  end
  object eMySideID: TComboBox
    Left = 104
    Top = 24
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object eMissionNumber: TSpinEdit
    Left = 104
    Top = 56
    Width = 121
    Height = 22
    MaxLength = 1
    MaxValue = 9
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object eDifficultyLevel: TComboBox
    Left = 104
    Top = 88
    Width = 121
    Height = 21
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 2
    Text = 'Normal'
    Items.Strings = (
      'Easy'
      'Normal'
      'Hard')
  end
  object eSeed: TEdit
    Left = 104
    Top = 120
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object eTextUib: TEdit
    Left = 104
    Top = 152
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'TEXT.UIB'
  end
  object eParameters: TEdit
    Left = 104
    Top = 184
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 24
    Top = 224
    Width = 89
    Height = 25
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = btnCancelClick
  end
  object btnLaunch: TButton
    Left = 136
    Top = 224
    Width = 89
    Height = 25
    Caption = 'Launch'
    TabOrder = 7
    OnClick = btnLaunchClick
  end
  object btnRandomSeed: TButton
    Left = 232
    Top = 120
    Width = 57
    Height = 25
    Caption = 'Random'
    TabOrder = 8
    OnClick = btnRandomSeedClick
  end
  object btnDefaultTextUib: TButton
    Left = 232
    Top = 152
    Width = 57
    Height = 25
    Caption = 'Default'
    TabOrder = 9
    OnClick = btnDefaultTextUibClick
  end
end
