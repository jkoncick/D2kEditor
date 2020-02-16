object TestMapDialog: TTestMapDialog
  Left = 409
  Top = 199
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Test Map Settings'
  ClientHeight = 257
  ClientWidth = 297
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
  PixelsPerInch = 96
  TextHeight = 13
  object lblMySideID: TLabel
    Left = 16
    Top = 16
    Width = 34
    Height = 13
    Caption = 'Play as'
  end
  object lblMissionNumber: TLabel
    Left = 16
    Top = 48
    Width = 35
    Height = 13
    Caption = 'Mission'
  end
  object lblDifficultyLevel: TLabel
    Left = 16
    Top = 80
    Width = 40
    Height = 13
    Caption = 'Difficulty'
  end
  object lblSeed: TLabel
    Left = 16
    Top = 112
    Width = 25
    Height = 13
    Caption = 'Seed'
  end
  object lblTextUib: TLabel
    Left = 16
    Top = 144
    Width = 38
    Height = 13
    Caption = 'Text.uib'
  end
  object lblParameters: TLabel
    Left = 16
    Top = 176
    Width = 53
    Height = 13
    Caption = 'Parameters'
  end
  object eMySideID: TComboBox
    Left = 96
    Top = 16
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object eMissionNumber: TSpinEdit
    Left = 96
    Top = 48
    Width = 121
    Height = 22
    MaxLength = 1
    MaxValue = 9
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object eDifficultyLevel: TComboBox
    Left = 96
    Top = 80
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
    Left = 96
    Top = 112
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object eTextUib: TEdit
    Left = 96
    Top = 144
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'TEXT.UIB'
  end
  object eParameters: TEdit
    Left = 96
    Top = 176
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 16
    Top = 216
    Width = 89
    Height = 25
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = btnCancelClick
  end
  object btnLaunch: TButton
    Left = 128
    Top = 216
    Width = 89
    Height = 25
    Caption = 'Launch'
    TabOrder = 7
    OnClick = btnLaunchClick
  end
  object btnRandomSeed: TButton
    Left = 224
    Top = 112
    Width = 57
    Height = 25
    Caption = 'Random'
    TabOrder = 8
    OnClick = btnRandomSeedClick
  end
  object btnDefaultTextUib: TButton
    Left = 224
    Top = 144
    Width = 57
    Height = 25
    Caption = 'Clear'
    TabOrder = 9
    OnClick = btnDefaultTextUibClick
  end
end
