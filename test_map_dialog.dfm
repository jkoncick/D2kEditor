object TestMapDialog: TTestMapDialog
  Left = 409
  Top = 199
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Test Map Settings'
  ClientHeight = 193
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
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
  object eMySideID: TComboBox
    Left = 80
    Top = 16
    Width = 137
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object eMissionNumber: TSpinEdit
    Left = 80
    Top = 48
    Width = 137
    Height = 22
    MaxLength = 1
    MaxValue = 9
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object eDifficultyLevel: TComboBox
    Left = 80
    Top = 80
    Width = 137
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
    Left = 80
    Top = 112
    Width = 80
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object btnCancel: TButton
    Left = 16
    Top = 152
    Width = 89
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object btnLaunch: TButton
    Left = 128
    Top = 152
    Width = 89
    Height = 25
    Caption = 'Launch'
    TabOrder = 5
    OnClick = btnLaunchClick
  end
  object btnRandomSeed: TButton
    Left = 160
    Top = 112
    Width = 57
    Height = 21
    Caption = 'Random'
    TabOrder = 6
    OnClick = btnRandomSeedClick
  end
end
