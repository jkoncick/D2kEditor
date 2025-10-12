object TestMapDialog: TTestMapDialog
  Left = 409
  Top = 199
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Test Map Settings'
  ClientHeight = 263
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
    Top = 8
    Width = 37
    Height = 13
    Caption = 'Play as:'
  end
  object lblDifficultyLevel: TLabel
    Left = 16
    Top = 32
    Width = 43
    Height = 13
    Caption = 'Difficulty:'
  end
  object lblDebugFeatures: TLabel
    Left = 16
    Top = 56
    Width = 115
    Height = 13
    Caption = 'Enable Debug Features:'
  end
  object eMySideID: TComboBox
    Left = 80
    Top = 8
    Width = 137
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object eDifficultyLevel: TComboBox
    Left = 80
    Top = 32
    Width = 137
    Height = 21
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 1
    Text = 'Normal'
    Items.Strings = (
      'Easy'
      'Normal'
      'Hard')
  end
  object btnCancel: TButton
    Left = 16
    Top = 230
    Width = 89
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnLaunch: TButton
    Left = 128
    Top = 230
    Width = 89
    Height = 25
    Caption = 'Launch'
    TabOrder = 3
    OnClick = btnLaunchClick
  end
  object eDebugFeatures: TCheckListBox
    Left = 16
    Top = 72
    Width = 201
    Height = 150
    ItemHeight = 13
    Items.Strings = (
      'Reveal map'
      'Always show radar'
      'Instand build'
      'Unlimited power'
      'Quick switch side (numpad keys)'
      'Debug text on screen'
      'Debug tiles'
      'Debug unit path'
      'Show selected unit data'
      'Show selected building data'
      'Enable event profiling')
    TabOrder = 4
  end
end
