object SettingsDialog: TSettingsDialog
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 525
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbUISettings: TGroupBox
    Left = 8
    Top = 4
    Width = 457
    Height = 141
    Caption = 'User Interface Settings'
    TabOrder = 0
    object cbPreserveGUISettings: TCheckBox
      Left = 8
      Top = 20
      Width = 441
      Height = 17
      Caption = 
        'Preserve GUI (position and size of windows) after program is clo' +
        'sed and opened again'
      TabOrder = 0
    end
    object cbHidePresetWindow: TCheckBox
      Left = 8
      Top = 44
      Width = 441
      Height = 17
      Caption = 'Hide block preset selection window when block preset is selected'
      TabOrder = 1
    end
    object cbAlwaysAskOnQuit: TCheckBox
      Left = 8
      Top = 68
      Width = 441
      Height = 17
      Caption = 'Ask for before program is closed when changes were made into map'
      TabOrder = 2
    end
    object cbTranslateStructureNames: TCheckBox
      Left = 8
      Top = 92
      Width = 441
      Height = 17
      Caption = 'Translate building and unit names on Structures tab'
      TabOrder = 3
    end
    object cbEventGridShowEmptyLines: TCheckBox
      Left = 8
      Top = 116
      Width = 441
      Height = 17
      Caption = 'Show empty lines after the last event in the events table'
      TabOrder = 4
    end
  end
  object gbEditingSettings: TGroupBox
    Left = 8
    Top = 152
    Width = 457
    Height = 141
    Caption = 'Map Editing Settings'
    TabOrder = 1
    object cbRestrictPainting: TCheckBox
      Left = 8
      Top = 20
      Width = 441
      Height = 17
      Caption = 
        'Restrict painting of spice, sand, rock, dunes etc. on terrain wh' +
        'ere it can be painted'
      TabOrder = 0
    end
    object cbUseRandomPaintMap: TCheckBox
      Left = 8
      Top = 44
      Width = 441
      Height = 17
      Caption = 
        'Use predefined randomization map for painting sand, rock, dunes.' +
        '.. for better natural look'
      TabOrder = 1
    end
    object cbLoadCustomColoursBin: TCheckBox
      Left = 8
      Top = 68
      Width = 441
      Height = 17
      Caption = 'Use custom COLOURS.BIN for structures rendering'
      TabOrder = 2
    end
    object cbCheckMapErrorsOnSave: TCheckBox
      Left = 8
      Top = 92
      Width = 441
      Height = 17
      Caption = 'Check map errors on save'
      TabOrder = 3
    end
    object cbCheckMapErrorsOnTest: TCheckBox
      Left = 8
      Top = 116
      Width = 441
      Height = 17
      Caption = 'Check map errors on test'
      TabOrder = 4
    end
  end
  object gbDefaultValues: TGroupBox
    Left = 8
    Top = 300
    Width = 457
    Height = 81
    Caption = 'Default Values'
    TabOrder = 2
    object lblDefaultMapWidth: TLabel
      Left = 8
      Top = 20
      Width = 52
      Height = 13
      Caption = 'Map Width'
    end
    object lblDefaultMapHeight: TLabel
      Left = 128
      Top = 20
      Width = 55
      Height = 13
      Caption = 'Map Height'
    end
    object lblDefaultMisTechLevel: TLabel
      Left = 248
      Top = 20
      Width = 25
      Height = 13
      Caption = 'Tech'
    end
    object lblDefaultMisStartingMoney: TLabel
      Left = 328
      Top = 20
      Width = 32
      Height = 13
      Caption = 'Credits'
    end
    object lblDefaultTilesetName: TLabel
      Left = 8
      Top = 48
      Width = 31
      Height = 13
      Caption = 'Tileset'
    end
    object seDefaultMapWidth: TSpinEdit
      Left = 72
      Top = 20
      Width = 49
      Height = 22
      Increment = 2
      MaxValue = 128
      MinValue = 2
      TabOrder = 0
      Value = 32
    end
    object seDefaultMapHeight: TSpinEdit
      Left = 192
      Top = 20
      Width = 49
      Height = 22
      Increment = 2
      MaxValue = 128
      MinValue = 2
      TabOrder = 1
      Value = 32
    end
    object seDefaultMisTechLevel: TSpinEdit
      Left = 280
      Top = 20
      Width = 41
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 2
      Value = 1
    end
    object edDefaultMisStartingMoney: TEdit
      Left = 368
      Top = 20
      Width = 81
      Height = 21
      MaxLength = 10
      TabOrder = 3
    end
    object edDefaultTilesetName: TEdit
      Left = 48
      Top = 48
      Width = 105
      Height = 21
      MaxLength = 200
      TabOrder = 4
    end
  end
  object gbPaths: TGroupBox
    Left = 8
    Top = 388
    Width = 457
    Height = 97
    Caption = 'File Paths'
    TabOrder = 3
    object lblGamePath: TLabel
      Left = 8
      Top = 20
      Width = 53
      Height = 13
      Caption = 'Game Path'
    end
    object lblGameExecutable: TLabel
      Left = 8
      Top = 44
      Width = 49
      Height = 13
      Caption = 'Game Exe'
    end
    object lblMissionsPath: TLabel
      Left = 8
      Top = 68
      Width = 65
      Height = 13
      Caption = 'Missions Path'
    end
    object edGamePath: TEdit
      Left = 80
      Top = 20
      Width = 313
      Height = 21
      TabOrder = 0
    end
    object edGameExecutable: TEdit
      Left = 80
      Top = 44
      Width = 313
      Height = 21
      TabOrder = 1
    end
    object edMissionsPath: TEdit
      Left = 80
      Top = 68
      Width = 313
      Height = 21
      TabOrder = 2
    end
    object btnGamePath: TButton
      Left = 400
      Top = 20
      Width = 51
      Height = 21
      Caption = 'Browse'
      TabOrder = 3
      OnClick = btnGamePathClick
    end
    object btnGameExecutable: TButton
      Left = 400
      Top = 44
      Width = 51
      Height = 21
      Caption = 'Browse'
      TabOrder = 4
      OnClick = btnGameExecutableClick
    end
    object btnMissionsPath: TButton
      Left = 400
      Top = 68
      Width = 51
      Height = 21
      Caption = 'Browse'
      TabOrder = 5
      OnClick = btnMissionsPathClick
    end
  end
  object btnSave: TButton
    Left = 8
    Top = 492
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnCancel: TButton
    Left = 96
    Top = 492
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'Exe file (*.exe)|*.exe'
    Title = 'Select Game Executable'
    Top = 4
  end
end
