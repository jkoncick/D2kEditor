object DebugWindow: TDebugWindow
  Left = 192
  Top = 77
  Width = 648
  Height = 680
  Caption = 'Debug window'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vleDebugValueList: TValueListEditor
    Left = 0
    Top = 49
    Width = 640
    Height = 597
    Align = alClient
    DefaultColWidth = 200
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goThumbTracking]
    TabOrder = 0
    ColWidths = (
      200
      434)
  end
  object pnDebugControls: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object gbRenderingPerformance: TGroupBox
      Left = 4
      Top = 4
      Width = 285
      Height = 41
      Caption = 'Rendering performance'
      TabOrder = 0
      object cbShowRenderTime: TCheckBox
        Left = 8
        Top = 16
        Width = 105
        Height = 17
        Caption = 'Show render time'
        TabOrder = 0
        OnClick = DebugSettingChange
      end
      object cbShowDifferentialRendering: TCheckBox
        Left = 128
        Top = 16
        Width = 145
        Height = 17
        Caption = 'Show differential rendering'
        TabOrder = 1
        OnClick = DebugSettingChange
      end
    end
    object gbMissionLauncher: TGroupBox
      Left = 296
      Top = 4
      Width = 225
      Height = 41
      Caption = 'Mission launcher'
      TabOrder = 1
      object cbShowReplaceFilesFromModsFolderLog: TCheckBox
        Left = 8
        Top = 16
        Width = 209
        Height = 17
        Caption = 'Show Replace files from mods folder log'
        TabOrder = 0
        OnClick = DebugSettingChange
      end
    end
  end
end
