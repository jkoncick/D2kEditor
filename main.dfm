object MainWindow: TMainWindow
  Left = 190
  Top = 111
  Width = 960
  Height = 746
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Dune 2000 Map and Mission Editor'
  Color = clBtnFace
  Constraints.MinHeight = 544
  Constraints.MinWidth = 528
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = AppMenu
  OldCreateOrder = False
  Scaled = False
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MapCanvas: TImage
    Left = 4
    Top = 4
    Width = 1
    Height = 1
    OnDblClick = MapCanvasDblClick
    OnMouseDown = MapCanvasMouseDown
    OnMouseMove = MapCanvasMouseMove
    OnMouseUp = MapCanvasMouseUp
  end
  object CursorImage: TImage
    Left = 4
    Top = 4
    Width = 1
    Height = 1
    Visible = False
    OnMouseDown = CursorImageMouseDown
    OnMouseMove = CursorImageMouseMove
    OnMouseUp = MapCanvasMouseUp
  end
  object MapScrollH: TScrollBar
    Left = 4
    Top = 456
    Width = 480
    Height = 16
    LargeChange = 4
    PageSize = 0
    TabOrder = 0
    OnChange = MapScrollChange
  end
  object MapScrollV: TScrollBar
    Left = 488
    Top = 4
    Width = 16
    Height = 448
    Kind = sbVertical
    LargeChange = 4
    PageSize = 0
    TabOrder = 1
    OnChange = MapScrollChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 681
    Width = 952
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 70
      end
      item
        Width = 64
      end
      item
        Text = 'No map loaded'
        Width = 210
      end
      item
        Width = 32
      end
      item
        Text = 'W: 0  S: 0  B: 0'
        Width = 136
      end
      item
        Text = 'Power: 0%   (0/0)'
        Width = 150
      end
      item
        Text = 'v2.3'
        Width = 0
      end>
  end
  object EditorMenu: TPanel
    Left = 512
    Top = 0
    Width = 168
    Height = 575
    TabOrder = 3
    object MiniMapFrame: TBevel
      Left = 18
      Top = 4
      Width = 132
      Height = 132
      Shape = bsFrame
      Style = bsRaised
    end
    object MiniMap: TImage
      Left = 20
      Top = 6
      Width = 128
      Height = 128
      OnMouseDown = MiniMapMouseDown
      OnMouseMove = MiniMapMouseMove
    end
    object sbShowGrid: TSpeedButton
      Left = 8
      Top = 140
      Width = 38
      Height = 22
      Hint = 'Show Grid (Ctrl+G)'
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Grid'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object sbMarkImpassableTiles: TSpeedButton
      Tag = 1
      Left = 46
      Top = 140
      Width = 38
      Height = 22
      Hint = 'Mark impassable tiles (Ctrl+M)'
      AllowAllUp = True
      GroupIndex = 2
      Caption = 'Impas'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object sbMarkBuildableTiles: TSpeedButton
      Tag = 2
      Left = 84
      Top = 140
      Width = 38
      Height = 22
      Hint = 'Mark buildable tiles (Ctrl+B)'
      AllowAllUp = True
      GroupIndex = 3
      Caption = 'Build'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object sbMarkOwnerSide: TSpeedButton
      Tag = 3
      Left = 122
      Top = 140
      Width = 38
      Height = 22
      Hint = 'Mark Wall & Concrete owner side (Ctrl+W)'
      AllowAllUp = True
      GroupIndex = 4
      Caption = 'Own'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object sbShowEventMarkers: TSpeedButton
      Tag = 4
      Left = 8
      Top = 162
      Width = 38
      Height = 22
      Hint = 'Show event markers (Ctrl+E)'
      AllowAllUp = True
      GroupIndex = 5
      Caption = 'Event'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object sbShowEventAreas: TSpeedButton
      Tag = 5
      Left = 46
      Top = 162
      Width = 38
      Height = 22
      Hint = 'Show event areas (Ctrl+H)'
      AllowAllUp = True
      GroupIndex = 6
      Caption = 'Area'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object sbMarkDefenceAreas: TSpeedButton
      Tag = 6
      Left = 84
      Top = 162
      Width = 38
      Height = 22
      Hint = 'Show defence areas (Ctrl+D)'
      AllowAllUp = True
      GroupIndex = 7
      Caption = 'Def'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object sbShowCrateMarkers: TSpeedButton
      Tag = 7
      Left = 122
      Top = 162
      Width = 38
      Height = 22
      Hint = 'Show crate markers'
      AllowAllUp = True
      GroupIndex = 8
      Caption = 'Crate'
      ParentShowHint = False
      ShowHint = True
      OnClick = SettingChange
    end
    object EditorPages: TPageControl
      Left = 1
      Top = 188
      Width = 166
      Height = 386
      ActivePage = PageStructures
      Align = alBottom
      TabOrder = 0
      OnChange = EditorPagesChange
      object PageStructures: TTabSheet
        Caption = 'Structures       '
        object LbStructureValue: TLabel
          Left = 8
          Top = 4
          Width = 68
          Height = 13
          Caption = 'Special Value:'
        end
        object SpecialValue: TEdit
          Tag = -1
          Left = 80
          Top = 2
          Width = 49
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = SpecialValueChange
        end
        object btnFindSelectedObject: TButton
          Left = 133
          Top = 0
          Width = 20
          Height = 25
          Hint = 'Find selected object (Ctrl+F)'
          Caption = 'F'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnFindSelectedObjectClick
        end
        object StructPages: TPageControl
          Left = 0
          Top = 24
          Width = 158
          Height = 334
          ActivePage = PageStructBasic
          Align = alBottom
          TabOrder = 2
          OnChange = EditorPagesChange
          object PageStructBasic: TTabSheet
            Caption = 'Basic             '
            object lblBuildingGroup: TLabel
              Left = 0
              Top = 144
              Width = 42
              Height = 13
              Caption = 'Buildings'
            end
            object lblMiscObject: TLabel
              Left = 0
              Top = 16
              Width = 59
              Height = 13
              Caption = 'Misc objects'
            end
            object lblStructSide: TLabel
              Left = 0
              Top = 104
              Width = 21
              Height = 13
              Caption = 'Side'
            end
            object lblStructureName: TLabel
              Left = 0
              Top = 0
              Width = 148
              Height = 15
              AutoSize = False
              Color = 4259839
              ParentColor = False
              Transparent = False
              Layout = tlCenter
            end
            object lblUnitGroup: TLabel
              Left = 0
              Top = 240
              Width = 24
              Height = 13
              Caption = 'Units'
            end
            object cbxStructSide: TComboBox
              Left = 0
              Top = 120
              Width = 148
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 0
              OnChange = SideSelectChange
            end
            object lbBuildingGroup: TListBox
              Tag = -1
              Left = 0
              Top = 160
              Width = 148
              Height = 77
              ItemHeight = 13
              TabOrder = 1
              OnClick = lbBuildingGroupClick
            end
            object lbMiscObject: TListBox
              Tag = -1
              Left = 0
              Top = 32
              Width = 148
              Height = 70
              ItemHeight = 13
              TabOrder = 2
              OnClick = lbMiscObjectClick
            end
            object lbUnitGroup: TListBox
              Tag = -1
              Left = 0
              Top = 256
              Width = 148
              Height = 73
              ItemHeight = 13
              TabOrder = 3
              OnClick = lbUnitGroupClick
            end
          end
          object PageStructAdvanced: TTabSheet
            Caption = 'Advanced     '
            ImageIndex = 1
            object StructAdvancedPages: TPageControl
              Left = 0
              Top = 0
              Width = 150
              Height = 306
              ActivePage = PageStructBuildings
              Align = alClient
              TabOrder = 0
              OnChange = EditorPagesChange
              object PageStructBuildings: TTabSheet
                Caption = 'Buildings'
                object lblBuildingDirection: TLabel
                  Left = 0
                  Top = 2
                  Width = 45
                  Height = 13
                  Caption = 'Direction:'
                  Visible = False
                end
                object cbBuildingNoNewHarv: TCheckBox
                  Left = 0
                  Top = 0
                  Width = 113
                  Height = 17
                  Caption = 'No new harvester'
                  TabOrder = 2
                  OnClick = StructControlClick
                end
                object cbxBuildingSide: TComboBox
                  Tag = -1
                  Left = 0
                  Top = 40
                  Width = 140
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 0
                  OnChange = SideSelectChange
                end
                object lbBuildingType: TListBox
                  Tag = -1
                  Left = 0
                  Top = 64
                  Width = 140
                  Height = 233
                  ItemHeight = 13
                  TabOrder = 1
                  OnClick = lbBuildingTypeClick
                end
                object cbBuildingPrimary: TCheckBox
                  Left = 0
                  Top = 22
                  Width = 65
                  Height = 17
                  Caption = 'Primary'
                  TabOrder = 3
                  OnClick = StructControlClick
                end
                object cbBuildingTagged: TCheckBox
                  Left = 72
                  Top = 22
                  Width = 65
                  Height = 17
                  Caption = 'Tagged'
                  TabOrder = 4
                  OnClick = StructControlClick
                end
                object cbxBuildingDirection: TComboBox
                  Tag = -1
                  Left = 50
                  Top = 0
                  Width = 90
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  ItemIndex = 0
                  TabOrder = 5
                  Text = 'Up'
                  Visible = False
                  OnClick = StructControlClick
                  Items.Strings = (
                    'Up'
                    'Right'
                    'Down'
                    'Left')
                end
              end
              object PageStructUnits: TTabSheet
                Caption = 'Units   '
                ImageIndex = 1
                object lblUnitDirection: TLabel
                  Left = 0
                  Top = 2
                  Width = 45
                  Height = 13
                  Caption = 'Direction:'
                end
                object cbxUnitSide: TComboBox
                  Tag = -1
                  Left = 0
                  Top = 40
                  Width = 140
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 0
                  OnChange = SideSelectChange
                end
                object lbUnitType: TListBox
                  Tag = -1
                  Left = 0
                  Top = 64
                  Width = 140
                  Height = 233
                  ItemHeight = 13
                  TabOrder = 1
                  OnClick = StructControlClick
                end
                object cbxUnitDirection: TComboBox
                  Tag = -1
                  Left = 50
                  Top = 0
                  Width = 90
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  ItemIndex = 0
                  TabOrder = 2
                  Text = 'Up'
                  OnClick = StructControlClick
                  Items.Strings = (
                    'Up'
                    'Up-Right'
                    'Right'
                    'Down-Right'
                    'Down'
                    'Down-Left'
                    'Left'
                    'Up-Left')
                end
                object cbUnitStealth: TCheckBox
                  Left = 0
                  Top = 22
                  Width = 65
                  Height = 17
                  Caption = 'Stealth'
                  TabOrder = 3
                  OnClick = StructControlClick
                end
                object cbUnitTagged: TCheckBox
                  Left = 72
                  Top = 22
                  Width = 65
                  Height = 17
                  Caption = 'Tagged'
                  TabOrder = 4
                  OnClick = StructControlClick
                end
              end
              object PageStructCrates: TTabSheet
                Caption = 'Crates '
                ImageIndex = 2
                object lblCrateImage: TLabel
                  Left = 0
                  Top = 40
                  Width = 57
                  Height = 13
                  Caption = 'Crate Image'
                end
                object lblCrateType: TLabel
                  Left = 0
                  Top = 0
                  Width = 52
                  Height = 13
                  Caption = 'Crate Type'
                end
                object lblCrateExtData: TLabel
                  Left = 0
                  Top = 84
                  Width = 140
                  Height = 15
                  AutoSize = False
                  Caption = 'Extension data = 0'
                  Color = 4259839
                  ParentColor = False
                  Transparent = False
                  Layout = tlCenter
                end
                object lblCrateBloomSpawnerType: TLabel
                  Left = 0
                  Top = 58
                  Width = 27
                  Height = 13
                  Caption = 'Type:'
                  Visible = False
                end
                object cbxCrateType: TComboBox
                  Tag = -1
                  Left = 0
                  Top = 16
                  Width = 140
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 0
                  OnChange = CrateControlClick
                end
                object cbxCrateImage: TComboBox
                  Tag = -1
                  Left = 0
                  Top = 56
                  Width = 140
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 1
                  OnChange = StructControlClick
                end
                object pnCrateCash: TPanel
                  Left = 0
                  Top = 104
                  Width = 140
                  Height = 25
                  BevelOuter = bvNone
                  TabOrder = 2
                  object lblCrateCash: TLabel
                    Left = 0
                    Top = 2
                    Width = 64
                    Height = 13
                    Caption = 'Credits x 100:'
                  end
                  object seCrateCash: TSpinEdit
                    Tag = -1
                    Left = 72
                    Top = 0
                    Width = 67
                    Height = 22
                    MaxValue = 255
                    MinValue = 0
                    TabOrder = 0
                    Value = 0
                    OnChange = StructControlClick
                  end
                end
                object pnCrateExplode: TPanel
                  Left = 0
                  Top = 104
                  Width = 140
                  Height = 81
                  BevelOuter = bvNone
                  TabOrder = 3
                  object lblCrateExplodeWeaponType: TLabel
                    Left = 0
                    Top = 0
                    Width = 67
                    Height = 13
                    Caption = 'Weapon type:'
                  end
                  object cbxCrateExplodeWeaponType: TComboBox
                    Tag = -1
                    Left = 0
                    Top = 16
                    Width = 140
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    TabOrder = 0
                    OnChange = StructControlClick
                  end
                  object cbCrateExplodeShootable: TCheckBox
                    Left = 0
                    Top = 40
                    Width = 97
                    Height = 17
                    Caption = 'Shootable'
                    TabOrder = 1
                    OnClick = StructControlClick
                  end
                  object cbCrateExplodeDamageUnit: TCheckBox
                    Left = 0
                    Top = 60
                    Width = 129
                    Height = 17
                    Caption = 'Damage closer to unit'
                    TabOrder = 2
                    OnClick = StructControlClick
                  end
                end
                object pnCrateReveal: TPanel
                  Left = 0
                  Top = 104
                  Width = 140
                  Height = 73
                  BevelOuter = bvNone
                  TabOrder = 4
                  object lblCrateRevealRadius: TLabel
                    Left = 0
                    Top = 2
                    Width = 36
                    Height = 13
                    Caption = 'Radius:'
                  end
                  object lblCrateRevealXOffset: TLabel
                    Left = 0
                    Top = 26
                    Width = 39
                    Height = 13
                    Caption = 'X offset:'
                  end
                  object lblCrateRevealYOffset: TLabel
                    Left = 0
                    Top = 50
                    Width = 39
                    Height = 13
                    Caption = 'Y offset:'
                  end
                  object cbxCrateRevealRadius: TComboBox
                    Tag = -1
                    Left = 48
                    Top = 0
                    Width = 73
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    ItemIndex = 0
                    TabOrder = 0
                    Text = '4'
                    OnChange = StructControlClick
                    Items.Strings = (
                      '4'
                      '5'
                      '6'
                      '7')
                  end
                  object cbxCrateRevealXOffset: TComboBox
                    Tag = -1
                    Left = 48
                    Top = 24
                    Width = 73
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    ItemIndex = 0
                    TabOrder = 1
                    Text = '0'
                    OnChange = StructControlClick
                    Items.Strings = (
                      '0'
                      '4'
                      '8'
                      '12'
                      '-16'
                      '-12'
                      '-8'
                      '-4')
                  end
                  object cbxCrateRevealYOffset: TComboBox
                    Tag = -1
                    Left = 48
                    Top = 48
                    Width = 73
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    ItemIndex = 0
                    TabOrder = 2
                    Text = '0'
                    OnChange = StructControlClick
                    Items.Strings = (
                      '0'
                      '4'
                      '8'
                      '12'
                      '-16'
                      '-12'
                      '-8'
                      '-4')
                  end
                end
                object pnCrateUnit: TPanel
                  Left = 0
                  Top = 104
                  Width = 140
                  Height = 81
                  BevelOuter = bvNone
                  TabOrder = 5
                  object lblCrateUnitUnitType: TLabel
                    Left = 0
                    Top = 0
                    Width = 45
                    Height = 13
                    Caption = 'Unit type:'
                  end
                  object cbxCrateUnitUnitType: TComboBox
                    Tag = -1
                    Left = 0
                    Top = 16
                    Width = 140
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    TabOrder = 0
                    OnChange = StructControlClick
                  end
                  object cbCrateUnitFiveInfantry: TCheckBox
                    Left = 0
                    Top = 40
                    Width = 105
                    Height = 17
                    Caption = 'Spawn 5 infantry'
                    TabOrder = 1
                    OnClick = StructControlClick
                  end
                  object cbCrateUnitUnitGroup: TCheckBox
                    Left = 0
                    Top = 60
                    Width = 129
                    Height = 17
                    Caption = 'Side version of unit'
                    TabOrder = 2
                    OnClick = CrateControlClick
                  end
                end
                object pnCratePowerup: TPanel
                  Left = 0
                  Top = 104
                  Width = 140
                  Height = 89
                  BevelOuter = bvNone
                  TabOrder = 6
                  object lblCratePowerupRadius: TLabel
                    Left = 0
                    Top = 2
                    Width = 36
                    Height = 13
                    Caption = 'Radius:'
                  end
                  object lblCratePowerupType: TLabel
                    Left = 0
                    Top = 26
                    Width = 27
                    Height = 13
                    Caption = 'Type:'
                  end
                  object cbxCratePowerupRadius: TComboBox
                    Tag = -1
                    Left = 48
                    Top = 0
                    Width = 91
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    ItemIndex = 0
                    TabOrder = 0
                    Text = 'One unit'
                    OnClick = StructControlClick
                    Items.Strings = (
                      'One unit'
                      '1 tile'
                      '2 tiles'
                      '3 tiles')
                  end
                  object cbxCratePowerupType: TComboBox
                    Tag = -1
                    Left = 48
                    Top = 24
                    Width = 91
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    ItemIndex = 0
                    TabOrder = 1
                    Text = 'Stealth'
                    OnClick = StructControlClick
                    Items.Strings = (
                      'Stealth'
                      '100% heal'
                      '50% heal'
                      '25% heal'
                      'Change type')
                  end
                  object cbCratePowerupAnimation: TCheckBox
                    Left = 0
                    Top = 48
                    Width = 113
                    Height = 17
                    Caption = 'Pickup animation'
                    TabOrder = 2
                    OnClick = StructControlClick
                  end
                  object cbCratePowerupAlwaysPickup: TCheckBox
                    Left = 0
                    Top = 68
                    Width = 97
                    Height = 17
                    Caption = 'Always pick up'
                    TabOrder = 3
                    OnClick = StructControlClick
                  end
                end
                object pnCrateEvent: TPanel
                  Left = 0
                  Top = 104
                  Width = 140
                  Height = 25
                  BevelOuter = bvNone
                  TabOrder = 7
                  object lblCrateEvent: TLabel
                    Left = 0
                    Top = 2
                    Width = 69
                    Height = 13
                    Caption = 'Event number:'
                  end
                  object seCrateEvent: TSpinEdit
                    Tag = -1
                    Left = 72
                    Top = 0
                    Width = 67
                    Height = 22
                    MaxValue = 255
                    MinValue = 0
                    TabOrder = 0
                    Value = 0
                    OnChange = StructControlClick
                  end
                end
                object pnCrateBloom: TPanel
                  Left = 0
                  Top = 104
                  Width = 140
                  Height = 105
                  BevelOuter = bvNone
                  TabOrder = 8
                  object lblCrateBloomRadius: TLabel
                    Left = 0
                    Top = 2
                    Width = 36
                    Height = 13
                    Caption = 'Radius:'
                  end
                  object lblCrateBloomType: TLabel
                    Left = 0
                    Top = 26
                    Width = 27
                    Height = 13
                    Caption = 'Type:'
                  end
                  object lblCrateBloomRadiusPlus: TLabel
                    Left = 112
                    Top = 2
                    Width = 3
                    Height = 13
                  end
                  object cbxCrateBloomType: TComboBox
                    Tag = -1
                    Left = 48
                    Top = 24
                    Width = 91
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    ItemIndex = 0
                    TabOrder = 0
                    Text = 'Classic'
                    OnChange = CrateControlClick
                    Items.Strings = (
                      'Classic'
                      'Instant square'
                      'Instant circle'
                      'Dune2 style')
                  end
                  object cbCrateBloomDestroyUnit: TCheckBox
                    Left = 0
                    Top = 48
                    Width = 113
                    Height = 17
                    Caption = 'Always destroy unit'
                    TabOrder = 1
                    OnClick = StructControlClick
                  end
                  object cbCrateBloomNotShootable: TCheckBox
                    Left = 0
                    Top = 68
                    Width = 97
                    Height = 17
                    Caption = 'Not shootable'
                    TabOrder = 2
                    OnClick = StructControlClick
                  end
                  object seCrateBloomRadius: TSpinEdit
                    Tag = -1
                    Left = 48
                    Top = 0
                    Width = 57
                    Height = 22
                    MaxValue = 15
                    MinValue = 0
                    TabOrder = 3
                    Value = 0
                    OnChange = StructControlClick
                  end
                  object cbCrateBloomRandomizer: TCheckBox
                    Left = 0
                    Top = 88
                    Width = 97
                    Height = 17
                    Caption = 'Randomizer'
                    TabOrder = 4
                    OnClick = StructControlClick
                  end
                end
                object cbxCrateBloomSpawnerType: TComboBox
                  Tag = -1
                  Left = 32
                  Top = 56
                  Width = 108
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  ItemIndex = 0
                  TabOrder = 9
                  Text = 'No spice'
                  Visible = False
                  OnChange = StructControlClick
                  Items.Strings = (
                    'No spice'
                    'Thin spice'
                    'Thick spice'
                    'Immediate spawn')
                end
                object cbCrateBloomSpawnerRespawning: TCheckBox
                  Left = 0
                  Top = 38
                  Width = 97
                  Height = 17
                  Caption = 'Respawning'
                  TabOrder = 10
                  Visible = False
                  OnClick = StructControlClick
                end
              end
            end
          end
        end
      end
      object PageTerrain: TTabSheet
        Caption = 'Terrain            '
        ImageIndex = 1
        object LbBrushSize: TLabel
          Left = 4
          Top = 6
          Width = 51
          Height = 13
          Caption = 'Brush size:'
        end
        object BlockFrame: TBevel
          Left = 14
          Top = 258
          Width = 132
          Height = 132
          Shape = bsFrame
          Style = bsRaised
        end
        object BlockImage: TImage
          Left = 16
          Top = 260
          Width = 128
          Height = 128
          OnClick = BlockImageClick
        end
        object Bevel1: TBevel
          Left = 2
          Top = 164
          Width = 152
          Height = 4
          Shape = bsBottomLine
        end
        object Bevel2: TBevel
          Left = 2
          Top = 220
          Width = 152
          Height = 4
          Shape = bsBottomLine
        end
        object LbPaintTileGroupName: TLabel
          Left = 80
          Top = 30
          Width = 3
          Height = 13
        end
        object lbSelectAreaType: TLabel
          Left = 4
          Top = 200
          Width = 25
          Height = 13
          Caption = 'Area:'
        end
        object RbBlockMode: TRadioButton
          Left = 4
          Top = 232
          Width = 77
          Height = 17
          Caption = 'Block mode'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = RbTerrainModeClick
        end
        object RbPaintMode: TRadioButton
          Left = 4
          Top = 28
          Width = 77
          Height = 17
          Caption = 'Paint mode'
          TabOrder = 1
          OnClick = RbTerrainModeClick
        end
        object OpenTileset: TButton
          Left = 88
          Top = 228
          Width = 65
          Height = 25
          Caption = 'Open tileset'
          TabOrder = 2
          OnClick = OpenTilesetClick
        end
        object RbSelectMode: TRadioButton
          Left = 4
          Top = 174
          Width = 81
          Height = 17
          Caption = 'Select mode'
          TabOrder = 3
          OnClick = RbTerrainModeClick
        end
        object CbSelectStructures: TCheckBox
          Left = 90
          Top = 174
          Width = 97
          Height = 17
          Hint = 
            'Unchecked = copy terrain only'#13#10'Checked = copy terrain and struct' +
            'ures'#13#10'Grayed = copy structures only'
          AllowGrayed = True
          Caption = 'Structures'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 4
        end
        object cbSelectAreaType: TComboBox
          Left = 32
          Top = 196
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 5
        end
        object cbxConcreteSide: TComboBox
          Left = 48
          Top = 26
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 6
          Visible = False
          OnChange = SideSelectChange
        end
        object tbBrushSize: TTrackBar
          Left = 75
          Top = 0
          Width = 81
          Height = 24
          Max = 4
          Min = 1
          PageSize = 1
          Position = 1
          TabOrder = 7
          ThumbLength = 16
        end
      end
    end
  end
  object AppMenu: TMainMenu
    object File1: TMenuItem
      Caption = 'File'
      object Newmap1: TMenuItem
        Caption = 'New map'
        ShortCut = 16462
        OnClick = Newmap1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Openmap1: TMenuItem
        Caption = 'Open map'
        ShortCut = 16463
        OnClick = Openmap1Click
      end
      object Reopenmap1: TMenuItem
        Caption = 'Reopen map'
        ShortCut = 16466
        OnClick = Reopenmap1Click
      end
      object Recentfiles1: TMenuItem
        Caption = 'Recent files'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Savemap1: TMenuItem
        Caption = 'Save map'
        ShortCut = 16467
        OnClick = Savemap1Click
      end
      object Savemapas1: TMenuItem
        Caption = 'Save map as'
        OnClick = Savemapas1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Savemapimage1: TMenuItem
        Caption = 'Save map image'
        ShortCut = 16464
        OnClick = Savemapimage1Click
      end
      object Saveminimapimage1: TMenuItem
        Caption = 'Save minimap image'
        OnClick = Saveminimapimage1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Missionlauncher1: TMenuItem
        Caption = 'Mission browser'
        ShortCut = 16460
        OnClick = Missionlauncher1Click
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Caption = 'Undo'
        Enabled = False
        ShortCut = 16474
        OnClick = Undo1Click
      end
      object Redo1: TMenuItem
        Caption = 'Redo'
        Enabled = False
        ShortCut = 16473
        OnClick = Redo1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Copy1: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = Paste1Click
      end
    end
    object ileset1: TMenuItem
      Caption = 'Tileset'
      object Changetileset1: TMenuItem
        Caption = 'Change tileset...'
        ShortCut = 16468
        OnClick = Changetileset1Click
      end
      object Selectnext1: TMenuItem
        Caption = 'Next tileset'
        ShortCut = 24660
        OnClick = Selectnext1Click
      end
      object Reloadtileset1: TMenuItem
        Caption = 'Reload tileset'
        ShortCut = 24658
        OnClick = Reloadtileset1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Tileseteditor1: TMenuItem
        Caption = 'Tileset editor...'
        ShortCut = 16449
        OnClick = Tileseteditor1Click
      end
    end
    object Structures1: TMenuItem
      Caption = 'Structures'
      object Structureseditor1: TMenuItem
        Caption = 'Structures editor...'
        ShortCut = 16472
        OnClick = Structureseditor1Click
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
      object Usehouseidcolors1: TMenuItem
        Tag = 8
        AutoCheck = True
        Caption = 'Use house ID colors'
        ShortCut = 16457
        OnClick = SettingChange
      end
      object Showunknownspecials1: TMenuItem
        Tag = 9
        AutoCheck = True
        Caption = 'Show unknown specials'
        ShortCut = 16469
        OnClick = SettingChange
      end
      object Gridcolor1: TMenuItem
        Tag = 10
        Caption = 'Grid / Event areas color...'
        OnClick = SettingChange
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Preferences1: TMenuItem
        Caption = 'Preferences...'
        OnClick = Preferences1Click
      end
    end
    object Map1: TMenuItem
      Caption = 'Map'
      object Setmapsize1: TMenuItem
        Caption = 'Set map size'
        ShortCut = 116
        OnClick = Setmapsize1Click
      end
      object Shiftmap1: TMenuItem
        Caption = 'Shift map'
        ShortCut = 117
        OnClick = Shiftmap1Click
      end
      object Changestructureowner1: TMenuItem
        Caption = 'Change structure owner'
        ShortCut = 118
        OnClick = Changestructureowner1Click
      end
      object Remaptiles1: TMenuItem
        Caption = 'Remap tiles'
        ShortCut = 16459
        OnClick = Remaptiles1Click
      end
      object Remapstructures1: TMenuItem
        Caption = 'Remap structures'
        ShortCut = 16458
        OnClick = Remapstructures1Click
      end
      object Converttoadvanced1: TMenuItem
        Caption = 'Convert to advanced'
        OnClick = Converttoadvanced1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Showmapstatistics1: TMenuItem
        Caption = 'Show map statistics'
        ShortCut = 16465
        OnClick = Showmapstatistics1Click
      end
    end
    object Mission1: TMenuItem
      Caption = 'Mission'
      object Missionsettings1: TMenuItem
        Caption = 'Mission settings'
        Enabled = False
        ShortCut = 121
        OnClick = Missionsettings1Click
      end
      object EventsandConditions1: TMenuItem
        Caption = 'Events and Conditions'
        Enabled = False
        ShortCut = 122
        OnClick = EventsandConditions1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Assignmisfile1: TMenuItem
        Caption = 'Assign .mis file'
        OnClick = Assignmisfile1Click
      end
    end
    object Launchgame1: TMenuItem
      Caption = 'Test'
      object Quicklaunch1: TMenuItem
        Caption = 'Quick launch'
        ShortCut = 119
        OnClick = Quicklaunch1Click
      end
      object Launchwithsettings1: TMenuItem
        Caption = 'Launch with settings'
        ShortCut = 120
        OnClick = Launchwithsettings1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object N12: TMenuItem
        Caption = '-'
      end
      object KeyShortcuts1: TMenuItem
        Caption = 'Key Shortcuts'
        OnClick = KeyShortcuts1Click
      end
      object Mouseactions1: TMenuItem
        Caption = 'Mouse actions'
        OnClick = Mouseactions1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Debugwindow1: TMenuItem
        Caption = 'Debug window'
        ShortCut = 255
        OnClick = Debugwindow1Click
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About...'
        OnClick = About1Click
      end
    end
  end
  object MapOpenDialog: TOpenDialog
    DefaultExt = 'map'
    Filter = 'Dune 2000 map (*.map)|*.map|All files (*.*)|*.*'
    Title = 'Open map'
    Left = 32
  end
  object MapSaveDialog: TSaveDialog
    DefaultExt = 'map'
    Filter = 'Dune 2000 map (*.map)|*.map|All files (*.*)|*.*'
    Title = 'Save map'
    Left = 64
  end
  object XPManifest1: TXPManifest
    Left = 128
  end
  object MapImageSaveDialog: TSaveDialog
    DefaultExt = 'png'
    Filter = 'BMP Image (*.bmp)|*.bmp|PNG Image (*.png)|*.png'
    FilterIndex = 2
    Title = 'Save map image'
    Left = 96
  end
  object GridColorDialog: TColorDialog
    Left = 160
  end
  object FindDune2000Dialog: TOpenDialog
    Filter = 'Dune 2000 game executable (DUNE2000.EXE)|dune2000.exe'
    InitialDir = 'C:\'
    Title = 'Find Dune 2000 game location'
    Left = 256
  end
  object RemapTilesOpenDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Remap tiles ini file (*.ini)|*.ini'
    Title = 'Select remap tiles ini file'
    Left = 192
  end
  object RemapStructuresOpenDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Remap structures ini file (*.ini)|*.ini'
    Title = 'Select remap structures ini file'
    Left = 224
  end
end
