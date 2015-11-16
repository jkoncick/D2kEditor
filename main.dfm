object MainWindow: TMainWindow
  Left = 190
  Top = 111
  Width = 950
  Height = 650
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
    OnKeyDown = MapScrollHKeyDown
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
    OnKeyDown = MapScrollVKeyDown
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 577
    Width = 942
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Text = 'BLOXBGBS'
        Width = 70
      end
      item
        Width = 64
      end
      item
        Text = 'No map loaded'
        Width = 230
      end
      item
        Width = 32
      end
      item
        Text = 'W: 0  S: 0  B: 0'
        Width = 100
      end
      item
        Text = 'Power: 0%   (0/0)'
        Width = 150
      end
      item
        Text = 'v1.1'
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
      Top = 6
      Width = 132
      Height = 132
      Shape = bsFrame
      Style = bsRaised
    end
    object MiniMap: TImage
      Left = 20
      Top = 8
      Width = 128
      Height = 128
      OnMouseDown = MiniMapMouseDown
      OnMouseMove = MiniMapMouseMove
    end
    object EditorPages: TPageControl
      Left = 1
      Top = 141
      Width = 166
      Height = 433
      ActivePage = PageStructures
      Align = alBottom
      TabOrder = 0
      OnChange = EditorPagesChange
      object PageStructures: TTabSheet
        Caption = 'Structures       '
        object LbStructureValue: TLabel
          Left = 8
          Top = 2
          Width = 65
          Height = 13
          Caption = 'Special Value'
        end
        object LbMiscObjList: TLabel
          Left = 8
          Top = 24
          Width = 59
          Height = 13
          Caption = 'Misc objects'
        end
        object LbPlayerSelect: TLabel
          Left = 8
          Top = 126
          Width = 29
          Height = 13
          Caption = 'Player'
        end
        object LbBuildingList: TLabel
          Left = 8
          Top = 166
          Width = 42
          Height = 13
          Caption = 'Buildings'
        end
        object LbUnitList: TLabel
          Left = 8
          Top = 270
          Width = 24
          Height = 13
          Caption = 'Units'
        end
        object SpecialValue: TEdit
          Left = 80
          Top = 2
          Width = 73
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object MiscObjList: TListBox
          Left = 8
          Top = 40
          Width = 145
          Height = 82
          ItemHeight = 13
          TabOrder = 1
          OnClick = MiscObjListClick
        end
        object PlayerSelect: TComboBox
          Left = 8
          Top = 142
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = PlayerSelectChange
        end
        object BuildingList: TListBox
          Left = 8
          Top = 182
          Width = 145
          Height = 83
          ItemHeight = 13
          TabOrder = 3
          OnClick = BuildingListClick
        end
        object UnitList: TListBox
          Left = 8
          Top = 286
          Width = 145
          Height = 83
          ItemHeight = 13
          TabOrder = 4
          OnClick = UnitListClick
        end
      end
      object PageTerrain: TTabSheet
        Caption = 'Terrain            '
        ImageIndex = 1
        object LbBrushSize: TLabel
          Left = 4
          Top = 8
          Width = 51
          Height = 13
          Caption = 'Brush size:'
        end
        object BlockFrame: TBevel
          Left = 14
          Top = 182
          Width = 132
          Height = 132
          Shape = bsFrame
          Style = bsRaised
        end
        object BlockImage: TImage
          Left = 16
          Top = 184
          Width = 128
          Height = 128
          OnClick = BlockImageClick
        end
        object Bevel1: TBevel
          Left = 2
          Top = 112
          Width = 152
          Height = 4
          Shape = bsBottomLine
        end
        object Bevel2: TBevel
          Left = 2
          Top = 144
          Width = 152
          Height = 4
          Shape = bsBottomLine
        end
        object sbThinSpice: TSpeedButton
          Tag = -1
          Left = 78
          Top = 32
          Width = 38
          Height = 38
          Hint = 'Thin spice'
          AllowAllUp = True
          GroupIndex = 1
          ParentShowHint = False
          ShowHint = True
          OnClick = PaintTileSelectClick
        end
        object sbThickSpice: TSpeedButton
          Tag = -2
          Left = 116
          Top = 32
          Width = 38
          Height = 38
          Hint = 'Thick spice'
          AllowAllUp = True
          GroupIndex = 1
          ParentShowHint = False
          ShowHint = True
          OnClick = PaintTileSelectClick
        end
        object LbPaintTileGroupName: TLabel
          Left = 20
          Top = 52
          Width = 3
          Height = 13
        end
        object RbBlockMode: TRadioButton
          Left = 4
          Top = 156
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
          Top = 32
          Width = 74
          Height = 17
          Caption = 'Paint mode'
          TabOrder = 1
          OnClick = RbTerrainModeClick
        end
        object OpenTileset: TButton
          Left = 88
          Top = 152
          Width = 65
          Height = 25
          Caption = 'Open tileset'
          TabOrder = 2
          OnClick = OpenTilesetClick
        end
        object RbSelectMode: TRadioButton
          Left = 4
          Top = 122
          Width = 81
          Height = 17
          Caption = 'Select mode'
          TabOrder = 3
          OnClick = RbTerrainModeClick
        end
        object CbSelectStructures: TCheckBox
          Left = 90
          Top = 122
          Width = 97
          Height = 17
          Caption = 'Structures'
          TabOrder = 4
        end
        object cbBrushSize: TComboBox
          Left = 80
          Top = 4
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 5
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
        OnClick = Savemapimage1Click
      end
      object N2: TMenuItem
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
      object Selecttileset1: TMenuItem
        Caption = 'Select tileset'
      end
      object Selectnext1: TMenuItem
        Caption = 'Select next'
        ShortCut = 16468
        OnClick = Selectnext1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Loadtileset1: TMenuItem
        Caption = 'Load image'
        OnClick = Loadtileset1Click
      end
      object Loadtilesetattributes1: TMenuItem
        Caption = 'Load attributes'
        OnClick = Loadtilesetattributes1Click
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
      object ShowGrid1: TMenuItem
        AutoCheck = True
        Caption = 'Show Grid'
        ShortCut = 16455
        OnClick = SettingChange
      end
      object Drawconcrete1: TMenuItem
        AutoCheck = True
        Caption = 'Draw concrete'
        ShortCut = 16456
        OnClick = SettingChange
      end
      object Marktiles1: TMenuItem
        AutoCheck = True
        Caption = 'Mark impassable tiles'
        ShortCut = 16461
        OnClick = SettingChange
      end
      object Markbuildabletiles1: TMenuItem
        AutoCheck = True
        Caption = 'Mark buildable tiles'
        ShortCut = 16450
        OnClick = SettingChange
      end
      object Showunknownspecials1: TMenuItem
        AutoCheck = True
        Caption = 'Show unknown specials'
        ShortCut = 16469
        OnClick = SettingChange
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Useallocationindexes1: TMenuItem
        Caption = 'Use allocation indexes'
        ShortCut = 16457
        OnClick = Useallocationindexes1Click
      end
      object Showeventmarkers1: TMenuItem
        AutoCheck = True
        Caption = 'Show event markers'
        Checked = True
        ShortCut = 16453
        OnClick = SettingChange
      end
      object Markdefenceareas1: TMenuItem
        AutoCheck = True
        Caption = 'Mark defence areas'
        Checked = True
        ShortCut = 16452
        OnClick = SettingChange
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
      object N11: TMenuItem
        Caption = '-'
      end
      object Showmapstatistics1: TMenuItem
        Caption = 'Show map statistics'
        ShortCut = 16471
        OnClick = Showmapstatistics1Click
      end
    end
    object Mission1: TMenuItem
      Caption = 'Mission'
      object Missionsettings1: TMenuItem
        Caption = 'Mission settings'
        ShortCut = 121
        OnClick = Missionsettings1Click
      end
      object EventsandConditions1: TMenuItem
        Caption = 'Events and Conditions'
        ShortCut = 122
        OnClick = EventsandConditions1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Assignmisfile1: TMenuItem
        Caption = '(Un)Assign .mis file'
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
  object TilesetOpenDialog: TOpenDialog
    DefaultExt = 'bmp'
    Filter = 'Dune 2000 Tileset image (*.bmp)|*.bmp'
    InitialDir = '.\tilesets'
    Title = 'Load Tileset image'
    Left = 96
  end
  object MapSaveDialog: TSaveDialog
    DefaultExt = 'map'
    Filter = 'Dune 2000 map (*.map)|*.map|All files (*.*)|*.*'
    Title = 'Save map'
    Left = 64
  end
  object XPManifest1: TXPManifest
    Left = 192
  end
  object MapImageSaveDialog: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Image (*.bmp)|*.bmp'
    Title = 'Save map image'
    Left = 160
  end
  object TileatrOpenDialog: TOpenDialog
    DefaultExt = 'bin'
    Filter = 'Dune 2000 Tileset attributes (*.bin)|*.bin'
    InitialDir = '.\tilesets'
    Title = 'Load Tileset attributes'
    Left = 128
  end
end
