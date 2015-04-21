object MainWindow: TMainWindow
  Left = 193
  Top = 109
  Width = 950
  Height = 650
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Dune 2000 Campaign Map Editor'
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
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseMove = FormMouseMove
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
    OnMouseDown = MapCanvasMouseDown
    OnMouseMove = MapCanvasMouseMove
  end
  object MapScrollH: TScrollBar
    Left = 4
    Top = 456
    Width = 480
    Height = 16
    LargeChange = 4
    PageSize = 0
    TabOrder = 0
    OnChange = MapScrollHChange
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
    OnChange = MapScrollVChange
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
        Width = 60
      end
      item
        Width = 60
      end
      item
        Text = 'No map loaded'
        Width = 320
      end
      item
        Text = 'Power: 0%'
        Width = 80
      end
      item
        Text = 'alpha 0.3'
        Width = 0
      end>
  end
  object EditorMenu: TPanel
    Left = 512
    Top = 0
    Width = 168
    Height = 475
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
    object MiniMapTmp: TImage
      Left = 16
      Top = 8
      Width = 128
      Height = 128
      Visible = False
    end
    object EditorPages: TPageControl
      Left = 1
      Top = 141
      Width = 166
      Height = 333
      ActivePage = PageStructures
      Align = alBottom
      TabOrder = 0
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
        object LbStructureList: TLabel
          Left = 8
          Top = 166
          Width = 48
          Height = 13
          Caption = 'Structures'
        end
        object StructureValue: TEdit
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
          Items.Strings = (
            'Nothing'
            'Thin Spice'
            'Thick Spice'
            'Worm Spawner'
            'Player Start'
            'Spice Bloom')
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
          ItemIndex = 0
          TabOrder = 2
          Text = '0 - Atreides'
          OnChange = PlayerSelectChange
          Items.Strings = (
            '0 - Atreides'
            '1 - Harkonnen'
            '2 - Ordos'
            '3 - Emperor'
            '4 - Fremen'
            '5 - Smugglers'
            '6 - Mercenaries')
        end
        object StructureList: TListBox
          Left = 8
          Top = 182
          Width = 145
          Height = 123
          ItemHeight = 13
          Items.Strings = (
            'Wall'
            'Wind Trap'
            'Construction Yard'
            'Barracks'
            'Refinery'
            'Outpost'
            'Light Factory'
            'Silo'
            'Heavy Factory'
            'Repair Pad'
            'Gun Turret'
            'High Tech Factory'
            'Rocket Turret'
            'IX Research Centre'
            'Starport'
            'Palace'
            'Sietch'
            'Modified Outpost'
            'Light Infantry'
            'Trooper'
            'St. Fremen / Saboteur'
            'Sardakaur / Fremen'
            'Engineer'
            'Harvester'
            'MCV'
            'Trike / Raider'
            'Quad'
            'Combat Tank'
            'Missile Tank'
            'Siege Tank'
            'Carryall'
            'House Special Tank')
          TabOrder = 3
          OnClick = StructureListClick
        end
      end
      object PageTerrain: TTabSheet
        Caption = 'Terrain            '
        ImageIndex = 1
        object LbBlockSize: TLabel
          Left = 0
          Top = 8
          Width = 48
          Height = 13
          Caption = 'Block size'
        end
        object LbX: TLabel
          Left = 100
          Top = 8
          Width = 7
          Height = 13
          Caption = 'X'
        end
        object BlockImage: TImage
          Left = 16
          Top = 104
          Width = 128
          Height = 128
          OnClick = OpenTilesetClick
        end
        object RbCustomBlock: TRadioButton
          Left = 8
          Top = 76
          Width = 73
          Height = 17
          Caption = 'Tile Block'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RbSand: TRadioButton
          Left = 8
          Top = 240
          Width = 113
          Height = 17
          Caption = 'Paint Sand'
          TabOrder = 1
        end
        object RbRock: TRadioButton
          Left = 8
          Top = 264
          Width = 113
          Height = 17
          Caption = 'Paint Rock'
          TabOrder = 2
        end
        object RbDunes: TRadioButton
          Left = 8
          Top = 288
          Width = 113
          Height = 17
          Caption = 'Paint Dunes'
          TabOrder = 3
        end
        object BlockWidth: TSpinEdit
          Left = 56
          Top = 4
          Width = 41
          Height = 22
          MaxValue = 4
          MinValue = 1
          TabOrder = 4
          Value = 1
        end
        object BlockHeight: TSpinEdit
          Left = 112
          Top = 4
          Width = 41
          Height = 22
          MaxValue = 4
          MinValue = 1
          TabOrder = 5
          Value = 1
        end
        object Block11: TButton
          Tag = 1
          Left = 4
          Top = 32
          Width = 38
          Height = 17
          Caption = '1 x 1'
          TabOrder = 6
          OnClick = SetBlockSize
        end
        object Block22: TButton
          Tag = 2
          Left = 41
          Top = 32
          Width = 38
          Height = 17
          Caption = '2 x 2'
          TabOrder = 7
          OnClick = SetBlockSize
        end
        object Block33: TButton
          Tag = 3
          Left = 78
          Top = 32
          Width = 38
          Height = 17
          Caption = '3 x 3'
          TabOrder = 8
          OnClick = SetBlockSize
        end
        object Block44: TButton
          Tag = 4
          Left = 115
          Top = 32
          Width = 38
          Height = 17
          Caption = '4 x 4'
          TabOrder = 9
          OnClick = SetBlockSize
        end
        object OpenTileset: TButton
          Left = 84
          Top = 72
          Width = 65
          Height = 25
          Caption = 'Open tileset'
          TabOrder = 10
          OnClick = OpenTilesetClick
        end
        object BlockUndo: TButton
          Left = 96
          Top = 240
          Width = 49
          Height = 25
          Caption = 'Undo'
          TabOrder = 11
          OnClick = BlockUndoClick
        end
        object Block21: TButton
          Tag = 5
          Left = 4
          Top = 48
          Width = 38
          Height = 17
          Caption = '2 x 1'
          TabOrder = 12
          OnClick = SetBlockSize
        end
        object Block12: TButton
          Tag = 6
          Left = 41
          Top = 48
          Width = 38
          Height = 17
          Caption = '1 x 2'
          TabOrder = 13
          OnClick = SetBlockSize
        end
        object Block32: TButton
          Tag = 7
          Left = 78
          Top = 48
          Width = 38
          Height = 17
          Caption = '3 x 2'
          TabOrder = 14
          OnClick = SetBlockSize
        end
        object Block23: TButton
          Tag = 8
          Left = 115
          Top = 48
          Width = 38
          Height = 17
          Caption = '2 x 3'
          TabOrder = 15
          OnClick = SetBlockSize
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
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object ileset1: TMenuItem
      Caption = 'Tileset'
      object Selecttileset1: TMenuItem
        Caption = 'Select tileset'
        object BLOXBASE1: TMenuItem
          Tag = 1
          Caption = 'BLOXBASE'
          GroupIndex = 1
          RadioItem = True
          OnClick = SelectTileset
        end
        object BLOXBAT1: TMenuItem
          Tag = 2
          Caption = 'BLOXBAT'
          GroupIndex = 1
          RadioItem = True
          OnClick = SelectTileset
        end
        object BLOXBGBS1: TMenuItem
          Tag = 3
          Caption = 'BLOXBGBS'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = SelectTileset
        end
        object BLOXICE1: TMenuItem
          Tag = 4
          Caption = 'BLOXICE'
          GroupIndex = 1
          RadioItem = True
          OnClick = SelectTileset
        end
        object BLOXTREE1: TMenuItem
          Tag = 5
          Caption = 'BLOXTREE'
          GroupIndex = 1
          RadioItem = True
          OnClick = SelectTileset
        end
        object BLOXWAST1: TMenuItem
          Tag = 6
          Caption = 'BLOXWAST'
          GroupIndex = 1
          RadioItem = True
          OnClick = SelectTileset
        end
        object BLOXXMAS1: TMenuItem
          Tag = 7
          Caption = 'BLOXXMAS'
          GroupIndex = 1
          RadioItem = True
          OnClick = SelectTileset
        end
      end
      object Selectnext1: TMenuItem
        Caption = 'Select next'
        ShortCut = 16468
        OnClick = Selectnext1Click
      end
      object Loadtileset1: TMenuItem
        Caption = 'Load from file'
        OnClick = Loadtileset1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Detectfrommis1: TMenuItem
        AutoCheck = True
        Caption = 'Detect from .mis'
        Checked = True
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
      object ShowGrid1: TMenuItem
        Caption = 'Show Grid'
        ShortCut = 16455
        OnClick = ShowGrid1Click
      end
      object Marktiles1: TMenuItem
        Caption = 'Mark impassable tiles'
        ShortCut = 16461
        OnClick = Marktiles1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Useallocationindexes1: TMenuItem
        Caption = 'Use allocation indexes'
        ShortCut = 16457
        OnClick = Useallocationindexes1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Fastrendering1: TMenuItem
        AutoCheck = True
        Caption = 'Fast rendering'
        Checked = True
      end
    end
    object Map1: TMenuItem
      Caption = 'Map'
      object Setmapsize1: TMenuItem
        Caption = 'Set map size'
        OnClick = Setmapsize1Click
      end
      object Shiftmap1: TMenuItem
        Caption = 'Shift map'
        OnClick = Shiftmap1Click
      end
      object Changestructureowner1: TMenuItem
        Caption = 'Change structure owner'
        OnClick = Changestructureowner1Click
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
    Filter = 'Dune 2000 Tileset (*.bmp)|*.bmp'
    InitialDir = '.\graphics'
    Title = 'Load Tileset'
    Left = 96
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
end
