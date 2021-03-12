object StructuresEditor: TStructuresEditor
  Left = 190
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Structures Editor'
  ClientHeight = 684
  ClientWidth = 1016
  Color = clBtnFace
  Constraints.MaxHeight = 738
  Constraints.MaxWidth = 1024
  Constraints.MinHeight = 738
  Constraints.MinWidth = 1024
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 665
    Width = 1016
    Height = 19
    Panels = <
      item
        Width = 544
      end
      item
        Width = 50
      end>
  end
  object PageControl: TPageControl
    Tag = 5
    Left = 0
    Top = 0
    Width = 1016
    Height = 665
    ActivePage = PageBuildings
    Align = alClient
    TabOrder = 1
    OnChange = PageControlChange
    OnChanging = PageControlChanging
    object PageBuildings: TTabSheet
      Caption = 'Buildings   '
      object pnBuildingTypeList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lbBuildingTypeList: TListBox
          Tag = 2
          Left = 0
          Top = 16
          Width = 161
          Height = 537
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbBuildingTypeListClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
      object pnBuildingList: TPanel
        Left = 168
        Top = 0
        Width = 225
        Height = 637
        BevelOuter = bvNone
        TabOrder = 1
        object lbBuildingList: TListBox
          Left = 0
          Top = 16
          Width = 225
          Height = 564
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbBuildingListClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
      object gbBuildingBasic: TGroupBox
        Left = 400
        Top = 0
        Width = 289
        Height = 137
        Caption = 'Basic'
        TabOrder = 2
        object lblBuildingOwnerSide: TLabel
          Left = 80
          Top = 16
          Width = 34
          Height = 26
          Caption = 'Owner side:'
          WordWrap = True
        end
        object lblBuildingName: TLabel
          Left = 8
          Top = 80
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object lblBuildingType: TLabel
          Left = 8
          Top = 104
          Width = 27
          Height = 13
          Caption = 'Type:'
        end
        object imgBuildingIcon: TImage
          Left = 8
          Top = 16
          Width = 60
          Height = 47
        end
        object clbBuildingOwnerSide: TCheckListBox
          Left = 120
          Top = 16
          Width = 161
          Height = 57
          OnClickCheck = RedrawBuildingPreview
          Columns = 2
          ItemHeight = 13
          TabOrder = 0
        end
        object edBuildingName: TEdit
          Left = 40
          Top = 80
          Width = 241
          Height = 21
          MaxLength = 499
          TabOrder = 1
        end
        object cbxBuildingType: TComboBox
          Left = 40
          Top = 104
          Width = 241
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
      end
      object gbBuildingBuildRequirements: TGroupBox
        Left = 400
        Top = 144
        Width = 289
        Height = 289
        Caption = 'Build requirements'
        TabOrder = 3
        object lblBuildingRequirementsTech: TLabel
          Left = 8
          Top = 32
          Width = 28
          Height = 13
          Caption = 'Tech:'
        end
        object lblBuildingRequirementsCost: TLabel
          Left = 8
          Top = 56
          Width = 24
          Height = 13
          Caption = 'Cost:'
        end
        object lblBuildingRequirementsSpeed: TLabel
          Left = 8
          Top = 80
          Width = 34
          Height = 13
          Caption = 'Speed:'
        end
        object lblBuildingRequirementsBuild: TLabel
          Left = 48
          Top = 16
          Width = 23
          Height = 13
          Caption = 'Build'
        end
        object lblBuildingRequirementsUpgrade1: TLabel
          Left = 108
          Top = 16
          Width = 50
          Height = 13
          Caption = 'Upgrade 1'
        end
        object lblBuildingRequirementsUpgrade2: TLabel
          Left = 168
          Top = 16
          Width = 50
          Height = 13
          Caption = 'Upgrade 2'
        end
        object lblBuildingRequirementsUpgrade3: TLabel
          Left = 228
          Top = 16
          Width = 47
          Height = 13
          Caption = 'Upgrade3'
        end
        object lblBuildingPrereq1BuildingType: TLabel
          Left = 8
          Top = 112
          Width = 67
          Height = 13
          Caption = 'Prerequisite 1:'
        end
        object lblBuildingPrereq2BuildingType: TLabel
          Left = 8
          Top = 200
          Width = 67
          Height = 13
          Caption = 'Prerequisite 2:'
        end
        object lblBuildingPrereq1UpgradesNeeded: TLabel
          Left = 8
          Top = 160
          Width = 49
          Height = 26
          Caption = 'Upgrades needed:'
          WordWrap = True
        end
        object lblBuildingPrereq2UpgradesNeeded: TLabel
          Left = 8
          Top = 248
          Width = 49
          Height = 26
          Caption = 'Upgrades needed:'
          WordWrap = True
        end
        object lblBuildingPrereq1OwnerSide: TLabel
          Left = 8
          Top = 136
          Width = 95
          Height = 13
          Caption = 'Owner side needed:'
        end
        object lblBuildingPrereq2OwnerSide: TLabel
          Left = 8
          Top = 224
          Width = 95
          Height = 13
          Caption = 'Owner side needed:'
        end
        object seBuildingTechLevelBuild: TSpinEdit
          Left = 48
          Top = 32
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 0
          Value = 0
        end
        object seBuildingTechLevelUpgrade1: TSpinEdit
          Left = 108
          Top = 32
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 1
          Value = 0
        end
        object seBuildingTechLevelUpgrade2: TSpinEdit
          Left = 168
          Top = 32
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 2
          Value = 0
        end
        object seBuildingTechLevelUpgrade3: TSpinEdit
          Left = 228
          Top = 32
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 3
          Value = 0
        end
        object edBuildingCostBuild: TEdit
          Left = 48
          Top = 56
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 4
        end
        object edBuildingCostUpgrade1: TEdit
          Left = 108
          Top = 56
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 5
        end
        object edBuildingCostUpgrade2: TEdit
          Left = 168
          Top = 56
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 6
        end
        object edBuildingCostUpgrade3: TEdit
          Left = 228
          Top = 56
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 7
        end
        object edBuildingBuildSpeedBuild: TEdit
          Left = 48
          Top = 80
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 8
        end
        object edBuildingBuildSpeedUpgrade1: TEdit
          Left = 108
          Top = 80
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 9
        end
        object edBuildingBuildSpeedUpgrade2: TEdit
          Left = 168
          Top = 80
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 10
        end
        object edBuildingBuildSpeedUpgrade3: TEdit
          Left = 228
          Top = 80
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 11
        end
        object clbBuildingPrereq1OwnerSide: TCheckListBox
          Left = 120
          Top = 136
          Width = 161
          Height = 57
          Columns = 2
          ItemHeight = 13
          TabOrder = 12
        end
        object clbBuildingPrereq2OwnerSide: TCheckListBox
          Left = 120
          Top = 224
          Width = 161
          Height = 57
          Columns = 2
          ItemHeight = 13
          TabOrder = 13
        end
        object cbxBuildingPrereq1BuildingType: TComboBox
          Left = 80
          Top = 112
          Width = 201
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 14
        end
        object cbxBuildingPrereq2BuildingType: TComboBox
          Left = 80
          Top = 200
          Width = 201
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 15
        end
        object seBuildingPrereq1UpgradesNeeded: TSpinEdit
          Left = 64
          Top = 160
          Width = 49
          Height = 22
          MaxLength = 1
          MaxValue = 3
          MinValue = 0
          TabOrder = 16
          Value = 0
        end
        object seBuildingPrereq2UpgradesNeeded: TSpinEdit
          Left = 64
          Top = 248
          Width = 49
          Height = 22
          MaxLength = 1
          MaxValue = 3
          MinValue = 0
          TabOrder = 17
          Value = 0
        end
      end
      object gbBuildingSpaceRequirements: TGroupBox
        Left = 400
        Top = 440
        Width = 289
        Height = 129
        Caption = 'Space requirements'
        TabOrder = 4
        object imgBuildingTilesOccupiedAll: TImage
          Left = 8
          Top = 32
          Width = 67
          Height = 67
          OnMouseDown = imgBuildingTilesOccupiedAllMouseDown
        end
        object imgBuildingTilesOccupiedSolid: TImage
          Left = 88
          Top = 33
          Width = 67
          Height = 67
          OnMouseDown = imgBuildingTilesOccupiedSolidMouseDown
        end
        object lblBuildingTilesOccupiedAll: TLabel
          Left = 8
          Top = 16
          Width = 67
          Height = 13
          Caption = 'Occupied tiles'
        end
        object lblBuildingTilesOccupiedSolid: TLabel
          Left = 88
          Top = 16
          Width = 58
          Height = 13
          Caption = 'Building tiles'
        end
        object lblBuildingExitPoint1X: TLabel
          Left = 160
          Top = 16
          Width = 65
          Height = 13
          Caption = 'Exit point 1 X:'
        end
        object lblBuildingExitPoint1Y: TLabel
          Left = 160
          Top = 40
          Width = 65
          Height = 13
          Caption = 'Exit point 1 Y:'
        end
        object lblBuildingExitPoint2X: TLabel
          Left = 160
          Top = 64
          Width = 65
          Height = 13
          Caption = 'Exit point 2 X:'
        end
        object lblBuildingExitPoint2Y: TLabel
          Left = 160
          Top = 88
          Width = 65
          Height = 13
          Caption = 'Exit point 2 Y:'
        end
        object seBuildingExitPoint1X: TSpinEdit
          Left = 232
          Top = 16
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 0
          Value = 0
          OnChange = RedrawBuildingPreview
        end
        object seBuildingExitPoint1Y: TSpinEdit
          Left = 232
          Top = 40
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 1
          Value = 0
        end
        object seBuildingExitPoint2X: TSpinEdit
          Left = 232
          Top = 64
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 2
          Value = 0
        end
        object seBuildingExitPoint2Y: TSpinEdit
          Left = 232
          Top = 88
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 3
          Value = 0
        end
        object cbBuildingFlagHAS_SKIRT: TCheckBox
          Tag = 2097152
          Left = 8
          Top = 104
          Width = 97
          Height = 17
          Caption = 'Has skirt'
          TabOrder = 4
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagNO_CONCRETE: TCheckBox
          Tag = 4194304
          Left = 80
          Top = 104
          Width = 145
          Height = 17
          Caption = 'Does not require concrete'
          TabOrder = 5
          OnClick = BuildingFlagCheckboxChange
        end
      end
      object gbBuildingProperties: TGroupBox
        Left = 696
        Top = 0
        Width = 305
        Height = 137
        Caption = 'Properties and behavior'
        TabOrder = 5
        object lblBuildingHitPoints: TLabel
          Left = 8
          Top = 16
          Width = 34
          Height = 13
          Caption = 'Health:'
        end
        object lblBuildingArmorType: TLabel
          Left = 120
          Top = 16
          Width = 30
          Height = 13
          Caption = 'Armor:'
        end
        object lblBuildingPowerConsumption: TLabel
          Left = 8
          Top = 40
          Width = 33
          Height = 13
          Caption = 'Power:'
        end
        object lblBuildingHealthBarSize: TLabel
          Left = 120
          Top = 40
          Width = 73
          Height = 13
          Caption = 'Health bar size:'
        end
        object lblBuildingSpecialBehavior: TLabel
          Left = 96
          Top = 64
          Width = 45
          Height = 13
          Caption = 'Behavior:'
        end
        object lblBuildingSightRadius: TLabel
          Left = 8
          Top = 64
          Width = 27
          Height = 13
          Caption = 'Sight:'
        end
        object edBuildingHitPoints: TEdit
          Left = 48
          Top = 16
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object cbxBuildingArmorType: TComboBox
          Left = 152
          Top = 16
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object edBuildingPowerConsumption: TEdit
          Left = 48
          Top = 40
          Width = 65
          Height = 21
          TabOrder = 2
        end
        object cbxBuildingHealthBarSize: TComboBox
          Left = 200
          Top = 40
          Width = 97
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          Items.Strings = (
            '0 - 16px'
            '1 - 24px'
            '2 - 32px'
            '3 - 64px'
            '4 - 96px'
            '5 - 32px')
        end
        object cbxBuildingSpecialBehavior: TComboBox
          Left = 144
          Top = 64
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
          OnChange = RedrawBuildingPreview
          Items.Strings = (
            '0 - None'
            '1 - Radar'
            '2 - Construction Yard'
            '3 - Unknown/Unused'
            '4 - Refinery'
            '5 - Silo'
            '6 - Repair Pad'
            '7 - Production (Light F)'
            '8 - Production (Heavy F)'
            '9 - Production (HTF)'
            '10 - Production (AT HTF)'
            '11 - Production (Barracks)'
            '12 - Production (Unused)'
            '13 - Starport'
            '14 - Wall'
            '15 - Concrete'
            '16 - Turret'
            '17 - Production (H Palace)'
            '18 - Production (O Palace)'
            '19 - Production (A Palace)')
        end
        object cbBuildingFlagSELECT_REPAIR: TCheckBox
          Tag = 512
          Left = 8
          Top = 88
          Width = 137
          Height = 17
          Caption = 'Selectable && Repairable'
          TabOrder = 5
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagCAN_CAPTURE: TCheckBox
          Tag = 1024
          Left = 160
          Top = 88
          Width = 121
          Height = 17
          Caption = 'Can be captured'
          TabOrder = 6
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagALWAYS_DECAY: TCheckBox
          Tag = 32768
          Left = 8
          Top = 112
          Width = 137
          Height = 17
          Caption = 'Decay even on concrete'
          TabOrder = 7
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagCANNOT_SELL: TCheckBox
          Tag = 16777216
          Left = 160
          Top = 112
          Width = 97
          Height = 17
          Caption = 'Cannot be sold'
          TabOrder = 8
          OnClick = BuildingFlagCheckboxChange
        end
        object seBuildingSightRadius: TSpinEdit
          Left = 48
          Top = 64
          Width = 41
          Height = 22
          MaxLength = 1
          MaxValue = 7
          MinValue = 0
          TabOrder = 9
          Value = 0
        end
      end
      object gbBuildingTurret: TGroupBox
        Left = 696
        Top = 144
        Width = 305
        Height = 121
        Caption = 'Turret properties'
        TabOrder = 6
        object lblBuildingPrimaryWeapon: TLabel
          Left = 8
          Top = 40
          Width = 78
          Height = 13
          Caption = 'Primary weapon:'
        end
        object lblBuildingSecondaryWeapon: TLabel
          Left = 8
          Top = 64
          Width = 95
          Height = 13
          Caption = 'Secondary weapon:'
        end
        object lblBuildingRateOfFire: TLabel
          Left = 8
          Top = 88
          Width = 55
          Height = 13
          Caption = 'Rate of fire:'
        end
        object lblBuildingBarrelRotationSpeed: TLabel
          Left = 144
          Top = 88
          Width = 83
          Height = 13
          Caption = 'Barrel turn speed:'
        end
        object cbBuildingActLikeTurret: TCheckBox
          Left = 8
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Act like turret'
          TabOrder = 0
        end
        object cbBuildingRequireEnoughPower: TCheckBox
          Left = 160
          Top = 16
          Width = 137
          Height = 17
          Caption = 'Require enough power'
          TabOrder = 1
        end
        object cbxBuildingPrimaryWeapon: TComboBox
          Left = 112
          Top = 40
          Width = 185
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
        object cbxBuildingSecondaryWeapon: TComboBox
          Left = 112
          Top = 64
          Width = 185
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
        end
        object seBuildingRateOfFire: TSpinEdit
          Left = 72
          Top = 88
          Width = 65
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 4
          Value = 0
        end
        object seBuildingBarrelRotationSpeed: TSpinEdit
          Left = 232
          Top = 88
          Width = 65
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 5
          Value = 0
        end
      end
      object gbBuildingVisuals: TGroupBox
        Left = 696
        Top = 272
        Width = 305
        Height = 365
        Caption = 'Visuals and animations'
        TabOrder = 7
        object imgBuildingImage: TImage
          Left = 168
          Top = 16
          Width = 128
          Height = 160
        end
        object lblBuildingBuildingArt: TLabel
          Left = 8
          Top = 16
          Width = 55
          Height = 13
          Caption = 'Building art:'
        end
        object lblBuildingBarrelArt: TLabel
          Left = 8
          Top = 56
          Width = 45
          Height = 13
          Caption = 'Barrel art:'
        end
        object lblBuildingArtWidth: TLabel
          Left = 8
          Top = 96
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object lblBuildingArtHeight: TLabel
          Left = 84
          Top = 96
          Width = 34
          Height = 13
          Caption = 'Height:'
        end
        object lblBuildingBuildingAnimation: TLabel
          Left = 8
          Top = 168
          Width = 88
          Height = 13
          Caption = 'Building animation:'
        end
        object lblBuildingAnimationSpeed: TLabel
          Left = 168
          Top = 184
          Width = 61
          Height = 13
          Caption = 'Anim. speed:'
        end
        object lblBuildingBuildupArt: TLabel
          Left = 8
          Top = 208
          Width = 86
          Height = 13
          Caption = 'Buildup animation:'
        end
        object lblBuildingBuildupFramesToShow: TLabel
          Left = 168
          Top = 208
          Width = 70
          Height = 39
          Caption = 'Frames before building is shown:'
          WordWrap = True
        end
        object lblBuildingDeathExplosion: TLabel
          Left = 8
          Top = 248
          Width = 104
          Height = 13
          Caption = 'Destruction explosion:'
        end
        object lblBuildingMuzzleFlashExplosion: TLabel
          Left = 160
          Top = 248
          Width = 108
          Height = 13
          Caption = 'Muzzle flash explosion:'
        end
        object lblBuildingDirectionFrames: TLabel
          Left = 8
          Top = 288
          Width = 79
          Height = 13
          Caption = 'Direction frames:'
        end
        object cbxBuildingBuildingArt: TComboBox
          Left = 8
          Top = 32
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = RedrawBuildingPreview
        end
        object cbxBuildingBarrelArt: TComboBox
          Left = 8
          Top = 72
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = RedrawBuildingPreview
        end
        object edBuildingArtWidth: TEdit
          Left = 40
          Top = 96
          Width = 41
          Height = 21
          TabOrder = 2
          OnChange = RedrawBuildingPreview
        end
        object edBuildingArtHeight: TEdit
          Left = 120
          Top = 96
          Width = 41
          Height = 21
          TabOrder = 3
          OnChange = RedrawBuildingPreview
        end
        object cbxBuildingBuildingAnimation: TComboBox
          Left = 8
          Top = 184
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
        end
        object cbBuildingFlagHAS_ANIMATION: TCheckBox
          Tag = 64
          Left = 8
          Top = 120
          Width = 137
          Height = 17
          Caption = 'Has building animation'
          TabOrder = 5
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagANIM_PERMANENT: TCheckBox
          Tag = 16
          Left = 8
          Top = 144
          Width = 89
          Height = 17
          Caption = 'Is permanent'
          TabOrder = 6
          OnClick = BuildingFlagCheckboxChange
        end
        object seBuildingAnimationSpeed: TSpinEdit
          Left = 232
          Top = 184
          Width = 65
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 7
          Value = 0
        end
        object cbxBuildingBuildupArt: TComboBox
          Left = 8
          Top = 224
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 8
        end
        object seBuildingBuildupFramesToShow: TSpinEdit
          Left = 232
          Top = 224
          Width = 65
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 9
          Value = 0
        end
        object cbxBuildingDeathExplosion: TComboBox
          Left = 8
          Top = 264
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 10
        end
        object cbxBuildingMuzzleFlashExplosion: TComboBox
          Left = 160
          Top = 264
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 11
        end
        object sgBuildingDirectionFrames: TStringGrid
          Left = 94
          Top = 290
          Width = 203
          Height = 67
          ColCount = 8
          DefaultColWidth = 24
          DefaultRowHeight = 15
          FixedCols = 0
          RowCount = 4
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 12
        end
        object cbBuildingFlagANIM_ALPHA: TCheckBox
          Tag = 8388608
          Left = 104
          Top = 144
          Width = 57
          Height = 17
          Caption = 'Alpha'
          TabOrder = 13
          OnClick = BuildingFlagCheckboxChange
        end
        object btnBuildingDirectionFrames0: TButton
          Left = 8
          Top = 304
          Width = 75
          Height = 17
          Caption = 'No directions'
          TabOrder = 14
          OnClick = btnBuildingDirectionFramesClick
        end
        object btnBuildingDirectionFrames8: TButton
          Tag = 8
          Left = 8
          Top = 320
          Width = 75
          Height = 17
          Caption = '8 directions'
          TabOrder = 15
          OnClick = btnBuildingDirectionFramesClick
        end
        object btnBuildingDirectionFrames32: TButton
          Tag = 32
          Left = 8
          Top = 336
          Width = 75
          Height = 17
          Caption = '32 directions'
          TabOrder = 16
          OnClick = btnBuildingDirectionFramesClick
        end
        object btnBuildingBuildingAnimationPlay: TButton
          Left = 122
          Top = 184
          Width = 39
          Height = 21
          Caption = 'Play'
          TabOrder = 17
          OnClick = btnBuildingBuildingAnimationPlayClick
        end
        object btnBuildingBuildupArtPlay: TButton
          Left = 122
          Top = 224
          Width = 39
          Height = 21
          Caption = 'Play'
          TabOrder = 18
          OnClick = btnBuildingBuildupArtPlayClick
        end
      end
      object gbBuildingOther: TGroupBox
        Left = 400
        Top = 576
        Width = 289
        Height = 61
        Caption = 'Others'
        TabOrder = 8
        object lblBuildingSellPriority: TLabel
          Left = 88
          Top = 16
          Width = 53
          Height = 13
          Caption = 'Sell priority:'
        end
        object lblBuildingFlags: TLabel
          Left = 8
          Top = 16
          Width = 28
          Height = 13
          Caption = 'Flags:'
        end
        object seBuildingSellPriority: TSpinEdit
          Left = 144
          Top = 16
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object edBuildingFlags: TEdit
          Left = 8
          Top = 32
          Width = 73
          Height = 21
          TabOrder = 1
          OnChange = edBuildingFlagsChange
        end
        object cbBuildingFlagAUTOREPAIR: TCheckBox
          Tag = 1
          Left = 200
          Top = 38
          Width = 81
          Height = 17
          Caption = 'Repairing'
          TabOrder = 2
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagUNKNOWN9: TCheckBox
          Tag = 256
          Left = 200
          Top = 14
          Width = 87
          Height = 17
          Caption = 'Unknown flag'
          TabOrder = 3
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingScreenShake: TCheckBox
          Left = 88
          Top = 38
          Width = 97
          Height = 17
          Caption = 'Screen shake'
          TabOrder = 4
        end
      end
    end
    object PageUnits: TTabSheet
      Caption = 'Units         '
      ImageIndex = 1
      object pnUnitTypeList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lbUnitTypeList: TListBox
          Tag = 3
          Left = 0
          Top = 16
          Width = 161
          Height = 537
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbUnitTypeListClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
      object pnUnitList: TPanel
        Left = 168
        Top = 0
        Width = 225
        Height = 637
        BevelOuter = bvNone
        TabOrder = 1
        object lbUnitList: TListBox
          Tag = 1
          Left = 0
          Top = 16
          Width = 225
          Height = 564
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbUnitListClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
      object gbUnitBasic: TGroupBox
        Left = 400
        Top = 0
        Width = 289
        Height = 137
        Caption = 'Basic'
        TabOrder = 2
        object lblUnitOwnerSide: TLabel
          Left = 80
          Top = 16
          Width = 34
          Height = 26
          Caption = 'Owner side:'
          WordWrap = True
        end
        object lblUnitName: TLabel
          Left = 8
          Top = 80
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object lblUnitType: TLabel
          Left = 8
          Top = 104
          Width = 27
          Height = 13
          Caption = 'Type:'
        end
        object imgUnitIcon: TImage
          Left = 8
          Top = 16
          Width = 60
          Height = 47
        end
        object clbUnitOwnerSide: TCheckListBox
          Left = 120
          Top = 16
          Width = 161
          Height = 57
          OnClickCheck = RedrawUnitPreview
          Columns = 2
          ItemHeight = 13
          TabOrder = 0
        end
        object edUnitName: TEdit
          Left = 40
          Top = 80
          Width = 241
          Height = 21
          MaxLength = 499
          TabOrder = 1
          OnChange = RedrawUnitPreview
        end
        object cbxUnitType: TComboBox
          Left = 40
          Top = 104
          Width = 241
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
      end
      object gbUnitBuildRequirements: TGroupBox
        Left = 400
        Top = 144
        Width = 289
        Height = 201
        Caption = 'Build requirements'
        TabOrder = 3
        object lblUnitTechLevel: TLabel
          Left = 8
          Top = 24
          Width = 28
          Height = 13
          Caption = 'Tech:'
        end
        object lblUnitCost: TLabel
          Left = 96
          Top = 24
          Width = 24
          Height = 13
          Caption = 'Cost:'
        end
        object lblUnitBuildSpeed: TLabel
          Left = 184
          Top = 24
          Width = 32
          Height = 26
          Caption = 'Build speed:'
          WordWrap = True
        end
        object lblUnitPrereq1BuildingType: TLabel
          Left = 8
          Top = 56
          Width = 67
          Height = 13
          Caption = 'Prerequisite 1:'
        end
        object lblUnitPrereq2BuildingType: TLabel
          Left = 8
          Top = 144
          Width = 67
          Height = 13
          Caption = 'Prerequisite 2:'
        end
        object lblUnitPrereq1UpgradesNeeded: TLabel
          Left = 8
          Top = 104
          Width = 49
          Height = 26
          Caption = 'Upgrades needed:'
          WordWrap = True
        end
        object lblUnitPrereq1OwnerSide: TLabel
          Left = 8
          Top = 80
          Width = 95
          Height = 13
          Caption = 'Owner side needed:'
        end
        object seUnitTechLevel: TSpinEdit
          Left = 40
          Top = 24
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 0
          Value = 0
        end
        object edUnitCost: TEdit
          Left = 128
          Top = 24
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 1
        end
        object edUnitBuildSpeed: TEdit
          Left = 224
          Top = 24
          Width = 53
          Height = 21
          MaxLength = 10
          TabOrder = 2
        end
        object clbUnitPrereq1OwnerSide: TCheckListBox
          Left = 120
          Top = 80
          Width = 161
          Height = 57
          Columns = 2
          ItemHeight = 13
          TabOrder = 3
        end
        object cbxUnitPrereq1BuildingType: TComboBox
          Left = 80
          Top = 56
          Width = 201
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
        end
        object cbxUnitPrereq2BuildingType: TComboBox
          Left = 80
          Top = 144
          Width = 201
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 5
        end
        object seUnitPrereq1UpgradesNeeded: TSpinEdit
          Left = 64
          Top = 104
          Width = 49
          Height = 22
          MaxLength = 1
          MaxValue = 3
          MinValue = 0
          TabOrder = 6
          Value = 0
        end
        object cbUnitAvailableInStarport: TCheckBox
          Left = 8
          Top = 176
          Width = 121
          Height = 17
          Caption = 'Available in Starport'
          TabOrder = 7
        end
        object cbUnitMultiplayerOnly: TCheckBox
          Left = 152
          Top = 176
          Width = 97
          Height = 17
          Caption = 'Multiplayer only'
          TabOrder = 8
        end
      end
      object gbUnitVoices: TGroupBox
        Left = 400
        Top = 352
        Width = 289
        Height = 285
        Caption = 'Voices'
        TabOrder = 4
        object lblUnitReportingSounds: TLabel
          Left = 8
          Top = 16
          Width = 86
          Height = 13
          Caption = 'Reporting sounds:'
        end
        object lblUnitConfirmedSounds: TLabel
          Left = 148
          Top = 16
          Width = 87
          Height = 13
          Caption = 'Confirmed sounds:'
        end
        object lblUnitVoicePriority: TLabel
          Left = 8
          Top = 256
          Width = 63
          Height = 13
          Caption = 'Voice priority:'
        end
        object seUnitVoicePriority: TSpinEdit
          Left = 80
          Top = 256
          Width = 61
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
      end
      object gbUnitProperties: TGroupBox
        Left = 696
        Top = 0
        Width = 305
        Height = 113
        Caption = 'Properties and behavior'
        TabOrder = 5
        object lblUnitHitPoints: TLabel
          Left = 8
          Top = 16
          Width = 34
          Height = 13
          Caption = 'Health:'
        end
        object lblUnitArmorType: TLabel
          Left = 120
          Top = 16
          Width = 30
          Height = 13
          Caption = 'Armor:'
        end
        object lblUnitHealthBarSize: TLabel
          Left = 120
          Top = 40
          Width = 73
          Height = 13
          Caption = 'Health bar size:'
        end
        object lblUnitSpecialBehavior: TLabel
          Left = 96
          Top = 64
          Width = 45
          Height = 13
          Caption = 'Behavior:'
        end
        object lblUnitSightRadius: TLabel
          Left = 8
          Top = 40
          Width = 27
          Height = 13
          Caption = 'Sight:'
        end
        object edUnitHitPoints: TEdit
          Left = 48
          Top = 16
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object cbxUnitArmorType: TComboBox
          Left = 152
          Top = 16
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object cbxUnitHealthBarSize: TComboBox
          Left = 200
          Top = 40
          Width = 97
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            '0 - 16px'
            '1 - 24px'
            '2 - 32px'
            '3 - 64px'
            '4 - 96px'
            '5 - 32px')
        end
        object cbxUnitSpecialBehavior: TComboBox
          Left = 144
          Top = 64
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          OnChange = RedrawUnitPreview
          Items.Strings = (
            '0 - None'
            '1 - Harvester'
            '2 - Carryall'
            '3 - Engineer'
            '4 - Saboteur'
            '5 - Sandworm'
            '6 - MCV'
            '7 - Devastator'
            '8 - Frigate'
            '9 - Ornithopter'
            '10 - DH missile'
            '11 - Sardaukar'
            '12 - Fremen'
            '13 - (unused?)'
            '14 - Thumper'
            '15 - (crash)')
        end
        object cbUnitIsInfantry: TCheckBox
          Tag = 512
          Left = 8
          Top = 64
          Width = 73
          Height = 17
          Caption = 'Is Infantry'
          TabOrder = 4
        end
        object cbUnitFlagUF_STEALTH: TCheckBox
          Tag = 16
          Left = 112
          Top = 88
          Width = 57
          Height = 17
          Caption = 'Stealth'
          TabOrder = 5
          OnClick = UnitFlagCheckboxChange
        end
        object cbUnitFlagUF_SELFHEALING: TCheckBox
          Tag = 8388608
          Left = 8
          Top = 88
          Width = 81
          Height = 17
          Caption = 'Self-healing'
          TabOrder = 6
          OnClick = UnitFlagCheckboxChange
        end
        object seUnitSightRadius: TSpinEdit
          Left = 48
          Top = 40
          Width = 41
          Height = 22
          MaxLength = 1
          MaxValue = 7
          MinValue = 0
          TabOrder = 7
          Value = 0
        end
        object cbUnitFlagUF_NO_AI: TCheckBox
          Tag = 2048
          Left = 200
          Top = 88
          Width = 57
          Height = 17
          Caption = 'No AI'
          TabOrder = 8
          OnClick = UnitFlagCheckboxChange
        end
      end
      object gbUnitMovement: TGroupBox
        Left = 696
        Top = 120
        Width = 305
        Height = 73
        Caption = 'Movement'
        TabOrder = 6
        object lblUnitSpeed: TLabel
          Left = 8
          Top = 16
          Width = 34
          Height = 13
          Caption = 'Speed:'
        end
        object lblUnitSpeedType: TLabel
          Left = 120
          Top = 16
          Width = 57
          Height = 13
          Caption = 'Speed type:'
        end
        object lblUnitUnitRotationSpeed: TLabel
          Left = 8
          Top = 40
          Width = 71
          Height = 13
          Caption = 'Turning speed:'
        end
        object edUnitSpeed: TEdit
          Left = 48
          Top = 16
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object cbxUnitSpeedType: TComboBox
          Left = 184
          Top = 16
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object seUnitUnitRotationSpeed: TSpinEdit
          Left = 88
          Top = 40
          Width = 57
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object cbUnitCanCrushInfantry: TCheckBox
          Left = 152
          Top = 40
          Width = 113
          Height = 17
          Caption = 'Can crush infantry'
          TabOrder = 3
        end
      end
      object gbUnitWeapons: TGroupBox
        Left = 696
        Top = 200
        Width = 305
        Height = 145
        Caption = 'Weapons'
        TabOrder = 7
        object lblUnitPrimaryWeapon: TLabel
          Left = 8
          Top = 16
          Width = 78
          Height = 13
          Caption = 'Primary weapon:'
        end
        object lblUnitSecondaryWeapon: TLabel
          Left = 8
          Top = 40
          Width = 95
          Height = 13
          Caption = 'Secondary weapon:'
        end
        object lblUnitRateOfFire: TLabel
          Left = 8
          Top = 88
          Width = 55
          Height = 13
          Caption = 'Rate of fire:'
        end
        object lblUnitBarrelRotationSpeed: TLabel
          Left = 144
          Top = 88
          Width = 83
          Height = 13
          Caption = 'Barrel turn speed:'
        end
        object lblUnitProjectileShootOffset: TLabel
          Left = 8
          Top = 116
          Width = 104
          Height = 13
          Caption = 'Projectile shoot offset:'
        end
        object cbUnitHasBarrel: TCheckBox
          Left = 8
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Has barrel'
          TabOrder = 0
        end
        object cbUnitFlagUF_FIXED_BARREL: TCheckBox
          Tag = 32768
          Left = 144
          Top = 64
          Width = 137
          Height = 17
          Caption = 'Barrel is fixed'
          TabOrder = 1
          OnClick = UnitFlagCheckboxChange
        end
        object cbxUnitPrimaryWeapon: TComboBox
          Left = 112
          Top = 16
          Width = 185
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
        object cbxUnitSecondaryWeapon: TComboBox
          Left = 112
          Top = 40
          Width = 185
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
        end
        object seUnitRateOfFire: TSpinEdit
          Left = 72
          Top = 88
          Width = 65
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 4
          Value = 0
        end
        object seUnitBarrelRotationSpeed: TSpinEdit
          Left = 232
          Top = 88
          Width = 65
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 5
          Value = 0
        end
        object seUnitProjectileShootOffset: TSpinEdit
          Left = 120
          Top = 116
          Width = 65
          Height = 22
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          TabOrder = 6
          Value = 0
        end
      end
      object gbUnitVisuals: TGroupBox
        Left = 696
        Top = 352
        Width = 305
        Height = 225
        Caption = 'Visuals'
        TabOrder = 8
        object imgUnitImage: TImage
          Left = 192
          Top = 16
          Width = 80
          Height = 80
        end
        object lblUnitUnitArt: TLabel
          Left = 8
          Top = 16
          Width = 37
          Height = 13
          Caption = 'Unit art:'
        end
        object lblUnitBarrelArt: TLabel
          Left = 8
          Top = 56
          Width = 45
          Height = 13
          Caption = 'Barrel art:'
        end
        object lblUnitDeathExplosion: TLabel
          Left = 8
          Top = 104
          Width = 104
          Height = 13
          Caption = 'Destruction explosion:'
        end
        object lblUnitMuzzleFlashExplosion: TLabel
          Left = 160
          Top = 104
          Width = 108
          Height = 13
          Caption = 'Muzzle flash explosion:'
        end
        object lblUnitDirectionFrames: TLabel
          Left = 8
          Top = 144
          Width = 79
          Height = 13
          Caption = 'Direction frames:'
        end
        object cbxUnitUnitArt: TComboBox
          Left = 8
          Top = 32
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = RedrawUnitPreview
        end
        object cbxUnitBarrelArt: TComboBox
          Left = 8
          Top = 72
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = RedrawUnitPreview
        end
        object cbxUnitDeathExplosion: TComboBox
          Left = 8
          Top = 120
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
        object cbxUnitMuzzleFlashExplosion: TComboBox
          Left = 160
          Top = 120
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
        end
        object sgUnitDirectionFrames: TStringGrid
          Left = 94
          Top = 146
          Width = 203
          Height = 67
          ColCount = 8
          DefaultColWidth = 24
          DefaultRowHeight = 15
          FixedCols = 0
          RowCount = 4
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 4
        end
        object btnUnitDirectionFrames0: TButton
          Left = 8
          Top = 160
          Width = 75
          Height = 17
          Caption = 'No directions'
          TabOrder = 5
          OnClick = btnUnitDirectionFramesClick
        end
        object btnUnitDirectionFrames8: TButton
          Tag = 8
          Left = 8
          Top = 176
          Width = 75
          Height = 17
          Caption = '8 directions'
          TabOrder = 6
          OnClick = btnUnitDirectionFramesClick
        end
        object btnUnitDirectionFrames32: TButton
          Tag = 32
          Left = 8
          Top = 192
          Width = 75
          Height = 17
          Caption = '32 directions'
          TabOrder = 7
          OnClick = btnUnitDirectionFramesClick
        end
      end
      object gbUnitOther: TGroupBox
        Left = 696
        Top = 584
        Width = 305
        Height = 53
        Caption = 'Others'
        TabOrder = 9
        object lblUnitUnknown52: TLabel
          Left = 216
          Top = 24
          Width = 22
          Height = 13
          Caption = 'B52:'
        end
        object lblUnitFlags: TLabel
          Left = 8
          Top = 24
          Width = 28
          Height = 13
          Caption = 'Flags:'
        end
        object lblUnitUnknown46: TLabel
          Left = 128
          Top = 24
          Width = 22
          Height = 13
          Caption = 'B46:'
        end
        object seUnitUnknown52: TSpinEdit
          Left = 248
          Top = 24
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object edUnitFlags: TEdit
          Left = 40
          Top = 24
          Width = 73
          Height = 21
          TabOrder = 1
          OnChange = edUnitFlagsChange
        end
        object seUnitUnknown46: TSpinEdit
          Left = 160
          Top = 24
          Width = 49
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
      end
    end
    object PageBuildingArt: TTabSheet
      Caption = 'Building Art'
      ImageIndex = 6
      object pnBuildingArtList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lblBuildingArtList: TLabel
          Left = 0
          Top = 0
          Width = 52
          Height = 13
          Caption = 'Building art'
        end
        object lblBuildingArtDirections: TLabel
          Left = 0
          Top = 536
          Width = 79
          Height = 13
          Caption = 'Direction frames:'
        end
        object lbBuildingArtList: TListBox
          Left = 0
          Top = 16
          Width = 161
          Height = 513
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbBuildingArtListClick
          OnKeyDown = ArtControlGroupListKeyDown
        end
        object seBuildingArtDirections: TSpinEdit
          Left = 88
          Top = 536
          Width = 73
          Height = 22
          MaxValue = 255
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
      end
      object pnBuildingArtControlGroup: TPanel
        Left = 176
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pnBuildingAnimationArtList: TPanel
        Left = 360
        Top = 0
        Width = 273
        Height = 637
        BevelOuter = bvNone
        TabOrder = 2
        object lblBuildingAnimationArtList: TLabel
          Left = 0
          Top = 0
          Width = 201
          Height = 13
          Caption = 'Building animations and buildup animations'
        end
        object lblBuildingAnimationFrames: TLabel
          Left = 0
          Top = 596
          Width = 83
          Height = 13
          Caption = 'Animation frames:'
        end
        object lblBuildupArtFrames: TLabel
          Left = 96
          Top = 596
          Width = 72
          Height = 13
          Caption = 'Buildup frames:'
        end
        object lbBuildingAnimationArtList: TListBox
          Tag = 1
          Left = 0
          Top = 16
          Width = 273
          Height = 577
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbBuildingAnimationArtListClick
          OnKeyDown = ArtControlGroupListKeyDown
        end
        object btnBuildingAnimationArtModify: TButton
          Left = 192
          Top = 612
          Width = 81
          Height = 25
          Caption = 'Change'
          TabOrder = 1
          OnClick = btnBuildingAnimationArtModifyClick
        end
        object seBuildingAnimationFrames: TSpinEdit
          Left = 0
          Top = 612
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object seBuildupArtFrames: TSpinEdit
          Left = 96
          Top = 612
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 3
          Value = 0
        end
      end
      object pnBuildingAnimationControlGroup: TPanel
        Left = 648
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 3
      end
      object pnBuildupArtControlGroup: TPanel
        Left = 832
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 4
      end
    end
    object PageBuilExp: TTabSheet
      Caption = 'BuilExp     '
      ImageIndex = 8
      object lblBuilExpNumAnimations: TLabel
        Left = 272
        Top = 184
        Width = 105
        Height = 13
        Caption = 'Number of animations:'
      end
      object imgBuilExpImage: TImage
        Left = 272
        Top = 16
        Width = 128
        Height = 160
      end
      object pnBuilExpBuildingList: TPanel
        Left = 0
        Top = 0
        Width = 257
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lblBuilExpBuildingList: TLabel
          Left = 0
          Top = 0
          Width = 42
          Height = 13
          Caption = 'Buildings'
        end
        object lbBuilExpBuildingList: TListBox
          Left = 0
          Top = 16
          Width = 257
          Height = 617
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbBuilExpBuildingListClick
        end
      end
      object seBuilExpNumAnimations: TSpinEdit
        Left = 384
        Top = 184
        Width = 49
        Height = 22
        MaxValue = 8
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = seBuilExpNumAnimationsChange
      end
      object pnBuilExpAnimations: TPanel
        Left = 272
        Top = 208
        Width = 409
        Height = 241
        BevelOuter = bvNone
        TabOrder = 2
        Visible = False
        object lblBuilExpAnimOffsetX: TLabel
          Left = 64
          Top = 8
          Width = 38
          Height = 13
          Caption = 'Offset X'
        end
        object lblBuilExpAnimOffsetY: TLabel
          Left = 128
          Top = 8
          Width = 38
          Height = 13
          Caption = 'Offset Y'
        end
        object lblBuilExpAnimExplosion: TLabel
          Left = 192
          Top = 8
          Width = 45
          Height = 13
          Caption = 'Explosion'
        end
        object lblBuilExpAnimNumFrames: TLabel
          Left = 336
          Top = 8
          Width = 34
          Height = 13
          Caption = 'Frames'
        end
      end
    end
    object PageUnitArt: TTabSheet
      Caption = 'Unit Art     '
      ImageIndex = 7
      object pnUnitArtList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lblUnitArtList: TLabel
          Left = 0
          Top = 0
          Width = 34
          Height = 13
          Caption = 'Unit art'
        end
        object lblUnitArtAnimationFrames: TLabel
          Left = 0
          Top = 512
          Width = 83
          Height = 13
          Caption = 'Animation frames:'
        end
        object lblUnitArtDirectionFrames: TLabel
          Left = 0
          Top = 536
          Width = 79
          Height = 13
          Caption = 'Direction frames:'
        end
        object lbUnitArtList: TListBox
          Tag = 3
          Left = 0
          Top = 16
          Width = 161
          Height = 489
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbUnitArtListClick
          OnKeyDown = ArtControlGroupListKeyDown
        end
        object seUnitArtAnimationFrames: TSpinEdit
          Left = 88
          Top = 512
          Width = 73
          Height = 22
          MaxValue = 255
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
        object seUnitArtDirectionFrames: TSpinEdit
          Left = 88
          Top = 536
          Width = 73
          Height = 22
          MaxValue = 255
          MinValue = 1
          TabOrder = 2
          Value = 1
        end
      end
      object pnUnitArtControlGroup: TPanel
        Left = 176
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object PageWeapons: TTabSheet
      Caption = 'Weapons  '
      ImageIndex = 2
      object lblWeaponUsedBy: TLabel
        Left = 168
        Top = 408
        Width = 337
        Height = 97
        AutoSize = False
        WordWrap = True
      end
      object pnProjectileArtList: TPanel
        Left = 512
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lblProjectileArtList: TLabel
          Left = 0
          Top = 0
          Width = 58
          Height = 13
          Caption = 'Projectile art'
        end
        object lblProjectileArtDirections: TLabel
          Left = 0
          Top = 586
          Width = 79
          Height = 13
          Caption = 'Direction frames:'
        end
        object lbProjectileArtList: TListBox
          Tag = 4
          Left = 0
          Top = 16
          Width = 161
          Height = 564
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbProjectileArtListClick
          OnKeyDown = ArtControlGroupListKeyDown
        end
        object seProjectileArtDirections: TSpinEdit
          Left = 88
          Top = 586
          Width = 73
          Height = 22
          MaxValue = 255
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
      end
      object pnProjectileArtControlGroup: TPanel
        Left = 688
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pnWeaponList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 2
        object lbWeaponList: TListBox
          Tag = 4
          Left = 0
          Top = 16
          Width = 161
          Height = 564
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbWeaponListClick
          OnDblClick = btnWeaponFiringSoundPlayClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
      object gbWeaponProperties: TGroupBox
        Left = 168
        Top = 16
        Width = 337
        Height = 121
        Caption = 'Properties and behavior'
        TabOrder = 3
        object lblWeaponDamage: TLabel
          Left = 8
          Top = 48
          Width = 43
          Height = 13
          Caption = 'Damage:'
        end
        object lblWeaponWarhead: TLabel
          Left = 136
          Top = 48
          Width = 47
          Height = 13
          Caption = 'Warhead:'
        end
        object lblWeaponRange: TLabel
          Left = 8
          Top = 72
          Width = 35
          Height = 13
          Caption = 'Range:'
        end
        object lblWeaponName: TLabel
          Left = 8
          Top = 20
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object edWeaponDamage: TEdit
          Left = 56
          Top = 48
          Width = 73
          Height = 21
          TabOrder = 0
        end
        object cbxWeaponWarhead: TComboBox
          Left = 192
          Top = 48
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object edWeaponRange: TEdit
          Left = 56
          Top = 72
          Width = 73
          Height = 21
          TabOrder = 2
        end
        object cbWeaponAntiAircraft: TCheckBox
          Left = 136
          Top = 72
          Width = 97
          Height = 17
          Caption = 'Anti-aircraft'
          TabOrder = 3
        end
        object cbWeaponFlagWF_BLOCKED_BY_WALL: TCheckBox
          Tag = 4096
          Left = 224
          Top = 72
          Width = 97
          Height = 17
          Caption = 'Blocked by wall'
          TabOrder = 4
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_DEVIATOR: TCheckBox
          Tag = 64
          Left = 136
          Top = 96
          Width = 81
          Height = 17
          Caption = 'Deviator'
          TabOrder = 5
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_SONIC: TCheckBox
          Tag = 2048
          Left = 224
          Top = 96
          Width = 65
          Height = 17
          Caption = 'Sonic'
          TabOrder = 6
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_FALLING: TCheckBox
          Tag = 256
          Left = 8
          Top = 96
          Width = 57
          Height = 17
          Caption = 'Falling'
          TabOrder = 7
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_DEBRIS: TCheckBox
          Tag = 2
          Left = 72
          Top = 96
          Width = 57
          Height = 17
          Caption = 'Debris'
          TabOrder = 8
          OnClick = WeaponFlagCheckboxChange
        end
        object edWeaponName: TEdit
          Left = 56
          Top = 20
          Width = 241
          Height = 21
          MaxLength = 499
          TabOrder = 9
        end
      end
      object dbWeaponMovement: TGroupBox
        Left = 168
        Top = 144
        Width = 337
        Height = 57
        Caption = 'Projectile movement'
        TabOrder = 4
        object lblWeaponProjectileSpeed: TLabel
          Left = 8
          Top = 24
          Width = 34
          Height = 13
          Caption = 'Speed:'
        end
        object edWeaponProjectileSpeed: TEdit
          Left = 48
          Top = 24
          Width = 73
          Height = 21
          TabOrder = 0
        end
        object cbWeaponFlagWF_ARC_TRAJECTORY: TCheckBox
          Tag = 1
          Left = 200
          Top = 24
          Width = 129
          Height = 17
          Caption = 'Arc trajectory + shadow'
          TabOrder = 1
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_HOMING: TCheckBox
          Tag = 32
          Left = 128
          Top = 24
          Width = 57
          Height = 17
          Caption = 'Homing'
          TabOrder = 2
          OnClick = WeaponFlagCheckboxChange
        end
      end
      object gbWeaponVisuals: TGroupBox
        Left = 168
        Top = 208
        Width = 337
        Height = 129
        Caption = 'Visuals and sounds'
        TabOrder = 5
        object lblWeaponProjectileArt: TLabel
          Left = 8
          Top = 24
          Width = 61
          Height = 13
          Caption = 'Projectile art:'
        end
        object lblWeaponFiringSound: TLabel
          Left = 8
          Top = 48
          Width = 60
          Height = 13
          Caption = 'Firing sound:'
        end
        object lblWeaponTrailExplosion: TLabel
          Left = 8
          Top = 96
          Width = 70
          Height = 13
          Caption = 'Trail explosion:'
        end
        object lblWeaponHitExplosion: TLabel
          Left = 8
          Top = 72
          Width = 63
          Height = 13
          Caption = 'Hit explosion:'
        end
        object cbxWeaponProjectileArt: TComboBox
          Left = 80
          Top = 24
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbxWeaponProjectileArtChange
        end
        object cbxWeaponFiringSound: TComboBox
          Left = 80
          Top = 48
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object cbxWeaponHitExplosion: TComboBox
          Left = 80
          Top = 72
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
        object cbxWeaponTrailExplosion: TComboBox
          Left = 80
          Top = 96
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
        end
        object cbWeaponFlagWF_PROJECTILE_ALPHA: TCheckBox
          Tag = 512
          Left = 224
          Top = 24
          Width = 105
          Height = 17
          Caption = 'Projectile art alpha'
          TabOrder = 4
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_ANIM_PROJECTILE: TCheckBox
          Tag = 128
          Left = 224
          Top = 72
          Width = 109
          Height = 25
          Caption = 'Animated (rotating) projectile'
          TabOrder = 5
          WordWrap = True
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_MAKE_TRAIL: TCheckBox
          Tag = 16
          Left = 224
          Top = 102
          Width = 97
          Height = 17
          Caption = 'Makes trail'
          TabOrder = 6
          OnClick = WeaponFlagCheckboxChange
        end
        object btnWeaponFiringSoundPlay: TButton
          Left = 224
          Top = 48
          Width = 57
          Height = 21
          Caption = 'Play'
          TabOrder = 7
          OnClick = btnWeaponFiringSoundPlayClick
        end
      end
      object gbWeaponOtherUnknown: TGroupBox
        Left = 168
        Top = 344
        Width = 337
        Height = 57
        Caption = 'Others and unknown'
        TabOrder = 6
        object lblWeaponFlags: TLabel
          Left = 8
          Top = 24
          Width = 28
          Height = 13
          Caption = 'Flags:'
        end
        object lblWeaponUnknown19: TLabel
          Left = 124
          Top = 24
          Width = 39
          Height = 13
          Caption = 'Byte 19:'
        end
        object edWeaponFlags: TEdit
          Left = 40
          Top = 24
          Width = 73
          Height = 21
          TabOrder = 0
          OnChange = edWeaponFlagsChange
        end
        object seWeaponUnknown19: TSpinEdit
          Left = 172
          Top = 23
          Width = 53
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
    end
    object PageExplosions: TTabSheet
      Caption = 'Explosions '
      ImageIndex = 3
      object lblExplosionMyIndex: TLabel
        Left = 406
        Top = 48
        Width = 45
        Height = 13
        Caption = 'My index:'
      end
      object lblExplosionSound: TLabel
        Left = 168
        Top = 48
        Width = 34
        Height = 13
        Caption = 'Sound:'
      end
      object lblExplosionMuzzleFlashPattern: TLabel
        Left = 336
        Top = 128
        Width = 97
        Height = 13
        Caption = 'Muzzle flash pattern:'
      end
      object lblExplosionUsedBy: TLabel
        Left = 168
        Top = 160
        Width = 337
        Height = 449
        AutoSize = False
        WordWrap = True
      end
      object lblExplosionFlags: TLabel
        Left = 168
        Top = 128
        Width = 28
        Height = 13
        Caption = 'Flags:'
      end
      object lblExplosionName: TLabel
        Left = 168
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object pnAnimationArtList: TPanel
        Left = 512
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lblAnimationArtList: TLabel
          Left = 0
          Top = 0
          Width = 61
          Height = 13
          Caption = 'Animation art'
        end
        object lblAnimationArtFrames: TLabel
          Left = 0
          Top = 586
          Width = 37
          Height = 13
          Caption = 'Frames:'
        end
        object lbAnimationArtList: TListBox
          Tag = 5
          Left = 0
          Top = 16
          Width = 161
          Height = 564
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbAnimationArtListClick
          OnKeyDown = ArtControlGroupListKeyDown
        end
        object seAnimationArtFrames: TSpinEdit
          Left = 88
          Top = 586
          Width = 73
          Height = 22
          MaxValue = 255
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
      end
      object pnAnimationArtControlGroup: TPanel
        Left = 688
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pnExplosionList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 2
        object lbExplosionList: TListBox
          Tag = 5
          Left = 0
          Top = 16
          Width = 161
          Height = 564
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbExplosionListClick
          OnDblClick = btnExplosionSoundPlayClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
      object cbxExplosionSound: TComboBox
        Left = 208
        Top = 48
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
      end
      object edExplosionMuzzleFlashPattern: TEdit
        Left = 440
        Top = 128
        Width = 65
        Height = 21
        MaxLength = 7
        TabOrder = 4
      end
      object edExplosionMyIndex: TEdit
        Left = 456
        Top = 48
        Width = 49
        Height = 21
        MaxLength = 3
        TabOrder = 5
      end
      object edExplosionFlags: TEdit
        Left = 208
        Top = 128
        Width = 73
        Height = 21
        TabOrder = 6
        OnChange = edExplosionFlagsChange
      end
      object cbExplosionFlagEF_ADDITIVE_ALPHA: TCheckBox
        Tag = 512
        Left = 168
        Top = 80
        Width = 89
        Height = 17
        Caption = 'Additive alpha'
        TabOrder = 7
        OnClick = ExplosionFlagCheckboxChange
      end
      object cbExplosionFlagEF_SUBSTRACTIVE_ALPA: TCheckBox
        Tag = 2
        Left = 272
        Top = 80
        Width = 113
        Height = 17
        Caption = 'Substractive alpha'
        TabOrder = 8
        OnClick = ExplosionFlagCheckboxChange
      end
      object cbExplosionFlagEF_SEMI_TRANSPARENCY: TCheckBox
        Tag = 16
        Left = 392
        Top = 80
        Width = 113
        Height = 17
        Caption = 'Semi-transparency'
        TabOrder = 9
        OnClick = ExplosionFlagCheckboxChange
      end
      object cbExplosionFlagEF_RISE_UP: TCheckBox
        Tag = 1
        Left = 272
        Top = 104
        Width = 97
        Height = 17
        Caption = 'Rise up (smoke)'
        TabOrder = 10
        OnClick = ExplosionFlagCheckboxChange
      end
      object cbExplosionFlagEF_HOUSE_COLORED: TCheckBox
        Tag = 8
        Left = 392
        Top = 104
        Width = 97
        Height = 17
        Caption = 'House-colored'
        TabOrder = 11
        OnClick = ExplosionFlagCheckboxChange
      end
      object cbExplosionFlagEF_MUZZLE_FLASH: TCheckBox
        Tag = 1024
        Left = 168
        Top = 104
        Width = 97
        Height = 17
        Caption = 'Muzzle flash'
        TabOrder = 12
        OnClick = ExplosionFlagCheckboxChange
      end
      object edExplosionName: TEdit
        Left = 208
        Top = 16
        Width = 241
        Height = 21
        MaxLength = 499
        TabOrder = 13
      end
      object btnExplosionSoundPlay: TButton
        Left = 344
        Top = 48
        Width = 57
        Height = 21
        Caption = 'Play'
        TabOrder = 14
        OnClick = btnExplosionSoundPlayClick
      end
    end
    object PageArmour: TTabSheet
      Caption = 'Armour       '
      ImageIndex = 4
      object lblWarheadUsedBy: TLabel
        Left = 152
        Top = 620
        Width = 849
        Height = 13
        AutoSize = False
      end
      object lblArmourTypeUsedBy: TLabel
        Left = 151
        Top = 564
        Width = 850
        Height = 53
        AutoSize = False
        WordWrap = True
      end
      object sgArmourValues: TStringGrid
        Left = 152
        Top = 0
        Width = 849
        Height = 561
        ColCount = 15
        DefaultColWidth = 51
        DefaultRowHeight = 20
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goAlwaysShowEditor, goThumbTracking]
        TabOrder = 2
      end
      object pnArmourTypeList: TPanel
        Left = 0
        Top = 0
        Width = 146
        Height = 284
        BevelOuter = bvNone
        TabOrder = 0
        object lbArmourTypeList: TListBox
          Tag = 6
          Left = 0
          Top = 16
          Width = 146
          Height = 160
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbArmourTypeListClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
      object pnWarheadList: TPanel
        Left = 0
        Top = 287
        Width = 146
        Height = 350
        BevelOuter = bvNone
        TabOrder = 1
        object lbWarheadList: TListBox
          Tag = 7
          Left = 0
          Top = 16
          Width = 146
          Height = 225
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbWarheadListClick
          OnKeyDown = ItemControlGroupListKeyDown
        end
      end
    end
    object PageSpeed: TTabSheet
      Caption = 'Speed       '
      ImageIndex = 5
      object lblSpeedTypeUsedBy: TLabel
        Left = 0
        Top = 200
        Width = 481
        Height = 49
        AutoSize = False
        WordWrap = True
      end
      object sgSpeedValues: TStringGrid
        Left = 152
        Top = 0
        Width = 329
        Height = 193
        DefaultRowHeight = 20
        RowCount = 9
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goAlwaysShowEditor]
        TabOrder = 0
      end
      object pnSpeedTypeList: TPanel
        Left = 0
        Top = 0
        Width = 145
        Height = 193
        BevelOuter = bvNone
        TabOrder = 1
        object lblSpeedTypeList: TLabel
          Left = 0
          Top = 0
          Width = 59
          Height = 13
          Caption = 'Speed types'
        end
        object lbSpeedTypeList: TListBox
          Left = 0
          Top = 16
          Width = 145
          Height = 113
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbSpeedTypeListClick
        end
        object edSpeedTypeName: TEdit
          Left = 0
          Top = 136
          Width = 145
          Height = 21
          MaxLength = 31
          TabOrder = 1
        end
        object btnSpeedTypeRename: TButton
          Left = 24
          Top = 164
          Width = 97
          Height = 25
          Caption = 'Rename selected'
          TabOrder = 2
          OnClick = btnSpeedTypeRenameClick
        end
      end
    end
    object PageOther: TTabSheet
      Caption = 'Other         '
      ImageIndex = 9
      object lblOtherArtList: TLabel
        Left = 512
        Top = 0
        Width = 41
        Height = 13
        Caption = 'Other art'
      end
      object vleTemplatesOther: TValueListEditor
        Left = 0
        Top = 0
        Width = 425
        Height = 624
        DefaultColWidth = 210
        DefaultRowHeight = 19
        TabOrder = 0
        TitleCaptions.Strings = (
          'Byte'
          'Value')
        OnSelectCell = vleTemplatesOtherSelectCell
        OnTopLeftChanged = vleTemplatesOtherTopLeftChanged
        ColWidths = (
          210
          209)
      end
      object cbxTemplatesOtherSelect: TComboBox
        Tag = 1
        Left = 212
        Top = 21
        Width = 194
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        Visible = False
        OnChange = cbxTemplatesOtherSelectChange
      end
      object cbxTemplatesOtherUnitSelect: TComboBox
        Left = 216
        Top = 48
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Visible = False
      end
      object pnOtherArtControlGroup: TPanel
        Left = 688
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 3
      end
      object lbOtherArtList: TListBox
        Tag = 6
        Left = 512
        Top = 16
        Width = 161
        Height = 537
        ItemHeight = 13
        Items.Strings = (
          'Placement markers'
          'Repair icon'
          'Empty image'
          'Selection markers'
          'Health bars'
          'Dot markers'
          'Small numbers'
          'Credit numbers'
          'Shroud'
          'Crates'
          'Spice blooms'
          'Building/unit markers'
          'Rock craters'
          'Sand craters'
          'Stealth animation'
          'Stealth reveal mask')
        TabOrder = 4
        OnClick = lbOtherArtListClick
        OnKeyDown = ArtControlGroupListKeyDown
      end
    end
    object PageTechpos: TTabSheet
      Caption = 'Techpos   '
      ImageIndex = 10
      object imgTechposPreview: TImage
        Left = 120
        Top = 40
        Width = 224
        Height = 224
        OnMouseDown = imgTechposPreviewMouseDown
      end
      object sbTechposAtreides: TSpeedButton
        Left = 120
        Top = 8
        Width = 73
        Height = 22
        GroupIndex = 1
        Down = True
        Caption = 'Atreides'
        OnClick = TechposPreviewChange
      end
      object sbTechposHarkonnen: TSpeedButton
        Tag = 1
        Left = 196
        Top = 8
        Width = 73
        Height = 22
        GroupIndex = 1
        Caption = 'Harkonnen'
        OnClick = TechposPreviewChange
      end
      object sbTechposOrdos: TSpeedButton
        Tag = 2
        Left = 272
        Top = 8
        Width = 73
        Height = 22
        GroupIndex = 1
        Caption = 'Ordos'
        OnClick = TechposPreviewChange
      end
      object lblTechposNumUnits: TLabel
        Left = 360
        Top = 216
        Width = 132
        Height = 13
        Caption = 'Number of units to show: 10'
      end
      object rgTechposTechLevel: TRadioGroup
        Left = 0
        Top = 40
        Width = 105
        Height = 225
        Caption = 'Tech level'
        TabOrder = 0
        OnClick = rgTechposTechLevelClick
      end
      object sgTechposData: TStringGrid
        Left = 0
        Top = 280
        Width = 529
        Height = 225
        ColCount = 6
        DefaultColWidth = 40
        DefaultRowHeight = 19
        RowCount = 11
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 1
        OnSelectCell = sgTechposDataSelectCell
        OnSetEditText = sgTechposDataSetEditText
      end
      object cbxTechposUnitType: TComboBox
        Left = 124
        Top = 301
        Width = 135
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Visible = False
        OnChange = cbxTechposUnitTypeChange
      end
      object tbTechposNumUnits: TTrackBar
        Left = 352
        Top = 232
        Width = 177
        Height = 33
        Min = 1
        Position = 10
        TabOrder = 3
        OnChange = TechposPreviewChange
      end
    end
    object PageSounds: TTabSheet
      Caption = 'Sounds     '
      ImageIndex = 11
      object lblSamplesUib: TLabel
        Left = 0
        Top = 0
        Width = 91
        Height = 13
        Caption = 'Samples.uib entries'
      end
      object lblSoundRs: TLabel
        Left = 336
        Top = 0
        Width = 91
        Height = 13
        Caption = 'SOUND.RS entries'
      end
      object sgSamplesUib: TStringGrid
        Left = 0
        Top = 16
        Width = 320
        Height = 585
        ColCount = 3
        DefaultColWidth = 96
        DefaultRowHeight = 19
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goThumbTracking]
        TabOrder = 0
        OnDblClick = btnSoundRsPlayClick
        OnSelectCell = sgSamplesUibSelectCell
      end
      object sgSoundRs: TStringGrid
        Left = 336
        Top = 16
        Width = 320
        Height = 585
        ColCount = 3
        DefaultColWidth = 96
        DefaultRowHeight = 19
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goThumbTracking]
        TabOrder = 1
        OnDblClick = btnSoundRsPlayClick
        RowHeights = (
          19
          19
          19
          19
          19)
      end
      object btnSoundRsPlay: TButton
        Left = 336
        Top = 608
        Width = 72
        Height = 25
        Caption = 'Play sound'
        TabOrder = 2
        OnClick = btnSoundRsPlayClick
      end
      object btnSoundRsExport: TButton
        Left = 408
        Top = 608
        Width = 56
        Height = 25
        Caption = 'Export'
        TabOrder = 3
        OnClick = btnSoundRsExportClick
      end
      object brnSoundRsReplace: TButton
        Left = 464
        Top = 608
        Width = 56
        Height = 25
        Caption = 'Replace'
        TabOrder = 4
        OnClick = brnSoundRsReplaceClick
      end
      object btnSoundRsAddNew: TButton
        Left = 520
        Top = 608
        Width = 64
        Height = 25
        Caption = 'Add new'
        TabOrder = 5
        OnClick = btnSoundRsAddNewClick
      end
      object btnSoundRsRemoveLast: TButton
        Left = 584
        Top = 608
        Width = 72
        Height = 25
        Caption = 'Remove last'
        TabOrder = 6
        OnClick = btnSoundRsRemoveLastClick
      end
    end
  end
  object MainMenu: TMainMenu
    object Applychanges1: TMenuItem
      Caption = 'Apply changes (Ctrl+A)'
      ShortCut = 16449
      OnClick = Applychanges1Click
    end
    object Savetofiles1: TMenuItem
      Caption = 'Save to files (Ctrl+S)'
      ShortCut = 16467
      OnClick = Savetofiles1Click
    end
    object Saveandtest1: TMenuItem
      Caption = 'Save and test (F8)'
      ShortCut = 119
      OnClick = Saveandtest1Click
    end
    object Reloadfiles1: TMenuItem
      Caption = 'Reload files (Ctrl+R)'
      ShortCut = 16466
      OnClick = Reloadfiles1Click
    end
    object CopyfilestoModsfolder1: TMenuItem
      Caption = 'Copy files to Mods folder'
      OnClick = CopyfilestoModsfolder1Click
    end
  end
  object tmBuildingBuildingAnimation: TTimer
    Enabled = False
    Interval = 20
    OnTimer = tmBuildingBuildingAnimationTimer
    Left = 988
  end
  object tmBuildingBuildupArt: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmBuildingBuildupArtTimer
    Left = 956
  end
  object ItemExportDialog: TSaveDialog
    Left = 892
  end
  object ItemImportDialog: TOpenDialog
    Left = 924
  end
  object ArtImportDialog: TOpenDialog
    DefaultExt = 'R16'
    Filter = 'Dune 2000 graphics file (*.R16)|*.R16'
    Title = 'Import Art'
    Left = 860
  end
  object ArtExportDialog: TSaveDialog
    DefaultExt = 'R16'
    Filter = 'Dune 2000 graphics file (*.R16)|*.R16'
    Title = 'Export Art'
    Left = 828
  end
  object SoundImportDialog: TOpenDialog
    DefaultExt = 'WAV'
    Filter = 'Wave sound (*.WAV)|*.WAV'
    Title = 'Import Sound'
    Left = 796
  end
  object SoundExportDialog: TSaveDialog
    DefaultExt = 'WAV'
    Filter = 'Wave sound (*.WAV)|*.WAV'
    Title = 'Export Sound'
    Left = 764
  end
end
