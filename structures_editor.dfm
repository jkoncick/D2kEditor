object StructuresEditor: TStructuresEditor
  Left = 198
  Top = 12
  Width = 1024
  Height = 740
  Caption = 'StructuresEditor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 667
    Width = 1016
    Height = 19
    Panels = <
      item
        Width = 250
      end>
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 1016
    Height = 667
    ActivePage = PageBuildings
    Align = alClient
    TabOrder = 1
    object PageBuildings: TTabSheet
      Caption = 'Buildings   '
      object pnBuildingTypeList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 633
        BevelOuter = bvNone
        TabOrder = 0
        object lblBuildingTypeList: TLabel
          Left = 0
          Top = 0
          Width = 65
          Height = 13
          Caption = 'Building types'
        end
        object lbBuildingTypeList: TListBox
          Left = 0
          Top = 16
          Width = 161
          Height = 529
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object pnBuildingList: TPanel
        Left = 168
        Top = 0
        Width = 225
        Height = 633
        BevelOuter = bvNone
        TabOrder = 1
        object lblBuildingList: TLabel
          Left = 0
          Top = 0
          Width = 42
          Height = 13
          Caption = 'Buildings'
        end
        object lbBuildingList: TListBox
          Left = 0
          Top = 16
          Width = 225
          Height = 593
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbBuildingListClick
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
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object seBuildingTechLevelUpgrade1: TSpinEdit
          Left = 108
          Top = 32
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object seBuildingTechLevelUpgrade2: TSpinEdit
          Left = 168
          Top = 32
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object seBuildingTechLevelUpgrade3: TSpinEdit
          Left = 228
          Top = 32
          Width = 53
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
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
          Caption = 'Do not require concrete'
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
          Left = 152
          Top = 88
          Width = 71
          Height = 13
          Caption = 'Turning speed:'
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
        object lblBuildingFiringExplosion: TLabel
          Left = 160
          Top = 248
          Width = 100
          Height = 13
          Caption = 'Firing flash explosion:'
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
        object cbxBuildingFiringExplosion: TComboBox
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
        end
        object btnBuildingDirectionFrames8: TButton
          Left = 8
          Top = 320
          Width = 75
          Height = 17
          Caption = '8 directions'
          TabOrder = 15
        end
        object btnBuildingDirectionFrames32: TButton
          Left = 8
          Top = 336
          Width = 75
          Height = 17
          Caption = '32 directions'
          TabOrder = 16
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
      object gbBuildingOtherUnknown: TGroupBox
        Left = 400
        Top = 576
        Width = 289
        Height = 61
        Caption = 'Others and unknown'
        TabOrder = 8
        object lblBuildingUnknown93: TLabel
          Left = 64
          Top = 16
          Width = 39
          Height = 13
          Caption = 'Byte 93:'
        end
        object lblBuildingFlags: TLabel
          Left = 120
          Top = 16
          Width = 28
          Height = 13
          Caption = 'Flags:'
        end
        object lblBuildingUnknown8: TLabel
          Left = 8
          Top = 16
          Width = 30
          Height = 13
          Caption = 'Byte 8'
        end
        object seBuildingUnknown93: TSpinEdit
          Left = 64
          Top = 32
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object edBuildingFlags: TEdit
          Left = 120
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
          Caption = 'Autorepair'
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
        object seBuildingUnknown8: TSpinEdit
          Left = 8
          Top = 32
          Width = 49
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 4
          Value = 0
        end
      end
    end
    object PageUnits: TTabSheet
      Caption = 'Units         '
      ImageIndex = 1
    end
  end
  object MainMenu: TMainMenu
    object File1: TMenuItem
      Caption = 'File'
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
end
