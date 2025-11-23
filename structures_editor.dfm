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
  Constraints.MinHeight = 730
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
      object pnBuildingGroupList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lbBuildingGroupList: TListBox
          Tag = 2
          Left = 0
          Top = 16
          Width = 161
          Height = 537
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbBuildingGroupListClick
          OnKeyDown = IcgListKeyDown
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
          OnKeyDown = IcgListKeyDown
        end
      end
      object gbBuildingBasic: TGroupBox
        Left = 400
        Top = 0
        Width = 289
        Height = 137
        Caption = 'Basic'
        TabOrder = 2
        object lblBuildingOwnerHouse: TLabel
          Left = 80
          Top = 16
          Width = 34
          Height = 26
          Caption = 'Owner House:'
          WordWrap = True
        end
        object lblBuildingName: TLabel
          Left = 8
          Top = 80
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object lblBuildingGroup: TLabel
          Left = 8
          Top = 104
          Width = 32
          Height = 13
          Caption = 'Group:'
        end
        object imgBuildingIcon: TImage
          Left = 8
          Top = 16
          Width = 60
          Height = 47
          Hint = 'Left = import image, Right = export image, Middle = view palette'
          ParentShowHint = False
          ShowHint = True
          OnMouseDown = imgBuildingIconMouseDown
        end
        object clbBuildingOwnerHouse: TCheckListBox
          Left = 120
          Top = 16
          Width = 161
          Height = 57
          Hint = 
            'Determines which house gets which version of building, if there ' +
            'are more building types in same group.'
          OnClickCheck = RedrawBuildingPreview
          Columns = 2
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object edBuildingName: TEdit
          Left = 48
          Top = 80
          Width = 233
          Height = 21
          MaxLength = 499
          TabOrder = 1
        end
        object cbxBuildingGroup: TComboBox
          Left = 48
          Top = 104
          Width = 233
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
        object lblBuildingPrereq1BuildingGroup: TLabel
          Left = 8
          Top = 112
          Width = 67
          Height = 13
          Caption = 'Prerequisite 1:'
        end
        object lblBuildingPrereq2BuildingGroup: TLabel
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
        object lblBuildingPrereq1OwnerHouse: TLabel
          Left = 8
          Top = 136
          Width = 105
          Height = 13
          Caption = 'Owner house needed:'
        end
        object lblBuildingPrereq2OwnerHouse: TLabel
          Left = 8
          Top = 224
          Width = 105
          Height = 13
          Caption = 'Owner house needed:'
        end
        object seBuildingTechLevelBuild: TSpinEdit
          Left = 48
          Top = 32
          Width = 53
          Height = 22
          Hint = 
            'Minimum tech level needed for the building to become available. ' +
            '-1 = cannot be built.'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Value = 0
        end
        object seBuildingTechLevelUpgrade1: TSpinEdit
          Left = 108
          Top = 32
          Width = 53
          Height = 22
          Hint = 'Minimum tech level needed for 1st upgrade to become available.'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Value = 0
        end
        object seBuildingTechLevelUpgrade2: TSpinEdit
          Left = 168
          Top = 32
          Width = 53
          Height = 22
          Hint = 'Minimum tech level needed for 2nd upgrade to become available.'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          Value = 0
        end
        object seBuildingTechLevelUpgrade3: TSpinEdit
          Left = 228
          Top = 32
          Width = 53
          Height = 22
          Hint = 'Minimum tech level needed for 3rd upgrade to become available.'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
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
          Hint = 
            'Build speed, the higher value the faster.'#13'922 / value = in-game ' +
            'seconds it takes to build on basic speed.'
          MaxLength = 10
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
        end
        object edBuildingBuildSpeedUpgrade1: TEdit
          Left = 108
          Top = 80
          Width = 53
          Height = 21
          Hint = 
            'Build speed, the higher value the faster.'#13'922 / value = in-game ' +
            'seconds it takes to build on basic speed.'
          MaxLength = 10
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
        end
        object edBuildingBuildSpeedUpgrade2: TEdit
          Left = 168
          Top = 80
          Width = 53
          Height = 21
          Hint = 
            'Build speed, the higher value the faster.'#13'922 / value = in-game ' +
            'seconds it takes to build on basic speed.'
          MaxLength = 10
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
        end
        object edBuildingBuildSpeedUpgrade3: TEdit
          Left = 228
          Top = 80
          Width = 53
          Height = 21
          Hint = 
            'Build speed, the higher value the faster.'#13'922 / value = in-game ' +
            'seconds it takes to build on basic speed.'
          MaxLength = 10
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
        end
        object clbBuildingPrereq1OwnerHouse: TCheckListBox
          Left = 120
          Top = 136
          Width = 161
          Height = 57
          Hint = 
            'The prerequisite building'#39's owner house must be any of selected ' +
            'houses for this building to become available.'
          Columns = 2
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 12
        end
        object clbBuildingPrereq2OwnerHouse: TCheckListBox
          Left = 120
          Top = 224
          Width = 161
          Height = 57
          Hint = 
            'The prerequisite building'#39's owner house must be any of selected ' +
            'houses for this building to become available.'
          Columns = 2
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
        end
        object cbxBuildingPrereq1BuildingGroup: TComboBox
          Left = 80
          Top = 112
          Width = 201
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 14
        end
        object cbxBuildingPrereq2BuildingGroup: TComboBox
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
        Height = 145
        Caption = 'Space requirements'
        TabOrder = 4
        object imgBuildingTilesOccupiedAll: TImage
          Left = 8
          Top = 30
          Width = 63
          Height = 63
          OnMouseDown = imgBuildingTilesOccupiedAllMouseDown
        end
        object imgBuildingTilesOccupiedSolid: TImage
          Left = 88
          Top = 30
          Width = 63
          Height = 63
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
          Width = 44
          Height = 13
          Caption = 'Solid tiles'
        end
        object lblBuildingExitPoint1: TLabel
          Left = 160
          Top = 16
          Width = 86
          Height = 13
          Caption = 'Exit point 1  X / Y:'
        end
        object lblBuildingExitPoint2: TLabel
          Left = 160
          Top = 56
          Width = 86
          Height = 13
          Caption = 'Exit point 2  X / Y:'
        end
        object lblBuildingBuildRestriction: TLabel
          Left = 88
          Top = 100
          Width = 39
          Height = 13
          Caption = 'Restrict:'
        end
        object lblBuildingBuildMaxDistance: TLabel
          Left = 216
          Top = 100
          Width = 66
          Height = 13
          Caption = 'Max distance:'
        end
        object seBuildingExitPoint1X: TSpinEdit
          Left = 160
          Top = 32
          Width = 49
          Height = 22
          Hint = 
            'For production buildings, it is relative X-offset of tile the pr' +
            'oduced unit is spawned at.'#13'(can also serve different purpose on ' +
            'different building types)'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Value = 0
          OnChange = RedrawBuildingPreview
        end
        object seBuildingExitPoint1Y: TSpinEdit
          Left = 216
          Top = 32
          Width = 49
          Height = 22
          Hint = 
            'For production buildings, it is relative Y-offset of tile the pr' +
            'oduced unit is spawned at.'#13'For turrets, it is Y-offset (in pixel' +
            's) of bullet spawn position.'#13'(can also serve different purpose o' +
            'n different building types)'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Value = 0
        end
        object seBuildingExitPoint2X: TSpinEdit
          Left = 160
          Top = 72
          Width = 49
          Height = 22
          Hint = 
            'For production buildings, it is relative X-offset of tile the pr' +
            'oduced unit will move to after it'#39's spawned.'#13'(can also serve dif' +
            'ferent purpose on different building types)'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          Value = 0
        end
        object seBuildingExitPoint2Y: TSpinEdit
          Left = 216
          Top = 72
          Width = 49
          Height = 22
          Hint = 
            'For production buildings, it is relative Y-offset of tile the pr' +
            'oduced unit will move to after it'#39's spawned.'#13'(can also serve dif' +
            'ferent purpose on different building types)'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Value = 0
        end
        object cbBuildingFlagHAS_SKIRT: TCheckBox
          Tag = 2097152
          Left = 8
          Top = 100
          Width = 65
          Height = 17
          Hint = 
            'Building has skirt on tiles below it - only for 2 or 3 tiles wid' +
            'e buildings'
          Caption = 'Has skirt'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagNO_CONCRETE: TCheckBox
          Tag = 4194304
          Left = 8
          Top = 120
          Width = 81
          Height = 17
          Hint = 'Building is spawned without concrete and never decays'
          Caption = 'No concrete'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = BuildingFlagCheckboxChange
        end
        object cbxBuildingBuildRestriction: TComboBox
          Left = 132
          Top = 98
          Width = 77
          Height = 21
          Hint = 
            'Customizable building placement restrictions.'#13'Normal = Default b' +
            'ehavior (building can be built on tiles with build-on attribute)' +
            '.'#13'Or = In addition to tiles with build-on attribute, building ca' +
            'n be built also on tiles with specified terrain types.'#13'And = Bui' +
            'lding can be built on tiles having build-on attribute and being ' +
            'one of specified terrain types at same time.'#13'Terrain = Building ' +
            'can be built only on tiles with specified terrain types.'#13'Buildin' +
            'gs built on tiles without build-on attribute won'#39't provide build' +
            ' radius.'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          Items.Strings = (
            'Normal or'
            'Normal and'
            'Terrain')
        end
        object seBuildingBuildMaxDistance: TSpinEdit
          Left = 216
          Top = 116
          Width = 41
          Height = 22
          Hint = 
            'Maximum distance (in tiles) away from other buildings this build' +
            'ing is allowed to build'#13'(when set to 0 default distance of 2 app' +
            'lies)'
          MaxLength = 1
          MaxValue = 7
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Value = 0
        end
        object edBuildingBuildRestrictionTerrain: TEdit
          Left = 132
          Top = 120
          Width = 77
          Height = 21
          Hint = 
            'List of terrain types.'#13'It'#39's a binary number with 8 digits (0 or ' +
            '1), each digit is for one terrain type,'#13'leftmost is terrain 0 an' +
            'd rightmost is terrain 7.'
          MaxLength = 8
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
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
          Left = 111
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
          Left = 92
          Top = 40
          Width = 49
          Height = 13
          Caption = 'Healthbar:'
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
        object lblBuildingStorageCapacity: TLabel
          Left = 212
          Top = 40
          Width = 40
          Height = 13
          Caption = 'Storage:'
        end
        object edBuildingHitPoints: TEdit
          Left = 48
          Top = 16
          Width = 57
          Height = 21
          Hint = 'Number of Hit Points'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object cbxBuildingArmorType: TComboBox
          Left = 144
          Top = 16
          Width = 153
          Height = 21
          Hint = 'Armor type'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object edBuildingPowerConsumption: TEdit
          Left = 48
          Top = 40
          Width = 41
          Height = 21
          Hint = 
            'Positive value = power consumption, Negative value = power produ' +
            'ction'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object cbxBuildingHealthBarSize: TComboBox
          Left = 144
          Top = 40
          Width = 65
          Height = 21
          Hint = 'Health bar width'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
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
        end
        object cbBuildingFlagSELECT_REPAIR: TCheckBox
          Tag = 512
          Left = 8
          Top = 88
          Width = 97
          Height = 17
          Hint = 'Building can be selected, repaired and sabotaged'
          Caption = 'Select && Repair'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagCAN_CAPTURE: TCheckBox
          Tag = 1024
          Left = 112
          Top = 88
          Width = 81
          Height = 17
          Hint = 'Building can be captured'
          Caption = 'Can capture'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagDECAY: TCheckBox
          Tag = 32768
          Left = 8
          Top = 112
          Width = 97
          Height = 17
          Hint = 'Building decays even if it'#39's on concrete'
          Caption = 'Always decay'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagCANNOT_SELL: TCheckBox
          Tag = 16777216
          Left = 112
          Top = 112
          Width = 81
          Height = 17
          Hint = 'Building cannot be sold and captured or sabotaged'
          Caption = 'Cannot sell'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          OnClick = BuildingFlagCheckboxChange
        end
        object seBuildingSightRadius: TSpinEdit
          Left = 48
          Top = 64
          Width = 41
          Height = 22
          Hint = 'Radius of area the building reveals'
          MaxLength = 1
          MaxValue = 7
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          Value = 0
        end
        object cbBuildingScreenShake: TCheckBox
          Left = 200
          Top = 88
          Width = 97
          Height = 17
          Hint = 'Screen shakes when building is destroyed'
          Caption = 'Screen shake'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
        end
        object cbBuildingFlagREPAIRING: TCheckBox
          Tag = 1
          Left = 200
          Top = 112
          Width = 81
          Height = 17
          Hint = 'Building repairs when it'#39's built or spawned'
          Caption = 'Repairing'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
          OnClick = BuildingFlagCheckboxChange
        end
        object edBuildingStorageCapacity: TEdit
          Left = 256
          Top = 40
          Width = 41
          Height = 21
          Hint = 
            'Spice storage capacity, applicable only to Refinery and Silo.'#13'If' +
            ' set to 0, default values are used (2000 Refinery, 1500 Silo).'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 12
        end
      end
      object gbBuildingWeapons: TGroupBox
        Left = 696
        Top = 144
        Width = 305
        Height = 141
        Caption = 'Turret weapons'
        TabOrder = 6
        object lblBuildingBarrelRotationSpeed: TLabel
          Left = 192
          Top = 117
          Width = 57
          Height = 13
          Caption = 'Turn speed:'
        end
        object lblBuildingPrimaryWeaponBulkShots: TLabel
          Left = 8
          Top = 38
          Width = 52
          Height = 13
          Caption = 'Bulk shots:'
        end
        object lblBuildingPrimaryWeaponShortLongDelay: TLabel
          Left = 66
          Top = 38
          Width = 85
          Height = 13
          Caption = 'Short/Long delay:'
        end
        object lblBuildingSecondaryWeaponBulkShots: TLabel
          Left = 154
          Top = 38
          Width = 52
          Height = 13
          Caption = 'Bulk shots:'
        end
        object lblBuildingSecondaryWeaponShortLongDelay: TLabel
          Left = 212
          Top = 38
          Width = 85
          Height = 13
          Caption = 'Short/Long delay:'
        end
        object lblBuildingPrimaryWeaponOffsetAngle: TLabel
          Left = 66
          Top = 76
          Width = 76
          Height = 13
          Caption = 'Offset:     Angle:'
        end
        object lblBuildingSecondaryWeaponOffsetAngle: TLabel
          Left = 212
          Top = 76
          Width = 76
          Height = 13
          Caption = 'Offset:     Angle:'
        end
        object cbBuildingActLikeTurret: TCheckBox
          Left = 8
          Top = 116
          Width = 65
          Height = 17
          Hint = 
            'Building acts like turret and is automatically targeted by nearb' +
            'y enemy units or turrets'
          Caption = 'Is turret'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object cbBuildingRequireEnoughPower: TCheckBox
          Left = 80
          Top = 116
          Width = 97
          Height = 17
          Hint = 'Turret requires enough power (100% or more) to operate'
          Caption = 'Require power'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object cbxBuildingPrimaryWeapon: TComboBox
          Left = 8
          Top = 16
          Width = 143
          Height = 21
          Hint = 'Primary weapon type'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object cbxBuildingSecondaryWeapon: TComboBox
          Left = 154
          Top = 16
          Width = 143
          Height = 21
          Hint = 'Secondary weapon type'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object seBuildingBarrelRotationSpeed: TSpinEdit
          Left = 256
          Top = 116
          Width = 41
          Height = 22
          Hint = 'Turret turning speed (the higher value the slower)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Value = 0
        end
        object seBuildingPrimaryWeaponBulkShots: TSpinEdit
          Left = 8
          Top = 52
          Width = 41
          Height = 22
          Hint = 
            'Number of additional shots in a bulk (0 = single shot, 1 = two s' +
            'hots etc.)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Value = 0
        end
        object seBuildingPrimaryWeaponShortDelay: TSpinEdit
          Left = 66
          Top = 52
          Width = 41
          Height = 22
          Hint = 'Delay between shots within a bulk'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          Value = 0
        end
        object seBuildingPrimaryWeaponLongDelay: TSpinEdit
          Left = 110
          Top = 52
          Width = 41
          Height = 22
          Hint = 'Delay between bulks'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Value = 0
        end
        object seBuildingSecondaryWeaponBulkShots: TSpinEdit
          Left = 154
          Top = 52
          Width = 41
          Height = 22
          Hint = 
            'Number of additional shots in a bulk (0 = single shot, 1 = two s' +
            'hots etc.)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          Value = 0
        end
        object seBuildingSecondaryWeaponShortDelay: TSpinEdit
          Left = 212
          Top = 52
          Width = 41
          Height = 22
          Hint = 'Delay between shots within a bulk'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          Value = 0
        end
        object seBuildingSecondaryWeaponLongDelay: TSpinEdit
          Left = 256
          Top = 52
          Width = 41
          Height = 22
          Hint = 
            'Delay between bulks. If set to 0, the primary weapon'#39's value is ' +
            'used.'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
          Value = 0
        end
        object cbBuildingPrimaryWeaponDoubleShot: TCheckBox
          Left = 8
          Top = 79
          Width = 57
          Height = 17
          Hint = 'Turret fires two shots at once'
          Caption = 'Double'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
        end
        object seBuildingPrimaryWeaponShootOffset: TSpinEdit
          Left = 66
          Top = 90
          Width = 41
          Height = 22
          Hint = 'Distance of the bullet'#39's origin from center of unit'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 12
          Value = 0
        end
        object seBuildingPrimaryWeaponShootAngle: TSpinEdit
          Left = 110
          Top = 90
          Width = 41
          Height = 22
          Hint = 
            'Angle of splitting one shot from the other (usable for double sh' +
            'ot)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
          Value = 0
        end
        object cbBuildingSecondaryWeaponDoubleShot: TCheckBox
          Left = 154
          Top = 79
          Width = 55
          Height = 17
          Hint = 'Turret fires two shots at once'
          Caption = 'Double'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 14
        end
        object seBuildingSecondaryWeaponShootOffset: TSpinEdit
          Left = 212
          Top = 90
          Width = 41
          Height = 22
          Hint = 'Distance of the bullet'#39's origin from center of unit'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 15
          Value = 0
        end
        object seBuildingSecondaryWeaponShootAngle: TSpinEdit
          Left = 256
          Top = 90
          Width = 41
          Height = 22
          Hint = 
            'Angle of splitting one shot from the other (usable for double sh' +
            'ot)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 16
          Value = 0
        end
      end
      object gbBuildingVisuals: TGroupBox
        Left = 696
        Top = 292
        Width = 305
        Height = 345
        Caption = 'Visuals and animations'
        TabOrder = 7
        object imgBuildingImage: TImage
          Left = 168
          Top = 16
          Width = 128
          Height = 160
        end
        object lblBuildingBarrelArt: TLabel
          Left = 8
          Top = 40
          Width = 45
          Height = 13
          Caption = 'Barrel art:'
        end
        object lblBuildingArtWidth: TLabel
          Left = 8
          Top = 80
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object lblBuildingArtHeight: TLabel
          Left = 84
          Top = 80
          Width = 34
          Height = 13
          Caption = 'Height:'
        end
        object lblBuildingBuildingAnimation: TLabel
          Left = 8
          Top = 152
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
          Top = 192
          Width = 86
          Height = 13
          Caption = 'Buildup animation:'
        end
        object lblBuildingBuildupFramesToShow: TLabel
          Left = 168
          Top = 204
          Width = 70
          Height = 26
          Caption = 'Frames before shown:'
          WordWrap = True
        end
        object lblBuildingDeathExplosion: TLabel
          Left = 8
          Top = 232
          Width = 104
          Height = 13
          Caption = 'Destruction explosion:'
        end
        object lblBuildingMuzzleFlashExplosion: TLabel
          Left = 160
          Top = 232
          Width = 108
          Height = 13
          Caption = 'Muzzle flash explosion:'
        end
        object lblBuildingDirectionFrames: TLabel
          Left = 8
          Top = 272
          Width = 79
          Height = 13
          Caption = 'Direction frames:'
        end
        object cbxBuildingBuildingArt: TComboBox
          Left = 8
          Top = 16
          Width = 153
          Height = 21
          Hint = 'Building art'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnChange = RedrawBuildingPreview
        end
        object cbxBuildingBarrelArt: TComboBox
          Left = 8
          Top = 56
          Width = 153
          Height = 21
          Hint = 'Barrel art. If present, building can be given attack orders.'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnChange = RedrawBuildingPreview
        end
        object edBuildingArtWidth: TEdit
          Left = 40
          Top = 80
          Width = 41
          Height = 21
          Hint = 
            'Building width in pixels. This determines kind of skirt (only wi' +
            'dths 64 or 96 can have skirt).'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnChange = RedrawBuildingPreview
        end
        object edBuildingArtHeight: TEdit
          Left = 120
          Top = 80
          Width = 41
          Height = 21
          Hint = 'Building height in pixels'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnChange = RedrawBuildingPreview
        end
        object cbxBuildingBuildingAnimation: TComboBox
          Left = 8
          Top = 168
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
        end
        object cbBuildingFlagHAS_ANIMATION: TCheckBox
          Tag = 64
          Left = 8
          Top = 104
          Width = 137
          Height = 17
          Hint = 'Building has animation'
          Caption = 'Has building animation'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = BuildingFlagCheckboxChange
        end
        object cbBuildingFlagANIM_PERMANENT: TCheckBox
          Tag = 16
          Left = 8
          Top = 128
          Width = 89
          Height = 17
          Hint = 'Animation plays permanently'
          Caption = 'Is permanent'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = BuildingFlagCheckboxChange
        end
        object seBuildingAnimationSpeed: TSpinEdit
          Left = 232
          Top = 184
          Width = 65
          Height = 22
          Hint = 'Animation speed - number of ticks between frames'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Value = 0
        end
        object cbxBuildingBuildupArt: TComboBox
          Left = 8
          Top = 208
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 8
        end
        object seBuildingBuildupFramesToShow: TSpinEdit
          Left = 232
          Top = 208
          Width = 65
          Height = 22
          Hint = 
            'When building is built and buildup animation is played, this is ' +
            #13'number of frames after which the building itself is rendered'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          Value = 0
        end
        object cbxBuildingDeathExplosion: TComboBox
          Left = 8
          Top = 248
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 10
        end
        object cbxBuildingMuzzleFlashExplosion: TComboBox
          Left = 160
          Top = 248
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 11
        end
        object sgBuildingDirectionFrames: TStringGrid
          Left = 94
          Top = 274
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
          Top = 128
          Width = 57
          Height = 17
          Hint = 'Animation is rendered transparently'
          Caption = 'Alpha'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
          OnClick = BuildingFlagCheckboxChange
        end
        object btnBuildingDirectionFrames0: TButton
          Left = 8
          Top = 288
          Width = 75
          Height = 17
          Caption = 'No directions'
          TabOrder = 14
          OnClick = btnBuildingDirectionFramesClick
        end
        object btnBuildingDirectionFrames8: TButton
          Tag = 8
          Left = 8
          Top = 304
          Width = 75
          Height = 17
          Caption = '8 directions'
          TabOrder = 15
          OnClick = btnBuildingDirectionFramesClick
        end
        object btnBuildingDirectionFrames32: TButton
          Tag = 32
          Left = 8
          Top = 320
          Width = 75
          Height = 17
          Caption = '32 directions'
          TabOrder = 16
          OnClick = btnBuildingDirectionFramesClick
        end
        object btnBuildingBuildingAnimationPlay: TButton
          Left = 122
          Top = 168
          Width = 39
          Height = 21
          Caption = 'Play'
          TabOrder = 17
          OnClick = btnBuildingBuildingAnimationPlayClick
        end
        object btnBuildingBuildupArtPlay: TButton
          Left = 122
          Top = 208
          Width = 39
          Height = 21
          Caption = 'Play'
          TabOrder = 18
          OnClick = btnBuildingBuildupArtPlayClick
        end
      end
      object gbBuildingOther: TGroupBox
        Left = 400
        Top = 592
        Width = 289
        Height = 45
        Caption = 'Others'
        TabOrder = 8
        object lblBuildingSellPriority: TLabel
          Left = 108
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
          Left = 164
          Top = 16
          Width = 49
          Height = 22
          Hint = 
            'The priority at which AI decides to sell this building when it'#39's' +
            #13'on low power. The building with highest value is sold first.'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Value = 0
        end
        object edBuildingFlags: TEdit
          Left = 40
          Top = 16
          Width = 65
          Height = 21
          MaxLength = 8
          TabOrder = 1
          OnChange = edBuildingFlagsChange
        end
        object cbBuildingFlagUNKNOWN9: TCheckBox
          Tag = 256
          Left = 218
          Top = 14
          Width = 65
          Height = 17
          Caption = 'Unknown'
          TabOrder = 2
          OnClick = BuildingFlagCheckboxChange
        end
      end
    end
    object PageUnits: TTabSheet
      Caption = 'Units         '
      ImageIndex = 1
      object pnUnitGroupList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 637
        BevelOuter = bvNone
        TabOrder = 0
        object lbUnitGroupList: TListBox
          Tag = 3
          Left = 0
          Top = 16
          Width = 161
          Height = 537
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbUnitGroupListClick
          OnKeyDown = IcgListKeyDown
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
          OnKeyDown = IcgListKeyDown
        end
      end
      object gbUnitBasic: TGroupBox
        Left = 400
        Top = 0
        Width = 289
        Height = 137
        Caption = 'Basic'
        TabOrder = 2
        object lblUnitOwnerHouse: TLabel
          Left = 80
          Top = 16
          Width = 34
          Height = 26
          Caption = 'Owner house:'
          WordWrap = True
        end
        object lblUnitName: TLabel
          Left = 8
          Top = 80
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object lblUnitGroup: TLabel
          Left = 8
          Top = 104
          Width = 32
          Height = 13
          Caption = 'Group:'
        end
        object imgUnitIcon: TImage
          Left = 8
          Top = 16
          Width = 60
          Height = 47
          Hint = 'Left = import image, Right = export image, Middle = view palette'
          ParentShowHint = False
          ShowHint = True
          OnMouseDown = imgUnitIconMouseDown
        end
        object clbUnitOwnerHouse: TCheckListBox
          Left = 120
          Top = 16
          Width = 161
          Height = 57
          Hint = 
            'Determines which house gets which version of unit, if there are ' +
            'more unit types in same group.'
          OnClickCheck = RedrawUnitPreview
          Columns = 2
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object edUnitName: TEdit
          Left = 48
          Top = 80
          Width = 233
          Height = 21
          MaxLength = 499
          TabOrder = 1
          OnChange = RedrawUnitPreview
        end
        object cbxUnitGroup: TComboBox
          Left = 48
          Top = 104
          Width = 233
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
        object lblUnitPrereq1BuildingGroup: TLabel
          Left = 8
          Top = 56
          Width = 67
          Height = 13
          Caption = 'Prerequisite 1:'
        end
        object lblUnitPrereq2BuildingGroup: TLabel
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
        object lblUnitPrereq1OwnerHouse: TLabel
          Left = 8
          Top = 80
          Width = 105
          Height = 13
          Caption = 'Owner house needed:'
        end
        object seUnitTechLevel: TSpinEdit
          Left = 40
          Top = 24
          Width = 53
          Height = 22
          Hint = 
            'Minimum tech level needed for the unit to become available. -1 =' +
            ' cannot be built.'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
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
          Hint = 
            'Build speed, the higher value the faster.'#13'922 / value = in-game ' +
            'seconds it takes to build on basic speed.'
          MaxLength = 10
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object clbUnitPrereq1OwnerHouse: TCheckListBox
          Left = 120
          Top = 80
          Width = 161
          Height = 57
          Hint = 
            'The prerequisite building'#39's owner house must be any of selected ' +
            'houses for this unit to become available.'
          Columns = 2
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object cbxUnitPrereq1BuildingGroup: TComboBox
          Left = 80
          Top = 56
          Width = 201
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
        end
        object cbxUnitPrereq2BuildingGroup: TComboBox
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
        object lblUnitVoicePriority: TLabel
          Left = 8
          Top = 224
          Width = 63
          Height = 13
          Caption = 'Voice priority:'
        end
        object lblUnitCustomDeathSound: TLabel
          Left = 8
          Top = 252
          Width = 65
          Height = 26
          Caption = 'Custom death'#13'sounds:'
        end
        object lblUnitCustomDeathSoundNumber: TLabel
          Left = 204
          Top = 256
          Width = 25
          Height = 13
          Caption = 'Num:'
        end
        object seUnitVoicePriority: TSpinEdit
          Left = 80
          Top = 224
          Width = 61
          Height = 22
          Hint = 
            'Determines which voice is played if different unit types are sel' +
            'ected'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Value = 0
        end
        object btnUnitVoicesExport: TButton
          Tag = 8
          Left = 152
          Top = 224
          Width = 64
          Height = 21
          Hint = 'Export Voices section into file'
          Caption = 'Export'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = IcgExportClick
        end
        object btnUnitVoicesImport: TButton
          Tag = 8
          Left = 216
          Top = 224
          Width = 64
          Height = 21
          Hint = 'Import Voices section from file'
          Caption = 'Import'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = IcgImportClick
        end
        object sgUnitVoices: TStringGrid
          Left = 8
          Top = 16
          Width = 273
          Height = 203
          ColCount = 3
          DefaultColWidth = 88
          DefaultRowHeight = 19
          RowCount = 10
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
          TabOrder = 3
          OnSelectCell = sgUnitVoicesSelectCell
        end
        object cbxUnitVoice: TComboBox
          Left = 37
          Top = 37
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
          OnChange = cbxUnitVoiceChange
        end
        object cbxUnitCustomDeathSound: TComboBox
          Left = 80
          Top = 256
          Width = 121
          Height = 21
          Hint = 
            'Customized infantry death sound. This is the first sound.'#13'If num' +
            'ber is 0, this is ignored and vanilla sounds are used.'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
        end
        object seUnitCustomDeathSoundNumber: TSpinEdit
          Left = 232
          Top = 256
          Width = 49
          Height = 22
          Hint = 
            'Number of custom death sounds. If value is 0, vanilla sounds are' +
            ' used.'
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
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
          Left = 111
          Top = 16
          Width = 30
          Height = 13
          Caption = 'Armor:'
        end
        object lblUnitHealthBarSize: TLabel
          Left = 92
          Top = 40
          Width = 49
          Height = 13
          Caption = 'Healthbar:'
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
        object lblUnitStorageCapacity: TLabel
          Left = 212
          Top = 40
          Width = 40
          Height = 13
          Caption = 'Storage:'
        end
        object edUnitHitPoints: TEdit
          Left = 48
          Top = 16
          Width = 57
          Height = 21
          Hint = 'Number of Hit Points'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object cbxUnitArmorType: TComboBox
          Left = 144
          Top = 16
          Width = 153
          Height = 21
          Hint = 'Armor type'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object cbxUnitHealthBarSize: TComboBox
          Left = 144
          Top = 40
          Width = 65
          Height = 21
          Hint = 'Health bar width'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
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
        end
        object cbUnitIsInfantry: TCheckBox
          Left = 8
          Top = 64
          Width = 73
          Height = 17
          Caption = 'Is Infantry'
          TabOrder = 4
        end
        object cbUnitFlagUF_STEALTH: TCheckBox
          Tag = 16
          Left = 184
          Top = 88
          Width = 57
          Height = 17
          Caption = 'Stealth'
          TabOrder = 5
          OnClick = UnitFlagCheckboxChange
        end
        object cbUnitFlagUF_SELFHEALING: TCheckBox
          Tag = 8388608
          Left = 96
          Top = 88
          Width = 73
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
          Hint = 'Radius of area the unit reveals'
          MaxLength = 1
          MaxValue = 7
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Value = 0
        end
        object cbUnitFlagUF_NO_AI: TCheckBox
          Tag = 2048
          Left = 248
          Top = 88
          Width = 49
          Height = 17
          Hint = 
            'The unit won'#39't be assigned any group by AI and will not be given' +
            ' any commands'
          Caption = 'No AI'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          OnClick = UnitFlagCheckboxChange
        end
        object cbUnitNotEdible: TCheckBox
          Left = 8
          Top = 88
          Width = 73
          Height = 17
          Hint = 
            'Unit cannot be eaten by sandworm and is not targeted by sandworm' +
            '.'
          Caption = 'Not edible'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
        end
        object edUnitStorageCapacity: TEdit
          Left = 256
          Top = 40
          Width = 41
          Height = 21
          Hint = 
            'Storage capacity, applicable only to Harvester and Saboteur.'#13'If ' +
            'set to 0, default values are used (7 for Harvester, 160 for Sabo' +
            'teur).'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
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
          Left = 96
          Top = 16
          Width = 27
          Height = 13
          Caption = 'Type:'
        end
        object lblUnitUnitRotationSpeed: TLabel
          Left = 8
          Top = 40
          Width = 57
          Height = 13
          Caption = 'Turn speed:'
        end
        object lblUnitMovementRestriction: TLabel
          Left = 220
          Top = 8
          Width = 53
          Height = 13
          Caption = 'Restriction:'
        end
        object lblUnitCanCrushInfantry: TLabel
          Left = 146
          Top = 55
          Width = 60
          Height = 13
          Caption = 'Uncrushable'
        end
        object edUnitSpeed: TEdit
          Left = 48
          Top = 16
          Width = 41
          Height = 21
          Hint = 'Speed in km/h'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object cbxUnitSpeedType: TComboBox
          Left = 128
          Top = 16
          Width = 85
          Height = 21
          Hint = 
            'Type of drive. This affects unit'#39's speed on different terrain ty' +
            'pes.'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object seUnitUnitRotationSpeed: TSpinEdit
          Left = 72
          Top = 40
          Width = 49
          Height = 22
          Hint = 'Delay in ticks the unit turns by one frame'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          Value = 0
        end
        object cbUnitCanCrushInfantry: TCheckBox
          Left = 128
          Top = 40
          Width = 81
          Height = 17
          Hint = 'Vehicles can crush infantry / Infantry is uncrushable'
          Caption = 'Can crush /'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object cbxUnitMovementRestriction: TComboBox
          Left = 220
          Top = 24
          Width = 77
          Height = 21
          Hint = 
            'Customizable unit movement restrictions.'#13'Normal = Default behavi' +
            'or (unit can move on tiles with walk-on or drive-on attribute re' +
            'spectively).'#13'Sandy = Unit can move on tiles with sandy attribute' +
            ' (like a sandworm).'#13'Or = In addition to tiles where unit can nor' +
            'mally move, it will also be able to move on tiles with specified' +
            ' terrain types.'#13'And = Unit can move on tiles where it can normal' +
            'ly move, which must also be one of specified terrain types.'#13'Terr' +
            'ain = Unit can move only on tiles with specified terrain types.'#13 +
            'Units with other than "Normal or" restriction won'#39't be picked up' +
            ' by carryalls.'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Items.Strings = (
            'Normal or'
            'Normal and'
            'Sandy or'
            'Sandy and'
            'Terrain')
        end
        object edUnitMovementRestrictionTerrain: TEdit
          Left = 220
          Top = 46
          Width = 77
          Height = 21
          Hint = 
            'List of terrain types.'#13'It'#39's a binary number with 8 digits (0 or ' +
            '1), each digit is for one terrain type,'#13'leftmost is terrain 0 an' +
            'd rightmost is terrain 7.'
          MaxLength = 8
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
        end
      end
      object gbUnitWeapons: TGroupBox
        Left = 696
        Top = 200
        Width = 305
        Height = 145
        Caption = 'Weapons'
        TabOrder = 7
        object lblUnitBarrelRotationSpeed: TLabel
          Left = 168
          Top = 117
          Width = 83
          Height = 13
          Caption = 'Barrel turn speed:'
        end
        object lblUnitPrimaryWeaponOffsetAngle: TLabel
          Left = 66
          Top = 76
          Width = 76
          Height = 13
          Caption = 'Offset:     Angle:'
        end
        object lblUnitPrimaryWeaponBulkShots: TLabel
          Left = 8
          Top = 38
          Width = 52
          Height = 13
          Caption = 'Bulk shots:'
        end
        object lblUnitPrimaryWeaponShortLongDelay: TLabel
          Left = 66
          Top = 38
          Width = 85
          Height = 13
          Caption = 'Short/Long delay:'
        end
        object lblUnitSecondaryWeaponShortLongDelay: TLabel
          Left = 212
          Top = 38
          Width = 85
          Height = 13
          Caption = 'Short/Long delay:'
        end
        object lblUnitSecondaryWeaponBulkShots: TLabel
          Left = 154
          Top = 38
          Width = 52
          Height = 13
          Caption = 'Bulk shots:'
        end
        object lblUnitSecondaryWeaponOffsetAngle: TLabel
          Left = 212
          Top = 76
          Width = 76
          Height = 13
          Caption = 'Offset:     Angle:'
        end
        object cbUnitHasBarrel: TCheckBox
          Left = 8
          Top = 116
          Width = 73
          Height = 17
          Hint = 'Unit has barrel'
          Caption = 'Has barrel'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object cbUnitFlagUF_FIXED_BARREL: TCheckBox
          Tag = 32768
          Left = 84
          Top = 116
          Width = 73
          Height = 17
          Hint = 
            'Barrel is fixed - always is turned in the same direction as the ' +
            'unit itself'
          Caption = 'Fixed barrel'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = UnitFlagCheckboxChange
        end
        object cbxUnitPrimaryWeapon: TComboBox
          Left = 8
          Top = 16
          Width = 143
          Height = 21
          Hint = 'Primary weapon type'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object cbxUnitSecondaryWeapon: TComboBox
          Left = 154
          Top = 16
          Width = 143
          Height = 21
          Hint = 'Secondary weapon type'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object seUnitPrimaryWeaponLongDelay: TSpinEdit
          Left = 110
          Top = 52
          Width = 41
          Height = 22
          Hint = 'Delay between bulks'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Value = 0
        end
        object seUnitBarrelRotationSpeed: TSpinEdit
          Left = 256
          Top = 116
          Width = 41
          Height = 22
          Hint = 'Delay in ticks the barrel turns by one frame'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Value = 0
        end
        object seUnitPrimaryWeaponShootOffset: TSpinEdit
          Left = 66
          Top = 90
          Width = 41
          Height = 22
          Hint = 'Distance of the bullet'#39's origin from center of unit'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          Value = 0
        end
        object seUnitPrimaryWeaponBulkShots: TSpinEdit
          Left = 8
          Top = 52
          Width = 41
          Height = 22
          Hint = 
            'Number of additional shots in a bulk (0 = single shot, 1 = two s' +
            'hots etc.)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Value = 0
        end
        object seUnitPrimaryWeaponShortDelay: TSpinEdit
          Left = 66
          Top = 52
          Width = 41
          Height = 22
          Hint = 'Delay between shots within a bulk'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          Value = 0
        end
        object seUnitSecondaryWeaponBulkShots: TSpinEdit
          Left = 154
          Top = 52
          Width = 41
          Height = 22
          Hint = 
            'Number of additional shots in a bulk (0 = single shot, 1 = two s' +
            'hots etc.)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          Value = 0
        end
        object seUnitSecondaryWeaponShortDelay: TSpinEdit
          Left = 212
          Top = 52
          Width = 41
          Height = 22
          Hint = 'Delay between shots within a bulk'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
          Value = 0
        end
        object seUnitSecondaryWeaponLongDelay: TSpinEdit
          Left = 256
          Top = 52
          Width = 41
          Height = 22
          Hint = 
            'Delay between bulks. If set to 0, the primary weapon'#39's value is ' +
            'used.'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
          Value = 0
        end
        object cbUnitPrimaryWeaponDoubleShot: TCheckBox
          Left = 8
          Top = 79
          Width = 57
          Height = 17
          Hint = 'Unit fires two shots at once'
          Caption = 'Double'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 12
        end
        object seUnitPrimaryWeaponShootAngle: TSpinEdit
          Left = 110
          Top = 90
          Width = 41
          Height = 22
          Hint = 
            'Angle of splitting one shot from the other (usable for double sh' +
            'ot)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
          Value = 0
        end
        object seUnitSecondaryWeaponShootOffset: TSpinEdit
          Left = 212
          Top = 90
          Width = 41
          Height = 22
          Hint = 'Distance of the bullet'#39's origin from center of unit'
          MaxLength = 3
          MaxValue = 127
          MinValue = -128
          ParentShowHint = False
          ShowHint = True
          TabOrder = 14
          Value = 0
        end
        object seUnitSecondaryWeaponShootAngle: TSpinEdit
          Left = 256
          Top = 90
          Width = 41
          Height = 22
          Hint = 
            'Angle of splitting one shot from the other (usable for double sh' +
            'ot)'
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 15
          Value = 0
        end
        object cbUnitSecondaryWeaponDoubleShot: TCheckBox
          Left = 154
          Top = 79
          Width = 55
          Height = 17
          Hint = 'Unit fires two shots at once'
          Caption = 'Double'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 16
        end
        object cbUnitPrimaryWeaponNoAutoAttack: TCheckBox
          Left = 8
          Top = 97
          Width = 57
          Height = 17
          Hint = 
            'Don'#39't automatically attack nearby enemy units/buildings with thi' +
            's weapon'
          Caption = 'No auto'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 17
        end
        object cbUnitSecondaryWeaponNoAutoAttack: TCheckBox
          Left = 154
          Top = 97
          Width = 57
          Height = 17
          Hint = 
            'Don'#39't automatically attack nearby enemy units/buildings with thi' +
            's weapon'
          Caption = 'No auto'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 18
        end
      end
      object gbUnitVisuals: TGroupBox
        Left = 696
        Top = 352
        Width = 305
        Height = 217
        Caption = 'Visuals and animations'
        TabOrder = 8
        object imgUnitImage: TImage
          Left = 192
          Top = 12
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
          Top = 96
          Width = 104
          Height = 13
          Caption = 'Destruction explosion:'
        end
        object lblUnitMuzzleFlashExplosion: TLabel
          Left = 160
          Top = 96
          Width = 108
          Height = 13
          Caption = 'Muzzle flash explosion:'
        end
        object lblUnitDirectionFrames: TLabel
          Left = 8
          Top = 136
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
          Top = 112
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
        object cbxUnitMuzzleFlashExplosion: TComboBox
          Left = 160
          Top = 112
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
        end
        object sgUnitDirectionFrames: TStringGrid
          Left = 94
          Top = 138
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
          Top = 152
          Width = 75
          Height = 17
          Caption = 'No directions'
          TabOrder = 5
          OnClick = btnUnitDirectionFramesClick
        end
        object btnUnitDirectionFrames8: TButton
          Tag = 8
          Left = 8
          Top = 168
          Width = 75
          Height = 17
          Caption = '8 directions'
          TabOrder = 6
          OnClick = btnUnitDirectionFramesClick
        end
        object btnUnitDirectionFrames32: TButton
          Tag = 32
          Left = 8
          Top = 184
          Width = 75
          Height = 17
          Caption = '32 directions'
          TabOrder = 7
          OnClick = btnUnitDirectionFramesClick
        end
      end
      object gbUnitOther: TGroupBox
        Left = 696
        Top = 576
        Width = 305
        Height = 61
        Caption = 'Others'
        TabOrder = 9
        object lblUnitUnknown52: TLabel
          Left = 224
          Top = 34
          Width = 22
          Height = 13
          Caption = 'B52:'
        end
        object lblUnitFlags: TLabel
          Left = 8
          Top = 16
          Width = 28
          Height = 13
          Caption = 'Flags:'
        end
        object lblUnitUnknown46: TLabel
          Left = 224
          Top = 10
          Width = 22
          Height = 13
          Caption = 'B46:'
        end
        object seUnitUnknown52: TSpinEdit
          Left = 248
          Top = 34
          Width = 49
          Height = 22
          MaxLength = 3
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object edUnitFlags: TEdit
          Left = 8
          Top = 32
          Width = 73
          Height = 21
          TabOrder = 1
          OnChange = edUnitFlagsChange
        end
        object seUnitUnknown46: TSpinEdit
          Left = 248
          Top = 10
          Width = 49
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object cbxUnitUpgradeTargetType: TComboBox
          Left = 88
          Top = 32
          Width = 129
          Height = 21
          Hint = 
            'The type unit can be changed to by picking up a "Change type" po' +
            'werup crate'
          Style = csDropDownList
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object cbUnitUpgradeAllowed: TCheckBox
          Left = 88
          Top = 14
          Width = 105
          Height = 17
          Hint = 
            'If checked, unit'#39's type can be changed to the selected type by p' +
            'icking up a "Change type" powerup crate'
          Caption = 'Unit upgrade type:'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
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
          OnKeyDown = AcgArtListKeyDown
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
          OnKeyDown = AcgArtListKeyDown
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
          OnKeyDown = AcgArtListKeyDown
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
        Top = 432
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
          OnKeyDown = AcgArtListKeyDown
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
          OnKeyDown = IcgListKeyDown
        end
      end
      object gbWeaponProperties: TGroupBox
        Left = 168
        Top = 16
        Width = 337
        Height = 145
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
          Left = 128
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
        object lblWeaponInaccuracy: TLabel
          Left = 128
          Top = 72
          Width = 56
          Height = 13
          Caption = 'Inaccuracy:'
        end
        object lblWeaponBehavior: TLabel
          Left = 8
          Top = 96
          Width = 45
          Height = 13
          Caption = 'Behavior:'
        end
        object edWeaponDamage: TEdit
          Left = 56
          Top = 48
          Width = 65
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
          Width = 65
          Height = 21
          Hint = 'Range in pixels, 32 = 1 tile'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object cbWeaponAntiAircraft: TCheckBox
          Left = 248
          Top = 72
          Width = 81
          Height = 17
          Caption = 'Anti-aircraft'
          TabOrder = 3
        end
        object cbWeaponFlagWF_BLOCKED_BY_WALL: TCheckBox
          Tag = 4096
          Left = 204
          Top = 96
          Width = 97
          Height = 17
          Caption = 'Blocked by wall'
          TabOrder = 4
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_DEVIATOR: TCheckBox
          Tag = 64
          Left = 176
          Top = 120
          Width = 81
          Height = 17
          Caption = 'Deviator'
          TabOrder = 5
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_SONIC: TCheckBox
          Tag = 2048
          Left = 256
          Top = 120
          Width = 49
          Height = 17
          Caption = 'Sonic'
          TabOrder = 6
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_FALLING: TCheckBox
          Tag = 256
          Left = 8
          Top = 120
          Width = 57
          Height = 17
          Hint = 'Usable for flying units and debris made by destructed buildings.'
          Caption = 'Falling'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_DEBRIS: TCheckBox
          Tag = 2
          Left = 80
          Top = 120
          Width = 81
          Height = 17
          Hint = 
            'Shadow is drawn below projectile. For some weapon behaviors it i' +
            's set forcefully in game.'
          Caption = 'Has shadow'
          ParentShowHint = False
          ShowHint = True
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
        object seWeaponInaccuracy: TSpinEdit
          Left = 192
          Top = 71
          Width = 49
          Height = 22
          Hint = 
            'Weapon'#39's inaccuracy increases with distance.'#13'Value of 0 is vanil' +
            'la behavior and is same as 16 (4 pixels deviation per tile of di' +
            'stance).'#13'1 is almost perfectly accurate, the higher value the le' +
            'ss accurate.'
          MaxValue = 255
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
          Value = 0
        end
        object cbxWeaponBehavior: TComboBox
          Left = 56
          Top = 96
          Width = 137
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 11
        end
      end
      object dbWeaponMovement: TGroupBox
        Left = 168
        Top = 168
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
          Hint = 
            'Speed the projectile moves at.'#13'For Railgun behavior, it determin' +
            'es the density of trail explosions and damege dealt.'
          ParentShowHint = False
          ShowHint = True
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
        Top = 232
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
          Hint = 'Projectile is drawn transparently'
          Caption = 'Projectile art alpha'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = WeaponFlagCheckboxChange
        end
        object cbWeaponFlagWF_ANIM_PROJECTILE: TCheckBox
          Tag = 128
          Left = 224
          Top = 72
          Width = 109
          Height = 25
          Hint = 
            'Projectile is not drawn by its direction, but is animated instea' +
            'd'
          Caption = 'Animated (rotating) projectile'
          ParentShowHint = False
          ShowHint = True
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
        Top = 368
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
          OnKeyDown = AcgArtListKeyDown
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
          OnKeyDown = IcgListKeyDown
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
        Hint = 
          'Binary number, muzzle flash lasts for max 7 ticks, 1 = drawn, 0 ' +
          '= not drawn'
        MaxLength = 7
        ParentShowHint = False
        ShowHint = True
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
          OnKeyDown = IcgListKeyDown
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
          OnKeyDown = IcgListKeyDown
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
      object lblGroupIDs: TLabel
        Left = 0
        Top = 0
        Width = 48
        Height = 13
        Caption = 'Group IDs'
      end
      object vleGroupIDs: TValueListEditor
        Left = 0
        Top = 16
        Width = 425
        Height = 624
        DefaultColWidth = 210
        DefaultRowHeight = 19
        TabOrder = 0
        TitleCaptions.Strings = (
          'Byte'
          'Value')
        OnSelectCell = vleGroupIDsSelectCell
        OnTopLeftChanged = vleGroupIDsTopLeftChanged
        ColWidths = (
          210
          209)
      end
      object cbxGroupIDsSelect: TComboBox
        Tag = 1
        Left = 212
        Top = 37
        Width = 194
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        Visible = False
        OnChange = cbxGroupIDsSelectChange
      end
      object pnOtherArtControlGroup: TPanel
        Left = 688
        Top = 0
        Width = 169
        Height = 637
        BevelOuter = bvNone
        TabOrder = 2
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
        TabOrder = 3
        OnClick = lbOtherArtListClick
        OnKeyDown = AcgArtListKeyDown
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
      object cbxTechposUnitGroup: TComboBox
        Left = 124
        Top = 301
        Width = 135
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Visible = False
        OnChange = cbxTechposUnitGroupChange
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
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goAlwaysShowEditor, goThumbTracking]
        TabOrder = 0
        OnDblClick = btnSoundRsPlayClick
        OnSelectCell = sgSamplesUibSelectCell
        OnSetEditText = sgSamplesUibSetEditText
        RowHeights = (
          19
          19
          19
          19
          19)
      end
      object sgSoundRs: TStringGrid
        Left = 336
        Top = 16
        Width = 320
        Height = 564
        ColCount = 3
        DefaultColWidth = 96
        DefaultRowHeight = 19
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goThumbTracking]
        TabOrder = 1
        OnDblClick = btnSoundRsPlayClick
        OnSelectCell = sgSoundRsSelectCell
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
      object btnSoundRsAdd: TButton
        Left = 520
        Top = 608
        Width = 64
        Height = 25
        Caption = 'Add new'
        TabOrder = 5
        OnClick = btnSoundRsAddClick
      end
      object btnSoundRsRemove: TButton
        Left = 584
        Top = 608
        Width = 72
        Height = 25
        Caption = 'Remove last'
        TabOrder = 6
        OnClick = btnSoundRsRemoveClick
      end
      object edSoundRsName: TEdit
        Left = 370
        Top = 584
        Width = 134
        Height = 21
        TabOrder = 7
      end
      object btnSoundRsRename: TButton
        Left = 508
        Top = 584
        Width = 57
        Height = 21
        Caption = 'Rename'
        TabOrder = 8
        OnClick = btnSoundRsRenameClick
      end
      object btnSamplesUibRemove: TButton
        Left = 160
        Top = 608
        Width = 100
        Height = 25
        Caption = 'Remove last'
        TabOrder = 9
        OnClick = btnSamplesUibRemoveClick
      end
      object btnSamplesUibAdd: TButton
        Left = 60
        Top = 608
        Width = 100
        Height = 25
        Caption = 'Add new'
        TabOrder = 10
        OnClick = btnSamplesUibAddClick
      end
    end
  end
  object pnImagePalette: TPanel
    Left = 1024
    Top = 216
    Width = 265
    Height = 289
    TabOrder = 2
    Visible = False
    object imgImagePalette: TImage
      Left = 4
      Top = 4
      Width = 257
      Height = 257
      OnClick = imgImagePaletteClick
      OnMouseMove = imgImagePaletteMouseMove
    end
    object lblImagePaletteColorIndex: TLabel
      Left = 4
      Top = 268
      Width = 29
      Height = 13
      Caption = 'Index:'
    end
    object btnImagePaletteSetDefaultColors: TButton
      Left = 164
      Top = 265
      Width = 97
      Height = 21
      Caption = 'Set default colors'
      TabOrder = 0
      OnClick = btnImagePaletteSetDefaultColorsClick
    end
    object btnImagePaletteRemapColors: TButton
      Left = 64
      Top = 265
      Width = 97
      Height = 21
      Caption = 'Remap colors'
      TabOrder = 1
      OnClick = btnImagePaletteRemapColorsClick
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
      Caption = 'Save and test'
      object Launchgame1: TMenuItem
        Caption = 'Launch game'
        ShortCut = 118
        OnClick = Launchgame1Click
      end
      object Launchmission1: TMenuItem
        Caption = 'Launch mission'
        ShortCut = 119
        OnClick = Launchmission1Click
      end
      object Launchwithsettings1: TMenuItem
        Caption = 'Launch with settings'
        ShortCut = 120
        OnClick = Launchwithsettings1Click
      end
    end
    object Reloadfiles1: TMenuItem
      Caption = 'Reload files (Ctrl+R)'
      ShortCut = 16466
      OnClick = Reloadfiles1Click
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
  object ImageImportDialog: TOpenDialog
    DefaultExt = 'png'
    Filter = 'Supported images (*.bmp, *.png)|*.bmp;*.png'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Import Image'
    Left = 876
    Top = 16
  end
  object ImageExportDialog: TSaveDialog
    DefaultExt = 'png'
    Filter = 'BMP Image (*.bmp)|*.bmp|PNG Image (*.png)|*.png'
    FilterIndex = 2
    Title = 'Export Image'
    Left = 844
    Top = 16
  end
  object ImageRemapColorsOpenDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Remap colors ini file (*.ini)|*.ini'
    Options = [ofEnableSizing]
    Title = 'Select remap colors ini file'
    Left = 904
    Top = 16
  end
end
