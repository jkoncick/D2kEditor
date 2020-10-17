unit structures_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, CheckLst, Spin, _structures,
  Grids, ValEdit, _utils, Buttons;

const side_names: array[0..CNT_PLAYERS-1] of string = ('Atreides', 'Harkonnen', 'Ordos', 'Emperor', 'Fremen', 'Smugglers', 'Mercenaries', 'Sandworm');
const unit_voices: array[0..8] of string = ('A1', 'A2', 'A3', 'H1', 'H2', 'H3', 'O1', 'O2', 'O3');

type
  TBuildingClipboard = record
    template: TBuildingTemplate;
    name: array[0..449] of char;
    builexp: TBuilExpEntry;
  end;

  TBuildingClipboardPtr = ^TBuildingClipboard;

type
  TUnitClipboard = record
    template: TUnitTemplate;
    name: array[0..449] of char;
  end;

  TUnitClipboardPtr = ^TUnitClipboard;

const TECHPOS_PREVIEW_SIZE = 7;

type
  TItemNameList = array[0..0, 0..49] of char;
  TItemNameListPtr = ^TItemNameList;

type
  TListControlGroup = record
    item_count_byte_ptr: PByte;
    item_name_list_ptr: TItemNameListPtr;
    max_item_count: integer;
    last_item_index_ptr: PInteger;
    list_control: TListBox;
    edit_item_name: TEdit;
    btn_item_add: TButton;
    btn_item_remove: TButton;
    btn_item_rename: TButton;
  end;

  TListControlGroupPtr = ^TListControlGroup;

const LCG_BUILDING_TYPES    = 0;
const LCG_UNIT_TYPES        = 1;
const LCG_WEAPONS           = 2;
const LCG_EXPLOSIONS        = 3;
const LCG_ARMOUR_TYPES      = 4;
const LCG_WARHEADS          = 5;

type
  TArtControlGroup = record
    first_image_index: integer;
    is_unit: boolean;
    view_image: TImage;
    frame_list: TListBox;
    lbl_frame_width: TLabel;
    lbl_frame_height: TLabel;
    lbl_image_width: TLabel;
    lbl_image_height: TLabel;
    lbl_image_offset_x: TLabel;
    lbl_image_offset_y: TLabel;
    edit_frame_width: TEdit;
    edit_frame_height: TEdit;
    edit_image_width: TEdit;
    edit_image_height: TEdit;
    edit_image_offset_x: TEdit;
    edit_image_offset_y: TEdit;
    btn_art_add: TButton;
    btn_art_remove: TButton;
    btn_art_modify: TButton;
  end;

  TArtControlGroupPtr = ^TArtControlGroup;

const ACG_BUIDING_ART       = 0;
const ACG_BUIDING_ANIMATION = 1;
const ACG_BUIDUP_ART        = 2;
const ACG_UNIT_ART          = 3;
const ACG_PROJECTILE_ART    = 4;
const ACG_ANIMATION_ART     = 5;

type
  TStructuresEditor = class(TForm)
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    PageControl: TPageControl;
    PageBuildings: TTabSheet;
    PageUnits: TTabSheet;
    pnBuildingTypeList: TPanel;
    lblBuildingTypeList: TLabel;
    lbBuildingTypeList: TListBox;
    pnBuildingList: TPanel;
    lblBuildingList: TLabel;
    lbBuildingList: TListBox;
    gbBuildingBasic: TGroupBox;
    gbBuildingBuildRequirements: TGroupBox;
    gbBuildingSpaceRequirements: TGroupBox;
    gbBuildingProperties: TGroupBox;
    gbBuildingTurret: TGroupBox;
    gbBuildingVisuals: TGroupBox;
    clbBuildingOwnerSide: TCheckListBox;
    lblBuildingOwnerSide: TLabel;
    edBuildingName: TEdit;
    lblBuildingName: TLabel;
    cbxBuildingType: TComboBox;
    lblBuildingType: TLabel;
    imgBuildingIcon: TImage;
    lblBuildingRequirementsTech: TLabel;
    lblBuildingRequirementsCost: TLabel;
    lblBuildingRequirementsSpeed: TLabel;
    seBuildingTechLevelBuild: TSpinEdit;
    seBuildingTechLevelUpgrade1: TSpinEdit;
    seBuildingTechLevelUpgrade2: TSpinEdit;
    seBuildingTechLevelUpgrade3: TSpinEdit;
    lblBuildingRequirementsBuild: TLabel;
    lblBuildingRequirementsUpgrade1: TLabel;
    lblBuildingRequirementsUpgrade2: TLabel;
    lblBuildingRequirementsUpgrade3: TLabel;
    edBuildingCostBuild: TEdit;
    edBuildingCostUpgrade1: TEdit;
    edBuildingCostUpgrade2: TEdit;
    edBuildingCostUpgrade3: TEdit;
    edBuildingBuildSpeedBuild: TEdit;
    edBuildingBuildSpeedUpgrade1: TEdit;
    edBuildingBuildSpeedUpgrade2: TEdit;
    edBuildingBuildSpeedUpgrade3: TEdit;
    lblBuildingPrereq1BuildingType: TLabel;
    lblBuildingPrereq2BuildingType: TLabel;
    clbBuildingPrereq1OwnerSide: TCheckListBox;
    clbBuildingPrereq2OwnerSide: TCheckListBox;
    cbxBuildingPrereq1BuildingType: TComboBox;
    cbxBuildingPrereq2BuildingType: TComboBox;
    seBuildingPrereq1UpgradesNeeded: TSpinEdit;
    seBuildingPrereq2UpgradesNeeded: TSpinEdit;
    lblBuildingPrereq1UpgradesNeeded: TLabel;
    lblBuildingPrereq2UpgradesNeeded: TLabel;
    lblBuildingPrereq1OwnerSide: TLabel;
    lblBuildingPrereq2OwnerSide: TLabel;
    lblBuildingHitPoints: TLabel;
    lblBuildingArmorType: TLabel;
    edBuildingHitPoints: TEdit;
    cbxBuildingArmorType: TComboBox;
    lblBuildingPowerConsumption: TLabel;
    lblBuildingHealthBarSize: TLabel;
    lblBuildingSpecialBehavior: TLabel;
    edBuildingPowerConsumption: TEdit;
    cbxBuildingHealthBarSize: TComboBox;
    cbxBuildingSpecialBehavior: TComboBox;
    cbBuildingFlagSELECT_REPAIR: TCheckBox;
    cbBuildingFlagCAN_CAPTURE: TCheckBox;
    cbBuildingFlagALWAYS_DECAY: TCheckBox;
    cbBuildingFlagCANNOT_SELL: TCheckBox;
    lblBuildingSightRadius: TLabel;
    seBuildingSightRadius: TSpinEdit;
    cbBuildingActLikeTurret: TCheckBox;
    cbBuildingRequireEnoughPower: TCheckBox;
    lblBuildingPrimaryWeapon: TLabel;
    lblBuildingSecondaryWeapon: TLabel;
    cbxBuildingPrimaryWeapon: TComboBox;
    cbxBuildingSecondaryWeapon: TComboBox;
    lblBuildingRateOfFire: TLabel;
    lblBuildingBarrelRotationSpeed: TLabel;
    seBuildingRateOfFire: TSpinEdit;
    seBuildingBarrelRotationSpeed: TSpinEdit;
    imgBuildingImage: TImage;
    lblBuildingBuildingArt: TLabel;
    cbxBuildingBuildingArt: TComboBox;
    lblBuildingBarrelArt: TLabel;
    cbxBuildingBarrelArt: TComboBox;
    lblBuildingArtWidth: TLabel;
    lblBuildingArtHeight: TLabel;
    edBuildingArtWidth: TEdit;
    edBuildingArtHeight: TEdit;
    cbxBuildingBuildingAnimation: TComboBox;
    cbBuildingFlagHAS_ANIMATION: TCheckBox;
    cbBuildingFlagANIM_PERMANENT: TCheckBox;
    lblBuildingBuildingAnimation: TLabel;
    lblBuildingAnimationSpeed: TLabel;
    seBuildingAnimationSpeed: TSpinEdit;
    lblBuildingBuildupArt: TLabel;
    cbxBuildingBuildupArt: TComboBox;
    lblBuildingBuildupFramesToShow: TLabel;
    seBuildingBuildupFramesToShow: TSpinEdit;
    imgBuildingTilesOccupiedAll: TImage;
    imgBuildingTilesOccupiedSolid: TImage;
    lblBuildingTilesOccupiedAll: TLabel;
    lblBuildingTilesOccupiedSolid: TLabel;
    lblBuildingExitPoint1X: TLabel;
    lblBuildingExitPoint1Y: TLabel;
    lblBuildingExitPoint2X: TLabel;
    lblBuildingExitPoint2Y: TLabel;
    seBuildingExitPoint1X: TSpinEdit;
    seBuildingExitPoint1Y: TSpinEdit;
    seBuildingExitPoint2X: TSpinEdit;
    seBuildingExitPoint2Y: TSpinEdit;
    cbBuildingFlagHAS_SKIRT: TCheckBox;
    cbBuildingFlagNO_CONCRETE: TCheckBox;
    lblBuildingDeathExplosion: TLabel;
    lblBuildingFiringExplosion: TLabel;
    cbxBuildingDeathExplosion: TComboBox;
    cbxBuildingFiringExplosion: TComboBox;
    sgBuildingDirectionFrames: TStringGrid;
    lblBuildingDirectionFrames: TLabel;
    gbBuildingOtherUnknown: TGroupBox;
    lblBuildingUnknown93: TLabel;
    seBuildingUnknown93: TSpinEdit;
    lblBuildingFlags: TLabel;
    edBuildingFlags: TEdit;
    cbBuildingFlagAUTOREPAIR: TCheckBox;
    cbBuildingFlagUNKNOWN9: TCheckBox;
    cbBuildingFlagANIM_ALPHA: TCheckBox;
    seBuildingUnknown8: TSpinEdit;
    lblBuildingUnknown8: TLabel;
    btnBuildingDirectionFrames0: TButton;
    btnBuildingDirectionFrames8: TButton;
    btnBuildingDirectionFrames32: TButton;
    btnBuildingBuildingAnimationPlay: TButton;
    tmBuildingBuildingAnimation: TTimer;
    btnBuildingBuildupArtPlay: TButton;
    tmBuildingBuildupArt: TTimer;
    pnUnitTypeList: TPanel;
    lblUnitTypeList: TLabel;
    lbUnitTypeList: TListBox;
    pnUnitList: TPanel;
    lblUnitList: TLabel;
    lbUnitList: TListBox;
    gbUnitBasic: TGroupBox;
    lblUnitOwnerSide: TLabel;
    lblUnitName: TLabel;
    lblUnitType: TLabel;
    imgUnitIcon: TImage;
    clbUnitOwnerSide: TCheckListBox;
    edUnitName: TEdit;
    cbxUnitType: TComboBox;
    gbUnitBuildRequirements: TGroupBox;
    lblUnitTechLevel: TLabel;
    lblUnitCost: TLabel;
    lblUnitBuildSpeed: TLabel;
    lblUnitPrereq1BuildingType: TLabel;
    lblUnitPrereq2BuildingType: TLabel;
    lblUnitPrereq1UpgradesNeeded: TLabel;
    lblUnitPrereq1OwnerSide: TLabel;
    seUnitTechLevel: TSpinEdit;
    edUnitCost: TEdit;
    edUnitBuildSpeed: TEdit;
    clbUnitPrereq1OwnerSide: TCheckListBox;
    cbxUnitPrereq1BuildingType: TComboBox;
    cbxUnitPrereq2BuildingType: TComboBox;
    seUnitPrereq1UpgradesNeeded: TSpinEdit;
    cbUnitAvailableInStarport: TCheckBox;
    cbUnitMultiplayerOnly: TCheckBox;
    gbUnitVoices: TGroupBox;
    gbUnitProperties: TGroupBox;
    lblUnitHitPoints: TLabel;
    lblUnitArmorType: TLabel;
    lblUnitHealthBarSize: TLabel;
    lblUnitSpecialBehavior: TLabel;
    lblUnitSightRadius: TLabel;
    edUnitHitPoints: TEdit;
    cbxUnitArmorType: TComboBox;
    cbxUnitHealthBarSize: TComboBox;
    cbxUnitSpecialBehavior: TComboBox;
    cbUnitIsInfantry: TCheckBox;
    cbUnitFlagUF_STEALTH: TCheckBox;
    cbUnitFlagUF_SELFHEALING: TCheckBox;
    seUnitSightRadius: TSpinEdit;
    gbUnitMovement: TGroupBox;
    gbUnitWeapons: TGroupBox;
    lblUnitPrimaryWeapon: TLabel;
    lblUnitSecondaryWeapon: TLabel;
    lblUnitRateOfFire: TLabel;
    lblUnitBarrelRotationSpeed: TLabel;
    cbUnitHasBarrel: TCheckBox;
    cbUnitFlagUF_FIXED_BARREL: TCheckBox;
    cbxUnitPrimaryWeapon: TComboBox;
    cbxUnitSecondaryWeapon: TComboBox;
    seUnitRateOfFire: TSpinEdit;
    seUnitBarrelRotationSpeed: TSpinEdit;
    edUnitSpeed: TEdit;
    lblUnitSpeed: TLabel;
    lblUnitSpeedType: TLabel;
    cbxUnitSpeedType: TComboBox;
    lblUnitUnitRotationSpeed: TLabel;
    seUnitUnitRotationSpeed: TSpinEdit;
    gbUnitVisuals: TGroupBox;
    imgUnitImage: TImage;
    lblUnitUnitArt: TLabel;
    lblUnitBarrelArt: TLabel;
    lblUnitDeathExplosion: TLabel;
    lblUnitFiringExplosion: TLabel;
    lblUnitDirectionFrames: TLabel;
    cbxUnitUnitArt: TComboBox;
    cbxUnitBarrelArt: TComboBox;
    cbxUnitDeathExplosion: TComboBox;
    cbxUnitFiringExplosion: TComboBox;
    sgUnitDirectionFrames: TStringGrid;
    btnUnitDirectionFrames0: TButton;
    btnUnitDirectionFrames8: TButton;
    btnUnitDirectionFrames32: TButton;
    gbUnitOtherUnknown: TGroupBox;
    lblUnitUnknown52: TLabel;
    lblUnitFlags: TLabel;
    lblUnitUnknown46: TLabel;
    seUnitUnknown52: TSpinEdit;
    edUnitFlags: TEdit;
    seUnitUnknown46: TSpinEdit;
    lblUnitUnknown55: TLabel;
    seUnitUnknown55: TSpinEdit;
    seUnitUnknown164: TSpinEdit;
    lblUnitUnknown164: TLabel;
    cbUnitCanCrushInfantry: TCheckBox;
    lblUnitReportingSounds: TLabel;
    lblUnitConfirmedSounds: TLabel;
    cbUnitFlagUF_NO_AI: TCheckBox;
    PageWeapons: TTabSheet;
    PageExplosions: TTabSheet;
    PageArmour: TTabSheet;
    PageSpeed: TTabSheet;
    sgSpeedValues: TStringGrid;
    btnBuildingAdd: TButton;
    btnBuildingRemove: TButton;
    btnBuildingCopy: TButton;
    btnBuildingPaste: TButton;
    Applychanges1: TMenuItem;
    Savetofiles1: TMenuItem;
    Saveandtest1: TMenuItem;
    Reloadfiles1: TMenuItem;
    CopyfilestoModsfolder1: TMenuItem;
    PageBuildingArt: TTabSheet;
    PageUnitArt: TTabSheet;
    pnBuildingArtList: TPanel;
    lblBuildingArtList: TLabel;
    lbBuildingArtList: TListBox;
    seBuildingArtDirections: TSpinEdit;
    lblBuildingArtDirections: TLabel;
    pnBuildingArtControlGroup: TPanel;
    pnBuildingAnimationArtList: TPanel;
    lblBuildingAnimationArtList: TLabel;
    lbBuildingAnimationArtList: TListBox;
    btnBuildingAnimationArtModify: TButton;
    pnBuildingAnimationControlGroup: TPanel;
    pnBuildupArtControlGroup: TPanel;
    seBuildingAnimationFrames: TSpinEdit;
    seBuildupArtFrames: TSpinEdit;
    lblBuildingAnimationFrames: TLabel;
    lblBuildupArtFrames: TLabel;
    pnUnitArtList: TPanel;
    lblUnitArtList: TLabel;
    lblUnitArtAnimationFrames: TLabel;
    lbUnitArtList: TListBox;
    seUnitArtAnimationFrames: TSpinEdit;
    lblUnitArtDirectionFrames: TLabel;
    seUnitArtDirectionFrames: TSpinEdit;
    pnUnitArtControlGroup: TPanel;
    pnProjectileArtList: TPanel;
    lblProjectileArtList: TLabel;
    lblProjectileArtDirections: TLabel;
    lbProjectileArtList: TListBox;
    seProjectileArtDirections: TSpinEdit;
    pnProjectileArtControlGroup: TPanel;
    pnAnimationArtList: TPanel;
    lblAnimationArtList: TLabel;
    lblAnimationArtFrames: TLabel;
    lbAnimationArtList: TListBox;
    seAnimationArtFrames: TSpinEdit;
    pnAnimationArtControlGroup: TPanel;
    pnWeaponList: TPanel;
    lblWeaponList: TLabel;
    lbWeaponList: TListBox;
    pnExplosionList: TPanel;
    lblExplosionList: TLabel;
    lbExplosionList: TListBox;
    pnArmourTypeList: TPanel;
    lblArmourTypeList: TLabel;
    lbArmourTypeList: TListBox;
    pnWarheadList: TPanel;
    lblWarheadList: TLabel;
    lbWarheadList: TListBox;
    sgArmourValues: TStringGrid;
    PageBuilExp: TTabSheet;
    pnBuilExpBuildingList: TPanel;
    lblBuilExpBuildingList: TLabel;
    lbBuilExpBuildingList: TListBox;
    seBuilExpNumAnimations: TSpinEdit;
    lblBuilExpNumAnimations: TLabel;
    imgBuilExpImage: TImage;
    pnBuilExpAnimations: TPanel;
    lblBuilExpAnimOffsetX: TLabel;
    lblBuilExpAnimOffsetY: TLabel;
    lblBuilExpAnimExplosion: TLabel;
    lblBuilExpAnimNumFrames: TLabel;
    gbWeaponProperties: TGroupBox;
    lblWeaponDamage: TLabel;
    lblWeaponWarhead: TLabel;
    edWeaponDamage: TEdit;
    cbxWeaponWarhead: TComboBox;
    lblWeaponRange: TLabel;
    edWeaponRange: TEdit;
    cbWeaponAntiAircraft: TCheckBox;
    cbWeaponFlagWF_BLOCKED_BY_WALL: TCheckBox;
    cbWeaponFlagWF_DEVIATOR: TCheckBox;
    cbWeaponFlagWF_SONIC: TCheckBox;
    cbWeaponFlagWF_FALLING: TCheckBox;
    dbWeaponMovement: TGroupBox;
    lblWeaponProjectileSpeed: TLabel;
    edWeaponProjectileSpeed: TEdit;
    cbWeaponFlagWF_ARC_TRAJECTORY: TCheckBox;
    cbWeaponFlagWF_HOMING: TCheckBox;
    gbWeaponVisuals: TGroupBox;
    lblWeaponProjectileArt: TLabel;
    cbxWeaponProjectileArt: TComboBox;
    lblWeaponFiringSound: TLabel;
    cbxWeaponFiringSound: TComboBox;
    lblWeaponTrailExplosion: TLabel;
    lblWeaponHitExplosion: TLabel;
    cbxWeaponHitExplosion: TComboBox;
    cbxWeaponTrailExplosion: TComboBox;
    cbWeaponFlagWF_PROJECTILE_ALPHA: TCheckBox;
    cbWeaponFlagWF_ANIM_PROJECTILE: TCheckBox;
    cbWeaponFlagWF_MAKE_TRAIL: TCheckBox;
    gbWeaponOtherUnknown: TGroupBox;
    lblWeaponFlags: TLabel;
    edWeaponFlags: TEdit;
    lblWeaponUnknown19: TLabel;
    seWeaponUnknown19: TSpinEdit;
    cbWeaponFlagWF_DEBRIS: TCheckBox;
    lblExplosionMyIndex: TLabel;
    lblExplosionSound: TLabel;
    cbxExplosionSound: TComboBox;
    lblExplosionFiringPattern: TLabel;
    lblWeaponUsedBy: TLabel;
    edExplosionFiringPattern: TEdit;
    lblExplosionUsedBy: TLabel;
    edExplosionMyIndex: TEdit;
    lblWarheadUsedBy: TLabel;
    pnSpeedTypeList: TPanel;
    lblSpeedTypeList: TLabel;
    lbSpeedTypeList: TListBox;
    edSpeedTypeName: TEdit;
    btnSpeedTypeRename: TButton;
    lblSpeedTypeUsedBy: TLabel;
    lblArmourTypeUsedBy: TLabel;
    lblExplosionFlags: TLabel;
    edExplosionFlags: TEdit;
    cbExplosionFlagEF_ADDITIVE_ALPHA: TCheckBox;
    cbExplosionFlagEF_SUBSTRACTIVE_ALPA: TCheckBox;
    cbExplosionFlagEF_SEMI_TRANSPARENCY: TCheckBox;
    cbExplosionFlagEF_RISE_UP: TCheckBox;
    cbExplosionFlagEF_HOUSE_COLORED: TCheckBox;
    cbExplosionFlagEF_FIRING_FLASH: TCheckBox;
    btnUnitAdd: TButton;
    btnUnitRemove: TButton;
    btnUnitCopy: TButton;
    btnUnitPaste: TButton;
    PageOther: TTabSheet;
    vleTemplatesOther: TValueListEditor;
    cbxTemplatesOtherSelect: TComboBox;
    cbxTemplatesOtherUnitSelect: TComboBox;
    PageTechpos: TTabSheet;
    imgTechposPreview: TImage;
    rgTechposTechLevel: TRadioGroup;
    sbTechposAtreides: TSpeedButton;
    sbTechposHarkonnen: TSpeedButton;
    sbTechposOrdos: TSpeedButton;
    sgTechposData: TStringGrid;
    cbxTechposUnitType: TComboBox;
    tbTechposNumUnits: TTrackBar;
    lblTechposNumUnits: TLabel;
    // Form events
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    // Main menu events
    procedure Applychanges1Click(Sender: TObject);
    procedure Savetofiles1Click(Sender: TObject);
    procedure Saveandtest1Click(Sender: TObject);
    procedure Reloadfiles1Click(Sender: TObject);
    procedure CopyfilestoModsfolder1Click(Sender: TObject);
    // Buildings tab events
    procedure lbBuildingTypeListClick(Sender: TObject);
    procedure lbBuildingListClick(Sender: TObject);
    procedure btnBuildingAddClick(Sender: TObject);
    procedure btnBuildingRemoveClick(Sender: TObject);
    procedure btnBuildingCopyClick(Sender: TObject);
    procedure btnBuildingPasteClick(Sender: TObject);
    procedure imgBuildingTilesOccupiedAllMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgBuildingTilesOccupiedSolidMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure edBuildingFlagsChange(Sender: TObject);
    procedure BuildingFlagCheckboxChange(Sender: TObject);
    procedure btnBuildingDirectionFramesClick(Sender: TObject);
    procedure RedrawBuildingPreview(Sender: TObject);
    procedure btnBuildingBuildingAnimationPlayClick(Sender: TObject);
    procedure tmBuildingBuildingAnimationTimer(Sender: TObject);
    procedure btnBuildingBuildupArtPlayClick(Sender: TObject);
    procedure tmBuildingBuildupArtTimer(Sender: TObject);
    // Units tab events
    procedure lbUnitTypeListClick(Sender: TObject);
    procedure lbUnitListClick(Sender: TObject);
    procedure btnUnitAddClick(Sender: TObject);
    procedure btnUnitRemoveClick(Sender: TObject);
    procedure btnUnitCopyClick(Sender: TObject);
    procedure btnUnitPasteClick(Sender: TObject);
    procedure edUnitFlagsChange(Sender: TObject);
    procedure UnitFlagCheckboxChange(Sender: TObject);
    procedure btnUnitDirectionFramesClick(Sender: TObject);
    procedure RedrawUnitPreview(Sender: TObject);
    // Building Art tab events
    procedure lbBuildingArtListClick(Sender: TObject);
    procedure lbBuildingAnimationArtListClick(Sender: TObject);
    procedure btnBuildingAnimationArtModifyClick(Sender: TObject);
    // BuilExp tab events
    procedure lbBuilExpBuildingListClick(Sender: TObject);
    procedure seBuilExpNumAnimationsChange(Sender: TObject);
    procedure RedrawBuilExpPreview(Sender: TObject);
    // Unit Art tab events
    procedure lbUnitArtListClick(Sender: TObject);
    // Weapons tab events
    procedure lbWeaponListClick(Sender: TObject);
    procedure edWeaponFlagsChange(Sender: TObject);
    procedure WeaponFlagCheckboxChange(Sender: TObject);
    procedure cbxWeaponProjectileArtChange(Sender: TObject);
    procedure lbProjectileArtListClick(Sender: TObject);
    // Explosions tab events
    procedure lbExplosionListClick(Sender: TObject);
    procedure edExplosionFlagsChange(Sender: TObject);
    procedure ExplosionFlagCheckboxChange(Sender: TObject);
    procedure lbAnimationArtListClick(Sender: TObject);
    // Armour tab events
    procedure lbArmourTypeListClick(Sender: TObject);
    procedure lbWarheadListClick(Sender: TObject);
    // Speed tab events
    procedure lbSpeedTypeListClick(Sender: TObject);
    procedure btnSpeedTypeRenameClick(Sender: TObject);
    // Other tab events
    procedure vleTemplatesOtherSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure vleTemplatesOtherTopLeftChanged(Sender: TObject);
    procedure cbxTemplatesOtherSelectChange(Sender: TObject);
    // Techpos tab events
    procedure rgTechposTechLevelClick(Sender: TObject);
    procedure sgTechposDataSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgTechposDataSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure cbxTechposUnitTypeChange(Sender: TObject);
    procedure TechposPreviewChange(Sender: TObject);
    procedure imgTechposPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // List control group events
    procedure ListControlGroupAddClick(Sender: TObject);
    procedure ListControlGroupRemoveClick(Sender: TObject);
    procedure ListControlGroupRenameClick(Sender: TObject);
    // Art control group events
    procedure ArtControlGroupFrameListClick(Sender: TObject);
    procedure ArtControlGroupAddClick(Sender: TObject);
    procedure ArtControlGroupRemoveClick(Sender: TObject);
    procedure ArtControlGroupModifyClick(Sender: TObject);
  private
    // Dynamic controls
    cbxUnitVoices: array[0..17] of TComboBox;
    lblUnitVoices: array[0..17] of TLabel;
    lblBuilExpAnimIndex: array[0..MAX_BUILEXP_ANIMATIONS-1] of TLabel;
    seBuilExpAnimOffsetX: array[0..MAX_BUILEXP_ANIMATIONS-1] of TSpinEdit;
    seBuilExpAnimOffsetY: array[0..MAX_BUILEXP_ANIMATIONS-1] of TSpinEdit;
    cbxBuilExpAnimExplosion: array[0..MAX_BUILEXP_ANIMATIONS-1] of TComboBox;
    seBuilExpAnimNumFrames: array[0..MAX_BUILEXP_ANIMATIONS-1] of TSpinEdit;
    list_control_groups: array[0..5] of TListControlGroup;
    art_control_groups: array[0..5] of TArtControlGroup;
    // Clipboard formats
    clipboard_format_building: cardinal;
    clipboard_format_unit: cardinal;
    // Last indexes
    last_building_type_index: integer;
    last_building_index: integer;
    last_unit_type_index: integer;
    last_unit_index: integer;
    last_building_art_index: integer;
    last_building_animation_art_index: integer;
    last_builexp_building_index: integer;
    last_unit_art_index: integer;
    last_weapon_index: integer;
    last_projectile_art_index: integer;
    last_explosion_index: integer;
    last_animation_art_index: integer;
    last_armour_type_index: integer;
    last_warhead_index: integer;
    last_speed_type_index: integer;
    // Temporary data
    tmp_building_tiles_occupied_all: cardinal;
    tmp_building_tiles_occupied_solid: cardinal;
    // Loading flag
    loading: boolean;
  public
    // Fill data procedures
    procedure fill_data;
    procedure fill_building_data;
    procedure fill_unit_data;
    procedure fill_builexp_data;
    procedure fill_weapon_data;
    procedure fill_explosion_data;
    procedure set_owner_side_field_value(control: TCheckListBox; value: byte);
    // Store data procedures
    procedure store_data;
    procedure store_building_data;
    procedure store_unit_data;
    procedure store_builexp_data;
    procedure store_weapon_data;
    procedure store_explosion_data;
    function get_owner_side_field_value(control: TCheckListBox): byte;
    // Templates other procedures
    function get_templates_other_cell_text(index: integer; value: shortint): string;
    // List control group procedures
    procedure create_list_control_group(group_index: integer; item_count_byte_ptr: PByte; item_name_list_ptr: TItemNameListPtr; max_item_count: integer; last_item_index_ptr: PInteger; list_control: TListBox; container: TPanel);
    // Art control group procedures
    procedure create_art_control_group(group_index: integer; is_unit: boolean; container, container2: TPanel);
    procedure fill_art_control_group_frame_list(group_index: integer; first_image_index, num_frames: integer; frame_names: TStrings; selected_frame: integer);
    // Drawing procedures
    procedure draw_no_image_sign(img_target: TImage);
    procedure draw_building_tile_map(img_target: TImage; value: cardinal);
    procedure draw_building_preview(draw_building: boolean);
    procedure draw_building_frame(image_index: integer; alpha, animation: boolean);
    procedure draw_unit_preview;
    procedure draw_unit_frame(image_index, side: integer; is_stealth: boolean);
    procedure draw_builexp_preview;
    procedure draw_building_art_frame(img_target: TImage; image_index: integer; draw_background: boolean);
    procedure draw_unit_art_frame(img_target: TImage; image_index: integer);
    procedure draw_techpos_preview;
    // General procedures
    procedure apply_changes;
    procedure save_to_files;
  end;

var
  StructuresEditor: TStructuresEditor;

implementation

uses _tileset, _stringtable, Clipbrd, map_stats_dialog, event_dialog,
  mission_dialog, main, _launcher, _renderer, Math;

{$R *.dfm}

{ TStructuresEditor }

procedure TStructuresEditor.FormCreate(Sender: TObject);
var
  tmp_strings: TStringList;
  dummy: boolean;
  i: integer;
  c1, c2, f: Int64;
begin
  clipboard_format_building := RegisterClipboardFormat('D2kEditorBuildingTemplate');
  clipboard_format_unit := RegisterClipboardFormat('D2kEditorUnitTemplate');
  QueryPerformanceFrequency(f);
  QueryPerformanceCounter(c1);
  tmp_strings := TStringList.Create;
  // Side names
  for i := 0 to CNT_PLAYERS - 1 do
    tmp_strings.Add(side_names[i]);
  clbBuildingOwnerSide.Items := tmp_strings;
  clbBuildingPrereq1OwnerSide.Items := tmp_strings;
  clbBuildingPrereq2OwnerSide.Items := tmp_strings;
  clbUnitOwnerSide.Items := tmp_strings;
  clbUnitPrereq1OwnerSide.Items := tmp_strings;
  // Unit voices combo boxes
  tmp_strings.Clear;
  for i := 0 to SoundStringTable.get_table_size - 1 do
    tmp_strings.Add(inttostr(i) + ' - ' + SoundStringTable.get_text(i, false, dummy));
  for i := 0 to 17 do
  begin
    cbxUnitVoices[i] := TComboBox.Create(self);
    cbxUnitVoices[i].Style := csDropDownList;
    cbxUnitVoices[i].Parent := gbUnitVoices;
    cbxUnitVoices[i].Width := 109;
    cbxUnitVoices[i].Left := 32 + 140 * (i div 9);
    cbxUnitVoices[i].Top := 36 + 24 * (i mod 9);
    cbxUnitVoices[i].Items := tmp_strings;
    lblUnitVoices[i] := TLabel.Create(self);
    lblUnitVoices[i].Parent := gbUnitVoices;
    lblUnitVoices[i].Left := 8 + 140 * (i div 9);
    lblUnitVoices[i].Top := 36 + 24 * (i mod 9);
    lblUnitVoices[i].Caption := unit_voices[i mod 9] + ':';
  end;
  tmp_strings.Insert(0, '(none)');
  cbxWeaponFiringSound.Items := tmp_strings;
  cbxExplosionSound.Items := tmp_strings;
  // BuilExp controls
  for i := 0 to MAX_BUILEXP_ANIMATIONS - 1 do
  begin
    lblBuilExpAnimIndex[i] := TLabel.Create(self);
    lblBuilExpAnimIndex[i].Top := i * 24 + 24;
    lblBuilExpAnimIndex[i].Caption := 'Animation ' + inttostr(i);
    lblBuilExpAnimIndex[i].Parent := pnBuilExpAnimations;
    seBuilExpAnimOffsetX[i] := TSpinEdit.Create(self);
    seBuilExpAnimOffsetX[i].Top := i * 24 + 24;
    seBuilExpAnimOffsetX[i].Left := 64;
    seBuilExpAnimOffsetX[i].Width := 57;
    seBuilExpAnimOffsetX[i].MinValue := -128;
    seBuilExpAnimOffsetX[i].MaxValue := 127;
    seBuilExpAnimOffsetX[i].Parent := pnBuilExpAnimations;
    seBuilExpAnimOffsetX[i].OnChange := RedrawBuilExpPreview;
    seBuilExpAnimOffsetY[i] := TSpinEdit.Create(self);
    seBuilExpAnimOffsetY[i].Top := i * 24 + 24;
    seBuilExpAnimOffsetY[i].Left := 128;
    seBuilExpAnimOffsetY[i].Width := 57;
    seBuilExpAnimOffsetY[i].MinValue := -128;
    seBuilExpAnimOffsetY[i].MaxValue := 127;
    seBuilExpAnimOffsetY[i].Parent := pnBuilExpAnimations;
    seBuilExpAnimOffsetY[i].OnChange := RedrawBuilExpPreview;
    cbxBuilExpAnimExplosion[i] := TComboBox.Create(self);
    cbxBuilExpAnimExplosion[i].Style := csDropDownList;
    cbxBuilExpAnimExplosion[i].Top := i * 24 + 24;
    cbxBuilExpAnimExplosion[i].Left := 192;
    cbxBuilExpAnimExplosion[i].Width := 137;
    cbxBuilExpAnimExplosion[i].Parent := pnBuilExpAnimations;
    cbxBuilExpAnimExplosion[i].OnChange := RedrawBuilExpPreview;
    seBuilExpAnimNumFrames[i] := TSpinEdit.Create(self);
    seBuilExpAnimNumFrames[i].Top := i * 24 + 24;
    seBuilExpAnimNumFrames[i].Left := 336;
    seBuilExpAnimNumFrames[i].Width := 57;
    seBuilExpAnimNumFrames[i].MinValue := 0;
    seBuilExpAnimNumFrames[i].MaxValue := 255;
    seBuilExpAnimNumFrames[i].Parent := pnBuilExpAnimations;
    seBuilExpAnimNumFrames[i].OnChange := RedrawBuilExpPreview;
  end;

  QueryPerformanceCounter(c2);
  //Caption := floattostr((c2-c1)/f);
  // Armour
  sgArmourValues.ColWidths[0] := 98;
  sgArmourValues.Cells[13, 0] := 'Radius';
  sgArmourValues.Cells[14, 0] := 'Inf. death';
  // Speed modifier values
  sgSpeedValues.Cells[0, 0] := 'Modifier val.';
  for i := 0 to Length(Structures.speed.Values) - 1 do
    sgSpeedValues.Cells[0, i+1] := inttostr(i);
  // Techpos
  tmp_strings.Clear;
  for i := 0 to 9 do
  begin
    tmp_strings.Add('Tech level ' + inttostr(i));
    sgTechposData.Cells[0, i + 1] := inttostr(i + 1);
  end;
  rgTechposTechLevel.Items := tmp_strings;
  rgTechposTechLevel.ItemIndex := 1;
  tmp_strings.Destroy;
  sgTechposData.Cells[0, 0] := 'Unit #';
  sgTechposData.Cells[1, 0] := 'Pos X';
  sgTechposData.Cells[2, 0] := 'Pos Y';
  sgTechposData.Cells[3, 0] := 'Atreides unit';
  sgTechposData.Cells[4, 0] := 'Harkonnen unit';
  sgTechposData.Cells[5, 0] := 'Ordos unit';
  sgTechposData.ColWidths[3] := 133;
  sgTechposData.ColWidths[4] := 133;
  sgTechposData.ColWidths[5] := 133;
  // List control groups
  create_list_control_group(LCG_BUILDING_TYPES, Addr(Structures.templates.BuildingTypeCount), Addr(Structures.templates.BuildingTypeStrings), MAX_BUILDING_TYPES, Addr(last_building_type_index), lbBuildingTypeList, pnBuildingTypeList);
  create_list_control_group(LCG_UNIT_TYPES,     Addr(Structures.templates.UnitTypeCount),     Addr(Structures.templates.UnitTypeStrings),     MAX_UNIT_TYPES,     Addr(last_unit_type_index),     lbUnitTypeList,     pnUnitTypeList);
  create_list_control_group(LCG_WEAPONS,        Addr(Structures.templates.WeaponCount),       Addr(Structures.templates.WeaponStrings),       MAX_WEAPONS,        Addr(last_weapon_index),        lbWeaponList,       pnWeaponList);
  create_list_control_group(LCG_EXPLOSIONS,     Addr(Structures.templates.ExplosionCount),    Addr(Structures.templates.ExplosionStrings),    MAX_EXPLOSIONS,     Addr(last_explosion_index),     lbExplosionList,    pnExplosionList);
  create_list_control_group(LCG_ARMOUR_TYPES,   Addr(Structures.armour.ArmourTypeCount),      Addr(Structures.armour.ArmourTypeStrings),      MAX_ARMOUR_TYPES,   Addr(last_armour_type_index),   lbArmourTypeList,   pnArmourTypeList);
  create_list_control_group(LCG_WARHEADS,       Addr(Structures.armour.WarheadCount),         Addr(Structures.armour.WarheadStrings),         MAX_WARHEADS,       Addr(last_warhead_index),       lbWarheadList,      pnWarheadList);
  // Art control groups
  create_art_control_group(ACG_BUIDING_ART,       false, pnBuildingArtControlGroup,       pnBuildingArtList);
  create_art_control_group(ACG_BUIDING_ANIMATION, false, pnBuildingAnimationControlGroup, nil);
  create_art_control_group(ACG_BUIDUP_ART,        false, pnBuildupArtControlGroup,        nil);
  create_art_control_group(ACG_UNIT_ART,          true,  pnUnitArtControlGroup,           pnUnitArtList);
  create_art_control_group(ACG_PROJECTILE_ART,    true,  pnProjectileArtControlGroup,     pnProjectileArtList);
  create_art_control_group(ACG_ANIMATION_ART,     true,  pnAnimationArtControlGroup,      pnAnimationArtList);
end;

procedure TStructuresEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
    Close;
end;

procedure TStructuresEditor.Applychanges1Click(Sender: TObject);
begin
  apply_changes;
end;

procedure TStructuresEditor.Savetofiles1Click(Sender: TObject);
begin
  apply_changes;
  save_to_files;
end;

procedure TStructuresEditor.Saveandtest1Click(Sender: TObject);
begin
  if not MainWindow.check_map_can_be_tested then
    exit;
  apply_changes;
  save_to_files;
  Launcher.launch_current_mission;
end;

procedure TStructuresEditor.Reloadfiles1Click(Sender: TObject);
begin
  Structures.load_templates_bin(true);
  Structures.load_builexp_bin(true);
  Structures.load_armour_bin(true);
  Structures.load_techpos_bin(true);
  Structures.load_speed_bin(true);
  fill_data;
  apply_changes;
end;

procedure TStructuresEditor.CopyfilestoModsfolder1Click(Sender: TObject);
begin
  beep;
end;

procedure TStructuresEditor.lbBuildingTypeListClick(Sender: TObject);
begin
  last_building_type_index := lbBuildingTypeList.ItemIndex;
  list_control_groups[LCG_BUILDING_TYPES].edit_item_name.Text := Structures.templates.BuildingTypeStrings[lbBuildingTypeList.ItemIndex];
end;

procedure TStructuresEditor.lbBuildingListClick(Sender: TObject);
begin
  store_building_data;
  fill_building_data;
end;

procedure TStructuresEditor.btnBuildingAddClick(Sender: TObject);
begin
  if Structures.templates.BuildingCount = MAX_BUILDING_TYPES then
    exit;
  store_data;
  last_building_index := Structures.templates.BuildingCount;
  Inc(Structures.templates.BuildingCount);
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.btnBuildingRemoveClick(Sender: TObject);
var
  index: integer;
begin
  if Structures.templates.BuildingCount = 0 then
    exit;
  store_data;
  index := Structures.templates.BuildingCount - 1;
  if last_building_index = index then
    Dec(last_building_index);
  if last_building_animation_art_index = index then
    Dec(last_building_animation_art_index);
  if last_builexp_building_index = index then
    Dec(last_builexp_building_index);
  Dec(Structures.templates.BuildingCount);
  FillChar(Structures.templates.BuildingNameStrings[index], Length(Structures.templates.BuildingNameStrings[index]), 0);
  FillChar(Structures.templates.BuildingDefinitions[index], Sizeof(TBuildingTemplate), 0);
  FillChar(Structures.builexp[index], Sizeof(TBuilExpEntry), 0);
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.btnBuildingCopyClick(Sender: TObject);
var
  handle: THandle;
  pointer: TBuildingClipboardPtr;
  index: integer;
begin
  if lbBuildingList.ItemIndex < 0 then
    exit;
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TBuildingClipboard));
  pointer := GlobalLock(handle);

  store_data;
  index := lbBuildingList.ItemIndex;
  Move(Structures.templates.BuildingDefinitions[index], pointer.template, sizeof(TBuildingTemplate));
  Move(Structures.templates.BuildingNameStrings[index], pointer.name, Length(pointer.name));
  Move(Structures.builexp[index], pointer.builexp, sizeof(TBuilExpEntry));

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format_building, handle);
  CloseClipboard;
end;

procedure TStructuresEditor.btnBuildingPasteClick(Sender: TObject);
var
  handle: THandle;
  pointer: TBuildingClipboardPtr;
  index: integer;
begin
  if (lbBuildingList.ItemIndex < 0) or not Clipboard.HasFormat(clipboard_format_building) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format_building);
  pointer := GlobalLock(handle);

  index := lbBuildingList.ItemIndex;
  Move(pointer.template, Structures.templates.BuildingDefinitions[index], sizeof(TBuildingTemplate));
  Move(pointer.name, Structures.templates.BuildingNameStrings[index], Length(pointer.name));
  Move(pointer.builexp, Structures.builexp[index], sizeof(TBuilExpEntry));

  fill_data;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TStructuresEditor.imgBuildingTilesOccupiedAllMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X div 17;
  Y := Y div 17;
  if Button = mbLeft then
    tmp_building_tiles_occupied_all := tmp_building_tiles_occupied_all or (1 shl (Y * MAX_BUILDING_SIZE + x))
  else if Button = mbRight then
    tmp_building_tiles_occupied_all := tmp_building_tiles_occupied_all and (not (1 shl (Y * MAX_BUILDING_SIZE + x)));
  draw_building_tile_map(imgBuildingTilesOccupiedAll, tmp_building_tiles_occupied_all);
  draw_building_preview(true);
end;

procedure TStructuresEditor.imgBuildingTilesOccupiedSolidMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X div 17;
  Y := Y div 17;
  if Button = mbLeft then
    tmp_building_tiles_occupied_solid := tmp_building_tiles_occupied_solid or (1 shl (Y * MAX_BUILDING_SIZE + x))
  else if Button = mbRight then
    tmp_building_tiles_occupied_solid := tmp_building_tiles_occupied_solid and (not (1 shl (Y * MAX_BUILDING_SIZE + x)));
  draw_building_tile_map(imgBuildingTilesOccupiedSolid, tmp_building_tiles_occupied_solid);
end;

procedure TStructuresEditor.edBuildingFlagsChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edBuildingFlags.Text);
  loading := true;
  cbBuildingFlagSELECT_REPAIR.Checked := (value and BF_SELECT_REPAIR) <> 0;
  cbBuildingFlagCAN_CAPTURE.Checked := (value and BF_CAN_CAPTURE) <> 0;
  cbBuildingFlagALWAYS_DECAY.Checked := (value and BF_ALWAYS_DECAY) <> 0;
  cbBuildingFlagCANNOT_SELL.Checked := (value and BF_CANNOT_SELL) <> 0;
  cbBuildingFlagHAS_ANIMATION.Checked := (value and BF_HAS_ANIMATION) <> 0;
  cbBuildingFlagANIM_PERMANENT.Checked := (value and BF_ANIM_PERMANENT) <> 0;
  cbBuildingFlagANIM_ALPHA.Checked := (value and BF_ANIM_ALPHA) <> 0;
  cbBuildingFlagHAS_SKIRT.Checked := (value and BF_HAS_SKIRT) <> 0;
  cbBuildingFlagNO_CONCRETE.Checked := (value and BF_NO_CONCRETE) <> 0;
  cbBuildingFlagAUTOREPAIR.Checked := (value and BF_AUTOREPAIR) <> 0;
  cbBuildingFlagUNKNOWN9.Checked := (value and BF_UNKNOWN9) <> 0;
  loading := false;
  draw_building_preview(true);
end;

procedure TStructuresEditor.BuildingFlagCheckboxChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edBuildingFlags.Text);
  if (Sender as TCheckBox).Checked then
    value := value or  Cardinal((Sender as TCheckBox).Tag)
  else
    value := value and (not Cardinal((Sender as TCheckBox).Tag));
  loading := true;
  edBuildingFlags.Text := IntToHex(value, 8);
  loading := false;
  draw_building_preview(true);
end;

procedure TStructuresEditor.btnBuildingDirectionFramesClick(Sender: TObject);
var
  i: integer;
  count: integer;
  row, col: integer;
begin
  count := (Sender as TButton).Tag;
  for i := 0 to 31 do
  begin
    row := i div 8;
    col := i mod 8;
    if count = 0 then
      sgBuildingDirectionFrames.Cells[col, row] := inttostr(0)
    else if count = 32 then
      sgBuildingDirectionFrames.Cells[col, row] := inttostr(i)
    else if count = 8 then
      sgBuildingDirectionFrames.Cells[col, row] := inttostr(((i + 2) div 4) mod 8);
  end;
end;

procedure TStructuresEditor.RedrawBuildingPreview(Sender: TObject);
begin
  if loading then
    exit;
  draw_building_preview(true);
end;

procedure TStructuresEditor.btnBuildingBuildingAnimationPlayClick(Sender: TObject);
begin
  if tmBuildingBuildingAnimation.Enabled then
  begin
    tmBuildingBuildingAnimation.Enabled := false;
    btnBuildingBuildingAnimationPlay.Caption := 'Play';
    draw_building_preview(true);
  end else
  begin
    tmBuildingBuildingAnimation.Enabled := true;
    tmBuildingBuildingAnimation.Tag := 0;
    btnBuildingBuildingAnimationPlay.Caption := 'Stop';
  end;
end;

procedure TStructuresEditor.tmBuildingBuildingAnimationTimer(Sender: TObject);
var
  num_frames: integer;
  anim_speed: integer;
  current_frame: integer;
begin
  if tmBuildingBuildupArt.Enabled or (not cbBuildingFlagHAS_ANIMATION.Checked) then
    exit;
  num_frames := Structures.templates.BuildingAnimationFrames[cbxBuildingBuildingAnimation.ItemIndex];
  anim_speed := seBuildingAnimationSpeed.Value;
  if (num_frames = 0) or (anim_speed = 0) then
    exit;
  if tmBuildingBuildingAnimation.Tag mod anim_speed = 0 then
  begin
    current_frame := (tmBuildingBuildingAnimation.Tag div anim_speed) mod num_frames;
    draw_building_preview(true);
    draw_building_frame(Structures.building_animation_image_indexes[cbxBuildingBuildingAnimation.ItemIndex] + current_frame, cbBuildingFlagANIM_ALPHA.Checked, true);
  end;
  tmBuildingBuildingAnimation.Tag := tmBuildingBuildingAnimation.Tag + 1;
end;

procedure TStructuresEditor.btnBuildingBuildupArtPlayClick(Sender: TObject);
begin
  tmBuildingBuildupArt.Enabled := true;
  tmBuildingBuildupArt.Tag := 0;
end;

procedure TStructuresEditor.tmBuildingBuildupArtTimer(Sender: TObject);
var
  num_frames: integer;
  buildup_frames_to_show: integer;
begin
  num_frames := Structures.templates.BuildupArtFrames[cbxBuildingBuildupArt.ItemIndex];
  buildup_frames_to_show := seBuildingBuildupFramesToShow.Value;
  if tmBuildingBuildupArt.Tag >= num_frames then
  begin
    // Whole animation was played
    tmBuildingBuildupArt.Enabled := false;
    draw_building_preview(true);
    exit;
  end;
  draw_building_preview(tmBuildingBuildupArt.Tag >= buildup_frames_to_show);
  draw_building_frame(Structures.buildup_art_image_indexes[cbxBuildingBuildupArt.ItemIndex] + tmBuildingBuildupArt.Tag, false, true);
  tmBuildingBuildupArt.Tag := tmBuildingBuildupArt.Tag + 1;
end;

procedure TStructuresEditor.lbUnitTypeListClick(Sender: TObject);
begin
  last_unit_type_index := lbUnitTypeList.ItemIndex;
  list_control_groups[LCG_UNIT_TYPES].edit_item_name.Text := Structures.templates.UnitTypeStrings[lbUnitTypeList.ItemIndex];
end;

procedure TStructuresEditor.lbUnitListClick(Sender: TObject);
begin
  store_unit_data;
  fill_unit_data;
end;

procedure TStructuresEditor.btnUnitAddClick(Sender: TObject);
begin
  if Structures.templates.UnitCount = MAX_UNIT_TYPES then
    exit;
  store_data;
  last_unit_index := Structures.templates.UnitCount;
  Inc(Structures.templates.UnitCount);
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.btnUnitRemoveClick(Sender: TObject);
var
  index: integer;
begin
  if Structures.templates.UnitCount = 0 then
    exit;
  store_data;
  index := Structures.templates.UnitCount - 1;
  if last_unit_index = index then
    Dec(last_unit_index);
  Dec(Structures.templates.UnitCount);
  FillChar(Structures.templates.UnitNameStrings[index], Length(Structures.templates.UnitNameStrings[0]), 0);
  FillChar(Structures.templates.UnitDefinitions[index], Sizeof(TUnitTemplate), 0);
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.btnUnitCopyClick(Sender: TObject);
var
  handle: THandle;
  pointer: TUnitClipboardPtr;
  index: integer;
begin
  if lbUnitList.ItemIndex < 0 then
    exit;
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TUnitClipboard));
  pointer := GlobalLock(handle);

  store_data;
  index := lbUnitList.ItemIndex;
  Move(Structures.templates.UnitDefinitions[index], pointer.template, sizeof(TUnitTemplate));
  Move(Structures.templates.UnitNameStrings[index], pointer.name, Length(pointer.name));

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format_unit, handle);
  CloseClipboard;
end;

procedure TStructuresEditor.btnUnitPasteClick(Sender: TObject);
var
  handle: THandle;
  pointer: TUnitClipboardPtr;
  index: integer;
begin
  if (lbUnitList.ItemIndex < 0) or not Clipboard.HasFormat(clipboard_format_unit) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format_unit);
  pointer := GlobalLock(handle);

  index := lbUnitList.ItemIndex;
  Move(pointer.template, Structures.templates.UnitDefinitions[index], sizeof(TUnitTemplate));
  Move(pointer.name, Structures.templates.UnitNameStrings[index], Length(pointer.name));

  fill_data;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TStructuresEditor.edUnitFlagsChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edUnitFlags.Text);
  loading := true;
  cbUnitFlagUF_STEALTH.Checked := (value and UF_STEALTH) <> 0;
  cbUnitFlagUF_NO_AI.Checked := (value and UF_NO_AI) <> 0;
  cbUnitFlagUF_SELFHEALING.Checked := (value and UF_SELFHEALING) <> 0;
  cbUnitFlagUF_FIXED_BARREL.Checked := (value and UF_FIXED_BARREL) <> 0;
  loading := false;
  draw_unit_preview;
end;

procedure TStructuresEditor.UnitFlagCheckboxChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edUnitFlags.Text);
  if (Sender as TCheckBox).Checked then
    value := value or  Cardinal((Sender as TCheckBox).Tag)
  else
    value := value and (not Cardinal((Sender as TCheckBox).Tag));
  loading := true;
  edUnitFlags.Text := IntToHex(value, 8);
  loading := false;
  draw_unit_preview;
end;

procedure TStructuresEditor.btnUnitDirectionFramesClick(Sender: TObject);
var
  i: integer;
  count: integer;
  row, col: integer;
begin
  count := (Sender as TButton).Tag;
  for i := 0 to 31 do
  begin
    row := i div 8;
    col := i mod 8;
    if count = 0 then
      sgUnitDirectionFrames.Cells[col, row] := inttostr(0)
    else if count = 32 then
      sgUnitDirectionFrames.Cells[col, row] := inttostr(i)
    else if count = 8 then
      sgUnitDirectionFrames.Cells[col, row] := inttostr(((i + 2) div 4) mod 8);
  end;
end;

procedure TStructuresEditor.RedrawUnitPreview(Sender: TObject);
begin
  if loading then
    exit;
  draw_unit_preview;
end;

procedure TStructuresEditor.lbBuildingArtListClick(Sender: TObject);
var
  index: integer;
  i: integer;
  first_image_index: integer;
  directions: integer;
  header: TR16EntryHeaderPtr;
  tmp_strings: TStringList;
  frame_name: string;
  selected_frame: integer;
begin
  index := lbBuildingArtList.ItemIndex;
  if index < 0 then
    exit;
  last_building_art_index := index;

  tmp_strings := TStringList.Create;
  first_image_index := Structures.building_art_image_indexes[index];
  directions := Structures.templates.BuildingArtDirections[index];
  for i := 0 to directions * 2 do
  begin
    if i = 0 then
      frame_name := 'Base frame          '
    else if (i <= directions) then
      frame_name := 'Healthy frame ' + inttostr(i-1) + '   '
    else
      frame_name := 'Damaged frame ' + inttostr(i-directions-1);
    tmp_strings.Add(frame_name);
  end;

  selected_frame := 1;
  header := Structures.get_structure_image_header(first_image_index + 1);
  if (header = nil) or (header.EntryType = 0) then
    selected_frame := 0;
  fill_art_control_group_frame_list(ACG_BUIDING_ART, first_image_index, directions * 2 + 1, tmp_strings, selected_frame);
  tmp_strings.Destroy;
  seBuildingArtDirections.Value := directions;
end;

procedure TStructuresEditor.lbBuildingAnimationArtListClick(Sender: TObject);
var
  index: integer;
  first_image_index: integer;
  num_frames: integer;
begin
  index := lbBuildingAnimationArtList.ItemIndex;
  if index < 0 then
    exit;
  last_building_animation_art_index := index;

  first_image_index := Structures.building_animation_image_indexes[index];
  num_frames := Structures.templates.BuildingAnimationFrames[index];
  fill_art_control_group_frame_list(ACG_BUIDING_ANIMATION, first_image_index, num_frames, nil, 0);
  seBuildingAnimationFrames.Value := num_frames;

  first_image_index := Structures.buildup_art_image_indexes[index];
  num_frames := Structures.templates.BuildupArtFrames[index];
  fill_art_control_group_frame_list(ACG_BUIDUP_ART, first_image_index, num_frames, nil, 0);
  seBuildupArtFrames.Value := num_frames;
end;

procedure TStructuresEditor.btnBuildingAnimationArtModifyClick(Sender: TObject);
begin
  store_data;
  Structures.templates.BuildingAnimationFrames[lbBuildingAnimationArtList.ItemIndex] := seBuildingAnimationFrames.Value;
  Structures.templates.BuildupArtFrames[lbBuildingAnimationArtList.ItemIndex] := seBuildupArtFrames.Value;
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.lbBuilExpBuildingListClick(Sender: TObject);
begin
  store_builexp_data;
  fill_builexp_data;
end;

procedure TStructuresEditor.seBuilExpNumAnimationsChange(Sender: TObject);
var
  i: integer;
  value: integer;
  show: boolean;
begin
  if seBuilExpNumAnimations.Text = '' then
    exit;
  value := seBuilExpNumAnimations.Value;
  pnBuilExpAnimations.Visible := value > 0;
  for i := 0 to MAX_BUILEXP_ANIMATIONS - 1 do
  begin
    show := i < value;
    lblBuilExpAnimIndex[i].Visible := show;
    seBuilExpAnimOffsetX[i].Visible := show;
    seBuilExpAnimOffsetY[i].Visible := show;
    cbxBuilExpAnimExplosion[i].Visible := show;
    seBuilExpAnimNumFrames[i].Visible := show;
  end;
  RedrawBuilExpPreview(nil);
end;

procedure TStructuresEditor.RedrawBuilExpPreview(Sender: TObject);
var
  i: integer;
begin
  if loading then
    exit;
  for i := 0 to MAX_BUILEXP_ANIMATIONS - 1 do
  begin
    if seBuilExpAnimOffsetX[i].Text = '' then
      exit;
    if seBuilExpAnimOffsetY[i].Text = '' then
      exit;
    if seBuilExpAnimNumFrames[i].Text = '' then
      exit;
  end;
  draw_builexp_preview;
end;

procedure TStructuresEditor.lbUnitArtListClick(Sender: TObject);
var
  index: integer;
  i, j: integer;
  first_image_index: integer;
  frames, directions: integer;
  tmp_strings: TStringList;
begin
  index := lbUnitArtList.ItemIndex;
  if index < 0 then
    exit;
  last_unit_art_index := index;

  tmp_strings := TStringList.Create;
  first_image_index := Structures.unit_art_image_indexes[index];
  frames := Structures.templates.UnitArtAnimationFrames[index];
  directions := Structures.templates.UnitArtDirectionFrames[index];
  for i := 0 to frames - 1 do
    for j := 0 to directions - 1 do
      tmp_strings.Add(Format('Frame %d  Dir %d ', [i, j]));

  fill_art_control_group_frame_list(ACG_UNIT_ART, first_image_index, frames * directions, tmp_strings, 0);
  tmp_strings.Destroy;
  seUnitArtAnimationFrames.Value := frames;
  seUnitArtDirectionFrames.Value := directions;
end;

procedure TStructuresEditor.lbWeaponListClick(Sender: TObject);
begin
  store_weapon_data;
  fill_weapon_data;
  cbxWeaponProjectileArtChange(nil);
end;

procedure TStructuresEditor.edWeaponFlagsChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edWeaponFlags.Text);
  loading := true;
  cbWeaponFlagWF_BLOCKED_BY_WALL.Checked := (value and WF_BLOCKED_BY_WALL) <> 0;
  cbWeaponFlagWF_DEVIATOR.Checked := (value and WF_DEVIATOR) <> 0;
  cbWeaponFlagWF_SONIC.Checked := (value and WF_SONIC) <> 0;
  cbWeaponFlagWF_FALLING.Checked := (value and WF_FALLING) <> 0;
  cbWeaponFlagWF_ARC_TRAJECTORY.Checked := (value and WF_ARC_TRAJECTORY) <> 0;
  cbWeaponFlagWF_HOMING.Checked := (value and WF_HOMING) <> 0;
  cbWeaponFlagWF_PROJECTILE_ALPHA.Checked := (value and WF_PROJECTILE_ALPHA) <> 0;
  cbWeaponFlagWF_ANIM_PROJECTILE.Checked := (value and WF_ANIM_PROJECTILE) <> 0;
  cbWeaponFlagWF_MAKE_TRAIL.Checked := (value and WF_MAKE_TRAIL) <> 0;
  cbWeaponFlagWF_DEBRIS.Checked := (value and WF_DEBRIS) <> 0;
  loading := false;
end;

procedure TStructuresEditor.WeaponFlagCheckboxChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edWeaponFlags.Text);
  if (Sender as TCheckBox).Checked then
    value := value or  Cardinal((Sender as TCheckBox).Tag)
  else
    value := value and (not Cardinal((Sender as TCheckBox).Tag));
  loading := true;
  edWeaponFlags.Text := IntToHex(value, 8);
  loading := false;
end;

procedure TStructuresEditor.cbxWeaponProjectileArtChange(Sender: TObject);
begin
  lbProjectileArtList.ItemIndex := cbxWeaponProjectileArt.ItemIndex;
  lbProjectileArtListClick(nil);
end;

procedure TStructuresEditor.lbProjectileArtListClick(Sender: TObject);
var
  index: integer;
begin
  index := lbProjectileArtList.ItemIndex;
  if index < 0 then
    exit;
  last_projectile_art_index := index;

  fill_art_control_group_frame_list(ACG_PROJECTILE_ART, Structures.projectile_art_image_indexes[index], Structures.templates.ProjectileArtDirections[index], nil, 0);
  seProjectileArtDirections.Value := Structures.templates.ProjectileArtDirections[index];
end;

procedure TStructuresEditor.lbExplosionListClick(Sender: TObject);
begin
  store_explosion_data;
  fill_explosion_data;
  lbAnimationArtList.ItemIndex := lbExplosionList.ItemIndex;
  lbAnimationArtListClick(nil);
end;

procedure TStructuresEditor.edExplosionFlagsChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edExplosionFlags.Text);
  loading := true;
  cbExplosionFlagEF_ADDITIVE_ALPHA.Checked := (value and EF_ADDITIVE_ALPHA) <> 0;
  cbExplosionFlagEF_SUBSTRACTIVE_ALPA.Checked := (value and EF_SUBSTRACTIVE_ALPA) <> 0;
  cbExplosionFlagEF_SEMI_TRANSPARENCY.Checked := (value and EF_SEMI_TRANSPARENCY) <> 0;
  cbExplosionFlagEF_RISE_UP.Checked := (value and EF_RISE_UP) <> 0;
  cbExplosionFlagEF_HOUSE_COLORED.Checked := (value and EF_HOUSE_COLORED) <> 0;
  cbExplosionFlagEF_FIRING_FLASH.Checked := (value and EF_FIRING_FLASH) <> 0;
  loading := false;
end;

procedure TStructuresEditor.ExplosionFlagCheckboxChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edExplosionFlags.Text);
  if (Sender as TCheckBox).Checked then
    value := value or  Cardinal((Sender as TCheckBox).Tag)
  else
    value := value and (not Cardinal((Sender as TCheckBox).Tag));
  loading := true;
  edExplosionFlags.Text := IntToHex(value, 8);
  loading := false;
end;

procedure TStructuresEditor.lbAnimationArtListClick(Sender: TObject);
var
  index: integer;
begin
  index := lbAnimationArtList.ItemIndex;
  if index < 0 then
    exit;
  last_animation_art_index := index;

  fill_art_control_group_frame_list(ACG_ANIMATION_ART, Structures.animation_art_image_indexes[index], Structures.templates.AnimationArtFrames[index], nil, 0);
  seAnimationArtFrames.Value := Structures.templates.AnimationArtFrames[index];
end;

procedure TStructuresEditor.lbArmourTypeListClick(Sender: TObject);
var
  index: integer;
  i: integer;
  str: string;
begin
  index := lbArmourTypeList.ItemIndex;
  last_armour_type_index := index;
  list_control_groups[LCG_ARMOUR_TYPES].edit_item_name.Text := Structures.armour.ArmourTypeStrings[index];
  str := 'Armour type used by: ';
  for i := 0 to Structures.templates.BuildingCount - 1 do
    if Structures.templates.BuildingDefinitions[i].ArmorType = index then
      str := str + Structures.templates.BuildingNameStrings[i] + '; ';
  for i := 0 to Structures.templates.UnitCount - 1 do
    if Structures.templates.UnitDefinitions[i].ArmorType = index then
      str := str + Structures.templates.UnitNameStrings[i] + '; ';
  lblArmourTypeUsedBy.Caption := str;
end;

procedure TStructuresEditor.lbWarheadListClick(Sender: TObject);
var
  index: integer;
  i: integer;
  str: string;
begin
  index := lbWarheadList.ItemIndex;
  last_warhead_index := index;
  list_control_groups[LCG_WARHEADS].edit_item_name.Text := Structures.armour.WarheadStrings[index];
  str := 'Warhead used by: ';
  for i := 0 to Structures.templates.WeaponCount - 1 do
    if Structures.templates.WeaponDefinitions[i].Warhead = index then
      str := str + Structures.templates.WeaponStrings[i] + '; ';
  lblWarheadUsedBy.Caption := str;
end;

procedure TStructuresEditor.lbSpeedTypeListClick(Sender: TObject);
var
  index: integer;
  i: integer;
  str: string;
begin
  index := lbSpeedTypeList.ItemIndex;
  last_speed_type_index := index;
  edSpeedTypeName.Text := Structures.speed.SpeedNameStrings[index];
  str := 'Used by: ';
  for i := 0 to Structures.templates.UnitCount - 1 do
    if Structures.templates.UnitDefinitions[i].SpeedType = index then
      str := str + Structures.templates.UnitNameStrings[i] + '; ';
  lblSpeedTypeUsedBy.Caption := str;
end;

procedure TStructuresEditor.btnSpeedTypeRenameClick(Sender: TObject);
begin
  store_data;
  store_c_string(edSpeedTypeName.Text, Addr(Structures.speed.SpeedNameStrings[lbSpeedTypeList.ItemIndex]), Length(Structures.speed.SpeedNameStrings[0]));
  fill_data;
end;

procedure TStructuresEditor.vleTemplatesOtherSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  index: integer;
  item_name_list_combo: TComboBox;
begin
  index := ARow - 1;
  item_name_list_combo := nil;
  case Structures.templates_other_byte_types[index] of
    tobtBuilding:  item_name_list_combo := cbxBuildingPrereq1BuildingType;
    tobtUnit:      item_name_list_combo := cbxTemplatesOtherUnitSelect;
    tobtWeapon:    item_name_list_combo := cbxBuildingPrimaryWeapon;
    tobtExplosion: item_name_list_combo := cbxBuildingDeathExplosion;
  end;
  cbxTemplatesOtherSelect.Visible := Structures.templates_other_byte_types[index] <> tobtNone;
  cbxTemplatesOtherSelect.Tag := index;
  if Structures.templates_other_byte_types[index] <> tobtNone then
  begin
    cbxTemplatesOtherSelect.Items := item_name_list_combo.Items;
    cbxTemplatesOtherSelect.ItemIndex := Structures.templates.Other[index] + 1;
    cbxTemplatesOtherSelect.Top := (ARow - vleTemplatesOther.TopRow + 1) * 20 + 1;
  end;
end;

procedure TStructuresEditor.vleTemplatesOtherTopLeftChanged(Sender: TObject);
begin
  cbxTemplatesOtherSelect.Visible := (Structures.templates_other_byte_types[cbxTemplatesOtherSelect.Tag] <> tobtNone) and (vleTemplatesOther.Row >= vleTemplatesOther.TopRow) and (vleTemplatesOther.Row < vleTemplatesOther.TopRow + vleTemplatesOther.VisibleRowCount);
  cbxTemplatesOtherSelect.Top := (vleTemplatesOther.Row - vleTemplatesOther.TopRow + 1) * 20 + 1;
end;

procedure TStructuresEditor.cbxTemplatesOtherSelectChange(Sender: TObject);
begin
  Structures.templates.Other[cbxTemplatesOtherSelect.Tag] := cbxTemplatesOtherSelect.ItemIndex - 1;
  vleTemplatesOther.Cells[1, cbxTemplatesOtherSelect.Tag + 1] := get_templates_other_cell_text(cbxTemplatesOtherSelect.Tag, cbxTemplatesOtherSelect.ItemIndex - 1);
end;

procedure TStructuresEditor.rgTechposTechLevelClick(Sender: TObject);
var
  tech: integer;
  i: integer;
  dummy: boolean;
begin
  tech := rgTechposTechLevel.ItemIndex;
  for i := 0 to 9 do
  begin
    sgTechposData.Cells[1, i + 1] := inttostr(Structures.techpos[tech, i].PosX);
    sgTechposData.Cells[2, i + 1] := inttostr(Structures.techpos[tech, i].PosY);
    sgTechposData.Cells[3, i + 1] := cbxTechposUnitType.Items[Structures.techpos[tech, i].Units[0] + 1];
    sgTechposData.Cells[4, i + 1] := cbxTechposUnitType.Items[Structures.techpos[tech, i].Units[1] + 1];
    sgTechposData.Cells[5, i + 1] := cbxTechposUnitType.Items[Structures.techpos[tech, i].Units[2] + 1];
  end;
  sgTechposDataSelectCell(nil, sgTechposData.Col, sgTechposData.Row, dummy);
  draw_techpos_preview;
end;

procedure TStructuresEditor.sgTechposDataSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  side, unitnum, tech: integer;
begin
  cbxTechposUnitType.Visible := ACol >= 3;
  if ACol < 3 then
    exit;
  side := ACol - 3;
  unitnum := ARow - 1;
  tech := rgTechposTechLevel.ItemIndex;
  cbxTechposUnitType.Left := 124 + side * 134;
  cbxTechposUnitType.Top := 301 + unitnum * 20;
  cbxTechposUnitType.ItemIndex := Structures.techpos[tech, unitnum].Units[side] + 1;
  cbxTechposUnitType.Tag := side * 10 + unitnum;
end;

procedure TStructuresEditor.sgTechposDataSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
var
  tech, unitnum: integer;
  int_value: Shortint;
begin
  if ACol >= 3 then
    exit;
  int_value := StrToIntDef(Value, 0);
  tech := rgTechposTechLevel.ItemIndex;
  unitnum := ARow - 1;
  if ACol = 1 then
    Structures.techpos[tech, unitnum].PosX := int_value
  else
    Structures.techpos[tech, unitnum].PosY := int_value;
  draw_techpos_preview;
end;

procedure TStructuresEditor.cbxTechposUnitTypeChange(Sender: TObject);
var
  side, unitnum, tech: integer;
begin
  side := cbxTechposUnitType.Tag div 10;
  unitnum := cbxTechposUnitType.Tag mod 10;
  tech := rgTechposTechLevel.ItemIndex;
  Structures.techpos[tech, unitnum].Units[side] := cbxTechposUnitType.ItemIndex - 1;
  sgTechposData.Cells[3 + side, 1 + unitnum] := cbxTechposUnitType.Items[Structures.techpos[tech, unitnum].Units[side] + 1];
  draw_techpos_preview;
end;

procedure TStructuresEditor.TechposPreviewChange(Sender: TObject);
begin
  lblTechposNumUnits.Caption := 'Number of units to show: ' + inttostr(tbTechposNumUnits.Position);
  draw_techpos_preview;
end;

procedure TStructuresEditor.imgTechposPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
  i: integer;
  side, unitnum, tech: integer;
begin
  pos_x := (X div 32) - (TECHPOS_PREVIEW_SIZE div 2);
  pos_y := (Y div 32) - (TECHPOS_PREVIEW_SIZE div 2);
  if sbTechposAtreides.Down then
    side := 0
  else if sbTechposHarkonnen.Down then
    side := 1
  else
    side := 2;
  tech := rgTechposTechLevel.ItemIndex;
  unitnum := cbxTechposUnitType.Tag mod 10;
  for i := 0 to 9 do
    if (Structures.techpos[tech, i].PosX = pos_x) and (Structures.techpos[tech, i].PosY = pos_y) then
    begin
      unitnum := i;
      break;
    end;
  sgTechposData.Col := 3 + side;
  sgTechposData.Row := 1 + unitnum;
  cbxTechposUnitType.SetFocus;
end;

procedure TStructuresEditor.ListControlGroupAddClick(Sender: TObject);
var
  group_index: integer;
  lcg: TListControlGroupPtr;
begin
  group_index := (Sender as TButton).Tag;
  lcg := Addr(list_control_groups[group_index]);
  if lcg.item_count_byte_ptr^ = lcg.max_item_count then
    exit;
  store_data;
  // Store item name
  store_c_string(lcg.edit_item_name.Text, Addr(lcg.item_name_list_ptr[lcg.item_count_byte_ptr^]), 50);
  // Make the newly added item selected in the list box
  lcg.last_item_index_ptr^ := lcg.item_count_byte_ptr^;
  // Set default explosion data
  if group_index = LCG_EXPLOSIONS then
  begin
    Structures.templates.ExplosionDefinitions[lcg.item_count_byte_ptr^].Sound := -1;
    Structures.templates.ExplosionDefinitions[lcg.item_count_byte_ptr^].MyIndex := lcg.item_count_byte_ptr^;
  end;
  // Increase item count by 1
  Inc(lcg.item_count_byte_ptr^);
  fill_data;
end;

procedure TStructuresEditor.ListControlGroupRemoveClick(Sender: TObject);
var
  group_index: integer;
  lcg: TListControlGroupPtr;
begin
  group_index := (Sender as TButton).Tag;
  lcg := Addr(list_control_groups[group_index]);
  if lcg.item_count_byte_ptr^ = 0 then
    exit;
  store_data;
  // Decrease item count by 1
  Dec(lcg.item_count_byte_ptr^);
  // Erase item name
  FillChar(lcg.item_name_list_ptr[lcg.item_count_byte_ptr^], 50, 0);
  // If this item was selected last time in the list box, move to the previous item
  if lcg.last_item_index_ptr^ = lcg.item_count_byte_ptr^ then
    Dec(lcg.last_item_index_ptr^);
  // Erase weapon or explosion data
  if group_index = LCG_WEAPONS then
    FillChar(Structures.templates.WeaponDefinitions[lcg.item_count_byte_ptr^], sizeof(TWeaponTemplate), 0);
  if group_index = LCG_EXPLOSIONS then
  begin
    FillChar(Structures.templates.ExplosionDefinitions[lcg.item_count_byte_ptr^], sizeof(TExplosionTemplate), 0);
    Structures.templates.AnimationArtFlags[lcg.item_count_byte_ptr^] := 0;
  end;
  fill_data;
end;

procedure TStructuresEditor.ListControlGroupRenameClick(Sender: TObject);
var
  group_index: integer;
  lcg: TListControlGroupPtr;
begin
  group_index := (Sender as TButton).Tag;
  lcg := Addr(list_control_groups[group_index]);
  store_data;
  // Store new item name
  store_c_string(lcg.edit_item_name.Text, Addr(lcg.item_name_list_ptr[lcg.list_control.ItemIndex]), 50);
  fill_data;
end;

procedure TStructuresEditor.ArtControlGroupFrameListClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
  image_index: integer;
  header: TR16EntryHeaderPtr;
begin
  group_index := (Sender as TListBox).Tag;
  acg := Addr(art_control_groups[group_index]);
  image_index := 0;
  header := nil;
  if acg.frame_list.ItemIndex <> -1 then
  begin
    image_index := acg.first_image_index + acg.frame_list.ItemIndex;
    header := Structures.get_structure_image_header(image_index);
  end;
  if (header <> nil) and (header.EntryType <> 0) then
  begin
    if acg.is_unit then
      draw_unit_art_frame(acg.view_image, image_index)
    else
      draw_building_art_frame(acg.view_image, image_index, true);
    acg.edit_frame_width.Text := inttostr(header.FrameWidth);
    acg.edit_frame_height.Text := inttostr(header.FrameHeight);
    acg.edit_image_width.Text := inttostr(header.ImageWidth);
    acg.edit_image_height.Text := inttostr(header.ImageHeight);
    acg.edit_image_offset_x.Text := inttostr(IfThen(acg.is_unit, header.ImageOffsetX, header.ImageOffsetX * -1));
    acg.edit_image_offset_y.Text := inttostr(header.ImageOffsetY);
  end else
  begin
    draw_no_image_sign(acg.view_image);
    acg.edit_frame_width.Text := '';
    acg.edit_frame_height.Text := '';
    acg.edit_image_width.Text := '';
    acg.edit_image_height.Text := '';
    acg.edit_image_offset_x.Text := '';
    acg.edit_image_offset_y.Text := '';
  end;
end;

procedure TStructuresEditor.ArtControlGroupAddClick(Sender: TObject);
var
  group_index: integer;
begin
  group_index := (Sender as TButton).Tag;
  store_data;
  case group_index of
    ACG_BUIDING_ART:
      if Structures.templates.BuildingArtCount < MAX_BUILDING_ART then
      begin
        Structures.templates.BuildingArtDirections[Structures.templates.BuildingArtCount] := seBuildingArtDirections.Value;
        last_building_art_index := Structures.templates.BuildingArtCount;
        Inc(Structures.templates.BuildingArtCount);
      end;
    ACG_UNIT_ART:
      if Structures.templates.UnitArtCount < MAX_UNIT_ART then
      begin
        Structures.templates.UnitArtAnimationFrames[Structures.templates.UnitArtCount] := seUnitArtAnimationFrames.Value;
        Structures.templates.UnitArtDirectionFrames[Structures.templates.UnitArtCount] := seUnitArtDirectionFrames.Value;
        last_unit_art_index := Structures.templates.UnitArtCount;
        Inc(Structures.templates.UnitArtCount);
      end;
    ACG_PROJECTILE_ART:
      if Structures.templates.ProjectileArtCount < MAX_WEAPONS then
      begin
        Structures.templates.ProjectileArtDirections[Structures.templates.ProjectileArtCount] := seProjectileArtDirections.Value;
        last_projectile_art_index := Structures.templates.ProjectileArtCount;
        Inc(Structures.templates.ProjectileArtCount);
      end;
    ACG_ANIMATION_ART:
      if Structures.templates.AnimationArtCount < MAX_EXPLOSIONS then
      begin
        Structures.templates.AnimationArtFrames[Structures.templates.AnimationArtCount] := seAnimationArtFrames.Value;
        last_animation_art_index := Structures.templates.AnimationArtCount;
        Inc(Structures.templates.AnimationArtCount);
      end;
  end;
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.ArtControlGroupRemoveClick(Sender: TObject);
var
  group_index: integer;
begin
  group_index := (Sender as TButton).Tag;
  store_data;
  case group_index of
    ACG_BUIDING_ART:
      if Structures.templates.BuildingArtCount > 0 then
      begin
        Dec(Structures.templates.BuildingArtCount);
        Structures.templates.BuildingArtDirections[Structures.templates.BuildingArtCount] := 0;
        if last_building_art_index = Structures.templates.BuildingArtCount then
          Dec(last_building_art_index);
      end;
    ACG_UNIT_ART:
      if Structures.templates.UnitArtCount > 0 then
      begin
        Dec(Structures.templates.UnitArtCount);
        Structures.templates.UnitArtAnimationFrames[Structures.templates.UnitArtCount] := 0;
        Structures.templates.UnitArtDirectionFrames[Structures.templates.UnitArtCount] := 0;
        if last_unit_art_index = Structures.templates.UnitArtCount then
          Dec(last_unit_art_index);
      end;
    ACG_PROJECTILE_ART:
      if Structures.templates.ProjectileArtCount > 0 then
      begin
        Dec(Structures.templates.ProjectileArtCount);
        Structures.templates.ProjectileArtDirections[Structures.templates.ProjectileArtCount] := 0;
        if last_projectile_art_index = Structures.templates.ProjectileArtCount then
          Dec(last_projectile_art_index);
      end;
    ACG_ANIMATION_ART:
      if Structures.templates.AnimationArtCount > 0 then
      begin
        Dec(Structures.templates.AnimationArtCount);
        Structures.templates.AnimationArtFrames[Structures.templates.AnimationArtCount] := 0;
        if last_animation_art_index = Structures.templates.AnimationArtCount then
          Dec(last_animation_art_index);
      end;
  end;
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.ArtControlGroupModifyClick(Sender: TObject);
var
  group_index: integer;
begin
  group_index := (Sender as TButton).Tag;
  store_data;
  case group_index of
    ACG_BUIDING_ART:
      Structures.templates.BuildingArtDirections[lbBuildingArtList.ItemIndex] := seBuildingArtDirections.Value;
    ACG_UNIT_ART:
      begin
        Structures.templates.UnitArtAnimationFrames[lbUnitArtList.ItemIndex] := seUnitArtAnimationFrames.Value;
        Structures.templates.UnitArtDirectionFrames[lbUnitArtList.ItemIndex] := seUnitArtDirectionFrames.Value;
      end;
    ACG_PROJECTILE_ART:
      Structures.templates.ProjectileArtDirections[lbProjectileArtList.ItemIndex] := seProjectileArtDirections.Value;
    ACG_ANIMATION_ART:
      Structures.templates.AnimationArtFrames[lbAnimationArtList.ItemIndex] := seAnimationArtFrames.Value;
  end;
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.fill_data;
var
  tmp_strings: TStringList;
  i: integer;
  j: integer;
begin
  tmp_strings := TStringList.Create;

  // Building type list
  for i := 0 to Structures.templates.BuildingTypeCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.BuildingTypeStrings[i]]));
  lbBuildingTypeList.Items := tmp_strings;
  cbxBuildingType.Items := tmp_strings;
  tmp_strings.Insert(0, '(none)');
  cbxBuildingPrereq1BuildingType.Items := tmp_strings;
  cbxBuildingPrereq2BuildingType.Items := tmp_strings;
  cbxUnitPrereq1BuildingType.Items := tmp_strings;
  cbxUnitPrereq2BuildingType.Items := tmp_strings;
  lbBuildingTypeList.ItemIndex := last_building_type_index;
  lbBuildingTypeListClick(nil);

  // Building name list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.BuildingNameStrings[i]]));
  lbBuildingList.Items := tmp_strings;
  lbBuildingList.ItemIndex := last_building_index;

  // Unit type list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.UnitTypeCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.UnitTypeStrings[i]]));
  lbUnitTypeList.Items := tmp_strings;
  cbxUnitType.Items := tmp_strings;
  tmp_strings.Insert(0, '(none)');
  cbxTechposUnitType.Items := tmp_strings;
  lbUnitTypeList.ItemIndex := last_unit_type_index;
  lbUnitTypeListClick(nil);

  // Unit name list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.UnitCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.UnitNameStrings[i]]));
  lbUnitList.Items := tmp_strings;
  tmp_strings.Insert(0, '(none)');
  cbxTemplatesOtherUnitSelect.Items := tmp_strings;
  lbUnitList.ItemIndex := last_unit_index;

  // Building art list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingArtCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%dd)', [2, i, Structures.building_art_image_indexes[i], Structures.templates.BuildingArtDirections[i]]));
  lbBuildingArtList.Items := tmp_strings;
  tmp_strings.Insert(0, '(none)');
  cbxBuildingBuildingArt.Items := tmp_strings;
  cbxBuildingBarrelArt.Items := tmp_strings;
  lbBuildingArtList.ItemIndex := last_building_art_index;
  lbBuildingArtListClick(nil);

  // Building animation art list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('[ %.*d / %.*d ] %.*d %s', [2, Structures.templates.BuildingAnimationFrames[i], 2, Structures.templates.BuildupArtFrames[i], 2, i, Structures.templates.BuildingNameStrings[i]]));
  lbBuildingAnimationArtList.Items := tmp_strings;
  lbBuildingAnimationArtList.ItemIndex := last_building_animation_art_index;
  lbBuildingAnimationArtListClick(nil);

  // Building animation selection
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%df)', [2, i, Structures.building_animation_image_indexes[i], Structures.templates.BuildingAnimationFrames[i]]));
  cbxBuildingBuildingAnimation.Items := tmp_strings;

  // Buildup art selection
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%df)', [2, i, Structures.buildup_art_image_indexes[i], Structures.templates.BuildupArtFrames[i]]));
  cbxBuildingBuildupArt.Items := tmp_strings;

  // BuilExp building list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('[ %d ] %.*d %s', [Structures.builexp[i].NumAnimations, 2, i, Structures.templates.BuildingNameStrings[i]]));
  lbBuilExpBuildingList.Items := tmp_strings;
  lbBuilExpBuildingList.ItemIndex := last_builexp_building_index;

  // Unit art list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.UnitArtCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%dd %df)', [2, i, Structures.unit_art_image_indexes[i], Structures.templates.UnitArtDirectionFrames[i], Structures.templates.UnitArtAnimationFrames[i]]));
  lbUnitArtList.Items := tmp_strings;
  tmp_strings.Insert(0, '(none)');
  cbxUnitUnitArt.Items := tmp_strings;
  cbxUnitBarrelArt.Items := tmp_strings;
  lbUnitArtList.ItemIndex := last_unit_art_index;
  lbUnitArtListClick(nil);

  // Weapon list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.WeaponCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.templates.WeaponStrings[i]]));
  lbWeaponList.Items := tmp_strings;
  tmp_strings.Insert(0, '(none)');
  cbxBuildingPrimaryWeapon.Items := tmp_strings;
  cbxBuildingSecondaryWeapon.Items := tmp_strings;
  cbxUnitPrimaryWeapon.Items := tmp_strings;
  cbxUnitSecondaryWeapon.Items := tmp_strings;
  lbWeaponList.ItemIndex := last_weapon_index;

  // Projectile art list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.ProjectileArtCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%dd)', [2, i, Structures.projectile_art_image_indexes[i], Structures.templates.ProjectileArtDirections[i]]));
  lbProjectileArtList.Items := tmp_strings;
  cbxWeaponProjectileArt.Items := tmp_strings;
  lbProjectileArtList.ItemIndex := last_projectile_art_index;
  lbProjectileArtListClick(nil);

  // Explosion list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.ExplosionCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.templates.ExplosionStrings[i]]));
  lbExplosionList.Items := tmp_strings;
  for i := 0 to MAX_BUILEXP_ANIMATIONS - 1 do
    cbxBuilExpAnimExplosion[i].Items := tmp_strings;
  tmp_strings.Insert(0, '(none)');
  cbxBuildingDeathExplosion.Items := tmp_strings;
  cbxBuildingFiringExplosion.Items := tmp_strings;
  cbxUnitDeathExplosion.Items := tmp_strings;
  cbxUnitFiringExplosion.Items := tmp_strings;
  cbxWeaponHitExplosion.Items := tmp_strings;
  cbxWeaponTrailExplosion.Items := tmp_strings;
  lbExplosionList.ItemIndex := last_explosion_index;

  // Animation art list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.AnimationArtCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%df)', [2, i, Structures.animation_art_image_indexes[i], Structures.templates.AnimationArtFrames[i]]));
  lbAnimationArtList.Items := tmp_strings;
  lbAnimationArtList.ItemIndex := last_animation_art_index;
  lbAnimationArtListClick(nil);

  // Armour type list
  tmp_strings.Clear;
  for i := 0 to Structures.armour.ArmourTypeCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.armour.ArmourTypeStrings[i]]));
  lbArmourTypeList.Items := tmp_strings;
  cbxBuildingArmorType.Items := tmp_strings;
  cbxUnitArmorType.Items := tmp_strings;
  lbArmourTypeList.ItemIndex := last_armour_type_index;
  lbArmourTypeListClick(nil);

  // Warhead list
  tmp_strings.Clear;
  for i := 0 to Structures.armour.WarheadCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.armour.WarheadStrings[i]]));
  lbWarheadList.Items := tmp_strings;
  cbxWeaponWarhead.Items := tmp_strings;
  lbWarheadList.ItemIndex := last_warhead_index;
  lbWarheadListClick(nil);

  // Speed type list
  tmp_strings.Clear;
  for i := 0 to Length(Structures.speed.SpeedNameStrings) - 1 do
    tmp_strings.Add(Format('%.*d - %s', [1, i, Structures.speed.SpeedNameStrings[i]]));
  lbSpeedTypeList.Items := tmp_strings;
  cbxUnitSpeedType.Items := tmp_strings;
  lbSpeedTypeList.ItemIndex := last_speed_type_index;
  lbSpeedTypeListClick(nil);

  // Fill template data
  fill_building_data;
  fill_unit_data;
  fill_builexp_data;
  fill_weapon_data;
  fill_explosion_data;

  // Armour
  sgArmourValues.RowCount := Structures.armour.WarheadCount + 1;
  for i := 0 to Length(Structures.armour.ArmourTypeStrings) - 1 do
    sgArmourValues.Cells[i+1, 0] := Structures.prettify_structure_name(Structures.armour.ArmourTypeStrings[i]);
  for i := 0 to Structures.armour.WarheadCount - 1 do
    sgArmourValues.Cells[0, i+1] := Structures.armour.WarheadStrings[i];
  for i := 0 to Structures.armour.WarheadCount - 1 do
  begin
    for j := 0 to Length(Structures.armour.WarheadEntries[i].VersusArmorType) - 1 do
      sgArmourValues.Cells[j + 1, i + 1] := IntToStr(Structures.armour.WarheadEntries[i].VersusArmorType[j]);
    sgArmourValues.Cells[13, i + 1] := IntToStr(Structures.armour.WarheadEntries[i].Unknown12);
    sgArmourValues.Cells[14, i + 1] := IntToStr(Structures.armour.WarheadEntries[i].Unknown16);
  end;
  // Speed
  for i := 0 to Length(Structures.speed.SpeedNameStrings) - 1 do
    sgSpeedValues.Cells[i+1, 0] := Structures.speed.SpeedNameStrings[i];
  for i := 0 to Length(Structures.speed.Values) - 1 do
    for j := 0 to Length(Structures.speed.Values[i]) - 1 do
      sgSpeedValues.Cells[j+1, i+1] := floattostr(Round(Structures.speed.Values[i, j] * 100)/100);
  // Other
  tmp_strings.Clear;
  for i := 0 to Length(Structures.templates.Other) - 1 do
    tmp_strings.Add(Format('%s=%s', [Structures.templates_other[i], get_templates_other_cell_text(i, Structures.templates.Other[i])]));
  vleTemplatesOther.Strings := tmp_strings;
  vleTemplatesOther.Col := 0;
  // Techpos
  rgTechposTechLevelClick(nil);

  tmp_strings.Destroy;

  // Status bar
  StatusBar.Panels[0].Text := Structures.templates_bin_filename;
end;

procedure TStructuresEditor.fill_building_data;
var
  index: integer;
  i: integer;
  bld: TBuildingTemplatePtr;
  icon: TStructureImagePtr;
  was_already_loaded: boolean;
begin
  index := lbBuildingList.ItemIndex;
  if index < 0 then
    exit;
  loading := true;
  last_building_index := index;
  bld := Addr(Structures.templates.BuildingDefinitions[index]);
  // Basic group box
  edBuildingName.Text := Structures.templates.BuildingNameStrings[index];
  cbxBuildingType.ItemIndex := bld.BuildingType;
  icon := Structures.get_structure_image(Structures.first_building_icon_image_index + index, 0, false, false, was_already_loaded);
  if icon <> nil then
  begin
    imgBuildingIcon.Picture.Bitmap.Assign(icon.bitmap);
    Structures.clear_last_structure_image(Structures.first_building_icon_image_index + index, false);
  end else
    draw_no_image_sign(imgBuildingIcon);
  set_owner_side_field_value(clbBuildingOwnerSide, bld.OwnerSide);
  // Build requirements group box
  seBuildingTechLevelBuild.Value := bld.TechLevelBuild;
  seBuildingTechLevelUpgrade1.Value := bld.TechLevelUpgrade1;
  seBuildingTechLevelUpgrade2.Value := bld.TechLevelUpgrade2;
  seBuildingTechLevelUpgrade3.Value := bld.TechLevelUpgrade3;
  edBuildingCostBuild.Text := inttostr(bld.CostBuild);
  edBuildingCostUpgrade1.Text := inttostr(bld.CostUpgrade1);
  edBuildingCostUpgrade2.Text := inttostr(bld.CostUpgrade2);
  edBuildingCostUpgrade3.Text := inttostr(bld.CostUpgrade3);
  edBuildingBuildSpeedBuild.Text := inttostr(bld.BuildSpeedBuild);
  edBuildingBuildSpeedUpgrade1.Text := inttostr(bld.BuildSpeedUpgrade1);
  edBuildingBuildSpeedUpgrade2.Text := inttostr(bld.BuildSpeedUpgrade2);
  edBuildingBuildSpeedUpgrade3.Text := inttostr(bld.BuildSpeedUpgrade3);
  cbxBuildingPrereq1BuildingType.ItemIndex := bld.Prereq1BuildingType + 1;
  set_owner_side_field_value(clbBuildingPrereq1OwnerSide, bld.Prereq1OwnerSide);
  seBuildingPrereq1UpgradesNeeded.Value := bld.Prereq1UpgradesNeeded;
  cbxBuildingPrereq2BuildingType.ItemIndex := bld.Prereq2BuildingType + 1;
  set_owner_side_field_value(clbBuildingPrereq2OwnerSide, bld.Prereq2OwnerSide);
  seBuildingPrereq2UpgradesNeeded.Value := bld.Prereq2UpgradesNeeded;
  // Properties and behavior group box
  edBuildingHitPoints.Text := inttostr(bld.HitPoints);
  cbxBuildingArmorType.ItemIndex := bld.ArmorType;
  edBuildingPowerConsumption.Text := inttostr(bld.PowerConsumption);
  cbxBuildingHealthBarSize.ItemIndex := bld.HealthBarSize;
  seBuildingSightRadius.Value := bld.SightRadius;
  cbxBuildingSpecialBehavior.ItemIndex := bld.SpecialBehavior;
  // Turret properties group box
  cbBuildingActLikeTurret.Checked := bld.ActLikeTurret <> 0;
  cbBuildingRequireEnoughPower.Checked := bld.RequireEnoughPower <> 0;
  cbxBuildingPrimaryWeapon.ItemIndex := bld.PrimaryWeapon + 1;
  cbxBuildingSecondaryWeapon.ItemIndex := bld.SecondaryWeapon + 1;
  seBuildingRateOfFire.Value := bld.RateOfFire;
  seBuildingBarrelRotationSpeed.Value := bld.BarrelRotationSpeed;
  // Visuals and animations group box
  cbxBuildingBuildingArt.ItemIndex := bld.BuildingArt + 1;
  cbxBuildingBarrelArt.ItemIndex := bld.BarrelArt + 1;
  edBuildingArtWidth.Text := inttostr(bld.ArtWidth);
  edBuildingArtHeight.Text := inttostr(bld.ArtHeight);
  cbxBuildingBuildingAnimation.ItemIndex := bld.BuildingAnimation;
  seBuildingAnimationSpeed.Value := bld.AnimationSpeed;
  cbxBuildingBuildupArt.ItemIndex := bld.BuildupArt;
  seBuildingBuildupFramesToShow.Value := bld.BuildupFramesToShow;
  cbxBuildingDeathExplosion.ItemIndex := bld.DeathExplosion + 1;
  cbxBuildingFiringExplosion.ItemIndex := bld.FiringExplosion + 1;
  for i := 0 to 31 do
    sgBuildingDirectionFrames.Cells[i mod 8, i div 8] := inttostr(bld.DirectionFrames[i]);
  // Space requirements group box
  tmp_building_tiles_occupied_all := bld.TilesOccupiedAll;
  draw_building_tile_map(imgBuildingTilesOccupiedAll, bld.TilesOccupiedAll);
  tmp_building_tiles_occupied_solid := bld.TilesOccupiedSolid;
  draw_building_tile_map(imgBuildingTilesOccupiedSolid, bld.TilesOccupiedSolid);
  seBuildingExitPoint1X.Value := bld.ExitPoint1X;
  seBuildingExitPoint1Y.Value := bld.ExitPoint1Y;
  seBuildingExitPoint2X.Value := bld.ExitPoint2X;
  seBuildingExitPoint2Y.Value := bld.ExitPoint2Y;
  // Others and unknown group box
  seBuildingUnknown8.Value := bld.Unknown8;
  seBuildingUnknown93.Value := bld.Unknown93;
  edBuildingFlags.Text := IntToHex(bld.Flags, 8);

  loading := false;
  edBuildingFlagsChange(nil);
end;

procedure TStructuresEditor.fill_unit_data;
var
  index: integer;
  i: integer;
  unt: TUnitTemplatePtr;
  icon: TStructureImagePtr;
  was_already_loaded: boolean;
begin
  index := lbUnitList.ItemIndex;
  if index < 0 then
    exit;
  last_unit_index := index;
  loading := true;
  unt := Addr(Structures.templates.UnitDefinitions[index]);
  // Basic group box
  edUnitName.Text := Structures.templates.UnitNameStrings[index];
  cbxUnitType.ItemIndex := unt.UnitType;
  icon := Structures.get_structure_image(Structures.first_unit_icon_image_index + index, 0, false, false, was_already_loaded);
  if icon <> nil then
  begin
    imgUnitIcon.Picture.Bitmap.Assign(icon.bitmap);
    Structures.clear_last_structure_image(Structures.first_unit_icon_image_index + index, false);
  end else
    draw_no_image_sign(imgUnitIcon);
  set_owner_side_field_value(clbUnitOwnerSide, unt.OwnerSide);
  // Build requirements group box
  seUnitTechLevel.Value := unt.TechLevel;
  edUnitCost.Text := inttostr(unt.Cost);
  edUnitBuildSpeed.Text := inttostr(unt.BuildSpeed);
  cbxUnitPrereq1BuildingType.ItemIndex := unt.Prereq1BuildingType + 1;
  set_owner_side_field_value(clbUnitPrereq1OwnerSide, unt.Prereq1OwnerSide);
  seUnitPrereq1UpgradesNeeded.Value := unt.Prereq1UpgradesNeeded;
  cbxUnitPrereq2BuildingType.ItemIndex := unt.Prereq2BuildingType + 1;
  cbUnitAvailableInStarport.Checked := unt.AvailableInStarport <> 0;
  cbUnitMultiplayerOnly.Checked := unt.MultiplayerOnly <> 0;
  // Voices group box
  for i := 0 to Length(unt.Voices) - 1 do
    cbxUnitVoices[i].ItemIndex := unt.Voices[i];
  // Properties and behavior group box
  edUnitHitPoints.Text := inttostr(unt.HitPoints);
  cbxUnitArmorType.ItemIndex := unt.ArmorType;
  seUnitSightRadius.Value := unt.SightRadius;
  cbxUnitHealthBarSize.ItemIndex := unt.HealthBarSize;
  cbUnitIsInfantry.Checked := unt.IsInfantry <> 0;
  cbxUnitSpecialBehavior.ItemIndex := unt.SpecialBehavior;
  cbUnitCanCrushInfantry.Checked := unt.CanCrushInfantry <> 0;
  // Movement group box
  edUnitSpeed.Text := inttostr(unt.Speed shr 12);
  cbxUnitSpeedType.ItemIndex := unt.SpeedType;
  seUnitUnitRotationSpeed.Value := unt.UnitRotationSpeed;
  // Weapons group box
  cbxUnitPrimaryWeapon.ItemIndex := unt.PrimaryWeapon + 1;
  cbxUnitSecondaryWeapon.ItemIndex := unt.SecondaryWeapon + 1;
  cbUnitHasBarrel.Checked := unt.HasBarrel <> 0;
  seUnitRateOfFire.Value := unt.RateOfFire;
  seUnitBarrelRotationSpeed.Value := unt.BarrelRotationSpeed;
  // Visuals and animations group box
  cbxUnitUnitArt.ItemIndex := unt.UnitArt + 1;
  cbxUnitBarrelArt.ItemIndex := unt.BarrelArt + 1;
  cbxUnitDeathExplosion.ItemIndex := unt.DeathExplosion + 1;
  cbxUnitFiringExplosion.ItemIndex := unt.FiringExplosion + 1;
  for i := 0 to 31 do
    sgUnitDirectionFrames.Cells[i mod 8, i div 8] := inttostr(unt.DirectionFrames[i]);
  // Others and unknown group box
  seUnitUnknown46.Value := unt.Unknown46;
  seUnitUnknown52.Value := unt.Unknown52;
  seUnitUnknown55.Value := unt.Unknown55;
  seUnitUnknown164.Value := unt.Unknown164;
  edUnitFlags.Text := IntToHex(unt.Flags, 8);

  loading := false;
  edUnitFlagsChange(nil);
end;

procedure TStructuresEditor.fill_builexp_data;
var
  index: integer;
  i: integer;
  bxp: TBuilExpEntryPtr;
begin
  index := lbBuilExpBuildingList.ItemIndex;
  if index < 0 then
    exit;
  last_builexp_building_index := index;
  loading := true;
  bxp := Addr(Structures.builexp[index]);

  seBuilExpNumAnimations.Value := bxp.NumAnimations;
  for i := 0 to MAX_BUILEXP_ANIMATIONS - 1 do
  begin
    seBuilExpAnimOffsetX[i].Value := bxp.AnimOffsetX[i];
    seBuilExpAnimOffsetY[i].Value := bxp.AnimOffsetY[i];
    cbxBuilExpAnimExplosion[i].ItemIndex := bxp.AnimExplosion[i];
    seBuilExpAnimNumFrames[i].Value := bxp.AnimNumFrames[i];
  end;

  loading := false;
  draw_builexp_preview;
end;

procedure TStructuresEditor.fill_weapon_data;
var
  index: integer;
  wpn: TWeaponTemplatePtr;
  str: String;
  i: integer;
begin
  index := lbWeaponList.ItemIndex;
  if index < 0 then
    exit;
  last_weapon_index := index;
  loading := true;
  wpn := Addr(Structures.templates.WeaponDefinitions[index]);

  list_control_groups[LCG_WEAPONS].edit_item_name.Text := Structures.templates.WeaponStrings[index];
  // Properties and behavior tab
  edWeaponDamage.Text := IntToStr(wpn.Damage);
  cbxWeaponWarhead.ItemIndex := wpn.Warhead;
  edWeaponRange.Text := IntToStr(wpn.Range);
  cbWeaponAntiAircraft.Checked := wpn.AntiAircraft <> 0;
  // Projectile movement tab
  edWeaponProjectileSpeed.Text := IntToStr(wpn.ProjectileSpeed shr 10);
  // Visuals and sounds tab
  cbxWeaponProjectileArt.ItemIndex := wpn.ProjectileArt;
  cbxWeaponFiringSound.ItemIndex := wpn.FiringSound + 1;
  cbxWeaponHitExplosion.ItemIndex := wpn.HitExplosion + 1;
  cbxWeaponTrailExplosion.ItemIndex := wpn.TrailExplosion + 1;
  // Others and unknown tab
  seWeaponUnknown19.Value := wpn.Unknown19;
  edWeaponFlags.Text := IntToHex(wpn.Flags, 8);
  // Used by label
  str := 'Used by: ';
  for i := 0 to Structures.templates.BuildingCount - 1 do
  begin
    if Structures.templates.BuildingDefinitions[i].PrimaryWeapon = index then
      str := str + Structures.templates.BuildingNameStrings[i] + ' (pri) ';
    if Structures.templates.BuildingDefinitions[i].SecondaryWeapon = index then
      str := str + Structures.templates.BuildingNameStrings[i] + ' (sec) ';
  end;
  for i := 0 to Structures.templates.UnitCount - 1 do
  begin
    if Structures.templates.UnitDefinitions[i].PrimaryWeapon = index then
      str := str + Structures.templates.UnitNameStrings[i] + ' (pri) ';
    if Structures.templates.UnitDefinitions[i].SecondaryWeapon = index then
      str := str + Structures.templates.UnitNameStrings[i] + ' (sec) ';
  end;
  for i := 0 to Length(Structures.templates.Other) - 1 do
    if (Structures.templates_other_byte_types[i] = tobtWeapon) and ((i < 25) or (i > 48) or ((i - 25) < Structures.templates.Other[49])) and (Structures.templates.Other[i] = index) then
      str := str + Copy(Structures.templates_other[i], 2, 100) + ' (special) ';
  lblWeaponUsedBy.Caption := str;

  loading := false;
  edWeaponFlagsChange(nil);
end;

procedure TStructuresEditor.fill_explosion_data;
var
  index: integer;
  exp: TExplosionTemplatePtr;
  str: String;
  i, j: integer;
  used_builexp: boolean;
begin
  index := lbExplosionList.ItemIndex;
  if index < 0 then
    exit;
  last_explosion_index := index;
  loading := true;
  exp := Addr(Structures.templates.ExplosionDefinitions[index]);

  list_control_groups[LCG_EXPLOSIONS].edit_item_name.Text := Structures.templates.ExplosionStrings[index];
  edExplosionMyIndex.Text := IntToStr(exp.MyIndex);
  edExplosionFiringPattern.Text := IntToBin(exp.FiringPattern, 7);
  cbxExplosionSound.ItemIndex := exp.Sound + 1;
  edExplosionFlags.Text := IntToHex(Structures.templates.AnimationArtFlags[index], 8);
  // Used by label
  str := 'Used by: ';
  for i := 0 to Structures.templates.BuildingCount - 1 do
  begin
    if Structures.templates.BuildingDefinitions[i].DeathExplosion = index then
      str := str + Structures.templates.BuildingNameStrings[i] + ' (death) ';
    if Structures.templates.BuildingDefinitions[i].FiringExplosion = index then
      str := str + Structures.templates.BuildingNameStrings[i] + ' (firing) ';
  end;
  for i := 0 to Structures.templates.UnitCount - 1 do
  begin
    if Structures.templates.UnitDefinitions[i].DeathExplosion = index then
      str := str + Structures.templates.UnitNameStrings[i] + ' (death) ';
    if Structures.templates.UnitDefinitions[i].FiringExplosion = index then
      str := str + Structures.templates.UnitNameStrings[i] + ' (firing) ';
  end;
  for i := 0 to Structures.templates.WeaponCount - 1 do
  begin
    if Structures.templates.WeaponDefinitions[i].HitExplosion = index then
      str := str + Structures.templates.WeaponStrings[i] + ' (hit) ';
    if Structures.templates.WeaponDefinitions[i].TrailExplosion = index then
      str := str + Structures.templates.WeaponStrings[i] + ' (trail) ';
  end;
  for i := 0 to Length(Structures.templates.Other) - 1 do
    if (Structures.templates_other_byte_types[i] = tobtExplosion) and (Structures.templates.Other[i] = index) then
      str := str + Copy(Structures.templates_other[i], 2, 100) + ' (special) ';
  used_builexp := false;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    for j := 0 to Structures.builexp[i].NumAnimations - 1 do
      if Structures.builexp[i].AnimExplosion[j] = index then
        used_builexp := true;
  if used_builexp then
    str := str + 'BUILEXP';
  lblExplosionUsedBy.Caption := str;

  loading := false;
  edExplosionFlagsChange(nil);
end;

procedure TStructuresEditor.set_owner_side_field_value(control: TCheckListBox; value: byte);
var
  i: integer;
begin
  for i := 0 to CNT_PLAYERS - 1 do
    control.Checked[i] := (value and (1 shl i)) <> 0;
end;

procedure TStructuresEditor.store_data;
var
  i, j: integer;
begin
  store_building_data;
  store_unit_data;
  store_builexp_data;
  store_weapon_data;
  store_explosion_data;

  // Armour
  for i := 0 to Structures.armour.WarheadCount - 1 do
  begin
    for j := 0 to Length(Structures.armour.WarheadEntries[i].VersusArmorType) - 1 do
      Structures.armour.WarheadEntries[i].VersusArmorType[j] := StrToIntDef(sgArmourValues.Cells[j + 1, i + 1], 0);
    Structures.armour.WarheadEntries[i].Unknown12 := StrToIntDef(sgArmourValues.Cells[13, i + 1], 0);
    Structures.armour.WarheadEntries[i].Unknown16 := StrToIntDef(sgArmourValues.Cells[14, i + 1], 0);
  end;
  // Speed
  for i := 0 to Length(Structures.speed.Values) - 1 do
    for j := 0 to Length(Structures.speed.Values[i]) - 1 do
      Structures.speed.Values[i, j] := StrToFloatDef(sgSpeedValues.Cells[j+1, i+1], 1.0);
  // Other
  for i := 0 to Length(Structures.templates.Other) - 1 do
    if Structures.templates_other_byte_types[i] = tobtNone then
      Structures.templates.Other[i] := StrToIntDef(vleTemplatesOther.Cells[1, i+1], 0);

end;

procedure TStructuresEditor.store_building_data;
var
  index: integer;
  i: integer;
  bld: TBuildingTemplatePtr;
begin
  index := last_building_index;
  if index < 0 then
    exit;
  bld := Addr(Structures.templates.BuildingDefinitions[index]);
  // Basic group box
  bld.BuildingType := cbxBuildingType.ItemIndex;
  bld.OwnerSide := get_owner_side_field_value(clbBuildingOwnerSide);
  // Build requirements group box
  bld.TechLevelBuild := seBuildingTechLevelBuild.Value;
  bld.TechLevelUpgrade1 := seBuildingTechLevelUpgrade1.Value;
  bld.TechLevelUpgrade2 := seBuildingTechLevelUpgrade2.Value;
  bld.TechLevelUpgrade3 := seBuildingTechLevelUpgrade3.Value;
  bld.CostBuild := strtointdef(edBuildingCostBuild.Text, 0);
  bld.CostUpgrade1 := strtointdef(edBuildingCostUpgrade1.Text, 0);
  bld.CostUpgrade2 := strtointdef(edBuildingCostUpgrade2.Text, 0);
  bld.CostUpgrade3 := strtointdef(edBuildingCostUpgrade3.Text, 0);
  bld.BuildSpeedBuild := strtointdef(edBuildingBuildSpeedBuild.Text, 0);
  bld.BuildSpeedUpgrade1 := strtointdef(edBuildingBuildSpeedUpgrade1.Text, 0);
  bld.BuildSpeedUpgrade2 := strtointdef(edBuildingBuildSpeedUpgrade2.Text, 0);
  bld.BuildSpeedUpgrade3 := strtointdef(edBuildingBuildSpeedUpgrade3.Text, 0);
  bld.Prereq1BuildingType := cbxBuildingPrereq1BuildingType.ItemIndex - 1;
  bld.Prereq1OwnerSide := get_owner_side_field_value(clbBuildingPrereq1OwnerSide);
  bld.Prereq1UpgradesNeeded := seBuildingPrereq1UpgradesNeeded.Value;
  bld.Prereq2BuildingType := cbxBuildingPrereq2BuildingType.ItemIndex - 1;
  bld.Prereq2OwnerSide := get_owner_side_field_value(clbBuildingPrereq2OwnerSide);
  bld.Prereq2UpgradesNeeded := seBuildingPrereq2UpgradesNeeded.Value;
  // Properties and behavior group box
  bld.HitPoints := strtointdef(edBuildingHitPoints.Text, 0);
  bld.ArmorType := cbxBuildingArmorType.ItemIndex;
  bld.PowerConsumption := strtointdef(edBuildingPowerConsumption.Text, 0);
  bld.HealthBarSize := cbxBuildingHealthBarSize.ItemIndex;
  bld.SightRadius := seBuildingSightRadius.Value;
  bld.SpecialBehavior := cbxBuildingSpecialBehavior.ItemIndex;
  // Turret properties group box
  bld.ActLikeTurret := IfThen(cbBuildingActLikeTurret.Checked, 1, 0);
  bld.RequireEnoughPower := IfThen(cbBuildingRequireEnoughPower.Checked, 1, 0);
  bld.PrimaryWeapon := cbxBuildingPrimaryWeapon.ItemIndex - 1;
  bld.SecondaryWeapon := cbxBuildingSecondaryWeapon.ItemIndex - 1;
  bld.RateOfFire := seBuildingRateOfFire.Value;
  bld.BarrelRotationSpeed := seBuildingBarrelRotationSpeed.Value;
  // Visuals and animations group box
  bld.BuildingArt := cbxBuildingBuildingArt.ItemIndex - 1;
  bld.BarrelArt := cbxBuildingBarrelArt.ItemIndex - 1;
  bld.ArtWidth := strtointdef(edBuildingArtWidth.Text, 0);
  bld.ArtHeight := strtointdef(edBuildingArtHeight.Text, 0);
  bld.BuildingAnimation := cbxBuildingBuildingAnimation.ItemIndex;
  bld.AnimationSpeed := seBuildingAnimationSpeed.Value;
  bld.BuildupArt := cbxBuildingBuildupArt.ItemIndex;
  bld.BuildupFramesToShow := seBuildingBuildupFramesToShow.Value;
  bld.DeathExplosion := cbxBuildingDeathExplosion.ItemIndex - 1;
  bld.FiringExplosion := cbxBuildingFiringExplosion.ItemIndex - 1;
  for i := 0 to 31 do
    bld.DirectionFrames[i] := strtointdef(sgBuildingDirectionFrames.Cells[i mod 8, i div 8], 0);
  // Space requirements group box
  bld.TilesOccupiedAll := tmp_building_tiles_occupied_all;
  bld.TilesOccupiedSolid := tmp_building_tiles_occupied_solid;
  bld.ExitPoint1X := seBuildingExitPoint1X.Value;
  bld.ExitPoint1Y := seBuildingExitPoint1Y.Value;
  bld.ExitPoint2X := seBuildingExitPoint2X.Value;
  bld.ExitPoint2Y := seBuildingExitPoint2Y.Value;
  // Others and unknown group box
  bld.Unknown8 := seBuildingUnknown8.Value;
  bld.Unknown93 := seBuildingUnknown93.Value;
  bld.Flags := strtointdef('$' + edBuildingFlags.Text, 0);
  // Store building name
  if edBuildingName.Text <> Structures.templates.BuildingNameStrings[index] then
  begin
    beep;
    store_c_string(edBuildingName.Text, Addr(Structures.templates.BuildingNameStrings[index]), Length(Structures.templates.BuildingNameStrings[index]));
    store_data;
    last_building_index := lbBuildingList.ItemIndex;
    fill_data;
  end;
end;

procedure TStructuresEditor.store_unit_data;
var
  index: integer;
  i: integer;
  unt: TUnitTemplatePtr;
begin
  index := last_unit_index;
  if index < 0 then
    exit;
  unt := Addr(Structures.templates.UnitDefinitions[index]);
  // Basic group box
  unt.UnitType := cbxUnitType.ItemIndex;
  unt.OwnerSide := get_owner_side_field_value(clbUnitOwnerSide);
  // Build requirements group box
  unt.TechLevel := seUnitTechLevel.Value;
  unt.Cost := strtointdef(edUnitCost.Text, 0);
  unt.BuildSpeed := strtointdef(edUnitBuildSpeed.Text, 0);
  unt.Prereq1BuildingType := cbxUnitPrereq1BuildingType.ItemIndex - 1;
  unt.Prereq1OwnerSide := get_owner_side_field_value(clbUnitPrereq1OwnerSide);
  unt.Prereq1UpgradesNeeded := seUnitPrereq1UpgradesNeeded.Value;
  unt.Prereq2BuildingType := cbxUnitPrereq2BuildingType.ItemIndex - 1;
  unt.AvailableInStarport := IfThen(cbUnitAvailableInStarport.Checked, 1, 0);
  unt.MultiplayerOnly := IfThen(cbUnitMultiplayerOnly.Checked, 1, 0);
  // Voices group box
  for i := 0 to Length(unt.Voices) - 1 do
    unt.Voices[i] := cbxUnitVoices[i].ItemIndex;
  // Properties and behavior group box
  unt.HitPoints := strtointdef(edUnitHitPoints.Text, 0);
  unt.ArmorType := cbxUnitArmorType.ItemIndex;
  unt.SightRadius := seUnitSightRadius.Value;
  unt.HealthBarSize := cbxUnitHealthBarSize.ItemIndex;
  unt.IsInfantry := IfThen(cbUnitIsInfantry.Checked, 1, 0);
  unt.SpecialBehavior := cbxUnitSpecialBehavior.ItemIndex;
  // Movement group box
  unt.Speed := strtointdef(edUnitSpeed.Text, 0) shl 12;
  unt.SpeedType := cbxUnitSpeedType.ItemIndex;
  unt.UnitRotationSpeed := seUnitUnitRotationSpeed.Value;
  unt.CanCrushInfantry := IfThen(cbUnitCanCrushInfantry.Checked, 1, 0);
  // Weapons group box
  unt.PrimaryWeapon := cbxUnitPrimaryWeapon.ItemIndex - 1;
  unt.SecondaryWeapon := cbxUnitSecondaryWeapon.ItemIndex - 1;
  unt.HasBarrel := IfThen(cbUnitHasBarrel.Checked, 1, 0);
  unt.RateOfFire := seUnitRateOfFire.Value;
  unt.BarrelRotationSpeed := seUnitBarrelRotationSpeed.Value;
  // Visuals and animations group box
  unt.UnitArt := cbxUnitUnitArt.ItemIndex - 1;
  unt.BarrelArt := cbxUnitBarrelArt.ItemIndex - 1;
  unt.DeathExplosion := cbxUnitDeathExplosion.ItemIndex - 1;
  unt.FiringExplosion := cbxUnitFiringExplosion.ItemIndex - 1;
  for i := 0 to 31 do
    unt.DirectionFrames[i] := strtointdef(sgUnitDirectionFrames.Cells[i mod 8, i div 8], 0);
  // Others and unknown group box
  unt.Unknown46 := seUnitUnknown46.Value;
  unt.Unknown52 := seUnitUnknown52.Value;
  unt.Unknown55 := seUnitUnknown55.Value;
  unt.Unknown164 := seUnitUnknown164.Value;
  unt.Flags := strtointdef('$' + edUnitFlags.Text, 0);
  // Store unit name
  if edUnitName.Text <> Structures.templates.UnitNameStrings[index] then
  begin
    beep;
    store_c_string(edUnitName.Text, Addr(Structures.templates.UnitNameStrings[index]), Length(Structures.templates.UnitNameStrings[index]));
    store_data;
    last_unit_index := lbUnitList.ItemIndex;
    fill_data;
  end;
end;

procedure TStructuresEditor.store_builexp_data;
var
  index: integer;
  i: integer;
  bxp: TBuilExpEntryPtr;
begin
  index := last_builexp_building_index;
  if index < 0 then
    exit;
  bxp := Addr(Structures.builexp[index]);
  bxp.NumAnimations := seBuilExpNumAnimations.Value;
  for i := 0 to bxp.NumAnimations - 1 do
  begin
    bxp.AnimOffsetX[i] := seBuilExpAnimOffsetX[i].Value;
    bxp.AnimOffsetY[i] := seBuilExpAnimOffsetY[i].Value;
    bxp.AnimExplosion[i] := cbxBuilExpAnimExplosion[i].ItemIndex;
    bxp.AnimNumFrames[i] := seBuilExpAnimNumFrames[i].Value;
  end;
end;

procedure TStructuresEditor.store_weapon_data;
var
  index: integer;
  wpn: TWeaponTemplatePtr;
begin
  index := last_weapon_index;
  if index < 0 then
    exit;
  wpn := Addr(Structures.templates.WeaponDefinitions[index]);

  // Properties and behavior tab
  wpn.Damage := StrToIntDef(edWeaponDamage.Text, 0);
  wpn.Warhead := cbxWeaponWarhead.ItemIndex;
  wpn.Range := StrToIntDef(edWeaponRange.Text, 0);
  wpn.AntiAircraft := IfThen(cbWeaponAntiAircraft.Checked, 1, 0);
  // Projectile movement tab
  wpn.ProjectileSpeed := StrToIntDef(edWeaponProjectileSpeed.Text, 0) shl 10;
  // Visuals and sounds tab
  wpn.ProjectileArt := cbxWeaponProjectileArt.ItemIndex;
  wpn.FiringSound := cbxWeaponFiringSound.ItemIndex - 1;
  wpn.HitExplosion := cbxWeaponHitExplosion.ItemIndex - 1;
  wpn.TrailExplosion := cbxWeaponTrailExplosion.ItemIndex - 1;
  // Others and unknown tab
  wpn.Unknown19 := seWeaponUnknown19.Value;
  wpn.Flags := StrToIntDef('$' + edWeaponFlags.Text, 0);
end;

procedure TStructuresEditor.store_explosion_data;
var
  index: integer;
  exp: TExplosionTemplatePtr;
begin
  index := last_explosion_index;
  if index < 0 then
    exit;
  exp := Addr(Structures.templates.ExplosionDefinitions[index]);

  exp.MyIndex := StrToIntDef(edExplosionMyIndex.Text, 0);
  exp.FiringPattern := BinToInt(edExplosionFiringPattern.Text);
  exp.Sound := cbxExplosionSound.ItemIndex - 1;
  Structures.templates.AnimationArtFlags[index] := StrToIntDef('$' + edExplosionFlags.Text, 0);
end;

function TStructuresEditor.get_owner_side_field_value(control: TCheckListBox): byte;
var
  i: integer;
begin
  result := 0;
  for i := 0 to CNT_PLAYERS - 1 do
    if control.Checked[i] then
      result := result or (1 shl i);
end;

function TStructuresEditor.get_templates_other_cell_text(index: integer; value: shortint): string;
var
  item_name_list_combo: TComboBox;
begin
  item_name_list_combo := nil;
  case Structures.templates_other_byte_types[index] of
    tobtBuilding:  item_name_list_combo := cbxBuildingPrereq1BuildingType;
    tobtUnit:      item_name_list_combo := cbxTemplatesOtherUnitSelect;
    tobtWeapon:    item_name_list_combo := cbxBuildingPrimaryWeapon;
    tobtExplosion: item_name_list_combo := cbxBuildingDeathExplosion;
  end;
  if item_name_list_combo <> nil then
    result := item_name_list_combo.Items[value + 1]
  else
    result := inttostr(value);
end;

procedure TStructuresEditor.create_list_control_group(group_index: integer; item_count_byte_ptr: PByte; item_name_list_ptr: TItemNameListPtr; max_item_count: integer; last_item_index_ptr: PInteger; list_control: TListBox; container: TPanel);
var
  lcg: TListControlGroupPtr;
begin
  lcg := Addr(list_control_groups[group_index]);
  lcg.item_count_byte_ptr := item_count_byte_ptr;
  lcg.item_name_list_ptr := item_name_list_ptr;
  lcg.max_item_count := max_item_count;
  lcg.last_item_index_ptr := last_item_index_ptr;
  lcg.list_control := list_control;
  lcg.edit_item_name := TEdit.Create(self);
  lcg.edit_item_name.Width := container.Width;
  lcg.edit_item_name.Top := container.Height - 77;
  lcg.edit_item_name.MaxLength := 49;
  lcg.edit_item_name.Parent := container;
  lcg.btn_item_add := TButton.Create(self);
  lcg.btn_item_add.Top := container.Height - 53;
  lcg.btn_item_add.Width := 73;
  lcg.btn_item_add.Caption := 'Add new';
  lcg.btn_item_add.Tag := group_index;
  lcg.btn_item_add.OnClick := ListControlGroupAddClick;
  lcg.btn_item_add.Parent := container;
  lcg.btn_item_remove := TButton.Create(self);
  lcg.btn_item_remove.Top := container.Height - 53;
  lcg.btn_item_remove.Left := container.Width - 73;
  lcg.btn_item_remove.Width := 73;
  lcg.btn_item_remove.Caption := 'Remove last';
  lcg.btn_item_remove.Tag := group_index;
  lcg.btn_item_remove.OnClick := ListControlGroupRemoveClick;
  lcg.btn_item_remove.Parent := container;
  lcg.btn_item_rename := TButton.Create(self);
  lcg.btn_item_rename.Top := container.Height - 25;
  lcg.btn_item_rename.Left := (container.Width - 97) div 2;
  lcg.btn_item_rename.Width := 97;
  lcg.btn_item_rename.Caption := 'Rename selected';
  lcg.btn_item_rename.Tag := group_index;
  lcg.btn_item_rename.OnClick := ListControlGroupRenameClick;
  lcg.btn_item_rename.Parent := container;
end;

procedure TStructuresEditor.create_art_control_group(group_index: integer; is_unit: boolean; container, container2: TPanel);
var
  acg: TArtControlGroupPtr;
begin
  acg := Addr(art_control_groups[group_index]);
  acg.is_unit := is_unit;
  acg.view_image := TImage.Create(self);
  acg.view_image.Parent := container;
  acg.view_image.Width := IfThen(is_unit, 160, 128);
  acg.view_image.Height := 160;
  acg.view_image.Top := 16;
  acg.frame_list := TListBox.Create(self);
  acg.frame_list.Parent := container;
  acg.frame_list.Width := 169;
  acg.frame_list.Height := 381;
  acg.frame_list.Top := 256;
  acg.frame_list.Tag := group_index;
  acg.frame_list.OnClick := ArtControlGroupFrameListClick;
  acg.lbl_frame_width := TLabel.Create(self);
  acg.lbl_frame_width.Parent := container;
  acg.lbl_frame_width.Caption := 'Frame W:';
  acg.lbl_frame_width.Top := 184;
  acg.lbl_frame_height := TLabel.Create(self);
  acg.lbl_frame_height.Parent := container;
  acg.lbl_frame_height.Caption := 'Frame H:';
  acg.lbl_frame_height.Top := 184;
  acg.lbl_frame_height.Left := 88;
  acg.lbl_image_width := TLabel.Create(self);
  acg.lbl_image_width.Parent := container;
  acg.lbl_image_width.Caption := 'Image W:';
  acg.lbl_image_width.Top := 208;
  acg.lbl_image_height := TLabel.Create(self);
  acg.lbl_image_height.Parent := container;
  acg.lbl_image_height.Caption := 'Image H:';
  acg.lbl_image_height.Top := 208;
  acg.lbl_image_height.Left := 88;
  acg.lbl_image_offset_x := TLabel.Create(self);
  acg.lbl_image_offset_x.Parent := container;
  acg.lbl_image_offset_x.Caption := 'Offset X:';
  acg.lbl_image_offset_x.Top := 232;
  acg.lbl_image_offset_y := TLabel.Create(self);
  acg.lbl_image_offset_y.Parent := container;
  acg.lbl_image_offset_y.Caption := 'Offset Y:';
  acg.lbl_image_offset_y.Top := 232;
  acg.lbl_image_offset_y.Left := 88;
  acg.edit_frame_width := TEdit.Create(self);
  acg.edit_frame_width.Parent := container;
  acg.edit_frame_width.ReadOnly := true;
  acg.edit_frame_width.Width := 33;
  acg.edit_frame_width.Top := 184;
  acg.edit_frame_width.Left := 48;
  acg.edit_frame_height := TEdit.Create(self);
  acg.edit_frame_height.Parent := container;
  acg.edit_frame_height.ReadOnly := true;
  acg.edit_frame_height.Width := 33;
  acg.edit_frame_height.Top := 184;
  acg.edit_frame_height.Left := 136;
  acg.edit_image_width := TEdit.Create(self);
  acg.edit_image_width.Parent := container;
  acg.edit_image_width.ReadOnly := true;
  acg.edit_image_width.Width := 33;
  acg.edit_image_width.Top := 208;
  acg.edit_image_width.Left := 48;
  acg.edit_image_height := TEdit.Create(self);
  acg.edit_image_height.Parent := container;
  acg.edit_image_height.ReadOnly := true;
  acg.edit_image_height.Width := 33;
  acg.edit_image_height.Top := 208;
  acg.edit_image_height.Left := 136;
  acg.edit_image_offset_x := TEdit.Create(self);
  acg.edit_image_offset_x.Parent := container;
  acg.edit_image_offset_x.ReadOnly := true;
  acg.edit_image_offset_x.Width := 33;
  acg.edit_image_offset_x.Top := 232;
  acg.edit_image_offset_x.Left := 48;
  acg.edit_image_offset_y := TEdit.Create(self);
  acg.edit_image_offset_y.Parent := container;
  acg.edit_image_offset_y.ReadOnly := true;
  acg.edit_image_offset_y.Width := 33;
  acg.edit_image_offset_y.Top := 232;
  acg.edit_image_offset_y.Left := 136;
  if container2 = nil then
    exit;
  acg.btn_art_add := TButton.Create(self);
  acg.btn_art_add.Top := 584;
  acg.btn_art_add.Width := 73;
  acg.btn_art_add.Caption := 'Add new';
  acg.btn_art_add.Tag := group_index;
  acg.btn_art_add.OnClick := ArtControlGroupAddClick;
  acg.btn_art_add.Parent := container2;
  acg.btn_art_remove := TButton.Create(self);
  acg.btn_art_remove.Top := 584;
  acg.btn_art_remove.Left := 88;
  acg.btn_art_remove.Width := 73;
  acg.btn_art_remove.Caption := 'Remove last';
  acg.btn_art_remove.Tag := group_index;
  acg.btn_art_remove.OnClick := ArtControlGroupRemoveClick;
  acg.btn_art_remove.Parent := container2;
  acg.btn_art_modify := TButton.Create(self);
  acg.btn_art_modify.Top := 612;
  acg.btn_art_modify.Left := 32;
  acg.btn_art_modify.Width := 97;
  acg.btn_art_modify.Caption := 'Modify selected';
  acg.btn_art_modify.Tag := group_index;
  acg.btn_art_modify.OnClick := ArtControlGroupModifyClick;
  acg.btn_art_modify.Parent := container2;
end;

procedure TStructuresEditor.fill_art_control_group_frame_list(group_index, first_image_index, num_frames: integer; frame_names: TStrings; selected_frame: integer);
var
  acg: TArtControlGroupPtr;
  i: integer;
  header: TR16EntryHeaderPtr;
  tmp_strings: TStringList;
  frame_name: string;
  frame_size: string;
begin
  acg := Addr(art_control_groups[group_index]);
  acg.first_image_index := first_image_index;
  tmp_strings := TStringList.Create;
  for i := 0 to num_frames - 1 do
  begin
    if frame_names <> nil then
      frame_name := frame_names[i]
    else
      frame_name := 'Frame ' + inttostr(i);
    header := Structures.get_structure_image_header(first_image_index + i);
    if (header = nil) or (header.EntryType = 0) then
      frame_size := 'empty'
    else
      frame_size := Format('%d x %d', [header.ImageWidth, header.ImageHeight]);
    tmp_strings.Add(Format('%s (%s)', [frame_name, frame_size]));
  end;
  acg.frame_list.Items := tmp_strings;
  tmp_strings.Destroy;

  acg.frame_list.ItemIndex := selected_frame;
  ArtControlGroupFrameListClick(acg.frame_list);
end;

procedure TStructuresEditor.draw_no_image_sign(img_target: TImage);
var
  sign: string;
begin
  sign := 'No Image';
  img_target.Canvas.Pen.Color := clGray;
  img_target.Canvas.Brush.Color := clGray;
  img_target.Canvas.Brush.Style := bsSolid;
  img_target.Canvas.Rectangle(0, 0, img_target.Width, img_target.Height);
  img_target.Canvas.Font.Color := clWhite;
  img_target.Canvas.TextOut((img_target.Width - img_target.Canvas.TextWidth(sign)) div 2, (img_target.Height - img_target.Canvas.TextHeight(sign)) div 2, 'No Image');
  img_target.Canvas.Font.Color := clBlack;
end;

procedure TStructuresEditor.draw_building_tile_map(img_target: TImage; value: cardinal);
var
  x, y: integer;
begin
  img_target.Canvas.Pen.Color := clBlack;
  img_target.Canvas.Brush.Style := bsSolid;
  for y := 0 to MAX_BUILDING_SIZE - 1 do
    for x := 0 to MAX_BUILDING_SIZE - 1 do
    begin
      if (value and (1 shl (y * MAX_BUILDING_SIZE + x))) <> 0 then
        img_target.Canvas.Brush.Color := clGray
      else
        img_target.Canvas.Brush.Color := clWhite;
      img_target.Canvas.Rectangle(x*17, y*17, x*17+16, y*17+16);
    end;
end;

procedure TStructuresEditor.draw_building_preview(draw_building: boolean);
var
  art_width, art_height: integer;
  i, x, y: integer;
  paint_tile_group, start_index, tile_index, tile_x, tile_y: integer;
  src_rect, dest_rect: TRect;
  building_skirt: ^TBuildingSkirt;
  bottom_offset: integer;
begin
  art_width := strtointdef(edBuildingArtWidth.Text, 0);
  art_height := strtointdef(edBuildingArtHeight.Text, 0);
  imgBuildingImage.Canvas.CopyMode := cmSrcCopy;
  // Find proper paint tile group with buildable tiles
  paint_tile_group := 0;
  for i := 0 to cnt_paint_tile_groups - 1 do
    if Tileset.get_tile_type(Tileset.paint_tile_groups[i].tile_index) = ttBuildable then
    begin
      paint_tile_group := i;
      break;
    end;
  // Find starting tile index for paint tile group
  start_index := 0;
  for i := -4 to paint_tile_group - 1 do
    inc(start_index, Tileset.paint_tile_groups[i].paint_tiles_cnt);
  // Draw terrain
  for y := 0 to 4 do
    for x := 0 to 3 do
    begin
      if Tileset.paint_tile_groups[paint_tile_group].paint_tiles_cnt > 0 then
        tile_index := Tileset.paint_tiles[start_index + (y * 4 + x) mod Tileset.paint_tile_groups[paint_tile_group].paint_tiles_cnt]
      else
        tile_index := Tileset.paint_tile_groups[paint_tile_group].tile_index;
      tile_x := tile_index mod 20;
      tile_y := tile_index div 20;
      dest_rect := Rect((x)*32, (y)*32, (x)*32 + 32, (y)*32 + 32);
      src_rect := Rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32);
      imgBuildingImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
    end;
  // Draw concrete under building
  if not cbBuildingFlagNO_CONCRETE.Checked then
  begin
    for x := 0 to MAX_BUILDING_SIZE - 1 do
      for y := 0 to MAX_BUILDING_SIZE - 1 do
      begin
        // Check if tile is occupied by building
        if (tmp_building_tiles_occupied_all and (1 shl (y * MAX_BUILDING_SIZE + x))) = 0 then
          continue;
        tile_index := CONCRETE_TILES[(x + y) mod Length(CONCRETE_TILES)];
        tile_x := tile_index mod 20;
        tile_y := tile_index div 20;
        dest_rect := Rect((x)*32, (y+1)*32, (x)*32 + 32, (y+1)*32 + 32);
        src_rect := Rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32);
        imgBuildingImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      end;
  end;
  // Draw building's skirt
  if cbBuildingFlagHAS_SKIRT.Checked then
  begin
    building_skirt := nil;
    if art_width = 64 then
      building_skirt := Addr(BUILDING_SKIRT_2x2);
    if art_width = 96 then
      building_skirt := Addr(BUILDING_SKIRT_3x2);
    if building_skirt <> nil then
    begin
      bottom_offset := (art_height div 32) - building_skirt.size_y;
      for y := 0 to building_skirt.size_y - 1 do
        for x := 0 to building_skirt.size_x - 1 do
        begin
          dest_rect := Rect((x)*32, (y + 1 + bottom_offset)*32, (x)*32 + 32, (y + 1 + bottom_offset)*32 + 32);
          if (not cbBuildingFlagNO_CONCRETE.Checked) and ((tmp_building_tiles_occupied_all and (1 shl ((y + bottom_offset) * MAX_BUILDING_SIZE + x))) <> 0) then
            src_rect := Rect((x + building_skirt.conc_tile_x)*32, (y + building_skirt.conc_tile_y)*32, (x + building_skirt.conc_tile_x)*32+32, (y + building_skirt.conc_tile_y)*32+32)
          else
            src_rect := Rect((x + building_skirt.rock_tile_x)*32, (y + building_skirt.rock_tile_y)*32, (x + building_skirt.rock_tile_x)*32+32, (y + building_skirt.rock_tile_y)*32+32);
          imgBuildingImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
        end;
    end;
  end;
  // Draw building frames
  if not draw_building then
    exit;
  if cbxBuildingBuildingArt.ItemIndex <> 0 then
  begin
    draw_building_frame(Structures.building_art_image_indexes[cbxBuildingBuildingArt.ItemIndex-1], false, false);
    draw_building_frame(Structures.building_art_image_indexes[cbxBuildingBuildingArt.ItemIndex-1] + 1, false, false);
  end;
  if cbxBuildingBarrelArt.ItemIndex <> 0 then
  begin
    draw_building_frame(Structures.building_art_image_indexes[cbxBuildingBarrelArt.ItemIndex-1] + 1, false, false);
  end;
end;

procedure TStructuresEditor.draw_building_frame(image_index: integer; alpha, animation: boolean);
var
  side: integer;
  i: integer;
  art_height: integer;
  structure_image: TStructureImagePtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
  turret_offset_x: integer;
begin
  side := 0;
  for i := 0 to CNT_PLAYERS - 1 do
    if clbBuildingOwnerSide.Checked[i] then
    begin
      side := i;
      break;
    end;
  structure_image := Structures.get_structure_image(image_index, side, false, false, was_already_loaded);
  if structure_image <> nil then
  begin
    art_height := strtointdef(edBuildingArtHeight.Text, 0);
    turret_offset_x := 0;
    if (cbxBuildingSpecialBehavior.ItemIndex = 16) and (not animation) then
      turret_offset_x := seBuildingExitPoint1X.Value;
    src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
    dest_rect := Rect(structure_image.offset_x + turret_offset_x, art_height - structure_image.offset_y + 32, structure_image.offset_x + turret_offset_x + structure_image.bitmap.Width, art_height - structure_image.offset_y + structure_image.bitmap.Height + 32);
    if not alpha then
    begin
      imgBuildingImage.Canvas.CopyMode := cmSrcAnd;
      imgBuildingImage.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
    end;
    imgBuildingImage.Canvas.CopyMode := cmSrcPaint;
    imgBuildingImage.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
    if not was_already_loaded then
      Structures.clear_last_structure_image(image_index, false);
  end;
end;

procedure TStructuresEditor.draw_unit_preview;
var
  i: integer;
  side: integer;
  is_stealth: boolean;
begin
  // Get side
  side := 0;
  for i := 0 to CNT_PLAYERS - 1 do
    if clbUnitOwnerSide.Checked[i] then
    begin
      side := i;
      break;
    end;
  // Draw background
  imgUnitImage.Canvas.Brush.Style := bsSolid;
  imgUnitImage.Canvas.Pen.Color := clWhite;
  imgUnitImage.Canvas.Brush.Color := clWhite;
  imgUnitImage.Canvas.Rectangle(0, 0, 80, 80);
  imgUnitImage.Canvas.Pen.Color := $E0E0E0;
  imgUnitImage.Canvas.Brush.Color := $E0E0E0;
  imgUnitImage.Canvas.Rectangle(24, 24, 56, 56);
  imgUnitImage.Canvas.Brush.Color := clWhite;
  // Draw unit frames
  is_stealth := cbUnitFlagUF_STEALTH.Checked or (cbxUnitSpecialBehavior.ItemIndex = 12) or (edUnitName.Text = 'STEALTH RAIDER');
  if cbxUnitUnitArt.ItemIndex <> 0 then
  begin
    draw_unit_frame(Structures.unit_art_image_indexes[cbxUnitUnitArt.ItemIndex-1], side, is_stealth);
  end;
  if cbxUnitBarrelArt.ItemIndex <> 0 then
  begin
    draw_unit_frame(Structures.unit_art_image_indexes[cbxUnitBarrelArt.ItemIndex-1], side, is_stealth);
  end;
end;

procedure TStructuresEditor.draw_unit_frame(image_index, side: integer; is_stealth: boolean);
var
  structure_image: TStructureImagePtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
begin
  structure_image := Structures.get_structure_image(image_index, side, true, is_stealth, was_already_loaded);
  if structure_image <> nil then
  begin
    src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
    dest_rect := Rect(structure_image.offset_x + 24, structure_image.offset_y + 24, structure_image.offset_x + 24 + structure_image.bitmap.Width, structure_image.offset_y + 24 + structure_image.bitmap.Height);
    imgUnitImage.Canvas.CopyMode := cmSrcAnd;
    imgUnitImage.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
    imgUnitImage.Canvas.CopyMode := cmSrcPaint;
    imgUnitImage.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
    if not was_already_loaded then
      Structures.clear_last_structure_image(image_index, is_stealth);
  end;
end;

procedure TStructuresEditor.draw_builexp_preview;
var
  building_template: TBuildingTemplatePtr;
  structure_image: TStructureImagePtr;
  header: TR16EntryHeaderPtr;
  image_index: integer;
  was_already_loaded: boolean;
  i: integer;
  off_x, off_y: integer;
  src_rect, dest_rect: TRect;
begin
  imgBuilExpImage.Canvas.Pen.Color := clAqua;
  imgBuilExpImage.Canvas.Brush.Color := clAqua;
  imgBuilExpImage.Canvas.Brush.Style := bsSolid;
  imgBuilExpImage.Canvas.Rectangle(0, 0, 128, 160);
  imgBuilExpImage.Canvas.Brush.Color := clWhite;
  // Draw building frames
  building_template := Addr(Structures.templates.BuildingDefinitions[lbBuilExpBuildingList.ItemIndex]);
  if building_template.BuildingArt <> -1 then
  begin
    // Base frame
    image_index := Structures.building_art_image_indexes[building_template.BuildingArt];
    draw_building_art_frame(imgBuilExpImage, image_index, false);
    // Damaged frame
    image_index := Structures.building_art_image_indexes[building_template.BuildingArt] + Structures.templates.BuildingArtDirections[building_template.BuildingArt] + 1;
    header := Structures.get_structure_image_header(image_index);
    if (header = nil) or (header.EntryType = 0) then
      // If damaged frame does not exist, use healthy frame
      image_index := Structures.building_art_image_indexes[building_template.BuildingArt] + 1;
    draw_building_art_frame(imgBuilExpImage, image_index, false);
  end;
  if building_template.BarrelArt <> -1 then
  begin
    draw_building_art_frame(imgBuilExpImage, Structures.building_art_image_indexes[building_template.BarrelArt] + 1, false);
  end;
  // Draw animations
  for i := 0 to seBuilExpNumAnimations.Value - 1 do
  begin
    image_index := Structures.animation_art_image_indexes[cbxBuilExpAnimExplosion[i].ItemIndex] + seBuilExpAnimNumFrames[i].Value;
    structure_image := Structures.get_structure_image(image_index, 0, false, false, was_already_loaded);
    if structure_image <> nil then
    begin
      header := Structures.get_structure_image_header(image_index);
      src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
      off_x := seBuilExpAnimOffsetX[i].Value - header.ImageOffsetX;
      off_y := seBuilExpAnimOffsetY[i].Value - header.ImageOffsetY;
      dest_rect := Rect(off_x, off_y, off_x + structure_image.bitmap.Width, off_y + structure_image.bitmap.Height);
      imgBuilExpImage.Canvas.CopyMode := cmSrcAnd;
      imgBuilExpImage.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
      imgBuilExpImage.Canvas.CopyMode := cmSrcPaint;
      imgBuilExpImage.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
      if not was_already_loaded then
        Structures.clear_last_structure_image(image_index, false);
    end;
  end;
end;

procedure TStructuresEditor.draw_building_art_frame(img_target: TImage; image_index: integer; draw_background: boolean);
var
  structure_image: TStructureImagePtr;
  header: TR16EntryHeaderPtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
begin
  structure_image := Structures.get_structure_image(image_index, 0, false, false, was_already_loaded);
  if structure_image = nil then
    exit;
  header := Structures.get_structure_image_header(image_index);
  if draw_background then
  begin
    img_target.Canvas.Pen.Color := clAqua;
    img_target.Canvas.Brush.Color := clAqua;
    img_target.Canvas.Brush.Style := bsSolid;
    img_target.Canvas.Rectangle(0, 0, img_target.Width, img_target.Height);
  end;
  img_target.Canvas.Pen.Color := clGray;
  img_target.Canvas.Brush.Style := bsClear;
  img_target.Canvas.Rectangle(0, 0, header.FrameWidth, header.FrameHeight);
  img_target.Canvas.Brush.Style := bsSolid;
  src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
  dest_rect := Rect(structure_image.offset_x, header.FrameHeight - structure_image.offset_y, structure_image.offset_x + structure_image.bitmap.Width, header.FrameHeight - structure_image.offset_y + structure_image.bitmap.Height);
  img_target.Canvas.CopyMode := cmSrcAnd;
  img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
  img_target.Canvas.CopyMode := cmSrcPaint;
  img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
  if not was_already_loaded then
    Structures.clear_last_structure_image(image_index, false);
end;

procedure TStructuresEditor.draw_unit_art_frame(img_target: TImage; image_index: integer);
var
  structure_image: TStructureImagePtr;
  header: TR16EntryHeaderPtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
begin
  structure_image := Structures.get_structure_image(image_index, 0, true, false, was_already_loaded);
  if structure_image = nil then
    exit;
  header := Structures.get_structure_image_header(image_index);
  img_target.Canvas.Pen.Color := clAqua;
  img_target.Canvas.Brush.Color := clAqua;
  img_target.Canvas.Brush.Style := bsSolid;
  img_target.Canvas.Rectangle(0, 0, img_target.Width, img_target.Height);
  img_target.Canvas.Pen.Color := $E0E000;
  img_target.Canvas.Brush.Color := $E0E000;
  img_target.Canvas.Rectangle(48, 48, 112, 112);
  img_target.Canvas.Pen.Color := clGray;
  img_target.Canvas.Brush.Style := bsClear;
  img_target.Canvas.Rectangle(80 - header.FrameWidth, 80 - header.FrameHeight, 80 + header.FrameWidth, 80 + header.FrameHeight);
  img_target.Canvas.Brush.Style := bsSolid;
  src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
  dest_rect := Rect(structure_image.offset_x * 2 + 48, structure_image.offset_y * 2 + 48, structure_image.offset_x * 2 + 48 + structure_image.bitmap.Width * 2, structure_image.offset_y * 2 + 48 + structure_image.bitmap.Height * 2);
  img_target.Canvas.CopyMode := cmSrcAnd;
  img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
  img_target.Canvas.CopyMode := cmSrcPaint;
  img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
  if not was_already_loaded then
    Structures.clear_last_structure_image(image_index, false);
end;

procedure TStructuresEditor.draw_techpos_preview;
var
  i: integer;
  tech, side: integer;
  unit_type: integer;
  unit_template: TUnitTemplatePtr;
  is_stealth: boolean;
  structure_image: TStructureImagePtr;
  was_already_loaded: boolean;
  pos_x, pos_y: integer;
begin
  if Structures.techpos_bin_filename = '' then
    exit;
  imgTechposPreview.Canvas.Pen.Color := clBlack;
  imgTechposPreview.Canvas.Brush.Color := clCream;
  imgTechposPreview.Canvas.Brush.Style := bsSolid;
  imgTechposPreview.Canvas.Rectangle(0, 0, imgTechposPreview.Width, imgTechposPreview.Height);
  tech := rgTechposTechLevel.ItemIndex;
  if sbTechposAtreides.Down then
    side := 0
  else if sbTechposHarkonnen.Down then
    side := 1
  else
    side := 2;
  for i := 0 to tbTechposNumUnits.Position - 1 do
  begin
    unit_type := Structures.techpos[tech, i].Units[side];
    pos_x := ((TECHPOS_PREVIEW_SIZE div 2) + Structures.techpos[tech, i].PosX) * 32;
    pos_y := ((TECHPOS_PREVIEW_SIZE div 2) + Structures.techpos[tech, i].PosY) * 32;
    if unit_type = -1 then
      continue;
    unit_template := Structures.get_unit_template(unit_type, side);
    if (unit_template = nil) or (unit_template.SpecialBehavior = 5) then
      continue;
    // Check if unit is stealth
    is_stealth := ((unit_template.Flags and $10) <> 0) or (unit_template.SpecialBehavior = 12) or (Structures.templates.UnitNameStrings[Structures.unit_side_versions[unit_type, side]] = 'STEALTH RAIDER');
    // Draw unit
    if unit_template.UnitArt <> -1 then
    begin
      structure_image := Structures.get_structure_image(Structures.unit_art_image_indexes[unit_template.UnitArt], side, true, is_stealth, was_already_loaded);
      if structure_image <> nil then
        Renderer.draw_structure_image(imgTechposPreview.Canvas, pos_x + structure_image.offset_x, pos_y + structure_image.offset_y, 0, 0, imgTechposPreview.Width, imgTechposPreview.Height, structure_image);
    end;
    // Draw barrel
    if unit_template.BarrelArt <> -1 then
    begin
      structure_image := Structures.get_structure_image(Structures.unit_art_image_indexes[unit_template.BarrelArt], side, true, is_stealth, was_already_loaded);
      if structure_image <> nil then
        Renderer.draw_structure_image(imgTechposPreview.Canvas, pos_x + structure_image.offset_x, pos_y + structure_image.offset_y, 0, 0, imgTechposPreview.Width, imgTechposPreview.Height, structure_image);
    end;
  end;
end;

procedure TStructuresEditor.apply_changes;
begin
  store_data;
  Structures.compute_building_and_unit_side_versions;
  Structures.compute_building_type_mapping;
  MainWindow.update_structures_list;
  MapStatsDialog.update_structures_list;
  EventDialog.update_structures_list;
  Structures.update_mis_ai_properties;
  MainWindow.render_map;
  MainWindow.render_minimap;
  EventDialog.update_contents;
  MissionDialog.fill_ai_values;
end;

procedure TStructuresEditor.save_to_files;
begin
  Structures.save_templates_bin;
  Structures.save_builexp_bin;
  Structures.save_armour_bin;
  Structures.save_speed_bin;
  Structures.save_techpos_bin;
end;

end.
