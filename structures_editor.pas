unit structures_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Graphics, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, CheckLst, Spin, _structures,
  Grids, ValEdit, _utils, Buttons;

const side_names: array[0..CNT_PLAYERS-1] of string = ('Atreides', 'Harkonnen', 'Ordos', 'Emperor', 'Fremen', 'Smugglers', 'Mercenaries', 'Sandworm');
const unit_voices: array[0..8] of string = ('A1', 'A2', 'A3', 'H1', 'H2', 'H3', 'O1', 'O2', 'O3');
const TECHPOS_PREVIEW_SIZE = 7;

type
  TFillDataAction = (
    fdaNone,
    fdaAll,
    fdaAllArt,
    fdaBuildingGroupList,
    fdaBuildingList,
    fdaUnitGroupList,
    fdaUnitList,
    fdaBuilexpList,
    fdaWeaponList,
    fdaExplosionList,
    fdaArmourTypeList,
    fdaWarheadList,
    fdaSpeedTypeList
  );

// *****************************************************************************
// Item control group definitions
// *****************************************************************************

const ICG_BTN_ADD =      0;
const ICG_BTN_REMOVE =   1;
const ICG_BTN_RENAME =   2;
const ICG_BTN_EXPORT =   3;
const ICG_BTN_IMPORT =   4;
const ICG_BTN_COPY =     5;
const ICG_BTN_PASTE =    6;
const ICG_BTN_MOVEUP =   7;
const ICG_BTN_MOVEDOWN = 8;

const ICG_BTN_CAPTIONS: array[0..8] of string = ('Add new','Remove last','Rename selected','Export','Import','Copy','Paste','Move up','Move down');

type
  TItemControlGroupButtonLayout = record
    row: byte;
    left: byte;
    width: byte;
  end;

type
  TItemControlGroupLayout = array[0..8] of TItemControlGroupButtonLayout;
  TItemControlGroupLayoutPtr = ^TItemControlGroupLayout;

const ICG_LAYOUT_BLD_UNT_GROUP: TItemControlGroupLayout = (
    (row: 1; left: 0;  width: 73),
    (row: 1; left: 88; width: 73),
    (row: 2; left: 32; width: 97),
    (row: 0; left: 0;  width: 0),
    (row: 0; left: 0;  width: 0),
    (row: 0; left: 0;  width: 0),
    (row: 0; left: 0;  width: 0),
    (row: 0; left: 0;  width: 0),
    (row: 0; left: 0;  width: 0)
  );

const ICG_LAYOUT_BLD_UNT: TItemControlGroupLayout = (
    (row: 1; left: 0;   width: 68),
    (row: 2; left: 0;   width: 68),
    (row: 0; left: 0;   width: 0),
    (row: 1; left: 136; width: 44),
    (row: 1; left: 180; width: 45),
    (row: 2; left: 136; width: 44),
    (row: 2; left: 180; width: 45),
    (row: 1; left: 68;  width: 68),
    (row: 2; left: 68;  width: 68)
  );

const ICG_LAYOUT_WPN_EXP: TItemControlGroupLayout = (
    (row: 1; left: 0;   width: 73),
    (row: 2; left: 0;   width: 73),
    (row: 0; left: 0;   width: 0),
    (row: 1; left: 73;  width: 44),
    (row: 1; left: 117; width: 44),
    (row: 2; left: 73;  width: 44),
    (row: 2; left: 117; width: 44),
    (row: 0; left: 0;   width: 0),
    (row: 0; left: 0;   width: 0)
  );

const ICG_LAYOUT_ARM_WHD: TItemControlGroupLayout = (
    (row: 1; left: 0;   width: 68),
    (row: 2; left: 0;   width: 68),
    (row: 3; left: 24;  width: 97),
    (row: 1; left: 68;  width: 39),
    (row: 1; left: 107; width: 39),
    (row: 2; left: 68;  width: 39),
    (row: 2; left: 107; width: 39),
    (row: 0; left: 0;   width: 0),
    (row: 0; left: 0;   width: 0)
  );

type
  TItemControlGroup = record
    last_item_index: integer;
    fill_data_action: TFillDataAction;
    list_control: TListBox;
    lbl_header: TLabel;
    edit_item_name: TEdit;
    edit_item_name_explicit: boolean;
    buttons: array[0..8] of TButton;
  end;

  TItemControlGroupPtr = ^TItemControlGroup;

// *****************************************************************************
// Art control group definitions
// *****************************************************************************

type
  TArtControlGroup = record
    first_image_index: integer;
    last_item_index: integer;
    is_unit: boolean;
    // Referenced controls
    list_control: TListBox;
    se_directions: TSpinEdit;
    se_frames: TSpinEdit;
    // Dynamically created controls
    view_image: TImage;
    btn_view_palette: TButton;
    lbl_side_color: TLabel;
    se_side_color: TSpinEdit;
    cb_raw_image: TCheckBox;
    btn_image_export: TButton;
    btn_image_erase: TButton;
    btn_image_import: TButton;
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
    se_image_offset_x: TSpinEdit;
    se_image_offset_y: TSpinEdit;
    frame_list: TListBox;
    lbl_art_size: TLabel;
    btn_art_add: TButton;
    btn_art_remove: TButton;
    btn_art_modify: TButton;
    btn_art_export: TButton;
    btn_art_import: TButton;
    btn_art_move_up: TButton;
    btn_art_move_down: TButton;
  end;

  TArtControlGroupPtr = ^TArtControlGroup;

// *****************************************************************************
// TStructuresEditor class
// *****************************************************************************

type
  TStructuresEditor = class(TForm)
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    PageControl: TPageControl;
    PageBuildings: TTabSheet;
    PageUnits: TTabSheet;
    pnBuildingGroupList: TPanel;
    lbBuildingGroupList: TListBox;
    pnBuildingList: TPanel;
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
    cbxBuildingGroup: TComboBox;
    lblBuildingGroup: TLabel;
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
    lblBuildingPrereq1BuildingGroup: TLabel;
    lblBuildingPrereq2BuildingGroup: TLabel;
    clbBuildingPrereq1OwnerSide: TCheckListBox;
    clbBuildingPrereq2OwnerSide: TCheckListBox;
    cbxBuildingPrereq1BuildingGroup: TComboBox;
    cbxBuildingPrereq2BuildingGroup: TComboBox;
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
    lblBuildingMuzzleFlashExplosion: TLabel;
    cbxBuildingDeathExplosion: TComboBox;
    cbxBuildingMuzzleFlashExplosion: TComboBox;
    sgBuildingDirectionFrames: TStringGrid;
    lblBuildingDirectionFrames: TLabel;
    gbBuildingOther: TGroupBox;
    lblBuildingSellPriority: TLabel;
    seBuildingSellPriority: TSpinEdit;
    lblBuildingFlags: TLabel;
    edBuildingFlags: TEdit;
    cbBuildingFlagAUTOREPAIR: TCheckBox;
    cbBuildingFlagUNKNOWN9: TCheckBox;
    cbBuildingFlagANIM_ALPHA: TCheckBox;
    btnBuildingDirectionFrames0: TButton;
    btnBuildingDirectionFrames8: TButton;
    btnBuildingDirectionFrames32: TButton;
    btnBuildingBuildingAnimationPlay: TButton;
    tmBuildingBuildingAnimation: TTimer;
    btnBuildingBuildupArtPlay: TButton;
    tmBuildingBuildupArt: TTimer;
    pnUnitGroupList: TPanel;
    lbUnitGroupList: TListBox;
    pnUnitList: TPanel;
    lbUnitList: TListBox;
    gbUnitBasic: TGroupBox;
    lblUnitOwnerSide: TLabel;
    lblUnitName: TLabel;
    lblUnitGroup: TLabel;
    imgUnitIcon: TImage;
    clbUnitOwnerSide: TCheckListBox;
    edUnitName: TEdit;
    cbxUnitGroup: TComboBox;
    gbUnitBuildRequirements: TGroupBox;
    lblUnitTechLevel: TLabel;
    lblUnitCost: TLabel;
    lblUnitBuildSpeed: TLabel;
    lblUnitPrereq1BuildingGroup: TLabel;
    lblUnitPrereq2BuildingGroup: TLabel;
    lblUnitPrereq1UpgradesNeeded: TLabel;
    lblUnitPrereq1OwnerSide: TLabel;
    seUnitTechLevel: TSpinEdit;
    edUnitCost: TEdit;
    edUnitBuildSpeed: TEdit;
    clbUnitPrereq1OwnerSide: TCheckListBox;
    cbxUnitPrereq1BuildingGroup: TComboBox;
    cbxUnitPrereq2BuildingGroup: TComboBox;
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
    lblUnitMuzzleFlashExplosion: TLabel;
    lblUnitDirectionFrames: TLabel;
    cbxUnitUnitArt: TComboBox;
    cbxUnitBarrelArt: TComboBox;
    cbxUnitDeathExplosion: TComboBox;
    cbxUnitMuzzleFlashExplosion: TComboBox;
    sgUnitDirectionFrames: TStringGrid;
    btnUnitDirectionFrames0: TButton;
    btnUnitDirectionFrames8: TButton;
    btnUnitDirectionFrames32: TButton;
    gbUnitOther: TGroupBox;
    lblUnitUnknown52: TLabel;
    lblUnitFlags: TLabel;
    lblUnitUnknown46: TLabel;
    seUnitUnknown52: TSpinEdit;
    edUnitFlags: TEdit;
    seUnitUnknown46: TSpinEdit;
    cbUnitCanCrushInfantry: TCheckBox;
    lblUnitReportingSounds: TLabel;
    lblUnitConfirmedSounds: TLabel;
    cbUnitFlagUF_NO_AI: TCheckBox;
    PageWeapons: TTabSheet;
    PageExplosions: TTabSheet;
    PageArmour: TTabSheet;
    PageSpeed: TTabSheet;
    sgSpeedValues: TStringGrid;
    Applychanges1: TMenuItem;
    Savetofiles1: TMenuItem;
    Saveandtest1: TMenuItem;
    Reloadfiles1: TMenuItem;
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
    lbWeaponList: TListBox;
    pnExplosionList: TPanel;
    lbExplosionList: TListBox;
    pnArmourTypeList: TPanel;
    lbArmourTypeList: TListBox;
    pnWarheadList: TPanel;
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
    lblExplosionMuzzleFlashPattern: TLabel;
    lblWeaponUsedBy: TLabel;
    edExplosionMuzzleFlashPattern: TEdit;
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
    cbExplosionFlagEF_MUZZLE_FLASH: TCheckBox;
    PageOther: TTabSheet;
    vleGroupIDs: TValueListEditor;
    cbxGroupIDsSelect: TComboBox;
    PageTechpos: TTabSheet;
    imgTechposPreview: TImage;
    rgTechposTechLevel: TRadioGroup;
    sbTechposAtreides: TSpeedButton;
    sbTechposHarkonnen: TSpeedButton;
    sbTechposOrdos: TSpeedButton;
    sgTechposData: TStringGrid;
    cbxTechposUnitGroup: TComboBox;
    tbTechposNumUnits: TTrackBar;
    lblTechposNumUnits: TLabel;
    ItemExportDialog: TSaveDialog;
    ItemImportDialog: TOpenDialog;
    ArtImportDialog: TOpenDialog;
    ArtExportDialog: TSaveDialog;
    pnOtherArtControlGroup: TPanel;
    lblOtherArtList: TLabel;
    lbOtherArtList: TListBox;
    lblWeaponName: TLabel;
    edWeaponName: TEdit;
    lblExplosionName: TLabel;
    edExplosionName: TEdit;
    PageSounds: TTabSheet;
    sgSamplesUib: TStringGrid;
    lblSamplesUib: TLabel;
    sgSoundRs: TStringGrid;
    lblSoundRs: TLabel;
    btnSoundRsPlay: TButton;
    btnSoundRsExport: TButton;
    brnSoundRsReplace: TButton;
    btnSoundRsAdd: TButton;
    SoundImportDialog: TOpenDialog;
    SoundExportDialog: TSaveDialog;
    btnSoundRsRemove: TButton;
    btnWeaponFiringSoundPlay: TButton;
    btnExplosionSoundPlay: TButton;
    lblUnitVoicePriority: TLabel;
    seUnitVoicePriority: TSpinEdit;
    lblUnitProjectileShootOffset: TLabel;
    seUnitProjectileShootOffset: TSpinEdit;
    cbBuildingScreenShake: TCheckBox;
    Launchgame1: TMenuItem;
    Launchmission1: TMenuItem;
    Launchwithsettings1: TMenuItem;
    pnImagePalette: TPanel;
    imgImagePalette: TImage;
    ImageImportDialog: TOpenDialog;
    ImageExportDialog: TSaveDialog;
    edSoundRsName: TEdit;
    btnSoundRsRename: TButton;
    btnSamplesUibAdd: TButton;
    btnSamplesUibRemove: TButton;
    btnUnitVoicesExport: TButton;
    btnUnitVoicesImport: TButton;
    lblGroupIDs: TLabel;
    cbxUnitUpgradeTargetType: TComboBox;
    cbUnitUpgradeAllowed: TCheckBox;
    // Form events
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    // Main menu events
    procedure Applychanges1Click(Sender: TObject);
    procedure Savetofiles1Click(Sender: TObject);
    procedure Launchgame1Click(Sender: TObject);
    procedure Launchmission1Click(Sender: TObject);
    procedure Launchwithsettings1Click(Sender: TObject);
    procedure Reloadfiles1Click(Sender: TObject);
    // Page control events
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PageControlChange(Sender: TObject);
    // Buildings tab events
    procedure lbBuildingGroupListClick(Sender: TObject);
    procedure lbBuildingListClick(Sender: TObject);
    procedure imgBuildingIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    procedure lbUnitGroupListClick(Sender: TObject);
    procedure lbUnitListClick(Sender: TObject);
    procedure imgUnitIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    procedure btnWeaponFiringSoundPlayClick(Sender: TObject);
    procedure lbProjectileArtListClick(Sender: TObject);
    // Explosions tab events
    procedure lbExplosionListClick(Sender: TObject);
    procedure edExplosionFlagsChange(Sender: TObject);
    procedure ExplosionFlagCheckboxChange(Sender: TObject);
    procedure btnExplosionSoundPlayClick(Sender: TObject);
    procedure lbAnimationArtListClick(Sender: TObject);
    // Armour tab events
    procedure lbArmourTypeListClick(Sender: TObject);
    procedure lbWarheadListClick(Sender: TObject);
    // Speed tab events
    procedure lbSpeedTypeListClick(Sender: TObject);
    procedure btnSpeedTypeRenameClick(Sender: TObject);
    // Other tab events
    procedure vleGroupIDsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure vleGroupIDsTopLeftChanged(Sender: TObject);
    procedure cbxGroupIDsSelectChange(Sender: TObject);
    procedure lbOtherArtListClick(Sender: TObject);
    // Techpos tab events
    procedure rgTechposTechLevelClick(Sender: TObject);
    procedure sgTechposDataSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgTechposDataSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure cbxTechposUnitGroupChange(Sender: TObject);
    procedure TechposPreviewChange(Sender: TObject);
    procedure imgTechposPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // Sounds tab events
    procedure sgSamplesUibSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgSamplesUibSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure btnSamplesUibAddClick(Sender: TObject);
    procedure btnSamplesUibRemoveClick(Sender: TObject);
    procedure sgSoundRsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnSoundRsPlayClick(Sender: TObject);
    procedure btnSoundRsExportClick(Sender: TObject);
    procedure brnSoundRsReplaceClick(Sender: TObject);
    procedure btnSoundRsAddClick(Sender: TObject);
    procedure btnSoundRsRemoveClick(Sender: TObject);
    procedure btnSoundRsRenameClick(Sender: TObject);
    // Item control group events
    procedure IcgAddClick(Sender: TObject);
    procedure IcgRemoveClick(Sender: TObject);
    procedure IcgRenameClick(Sender: TObject);
    procedure IcgExportClick(Sender: TObject);
    procedure IcgImportClick(Sender: TObject);
    procedure IcgCopyClick(Sender: TObject);
    procedure IcgPasteClick(Sender: TObject);
    procedure IcgMoveUpClick(Sender: TObject);
    procedure IcgMoveDownClick(Sender: TObject);
    procedure IcgListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    // Art control group events
    procedure AcgFrameListClick(Sender: TObject);
    procedure AcgViewPaletteClick(Sender: TObject);
    procedure AcgImagePropertyChange(Sender: TObject);
    procedure AcgExportImageClick(Sender: TObject);
    procedure AcgEraseImageClick(Sender: TObject);
    procedure AcgImportImageClick(Sender: TObject);
    procedure AcgAddArtClick(Sender: TObject);
    procedure AcgRemoveArtClick(Sender: TObject);
    procedure AcgModifyArtClick(Sender: TObject);
    procedure AcgExportArtClick(Sender: TObject);
    procedure AcgImportArtClick(Sender: TObject);
    procedure AcgMoveUpArtClick(Sender: TObject);
    procedure AcgMoveDownArtClick(Sender: TObject);
    procedure AcgArtListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure imgImagePaletteClick(Sender: TObject);
  private
    // Dynamic controls
    cbxUnitVoices: array[0..17] of TComboBox;
    lblUnitVoices: array[0..17] of TLabel;
    lblBuilExpAnimIndex: array[0..MAX_BUILEXP_ANIMATIONS-1] of TLabel;
    seBuilExpAnimOffsetX: array[0..MAX_BUILEXP_ANIMATIONS-1] of TSpinEdit;
    seBuilExpAnimOffsetY: array[0..MAX_BUILEXP_ANIMATIONS-1] of TSpinEdit;
    cbxBuilExpAnimExplosion: array[0..MAX_BUILEXP_ANIMATIONS-1] of TComboBox;
    seBuilExpAnimNumFrames: array[0..MAX_BUILEXP_ANIMATIONS-1] of TSpinEdit;
    item_control_groups: array[0..8] of TItemControlGroup;
    art_control_groups: array[0..6] of TArtControlGroup;
    // Last indexes
    last_builexp_building_index: integer;
    last_speed_type_index: integer;
    // Temporary data
    tmp_building_tiles_occupied_all: cardinal;
    tmp_building_tiles_occupied_solid: cardinal;
    // Modified flags
    samples_uib_ui_modified: boolean;
    // Loading flag
    loading: boolean;
    // Pending action flags
    pending_update_sound_list: boolean;
    pending_update_contents: boolean;
    pending_update_tileset: boolean;
  public
    // Dispatcher procedures
    procedure update_game_lists;
    procedure update_sound_list;
    procedure update_contents;
    procedure update_tileset;
  private
    // Fill data procedures
    procedure fill_data(action: TFillDataAction);
    procedure fill_item_list(item_type: integer; tmp_strings: TStringList);
    procedure fill_page_data;
    procedure fill_status_bar;
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
    // Miscellaneous procedures
    function get_group_ids_cell_text(index: integer; value: shortint): string;
    // Item control group procedures
    procedure create_item_control_group(group_index: integer; fill_data_action: TFillDataAction; list_control: TListBox; edit_item_name: TEdit; layout: TItemControlGroupLayoutPtr; container: TPanel);
    // Art control group procedures
    procedure create_art_control_group(group_index: integer; is_unit: boolean; list_control: TListBox; se_directions, se_frames: TSpinEdit; container, container2: TPanel);
    procedure fill_art_control_group_frame_list(group_index: integer; first_image_index, num_frames: integer; frame_names: TStrings; selected_frame: integer);
    procedure update_art_control_group_frame_list(group_index: integer);
    // Drawing procedures
    procedure draw_no_image_sign(img_target: TImage);
    procedure draw_palette(image_index: integer);
    procedure draw_building_tile_map(img_target: TImage; value: cardinal);
    procedure draw_building_preview(draw_building: boolean);
    procedure draw_building_frame(image_index: integer; alpha, animation: boolean);
    procedure draw_unit_preview;
    procedure draw_unit_frame(image_index, side: integer; is_stealth: boolean);
    procedure draw_builexp_preview;
    procedure draw_building_art_frame(img_target: TImage; image_index, side: integer; raw, draw_background: boolean);
    procedure draw_unit_art_frame(img_target: TImage; image_index, side: integer; raw: boolean);
    procedure draw_techpos_preview;
    // General procedures
    procedure apply_changes;
    procedure save_to_files;
  end;

var
  StructuresEditor: TStructuresEditor;

implementation

uses Math, StrUtils, _settings, _tileset, _stringtable, _dispatcher, _launcher, _renderer, _graphics, _sounds, _gamelists, test_map_dialog;

{$R *.dfm}

{ TStructuresEditor }

procedure TStructuresEditor.FormCreate(Sender: TObject);
var
  tmp_strings: TStringList;
  i: integer;
begin
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
  for i := 0 to 17 do
  begin
    cbxUnitVoices[i] := TComboBox.Create(self);
    cbxUnitVoices[i].Style := csDropDownList;
    cbxUnitVoices[i].Width := 109;
    cbxUnitVoices[i].Left := 32 + 140 * (i div 9);
    cbxUnitVoices[i].Top := 36 + 24 * (i mod 9);
    cbxUnitVoices[i].Parent := gbUnitVoices;
    lblUnitVoices[i] := TLabel.Create(self);
    lblUnitVoices[i].Left := 8 + 140 * (i div 9);
    lblUnitVoices[i].Top := 36 + 24 * (i mod 9);
    lblUnitVoices[i].Caption := unit_voices[i mod 9] + ':';
    lblUnitVoices[i].Parent := gbUnitVoices;
  end;
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
  sgTechposData.Cells[0, 0] := 'Unit #';
  sgTechposData.Cells[1, 0] := 'Pos X';
  sgTechposData.Cells[2, 0] := 'Pos Y';
  sgTechposData.Cells[3, 0] := 'Atreides unit';
  sgTechposData.Cells[4, 0] := 'Harkonnen unit';
  sgTechposData.Cells[5, 0] := 'Ordos unit';
  sgTechposData.ColWidths[3] := 133;
  sgTechposData.ColWidths[4] := 133;
  sgTechposData.ColWidths[5] := 133;
  // Sounds
  sgSamplesUib.Cells[0, 0] := '#';
  sgSamplesUib.Cells[1, 0] := 'Key';
  sgSamplesUib.Cells[2, 0] := 'Value';
  sgSamplesUib.ColWidths[0] := 32;
  sgSamplesUib.ColWidths[1] := 132;
  sgSamplesUib.ColWidths[2] := 132;
  sgSoundRs.Cells[0, 0] := '#';
  sgSoundRs.Cells[1, 0] := 'File name';
  sgSoundRs.Cells[2, 0] := 'File size';
  sgSoundRs.ColWidths[0] := 32;
  sgSoundRs.ColWidths[1] := 132;
  sgSoundRs.ColWidths[2] := 132;
  // List control groups
  create_item_control_group(ITEM_BUILDING,       fdaBuildingList,      lbBuildingList,      edBuildingName,  Addr(ICG_LAYOUT_BLD_UNT),       pnBuildingList);
  create_item_control_group(ITEM_UNIT,           fdaUnitList,          lbUnitList,          edUnitName,      Addr(ICG_LAYOUT_BLD_UNT),       pnUnitList);
  create_item_control_group(ITEM_BUILDING_GROUP, fdaBuildingGroupList, lbBuildingGroupList, nil,             Addr(ICG_LAYOUT_BLD_UNT_GROUP), pnBuildingGroupList);
  create_item_control_group(ITEM_UNIT_GROUP,     fdaUnitGroupList,     lbUnitGroupList,     nil,             Addr(ICG_LAYOUT_BLD_UNT_GROUP), pnUnitGroupList);
  create_item_control_group(ITEM_WEAPON,         fdaWeaponList,        lbWeaponList,        edWeaponName,    Addr(ICG_LAYOUT_WPN_EXP),       pnWeaponList);
  create_item_control_group(ITEM_EXPLOSION,      fdaExplosionList,     lbExplosionList,     edExplosionName, Addr(ICG_LAYOUT_WPN_EXP),       pnExplosionList);
  create_item_control_group(ITEM_ARMOUR_TYPE,    fdaArmourTypeList,    lbArmourTypeList,    nil,             Addr(ICG_LAYOUT_ARM_WHD),       pnArmourTypeList);
  create_item_control_group(ITEM_WARHEAD,        fdaWarheadList,       lbWarheadList,       nil,             Addr(ICG_LAYOUT_ARM_WHD),       pnWarheadList);
  item_control_groups[ITEM_UNIT_VOICES].list_control := lbUnitList;
  item_control_groups[ITEM_UNIT_VOICES].edit_item_name := edUnitName;
  // Art control groups
  create_art_control_group(ART_BUILDING,           false, lbBuildingArtList,          seBuildingArtDirections,   nil,                       pnBuildingArtControlGroup,       pnBuildingArtList);
  create_art_control_group(ART_BUILDING_ANIMATION, false, lbBuildingAnimationArtList, nil,                       seBuildingAnimationFrames, pnBuildingAnimationControlGroup, nil);
  create_art_control_group(ART_BUILDUP,            false, lbBuildingAnimationArtList, nil,                       seBuildupArtFrames,        pnBuildupArtControlGroup,        nil);
  create_art_control_group(ART_UNIT,               true,  lbUnitArtList,              seUnitArtDirectionFrames,  seUnitArtAnimationFrames,  pnUnitArtControlGroup,           pnUnitArtList);
  create_art_control_group(ART_PROJECTILE,         true,  lbProjectileArtList,        seProjectileArtDirections, nil,                       pnProjectileArtControlGroup,     pnProjectileArtList);
  create_art_control_group(ART_ANIMATION,          true,  lbAnimationArtList,         nil,                       seAnimationArtFrames,      pnAnimationArtControlGroup,      pnAnimationArtList);
  create_art_control_group(ART_OTHER,              false, lbOtherArtList,             nil,                       nil,                       pnOtherArtControlGroup,          nil);

  tmp_strings.Destroy;
end;

procedure TStructuresEditor.FormShow(Sender: TObject);
begin
  if pending_update_sound_list then
    update_sound_list;
  if pending_update_contents then
    update_contents;
  if pending_update_tileset then
    update_tileset;
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
  if Launcher.check_game_is_running then
    exit;
  save_to_files;
  confirm_overwrite_original_file_last_answer := 0;
end;

procedure TStructuresEditor.Launchgame1Click(Sender: TObject);
begin
  apply_changes;
  if Launcher.check_game_is_running then
    exit;
  save_to_files;
  if confirm_overwrite_original_file_last_answer <> IDNO then
    Launcher.launch_game;
  confirm_overwrite_original_file_last_answer := 0;
end;

procedure TStructuresEditor.Launchmission1Click(Sender: TObject);
begin
  apply_changes;
  if not Launcher.check_map_can_be_tested then
    exit;
  save_to_files;
  if confirm_overwrite_original_file_last_answer <> IDNO then
    Launcher.launch_current_mission;
  confirm_overwrite_original_file_last_answer := 0;
end;

procedure TStructuresEditor.Launchwithsettings1Click(Sender: TObject);
begin
  apply_changes;
  if not Launcher.check_map_can_be_tested then
    exit;
  save_to_files;
  if confirm_overwrite_original_file_last_answer <> IDNO then
    if TestMapDialog.invoke = mrOk then
      Launcher.launch_current_mission;
  confirm_overwrite_original_file_last_answer := 0;
end;

procedure TStructuresEditor.Reloadfiles1Click(Sender: TObject);
begin
  Structures.load_templates_bin(true);
  Structures.load_builexp_bin(true);
  Structures.load_armour_bin(true);
  Structures.load_techpos_bin(true);
  Structures.load_speed_bin(true);
  StructGraphics.load_data_r16(true);
  Sounds.load_sound_rs(true);
  if StringTable.samples_uib_modified or samples_uib_ui_modified then
    StringTable.load_samples_uib(true);
  pnImagePalette.Visible := false;
end;

procedure TStructuresEditor.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  store_data;
end;

procedure TStructuresEditor.PageControlChange(Sender: TObject);
begin
  pnImagePalette.Visible := false;
  fill_page_data;
  fill_status_bar;
end;

procedure TStructuresEditor.lbBuildingGroupListClick(Sender: TObject);
begin
  item_control_groups[ITEM_BUILDING_GROUP].last_item_index := lbBuildingGroupList.ItemIndex;
  item_control_groups[ITEM_BUILDING_GROUP].edit_item_name.Text := Structures.templates.BuildingGroupStrings[lbBuildingGroupList.ItemIndex];
end;

procedure TStructuresEditor.lbBuildingListClick(Sender: TObject);
begin
  store_building_data;
  fill_building_data;
end;

procedure TStructuresEditor.imgBuildingIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  image_index: integer;
begin
  if lbBuildingList.ItemIndex = -1 then
    exit;
  image_index := Structures.first_building_icon_image_index + lbBuildingList.ItemIndex;
  if Button = mbLeft then
  begin
    if ImageImportDialog.Execute then
    begin
      if StructGraphics.import_single_image(ImageImportDialog.FileName, image_index, true) then
      begin
        store_building_data;
        fill_building_data;
        fill_status_bar;
      end;
    end;
  end
  else if Button = mbMiddle then
    draw_palette(image_index)
  else if Button = mbRight then
  begin
    ImageExportDialog.FileName := Structures.templates.BuildingNameStrings[lbBuildingList.ItemIndex];
    if ImageExportDialog.Execute then
      StructGraphics.export_single_image(ImageExportDialog.FileName, image_index);
  end;
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
  num_frames := Structures.templates.BuildupArtFrames[lbBuildingList.ItemIndex];
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

procedure TStructuresEditor.lbUnitGroupListClick(Sender: TObject);
begin
  item_control_groups[ITEM_UNIT_GROUP].last_item_index := lbUnitGroupList.ItemIndex;
  item_control_groups[ITEM_UNIT_GROUP].edit_item_name.Text := Structures.templates.UnitGroupStrings[lbUnitGroupList.ItemIndex];
end;

procedure TStructuresEditor.lbUnitListClick(Sender: TObject);
begin
  store_unit_data;
  fill_unit_data;
end;

procedure TStructuresEditor.imgUnitIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  image_index: integer;
begin
  if lbUnitList.ItemIndex = -1 then
    exit;
  image_index := Structures.first_unit_icon_image_index + lbUnitList.ItemIndex;
  if Button = mbLeft then
  begin
    if ImageImportDialog.Execute then
    begin
      if StructGraphics.import_single_image(ImageImportDialog.FileName, image_index, true) then
      begin
        store_unit_data;
        fill_unit_data;
        fill_status_bar;
      end;
    end;
  end
  else if Button = mbMiddle then
    draw_palette(image_index)
  else if Button = mbRight then
  begin
    ImageExportDialog.FileName := Structures.templates.UnitNameStrings[lbUnitList.ItemIndex];
    if ImageExportDialog.Execute then
      StructGraphics.export_single_image(ImageExportDialog.FileName, image_index);
  end;
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
  art_control_groups[ART_BUILDING].last_item_index := index;

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
  header := StructGraphics.get_structure_image_header(first_image_index + 1);
  if (header = nil) or (header.EntryType = 0) then
    selected_frame := 0;
  fill_art_control_group_frame_list(ART_BUILDING, first_image_index, directions * 2 + 1, tmp_strings, IfThen(sender <> nil, selected_frame, art_control_groups[ART_BUILDING].frame_list.ItemIndex));
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
  art_control_groups[ART_BUILDING_ANIMATION].last_item_index := index;

  first_image_index := Structures.building_animation_image_indexes[index];
  num_frames := Structures.templates.BuildingAnimationFrames[index];
  fill_art_control_group_frame_list(ART_BUILDING_ANIMATION, first_image_index, num_frames, nil, IfThen(sender <> nil, 0, art_control_groups[ART_BUILDING_ANIMATION].frame_list.ItemIndex));
  seBuildingAnimationFrames.Value := num_frames;

  first_image_index := Structures.buildup_art_image_indexes[index];
  num_frames := Structures.templates.BuildupArtFrames[index];
  fill_art_control_group_frame_list(ART_BUILDUP, first_image_index, num_frames, nil, IfThen(sender <> nil, 0, art_control_groups[ART_BUILDUP].frame_list.ItemIndex));
  seBuildupArtFrames.Value := num_frames;
end;

procedure TStructuresEditor.btnBuildingAnimationArtModifyClick(Sender: TObject);
begin
  Structures.modify_art(ART_BUILDING_ANIMATION, lbBuildingAnimationArtList.ItemIndex, 0, seBuildingAnimationFrames.Value);
  Structures.modify_art(ART_BUILDUP,            lbBuildingAnimationArtList.ItemIndex, 0, seBuildupArtFrames.Value);
  fill_data(fdaAllArt);
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
  art_control_groups[ART_UNIT].last_item_index:= index;

  tmp_strings := TStringList.Create;
  first_image_index := Structures.unit_art_image_indexes[index];
  frames := Structures.templates.UnitArtAnimationFrames[index];
  directions := Structures.templates.UnitArtDirectionFrames[index];
  for i := 0 to frames - 1 do
    for j := 0 to directions - 1 do
      tmp_strings.Add(Format('Frame %d  Dir %d ', [i, j]));

  fill_art_control_group_frame_list(ART_UNIT, first_image_index, frames * directions, tmp_strings, IfThen(sender <> nil, 0, art_control_groups[ART_UNIT].frame_list.ItemIndex));
  tmp_strings.Destroy;
  seUnitArtAnimationFrames.Value := frames;
  seUnitArtDirectionFrames.Value := directions;
end;

procedure TStructuresEditor.lbWeaponListClick(Sender: TObject);
begin
  store_weapon_data;
  fill_weapon_data;
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
  if lbProjectileArtList.ItemIndex = cbxWeaponProjectileArt.ItemIndex then
    exit;
  lbProjectileArtList.ItemIndex := cbxWeaponProjectileArt.ItemIndex;
  lbProjectileArtListClick(nil);
end;

procedure TStructuresEditor.btnWeaponFiringSoundPlayClick(Sender: TObject);
begin
  if cbxWeaponFiringSound.ItemIndex > 0 then
    Sounds.play_sound(Sounds.find_sound(StringTable.samples_uib.ValueFromIndex[cbxWeaponFiringSound.ItemIndex - 1]));
end;

procedure TStructuresEditor.lbProjectileArtListClick(Sender: TObject);
var
  index: integer;
begin
  index := lbProjectileArtList.ItemIndex;
  if index < 0 then
    exit;
  art_control_groups[ART_PROJECTILE].last_item_index := index;

  fill_art_control_group_frame_list(ART_PROJECTILE, Structures.projectile_art_image_indexes[index], Structures.templates.ProjectileArtDirections[index], nil, IfThen(sender <> nil, 0, art_control_groups[ART_PROJECTILE].frame_list.ItemIndex));
  seProjectileArtDirections.Value := Structures.templates.ProjectileArtDirections[index];
end;

procedure TStructuresEditor.lbExplosionListClick(Sender: TObject);
begin
  store_explosion_data;
  fill_explosion_data;
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
  cbExplosionFlagEF_MUZZLE_FLASH.Checked := (value and EF_MUZZLE_FLASH) <> 0;
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

procedure TStructuresEditor.btnExplosionSoundPlayClick(Sender: TObject);
begin
  if cbxExplosionSound.ItemIndex > 0 then
    Sounds.play_sound(Sounds.find_sound(StringTable.samples_uib.ValueFromIndex[cbxExplosionSound.ItemIndex - 1]));
end;

procedure TStructuresEditor.lbAnimationArtListClick(Sender: TObject);
var
  index: integer;
begin
  index := lbAnimationArtList.ItemIndex;
  if index < 0 then
    exit;
  art_control_groups[ART_ANIMATION].last_item_index := index;

  fill_art_control_group_frame_list(ART_ANIMATION, Structures.animation_art_image_indexes[index], Structures.templates.AnimationArtFrames[index], nil, IfThen(sender <> nil, 0, art_control_groups[ART_ANIMATION].frame_list.ItemIndex));
  seAnimationArtFrames.Value := Structures.templates.AnimationArtFrames[index];
end;

procedure TStructuresEditor.lbArmourTypeListClick(Sender: TObject);
var
  index: integer;
  i: integer;
  str: string;
begin
  index := lbArmourTypeList.ItemIndex;
  item_control_groups[ITEM_ARMOUR_TYPE].last_item_index := index;
  item_control_groups[ITEM_ARMOUR_TYPE].edit_item_name.Text := Structures.armour.ArmourTypeStrings[index];
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
  item_control_groups[ITEM_WARHEAD].last_item_index := index;
  item_control_groups[ITEM_WARHEAD].edit_item_name.Text := Structures.armour.WarheadStrings[index];
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
  Structures.speed_bin_modified := true;
  fill_data(fdaSpeedTypeList);
end;

procedure TStructuresEditor.vleGroupIDsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  index: integer;
  item_name_list_combo: TComboBox;
begin
  index := ARow - 1;
  item_name_list_combo := nil;
  case Structures.group_ids_byte_types[index] of
    gibtBuildingGroup: item_name_list_combo := cbxBuildingPrereq1BuildingGroup;
    gibtUnit:          item_name_list_combo := cbxUnitUpgradeTargetType;
    gibtWeapon:        item_name_list_combo := cbxBuildingPrimaryWeapon;
    gibtExplosion:     item_name_list_combo := cbxBuildingDeathExplosion;
  end;
  cbxGroupIDsSelect.Visible := Structures.group_ids_byte_types[index] <> gibtNone;
  cbxGroupIDsSelect.Tag := index;
  if Structures.group_ids_byte_types[index] <> gibtNone then
  begin
    cbxGroupIDsSelect.Items := item_name_list_combo.Items;
    cbxGroupIDsSelect.ItemIndex := Structures.templates.GroupIDs[index] + 1;
    cbxGroupIDsSelect.Top := (ARow - vleGroupIDs.TopRow + 1) * 20 + 17;
  end;
end;

procedure TStructuresEditor.vleGroupIDsTopLeftChanged(Sender: TObject);
begin
  cbxGroupIDsSelect.Visible := (Structures.group_ids_byte_types[cbxGroupIDsSelect.Tag] <> gibtNone) and (vleGroupIDs.Row >= vleGroupIDs.TopRow) and (vleGroupIDs.Row < vleGroupIDs.TopRow + vleGroupIDs.VisibleRowCount);
  cbxGroupIDsSelect.Top := (vleGroupIDs.Row - vleGroupIDs.TopRow + 1) * 20 + 17;
end;

procedure TStructuresEditor.cbxGroupIDsSelectChange(Sender: TObject);
begin
  Structures.templates.GroupIDs[cbxGroupIDsSelect.Tag] := cbxGroupIDsSelect.ItemIndex - 1;
  vleGroupIDs.Cells[1, cbxGroupIDsSelect.Tag + 1] := get_group_ids_cell_text(cbxGroupIDsSelect.Tag, cbxGroupIDsSelect.ItemIndex - 1);
end;

procedure TStructuresEditor.lbOtherArtListClick(Sender: TObject);
var
  index: integer;
  i: integer;
  frame_index: integer;
begin
  index := lbOtherArtList.ItemIndex;
  if index = -1 then
  begin
    lbOtherArtList.ItemIndex := 0;
    lbOtherArtListClick(lbOtherArtList);
    exit;
  end;
  frame_index := 0;
  for i := 0 to index - 1 do
    inc(frame_index, other_art_frames[i]);
  fill_art_control_group_frame_list(ART_OTHER, frame_index, other_art_frames[index], nil, IfThen(sender <> nil, 0, art_control_groups[ART_OTHER].frame_list.ItemIndex));
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
    sgTechposData.Cells[3, i + 1] := cbxTechposUnitGroup.Items[Structures.techpos[tech, i].Units[0] + 1];
    sgTechposData.Cells[4, i + 1] := cbxTechposUnitGroup.Items[Structures.techpos[tech, i].Units[1] + 1];
    sgTechposData.Cells[5, i + 1] := cbxTechposUnitGroup.Items[Structures.techpos[tech, i].Units[2] + 1];
  end;
  sgTechposDataSelectCell(nil, sgTechposData.Col, sgTechposData.Row, dummy);
  draw_techpos_preview;
end;

procedure TStructuresEditor.sgTechposDataSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  side, unitnum, tech: integer;
begin
  cbxTechposUnitGroup.Visible := ACol >= 3;
  if ACol < 3 then
    exit;
  side := ACol - 3;
  unitnum := ARow - 1;
  tech := rgTechposTechLevel.ItemIndex;
  cbxTechposUnitGroup.Left := 124 + side * 134;
  cbxTechposUnitGroup.Top := 301 + unitnum * 20;
  cbxTechposUnitGroup.ItemIndex := Structures.techpos[tech, unitnum].Units[side] + 1;
  cbxTechposUnitGroup.Tag := side * 10 + unitnum;
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
  Structures.techpos_bin_modified := true;
  fill_status_bar;
  draw_techpos_preview;
end;

procedure TStructuresEditor.cbxTechposUnitGroupChange(Sender: TObject);
var
  side, unitnum, tech: integer;
begin
  side := cbxTechposUnitGroup.Tag div 10;
  unitnum := cbxTechposUnitGroup.Tag mod 10;
  tech := rgTechposTechLevel.ItemIndex;
  Structures.techpos[tech, unitnum].Units[side] := cbxTechposUnitGroup.ItemIndex - 1;
  Structures.techpos_bin_modified := true;
  fill_status_bar;
  sgTechposData.Cells[3 + side, 1 + unitnum] := cbxTechposUnitGroup.Items[Structures.techpos[tech, unitnum].Units[side] + 1];
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
  unitnum := cbxTechposUnitGroup.Tag mod 10;
  for i := 0 to 9 do
    if (Structures.techpos[tech, i].PosX = pos_x) and (Structures.techpos[tech, i].PosY = pos_y) then
    begin
      unitnum := i;
      break;
    end;
  sgTechposData.Col := 3 + side;
  sgTechposData.Row := 1 + unitnum;
  cbxTechposUnitGroup.SetFocus;
end;

procedure TStructuresEditor.sgSamplesUibSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const
  Selection: TGridRect = (Left: 1; Top: 0; Right: 2; Bottom: 0);
  NoSelection: TGridRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);
var
  index: integer;
begin
  if ARow > 0 then
    index := Sounds.find_sound(sgSamplesUib.Cells[2, ARow])
  else
    index := -1;
  if (index <> -1) and (index < sgSoundRs.RowCount) then
  begin
    sgSoundRs.Selection := Selection;
    sgSoundRs.Row := index + 1;;
  end else
    sgSoundRs.Selection := NoSelection;
end;

procedure TStructuresEditor.sgSamplesUibSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
var
  cmp_value: string;
  dummy: boolean;
begin
  if ARow <> sgSamplesUib.Row then
    exit;
  if ARow - 1 >= StringTable.samples_uib.Count then
    exit;
  if ACol = 1 then
    cmp_value := StringTable.samples_uib.Names[ARow - 1]
  else
    cmp_value := StringTable.samples_uib.ValueFromIndex[ARow - 1];
  if cmp_value = Value then
    exit;
  if not samples_uib_ui_modified then
  begin
    samples_uib_ui_modified := true;
    fill_status_bar;
  end;
  sgSamplesUibSelectCell(sender, ACol, ARow, dummy);
end;

procedure TStructuresEditor.btnSamplesUibAddClick(Sender: TObject);
begin
  sgSamplesUib.RowCount := sgSamplesUib.RowCount + 1;
  sgSamplesUib.Cells[0, sgSamplesUib.RowCount - 1] := IntToStr(sgSamplesUib.RowCount - 2);
  sgSamplesUib.Row := sgSamplesUib.RowCount - 1;
  samples_uib_ui_modified := true;
  fill_status_bar;
end;

procedure TStructuresEditor.btnSamplesUibRemoveClick(Sender: TObject);
begin
  if sgSamplesUib.Row = sgSamplesUib.RowCount - 1 then
    sgSamplesUib.Row := sgSamplesUib.RowCount - 2;
  sgSamplesUib.RowCount := sgSamplesUib.RowCount - 1;
  sgSamplesUib.Cells[1, sgSamplesUib.RowCount] := '';
  sgSamplesUib.Cells[2, sgSamplesUib.RowCount] := '';
  samples_uib_ui_modified := true;
  fill_status_bar;
end;

procedure TStructuresEditor.sgSoundRsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if ARow > 0 then
    edSoundRsName.Text := Sounds.sound_rs_directory[ARow-1].name
  else
    edSoundRsName.Text := '';
end;

procedure TStructuresEditor.btnSoundRsPlayClick(Sender: TObject);
begin
  if sgSoundRs.Row > 0 then
    Sounds.play_sound(sgSoundRs.Row - 1);
end;

procedure TStructuresEditor.btnSoundRsExportClick(Sender: TObject);
begin
  if sgSoundRs.Row <= 0 then
    exit;
  SoundExportDialog.FileName := sgSoundRs.Cells[1, sgSoundRs.Row];
  if SoundExportDialog.Execute then
    Sounds.export_sound(sgSoundRs.Row - 1, SoundExportDialog.FileName);
end;

procedure TStructuresEditor.brnSoundRsReplaceClick(Sender: TObject);
begin
  if sgSoundRs.Row <= 0 then
    exit;
  if SoundImportDialog.Execute then
  begin
    Sounds.replace_sound(sgSoundRs.Row - 1, SoundImportDialog.FileName);
    fill_data(fdaNone);
  end;
end;

procedure TStructuresEditor.btnSoundRsAddClick(Sender: TObject);
begin
  if SoundImportDialog.Execute then
  begin
    Sounds.add_new_sound(SoundImportDialog.FileName);
    fill_data(fdaNone);
    sgSoundRs.Row := sgSoundRs.RowCount - 1;
  end;
end;

procedure TStructuresEditor.btnSoundRsRemoveClick(Sender: TObject);
begin
  Sounds.remove_last_sound;
  fill_data(fdaNone);
end;

procedure TStructuresEditor.btnSoundRsRenameClick(Sender: TObject);
begin
  if sgSoundRs.Row <= 0 then
    exit;
  Sounds.rename_sound(sgSoundRs.Row - 1, edSoundRsName.Text);
  fill_data(fdaNone);
end;

procedure TStructuresEditor.IcgAddClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  store_data;
  if Structures.add_new_item(group_index, IfThen(icg.edit_item_name_explicit, '', icg.edit_item_name.Text), icg.last_item_index) then
    fill_data(icg.fill_data_action);
end;

procedure TStructuresEditor.IcgRemoveClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  store_data;
  if Structures.remove_last_item(group_index) then
    fill_data(icg.fill_data_action);
end;

procedure TStructuresEditor.IcgRenameClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  if icg.list_control.ItemIndex < 0 then
    exit;
  store_data;
  // Store new item name
  Structures.rename_item(group_index, icg.list_control.ItemIndex, icg.edit_item_name.Text);
  fill_data(icg.fill_data_action);
end;

procedure TStructuresEditor.IcgExportClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  if icg.list_control.ItemIndex < 0 then
    exit;
  ItemExportDialog.Filter := Format('Dune2000 %s data (*.%s)|*.%s', [item_type_names[group_index], item_type_file_extensions[group_index], item_type_file_extensions[group_index]]);
  ItemExportDialog.DefaultExt := item_type_file_extensions[group_index];
  ItemExportDialog.Title := 'Export ' + item_type_names[group_index];
  ItemExportDialog.FileName := Format('%s.%s', [icg.edit_item_name.Text, item_type_file_extensions[group_index]]);
  if ItemExportDialog.Execute then
  begin
    store_data;
    Structures.export_item(group_index, icg.list_control.ItemIndex, ItemExportDialog.FileName);
  end;
end;

procedure TStructuresEditor.IcgImportClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  if icg.list_control.ItemIndex < 0 then
    exit;
  ItemImportDialog.Filter := Format('Dune2000 %s data (*.%s)|*.%s', [item_type_names[group_index], item_type_file_extensions[group_index], item_type_file_extensions[group_index]]);
  ItemImportDialog.DefaultExt := item_type_file_extensions[group_index];
  ItemImportDialog.Title := 'Import ' + item_type_names[group_index];
  ItemImportDialog.FileName := '';
  if ItemImportDialog.Execute then
  begin
    store_data;
    Structures.import_item(group_index, icg.list_control.ItemIndex, ItemImportDialog.FileName);
  end;
end;

procedure TStructuresEditor.IcgCopyClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  if icg.list_control.ItemIndex < 0 then
    exit;
  store_data;
  Structures.copy_item(group_index, icg.list_control.ItemIndex);
end;

procedure TStructuresEditor.IcgPasteClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  if icg.list_control.ItemIndex < 0 then
    exit;
  store_data;
  Structures.paste_item(group_index, icg.list_control.ItemIndex);
end;

procedure TStructuresEditor.IcgMoveUpClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  if icg.list_control.ItemIndex <= 0 then
    exit;
  store_data;
  Structures.swap_items(group_index, icg.list_control.ItemIndex, icg.list_control.ItemIndex - 1);
  icg.last_item_index := icg.list_control.ItemIndex - 1;
  fill_data(icg.fill_data_action);
end;

procedure TStructuresEditor.IcgMoveDownClick(Sender: TObject);
var
  group_index: integer;
  icg: TItemControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  icg := Addr(item_control_groups[group_index]);
  if (icg.list_control.ItemIndex < 0) or (icg.list_control.ItemIndex >= icg.list_control.Count - 1) then
    exit;
  store_data;
  Structures.swap_items(group_index, icg.list_control.ItemIndex, icg.list_control.ItemIndex + 1);
  icg.last_item_index := icg.list_control.ItemIndex + 1;
  fill_data(icg.fill_data_action);
end;

procedure TStructuresEditor.IcgListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  group_index: integer;
  can_export_import: boolean;
  matched: boolean;
begin
  group_index := (Sender as TListBox).Tag;
  can_export_import := Structures.item_type_pointers[group_index].export_data_size <> 0;
  matched := true;
  case key of
    45: IcgAddClick(Sender);
    46: IcgRemoveClick(Sender);
    69: if can_export_import then IcgExportClick(Sender);
    73: if can_export_import then IcgImportClick(Sender);
    67: if can_export_import then IcgCopyClick(Sender);
    86: if can_export_import then IcgPasteClick(Sender);
    33: IcgMoveUpClick(Sender);
    34: IcgMoveDownClick(Sender);
    else matched := false;
  end;
  if matched then
    key := 0;
end;

procedure TStructuresEditor.AcgFrameListClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
  image_index: integer;
  header: TR16EntryHeaderPtr;
  image_not_empty: boolean;
begin
  group_index := (Sender as TControl).Tag;
  acg := Addr(art_control_groups[group_index]);
  loading := true;
  image_index := 0;
  header := nil;
  if acg.frame_list.ItemIndex <> -1 then
  begin
    image_index := acg.first_image_index + acg.frame_list.ItemIndex;
    header := StructGraphics.get_structure_image_header(image_index);
  end;
  image_not_empty := (header <> nil) and (header.EntryType <> 0);
  if image_not_empty then
  begin
    if acg.is_unit then
      draw_unit_art_frame(acg.view_image, image_index, acg.se_side_color.Value, acg.cb_raw_image.Checked)
    else
      draw_building_art_frame(acg.view_image, image_index, acg.se_side_color.Value, acg.cb_raw_image.Checked, true);
    acg.edit_frame_width.Text := inttostr(header.FrameWidth);
    acg.edit_frame_height.Text := inttostr(header.FrameHeight);
    acg.edit_image_width.Text := inttostr(header.ImageWidth);
    acg.edit_image_height.Text := inttostr(header.ImageHeight);
    acg.se_image_offset_x.Value := IfThen(acg.is_unit, header.ImageOffsetX, header.ImageOffsetX * -1);
    acg.se_image_offset_y.Value := header.ImageOffsetY;
  end else
  begin
    draw_no_image_sign(acg.view_image);
    acg.edit_frame_width.Text := '';
    acg.edit_frame_height.Text := '';
    acg.edit_image_width.Text := '';
    acg.edit_image_height.Text := '';
    acg.se_image_offset_x.Text := '';
    acg.se_image_offset_y.Text := '';
  end;
  acg.edit_frame_width.Enabled := image_not_empty;
  acg.edit_frame_height.Enabled := image_not_empty;
  acg.se_image_offset_x.Enabled := image_not_empty;
  acg.se_image_offset_y.Enabled := image_not_empty;
  if pnImagePalette.Visible then
    AcgViewPaletteClick(Sender);
  loading := false;
end;

procedure TStructuresEditor.AcgViewPaletteClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
begin
  group_index := (Sender as TControl).Tag;
  acg := Addr(art_control_groups[group_index]);
  if acg.frame_list.ItemIndex = -1 then
    exit;
  draw_palette(acg.first_image_index + acg.frame_list.ItemIndex);
end;

procedure TStructuresEditor.AcgImagePropertyChange(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
  header: TR16EntryHeaderPtr;
begin
  if loading then
    exit;
  group_index := (Sender as TControl).Tag;
  acg := Addr(art_control_groups[group_index]);
  if acg.frame_list.ItemIndex = -1 then
    exit;
  header := StructGraphics.get_structure_image_header(acg.first_image_index + acg.frame_list.ItemIndex);
  if (header = nil) or (header.EntryType = 0) then
    exit;
  header.FrameWidth := strtointdef(acg.edit_frame_width.Text, 0);
  header.FrameHeight := strtointdef(acg.edit_frame_height.Text, 0);
  header.ImageOffsetX := IfThen(acg.is_unit, acg.se_image_offset_x.Value, acg.se_image_offset_x.Value * -1);
  header.ImageOffsetY := acg.se_image_offset_y.Value;
  StructGraphics.data_r16_modified := true;
  AcgFrameListClick(Sender);
  fill_status_bar;
end;

procedure TStructuresEditor.AcgExportImageClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
  image_index: integer;
begin
  group_index := (Sender as TControl).Tag;
  acg := Addr(art_control_groups[group_index]);
  if acg.frame_list.ItemIndex = -1 then
    exit;
  image_index := acg.first_image_index + acg.frame_list.ItemIndex;
  ImageExportDialog.FileName := 'Image_' + inttostr(image_index);
  if ImageExportDialog.Execute then
    StructGraphics.export_single_image(ImageExportDialog.FileName, image_index);
end;

procedure TStructuresEditor.AcgEraseImageClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
begin
  group_index := (Sender as TControl).Tag;
  acg := Addr(art_control_groups[group_index]);
  if acg.frame_list.ItemIndex = -1 then
    exit;
  StructGraphics.erase_single_image(acg.first_image_index + acg.frame_list.ItemIndex);
  update_art_control_group_frame_list(group_index);
  fill_status_bar;
end;

procedure TStructuresEditor.AcgImportImageClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
begin
  group_index := (Sender as TControl).Tag;
  acg := Addr(art_control_groups[group_index]);
  if acg.frame_list.ItemIndex = -1 then
    exit;
  if ImageImportDialog.Execute then
  begin
    if StructGraphics.import_single_image(ImageImportDialog.FileName, acg.first_image_index + acg.frame_list.ItemIndex, false) then
    begin
      update_art_control_group_frame_list(group_index);
      fill_status_bar;
    end;
  end;
end;

procedure TStructuresEditor.AcgAddArtClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
  directions, frames: integer;
begin
  group_index := (Sender as TComponent).Tag;
  acg := Addr(art_control_groups[group_index]);
  directions := 1;
  if acg.se_directions <> nil then
    directions := acg.se_directions.Value;
  frames := 1;
  if acg.se_frames <> nil then
    frames := acg.se_frames.Value;
  if Structures.add_new_art(group_index, directions, frames, acg.last_item_index) then
    fill_data(fdaAllArt);
end;

procedure TStructuresEditor.AcgRemoveArtClick(Sender: TObject);
begin
  if Structures.remove_last_art((Sender as TComponent).Tag) then
    fill_data(fdaAllArt);
end;

procedure TStructuresEditor.AcgModifyArtClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
  directions, frames: integer;
begin
  group_index := (Sender as TComponent).Tag;
  acg := Addr(art_control_groups[group_index]);
  directions := 1;
  if acg.se_directions <> nil then
    directions := acg.se_directions.Value;
  frames := 1;
  if acg.se_frames <> nil then
    frames := acg.se_frames.Value;
  Structures.modify_art(group_index, acg.list_control.ItemIndex, directions, frames);
  fill_data(fdaAllArt);
end;

procedure TStructuresEditor.AcgExportArtClick(Sender: TObject);
var
  group_index: integer;
  i: integer;
  filename: string;
begin
  group_index := (Sender as TComponent).Tag;
  filename := '';
  case group_index of
    ART_BUILDING:
      for i := 0 to Structures.templates.BuildingCount - 1 do
      begin
        if Structures.templates.BuildingDefinitions[i].BuildingArt = lbBuildingArtList.ItemIndex then
        begin
          filename := Structures.templates.BuildingNameStrings[i];
          break;
        end;
        if Structures.templates.BuildingDefinitions[i].BarrelArt = lbBuildingArtList.ItemIndex then
        begin
          filename := Structures.templates.BuildingNameStrings[i] + '_BARREL';
          break;
        end;
      end;
    ART_BUILDING_ANIMATION:
      filename := Structures.templates.BuildingNameStrings[lbBuildingAnimationArtList.ItemIndex] + '_ANIMATION';
    ART_BUILDUP:
      filename := Structures.templates.BuildingNameStrings[lbBuildingAnimationArtList.ItemIndex] + '_BUILDUP';
    ART_UNIT:
      for i := 0 to Structures.templates.UnitCount - 1 do
      begin
        if Structures.templates.UnitDefinitions[i].UnitArt = lbUnitArtList.ItemIndex then
        begin
          filename := Structures.templates.UnitNameStrings[i];
          break;
        end;
        if Structures.templates.UnitDefinitions[i].BarrelArt = lbUnitArtList.ItemIndex then
        begin
          filename := Structures.templates.UnitNameStrings[i] + '_BARREL';
          break;
        end;
      end;
    ART_PROJECTILE:
      filename := Structures.templates.WeaponStrings[lbProjectileArtList.ItemIndex];
    ART_ANIMATION:
      filename := Structures.templates.ExplosionStrings[lbAnimationArtList.ItemIndex];
    ART_OTHER:
      filename := lbOtherArtList.Items[lbOtherArtList.ItemIndex];
  end;
  ArtExportDialog.FileName := filename;
  if ArtExportDialog.Execute then
    StructGraphics.export_image_entries(ArtExportDialog.FileName, art_control_groups[group_index].first_image_index, art_control_groups[group_index].frame_list.Count);
end;

procedure TStructuresEditor.AcgImportArtClick(Sender: TObject);
var
  group_index: integer;
begin
  group_index := (Sender as TComponent).Tag;
  ArtImportDialog.FileName := '';
  if ArtImportDialog.Execute then
  begin
    if StructGraphics.import_image_entries(ArtImportDialog.FileName, art_control_groups[group_index].first_image_index, art_control_groups[group_index].frame_list.Count) then
    begin
      update_art_control_group_frame_list(group_index);
      fill_status_bar;
    end;
  end;
end;

procedure TStructuresEditor.AcgMoveUpArtClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  acg := Addr(art_control_groups[group_index]);
  if acg.list_control.ItemIndex <= 0 then
    exit;
  Structures.swap_arts(group_index, acg.list_control.ItemIndex, acg.list_control.ItemIndex - 1);
  acg.last_item_index := acg.list_control.ItemIndex - 1;
  fill_data(fdaAllArt);
end;

procedure TStructuresEditor.AcgMoveDownArtClick(Sender: TObject);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
begin
  group_index := (Sender as TComponent).Tag;
  acg := Addr(art_control_groups[group_index]);
  if (acg.list_control.ItemIndex < 0) or (acg.list_control.ItemIndex >= acg.list_control.Count - 1) then
    exit;
  Structures.swap_arts(group_index, acg.list_control.ItemIndex, acg.list_control.ItemIndex + 1);
  acg.last_item_index := acg.list_control.ItemIndex + 1;
  fill_data(fdaAllArt);
end;

procedure TStructuresEditor.AcgArtListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  group_index: integer;
  acg: TArtControlGroupPtr;
  matched: boolean;
begin
  group_index := (Sender as TListBox).Tag;
  acg := Addr(art_control_groups[group_index]);
  matched := true;
  case key of
    45: if acg.btn_art_add <> nil then       AcgAddArtClick(Sender);
    46: if acg.btn_art_remove <> nil then    AcgRemoveArtClick(Sender);
    69: if acg.btn_art_export <> nil then    AcgExportArtClick(Sender);
    73: if acg.btn_art_import <> nil then    AcgImportArtClick(Sender);
    33: if acg.btn_art_move_up <> nil then   AcgMoveUpArtClick(Sender);
    34: if acg.btn_art_move_down <> nil then AcgMoveDownArtClick(Sender);
    else matched := false;
  end;
  if matched then
    key := 0;
end;

procedure TStructuresEditor.imgImagePaletteClick(Sender: TObject);
begin
  pnImagePalette.Visible := false;
end;

procedure TStructuresEditor.update_game_lists;
var
  list: TStringList;
begin
  list := GameLists.get_numbered_list(glBuildingBehavior);
  cbxBuildingSpecialBehavior.Items := list;
  list.Destroy;
  list := GameLists.get_numbered_list(glUnitBehavior);
  cbxUnitSpecialBehavior.Items := list;
  list.Destroy;
end;

procedure TStructuresEditor.update_sound_list;
var
  tmp_strings: TStringList;
  i: integer;
  last_index: integer;
begin
  if not Visible then
  begin
    pending_update_sound_list := true;
    exit;
  end;
  pending_update_sound_list := false;
  tmp_strings := TStringList.Create;
  tmp_strings.Capacity := StringTable.samples_uib.Count;
  sgSamplesUib.RowCount := StringTable.samples_uib.Count + 1;
  for i := 0 to StringTable.samples_uib.Count - 1 do
  begin
    tmp_strings.Add(inttostr(i) + ' - ' + StringTable.samples_uib.ValueFromIndex[i]);
    sgSamplesUib.Cells[0, i + 1] := inttostr(i);
    sgSamplesUib.Cells[1, i + 1] := StringTable.samples_uib.Names[i];
    sgSamplesUib.Cells[2, i + 1] := StringTable.samples_uib.ValueFromIndex[i];
  end;
  samples_uib_ui_modified := false;
  // Unit voices combo boxes
  for i := 0 to 17 do
  begin
    last_index := cbxUnitVoices[i].ItemIndex;
    cbxUnitVoices[i].Items := tmp_strings;
    cbxUnitVoices[i].ItemIndex := last_index;
  end;
  tmp_strings.Insert(0, '(none)');
  cbxWeaponFiringSound.Items := tmp_strings;
  cbxExplosionSound.Items := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TStructuresEditor.update_contents;
begin
  if not Visible then
  begin
    pending_update_contents := true;
    exit;
  end;
  pending_update_contents := false;
  fill_data(fdaAll);
end;

procedure TStructuresEditor.update_tileset;
begin
  if not Visible then
  begin
    pending_update_tileset := true;
    exit;
  end;
  pending_update_tileset := false;
  draw_building_preview(true);
end;

procedure TStructuresEditor.fill_data(action: TFillDataAction);
var
  tmp_strings: TStringList;
  i: integer;
begin
  tmp_strings := TStringList.Create;

  if (action = fdaBuildingGroupList) or (action = fdaAll) then
  begin
    // Building type list
    fill_item_list(ITEM_BUILDING_GROUP, tmp_strings);
    cbxBuildingGroup.Items := tmp_strings;
    tmp_strings.Insert(0, '(none)');
    cbxBuildingPrereq1BuildingGroup.Items := tmp_strings;
    cbxBuildingPrereq2BuildingGroup.Items := tmp_strings;
    cbxUnitPrereq1BuildingGroup.Items := tmp_strings;
    cbxUnitPrereq2BuildingGroup.Items := tmp_strings;
    lbBuildingGroupListClick(nil);
  end;

  if (action = fdaBuildingList) or (action = fdaAll) then
  begin
    // Building list
    fill_item_list(ITEM_BUILDING, tmp_strings);
  end;

  if (action = fdaUnitGroupList) or (action = fdaAll) then
  begin
    // Unit type list
    fill_item_list(ITEM_UNIT_GROUP, tmp_strings);
    cbxUnitGroup.Items := tmp_strings;
    tmp_strings.Insert(0, '(none)');
    cbxTechposUnitGroup.Items := tmp_strings;
    lbUnitGroupListClick(nil);
  end;

  if (action = fdaUnitList) or (action = fdaAll) then
  begin
    // Unit list
    fill_item_list(ITEM_UNIT, tmp_strings);
    tmp_strings.Insert(0, '(none)');
    cbxUnitUpgradeTargetType.Items := tmp_strings;
  end;

  if (action = fdaAllArt) or (action = fdaAll) then
  begin
    // Building art list
    tmp_strings.Clear;
    for i := 0 to Structures.templates.BuildingArtCount - 1 do
      tmp_strings.Add(Format('%.*d - %4d (%dd)', [2, i, Structures.building_art_image_indexes[i], Structures.templates.BuildingArtDirections[i]]));
    lbBuildingArtList.Items := tmp_strings;
    tmp_strings.Insert(0, '(none)');
    cbxBuildingBuildingArt.Items := tmp_strings;
    cbxBuildingBarrelArt.Items := tmp_strings;
    lbBuildingArtList.ItemIndex := Min(art_control_groups[ART_BUILDING].last_item_index, Structures.templates.BuildingArtCount - 1);
    lbBuildingArtListClick(lbBuildingArtList);
    lblBuildingArtList.Caption := Format('Building art (%d of %d)', [Structures.templates.BuildingArtCount, MAX_BUILDING_ART]);
  end;

  if (action = fdaAllArt) or (action = fdaBuildingList) or (action = fdaUnitList) or (action = fdaAll) then
  begin
    // Building animation art list
    tmp_strings.Clear;
    for i := 0 to Structures.templates.BuildingCount - 1 do
      tmp_strings.Add(Format('[ %.*d / %.*d ] %.*d %s', [2, Structures.templates.BuildingAnimationFrames[i], 2, Structures.templates.BuildupArtFrames[i], 2, i, Structures.templates.BuildingNameStrings[i]]));
    lbBuildingAnimationArtList.Items := tmp_strings;
    lbBuildingAnimationArtList.ItemIndex := Min(art_control_groups[ART_BUILDING_ANIMATION].last_item_index, Structures.templates.BuildingCount - 1);
    lbBuildingAnimationArtListClick(lbBuildingAnimationArtList);

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
  end;

  if (action = fdaBuilexpList) or (action = fdaBuildingList) or (action = fdaAll) then
  begin
    // BuilExp building list
    tmp_strings.Clear;
    for i := 0 to Structures.templates.BuildingCount - 1 do
      tmp_strings.Add(Format('[ %d ] %.*d %s', [Structures.builexp[i].NumAnimations, 2, i, Structures.templates.BuildingNameStrings[i]]));
    lbBuilExpBuildingList.Items := tmp_strings;
    lbBuilExpBuildingList.ItemIndex := Min(last_builexp_building_index, Structures.templates.BuildingCount - 1);
  end;

  if (action = fdaAllArt) or (action = fdaAll) then
  begin
    // Unit art list
    tmp_strings.Clear;
    for i := 0 to Structures.templates.UnitArtCount - 1 do
      tmp_strings.Add(Format('%.*d - %4d (%dd %df)', [2, i, Structures.unit_art_image_indexes[i], Structures.templates.UnitArtDirectionFrames[i], Structures.templates.UnitArtAnimationFrames[i]]));
    lbUnitArtList.Items := tmp_strings;
    tmp_strings.Insert(0, '(none)');
    cbxUnitUnitArt.Items := tmp_strings;
    cbxUnitBarrelArt.Items := tmp_strings;
    lbUnitArtList.ItemIndex := Min(art_control_groups[ART_UNIT].last_item_index, Structures.templates.UnitArtCount - 1);
    lbUnitArtListClick(lbUnitArtList);
    lblUnitArtList.Caption := Format('Unit art (%d of %d)', [Structures.templates.UnitArtCount, MAX_UNIT_ART]);
  end;

  if (action = fdaWeaponList) or (action = fdaAll) then
  begin
    // Weapon list
    fill_item_list(ITEM_WEAPON, tmp_strings);
    tmp_strings.Insert(0, '(none)');
    cbxBuildingPrimaryWeapon.Items := tmp_strings;
    cbxBuildingSecondaryWeapon.Items := tmp_strings;
    cbxUnitPrimaryWeapon.Items := tmp_strings;
    cbxUnitSecondaryWeapon.Items := tmp_strings;
  end;

  if (action = fdaWeaponList) or (action = fdaAllArt) or (action = fdaAll) then
  begin
    // Projectile art list
    tmp_strings.Clear;
    for i := 0 to Structures.templates.ProjectileArtCount - 1 do
      tmp_strings.Add(Format('%.*d - %4d (%dd)', [2, i, Structures.projectile_art_image_indexes[i], Structures.templates.ProjectileArtDirections[i]]));
    lbProjectileArtList.Items := tmp_strings;
    cbxWeaponProjectileArt.Items := tmp_strings;
    lbProjectileArtList.ItemIndex := Min(art_control_groups[ART_PROJECTILE].last_item_index, Structures.templates.ProjectileArtCount - 1);
    lbProjectileArtListClick(lbProjectileArtList);
  end;

  if (action = fdaExplosionList) or (action = fdaAll) then
  begin
    // Explosion list
    fill_item_list(ITEM_EXPLOSION, tmp_strings);
    for i := 0 to MAX_BUILEXP_ANIMATIONS - 1 do
      cbxBuilExpAnimExplosion[i].Items := tmp_strings;
    tmp_strings.Insert(0, '(none)');
    cbxBuildingDeathExplosion.Items := tmp_strings;
    cbxBuildingMuzzleFlashExplosion.Items := tmp_strings;
    cbxUnitDeathExplosion.Items := tmp_strings;
    cbxUnitMuzzleFlashExplosion.Items := tmp_strings;
    cbxWeaponHitExplosion.Items := tmp_strings;
    cbxWeaponTrailExplosion.Items := tmp_strings;
  end;

  if (action = fdaExplosionList) or (action = fdaAllArt) or (action = fdaAll) then
  begin
    // Animation art list
    tmp_strings.Clear;
    for i := 0 to Structures.templates.AnimationArtCount - 1 do
      tmp_strings.Add(Format('%.*d - %4d (%df)', [2, i, Structures.animation_art_image_indexes[i], Structures.templates.AnimationArtFrames[i]]));
    lbAnimationArtList.Items := tmp_strings;
    lbAnimationArtList.ItemIndex := Min(art_control_groups[ART_ANIMATION].last_item_index, Structures.templates.AnimationArtCount - 1);
    lbAnimationArtListClick(lbAnimationArtList);
  end;

  if (action = fdaArmourTypeList) or (action = fdaAll) then
  begin
    // Armour type list
    fill_item_list(ITEM_ARMOUR_TYPE, tmp_strings);
    cbxBuildingArmorType.Items := tmp_strings;
    cbxUnitArmorType.Items := tmp_strings;
    lbArmourTypeListClick(nil);
  end;

  if (action = fdaWarheadList) or (action = fdaAll) then
  begin
    // Warhead list
    fill_item_list(ITEM_WARHEAD, tmp_strings);
    cbxWeaponWarhead.Items := tmp_strings;
    lbWarheadListClick(nil);
  end;

  if (action = fdaSpeedTypeList) or (action = fdaAll) then
  begin
    // Speed type list
    tmp_strings.Clear;
    for i := 0 to Length(Structures.speed.SpeedNameStrings) - 1 do
      tmp_strings.Add(Format('%.*d - %s', [1, i, Structures.speed.SpeedNameStrings[i]]));
    lbSpeedTypeList.Items := tmp_strings;
    cbxUnitSpeedType.Items := tmp_strings;
    lbSpeedTypeList.ItemIndex := last_speed_type_index;
    lbSpeedTypeListClick(nil);
  end;

  tmp_strings.Destroy;

  // Fill specific page data
  fill_page_data;
  fill_status_bar;
end;

procedure TStructuresEditor.fill_item_list(item_type: integer; tmp_strings: TStringList);
var
  i: integer;
  ptrs: TItemTypePointersPtr;
  icg: TItemControlGroupPtr;
  s: TDummyStringRecPtr;
begin
  tmp_strings.Clear;
  ptrs := Addr(Structures.item_type_pointers[item_type]);
  icg := Addr(item_control_groups[item_type]);
  for i := 0 to ptrs.item_count_byte_ptr^ - 1 do
  begin
    s := Addr(ptrs.item_name_list_ptr[i * ptrs.item_name_length]);
    tmp_strings.Add(Format('%.*d %s%s', [2, i, IfThen(item_type < ITEM_WEAPON, '', '- '), s.str]));
  end;
  icg.list_control.Items := tmp_strings;
  icg.list_control.ItemIndex := Min(icg.last_item_index, ptrs.item_count_byte_ptr^ - 1);
  icg.lbl_header.Caption := Format('%ss (%d of %d)', [item_type_names[item_type], ptrs.item_count_byte_ptr^, ptrs.max_item_count]);
end;

procedure TStructuresEditor.fill_page_data;
var
  tmp_strings: TStringList;
  i, j: integer;
begin
  if PageControl.ActivePage = PageBuildings then
    fill_building_data
  else if PageControl.ActivePage = PageUnits then
    fill_unit_data
  else if PageControl.ActivePage = PageWeapons then
    fill_weapon_data
  else if PageControl.ActivePage = PageExplosions then
    fill_explosion_data
  else if PageControl.ActivePage = PageBuilExp then
    fill_builexp_data
  else if PageControl.ActivePage = PageArmour then
  begin
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
      sgArmourValues.Cells[13, i + 1] := IntToStr(Structures.armour.WarheadEntries[i].Radius);
      sgArmourValues.Cells[14, i + 1] := IntToStr(Structures.armour.WarheadEntries[i].InfDeath);
    end;
  end else
  if PageControl.ActivePage = PageSpeed then
  begin
    // Speed
    for i := 0 to Length(Structures.speed.SpeedNameStrings) - 1 do
      sgSpeedValues.Cells[i+1, 0] := Structures.speed.SpeedNameStrings[i];
    for i := 0 to Length(Structures.speed.Values) - 1 do
      for j := 0 to Length(Structures.speed.Values[i]) - 1 do
        sgSpeedValues.Cells[j+1, i+1] := floattostr(Round(Structures.speed.Values[i, j] * 100)/100);
  end else
  if PageControl.ActivePage = PageOther then
  begin
    // Other
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Structures.templates.GroupIDs) - 1 do
      tmp_strings.Add(Format('%s=%s', [Structures.group_ids[i], get_group_ids_cell_text(i, Structures.templates.GroupIDs[i])]));
    vleGroupIDs.Strings := tmp_strings;
    vleGroupIDs.Col := 0;
    tmp_strings.Destroy;
    lbOtherArtListClick(nil);
  end else
  if PageControl.ActivePage = PageTechpos then
  begin
    // Techpos
    rgTechposTechLevelClick(nil);
  end else
  if PageControl.ActivePage = PageSounds then
  begin
    // Sounds
    sgSoundRs.RowCount := Length(Sounds.sound_rs_directory) + 1;
    for i := 0 to Length(Sounds.sound_rs_directory) - 1 do
    begin
      sgSoundRs.Cells[0, i+1] := IntToStr(i);
      sgSoundRs.Cells[1, i+1] := Sounds.sound_rs_directory[i].name;
      sgSoundRs.Cells[2, i+1] := IntToStr(Sounds.sound_rs_directory[i].size) + ' bytes';
    end;
  end;
end;

procedure TStructuresEditor.fill_status_bar;
var
  file1, file2: string;
begin
  // Set default file names
  file1 := Structures.templates_bin_filename;
  file2 := IfThen(StructGraphics.data_r16_modified, '*', '') + StructGraphics.data_r16_filename;
  // Override file names per specific tab
  if PageControl.ActivePage = PageBuilExp then
    file1 := Structures.builexp_bin_filename
  else if PageControl.ActivePage = PageArmour then
    file1 := Structures.armour_bin_filename
  else if PageControl.ActivePage = PageSpeed then
    file1 := IfThen(Structures.speed_bin_modified, '*', '') + Structures.speed_bin_filename
  else if PageControl.ActivePage = PageTechpos then
    file1 := IfThen(Structures.techpos_bin_modified, '*', '') + Structures.techpos_bin_filename
  else if PageControl.ActivePage = PageSounds then
  begin
    file1 := IfThen(StringTable.samples_uib_modified or samples_uib_ui_modified, '*', '') + StringTable.samples_uib_filename;
    file2 := IfThen(Sounds.sound_rs_modified, '*', '') + Sounds.sound_rs_filename;
  end;
  // Fill status bar
  StatusBar.Panels[0].Text := file1;
  StatusBar.Panels[1].Text := file2;
end;

procedure TStructuresEditor.fill_building_data;
var
  index: integer;
  i: integer;
  bld: TBuildingTemplatePtr;
  icon: TBitmap;
begin
  index := lbBuildingList.ItemIndex;
  if index < 0 then
    exit;
  loading := true;
  item_control_groups[ITEM_BUILDING].last_item_index := index;
  bld := Addr(Structures.templates.BuildingDefinitions[index]);
  // Basic group box
  edBuildingName.Text := Structures.templates.BuildingNameStrings[index];
  cbxBuildingGroup.ItemIndex := bld.BuildingGroup;
  icon := StructGraphics.get_raw_structure_image(Structures.first_building_icon_image_index + index);
  if icon <> nil then
  begin
    imgBuildingIcon.Picture.Bitmap.Assign(icon);
    icon.Destroy;
  end else
    draw_no_image_sign(imgBuildingIcon);
  if pnImagePalette.Visible then
    draw_palette(Structures.first_building_icon_image_index + index);
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
  cbxBuildingPrereq1BuildingGroup.ItemIndex := bld.Prereq1BuildingGroup + 1;
  set_owner_side_field_value(clbBuildingPrereq1OwnerSide, bld.Prereq1OwnerSide);
  seBuildingPrereq1UpgradesNeeded.Value := bld.Prereq1UpgradesNeeded;
  cbxBuildingPrereq2BuildingGroup.ItemIndex := bld.Prereq2BuildingGroup + 1;
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
  cbxBuildingMuzzleFlashExplosion.ItemIndex := bld.MuzzleFlashExplosion + 1;
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
  edBuildingFlags.Text := IntToHex(bld.Flags, 8);
  seBuildingSellPriority.Value := bld.SellPriority;
  cbBuildingScreenShake.Checked := bld.ScreenShake <> 0;

  loading := false;
  edBuildingFlagsChange(nil);
end;

procedure TStructuresEditor.fill_unit_data;
var
  index: integer;
  i: integer;
  unt: TUnitTemplatePtr;
  icon: TBitmap;
begin
  index := lbUnitList.ItemIndex;
  if index < 0 then
    exit;
  item_control_groups[ITEM_UNIT].last_item_index := index;
  loading := true;
  unt := Addr(Structures.templates.UnitDefinitions[index]);
  // Basic group box
  edUnitName.Text := Structures.templates.UnitNameStrings[index];
  cbxUnitGroup.ItemIndex := unt.UnitGroup;
  icon := StructGraphics.get_raw_structure_image(Structures.first_unit_icon_image_index + index);
  if icon <> nil then
  begin
    imgUnitIcon.Picture.Bitmap.Assign(icon);
    icon.Destroy;
  end else
    draw_no_image_sign(imgUnitIcon);
  if pnImagePalette.Visible then
    draw_palette(Structures.first_unit_icon_image_index + index);
  set_owner_side_field_value(clbUnitOwnerSide, unt.OwnerSide);
  // Build requirements group box
  seUnitTechLevel.Value := unt.TechLevel;
  edUnitCost.Text := inttostr(unt.Cost);
  edUnitBuildSpeed.Text := inttostr(unt.BuildSpeed);
  cbxUnitPrereq1BuildingGroup.ItemIndex := unt.Prereq1BuildingGroup + 1;
  set_owner_side_field_value(clbUnitPrereq1OwnerSide, unt.Prereq1OwnerSide);
  seUnitPrereq1UpgradesNeeded.Value := unt.Prereq1UpgradesNeeded;
  cbxUnitPrereq2BuildingGroup.ItemIndex := unt.Prereq2BuildingGroup + 1;
  cbUnitAvailableInStarport.Checked := unt.AvailableInStarport <> 0;
  cbUnitMultiplayerOnly.Checked := unt.MultiplayerOnly <> 0;
  // Voices group box
  for i := 0 to Length(unt.Voices) - 1 do
    cbxUnitVoices[i].ItemIndex := unt.Voices[i];
  seUnitVoicePriority.Value := unt.VoicePriority;
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
  seUnitProjectileShootOffset.Value := unt.ProjectileShootOffset;
  // Visuals and animations group box
  cbxUnitUnitArt.ItemIndex := unt.UnitArt + 1;
  cbxUnitBarrelArt.ItemIndex := unt.BarrelArt + 1;
  cbxUnitDeathExplosion.ItemIndex := unt.DeathExplosion + 1;
  cbxUnitMuzzleFlashExplosion.ItemIndex := unt.MuzzleFlashExplosion + 1;
  for i := 0 to 31 do
    sgUnitDirectionFrames.Cells[i mod 8, i div 8] := inttostr(unt.DirectionFrames[i]);
  // Others and unknown group box
  seUnitUnknown46.Value := unt._ProbablyUnused46;
  seUnitUnknown52.Value := unt._ProbablyUnused52;
  edUnitFlags.Text := IntToHex(unt.Flags, 8);
  cbUnitUpgradeAllowed.Checked := unt.UpgradeAllowed <> 0;
  cbxUnitUpgradeTargetType.ItemIndex := unt.UpgradeTargetType + 1;

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
  item_control_groups[ITEM_WEAPON].last_item_index := index;
  loading := true;
  wpn := Addr(Structures.templates.WeaponDefinitions[index]);

  // Properties and behavior tab
  edWeaponName.Text := Structures.templates.WeaponStrings[index];
  edWeaponDamage.Text := IntToStr(wpn.Damage);
  cbxWeaponWarhead.ItemIndex := wpn.Warhead;
  edWeaponRange.Text := IntToStr(wpn.Range);
  cbWeaponAntiAircraft.Checked := wpn.AntiAircraft <> 0;
  // Projectile movement tab
  edWeaponProjectileSpeed.Text := IntToStr(wpn.ProjectileSpeed shr 10);
  // Visuals and sounds tab
  cbxWeaponProjectileArt.ItemIndex := wpn.ProjectileArt;
  cbxWeaponProjectileArtChange(nil);
  cbxWeaponFiringSound.ItemIndex := wpn.FiringSound + 1;
  cbxWeaponHitExplosion.ItemIndex := wpn.HitExplosion + 1;
  cbxWeaponTrailExplosion.ItemIndex := wpn.TrailExplosion + 1;
  // Others and unknown tab
  seWeaponUnknown19.Value := wpn._Unknown19;
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
  for i := 0 to Length(Structures.templates.GroupIDs) - 1 do
    if (Structures.group_ids_byte_types[i] = gibtWeapon) and ((i < 25) or (i > 48) or ((i - 25) < Structures.templates.GroupIDs[49])) and (Structures.templates.GroupIDs[i] = index) then
      str := str + Copy(Structures.group_ids[i], 2, 100) + ' (special) ';
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
  item_control_groups[ITEM_EXPLOSION].last_item_index := index;
  loading := true;
  exp := Addr(Structures.templates.ExplosionDefinitions[index]);

  edExplosionName.Text := Structures.templates.ExplosionStrings[index];
  edExplosionMyIndex.Text := IntToStr(exp.MyIndex);
  edExplosionMuzzleFlashPattern.Text := IntToBin(exp.MuzzleFlashPattern, 7);
  cbxExplosionSound.ItemIndex := exp.Sound + 1;
  edExplosionFlags.Text := IntToHex(Structures.templates.AnimationArtFlags[index], 8);
  // Used by label
  str := 'Used by: ';
  for i := 0 to Structures.templates.BuildingCount - 1 do
  begin
    if Structures.templates.BuildingDefinitions[i].DeathExplosion = index then
      str := str + Structures.templates.BuildingNameStrings[i] + ' (death) ';
    if Structures.templates.BuildingDefinitions[i].MuzzleFlashExplosion = index then
      str := str + Structures.templates.BuildingNameStrings[i] + ' (muzzle) ';
  end;
  for i := 0 to Structures.templates.UnitCount - 1 do
  begin
    if Structures.templates.UnitDefinitions[i].DeathExplosion = index then
      str := str + Structures.templates.UnitNameStrings[i] + ' (death) ';
    if Structures.templates.UnitDefinitions[i].MuzzleFlashExplosion = index then
      str := str + Structures.templates.UnitNameStrings[i] + ' (muzzle) ';
  end;
  for i := 0 to Structures.templates.WeaponCount - 1 do
  begin
    if Structures.templates.WeaponDefinitions[i].HitExplosion = index then
      str := str + Structures.templates.WeaponStrings[i] + ' (hit) ';
    if Structures.templates.WeaponDefinitions[i].TrailExplosion = index then
      str := str + Structures.templates.WeaponStrings[i] + ' (trail) ';
  end;
  for i := 0 to Length(Structures.templates.GroupIDs) - 1 do
    if (Structures.group_ids_byte_types[i] = gibtExplosion) and (Structures.templates.GroupIDs[i] = index) then
      str := str + Copy(Structures.group_ids[i], 2, 100) + ' (special) ';
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

  if lbAnimationArtList.ItemIndex = lbExplosionList.ItemIndex then
    exit;
  lbAnimationArtList.ItemIndex := lbExplosionList.ItemIndex;
  lbAnimationArtListClick(nil);
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
  sp: single;
begin
  if PageControl.ActivePage = PageBuildings then
    store_building_data
  else if PageControl.ActivePage = PageUnits then
    store_unit_data
  else if PageControl.ActivePage = PageBuilExp then
    store_builexp_data
  else if PageControl.ActivePage = PageWeapons then
    store_weapon_data
  else if PageControl.ActivePage = PageExplosions then
    store_explosion_data
  else if PageControl.ActivePage = PageArmour then
  begin
    // Armour
    for i := 0 to Structures.armour.WarheadCount - 1 do
    begin
      for j := 0 to Length(Structures.armour.WarheadEntries[i].VersusArmorType) - 1 do
        Structures.armour.WarheadEntries[i].VersusArmorType[j] := StrToIntDef(sgArmourValues.Cells[j + 1, i + 1], 0);
      Structures.armour.WarheadEntries[i].Radius := StrToIntDef(sgArmourValues.Cells[13, i + 1], 0);
      Structures.armour.WarheadEntries[i].InfDeath := StrToIntDef(sgArmourValues.Cells[14, i + 1], 0);
    end;
  end else
  if PageControl.ActivePage = PageSpeed then
  begin
    // Speed
    for i := 0 to Length(Structures.speed.Values) - 1 do
      for j := 0 to Length(Structures.speed.Values[i]) - 1 do
        begin
          sp := StrToFloatDef(sgSpeedValues.Cells[j+1, i+1], 1.0);
          if Structures.speed.Values[i, j] <> sp then
            Structures.speed_bin_modified := true;
          Structures.speed.Values[i, j] := sp;
        end;
  end else
  if PageControl.ActivePage = PageOther then
  begin
    // Other
    for i := 0 to Length(Structures.templates.GroupIDs) - 1 do
      if Structures.group_ids_byte_types[i] = gibtNone then
        Structures.templates.GroupIDs[i] := StrToIntDef(vleGroupIDs.Cells[1, i+1], 0);
  end else
  if PageControl.ActivePage = PageSounds then
  begin
    // Sounds
    if samples_uib_ui_modified then
      StringTable.store_samples_uib_data_from_stringgrid(sgSamplesUib);
  end;
end;

procedure TStructuresEditor.store_building_data;
var
  index: integer;
  i: integer;
  bld: TBuildingTemplatePtr;
begin
  index := item_control_groups[ITEM_BUILDING].last_item_index;
  if index < 0 then
    exit;
  bld := Addr(Structures.templates.BuildingDefinitions[index]);
  // Basic group box
  bld.BuildingGroup := cbxBuildingGroup.ItemIndex;
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
  bld.Prereq1BuildingGroup := cbxBuildingPrereq1BuildingGroup.ItemIndex - 1;
  bld.Prereq1OwnerSide := get_owner_side_field_value(clbBuildingPrereq1OwnerSide);
  bld.Prereq1UpgradesNeeded := seBuildingPrereq1UpgradesNeeded.Value;
  bld.Prereq2BuildingGroup := cbxBuildingPrereq2BuildingGroup.ItemIndex - 1;
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
  bld.MuzzleFlashExplosion := cbxBuildingMuzzleFlashExplosion.ItemIndex - 1;
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
  bld.Flags := strtointdef('$' + edBuildingFlags.Text, 0);
  bld.SellPriority := seBuildingSellPriority.Value;
  bld.ScreenShake := IfThen(cbBuildingScreenShake.Checked, 1, 0);
  // Store building name
  if edBuildingName.Text <> Structures.templates.BuildingNameStrings[index] then
  begin
    Structures.rename_item(ITEM_BUILDING, index, edBuildingName.Text);
    item_control_groups[ITEM_BUILDING].last_item_index := lbBuildingList.ItemIndex;
    fill_data(fdaBuildingList);
  end;
end;

procedure TStructuresEditor.store_unit_data;
var
  index: integer;
  i: integer;
  unt: TUnitTemplatePtr;
begin
  index := item_control_groups[ITEM_UNIT].last_item_index;
  if index < 0 then
    exit;
  unt := Addr(Structures.templates.UnitDefinitions[index]);
  // Basic group box
  unt.UnitGroup := cbxUnitGroup.ItemIndex;
  unt.OwnerSide := get_owner_side_field_value(clbUnitOwnerSide);
  // Build requirements group box
  unt.TechLevel := seUnitTechLevel.Value;
  unt.Cost := strtointdef(edUnitCost.Text, 0);
  unt.BuildSpeed := strtointdef(edUnitBuildSpeed.Text, 0);
  unt.Prereq1BuildingGroup := cbxUnitPrereq1BuildingGroup.ItemIndex - 1;
  unt.Prereq1OwnerSide := get_owner_side_field_value(clbUnitPrereq1OwnerSide);
  unt.Prereq1UpgradesNeeded := seUnitPrereq1UpgradesNeeded.Value;
  unt.Prereq2BuildingGroup := cbxUnitPrereq2BuildingGroup.ItemIndex - 1;
  unt.AvailableInStarport := IfThen(cbUnitAvailableInStarport.Checked, 1, 0);
  unt.MultiplayerOnly := IfThen(cbUnitMultiplayerOnly.Checked, 1, 0);
  // Voices group box
  for i := 0 to Length(unt.Voices) - 1 do
    unt.Voices[i] := cbxUnitVoices[i].ItemIndex;
  unt.VoicePriority := seUnitVoicePriority.Value;
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
  unt.ProjectileShootOffset := seUnitProjectileShootOffset.Value;
  // Visuals and animations group box
  unt.UnitArt := cbxUnitUnitArt.ItemIndex - 1;
  unt.BarrelArt := cbxUnitBarrelArt.ItemIndex - 1;
  unt.DeathExplosion := cbxUnitDeathExplosion.ItemIndex - 1;
  unt.MuzzleFlashExplosion := cbxUnitMuzzleFlashExplosion.ItemIndex - 1;
  for i := 0 to 31 do
    unt.DirectionFrames[i] := strtointdef(sgUnitDirectionFrames.Cells[i mod 8, i div 8], 0);
  // Others and unknown group box
  unt._ProbablyUnused46 := seUnitUnknown46.Value;
  unt._ProbablyUnused52 := seUnitUnknown52.Value;
  unt.Flags := strtointdef('$' + edUnitFlags.Text, 0);
  unt.UpgradeAllowed := IfThen(cbUnitUpgradeAllowed.Checked, 1, 0);
  unt.UpgradeTargetType := cbxUnitUpgradeTargetType.ItemIndex - 1;
  // Store unit name
  if edUnitName.Text <> Structures.templates.UnitNameStrings[index] then
  begin
    Structures.rename_item(ITEM_UNIT, index, edUnitName.Text);
    item_control_groups[ITEM_UNIT].last_item_index := lbUnitList.ItemIndex;
    fill_data(fdaUnitList);
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
  for i := 0 to seBuilExpNumAnimations.Value - 1 do
  begin
    bxp.AnimOffsetX[i] := seBuilExpAnimOffsetX[i].Value;
    bxp.AnimOffsetY[i] := seBuilExpAnimOffsetY[i].Value;
    bxp.AnimExplosion[i] := cbxBuilExpAnimExplosion[i].ItemIndex;
    bxp.AnimNumFrames[i] := seBuilExpAnimNumFrames[i].Value;
  end;
  if seBuilExpNumAnimations.Value <> bxp.NumAnimations then
  begin
    bxp.NumAnimations := seBuilExpNumAnimations.Value;
    last_builexp_building_index := lbBuilExpBuildingList.ItemIndex;
    fill_data(fdaBuilexpList);
  end;
end;

procedure TStructuresEditor.store_weapon_data;
var
  index: integer;
  wpn: TWeaponTemplatePtr;
begin
  index := item_control_groups[ITEM_WEAPON].last_item_index;
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
  wpn._Unknown19 := seWeaponUnknown19.Value;
  wpn.Flags := StrToIntDef('$' + edWeaponFlags.Text, 0);
  // Store weapon name
  if edWeaponName.Text <> Structures.templates.WeaponStrings[index] then
  begin
    Structures.rename_item(ITEM_WEAPON, index, edWeaponName.Text);
    item_control_groups[ITEM_WEAPON].last_item_index := lbWeaponList.ItemIndex;
    fill_data(fdaWeaponList);
  end;
end;

procedure TStructuresEditor.store_explosion_data;
var
  index: integer;
  exp: TExplosionTemplatePtr;
begin
  index := item_control_groups[ITEM_EXPLOSION].last_item_index;
  if index < 0 then
    exit;
  exp := Addr(Structures.templates.ExplosionDefinitions[index]);

  exp.MyIndex := StrToIntDef(edExplosionMyIndex.Text, 0);
  exp.MuzzleFlashPattern := BinToInt(edExplosionMuzzleFlashPattern.Text);
  exp.Sound := cbxExplosionSound.ItemIndex - 1;
  Structures.templates.AnimationArtFlags[index] := StrToIntDef('$' + edExplosionFlags.Text, 0);
  // Store explosion name
  if edExplosionName.Text <> Structures.templates.ExplosionStrings[index] then
  begin
    Structures.rename_item(ITEM_EXPLOSION, index, edExplosionName.Text);
    item_control_groups[ITEM_EXPLOSION].last_item_index := lbExplosionList.ItemIndex;
    fill_data(fdaExplosionList);
  end;
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

function TStructuresEditor.get_group_ids_cell_text(index: integer; value: shortint): string;
var
  item_name_list_combo: TComboBox;
begin
  item_name_list_combo := nil;
  case Structures.group_ids_byte_types[index] of
    gibtBuildingGroup: item_name_list_combo := cbxBuildingPrereq1BuildingGroup;
    gibtUnit:          item_name_list_combo := cbxUnitUpgradeTargetType;
    gibtWeapon:        item_name_list_combo := cbxBuildingPrimaryWeapon;
    gibtExplosion:     item_name_list_combo := cbxBuildingDeathExplosion;
  end;
  if item_name_list_combo <> nil then
    result := item_name_list_combo.Items[value + 1]
  else
    result := inttostr(value);
end;

procedure TStructuresEditor.create_item_control_group(group_index: integer; fill_data_action: TFillDataAction; list_control: TListBox; edit_item_name: TEdit; layout: TItemControlGroupLayoutPtr; container: TPanel);
var
  icg: TItemControlGroupPtr;
  base_top: integer;
  i: integer;
begin
  icg := Addr(item_control_groups[group_index]);
  icg.last_item_index := 0;
  icg.fill_data_action := fill_data_action;
  icg.list_control := list_control;
  icg.lbl_header := TLabel.Create(self);
  icg.lbl_header.Parent := container;
  base_top := list_control.Height + list_control.Top + 7;
  if edit_item_name = nil then
  begin
    icg.edit_item_name := TEdit.Create(self);
    icg.edit_item_name.Width := container.Width;
    icg.edit_item_name.Top := base_top;
    icg.edit_item_name.MaxLength := 49;
    icg.edit_item_name.Parent := container;
    icg.edit_item_name_explicit := false;
    base_top := base_top + 27;
  end else
  begin
    icg.edit_item_name := edit_item_name;
    icg.edit_item_name_explicit := true;
  end;
  for i := 0 to Length(icg.buttons) - 1 do
  begin
    if layout[i].row = 0 then
    begin
      icg.buttons[i] := nil;
      continue;
    end;
    icg.buttons[i] := TButton.Create(self);
    icg.buttons[i].Left := layout[i].left;
    icg.buttons[i].Top := (layout[i].row - 1) * 25 + base_top;
    icg.buttons[i].Width := layout[i].width;
    icg.buttons[i].Tag := group_index;
    icg.buttons[i].Caption := ICG_BTN_CAPTIONS[i];
    case i of
      ICG_BTN_ADD:       icg.buttons[i].OnClick := IcgAddClick;
      ICG_BTN_REMOVE:    icg.buttons[i].OnClick := IcgRemoveClick;
      ICG_BTN_RENAME:    icg.buttons[i].OnClick := IcgRenameClick;
      ICG_BTN_EXPORT:    icg.buttons[i].OnClick := IcgExportClick;
      ICG_BTN_IMPORT:    icg.buttons[i].OnClick := IcgImportClick;
      ICG_BTN_COPY:      icg.buttons[i].OnClick := IcgCopyClick;
      ICG_BTN_PASTE:     icg.buttons[i].OnClick := IcgPasteClick;
      ICG_BTN_MOVEUP:    icg.buttons[i].OnClick := IcgMoveUpClick;
      ICG_BTN_MOVEDOWN:  icg.buttons[i].OnClick := IcgMoveDownClick;
    end;
    icg.buttons[i].Parent := container;
  end;
end;

procedure TStructuresEditor.create_art_control_group(group_index: integer; is_unit: boolean; list_control: TListBox; se_directions, se_frames: TSpinEdit; container, container2: TPanel);
var
  acg: TArtControlGroupPtr;
begin
  acg := Addr(art_control_groups[group_index]);
  acg.is_unit := is_unit;
  // Store references to referenced controls
  acg.list_control := list_control;
  acg.se_directions := se_directions;
  acg.se_frames := se_frames;
  // Create dynamic controls
  acg.view_image := TImage.Create(self);
  acg.view_image.Parent := container;
  acg.view_image.Width := IfThen(is_unit, 160, 128);
  acg.view_image.Height := 160;
  acg.view_image.Top := 16;
  acg.btn_view_palette := TButton.Create(self);
  acg.btn_view_palette.Left := 0;
  acg.btn_view_palette.Top := 180;
  acg.btn_view_palette.Width := 55;
  acg.btn_view_palette.Height := 21;
  acg.btn_view_palette.Caption := 'View pal.';
  acg.btn_view_palette.Tag := group_index;
  acg.btn_view_palette.OnClick := AcgViewPaletteClick;
  acg.btn_view_palette.Parent := container;
  acg.lbl_side_color := TLabel.Create(self);
  acg.lbl_side_color.Left := 58;
  acg.lbl_side_color.Top := 184;
  acg.lbl_side_color.Caption := 'Side:';
  acg.lbl_side_color.Parent := container;
  acg.se_side_color := TSpinEdit.Create(self);
  acg.se_side_color.Left := 86;
  acg.se_side_color.Top := 180;
  acg.se_side_color.MinValue := 0;
  acg.se_side_color.MaxValue := 7;
  acg.se_side_color.Width := 37;
  acg.se_side_color.Tag := group_index;
  acg.se_side_color.Parent := container;
  acg.se_side_color.OnChange := AcgFrameListClick;
  acg.cb_raw_image := TCheckBox.Create(self);
  acg.cb_raw_image.Left := 130;
  acg.cb_raw_image.Top := 182;
  acg.cb_raw_image.Caption := 'Raw';
  acg.cb_raw_image.Tag := group_index;
  acg.cb_raw_image.Parent := container;
  acg.cb_raw_image.OnClick := AcgFrameListClick;
  acg.btn_image_export := TButton.Create(self);
  acg.btn_image_export.Top := 204;
  acg.btn_image_export.Width := 55;
  acg.btn_image_export.Height := 21;
  acg.btn_image_export.Caption := 'Export';
  acg.btn_image_export.Tag := group_index;
  acg.btn_image_export.OnClick := AcgExportImageClick;
  acg.btn_image_export.Parent := container;
  acg.btn_image_erase := TButton.Create(self);
  acg.btn_image_erase.Left := 57;
  acg.btn_image_erase.Top := 204;
  acg.btn_image_erase.Width := 55;
  acg.btn_image_erase.Height := 21;
  acg.btn_image_erase.Caption := 'Erase';
  acg.btn_image_erase.Tag := group_index;
  acg.btn_image_erase.OnClick := AcgEraseImageClick;
  acg.btn_image_erase.Parent := container;
  acg.btn_image_import := TButton.Create(self);
  acg.btn_image_import.Left := 114;
  acg.btn_image_import.Top := 204;
  acg.btn_image_import.Width := 55;
  acg.btn_image_import.Height := 21;
  acg.btn_image_import.Caption := 'Import';
  acg.btn_image_import.Tag := group_index;
  acg.btn_image_import.OnClick := AcgImportImageClick;
  acg.btn_image_import.Parent := container;
  acg.lbl_frame_width := TLabel.Create(self);
  acg.lbl_frame_width.Parent := container;
  acg.lbl_frame_width.Caption := 'Frame W:';
  acg.lbl_frame_width.Top := 228;
  acg.lbl_frame_height := TLabel.Create(self);
  acg.lbl_frame_height.Parent := container;
  acg.lbl_frame_height.Caption := 'Frame H:';
  acg.lbl_frame_height.Top := 228;
  acg.lbl_frame_height.Left := 88;
  acg.lbl_image_width := TLabel.Create(self);
  acg.lbl_image_width.Parent := container;
  acg.lbl_image_width.Caption := 'Image W:';
  acg.lbl_image_width.Top := 252;
  acg.lbl_image_height := TLabel.Create(self);
  acg.lbl_image_height.Parent := container;
  acg.lbl_image_height.Caption := 'Image H:';
  acg.lbl_image_height.Top := 252;
  acg.lbl_image_height.Left := 88;
  acg.lbl_image_offset_x := TLabel.Create(self);
  acg.lbl_image_offset_x.Parent := container;
  acg.lbl_image_offset_x.Caption := 'Offs.X:';
  acg.lbl_image_offset_x.Top := 276;
  acg.lbl_image_offset_y := TLabel.Create(self);
  acg.lbl_image_offset_y.Parent := container;
  acg.lbl_image_offset_y.Caption := 'Offs.Y:';
  acg.lbl_image_offset_y.Top := 276;
  acg.lbl_image_offset_y.Left := 88;
  acg.edit_frame_width := TEdit.Create(self);
  acg.edit_frame_width.Width := 33;
  acg.edit_frame_width.Top := 228;
  acg.edit_frame_width.Left := 48;
  acg.edit_frame_width.Tag := group_index;
  acg.edit_frame_width.Parent := container;
  acg.edit_frame_width.OnChange := AcgImagePropertyChange;
  acg.edit_frame_height := TEdit.Create(self);
  acg.edit_frame_height.Width := 33;
  acg.edit_frame_height.Top := 228;
  acg.edit_frame_height.Left := 136;
  acg.edit_frame_height.Tag := group_index;
  acg.edit_frame_height.Parent := container;
  acg.edit_frame_height.OnChange := AcgImagePropertyChange;
  acg.edit_image_width := TEdit.Create(self);
  acg.edit_image_width.Parent := container;
  acg.edit_image_width.ReadOnly := true;
  acg.edit_image_width.Width := 33;
  acg.edit_image_width.Top := 252;
  acg.edit_image_width.Left := 48;
  acg.edit_image_height := TEdit.Create(self);
  acg.edit_image_height.Parent := container;
  acg.edit_image_height.ReadOnly := true;
  acg.edit_image_height.Width := 33;
  acg.edit_image_height.Top := 252;
  acg.edit_image_height.Left := 136;
  acg.se_image_offset_x := TSpinEdit.Create(self);
  acg.se_image_offset_x.Width := 45;
  acg.se_image_offset_x.Top := 276;
  acg.se_image_offset_x.Left := 36;
  acg.se_image_offset_x.Tag := group_index;
  acg.se_image_offset_x.Parent := container;
  acg.se_image_offset_x.OnChange := AcgImagePropertyChange;
  acg.se_image_offset_y := TSpinEdit.Create(self);
  acg.se_image_offset_y.Width := 45;
  acg.se_image_offset_y.Top := 276;
  acg.se_image_offset_y.Left := 124;
  acg.se_image_offset_y.Tag := group_index;
  acg.se_image_offset_y.Parent := container;
  acg.se_image_offset_y.OnChange := AcgImagePropertyChange;
  acg.frame_list := TListBox.Create(self);
  acg.frame_list.Parent := container;
  acg.frame_list.Width := 169;
  acg.frame_list.Height := 292;
  acg.frame_list.Top := 300;
  acg.frame_list.Tag := group_index;
  acg.frame_list.OnClick := AcgFrameListClick;
  acg.lbl_art_size := TLabel.Create(self);
  acg.lbl_art_size.Parent := container;
  acg.lbl_art_size.Top := 596;
  acg.btn_art_export := TButton.Create(self);
  acg.btn_art_export.Top := 612;
  acg.btn_art_export.Width := 81;
  acg.btn_art_export.Caption := 'Export art';
  acg.btn_art_export.Tag := group_index;
  acg.btn_art_export.OnClick := AcgExportArtClick;
  acg.btn_art_export.Parent := container;
  acg.btn_art_import := TButton.Create(self);
  acg.btn_art_import.Top := 612;
  acg.btn_art_import.Left := 88;
  acg.btn_art_import.Width := 81;
  acg.btn_art_import.Caption := 'Import art';
  acg.btn_art_import.Tag := group_index;
  acg.btn_art_import.OnClick := AcgImportArtClick;
  acg.btn_art_import.Parent := container;
  if container2 = nil then
    exit;
  acg.btn_art_modify := TButton.Create(self);
  acg.btn_art_modify.Top := 612;
  acg.btn_art_modify.Left := 32;
  acg.btn_art_modify.Width := 97;
  acg.btn_art_modify.Caption := 'Modify selected';
  acg.btn_art_modify.Tag := group_index;
  acg.btn_art_modify.OnClick := AcgModifyArtClick;
  acg.btn_art_modify.Parent := container2;
  if (group_index = ART_PROJECTILE) or (group_index = ART_ANIMATION) then
    exit;
  acg.btn_art_add := TButton.Create(self);
  acg.btn_art_add.Top := 562;
  acg.btn_art_add.Width := 73;
  acg.btn_art_add.Caption := 'Add new';
  acg.btn_art_add.Tag := group_index;
  acg.btn_art_add.OnClick := AcgAddArtClick;
  acg.btn_art_add.Parent := container2;
  acg.btn_art_remove := TButton.Create(self);
  acg.btn_art_remove.Top := 587;
  acg.btn_art_remove.Width := 73;
  acg.btn_art_remove.Caption := 'Remove last';
  acg.btn_art_remove.Tag := group_index;
  acg.btn_art_remove.OnClick := AcgRemoveArtClick;
  acg.btn_art_remove.Parent := container2;
  acg.btn_art_move_up := TButton.Create(self);
  acg.btn_art_move_up.Top := 562;
  acg.btn_art_move_up.Left := 88;
  acg.btn_art_move_up.Width := 73;
  acg.btn_art_move_up.Caption := 'Move up';
  acg.btn_art_move_up.Tag := group_index;
  acg.btn_art_move_up.OnClick := AcgMoveUpArtClick;
  acg.btn_art_move_up.Parent := container2;
  acg.btn_art_move_down := TButton.Create(self);
  acg.btn_art_move_down.Top := 587;
  acg.btn_art_move_down.Left := 88;
  acg.btn_art_move_down.Width := 73;
  acg.btn_art_move_down.Caption := 'Move down';
  acg.btn_art_move_down.Tag := group_index;
  acg.btn_art_move_down.OnClick := AcgMoveDownArtClick;
  acg.btn_art_move_down.Parent := container2;
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
    header := StructGraphics.get_structure_image_header(first_image_index + i);
    if (header = nil) or (header.EntryType = 0) then
      frame_size := 'empty'
    else
      frame_size := Format('%d x %d', [header.ImageWidth, header.ImageHeight]);
    tmp_strings.Add(Format('%s (%s)', [frame_name, frame_size]));
  end;
  acg.frame_list.Items := tmp_strings;
  tmp_strings.Destroy;
  acg.lbl_art_size.Caption := Format('Art size: %d bytes', [StructGraphics.get_image_entries_size(first_image_index, num_frames)]);
  acg.frame_list.ItemIndex := selected_frame;
  AcgFrameListClick(acg.frame_list);
end;

procedure TStructuresEditor.update_art_control_group_frame_list(group_index: integer);
begin
  case group_index of
    ART_BUILDING: lbBuildingArtListClick(nil);
    ART_BUILDING_ANIMATION: lbBuildingAnimationArtListClick(nil);
    ART_BUILDUP: lbBuildingAnimationArtListClick(nil);
    ART_UNIT: lbUnitArtListClick(nil);
    ART_PROJECTILE: lbProjectileArtListClick(nil);
    ART_ANIMATION: lbAnimationArtListClick(nil);
    ART_OTHER: lbOtherArtListClick(nil);
  end;
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

procedure TStructuresEditor.draw_palette(image_index: integer);
var
  palette: TR16PalettePtr;
  color: Cardinal;
  i, j: integer;
begin
  palette := StructGraphics.get_structure_image_palette(image_index);
  imgImagePalette.Canvas.Pen.Color := clBlack;
  imgImagePalette.Canvas.Brush.Color := clBlack;
  imgImagePalette.Canvas.Brush.Style := bsSolid;
  if palette = nil then
  begin
    imgImagePalette.Canvas.Rectangle(0, 0, imgImagePalette.Width, imgImagePalette.Height);
    exit;
  end;
  for j := 0 to 15 do
    for i := 0 to 15 do
      begin
        color := StructGraphics.colour_16bto32b(palette.Colors[j * 16 + i]);
        imgImagePalette.Canvas.Brush.Color := ((color and $FF0000) shr 16) or (color and $00FF00) or ((color and $0000FF) shl 16);
        imgImagePalette.Canvas.Rectangle(i * 16, j * 16, i * 16 + 17, j * 16 + 17);
      end;
  pnImagePalette.Left := 368;
  pnImagePalette.Visible := true;
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
  // Fill image with background color
  imgBuildingImage.Canvas.Pen.Color := clCream;
  imgBuildingImage.Canvas.Brush.Color := clCream;
  imgBuildingImage.Canvas.Brush.Style := bsSolid;
  imgBuildingImage.Canvas.Rectangle(0, 0, imgBuildingImage.Width, imgBuildingImage.Height);
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
  structure_image := StructGraphics.get_structure_image(image_index, side, false, false, was_already_loaded);
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
      StructGraphics.clear_last_structure_image(image_index, false);
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
  imgUnitImage.Canvas.Pen.Color := clCream;
  imgUnitImage.Canvas.Brush.Color := clCream;
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
  structure_image := StructGraphics.get_structure_image(image_index, side, true, is_stealth, was_already_loaded);
  if structure_image <> nil then
  begin
    src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
    dest_rect := Rect(structure_image.offset_x + 24, structure_image.offset_y + 24, structure_image.offset_x + 24 + structure_image.bitmap.Width, structure_image.offset_y + 24 + structure_image.bitmap.Height);
    imgUnitImage.Canvas.CopyMode := cmSrcAnd;
    imgUnitImage.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
    imgUnitImage.Canvas.CopyMode := cmSrcPaint;
    imgUnitImage.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
    if not was_already_loaded then
      StructGraphics.clear_last_structure_image(image_index, is_stealth);
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
    draw_building_art_frame(imgBuilExpImage, image_index, 0, false, false);
    // Damaged frame
    image_index := Structures.building_art_image_indexes[building_template.BuildingArt] + Structures.templates.BuildingArtDirections[building_template.BuildingArt] + 1;
    header := StructGraphics.get_structure_image_header(image_index);
    if (header = nil) or (header.EntryType = 0) then
      // If damaged frame does not exist, use healthy frame
      image_index := Structures.building_art_image_indexes[building_template.BuildingArt] + 1;
    draw_building_art_frame(imgBuilExpImage, image_index, 0, false, false);
  end;
  if building_template.BarrelArt <> -1 then
  begin
    draw_building_art_frame(imgBuilExpImage, Structures.building_art_image_indexes[building_template.BarrelArt] + 1, 0, false, false);
  end;
  // Draw animations
  for i := 0 to seBuilExpNumAnimations.Value - 1 do
  begin
    image_index := Structures.animation_art_image_indexes[cbxBuilExpAnimExplosion[i].ItemIndex] + seBuilExpAnimNumFrames[i].Value;
    structure_image := StructGraphics.get_structure_image(image_index, 0, false, false, was_already_loaded);
    if structure_image <> nil then
    begin
      header := StructGraphics.get_structure_image_header(image_index);
      src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
      off_x := seBuilExpAnimOffsetX[i].Value - header.ImageOffsetX;
      off_y := seBuilExpAnimOffsetY[i].Value - header.ImageOffsetY;
      dest_rect := Rect(off_x, off_y, off_x + structure_image.bitmap.Width, off_y + structure_image.bitmap.Height);
      imgBuilExpImage.Canvas.CopyMode := cmSrcAnd;
      imgBuilExpImage.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
      imgBuilExpImage.Canvas.CopyMode := cmSrcPaint;
      imgBuilExpImage.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
      if not was_already_loaded then
        StructGraphics.clear_last_structure_image(image_index, false);
    end;
  end;
end;

procedure TStructuresEditor.draw_building_art_frame(img_target: TImage; image_index, side: integer; raw, draw_background: boolean);
var
  structure_image: TStructureImagePtr;
  header: TR16EntryHeaderPtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
  raw_image: TBitmap;
begin
  structure_image := StructGraphics.get_structure_image(image_index, side, false, false, was_already_loaded);
  if structure_image = nil then
    exit;
  header := StructGraphics.get_structure_image_header(image_index);
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
  if not raw then
  begin
    img_target.Canvas.CopyMode := cmSrcAnd;
    img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
    img_target.Canvas.CopyMode := cmSrcPaint;
    img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
  end else
  begin
    raw_image := StructGraphics.get_raw_structure_image(image_index);
    img_target.Canvas.CopyMode := cmSrcCopy;
    img_target.Canvas.CopyRect(dest_rect, raw_image.Canvas, src_rect);
    raw_image.Destroy;
  end;
  if not was_already_loaded then
    StructGraphics.clear_last_structure_image(image_index, false);
end;

procedure TStructuresEditor.draw_unit_art_frame(img_target: TImage; image_index, side: integer; raw: boolean);
var
  structure_image: TStructureImagePtr;
  header: TR16EntryHeaderPtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
  raw_image: TBitmap;
begin
  structure_image := StructGraphics.get_structure_image(image_index, side, true, false, was_already_loaded);
  if structure_image = nil then
    exit;
  header := StructGraphics.get_structure_image_header(image_index);
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
  if not raw then
  begin
    img_target.Canvas.CopyMode := cmSrcAnd;
    img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
    img_target.Canvas.CopyMode := cmSrcPaint;
    img_target.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
  end else
  begin
    raw_image := StructGraphics.get_raw_structure_image(image_index);
    img_target.Canvas.CopyMode := cmSrcCopy;
    img_target.Canvas.CopyRect(dest_rect, raw_image.Canvas, src_rect);
    raw_image.Destroy;
  end;
  if not was_already_loaded then
    StructGraphics.clear_last_structure_image(image_index, false);
end;

procedure TStructuresEditor.draw_techpos_preview;
var
  i: integer;
  tech, side: integer;
  unit_group: integer;
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
    unit_group := Structures.techpos[tech, i].Units[side];
    pos_x := ((TECHPOS_PREVIEW_SIZE div 2) + Structures.techpos[tech, i].PosX) * 32;
    pos_y := ((TECHPOS_PREVIEW_SIZE div 2) + Structures.techpos[tech, i].PosY) * 32;
    if unit_group = -1 then
      continue;
    unit_template := Structures.get_unit_template(unit_group, side, true);
    if (unit_template = nil) or (unit_template.SpecialBehavior = 5) then
      continue;
    // Check if unit is stealth
    is_stealth := ((unit_template.Flags and $10) <> 0) or (unit_template.SpecialBehavior = 12) or (Structures.templates.UnitNameStrings[Structures.unit_side_versions[unit_group, side]] = 'STEALTH RAIDER');
    // Draw unit
    if unit_template.UnitArt <> -1 then
    begin
      structure_image := StructGraphics.get_structure_image(Structures.unit_art_image_indexes[unit_template.UnitArt], side, true, is_stealth, was_already_loaded);
      if structure_image <> nil then
        Renderer.draw_structure_image(imgTechposPreview.Canvas, pos_x + structure_image.offset_x, pos_y + structure_image.offset_y, 0, 0, imgTechposPreview.Width, imgTechposPreview.Height, structure_image);
    end;
    // Draw barrel
    if unit_template.BarrelArt <> -1 then
    begin
      structure_image := StructGraphics.get_structure_image(Structures.unit_art_image_indexes[unit_template.BarrelArt], side, true, is_stealth, was_already_loaded);
      if structure_image <> nil then
        Renderer.draw_structure_image(imgTechposPreview.Canvas, pos_x + structure_image.offset_x, pos_y + structure_image.offset_y, 0, 0, imgTechposPreview.Width, imgTechposPreview.Height, structure_image);
    end;
  end;
end;

procedure TStructuresEditor.apply_changes;
begin
  store_data;
  Structures.compute_building_and_unit_side_versions;
  Structures.compute_building_group_mapping;
  Dispatcher.register_event(evACStructuresEditor);
end;

procedure TStructuresEditor.save_to_files;
begin
  Structures.save_templates_bin;
  Structures.save_builexp_bin;
  Structures.save_armour_bin;
  Structures.save_speed_bin;
  Structures.save_techpos_bin;
  StructGraphics.save_data_r16;
  Sounds.save_sound_rs;
  StringTable.save_samples_uib;
  fill_status_bar;
end;

end.
