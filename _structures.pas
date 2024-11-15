unit _structures;

interface

uses Windows, Classes, SysUtils, Math, Types, _utils, _eventconfig;

// *****************************************************************************
// TEMPLATES.BIN file definitions
// *****************************************************************************

const UF_STEALTH      = $00000010;
const UF_NO_AI        = $00000800;
const UF_FIXED_BARREL = $00008000;
const UF_SELFHEALING  = $00800000;

type
  TUnitTemplate = packed record
    OwnerHouse:            byte;
    UnitGroup:             byte;
    ArmorType:             byte;
    UnitRotationSpeed:     byte;
    HitPoints:             cardinal;
    Speed:                 cardinal;
    PrimaryWeapon:         shortint;
    SecondaryWeapon:       shortint;
    RateOfFire:            byte;
    BarrelRotationSpeed:   byte;
    SightRadius:           byte;
    IsInfantry:            byte;
    _ZeroPadding18:        byte;
    _ZeroPadding19:        byte;
    UnitArt:               integer;
    BarrelArt:             integer;
    Cost:                  cardinal;
    BuildSpeed:            cardinal;
    TechLevel:             shortint;
    AvailableInStarport:   byte;
    HasBarrel:             byte;
    Prereq1UpgradesNeeded: byte;
    Prereq1BuildingGroup:  integer;
    Prereq1OwnerHouse:     byte;
    SpecialBehavior:       byte;
    _ProbablyUnused46:     byte;
    DeathExplosion:        shortint;
    Prereq2BuildingGroup:  integer;
    _ProbablyUnused52:     byte;
    CanCrushInfantry:      byte;
    HealthBarSize:         byte;
    ProjectileShootOffset: shortint;
    Flags:                 cardinal;
    DirectionFrames:       array[0..31] of byte;
    Voices:                array[0..17] of cardinal;
    VoicePriority:         cardinal;
    MuzzleFlashExplosion:  shortint;
    SpeedType:             byte;
    MultiplayerOnly:       byte;
    // Extra properties added by mod
    UpgradeAllowed:        byte;
    UpgradeTargetType:     byte;
    MovementRestriction:   byte;
    _ZeroBytes:            array[0..81] of byte;
  end;

  TUnitTemplatePtr = ^TUnitTemplate;

const BF_REPAIRING      = $00000001;
const BF_ANIM_PERMANENT = $00000010;
const BF_HAS_ANIMATION  = $00000040;
const BF_UNKNOWN9       = $00000100;
const BF_SELECT_REPAIR  = $00000200;
const BF_CAN_CAPTURE    = $00000400;
const BF_DECAY          = $00008000;
const BF_HAS_SKIRT      = $00200000;
const BF_NO_CONCRETE    = $00400000;
const BF_ANIM_ALPHA     = $00800000;
const BF_CANNOT_SELL    = $01000000;

type
  TBuildingTemplate = packed record
    HitPoints:             cardinal;
    OwnerHouse:            byte;
    ArmorType:             byte;
    BarrelRotationSpeed:   byte;
    RateOfFire:            byte;
    ScreenShake:           cardinal;
    PrimaryWeapon:         shortint;
    SecondaryWeapon:       shortint;
    SightRadius:           byte;
    ActLikeTurret:         byte;
    TechLevelBuild:        shortint;
    TechLevelUpgrade1:     shortint;
    TechLevelUpgrade2:     shortint;
    TechLevelUpgrade3:     shortint;
    BuildingArt:           integer;
    BarrelArt:             integer;
    CostBuild:             cardinal;
    CostUpgrade1:          cardinal;
    CostUpgrade2:          cardinal;
    CostUpgrade3:          cardinal;
    BuildSpeedBuild:       cardinal;
    BuildSpeedUpgrade1:    cardinal;
    BuildSpeedUpgrade2:    cardinal;
    BuildSpeedUpgrade3:    cardinal;
    PowerConsumption:      integer;
    Prereq1BuildingGroup:  integer;
    Prereq1OwnerHouse:     byte;
    Prereq1UpgradesNeeded: byte;
    _ZeroPadding70:        byte;
    _ZeroPadding71:        byte;
    Prereq2BuildingGroup:  integer;
    Prereq2OwnerHouse:     byte;
    Prereq2UpgradesNeeded: byte;
    RequireEnoughPower:    byte;
    DeathExplosion:        shortint;
    TilesOccupiedAll:      cardinal;
    TilesOccupiedSolid:    cardinal;
    Flags:                 cardinal;
    SpecialBehavior:       byte;
    SellPriority:          byte;
    _ProbablyUnused94:     byte;
    HealthBarSize:         byte;
    ExitPoint1X:           shortint;
    ExitPoint1Y:           shortint;
    ExitPoint2X:           shortint;
    ExitPoint2Y:           shortint;
    DirectionFrames:       array[0..31] of byte;
    _ProbablyUnused132:    byte;
    AnimationSpeed:        byte;
    ArtHeight:             byte;
    ArtWidth:              byte;
    BuildingGroup:         byte;
    BuildupFramesToShow:   byte;
    BuildupArt:            byte;
    BuildingAnimation:     byte;
    MuzzleFlashExplosion:  shortint;
    // Extra properties added by mod
    BuildRestriction:      byte;
    BuildMaxDistance:      byte;
    _ZeroBytes:            array[0..124] of byte;
  end;

  TBuildingTemplatePtr = ^TBuildingTemplate;

const WF_ARC_TRAJECTORY    = $00000001;
const WF_DEBRIS            = $00000002;
const WF_MAKE_TRAIL        = $00000010;
const WF_HOMING            = $00000020;
const WF_DEVIATOR          = $00000040;
const WF_ANIM_PROJECTILE   = $00000080;
const WF_FALLING           = $00000100;
const WF_PROJECTILE_ALPHA  = $00000200;
const WF_SONIC             = $00000800;
const WF_BLOCKED_BY_WALL   = $00001000;

type
  TWeaponTemplate = packed record
    ProjectileSpeed:       cardinal;
    Damage:                cardinal;
    Flags:                 cardinal;
    FiringSound:           integer;
    ProjectileArt:         byte;
    HitExplosion:          shortint;
    TrailExplosion:        shortint;
    _Unknown19:            byte;
    AntiAircraft:          byte;
    Warhead:               byte;
    _ZeroPadding22:        byte;
    _ZeroPadding23:        byte;
    Range:                 cardinal;
  end;

  TWeaponTemplatePtr = ^TWeaponTemplate;

const EF_RISE_UP           = $00000001;
const EF_SUBSTRACTIVE_ALPA = $00000002;
const EF_OR_OPERATION      = $00000004;
const EF_HOUSE_COLORED     = $00000008;
const EF_SEMI_TRANSPARENCY = $00000010;
const EF_RED               = $00000040;
const EF_GREEN             = $00000080;
const EF_ADDITIVE_ALPHA    = $00000200;
const EF_MUZZLE_FLASH      = $00000400;
const EF_INVISIBLE         = $00000800;

type
  TExplosionTemplate = packed record
    MyIndex:               byte;
    MuzzleFlashPattern:    byte;
    _ZeroPadding2:         byte;
    _ZeroPadding3:         byte;
    Sound:                 integer;
  end;

  TExplosionTemplatePtr = ^TExplosionTemplate;

const MAX_UNIT_TYPES = 60;
const MAX_BUILDING_TYPES = 100;
const MAX_WEAPONS = 64;
const MAX_EXPLOSIONS = 64;
const MAX_UNIT_ART = 90;
const MAX_BUILDING_ART = 120;

type
  TTemplatesBinFile = packed record
    UnitDefinitions:         array[0..MAX_UNIT_TYPES-1]     of TUnitTemplate;
    BuildingDefinitions:     array[0..MAX_BUILDING_TYPES-1] of TBuildingTemplate;
    WeaponDefinitions:       array[0..MAX_WEAPONS-1]        of TWeaponTemplate;
    ExplosionDefinitions:    array[0..MAX_EXPLOSIONS-1]     of TExplosionTemplate;
    UnitArtAnimationFrames:  array[0..MAX_UNIT_ART-1]       of integer;
    UnitArtDirectionFrames:  array[0..MAX_UNIT_ART-1]       of integer;
    UnitArtCount:            integer;
    UnitCount:               byte;
    AnimationArtCount:       integer;
    AnimationArtFrames:      array[0..MAX_EXPLOSIONS-1]     of integer;
    ExplosionCount:          byte;
    WeaponCount:             integer;
    ProjectileArtDirections: array[0..MAX_WEAPONS-1]        of integer;
    ProjectileArtCount:      byte;
    BuildingArtCount:        integer;
    BuildingArtDirections:   array[0..MAX_BUILDING_ART-1]   of integer;
    BuildingCount:           byte;
    BuildingNameStrings:     array[0..MAX_BUILDING_TYPES-1, 0..449] of char;
    WeaponStrings:           array[0..MAX_WEAPONS-1,        0..49]  of char;
    ExplosionStrings:        array[0..MAX_EXPLOSIONS-1,     0..49]  of char;
    UnitNameStrings:         array[0..MAX_UNIT_TYPES-1,     0..449] of char;
    UnitGroupStrings:        array[0..MAX_UNIT_TYPES-1,     0..49]  of char;
    BuildingGroupStrings:    array[0..MAX_BUILDING_TYPES-1, 0..49]  of char;
    GroupIDs:                array[0..85]                   of shortint;
    AnimationArtFlags:       array[0..MAX_EXPLOSIONS-1]     of cardinal;
    BuildingAnimationFrames: array[0..MAX_BUILDING_TYPES-1] of byte;
    BuildupArtFrames:        array[0..MAX_BUILDING_TYPES-1] of byte;
    BuildingGroupCount:      byte;
    UnitGroupCount:          byte;
  end;

// *****************************************************************************
// BUILEXP.BIN file definitions
// *****************************************************************************

const MAX_BUILEXP_ANIMATIONS = 8;

type
  TBuilExpEntry = packed record
    NumAnimations: byte;
    AnimOffsetX: array[0..MAX_BUILEXP_ANIMATIONS-1] of Shortint;
    AnimOffsetY: array[0..MAX_BUILEXP_ANIMATIONS-1] of Shortint;
    AnimExplosion: array[0..MAX_BUILEXP_ANIMATIONS-1] of byte;
    AnimNumFrames: array[0..MAX_BUILEXP_ANIMATIONS-1] of byte;
  end;

  TBuilExpEntryPtr = ^TBuilExpEntry;

  TBuilExpBinFile = array[0..MAX_BUILDING_TYPES-1] of TBuilExpEntry;

// *****************************************************************************
// ARMOUR.BIN file definitions
// *****************************************************************************

const MAX_WARHEADS = 30;
const MAX_ARMOUR_TYPES = 12;

type
  TWarheadEntry = packed record
    VersusArmorType: array[0..MAX_ARMOUR_TYPES-1] of byte;
    Radius:          cardinal;
    InfDeath:        cardinal;
  end;

  TArmourBinFile = packed record
    WarheadEntries:     array[0..MAX_WARHEADS-1] of TWarheadEntry;
    WarheadStrings:     array[0..MAX_WARHEADS-1,     0..49]  of char;
    ArmourTypeStrings:  array[0..MAX_ARMOUR_TYPES-1, 0..49]  of char;
    WarheadCount:       byte;
    ArmourTypeCount:    byte;
  end;

// *****************************************************************************
// SPEED.BIN file definitions
// *****************************************************************************

type
  TSpeedBinFile = packed record
    Values: array[0..7, 0..3] of Single;
    SpeedNameStrings: array[0..3, 0..31] of char;
    _ZeroBytes: array[0..127] of byte;
  end;

// *****************************************************************************
// TECHPOS.BIN file definitions
// *****************************************************************************

type
  TTechposUnitEntry = packed record
    Units: array[0..2] of shortint;
    PosX: shortint;
    PosY: shortint;
  end;

  TTechposBinFile = array[0..9,0..9] of TTechposUnitEntry;

// *****************************************************************************
// TILEDATA.BIN file definitions
// *****************************************************************************

const CNT_TILEDATA_ENTRIES = 1000;
const ST_NOTHING = 255;
const ST_BUILDING = 128;
const ST_UNIT = 0;
const ST_MISC_OBJECT = 1;

type
  TTileDataEntry = record
    index: word;
    side: byte;
    stype: byte;
  end;

  TTileDataEntryPtr = ^TTileDataEntry;

  TTileDataBinFile = array[0..CNT_TILEDATA_ENTRIES-1] of TTileDataEntry;

const EMPTY_TILEDATA_ENTRY: TTileDataEntry = (index: 0; side: 0; stype: ST_NOTHING);

// *****************************************************************************
// Misc. objects definitions
// *****************************************************************************

type
  TMiscObjectInfo = record
    name: String;
    value: word;
    obj_type: word;
    color: Cardinal;
    image: integer;
    mark: string;
  end;

  TMiscObjectInfoPtr = ^TMiscObjectInfo;

// Misc object types
const MOT_WORM_SPAWNER = 0;
const MOT_PLAYER_START = 1;
const MOT_SPICE_BLOOM = 2;
const MOT_CRATE = 3;

// Crate type marks
const crate_type_marks: array[0..15] of string = ('C', 'E', 'R', 'H', 'U', 'P', 'X', 'B', 'S', 'M', 'L', '11', '12', '13', '14', '15');

// *****************************************************************************
// Group IDs definitions
// *****************************************************************************

type
  TGroupIDsByteType = (gibtNone, gibtBuildingGroup, gibtUnit, gibtWeapon, gibtExplosion);

// *****************************************************************************
// Auxiliary constants and types
// *****************************************************************************

const MAX_BUILDING_SIZE = 4;

const ITEM_BUILDING         = 0;
const ITEM_UNIT             = 1;
const ITEM_BUILDING_GROUP   = 2;
const ITEM_UNIT_GROUP       = 3;
const ITEM_WEAPON           = 4;
const ITEM_EXPLOSION        = 5;
const ITEM_ARMOUR_TYPE      = 6;
const ITEM_WARHEAD          = 7;
const ITEM_UNIT_VOICES      = 8;

const item_type_names: array[0..8] of string = ('Building', 'Unit', 'Building group', 'Unit group', 'Weapon', 'Explosion', 'Armour type', 'Warhead', 'Unit voices');
const item_type_file_extensions: array[0..8] of string = ('d2kbld', 'd2kunt', '', '', 'd2kwpn', 'd2kexp', 'd2karm', 'd2kwhd', 'd2kvcs');

type
  TItemDataPointers = record
    data_ptr: TByteArrayPtr;
    data_size: integer;
  end;

  TItemDataPointersPtr = ^TItemDataPointers;

type
  TItemTypePointers = record
    item_count_byte_ptr: PByte;
    item_name_list_ptr: TByteArrayPtr;
    item_name_length: integer;
    max_item_count: integer;
    item_data_pointers_first: integer;
    item_data_pointers_count: integer;
    export_data_size: integer;
    clipboard_format: cardinal;
  end;

  TItemTypePointersPtr = ^TItemTypePointers;

const ART_BUILDING           = 0;
const ART_BUILDING_ANIMATION = 1;
const ART_BUILDUP            = 2;
const ART_UNIT               = 3;
const ART_PROJECTILE         = 4;
const ART_ANIMATION          = 5;
const ART_OTHER              = 6;

const art_type_names: array[0..5] of string = ('Building', 'Building animation', 'Buildup', 'Unit', 'Projectile', 'Animation');
const other_art_frames: array[0..15] of byte = (3, 1, 1, 4, 6, 2, 10, 11, 64, 4, 4, 4, 32, 32, 16, 12);

type
  TArtTypePointers = record
    art_count_byte_ptr: PByte;
    directions_list_ptr: TByteArrayPtr;
    frames_list_ptr: TByteArrayPtr;
    frames_size: integer;
    max_art_count: integer;
    image_indexes_list_ptr: TWordArrayPtr;
    next_free_image_index_ptr: PWord;
  end;

  TArtTypePointersPtr = ^TArtTypePointers;

type
  TRemapStructuresData = record
    remap_units: array[0..MAX_UNIT_TYPES-1] of byte;
    remap_unitgroups: array[0..MAX_UNIT_TYPES-1] of byte;
    remap_buildings: array[0..MAX_BUILDING_TYPES-1] of byte;
    remap_buildinggroups: array[0..MAX_BUILDING_TYPES-1] of byte;
    remap_weapons: array[0..MAX_WEAPONS-1] of byte;
    remap_explosions: array[0..MAX_EXPLOSIONS-1] of byte;
  end;

  TRemapStructuresDataPtr = ^TRemapStructuresData;

// *****************************************************************************
// Data Export / Import structures
// *****************************************************************************

type
  TItemReference = record
    item_name: array[0..49] of char;
  end;

type
  TArtReference = record
    directions: integer;
    frames: integer;
    art_size: integer;
    checksum: integer;
  end;

type
  TSoundReference = record
    key: array[0..19] of char;
    value: array[0..9] of char;
  end;

type
  TBuildingExportData = packed record
    building_name:      array[0..449] of char;
    building_template:  TBuildingTemplate;
    builexp_entry:      TBuilExpEntry;
    icon_data:          array[0..3368] of byte;
    item_references:    array[0..15] of TItemReference;
    art_references:     array[0..3] of TArtReference;
    building_group_str: array[0..49] of char;
  end;

  TBuildingExportDataPtr = ^TBuildingExportData;

type
  TUnitExportData = packed record
    unit_name:        array[0..449] of char;
    unit_template:    TUnitTemplate;
    icon_data:        array[0..3368] of byte;
    item_references:  array[0..7] of TItemReference;
    art_references:   array[0..1] of TArtReference;
    sound_references: array[0..17] of TSoundReference;
    unit_group_str:   array[0..49] of char;
  end;

  TUnitExportDataPtr = ^TUnitExportData;

type
  TWeaponExportData = packed record
    weapon_name:     array[0..49] of char;
    weapon_template: TWeaponTemplate;
    item_references: array[0..2] of TItemReference;
    art_reference:   TArtReference;
    sound_reference: TSoundReference;
  end;

  TWeaponExportDataPtr = ^TWeaponExportData;

type
  TExplosionExportData = packed record
    explosion_name:      array[0..49] of char;
    explosion_template:  TExplosionTemplate;
    animation_art_flags: cardinal;
    art_reference:       TArtReference;
    sound_reference:     TSoundReference;
  end;

  TExplosionExportDataPtr = ^TExplosionExportData;

type
  TArmourTypeExportData = packed record
    armour_type_name:      array[0..49] of char;
    versus_warhead_values: array[0..MAX_WARHEADS-1] of byte;
    warhead_references:    array[0..MAX_WARHEADS-1] of TItemReference;
  end;

  TArmourTypeExportDataPtr = ^TArmourTypeExportData;

type
  TWarheadExportData = packed record
    warhead_name:           array[0..49] of char;
    warhead_entry:          TWarheadEntry;
    armour_type_references: array[0..MAX_ARMOUR_TYPES-1] of TItemReference;
  end;

  TWarheadExportDataPtr = ^TWarheadExportData;

type
  TUnitVoicesExportData = packed record
    voices:           array[0..17] of cardinal;
    sound_references: array[0..17] of TSoundReference;
  end;

  TUnitVoicesExportDataPtr = ^TUnitVoicesExportData;

// *****************************************************************************
// TStructures class
// *****************************************************************************

type
  TStructures = class

  public
    // File names
    templates_bin_filename: String;
    builexp_bin_filename: String;
    armour_bin_filename: String;
    speed_bin_filename: String;
    techpos_bin_filename: String;
    tiledata_bin_filename: String;
    sides_ini_filename: String;
    misc_objects_ini_filename: String;
    limits_ini_filename: String;

    // TEMPLATES.BIN related data
    templates: TTemplatesBinFile;

    unit_art_image_indexes:           array[0..MAX_UNIT_ART-1] of word;
    building_art_image_indexes:       array[0..MAX_BUILDING_ART-1] of word;
    projectile_art_image_indexes:     array[0..MAX_WEAPONS-1] of word;
    animation_art_image_indexes:      array[0..MAX_EXPLOSIONS-1] of word;
    first_unit_icon_image_index:      word;
    first_building_icon_image_index:  word;
    buildup_art_image_indexes:        array[0..MAX_BUILDING_TYPES-1] of word;
    building_animation_image_indexes: array[0..MAX_BUILDING_TYPES-1] of word;

    unit_house_versions:              array[0..MAX_UNIT_TYPES-1,     0..CNT_SIDES-1] of shortint;
    building_house_versions:          array[0..MAX_BUILDING_TYPES-1, 0..CNT_SIDES-1] of shortint;

    building_group_mapping:           array[0..MAX_BUILDING_TYPES-1] of shortint;
    building_group_mapping_count:     integer;

    // BUILEXP.BIN related data
    builexp: TBuilExpBinFile;

    // ARMOUR.BIN related data
    armour: TArmourBinFile;

    // SPEED.BIN related data
    speed: TSpeedBinFile;
    speed_bin_modified: boolean;

    // TECHPOS.BIN related data
    techpos: TTechposBinFile;
    techpos_bin_modified: boolean;

    // TILEDATA.BIN related data
    tiledata: TTileDataBinFile;

    // Misc. objects related data
    misc_object_info: array of TMiscObjectInfo;
    adv_crate_misc_object_info: TMiscObjectInfo;
    cnt_misc_objects: integer;

    // Side related data
    side_names: array[0..CNT_SIDES-1] of string;
    side_names_short: array[0..CNT_SIDES-1] of string;

    // Limits related data
    limit_sandworm_required: boolean;
    limit_spice_blooms_crates: integer;
    limit_structures_total: integer;
    limit_structures_per_side: integer;
    limit_refineries_per_side: integer;

    // Group IDs related data
    group_ids: TStringList;
    group_ids_byte_types: array[0..85] of TGroupIDsByteType;

    // Auxiliary variables
    item_data_pointers: array[0..36] of TItemDataPointers;
    item_type_pointers: array[0..8] of TItemTypePointers;
    art_type_pointers: array[0..5] of TArtTypePointers;
  public
    // General procedures
    procedure init;

    // TEMPLATES.BIN related procedures
    procedure load_templates_bin(force: boolean);
    procedure save_templates_bin;
    procedure compute_image_indexes;
    procedure compute_building_and_unit_house_versions;
    procedure compute_building_group_mapping;
    function prettify_structure_name(str: String): String;
    function get_unit_name_str(index: integer): String;
    function get_unit_group_str(index: integer): String;
    function get_building_name_str(index: integer): String;
    function get_building_group_str(index: integer): String;
    function get_special_value_type(special: word): byte;
    function get_special_value_side(special: word): integer;
    function get_unit_template_for_special(special: word): TUnitTemplatePtr;
    function get_unit_template(index, side: integer; my_version: boolean): TUnitTemplatePtr;
    function check_unit_is_stealth_for_special(special: word): boolean;
    function get_building_template_for_special(special: word): TBuildingTemplatePtr;
    function get_building_template(index, side: integer; my_version: boolean): TBuildingTemplatePtr;
    function check_links_with_wall(special: word): boolean;
    procedure get_structure_size(special: word; var size_x, size_y: integer);
    // BUILEXP.BIN related procedures
    procedure load_builexp_bin(force: boolean);
    procedure save_builexp_bin;
    // ARMOUR.BIN related procedures
    procedure load_armour_bin(force: boolean);
    procedure save_armour_bin;
    // SPEED.BIN related procedures
    procedure load_speed_bin(force: boolean);
    procedure save_speed_bin;
    // TECHPOS.BIN related procedures
    procedure load_techpos_bin(force: boolean);
    procedure save_techpos_bin;
    // TILEDATA.BIN related procedures
    procedure load_tiledata_bin;
    function get_tiledata_entry(special: integer): TTileDataEntryPtr;
    // Misc. objects related procedures
    procedure load_misc_objects_ini;
    procedure register_misc_objects_in_tiledata;
    function get_misc_object_info_for_special(special: word): TMiscObjectInfoPtr;
    // Sides related procedures
    procedure load_sides_ini;
    // Limits related procedures
    procedure load_limits_ini;
    // Group IDs related procedures
    procedure load_group_ids;

  private
    // Auxiliary procedures
    procedure fix_reference(var value: integer;  v1, v2: integer; swap: boolean); overload;
    procedure fix_reference(var value: shortint; v1, v2: integer; swap: boolean); overload;
    procedure fix_reference(var value: byte;     v1, v2: integer; swap: boolean); overload;
    procedure fix_group_ids_reference(byte_type: TGroupIDsByteType; v1, v2: integer; swap: boolean);
    procedure swap_data(data: TByteArrayPtr; data_size, index1, index2: integer);

    // Item manipulation procedures
  private
    procedure init_item_data_pointers(item_data_type: integer; data_ptr: TByteArrayPtr; data_size: integer);
    procedure init_item_type_pointers(item_type: integer; item_count_byte_ptr: PByte; item_name_list_ptr: TByteArrayPtr; item_name_length, max_item_count, item_data_pointers_first, item_data_pointers_count, export_data_size: integer);
    procedure fix_item_references(item_type: integer; v1, v2: integer; swap: boolean);
  public
    function add_new_item(item_type: integer; name: string; var index: integer): boolean;
    function remove_last_item(item_type: integer): boolean;
    procedure rename_item(item_type, index: integer; name: string);
    procedure swap_items(item_type, index1, index2: integer);

    // Art manipulation procedures
  private
    procedure init_art_type_pointers(art_type: integer; art_count_byte_ptr: PByte; directions_list_ptr, frames_list_ptr: TByteArrayPtr; frames_size, max_art_count: integer; image_indexes_list_ptr: TWordArrayPtr; next_free_image_index_ptr: PWord);
    function get_art_num_images(art_type, index: integer): integer;
    procedure fix_art_references(art_type: integer; v1, v2: integer; swap: boolean);
  public
    function add_new_art(art_type, directions, frames: integer; var index: integer): boolean;
    function remove_last_art(art_type: integer): boolean;
    procedure modify_art(art_type, index, directions, frames: integer);
    procedure swap_arts(art_type, index1, index2: integer);

    // Data Export/Import, Copy/Paste related procedures
  private
    procedure store_item_reference   (var ref: TItemReference; item_type, index: integer);
    function  restore_item_reference (var ref: TItemReference; item_type, index: integer; import_path: string): integer;
    procedure store_art_reference    (var ref: TArtReference; art_type, index: integer);
    function  restore_art_reference  (var ref: TArtReference; art_type, ref_index, fixed_index: integer; import_filename: string): integer;
    procedure store_sound_reference  (var ref: TSoundReference; index: integer);
    function  restore_sound_reference(var ref: TSoundReference; index: integer; import_path: string; allow_aud_files: boolean): integer;

    procedure store_building_export_data     (index: integer; data: TBuildingExportDataPtr);
    procedure restore_building_export_data   (index: integer; data: TBuildingExportDataPtr; import_path: string);
    procedure store_unit_export_data         (index: integer; data: TUnitExportDataPtr);
    procedure restore_unit_export_data       (index: integer; data: TUnitExportDataPtr; import_path: string);
    procedure store_weapon_export_data       (index: integer; data: TWeaponExportDataPtr);
    procedure restore_weapon_export_data     (index: integer; data: TWeaponExportDataPtr; import_path: string);
    procedure store_explosion_export_data    (index: integer; data: TExplosionExportDataPtr);
    procedure restore_explosion_export_data  (index: integer; data: TExplosionExportDataPtr; import_path: string);
    procedure store_armour_type_export_data  (index: integer; data: TArmourTypeExportDataPtr);
    procedure restore_armour_type_export_data(index: integer; data: TArmourTypeExportDataPtr);
    procedure store_warhead_export_data      (index: integer; data: TWarheadExportDataPtr);
    procedure restore_warhead_export_data    (index: integer; data: TWarheadExportDataPtr);
    procedure store_unit_voices_export_data  (index: integer; data: TUnitVoicesExportDataPtr);
    procedure restore_unit_voices_export_data(index: integer; data: TUnitVoicesExportDataPtr; import_path: string);

    procedure store_item_export_data  (item_type, index: integer; data: TByteArrayPtr);
    procedure restore_item_export_data(item_type, index: integer; data: TByteArrayPtr; import_path: string);
  public
    procedure export_item(item_type, index: integer; filename: string);
    procedure import_item(item_type, index: integer; filename: string);
    procedure copy_item  (item_type, index: integer);
    procedure paste_item (item_type, index: integer);

    // Remap structures functions
    function load_remap_structures_ini_file(ini_filename: String; remap_structures_data: TRemapStructuresDataPtr): String;
    function remap_structure(remap_structures_data: TRemapStructuresDataPtr; list_type: ItemListType; struct_type: integer): integer;
  end;

var
  Structures: TStructures;

implementation

uses Forms, Clipbrd, IniFiles, _settings, _mission, _missionini, _graphics, _sounds, _stringtable, _dispatcher, _gamelists;

procedure TStructures.init;
var
  i: integer;
begin
  group_ids := TStringList.Create;
  // Load all files
  load_templates_bin(false);
  load_builexp_bin(false);
  load_armour_bin(false);
  load_speed_bin(false);
  load_techpos_bin(false);
  load_tiledata_bin;
  load_misc_objects_ini;
  load_sides_ini;
  load_limits_ini;
  load_group_ids;
  // Initialize item data pointers
  init_item_data_pointers(0, Addr(templates.BuildingDefinitions),  sizeof(TBuildingTemplate));
  init_item_data_pointers(1, Addr(builexp),                        sizeof(TBuilExpEntry));
  init_item_data_pointers(2, Addr(templates.UnitDefinitions),      sizeof(TUnitTemplate));
  init_item_data_pointers(3, Addr(templates.WeaponDefinitions),    sizeof(TWeaponTemplate));
  init_item_data_pointers(4, Addr(templates.ExplosionDefinitions), sizeof(TExplosionTemplate));
  init_item_data_pointers(5, Addr(templates.AnimationArtFlags),    4);
  init_item_data_pointers(6, Addr(armour.WarheadEntries),          sizeof(TWarheadEntry));
  for i := 0 to MAX_WARHEADS - 1 do
    init_item_data_pointers(7 + i, Addr(armour.WarheadEntries[i].VersusArmorType), 1);
  // Initialize item type pointers
  init_item_type_pointers(ITEM_BUILDING,       Addr(Structures.templates.BuildingCount),      Addr(Structures.templates.BuildingNameStrings),  450, MAX_BUILDING_TYPES, 0, 2, sizeof(TBuildingExportData));
  init_item_type_pointers(ITEM_UNIT,           Addr(Structures.templates.UnitCount),          Addr(Structures.templates.UnitNameStrings),      450, MAX_UNIT_TYPES,     2, 1, sizeof(TUnitExportData));
  init_item_type_pointers(ITEM_BUILDING_GROUP, Addr(Structures.templates.BuildingGroupCount), Addr(Structures.templates.BuildingGroupStrings), 50,  MAX_BUILDING_TYPES, 0, 0, 0);
  init_item_type_pointers(ITEM_UNIT_GROUP,     Addr(Structures.templates.UnitGroupCount),     Addr(Structures.templates.UnitGroupStrings),     50,  MAX_UNIT_TYPES,     0, 0, 0);
  init_item_type_pointers(ITEM_WEAPON,         Addr(Structures.templates.WeaponCount),        Addr(Structures.templates.WeaponStrings),        50,  MAX_WEAPONS,        3, 1, sizeof(TWeaponExportData));
  init_item_type_pointers(ITEM_EXPLOSION,      Addr(Structures.templates.ExplosionCount),     Addr(Structures.templates.ExplosionStrings),     50,  MAX_EXPLOSIONS,     4, 2, sizeof(TExplosionExportData));
  init_item_type_pointers(ITEM_ARMOUR_TYPE,    Addr(Structures.armour.ArmourTypeCount),       Addr(Structures.armour.ArmourTypeStrings),       50,  MAX_ARMOUR_TYPES,   7, MAX_WARHEADS, sizeof(TArmourTypeExportData));
  init_item_type_pointers(ITEM_WARHEAD,        Addr(Structures.armour.WarheadCount),          Addr(Structures.armour.WarheadStrings),          50,  MAX_WARHEADS,       6, 1, sizeof(TWarheadExportData));
  init_item_type_pointers(ITEM_UNIT_VOICES,    nil,                                           nil,                                             0,   0,                  0, 0, sizeof(TUnitVoicesExportData));
  // Initialize art type pointers
  init_art_type_pointers(ART_BUILDING,           Addr(templates.BuildingArtCount),   Addr(templates.BuildingArtDirections),   nil,                                     0, MAX_BUILDING_ART,   Addr(building_art_image_indexes),       Addr(projectile_art_image_indexes[0]));
  init_art_type_pointers(ART_BUILDING_ANIMATION, Addr(templates.BuildingCount),      nil,                                     Addr(templates.BuildingAnimationFrames), 1, MAX_BUILDING_TYPES, Addr(building_animation_image_indexes), nil);
  init_art_type_pointers(ART_BUILDUP,            Addr(templates.BuildingCount),      nil,                                     Addr(templates.BuildupArtFrames),        1, MAX_BUILDING_TYPES, Addr(buildup_art_image_indexes),        nil);
  init_art_type_pointers(ART_UNIT,               Addr(templates.UnitArtCount),       Addr(templates.UnitArtDirectionFrames),  Addr(templates.UnitArtAnimationFrames),  4, MAX_UNIT_ART,       Addr(unit_art_image_indexes),           Addr(building_art_image_indexes[0]));
  init_art_type_pointers(ART_PROJECTILE,         Addr(templates.ProjectileArtCount), Addr(templates.ProjectileArtDirections), nil,                                     0, MAX_WEAPONS,        Addr(projectile_art_image_indexes),     Addr(animation_art_image_indexes[0]));
  init_art_type_pointers(ART_ANIMATION,          Addr(templates.AnimationArtCount),  nil,                                     Addr(templates.AnimationArtFrames),      4, MAX_EXPLOSIONS,     Addr(animation_art_image_indexes),      Addr(first_unit_icon_image_index));
end;

procedure TStructures.load_templates_bin(force: boolean);
var
  tmp_filename: String;
begin
  tmp_filename := find_file('Data\bin\Templates.bin', 'game');
  if (tmp_filename = '') or ((tmp_filename = templates_bin_filename) and not force) then
    exit;
  templates_bin_filename := tmp_filename;

  load_binary_file(tmp_filename, templates, sizeof(templates));

  compute_image_indexes;
  compute_building_and_unit_house_versions;
  compute_building_group_mapping;

  // Register event in dispatcher
  Dispatcher.register_event(evFLTemplatesBin);
end;

procedure TStructures.save_templates_bin;
begin
  if templates_bin_filename = '' then
    exit;
  if not manage_filesave(templates_bin_filename, 'Data\bin\Templates.bin', evStructuresFilenameChange) then
    exit;
  save_binary_file(templates_bin_filename, templates, sizeof(templates));
end;

procedure TStructures.compute_image_indexes;
var
  data: ^TTemplatesBinFile;
  index: integer;
  i: integer;
begin
  data := Addr(templates);
  index := NUM_TECHNICAL_GRAPHICAL_ENTRIES;
  unit_art_image_indexes[0] := index;
  for i := 0 to data.UnitArtCount-1 do
  begin
    unit_art_image_indexes[i] := index;
    inc(index, data.UnitArtAnimationFrames[i] * data.UnitArtDirectionFrames[i]);
  end;
  building_art_image_indexes[0] := index;
  for i := 0 to data.BuildingArtCount-1 do
  begin
    building_art_image_indexes[i] := index;
    inc(index, data.BuildingArtDirections[i] * 2 + 1);
  end;
  projectile_art_image_indexes[0] := index;
  for i := 0 to data.ProjectileArtCount-1 do
  begin
    projectile_art_image_indexes[i] := index;
    inc(index, data.ProjectileArtDirections[i]);
  end;
  animation_art_image_indexes[0] := index;
  for i := 0 to data.AnimationArtCount-1 do
  begin
    animation_art_image_indexes[i] := index;
    inc(index, data.AnimationArtFrames[i]);
  end;
  first_unit_icon_image_index := index;
  inc(index, data.UnitCount);
  inc(index, NUM_EMPTY_UNIT_SIDEBAR_ICONS);
  first_building_icon_image_index := index;
  inc(index, data.BuildingCount);
  inc(index, 1);
  for i := 0 to data.BuildingCount-1 do
  begin
    buildup_art_image_indexes[i] := index;
    inc(index, data.BuildupArtFrames[i]);
  end;
  for i := 0 to data.BuildingCount-1 do
  begin
    building_animation_image_indexes[i] := index;
    inc(index, data.BuildingAnimationFrames[i]);
  end;
end;

procedure TStructures.compute_building_and_unit_house_versions;
var
  data: ^TTemplatesBinFile;
  i, j: integer;
  index: integer;
begin
  data := Addr(templates);
  // Initialize
  for i := 0 to MAX_UNIT_TYPES - 1 do
    for j := 0 to CNT_SIDES - 1 do
      unit_house_versions[i,j] := -1;
  for i := 0 to MAX_BUILDING_TYPES - 1 do
    for j := 0 to CNT_SIDES - 1 do
      building_house_versions[i,j] := -1;
  // Compute unit side versions
  for i := 0 to MAX_UNIT_TYPES - 1 do
    for j := 0 to CNT_SIDES - 1 do
      for index := 0 to data.UnitCount - 1 do
        if data.UnitDefinitions[index].UnitGroup = i then
        begin
          if unit_house_versions[i,j] = -1 then
            unit_house_versions[i,j] := index;
          if (data.UnitDefinitions[index].OwnerHouse and (1 shl j)) <> 0 then
            unit_house_versions[i,j] := index;
        end;
  // Compute building side versions
  for i := 0 to MAX_BUILDING_TYPES - 1 do
    for j := 0 to CNT_SIDES - 1 do
      for index := 0 to data.BuildingCount - 1 do
        if data.BuildingDefinitions[index].BuildingGroup = i then
        begin
          if building_house_versions[i,j] = -1 then
            building_house_versions[i,j] := index;
          if (data.BuildingDefinitions[index].OwnerHouse and (1 shl j)) <> 0 then
            building_house_versions[i,j] := index;
        end;
end;

procedure TStructures.compute_building_group_mapping;
var
  i, j: integer;
  building_template: TBuildingTemplatePtr;
begin
  j := 0;
  for i := 0 to templates.BuildingGroupCount - 1 do
  begin
    building_template := get_building_template(i, 0, true);
    // If building is concrete, do not add it to list
    if (building_template <> nil) and (building_template.SpecialBehavior = 15) then
      continue;
    building_group_mapping[j] := i;
    inc(j);
  end;
  building_group_mapping_count := j;
end;

function TStructures.prettify_structure_name(str: String): String;
var
  str_len, word_len: integer;
  i, j: integer;
begin
  str_len := Length(str);
  word_len := 0;
  for i := 1 to str_len + 1 do
  begin
    if (i = str_len + 1) or (str[i] = '_') or (str[i] = ' ') then
    begin
      // Reached end of word
      if (word_len > 3) or (i <> word_len + 1) then
        for j := (i - 1) downto (i - word_len + 1) do
          if (ord(str[j]) >= ord('A')) and (ord(str[j]) <= ord('Z')) then
            str[j] := chr(ord(str[j]) + 32);
      if (i <= str_len) then
        str[i] := ' ';
      word_len := 0;
      continue;
    end;
    Inc(word_len);
  end;
  result := str;
end;

function TStructures.get_unit_name_str(index: integer): String;
begin
  if index < templates.UnitCount then
    result := prettify_structure_name(templates.UnitNameStrings[index])
  else
    result := 'UNDEFINED#' + inttostr(index);
end;

function TStructures.get_unit_group_str(index: integer): String;
var
  row: integer;
begin
  if index < templates.UnitGroupCount then
  begin
    row := -1;
    if settings.TranslateStructureNames then
      row := StringTable.text_uib.IndexOfName(templates.UnitGroupStrings[index]);
    if row <> -1 then
      result := StringTable.text_uib.ValueFromIndex[row]
    else
      result := prettify_structure_name(templates.UnitGroupStrings[index])
  end else
    result := 'UNDEFINED#' + inttostr(index);
end;

function TStructures.get_building_name_str(index: integer): String;
begin
  if index < templates.BuildingCount then
    result := prettify_structure_name(templates.BuildingNameStrings[index])
  else
    result := 'UNDEFINED#' + inttostr(index);
end;

function TStructures.get_building_group_str(index: integer): String;
var
  row: integer;
begin
  if index < templates.BuildingGroupCount then
  begin
    row := -1;
    if settings.TranslateStructureNames then
      row := StringTable.text_uib.IndexOfName(templates.BuildingGroupStrings[index]);
    if row <> -1 then
      result := StringTable.text_uib.ValueFromIndex[row]
    else
      result := prettify_structure_name(templates.BuildingGroupStrings[index]);
  end else
    result := 'UNDEFINED#' + inttostr(index);
end;

function TStructures.get_special_value_type(special: word): byte;
begin
  if special = 65535 then
    result := ST_NOTHING
  else if (special and 32768) <> 0 then
    result := ST_MISC_OBJECT
  else if (special and 16384) <> 0 then
    result := ST_UNIT
  else if (special and 8192) <> 0 then
    result := ST_BUILDING
  else
    result := (get_tiledata_entry(special)).stype;
end;

function TStructures.get_special_value_side(special: word): integer;
begin
  if (special and 32768) <> 0 then
    result := -1
  else if (special and 16384) <> 0 then
    result := (special shr 6) and 7
  else if (special and 8192) <> 0 then
    result := (special shr 7) and 7
  else
    result := (get_tiledata_entry(special)).side;
end;

function TStructures.get_unit_template_for_special(special: word): TUnitTemplatePtr;
var
  tiledata_entry: TTileDataEntryPtr;
begin
  result := nil;
  if ((special and 32768) = 0) and ((special and 16384) <> 0) then
    result := get_unit_template(special and 63, -1, false)
  else
  begin
    tiledata_entry := get_tiledata_entry(special);
    if tiledata_entry.stype = ST_UNIT then
      result := get_unit_template(tiledata_entry.index, Mission.get_side_house_id(tiledata_entry.side), true);
  end;
end;

function TStructures.get_unit_template(index, side: integer; my_version: boolean): TUnitTemplatePtr;
var
  unit_template_index: integer;
begin
  result := nil;
  if my_version then
  begin
    unit_template_index := unit_house_versions[index, side];
    if unit_template_index = -1 then
      exit;
    result := Addr(templates.UnitDefinitions[unit_template_index]);
  end else
  begin
    if index >= Structures.templates.UnitCount then
      exit;
    result := Addr(templates.UnitDefinitions[index]);
  end;
end;

function TStructures.check_unit_is_stealth_for_special(special: word): boolean;
var
  tiledata_entry: TTileDataEntryPtr;
  unit_index: integer;
begin
  result := false;
  unit_index := -1;
  if ((special and 32768) = 0) and ((special and 16384) <> 0) then
  begin
    if (special and 4096) <> 0 then
    begin
      result := true;
      exit;
    end;
    unit_index := special and 63
  end else
  begin
    tiledata_entry := get_tiledata_entry(special);
    if tiledata_entry.stype = ST_UNIT then
      unit_index := unit_house_versions[tiledata_entry.index, Mission.get_side_house_id(tiledata_entry.side)];
  end;
  if (unit_index = -1) or (unit_index >= templates.UnitCount) then
    exit;
  result := ((templates.UnitDefinitions[unit_index].Flags and $10) <> 0) or (templates.UnitDefinitions[unit_index].SpecialBehavior = 12) or (templates.UnitNameStrings[unit_index] = 'STEALTH RAIDER');
end;

function TStructures.get_building_template_for_special(special: word): TBuildingTemplatePtr;
var
  tiledata_entry: TTileDataEntryPtr;
begin
  result := nil;
  if ((special and 49152) = 0) and ((special and 8192) <> 0) then
    result := get_building_template(special and 127, -1, false)
  else
  begin
    tiledata_entry := get_tiledata_entry(special);
    if tiledata_entry.stype = ST_BUILDING then
      result := get_building_template(tiledata_entry.index, Mission.get_side_house_id(tiledata_entry.side), true);
  end;
end;

function TStructures.get_building_template(index, side: integer; my_version: boolean): TBuildingTemplatePtr;
var
  building_template_index: integer;
begin
  result := nil;
  if my_version then
  begin
    building_template_index := building_house_versions[index, side];
    if building_template_index = -1 then
      exit;
    result := Addr(templates.BuildingDefinitions[building_template_index]);
  end else
  begin
    if index >= Structures.templates.BuildingCount then
      exit;
    result := Addr(templates.BuildingDefinitions[index]);
  end;
end;

function TStructures.check_links_with_wall(special: word): boolean;
var
  building_template: TBuildingTemplatePtr;
begin
  building_template := get_building_template_for_special(special);
  result := (building_template <> nil) and ((building_template.SpecialBehavior = 14) or (building_template.SpecialBehavior = 16));
end;

procedure TStructures.get_structure_size(special: word; var size_x, size_y: integer);
var
  structure_type: byte;
  building_template: TBuildingTemplatePtr;
  xx, yy: integer;
begin
  size_x := 0;
  size_y := 0;
  structure_type := get_special_value_type(special);
  if structure_type = ST_BUILDING then
  begin
    building_template := get_building_template_for_special(special);
    if building_template = nil then
      exit;
    // Get building size
    for yy := 0 to MAX_BUILDING_SIZE - 1 do
      for xx := 0 to MAX_BUILDING_SIZE - 1 do
        if (building_template.TilesOccupiedAll and (1 shl ((yy * MAX_BUILDING_SIZE) + xx))) <> 0 then
        begin
          size_x := Max(size_x, xx + 1);
          size_y := Max(size_y, yy + 1);
        end;
  end else
  if structure_type = ST_UNIT then
  begin
    size_x := 1;
    size_y := 1;
  end;
end;

procedure TStructures.load_builexp_bin(force: boolean);
var
  tmp_filename: String;
begin
  tmp_filename := find_file('Data\bin\BUILEXP.BIN', 'game');
  if (tmp_filename = '') or ((tmp_filename = builexp_bin_filename) and not force) then
    exit;
  builexp_bin_filename := tmp_filename;
  // Read BUILEXP.BIN file
  load_binary_file(tmp_filename, builexp, sizeof(builexp));
  // Register event in dispatcher
  Dispatcher.register_event(evFLBuilexpBin);
end;

procedure TStructures.save_builexp_bin;
begin
  if builexp_bin_filename = '' then
    exit;
  if not manage_filesave(builexp_bin_filename, 'Data\bin\BUILEXP.BIN', evStructuresFilenameChange) then
    exit;
  save_binary_file(builexp_bin_filename, builexp, sizeof(builexp));
end;

procedure TStructures.load_armour_bin(force: boolean);
var
  tmp_filename: String;
begin
  tmp_filename := find_file('Data\bin\ARMOUR.BIN', 'game');
  if (tmp_filename = '') or ((tmp_filename = armour_bin_filename) and not force) then
    exit;
  armour_bin_filename := tmp_filename;
  // Read ARMOUR.BIN file
  load_binary_file(tmp_filename, armour, sizeof(armour));
  // Register event in dispatcher
  Dispatcher.register_event(evFLArmourBin);
end;


procedure TStructures.save_armour_bin;
begin
  if armour_bin_filename = '' then
    exit;
  if not manage_filesave(armour_bin_filename, 'Data\bin\ARMOUR.BIN', evStructuresFilenameChange) then
    exit;
  save_binary_file(armour_bin_filename, armour, sizeof(armour));
end;

procedure TStructures.load_speed_bin(force: boolean);
var
  tmp_filename: String;
begin
  tmp_filename := find_file('Data\bin\SPEED.BIN', 'game');
  if (tmp_filename = '') or ((tmp_filename = speed_bin_filename) and not force) then
    exit;
  speed_bin_filename := tmp_filename;
  // Read SPEED.BIN file
  load_binary_file(tmp_filename, speed, sizeof(speed));
  speed_bin_modified := false;
  // Register event in dispatcher
  Dispatcher.register_event(evFLSpeedBin);
end;

procedure TStructures.save_speed_bin;
begin
  if (speed_bin_filename = '') or not speed_bin_modified then
    exit;
  if not manage_filesave(speed_bin_filename, 'Data\bin\SPEED.BIN', evStructuresFilenameChange) then
    exit;
  save_binary_file(speed_bin_filename, speed, sizeof(speed));
  speed_bin_modified := false;
end;

procedure TStructures.load_techpos_bin(force: boolean);
var
  tmp_filename: String;
begin
  tmp_filename := find_file('Data\bin\TECHPOS.BIN', 'game');
  if (tmp_filename = '') or ((tmp_filename = techpos_bin_filename) and not force) then
    exit;
  techpos_bin_filename := tmp_filename;
  // Read TECHPOS.BIN file
  load_binary_file(tmp_filename, techpos, sizeof(techpos));
  techpos_bin_modified := false;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTechposBin);
end;

procedure TStructures.save_techpos_bin;
begin
  if (techpos_bin_filename = '') or not techpos_bin_modified then
    exit;
  if not manage_filesave(techpos_bin_filename, 'Data\bin\TECHPOS.BIN', evStructuresFilenameChange) then
    exit;
  save_binary_file(techpos_bin_filename, techpos, sizeof(techpos));
  techpos_bin_modified := false;
end;

procedure TStructures.load_tiledata_bin;
var
  tmp_filename: String;
begin
  tmp_filename := find_file('Data\bin\TILEDATA.BIN', 'game');
  if (tmp_filename = '') or (tmp_filename = tiledata_bin_filename) then
    exit;
  tiledata_bin_filename := tmp_filename;
  // Read TILEDATA.BIN file
  load_binary_file(tmp_filename, tiledata, sizeof(tiledata));

  // Add misc objects into TILEDATA.BIN
  register_misc_objects_in_tiledata;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTiledataBin);
end;

function TStructures.get_tiledata_entry(special: integer): TTileDataEntryPtr;
begin
  if (special <= 0) or (special >= CNT_TILEDATA_ENTRIES) then
    result := Addr(EMPTY_TILEDATA_ENTRY)
  else
    result := Addr(tiledata[special]);
end;

procedure TStructures.load_misc_objects_ini;
var
  tmp_filename: String;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i: integer;
  sname : string;
begin
  tmp_filename := find_file('config\misc_objects.ini', 'configuration');
  if (tmp_filename = '') or (tmp_filename = misc_objects_ini_filename) then
    exit;
  misc_objects_ini_filename := tmp_filename;
  // Read list of miscellaneous objects
  ini := TMemIniFile.Create(tmp_filename);
  tmp_strings := TStringList.Create;
  ini.ReadSections(tmp_strings);
  cnt_misc_objects := tmp_strings.Count;
  SetLength(misc_object_info, cnt_misc_objects);
  for i := 0 to cnt_misc_objects-1 do
  begin
    sname := tmp_strings[i];
    with misc_object_info[i] do
    begin
      name := sname;
      value := ini.ReadInteger(sname, 'value', 0);
      obj_type := ini.ReadInteger(sname, 'type', 0);
      color := ini.ReadInteger(sname, 'color', $0);
      image := ini.ReadInteger(sname, 'image', 0);
      mark := ini.ReadString(sname, 'mark', '');
    end;
  end;
  register_misc_objects_in_tiledata;
  ini.Destroy;
  tmp_strings.Destroy;
  // Register event in dispatcher
  Dispatcher.register_event(evFLMiscObjectsIni);
end;

procedure TStructures.register_misc_objects_in_tiledata;
var
  i: integer;
begin
  for i := 0 to cnt_misc_objects-1 do
  begin
    with misc_object_info[i] do
    begin
      if obj_type = MOT_WORM_SPAWNER then
        continue;
      tiledata[value].index := i;
      tiledata[value].side := 0;
      tiledata[value].stype := ST_MISC_OBJECT;
    end;
  end;
end;

function TStructures.get_misc_object_info_for_special(special: word): TMiscObjectInfoPtr;
var
  tiledata_entry: TTileDataEntryPtr;
  crate_type: integer;
begin
  result := nil;
  if special = 65535 then
    exit;
  if (special and 32768) <> 0 then
    with adv_crate_misc_object_info do
    begin
      obj_type := 3;
      color := $00C000;
      crate_type := (special shr 11) and 15;
      image := IfThen(crate_type = 7, -3, (special shr 8) and 7 + 102);
      mark := crate_type_marks[crate_type];
      result := Addr(adv_crate_misc_object_info);
    end else
  begin
    tiledata_entry := get_tiledata_entry(special);
    if tiledata_entry.stype = ST_MISC_OBJECT then
      result := Addr(misc_object_info[tiledata_entry.index]);
  end;
end;

procedure TStructures.load_sides_ini;
var
  tmp_filename, tmp_filename2: String;
  ini: TMemIniFile;
  i: integer;
begin
  // Step 1 - editor's internal file
  tmp_filename := current_dir + 'config\players.ini';
  // Step 2 - file under CustomCampaignData folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\' + MissionIni.ModsFolder + '\config\players.ini';
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Step 3 - file under Players folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\Players\' + MissionIni.PlayersFile;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file config\players.ini', 'Error loading configuration file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // This file is already loaded - do not load it again
  if tmp_filename = sides_ini_filename then
    exit;
  sides_ini_filename := tmp_filename;
  // Read list of sides
  ini := TMemIniFile.Create(tmp_filename);
  for i := 0 to CNT_SIDES-1 do
  begin
    side_names[i] := ini.ReadString('Player'+inttostr(i), 'name', 'Unnamed');
    side_names_short[i] := ini.ReadString('Player'+inttostr(i), 'short', side_names[i]);
  end;
  ini.Destroy;
  // Register event in dispatcher
  Dispatcher.register_event(evFLSidesIni);
end;

procedure TStructures.load_limits_ini;
var
  tmp_filename: String;
  ini: TMemIniFile;
begin
  tmp_filename := find_file('config\limits.ini', 'configuration');
  if (tmp_filename = '') or (tmp_filename = limits_ini_filename) then
    exit;
  limits_ini_filename := tmp_filename;
  // Read limits from limits.ini
  ini := TMemIniFile.Create(tmp_filename);
  limit_sandworm_required := ini.ReadBool('Limits', 'sandworm_required', true);
  limit_spice_blooms_crates := ini.ReadInteger('Limits', 'spice_blooms_crates', 30);
  limit_structures_total := ini.ReadInteger('Limits', 'structures_total', 1000);
  limit_structures_per_side := ini.ReadInteger('Limits', 'structures_per_side', 1000);
  limit_refineries_per_side := ini.ReadInteger('Limits', 'refineries_per_side', 10);
  ini.Destroy;
end;

procedure TStructures.load_group_ids;
var
  i: integer;
  prefix: string;
begin
  group_ids.Assign(GameLists.get_list_ref('GroupIDs'));
  for i := 0 to Length(group_ids_byte_types) - 1 do
  begin
    prefix := Copy(group_ids[i], 1, 2);
    if prefix = 'B ' then
      group_ids_byte_types[i] := gibtBuildingGroup
    else if prefix = 'U ' then
      group_ids_byte_types[i] := gibtUnit
    else if prefix = 'W ' then
      group_ids_byte_types[i] := gibtWeapon
    else if prefix = 'E ' then
      group_ids_byte_types[i] := gibtExplosion
    else
      group_ids_byte_types[i] := gibtNone
  end;
end;

procedure TStructures.fix_reference(var value: integer; v1, v2: integer; swap: boolean);
begin
  if value = v1 then
    value := v2
  else if swap and (value = v2) then
    value := v1;
end;

procedure TStructures.fix_reference(var value: shortint; v1, v2: integer; swap: boolean);
begin
  if value = v1 then
    value := v2
  else if swap and (value = v2) then
    value := v1;
end;

procedure TStructures.fix_reference(var value: byte; v1, v2: integer; swap: boolean);
begin
  if value = v1 then
    value := Max(v2, 0)
  else if swap and (value = v2) then
    value := v1;
end;

procedure TStructures.fix_group_ids_reference(byte_type: TGroupIDsByteType; v1, v2: integer; swap: boolean);
var
  i: integer;
begin
  for i := 0 to Length(templates.GroupIDs) - 1 do
    if group_ids_byte_types[i] = byte_type then
      fix_reference(templates.GroupIDs[i], v1, v2, swap);
end;

procedure TStructures.swap_data(data: TByteArrayPtr; data_size, index1, index2: integer);
var
  tmp_data: array[0..1023] of byte;
begin
  move(data[index1 * data_size], tmp_data[0], data_size);
  move(data[index2 * data_size], data[index1 * data_size], data_size);
  move(tmp_data[0], data[index2 * data_size], data_size);
end;

procedure TStructures.init_item_data_pointers(item_data_type: integer; data_ptr: TByteArrayPtr; data_size: integer);
begin
  item_data_pointers[item_data_type].data_ptr := data_ptr;
  item_data_pointers[item_data_type].data_size := data_size;
end;

procedure TStructures.init_item_type_pointers(item_type: integer; item_count_byte_ptr: PByte; item_name_list_ptr: TByteArrayPtr; item_name_length, max_item_count, item_data_pointers_first, item_data_pointers_count, export_data_size: integer);
begin
  item_type_pointers[item_type].item_count_byte_ptr := item_count_byte_ptr;
  item_type_pointers[item_type].item_name_list_ptr := item_name_list_ptr;
  item_type_pointers[item_type].item_name_length := item_name_length;
  item_type_pointers[item_type].max_item_count := max_item_count;
  item_type_pointers[item_type].item_data_pointers_first := item_data_pointers_first;
  item_type_pointers[item_type].item_data_pointers_count := item_data_pointers_count;
  item_type_pointers[item_type].export_data_size := export_data_size;
  if export_data_size <> 0 then
    item_type_pointers[item_type].clipboard_format := RegisterClipboardFormat(PChar(Format('D2kEditor%sExportData', [item_type_names[item_type]])));
end;

procedure TStructures.fix_item_references(item_type, v1, v2: integer; swap: boolean);
var
  i, j: integer;
begin
  case item_type of
    ITEM_UNIT:
      fix_group_ids_reference(gibtUnit, v1, v2, swap);
    ITEM_BUILDING_GROUP:
      begin
        for i := 0 to templates.BuildingCount - 1 do
        begin
          fix_reference(templates.BuildingDefinitions[i].BuildingGroup, v1, v2, swap);
          fix_reference(templates.BuildingDefinitions[i].Prereq1BuildingGroup, v1, v2, swap);
          fix_reference(templates.BuildingDefinitions[i].Prereq2BuildingGroup, v1, v2, swap);
        end;
        for i := 0 to templates.UnitCount - 1 do
        begin
          fix_reference(templates.UnitDefinitions[i].Prereq1BuildingGroup, v1, v2, swap);
          fix_reference(templates.UnitDefinitions[i].Prereq2BuildingGroup, v1, v2, swap);
        end;
        fix_group_ids_reference(gibtBuildingGroup, v1, v2, swap);
      end;
    ITEM_UNIT_GROUP:
      for i := 0 to templates.UnitCount - 1 do
        fix_reference(templates.UnitDefinitions[i].UnitGroup, v1, v2, swap);
    ITEM_WEAPON:
      begin
        for i := 0 to templates.BuildingCount - 1 do
        begin
          fix_reference(templates.BuildingDefinitions[i].PrimaryWeapon, v1, v2, swap);
          fix_reference(templates.BuildingDefinitions[i].SecondaryWeapon, v1, v2, swap);
        end;
        for i := 0 to templates.UnitCount - 1 do
        begin
          fix_reference(templates.UnitDefinitions[i].PrimaryWeapon, v1, v2, swap);
          fix_reference(templates.UnitDefinitions[i].SecondaryWeapon, v1, v2, swap);
        end;
        fix_group_ids_reference(gibtWeapon, v1, v2, swap);
      end;
    ITEM_EXPLOSION:
      begin
        for i := 0 to templates.BuildingCount - 1 do
        begin
          fix_reference(templates.BuildingDefinitions[i].DeathExplosion, v1, v2, swap);
          fix_reference(templates.BuildingDefinitions[i].MuzzleFlashExplosion, v1, v2, swap);
          for j := 0 to builexp[i].NumAnimations - 1 do
            fix_reference(builexp[i].AnimExplosion[j], v1, v2, swap);
        end;
        for i := 0 to templates.UnitCount - 1 do
        begin
          fix_reference(templates.UnitDefinitions[i].DeathExplosion, v1, v2, swap);
          fix_reference(templates.UnitDefinitions[i].MuzzleFlashExplosion, v1, v2, swap);
        end;
        for i := 0 to templates.WeaponCount - 1 do
        begin
          fix_reference(templates.WeaponDefinitions[i].HitExplosion, v1, v2, swap);
          fix_reference(templates.WeaponDefinitions[i].TrailExplosion, v1, v2, swap);
        end;
        for i := 0 to templates.ExplosionCount - 1 do
          fix_reference(templates.ExplosionDefinitions[i].MyIndex, v1, v2, swap);
        fix_group_ids_reference(gibtExplosion, v1, v2, swap)
      end;
    ITEM_WARHEAD:
      for i := 0 to templates.WeaponCount - 1 do
        fix_reference(templates.WeaponDefinitions[i].Warhead, v1, v2, swap);
    ITEM_ARMOUR_TYPE:
      begin
        for i := 0 to templates.BuildingCount - 1 do
          fix_reference(templates.BuildingDefinitions[i].ArmorType, v1, v2, swap);
        for i := 0 to templates.UnitCount - 1 do
          fix_reference(templates.UnitDefinitions[i].ArmorType, v1, v2, swap);
      end;
  end;
end;

function TStructures.add_new_item(item_type: integer; name: string; var index: integer): boolean;
var
  ptrs: TItemTypePointersPtr;
  dummy: integer;
  need_compute_image_indexes: boolean;
begin
  result := false;
  ptrs := Addr(item_type_pointers[item_type]);
  if ptrs.item_count_byte_ptr^ = ptrs.max_item_count then
    exit;
  need_compute_image_indexes := false;
  // Store item name
  store_c_string(name, Addr(ptrs.item_name_list_ptr[ptrs.item_count_byte_ptr^ * ptrs.item_name_length]), ptrs.item_name_length);
  // Return index of newly created item
  index := ptrs.item_count_byte_ptr^;
  // Perform item-type-specific action
  case item_type of
    ITEM_BUILDING:
      begin
        // Set default building data
        with templates.BuildingDefinitions[templates.BuildingCount] do
        begin
          PrimaryWeapon := -1;
          SecondaryWeapon := -1;
          BarrelArt := -1;
          Prereq2BuildingGroup := -1;
          DeathExplosion := -1;
          BuildupArt := templates.BuildingCount;
          BuildingAnimation := templates.BuildingCount;
          MuzzleFlashExplosion := -1;
        end;
        // Add building icon
        StructGraphics.add_empty_image_entries(first_building_icon_image_index + templates.BuildingCount, 1);
        need_compute_image_indexes := true;
      end;
    ITEM_UNIT:
      begin
        // Set default unit data
        with templates.UnitDefinitions[templates.UnitCount] do
        begin
          PrimaryWeapon := -1;
          SecondaryWeapon := -1;
          BarrelArt := -1;
          Prereq2BuildingGroup := -1;
          DeathExplosion := -1;
          MuzzleFlashExplosion := -1;
        end;
        // Add unit icon
        StructGraphics.add_empty_image_entries(first_unit_icon_image_index + templates.UnitCount, 1);
        need_compute_image_indexes := true;
      end;
    ITEM_WEAPON:
      begin
        // Set default weapon data
        with templates.WeaponDefinitions[templates.WeaponCount] do
        begin
          FiringSound := -1;
          ProjectileArt := templates.WeaponCount;
          HitExplosion := -1;
          TrailExplosion := -1;
        end;
        // Add new projectile art
        add_new_art(ART_PROJECTILE, 1, 0, dummy);
      end;
    ITEM_EXPLOSION:
      begin
        // Set default explosion data
        templates.ExplosionDefinitions[templates.ExplosionCount].Sound := -1;
        templates.ExplosionDefinitions[templates.ExplosionCount].MyIndex := templates.ExplosionCount;
        // Add new animation art
        add_new_art(ART_ANIMATION, 0, 1, dummy);
      end;
  end;
  // Increase item count by 1
  Inc(ptrs.item_count_byte_ptr^);
  // Compute image indexes if needed
  if need_compute_image_indexes then
    compute_image_indexes;
  result := true;
end;

function TStructures.remove_last_item(item_type: integer): boolean;
var
  ptrs: TItemTypePointersPtr;
  data_ptrs: TItemDataPointersPtr;
  i: integer;
begin
  result := false;
  ptrs := Addr(item_type_pointers[item_type]);
  if ptrs.item_count_byte_ptr^ = 0 then
    exit;
  // Decrease item count by 1
  Dec(ptrs.item_count_byte_ptr^);
  // Erase item name
  FillChar(ptrs.item_name_list_ptr[ptrs.item_count_byte_ptr^ * ptrs.item_name_length], ptrs.item_name_length, 0);
  // Erase item data
  for i := 0 to ptrs.item_data_pointers_count - 1 do
  begin
    data_ptrs := Addr(item_data_pointers[ptrs.item_data_pointers_first + i]);
    FillChar(data_ptrs.data_ptr[ptrs.item_count_byte_ptr^ * data_ptrs.data_size], data_ptrs.data_size, 0);
  end;
  // Perform item-type-specific action
  case item_type of
    ITEM_BUILDING:
      begin
        // Remove animation art, buildup art and building icon
        remove_last_art(ART_BUILDING_ANIMATION);
        remove_last_art(ART_BUILDUP);
        StructGraphics.remove_image_entries(first_building_icon_image_index + templates.BuildingCount, 1);
        compute_image_indexes;
      end;
    ITEM_UNIT:
      begin
        // Remove building icon
        StructGraphics.remove_image_entries(first_unit_icon_image_index + templates.UnitCount, 1);
        compute_image_indexes;
      end;
    ITEM_WEAPON:
      // Remove projectile art
      remove_last_art(ART_PROJECTILE);
    ITEM_EXPLOSION:
      // Remove animation art
      remove_last_art(ART_ANIMATION);
  end;
  // Fix item references
  fix_item_references(item_type, ptrs.item_count_byte_ptr^, -1, false);
  result := true;
end;

procedure TStructures.rename_item(item_type, index: integer; name: string);
var
  ptrs: TItemTypePointersPtr;
begin
  if index < 0 then
    exit;
  ptrs := Addr(item_type_pointers[item_type]);
  store_c_string(name, Addr(ptrs.item_name_list_ptr[index * ptrs.item_name_length]), ptrs.item_name_length);
end;

procedure TStructures.swap_items(item_type, index1, index2: integer);
var
  ptrs: TItemTypePointersPtr;
  data_ptrs: TItemDataPointersPtr;
  i: integer;
begin
  ptrs := Addr(item_type_pointers[item_type]);
  // Swap item name
  swap_data(ptrs.item_name_list_ptr, ptrs.item_name_length, index1, index2);
  // Swap item data
  for i := 0 to ptrs.item_data_pointers_count - 1 do
  begin
    data_ptrs := Addr(item_data_pointers[ptrs.item_data_pointers_first + i]);
    swap_data(data_ptrs.data_ptr, data_ptrs.data_size, index1, index2);
  end;
  // Perform item-type-specific action
  case item_type of
    ITEM_BUILDING:
      begin
        // Swap animation art, buildup art and building icon
        swap_arts(ART_BUILDING_ANIMATION, index1, index2);
        swap_arts(ART_BUILDUP, index1, index2);
        StructGraphics.swap_image_entries(first_building_icon_image_index + index1, first_building_icon_image_index + index2, 1, 1);
       end;
    ITEM_UNIT:
      // Swap unit icon
      StructGraphics.swap_image_entries(first_unit_icon_image_index + index1, first_unit_icon_image_index + index2, 1, 1);
    ITEM_WEAPON:
      // Swap projectile art
      swap_arts(ART_PROJECTILE, index1, index2);
    ITEM_EXPLOSION:
      // Swap animation art
      swap_arts(ART_ANIMATION, index1, index2);
  end;
  // Fix item references
  fix_item_references(item_type, index1, index2, true);
end;

procedure TStructures.init_art_type_pointers(art_type: integer; art_count_byte_ptr: PByte; directions_list_ptr, frames_list_ptr: TByteArrayPtr; frames_size, max_art_count: integer; image_indexes_list_ptr: TWordArrayPtr; next_free_image_index_ptr: PWord);
begin
  art_type_pointers[art_type].art_count_byte_ptr := art_count_byte_ptr;
  art_type_pointers[art_type].directions_list_ptr := directions_list_ptr;
  art_type_pointers[art_type].frames_list_ptr := frames_list_ptr;
  art_type_pointers[art_type].frames_size := frames_size;
  art_type_pointers[art_type].max_art_count := max_art_count;
  art_type_pointers[art_type].image_indexes_list_ptr := image_indexes_list_ptr;
  art_type_pointers[art_type].next_free_image_index_ptr := next_free_image_index_ptr;
end;

function TStructures.get_art_num_images(art_type, index: integer): integer;
var
  ptrs: TArtTypePointersPtr;
  directions, frames: integer;
begin
  ptrs := Addr(art_type_pointers[art_type]);
  directions := 1;
  if ptrs.directions_list_ptr <> nil then
    directions := ptrs.directions_list_ptr[index * 4];
  frames := 1;
  if ptrs.frames_list_ptr <> nil then
    frames := ptrs.frames_list_ptr[index * ptrs.frames_size];
  result := IfThen(art_type = ART_BUILDING, directions * 2 + 1, directions * frames);
end;

procedure TStructures.fix_art_references(art_type, v1, v2: integer; swap: boolean);
var
  i: integer;
begin
  case art_type of
    ART_BUILDING:
      // Reset building art references in buildings
      for i := 0 to templates.BuildingCount - 1 do
      begin
        fix_reference(templates.BuildingDefinitions[i].BuildingArt, v1, v2, swap);
        fix_reference(templates.BuildingDefinitions[i].BarrelArt, v1, v2, swap);
      end;
    ART_BUILDING_ANIMATION:
      // Reset building animation art references in buildings
      for i := 0 to templates.BuildingCount - 1 do
        if (templates.BuildingDefinitions[i].Flags and BF_HAS_ANIMATION) <> 0 then
          fix_reference(templates.BuildingDefinitions[i].BuildingAnimation, v1, IfThen(v2 = -1, i, v2), swap);
    ART_BUILDUP:
      // Reset buildup art references in buildings
      for i := 0 to templates.BuildingCount - 1 do
        if templates.BuildupArtFrames[i] <> 0 then
          fix_reference(templates.BuildingDefinitions[i].BuildupArt, v1, IfThen(v2 = -1, i, v2), swap);
    ART_UNIT:
      // Reset unit art references in units
      for i := 0 to templates.UnitCount - 1 do
      begin
        fix_reference(templates.UnitDefinitions[i].UnitArt, v1, v2, swap);
        fix_reference(templates.UnitDefinitions[i].BarrelArt, v1, v2, swap);
      end;
    ART_PROJECTILE:
      // Reset projectile art references in weapons
      for i := 0 to templates.WeaponCount - 1 do
        fix_reference(templates.WeaponDefinitions[i].ProjectileArt, v1, IfThen(v2 = -1, i, v2), swap);
  end;
end;

function TStructures.add_new_art(art_type, directions, frames: integer; var index: integer): boolean;
var
  ptrs: TArtTypePointersPtr;
  num_images: integer;
begin
  result := false;
  ptrs := Addr(art_type_pointers[art_type]);
  if ptrs.art_count_byte_ptr^ = ptrs.max_art_count then
    exit;
  // Add new empty image entries
  directions := IfThen(ptrs.directions_list_ptr <> nil, directions, 1);
  frames := IfThen(ptrs.frames_list_ptr <> nil, frames, 1);
  num_images := IfThen(art_type = ART_BUILDING, directions * 2 + 1, directions * frames);
  StructGraphics.add_empty_image_entries(ptrs.next_free_image_index_ptr^, num_images);
  // Store number of directions and frames
  if ptrs.directions_list_ptr <> nil then
    ptrs.directions_list_ptr[ptrs.art_count_byte_ptr^ * 4] := directions;
  if ptrs.frames_list_ptr <> nil then
    ptrs.frames_list_ptr[ptrs.art_count_byte_ptr^ * ptrs.frames_size] := frames;
  // Return index of newly added art
  index := ptrs.art_count_byte_ptr^;
  // Increase number of arts
  Inc(ptrs.art_count_byte_ptr^);
  compute_image_indexes;
  result := true;
end;

function TStructures.remove_last_art(art_type: integer): boolean;
var
  ptrs: TArtTypePointersPtr;
  use_art_count: boolean;
begin
  result := false;
  ptrs := Addr(art_type_pointers[art_type]);
  use_art_count := (art_type <> ART_BUILDING_ANIMATION) and (art_type <> ART_BUILDUP);
  if (use_art_count) and (ptrs.art_count_byte_ptr^ = 0) then
    exit;
  // Decrease number of arts
  if use_art_count then
    Dec(ptrs.art_count_byte_ptr^);
  // Remove image entries
  StructGraphics.remove_image_entries(ptrs.image_indexes_list_ptr[ptrs.art_count_byte_ptr^], get_art_num_images(art_type, ptrs.art_count_byte_ptr^));
  // Reset number of directions and frames
  if ptrs.directions_list_ptr <> nil then
    ptrs.directions_list_ptr[ptrs.art_count_byte_ptr^ * 4] := 0;
  if ptrs.frames_list_ptr <> nil then
    ptrs.frames_list_ptr[ptrs.art_count_byte_ptr^ * ptrs.frames_size] := 0;
  // Fix art references
  fix_art_references(art_type, ptrs.art_count_byte_ptr^, -1, false);
  compute_image_indexes;
  result := true;
end;

procedure TStructures.modify_art(art_type, index, directions, frames: integer);
var
  ptrs: TArtTypePointersPtr;
begin
  if index < 0 then
    exit;
  ptrs := Addr(art_type_pointers[art_type]);
  // Get new number of frames
  directions := IfThen(ptrs.directions_list_ptr <> nil, directions, 1);
  frames := IfThen(ptrs.frames_list_ptr <> nil, frames, 1);
  // Modify number of image entries
  if art_type = ART_BUILDING then
  begin
    StructGraphics.change_image_entry_count(building_art_image_indexes[index] + 1 + templates.BuildingArtDirections[index], templates.BuildingArtDirections[index], directions);
    StructGraphics.change_image_entry_count(building_art_image_indexes[index] + 1, templates.BuildingArtDirections[index], directions);
  end else
    StructGraphics.change_image_entry_count(ptrs.image_indexes_list_ptr[index], get_art_num_images(art_type, index), directions * frames);
  // Store new number of directions and frames
  if ptrs.directions_list_ptr <> nil then
    ptrs.directions_list_ptr[index * 4] := directions;
  if ptrs.frames_list_ptr <> nil then
    ptrs.frames_list_ptr[index * ptrs.frames_size] := frames;
  compute_image_indexes;
end;

procedure TStructures.swap_arts(art_type, index1, index2: integer);
var
  ptrs: TArtTypePointersPtr;
begin
  ptrs := Addr(art_type_pointers[art_type]);
  // Swap image entries
  StructGraphics.swap_image_entries(ptrs.image_indexes_list_ptr[index1], ptrs.image_indexes_list_ptr[index2], get_art_num_images(art_type, index1), get_art_num_images(art_type, index2));
  // Swap directions and frames count
  if ptrs.directions_list_ptr <> nil then
    swap_data(ptrs.directions_list_ptr, 4, index1, index2);
  if ptrs.frames_list_ptr <> nil then
    swap_data(ptrs.frames_list_ptr, ptrs.frames_size, index1, index2);
  // Fix art references
  fix_art_references(art_type, index1, index2, true);
  compute_image_indexes;
end;

procedure TStructures.store_item_reference(var ref: TItemReference; item_type, index: integer);
begin
  if index = -1 then
    FillChar(ref.item_name, 50, 0)
  else
    Move(item_type_pointers[item_type].item_name_list_ptr[index * 50], ref.item_name, 50);
end;

function TStructures.restore_item_reference(var ref: TItemReference; item_type, index: integer; import_path: string): integer;
var
  ptrs: TItemTypePointersPtr;
  srec: TDummyStringRecPtr;
  i: integer;
  new_item_added: boolean;
  import_filename: string;
begin
  result := index;
  if index = -1 then
    exit; // No item is referenced
  ptrs := Addr(item_type_pointers[item_type]);
  // Check if name of referenced item matches with name of existing item on referenced index
  srec := Addr(ptrs.item_name_list_ptr[index * ptrs.item_name_length]);
  if (index >= ptrs.item_count_byte_ptr^) or (ref.item_name <> srec.str) then
  begin
    // Name does not match. Try to find referenced item among list of existing items.
    result := -1;
    for i := 0 to ptrs.item_count_byte_ptr^ - 1 do
    begin
      srec := Addr(ptrs.item_name_list_ptr[i * ptrs.item_name_length]);
      if ref.item_name = srec.str then
      begin
        // Referenced item found
        result := i;
        break;
      end;
    end;
  end;
  // If referenced item does not exist among existing items, add new item with this name
  new_item_added := false;
  if result = -1 then
  begin
    if not add_new_item(item_type, ref.item_name, result) then
    begin
      Application.MessageBox(PChar(Format('Could not add new %s %s because there is no more room for it.', [item_type_names[item_type], ref.item_name])), 'Import item', MB_ICONERROR or MB_OK);
      result := 0;
      exit;
    end;
    new_item_added := true;
  end;
  // Try to import referenced item from file, if it exists
  if ptrs.export_data_size = 0 then
  begin
    // Item of this type cannot be imported
    if new_item_added then
      Application.MessageBox(PChar(Format('New %s %s was added at position %d.', [item_type_names[item_type], ref.item_name, result])), 'Import item', MB_ICONINFORMATION or MB_OK);
    exit;
  end;
  import_filename := import_path + ref.item_name + '.' + item_type_file_extensions[item_type];
  if FileExists(import_filename) then
  begin
    if new_item_added then
      Application.MessageBox(PChar(Format('New %s %s was added at position %d and it is being imported from file %s.', [item_type_names[item_type], ref.item_name, result, import_filename])), 'Import item', MB_ICONINFORMATION or MB_OK)
    else
      Application.MessageBox(PChar(Format('Replacing %s %s by import from file %s.', [item_type_names[item_type], ref.item_name, import_filename])), 'Import item', MB_ICONINFORMATION or MB_OK);
    import_item(item_type, result, import_filename);
  end
  else if new_item_added then
    Application.MessageBox(PChar(Format('New %s %s was added at position %d but is empty. You should set it up or import it.', [item_type_names[item_type], ref.item_name, result])), 'Import item', MB_ICONWARNING or MB_OK);
end;

procedure TStructures.store_art_reference(var ref: TArtReference; art_type, index: integer);
var
  ptrs: TArtTypePointersPtr;
  i: integer;
  art_bytes: TByteArrayPtr;
begin
  FillChar(ref, sizeof(TArtReference), 0);
  ptrs := Addr(art_type_pointers[art_type]);
  if (index = -1) or (index >= ptrs.art_count_byte_ptr^) then
    exit;
  if ptrs.directions_list_ptr <> nil then
    ref.directions := ptrs.directions_list_ptr[index * 4];
  if ptrs.frames_list_ptr <> nil then
    ref.frames := ptrs.frames_list_ptr[index * ptrs.frames_size];
  ref.art_size := StructGraphics.get_image_entries_size(ptrs.image_indexes_list_ptr[index], get_art_num_images(art_type, index));
  art_bytes := TByteArrayPtr(StructGraphics.get_structure_image_header(ptrs.image_indexes_list_ptr[index]));
  for i := 0 to ref.art_size - 1 do
    inc(ref.checksum, art_bytes[i]);
end;

function TStructures.restore_art_reference(var ref: TArtReference; art_type, ref_index, fixed_index: integer; import_filename: string): integer;
var
  ptrs: TArtTypePointersPtr;
  cmp_art_ref: TArtReference;
  i: integer;
  referenced_art_found: boolean;
  import_file_exists: boolean;
begin
  result := ref_index;
  if ref_index = -1 then
    exit; // No art is referenced
  ptrs := Addr(art_type_pointers[art_type]);
  // Check if referenced art matches with existing art on referenced index
  store_art_reference(cmp_art_ref, art_type, ref_index);
  if (ref_index >= ptrs.art_count_byte_ptr^) or (ref.directions <> cmp_art_ref.directions) or (ref.frames <> cmp_art_ref.frames) or (ref.art_size <> cmp_art_ref.art_size) or (ref.checksum <> cmp_art_ref.checksum) then
  begin
    // Art does not match. Try to find referenced art among list of existing arts.
    result := -1;
    for i := 0 to ptrs.art_count_byte_ptr^ - 1 do
    begin
      store_art_reference(cmp_art_ref, art_type, i);
      if (ref.directions = cmp_art_ref.directions) and (ref.frames = cmp_art_ref.frames) and (ref.art_size = cmp_art_ref.art_size) and (ref.checksum = cmp_art_ref.checksum) then
      begin
        // Referenced art found
        result := i;
        break;
      end;
    end;
  end;
  // If referenced art does not exist among existing arts, use new art with same amount of directions and frames
  referenced_art_found := result <> -1;
  if result = -1 then
  begin
    if fixed_index = -1 then
    begin
      // Add new art if there's no fixed index (building and unit art)
      if not add_new_art(art_type, ref.directions, ref.frames, result) then
      begin
        Application.MessageBox(PChar(Format('Could not add new %s art because there is no more room for it.', [art_type_names[art_type]])), 'Import art', MB_ICONERROR or MB_OK);
        result := 0;
        exit;
      end;
    end else
    begin
      // Otherwise use art on fixed index
      modify_art(art_type, fixed_index, ref.directions, ref.frames);
      result := fixed_index;
    end;
  end;
  // If referenced art was found among existing arts, it's not needed to import it
  import_filename := import_filename + '.R16';
  import_file_exists := FileExists(import_filename);
  if referenced_art_found then
  begin
    if import_file_exists then
      Application.MessageBox(PChar(Format('Skipping art import from file %s because such art already exists.', [import_filename])), 'Import art', MB_ICONINFORMATION or MB_OK);
    exit;
  end;
  // Import art from file
  if import_file_exists then
  begin
    if fixed_index = -1 then
      Application.MessageBox(PChar(Format('New %s art was added at position %d and it is being imported from file %s.', [art_type_names[art_type], result, import_filename])), 'Import art', MB_ICONINFORMATION or MB_OK)
    else
      Application.MessageBox(PChar(Format('Replacing %s art by import from file %s.', [art_type_names[art_type], import_filename])), 'Import art', MB_ICONINFORMATION or MB_OK);
    StructGraphics.import_image_entries(import_filename, ptrs.image_indexes_list_ptr[result], get_art_num_images(art_type, result));
  end else
    Application.MessageBox(PChar(Format('New %s art was added at position %d but is empty. You should set it up or import it.', [art_type_names[art_type], result])), 'Import art', MB_ICONWARNING or MB_OK);
end;

procedure TStructures.store_sound_reference(var ref: TSoundReference; index: integer);
begin
  FillChar(ref, sizeof(TSoundReference), 0);
  if index = -1 then
    exit;
  store_c_string(StringTable.samples_uib.Names[index], Addr(ref.key), Length(ref.key));
  store_c_string(StringTable.samples_uib.ValueFromIndex[index], Addr(ref.value), Length(ref.value));
end;

function TStructures.restore_sound_reference(var ref: TSoundReference; index: integer; import_path: string; allow_aud_files: boolean): integer;
var
  sound_name: string;
  game_aud_filename: string;
  import_wav_filename: string;
  import_wav_file_exists: boolean;
  sound_rs_index: integer;
begin
  result := index;
  if index = -1 then
    exit;
  // Deal with samples.uib entries
  result := StringTable.find_samples_uib_value(ref.value, index);
  if result = -1 then
  begin
    result := StringTable.add_samples_uib_entry(ref.key, ref.value);
    Application.MessageBox(PChar(Format('New entry %s=%s was added into samples.uib file.', [ref.key, ref.value])), 'Import sound', MB_ICONINFORMATION or MB_OK);
  end;
  // Deal with sounds and SOUND.RS entries
  sound_name := UpperCase(ref.value);
  game_aud_filename := Settings.GamePath + '\Data\GAMESFX\' + sound_name + '.AUD';
  import_wav_filename := import_path + sound_name + '.WAV';
  import_wav_file_exists := FileExists(import_wav_filename);
  if (not import_wav_file_exists) and allow_aud_files and FileExists(game_aud_filename) then
    exit;
  sound_rs_index := Sounds.find_sound(sound_name);
  if import_wav_file_exists then
  begin
    if sound_rs_index = -1 then
    begin
      Application.MessageBox(PChar(Format('New sound %s was added into SOUND.RS and it is being imported from file %s.', [sound_name, import_wav_filename])), 'Import sound', MB_ICONINFORMATION or MB_OK);
      Sounds.add_new_sound(import_wav_filename);
    end else
    begin
      Application.MessageBox(PChar(Format('Replacing sound %s by import from file %s.', [sound_name, import_wav_filename])), 'Import sound', MB_ICONINFORMATION or MB_OK);
      Sounds.replace_sound(sound_rs_index, import_wav_filename);
    end;
  end
  else if sound_rs_index = -1 then
    Application.MessageBox(PChar(Format('Referenced sound %s does not exist in SOUND.RS file. You should import it.', [sound_name])), 'Import sound', MB_ICONWARNING or MB_OK);
end;

procedure TStructures.store_building_export_data(index: integer; data: TBuildingExportDataPtr);
var
  icon_ptr: TR16EntryHeaderPtr;
  i: integer;
begin
  // Building icon
  icon_ptr := StructGraphics.get_structure_image_header(first_building_icon_image_index + index);
  if icon_ptr.EntryType <> 0 then
    Move(icon_ptr.EntryType, data.icon_data, sizeof(data.icon_data))
  else
    FillChar(data.icon_data, sizeof(data.icon_data), 0);
  // Item references
  store_item_reference(data.item_references[0], ITEM_BUILDING_GROUP, data.building_template.BuildingGroup);
  store_item_reference(data.item_references[1], ITEM_BUILDING_GROUP, data.building_template.Prereq1BuildingGroup);
  store_item_reference(data.item_references[2], ITEM_BUILDING_GROUP, data.building_template.Prereq2BuildingGroup);
  store_item_reference(data.item_references[3], ITEM_WEAPON,         data.building_template.PrimaryWeapon);
  store_item_reference(data.item_references[4], ITEM_WEAPON,         data.building_template.SecondaryWeapon);
  store_item_reference(data.item_references[5], ITEM_EXPLOSION,      data.building_template.DeathExplosion);
  store_item_reference(data.item_references[6], ITEM_EXPLOSION,      data.building_template.MuzzleFlashExplosion);
  store_item_reference(data.item_references[7], ITEM_ARMOUR_TYPE,    data.building_template.ArmorType);
  for i := 0 to MAX_BUILEXP_ANIMATIONS - 1 do
    store_item_reference(data.item_references[8+i], ITEM_EXPLOSION, IfThen(i < data.builexp_entry.NumAnimations, data.builexp_entry.AnimExplosion[i], -1));
  // Art references
  store_art_reference(data.art_references[0], ART_BUILDING,           data.building_template.BuildingArt);
  store_art_reference(data.art_references[1], ART_BUILDING,           data.building_template.BarrelArt);
  store_art_reference(data.art_references[2], ART_BUILDING_ANIMATION, data.building_template.BuildingAnimation);
  store_art_reference(data.art_references[3], ART_BUILDUP,            data.building_template.BuildupArt);
  // Building type string
  store_c_string(StringTable.text_uib.Values[templates.BuildingGroupStrings[data.building_template.BuildingGroup]], Addr(data.building_group_str), Length(data.building_group_str));
end;

procedure TStructures.restore_building_export_data(index: integer; data: TBuildingExportDataPtr; import_path: string);
var
  i: integer;
begin
  // Building icon
  if data.icon_data[0] <> 0 then
    StructGraphics.modify_image_data(first_building_icon_image_index + index, data.icon_data, sizeof(data.icon_data))
  else
    StructGraphics.modify_image_data(first_building_icon_image_index + index, data.icon_data, 1);
  // Buildup art frames
  if templates.BuildupArtFrames[index] <> data.art_references[3].frames then
  begin
    StructGraphics.change_image_entry_count(buildup_art_image_indexes[index], templates.BuildupArtFrames[index], data.art_references[3].frames);
    templates.BuildupArtFrames[index] := data.art_references[3].frames;
    compute_image_indexes;
  end;
  // Item references
  data.building_template.BuildingGroup :=        restore_item_reference(data.item_references[0], ITEM_BUILDING_GROUP, data.building_template.BuildingGroup,        import_path);
  data.building_template.Prereq1BuildingGroup := restore_item_reference(data.item_references[1], ITEM_BUILDING_GROUP, data.building_template.Prereq1BuildingGroup, import_path);
  data.building_template.Prereq2BuildingGroup := restore_item_reference(data.item_references[2], ITEM_BUILDING_GROUP, data.building_template.Prereq2BuildingGroup, import_path);
  data.building_template.PrimaryWeapon :=        restore_item_reference(data.item_references[3], ITEM_WEAPON,         data.building_template.PrimaryWeapon,        import_path);
  data.building_template.SecondaryWeapon :=      restore_item_reference(data.item_references[4], ITEM_WEAPON,         data.building_template.SecondaryWeapon,      import_path);
  data.building_template.DeathExplosion :=       restore_item_reference(data.item_references[5], ITEM_EXPLOSION,      data.building_template.DeathExplosion,       import_path);
  data.building_template.MuzzleFlashExplosion := restore_item_reference(data.item_references[6], ITEM_EXPLOSION,      data.building_template.MuzzleFlashExplosion, import_path);
  data.building_template.ArmorType :=            restore_item_reference(data.item_references[7], ITEM_ARMOUR_TYPE,    data.building_template.ArmorType,            import_path);
  for i := 0 to data.builexp_entry.NumAnimations - 1 do
    data.builexp_entry.AnimExplosion[i] := restore_item_reference(data.item_references[8+i], ITEM_EXPLOSION, data.builexp_entry.AnimExplosion[i], import_path);
  // Art references
  data.building_template.BuildingArt :=       restore_art_reference(data.art_references[0], ART_BUILDING,           data.building_template.BuildingArt,       -1,    import_path + data.building_name);
  data.building_template.BarrelArt :=         restore_art_reference(data.art_references[1], ART_BUILDING,           data.building_template.BarrelArt,         -1,    import_path + data.building_name + '_BARREL');
  data.building_template.BuildingAnimation := restore_art_reference(data.art_references[2], ART_BUILDING_ANIMATION, data.building_template.BuildingAnimation, index, import_path + data.building_name + '_ANIMATION');
  modify_art(ART_BUILDUP, index, 0, data.art_references[3].frames);
  data.building_template.BuildupArt :=        restore_art_reference(data.art_references[3], ART_BUILDUP,            data.building_template.BuildupArt,        index, import_path + data.building_name + '_BUILDUP');
  // Building type string
  if (Ord(data.building_group_str[0]) <> 0) and (StringTable.text_uib.IndexOfName(data.item_references[0].item_name) = -1) then
    Application.MessageBox(PChar(Format('The key %s is missing in current Text.UIB file. Add this key with value "%s" so that the building name appears in game.', [data.item_references[0].item_name, data.building_group_str])), 'Add string to Text.UIB', MB_OK or MB_ICONWARNING);
end;

procedure TStructures.store_unit_export_data(index: integer; data: TUnitExportDataPtr);
var
  icon_ptr: TR16EntryHeaderPtr;
  i: integer;
begin
  // Unit icon
  icon_ptr := StructGraphics.get_structure_image_header(first_unit_icon_image_index + index);
  if icon_ptr.EntryType <> 0 then
    Move(icon_ptr.EntryType, data.icon_data, sizeof(data.icon_data))
  else
    FillChar(data.icon_data, sizeof(data.icon_data), 0);
  // Item references
  store_item_reference(data.item_references[0], ITEM_UNIT_GROUP,     data.unit_template.UnitGroup);
  store_item_reference(data.item_references[1], ITEM_BUILDING_GROUP, data.unit_template.Prereq1BuildingGroup);
  store_item_reference(data.item_references[2], ITEM_BUILDING_GROUP, data.unit_template.Prereq2BuildingGroup);
  store_item_reference(data.item_references[3], ITEM_WEAPON,         data.unit_template.PrimaryWeapon);
  store_item_reference(data.item_references[4], ITEM_WEAPON,         data.unit_template.SecondaryWeapon);
  store_item_reference(data.item_references[5], ITEM_EXPLOSION,      data.unit_template.DeathExplosion);
  store_item_reference(data.item_references[6], ITEM_EXPLOSION,      data.unit_template.MuzzleFlashExplosion);
  store_item_reference(data.item_references[7], ITEM_ARMOUR_TYPE,    data.unit_template.ArmorType);
  // Art references
  store_art_reference(data.art_references[0], ART_UNIT, data.unit_template.UnitArt);
  store_art_reference(data.art_references[1], ART_UNIT, data.unit_template.BarrelArt);
  // Sound references
  for i := 0 to Length(data.sound_references) - 1 do
    store_sound_reference(data.sound_references[i], data.unit_template.Voices[i]);
  // Unit type string
  store_c_string(StringTable.text_uib.Values[templates.UnitGroupStrings[data.unit_template.UnitGroup]], Addr(data.unit_group_str), Length(data.unit_group_str));
end;

procedure TStructures.restore_unit_export_data(index: integer; data: TUnitExportDataPtr; import_path: string);
var
  i: integer;
begin
  // Unit icon
  if data.icon_data[0] <> 0 then
    StructGraphics.modify_image_data(first_unit_icon_image_index + index, data.icon_data, sizeof(data.icon_data))
  else
    StructGraphics.modify_image_data(first_unit_icon_image_index + index, data.icon_data, 1);
  // Item references
  data.unit_template.UnitGroup :=            restore_item_reference(data.item_references[0],  ITEM_UNIT_GROUP,     data.unit_template.UnitGroup,            import_path);
  data.unit_template.Prereq1BuildingGroup := restore_item_reference(data.item_references[1],  ITEM_BUILDING_GROUP, data.unit_template.Prereq1BuildingGroup, import_path);
  data.unit_template.Prereq2BuildingGroup := restore_item_reference(data.item_references[2],  ITEM_BUILDING_GROUP, data.unit_template.Prereq2BuildingGroup, import_path);
  data.unit_template.PrimaryWeapon :=        restore_item_reference(data.item_references[3],  ITEM_WEAPON,         data.unit_template.PrimaryWeapon,        import_path);
  data.unit_template.SecondaryWeapon :=      restore_item_reference(data.item_references[4],  ITEM_WEAPON,         data.unit_template.SecondaryWeapon,      import_path);
  data.unit_template.DeathExplosion :=       restore_item_reference(data.item_references[5],  ITEM_EXPLOSION,      data.unit_template.DeathExplosion,       import_path);
  data.unit_template.MuzzleFlashExplosion := restore_item_reference(data.item_references[6],  ITEM_EXPLOSION,      data.unit_template.MuzzleFlashExplosion, import_path);
  data.unit_template.ArmorType :=            restore_item_reference(data.item_references[7],  ITEM_ARMOUR_TYPE,    data.unit_template.ArmorType,            import_path);
  // Art references
  data.unit_template.UnitArt   := restore_art_reference(data.art_references[0], ART_UNIT, data.unit_template.UnitArt,   -1, import_path + data.unit_name);
  data.unit_template.BarrelArt := restore_art_reference(data.art_references[1], ART_UNIT, data.unit_template.BarrelArt, -1, import_path + data.unit_name + '_BARREL');
  // Sound references
  for i := 0 to Length(data.sound_references) - 1 do
    data.unit_template.Voices[i] := restore_sound_reference(data.sound_references[i], data.unit_template.Voices[i], import_path, true);
  // Unit type string
  if (Ord(data.unit_group_str[0]) <> 0) and (StringTable.text_uib.IndexOfName(data.item_references[0].item_name) = -1) then
    Application.MessageBox(PChar(Format('The key %s is missing in current Text.UIB file. Add this key with value "%s" so that the unit name appears in game.', [data.item_references[0].item_name, data.unit_group_str])), 'Add string to Text.UIB', MB_OK or MB_ICONWARNING);
end;

procedure TStructures.store_weapon_export_data(index: integer; data: TWeaponExportDataPtr);
begin
  store_item_reference(data.item_references[0], ITEM_EXPLOSION, data.weapon_template.HitExplosion);
  store_item_reference(data.item_references[1], ITEM_EXPLOSION, data.weapon_template.TrailExplosion);
  store_item_reference(data.item_references[2], ITEM_WARHEAD,   data.weapon_template.Warhead);
  store_art_reference(data.art_reference, ART_PROJECTILE, data.weapon_template.ProjectileArt);
  store_sound_reference(data.sound_reference, data.weapon_template.FiringSound);
end;

procedure TStructures.restore_weapon_export_data(index: integer; data: TWeaponExportDataPtr; import_path: string);
begin
  data.weapon_template.HitExplosion :=   restore_item_reference(data.item_references[0], ITEM_EXPLOSION, data.weapon_template.HitExplosion,   import_path);
  data.weapon_template.TrailExplosion := restore_item_reference(data.item_references[1], ITEM_EXPLOSION, data.weapon_template.TrailExplosion, import_path);
  data.weapon_template.Warhead :=        restore_item_reference(data.item_references[2], ITEM_WARHEAD,   data.weapon_template.Warhead,        import_path);
  modify_art(ART_PROJECTILE, index, data.art_reference.directions, 0);
  data.weapon_template.ProjectileArt :=  restore_art_reference(data.art_reference, ART_PROJECTILE, data.weapon_template.ProjectileArt, index, import_path + data.weapon_name);
  data.weapon_template.FiringSound :=    restore_sound_reference(data.sound_reference, data.weapon_template.FiringSound, import_path, false);
end;

procedure TStructures.store_explosion_export_data(index: integer; data: TExplosionExportDataPtr);
begin
  store_art_reference(data.art_reference, ART_ANIMATION, index);
  store_sound_reference(data.sound_reference, data.explosion_template.Sound);
end;

procedure TStructures.restore_explosion_export_data(index: integer; data: TExplosionExportDataPtr; import_path: string);
begin
  restore_art_reference(data.art_reference, ART_ANIMATION, index, index, import_path + data.explosion_name);
  data.explosion_template.Sound := restore_sound_reference(data.sound_reference, data.explosion_template.Sound, import_path, false);
  data.explosion_template.MyIndex := index;
end;

procedure TStructures.store_armour_type_export_data(index: integer; data: TArmourTypeExportDataPtr);
var
  i: integer;
begin
  for i := 0 to MAX_WARHEADS - 1 do
    store_item_reference(data.warhead_references[i], ITEM_WARHEAD, i);
end;

procedure TStructures.restore_armour_type_export_data(index: integer; data: TArmourTypeExportDataPtr);
var
  tmp_versus_warhead_values: array[0..MAX_WARHEADS-1] of byte;
  i, j: integer;
begin
  for i := 0 to MAX_WARHEADS - 1 do
    tmp_versus_warhead_values[i] := armour.WarheadEntries[i].VersusArmorType[index];
  for i := 0 to MAX_WARHEADS - 1 do
  begin
    if data.warhead_references[i].item_name = '' then
      break;
    for j := 0 to armour.WarheadCount - 1 do
      if data.warhead_references[i].item_name = armour.WarheadStrings[j] then
      begin
        tmp_versus_warhead_values[j] := data.versus_warhead_values[i];
        break;
      end;
  end;
  Move(tmp_versus_warhead_values[0], data.versus_warhead_values[0], MAX_WARHEADS);
end;

procedure TStructures.store_warhead_export_data(index: integer; data: TWarheadExportDataPtr);
var
  i: integer;
begin
  for i := 0 to MAX_ARMOUR_TYPES - 1 do
    store_item_reference(data.armour_type_references[i], ITEM_ARMOUR_TYPE, i);
end;

procedure TStructures.restore_warhead_export_data(index: integer; data: TWarheadExportDataPtr);
var
  tmp_versus_armor_type: array[0..MAX_ARMOUR_TYPES-1] of byte;
  i, j: integer;
begin
  for i := 0 to MAX_ARMOUR_TYPES - 1 do
    tmp_versus_armor_type[i] := armour.WarheadEntries[index].VersusArmorType[i];
  for i := 0 to MAX_ARMOUR_TYPES - 1 do
  begin
    if data.armour_type_references[i].item_name = '' then
      break;
    for j := 0 to armour.ArmourTypeCount - 1 do
      if data.armour_type_references[i].item_name = armour.ArmourTypeStrings[j] then
      begin
        tmp_versus_armor_type[j] := data.warhead_entry.VersusArmorType[i];
        break;
      end;
  end;
  Move(tmp_versus_armor_type[0], data.warhead_entry.VersusArmorType[0], MAX_ARMOUR_TYPES);
end;

procedure TStructures.store_unit_voices_export_data(index: integer; data: TUnitVoicesExportDataPtr);
var
  i: integer;
begin
  Move(templates.UnitDefinitions[index].Voices[0], data.voices[0], Length(data.voices) * sizeof(Cardinal));
  for i := 0 to Length(data.sound_references) - 1 do
    store_sound_reference(data.sound_references[i], data.voices[i]);
end;

procedure TStructures.restore_unit_voices_export_data(index: integer; data: TUnitVoicesExportDataPtr; import_path: string);
var
  i: integer;
begin
  for i := 0 to Length(data.sound_references) - 1 do
    data.voices[i] := restore_sound_reference(data.sound_references[i], data.voices[i], import_path, true);
  Move(data.voices[0], templates.UnitDefinitions[index].Voices[0], Length(data.voices) * sizeof(Cardinal));
end;

procedure TStructures.store_item_export_data(item_type, index: integer; data: TByteArrayPtr);
var
  offset: integer;
  ptrs: TItemTypePointersPtr;
  data_ptrs: TItemDataPointersPtr;
  i: integer;
begin
  ptrs := Addr(item_type_pointers[item_type]);
  // Store item name
  Move(ptrs.item_name_list_ptr[index * ptrs.item_name_length], data[0], ptrs.item_name_length);
  offset := ptrs.item_name_length;
  // Store item data
  for i := 0 to ptrs.item_data_pointers_count - 1 do
  begin
    data_ptrs := Addr(item_data_pointers[ptrs.item_data_pointers_first + i]);
    Move(data_ptrs.data_ptr[index * data_ptrs.data_size], data[offset], data_ptrs.data_size);
    Inc(offset, data_ptrs.data_size);
  end;
  // Store item-specific data
  case item_type of
    ITEM_BUILDING:    store_building_export_data   (index, Addr(data[0]));
    ITEM_UNIT:        store_unit_export_data       (index, Addr(data[0]));
    ITEM_WEAPON:      store_weapon_export_data     (index, Addr(data[0]));
    ITEM_EXPLOSION:   store_explosion_export_data  (index, Addr(data[0]));
    ITEM_ARMOUR_TYPE: store_armour_type_export_data(index, Addr(data[0]));
    ITEM_WARHEAD:     store_warhead_export_data    (index, Addr(data[0]));
    ITEM_UNIT_VOICES: store_unit_voices_export_data(index, Addr(data[0]));
  end;
end;

procedure TStructures.restore_item_export_data(item_type, index: integer; data: TByteArrayPtr; import_path: string);
var
  offset: integer;
  ptrs: TItemTypePointersPtr;
  data_ptrs: TItemDataPointersPtr;
  i: integer;
begin
  ptrs := Addr(item_type_pointers[item_type]);
  // Retore item name
  Move(data[0], ptrs.item_name_list_ptr[index * ptrs.item_name_length], ptrs.item_name_length);
  offset := ptrs.item_name_length;
  // Restore item-specific data
  case item_type of
    ITEM_BUILDING:    restore_building_export_data   (index, Addr(data[0]), import_path);
    ITEM_UNIT:        restore_unit_export_data       (index, Addr(data[0]), import_path);
    ITEM_WEAPON:      restore_weapon_export_data     (index, Addr(data[0]), import_path);
    ITEM_EXPLOSION:   restore_explosion_export_data  (index, Addr(data[0]), import_path);
    ITEM_ARMOUR_TYPE: restore_armour_type_export_data(index, Addr(data[0]));
    ITEM_WARHEAD:     restore_warhead_export_data    (index, Addr(data[0]));
    ITEM_UNIT_VOICES: restore_unit_voices_export_data(index, Addr(data[0]), import_path);
  end;
  // Restore item data
  for i := 0 to ptrs.item_data_pointers_count - 1 do
  begin
    data_ptrs := Addr(item_data_pointers[ptrs.item_data_pointers_first + i]);
    Move(data[offset], data_ptrs.data_ptr[index * data_ptrs.data_size], data_ptrs.data_size);
    Inc(offset, data_ptrs.data_size);
  end;
end;

procedure TStructures.export_item(item_type, index: integer; filename: string);
var
  ptrs: TItemTypePointersPtr;
  data: array of byte;
begin
  ptrs := Addr(item_type_pointers[item_type]);
  SetLength(data, ptrs.export_data_size);
  store_item_export_data(item_type, index, Addr(data[0]));
  save_binary_file(filename, data[0], ptrs.export_data_size);
  SetLength(data, 0);
end;

procedure TStructures.import_item(item_type, index: integer; filename: string);
var
  ptrs: TItemTypePointersPtr;
  data: array of byte;
begin
  ptrs := Addr(item_type_pointers[item_type]);
  SetLength(data, ptrs.export_data_size);
  load_binary_file(filename, data[0], ptrs.export_data_size);
  restore_item_export_data(item_type, index, Addr(data[0]), ExtractFilePath(filename));
  SetLength(data, 0);
  Dispatcher.register_event(evStructuresImportItem);
end;

procedure TStructures.copy_item(item_type, index: integer);
var
  ptrs: TItemTypePointersPtr;
  handle: THandle;
  data_ptr: Pointer;
begin
  ptrs := Addr(item_type_pointers[item_type]);
  OpenClipboard(Application.Handle);
  EmptyClipboard;
  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, ptrs.export_data_size);
  data_ptr := GlobalLock(handle);
  store_item_export_data(item_type, index, data_ptr);
  GlobalUnLock(handle);
  SetClipboardData(ptrs.clipboard_format, handle);
  CloseClipboard;
end;

procedure TStructures.paste_item(item_type, index: integer);
var
  ptrs: TItemTypePointersPtr;
  handle: THandle;
  data_ptr: Pointer;
begin
  ptrs := Addr(item_type_pointers[item_type]);
  if not Clipboard.HasFormat(ptrs.clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(ptrs.clipboard_format);
  data_ptr := GlobalLock(handle);
  restore_item_export_data(item_type, index, data_ptr, '');
  GlobalUnLock(handle);
  CloseClipboard;
  Dispatcher.register_event(evStructuresImportItem);
end;

function TStructures.load_remap_structures_ini_file(ini_filename: String; remap_structures_data: TRemapStructuresDataPtr): String;
var
  ini: TMemIniFile;
  strings: TStringList;
  i, j: integer;
begin
  result := 'The ini file must contain at least one of following sections:'#13 +
            '[Remap_Units] [Remap_UnitGroups] [Remap_Buildings] [Remap_BuildingGroups] [Remap_Weapons] [Remap_Explosions]'#13 +
            'with key-value pairs in the form'#13 +
            'from_type=to_type'#13 +
            'where key and value is a number.';
  // Load ini file
  ini := TMemIniFile.Create(ini_filename);
  strings := TStringList.Create;
  // Check for ini file validity
  ini.ReadSections(strings);
  for i := 0 to strings.Count - 1 do
    for j := Ord(ilUnits) to Ord(ilExplosions) do
      if strings[i] = 'Remap_' + ItemListTypeStr[j] then
        result := '';
  strings.Destroy;
  if result <> '' then
  begin
    ini.Destroy;
    exit;
  end;
  // Read ini file
  for i := 0 to MAX_UNIT_TYPES-1 do
  begin
    remap_structures_data.remap_units[i] := ini.ReadInteger('Remap_Units', IntToStr(i), i);
    if (remap_structures_data.remap_units[i] >= templates.UnitCount) and (remap_structures_data.remap_units[i] <> i) then
    begin
      result := 'Invalid target type ' + IntToStr(remap_structures_data.remap_units[i]) + ' in section [Remap_Units]';
      ini.Destroy;
      exit;
    end;
    remap_structures_data.remap_unitgroups[i] := ini.ReadInteger('Remap_UnitGroups', IntToStr(i), i);
    if (remap_structures_data.remap_unitgroups[i] >= templates.UnitGroupCount) and (remap_structures_data.remap_unitgroups[i] <> i) then
    begin
      result := 'Invalid target type ' + IntToStr(remap_structures_data.remap_unitgroups[i]) + ' in section [Remap_UnitGroups]';
      ini.Destroy;
      exit;
    end;
  end;
  for i := 0 to MAX_BUILDING_TYPES-1 do
  begin
    remap_structures_data.remap_buildings[i] := ini.ReadInteger('Remap_Buildings', IntToStr(i), i);
    if (remap_structures_data.remap_buildings[i] >= templates.BuildingCount) and (remap_structures_data.remap_buildings[i] <> i) then
    begin
      result := 'Invalid target type ' + IntToStr(remap_structures_data.remap_buildings[i]) + ' in section [Remap_Buildings]';
      ini.Destroy;
      exit;
    end;
    remap_structures_data.remap_buildinggroups[i] := ini.ReadInteger('Remap_BuildingGroups', IntToStr(i), i);
    if (remap_structures_data.remap_buildinggroups[i] >= templates.BuildingGroupCount) and (remap_structures_data.remap_buildinggroups[i] <> i) then
    begin
      result := 'Invalid target type ' + IntToStr(remap_structures_data.remap_buildinggroups[i]) + ' in section [Remap_BuildingGroups]';
      ini.Destroy;
      exit;
    end;
  end;
  for i := 0 to MAX_WEAPONS-1 do
  begin
    remap_structures_data.remap_weapons[i] := ini.ReadInteger('Remap_Weapons', IntToStr(i), i);
    if (remap_structures_data.remap_weapons[i] >= templates.WeaponCount) and (remap_structures_data.remap_weapons[i] <> i) then
    begin
      result := 'Invalid target type ' + IntToStr(remap_structures_data.remap_weapons[i]) + ' in section [Remap_Weapons]';
      ini.Destroy;
      exit;
    end;
  end;
  for i := 0 to MAX_EXPLOSIONS-1 do
  begin
    remap_structures_data.remap_explosions[i] := ini.ReadInteger('Remap_Explosions', IntToStr(i), i);
    if (remap_structures_data.remap_explosions[i] >= templates.ExplosionCount) and (remap_structures_data.remap_explosions[i] <> i) then
    begin
      result := 'Invalid target type ' + IntToStr(remap_structures_data.remap_explosions[i]) + ' in section [Remap_Explosions]';
      ini.Destroy;
      exit;
    end;
  end;
  ini.Destroy;
end;

function TStructures.remap_structure(remap_structures_data: TRemapStructuresDataPtr; list_type: ItemListType; struct_type: integer): integer;
begin
  result := struct_type;
  case list_type of
    ilUnits:          result := remap_structures_data.remap_units[struct_type];
    ilUnitGroups:     result := remap_structures_data.remap_unitgroups[struct_type];
    ilBuildings:      result := remap_structures_data.remap_buildings[struct_type];
    ilBuildingGroups: result := remap_structures_data.remap_buildinggroups[struct_type];
    ilWeapons:        result := remap_structures_data.remap_weapons[struct_type];
    ilExplosions:     result := remap_structures_data.remap_explosions[struct_type];
  end;
end;

end.

