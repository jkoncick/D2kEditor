unit _structures;

interface

uses Windows, Classes, Graphics, SysUtils, Math, Types, _utils;

// *****************************************************************************
// TEMPLATES.BIN file definitions
// *****************************************************************************

const UF_STEALTH      = $00000010;
const UF_NO_AI        = $00000800;
const UF_FIXED_BARREL = $00008000;
const UF_SELFHEALING  = $00800000;

type
  TUnitTemplate = packed record
    OwnerSide:             byte;
    UnitType:              byte;
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
    Unknown18:             byte;
    Unknown19:             byte;
    UnitArt:               integer;
    BarrelArt:             integer;
    Cost:                  cardinal;
    BuildSpeed:            cardinal;
    TechLevel:             shortint;
    AvailableInStarport:   byte;
    HasBarrel:             byte;
    Prereq1UpgradesNeeded: byte;
    Prereq1BuildingType:   integer;
    Prereq1OwnerSide:      byte;
    SpecialBehavior:       byte;
    Unknown46:             byte;
    DeathExplosion:        shortint;
    Prereq2BuildingType:   integer;
    Unknown52:             byte;
    CanCrushInfantry:      byte;
    HealthBarSize:         byte;
    Unknown55:             byte;
    Flags:                 cardinal;
    DirectionFrames:       array[0..31] of byte;
    Voices:                array[0..17] of cardinal;
    Unknown164:            cardinal;
    FiringExplosion:       shortint;
    SpeedType:             byte;
    MultiplayerOnly:       byte;
    ZeroBytes:             array[0..84] of byte;
  end;

  TUnitTemplatePtr = ^TUnitTemplate;

const BF_AUTOREPAIR     = $00000001;
const BF_ANIM_PERMANENT = $00000010;
const BF_HAS_ANIMATION  = $00000040;
const BF_UNKNOWN9       = $00000100;
const BF_SELECT_REPAIR  = $00000200;
const BF_CAN_CAPTURE    = $00000400;
const BF_ALWAYS_DECAY   = $00008000;
const BF_HAS_SKIRT      = $00200000;
const BF_NO_CONCRETE    = $00400000;
const BF_ANIM_ALPHA     = $00800000;
const BF_CANNOT_SELL    = $01000000;

type
  TBuildingTemplate = packed record
    HitPoints:             cardinal;
    OwnerSide:             byte;
    ArmorType:             byte;
    BarrelRotationSpeed:   byte;
    RateOfFire:            byte;
    Unknown8:              cardinal;
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
    Prereq1BuildingType:   integer;
    Prereq1OwnerSide:      byte;
    Prereq1UpgradesNeeded: byte;
    Unknown70:             byte;
    Unknown71:             byte;
    Prereq2BuildingType:   integer;
    Prereq2OwnerSide:      byte;
    Prereq2UpgradesNeeded: byte;
    RequireEnoughPower:    byte;
    DeathExplosion:        shortint;
    TilesOccupiedAll:      cardinal;
    TilesOccupiedSolid:    cardinal;
    Flags:                 cardinal;
    SpecialBehavior:       byte;
    Unknown93:             byte;
    Unknown94:             byte;
    HealthBarSize:         byte;
    ExitPoint1X:           shortint;
    ExitPoint1Y:           shortint;
    ExitPoint2X:           shortint;
    ExitPoint2Y:           shortint;
    DirectionFrames:       array[0..31] of byte;
    Unknown132:            byte;
    AnimationSpeed:        byte;
    ArtHeight:             byte;
    ArtWidth:              byte;
    BuildingType:          byte;
    BuildupFramesToShow:   byte;
    BuildupArt:            byte;
    BuildingAnimation:     byte;
    FiringExplosion:       shortint;
    ZeroBytes:             array[0..126] of byte;
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
    Unknown19:             byte;
    AntiAircraft:          byte;
    Warhead:               byte;
    Unknown22:             byte;
    Unknown23:             byte;
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
const EF_FIRING_FLASH      = $00000400;
const EF_INVISIBLE         = $00000800;

type
  TExplosionTemplate = packed record
    MyIndex:               byte;
    FiringPattern:         byte;
    Unknown2:              byte;
    Unknown3:              byte;
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
    UnitTypeStrings:         array[0..MAX_UNIT_TYPES-1,     0..49]  of char;
    BuildingTypeStrings:     array[0..MAX_BUILDING_TYPES-1, 0..49]  of char;
    Other:                   array[0..85]                   of shortint;
    AnimationArtFlags:       array[0..MAX_EXPLOSIONS-1]     of cardinal;
    BuildingAnimationFrames: array[0..MAX_BUILDING_TYPES-1] of byte;
    BuildupArtFrames:        array[0..MAX_BUILDING_TYPES-1] of byte;
    BuildingTypeCount:       byte;
    UnitTypeCount:           byte;
  end;

const NUM_TECHNICAL_GRAPHICAL_ENTRIES = 206;
const NUM_EMPTY_UNIT_SIDEBAR_ICONS = 10;
const MAX_BUILDING_SIZE = 4;

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

// *****************************************************************************
// ARMOUR.BIN file definitions
// *****************************************************************************

const MAX_WARHEADS = 30;
const MAX_ARMOUR_TYPES = 12;

type
  TWarheadEntry = packed record
    VersusArmorType: array[0..MAX_ARMOUR_TYPES-1] of byte;
    Unknown12:       cardinal;
    Unknown16:       cardinal;
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
    ZeroBytes: array[0..127] of byte;
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
    player: byte;
    stype: byte;
  end;

  TTileDataEntryPtr = ^TTileDataEntry;

const EMPTY_TILEDATA_ENTRY: TTileDataEntry = (index: 0; player: 0; stype: ST_NOTHING);

// *****************************************************************************
// DATA.R16 file definitions
// *****************************************************************************

type
  TR16EntryHeader = packed record
    EntryType: byte;
    ImageWidth: integer;
    ImageHeight: integer;
    ImageOffsetX: integer;
    ImageOffsetY: integer;
    ImageHandle: cardinal;
    PaletteHandle: cardinal;
    BitsPerPixel: byte;
    FrameHeight: byte;
    FrameWidth: byte;
    Alignment: byte;
  end;

  TR16EntryHeaderPtr = ^TR16EntryHeader;

  TR16Palette = packed record
    Memory: integer;
    PaletteHandle2: integer;
    Colors: array[0..255] of word;
  end;

  TR16PalettePtr = ^TR16Palette;

// *****************************************************************************
// Internal graphical data structure definitions
// *****************************************************************************

type
  TStructureImage = record
    bitmap: TBitmap;
    bitmap_mask: TBitmap;
    bitmap_data: TWordArrayPtr;
    offset_x: SmallInt;
    offset_y: SmallInt;
    house_color_pixel_first_index: integer;
    house_color_pixel_count: word;
    current_house_color: SmallInt;
  end;

  TStructureImagePtr = ^TStructureImage;

  TStructureImageMappingEntry = record
    normal_index: smallint;
    stealth_index: smallint;
  end;

const STRUCTURE_IMAGE_ENTRY_NOT_LOADED = -1;
const STRUCTURE_IMAGE_ENTRY_EMPTY = -2;

const DATA_R16_FILE_ENTRY_POSITIONS_ALLOC_STEP = 1024;
const STRUCTURE_IMAGES_ALLOC_STEP = 128;
const HOUSE_COLOR_PIXEL_ALLOC_STEP = 8192;

const WALL_FRAME_MAPPING: array[0..15] of integer = (0, 2, 3, 12, 1, 6, 11, 7, 4, 13, 5, 9, 15, 10, 8, 14);

type
  TBuildingSkirt = record
    size_x: integer;
    size_y: integer;
    rock_tile_x: integer;
    rock_tile_y: integer;
    conc_tile_x: integer;
    conc_tile_y: integer;
  end;

const CONCRETE_TILES: array[0..2] of word = (651, 671, 691);

const BUILDING_SKIRT_2x2: TBuildingSkirt = (size_x: 2; size_y: 2; rock_tile_x: 17; rock_tile_y: 30; conc_tile_x:  9; conc_tile_y: 32);
const BUILDING_SKIRT_3x2: TBuildingSkirt = (size_x: 3; size_y: 2; rock_tile_x: 11; rock_tile_y: 30; conc_tile_x:  3; conc_tile_y: 32);

// *****************************************************************************
// Mis AI properties definitions
// *****************************************************************************

type
  TMisAIProperty = record
    name: String;
    data_type: char;
    position: integer;
  end;

// *****************************************************************************
// Misc. objects definitions
// *****************************************************************************

type
  TMiscObjectInfo = record
    name: String;
    value: word;
    color: Cardinal;
    stats_group: word;
  end;

type TObjectStatsGroup = (sgNone, sgWormSpawners, sgPlayerStarts, sgSpiceBlooms);

// *****************************************************************************
// Player definitions
// *****************************************************************************

type
  TPlayerInfo = record
    name: String;
    shortname: String;
    color: Cardinal;
    color_inv: Cardinal;
  end;

// *****************************************************************************
// Templates other definitions
// *****************************************************************************

type
  TTemplatesOtherByteType = (tobtNone, tobtBuilding, tobtUnit, tobtWeapon, tobtExplosion);

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
    colours_bin_filename: String;
    data_r16_filename: String;
    graphics_misc_objects_filename: String;
    players_ini_filename: String;
    misc_objects_ini_filename: String;
    limits_ini_filename: String;
    templates_other_txt_filename: String;

    // TEMPLATES.BIN related data
    templates: TTemplatesBinFile;

    unit_art_image_indexes:           array[0..MAX_UNIT_ART-1] of word;
    building_art_image_indexes:       array[0..MAX_BUILDING_ART-1] of word;
    projectile_art_image_indexes:     array[0..MAX_WEAPONS-1] of word;
    animation_art_image_indexes:      array[0..MAX_EXPLOSIONS-1] of word;
    first_unit_icon_image_index:      word;
    first_building_icon_image_index:  word;
    buildup_art_image_indexes:        array[0..MAX_BUILDING_TYPES-1] of word;
    building_animation_image_indexes: array[0..MAX_BUILDING_ART-1] of word;

    unit_side_versions:     array[0..MAX_UNIT_TYPES-1,     0..CNT_PLAYERS-1] of shortint;
    building_side_versions: array[0..MAX_BUILDING_TYPES-1, 0..CNT_PLAYERS-1] of shortint;

    building_type_mapping:  array[0..MAX_BUILDING_TYPES-1] of shortint;
    building_type_mapping_count:      integer;

    // BUILEXP.BIN related data
    builexp: array[0..MAX_BUILDING_TYPES-1] of TBuilExpEntry;

    // ARMOUR.BIN related data
    armour: TArmourBinFile;

    // SPEED.BIN related data
    speed: TSpeedBinFile;

    // TECHPOS.BIN related data
    techpos: array[0..9,0..9] of TTechposUnitEntry;

    // TILEDATA.BIN related data
    tiledata: array[0..CNT_TILEDATA_ENTRIES-1] of TTileDataEntry;

    // COLOURS.BIN related data
    colours: array[0..7,0..15] of word;

    // DATA.R16 related data
    data_r16_file_contents: array of byte;
    data_r16_file_entry_positions: array of integer;
    data_r16_file_entry_count: integer;

    structure_images: array of TStructureImage;
    structure_images_count: integer;
    structure_image_entry_mapping: array of TStructureImageMappingEntry;
    house_color_pixel_indexes: array of word;
    house_color_pixel_shades: array of byte;
    house_color_pixel_count_total: integer;

    // Mis AI properties related data
    mis_ai_properties_template: array of TMisAIProperty;
    mis_ai_properties: array of TMisAIProperty;
    cnt_mis_ai_properties: integer;

    // Misc. objects related data
    graphics_misc_objects: TBitmap;
    graphics_misc_objects_mask: TBitmap;
    misc_object_info: array of TMiscObjectInfo;
    cnt_misc_objects: integer;

    // Player related data
    player_info: array[0..CNT_PLAYERS-1] of TPlayerInfo;

    // Limits related data
    limit_spice_blooms: integer;
    limit_structures_total: integer;
    limit_refineries_per_player: integer;

    // Templates other related data
    templates_other: TStringList;
    templates_other_byte_types: array[0..85] of TTemplatesOtherByteType;

  public
    // General procedures
    procedure init;
    function get_status: String;

    // TEMPLATES.BIN related procedures
    procedure load_templates_bin(force: boolean);
    procedure save_templates_bin;
    procedure compute_image_indexes;
    procedure compute_building_and_unit_side_versions;
    procedure compute_building_type_mapping;
    function prettify_structure_name(str: String): String;
    function get_unit_name_str(index: integer): String;
    function get_unit_type_str(index: integer): String;
    function get_building_name_str(index: integer): String;
    function get_building_type_str(index: integer): String;
    function get_unit_template(unit_type, player: integer): TUnitTemplatePtr;
    function get_building_template(building_type, player: integer): TBuildingTemplatePtr;
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
    // COLOURS.BIN related procedures
    procedure load_colours_bin;
    // DATA.R16 related procedures
    procedure load_data_r16;
    procedure clear_structure_image_data;
    procedure clear_last_structure_image(entry_index: integer; is_stealth: boolean);
    procedure load_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean);
    procedure recolor_structure_image(image_index, house_index: integer);
    function get_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean; var was_already_loaded: boolean): TStructureImagePtr;
    function get_structure_image_header(entry_index: integer): TR16EntryHeaderPtr;
    // Mis AI properties related procedures
    procedure load_mis_ai_properties_ini;
    procedure cache_mis_ai_properties;
    // Misc. objects related procedures
    procedure load_graphics_misc_objects;
    procedure load_misc_objects_ini;
    procedure register_misc_objects_in_tiledata;
    // Players related procedures
    procedure load_players_ini;
    // Limits related procedures
    procedure load_limits_ini;
    // Templates other related procedures
    procedure load_templates_other_txt;
  end;

var
  Structures: TStructures;

implementation

uses Forms, IniFiles, mission_dialog, _settings, _mission, _stringtable, _dispatcher;

procedure TStructures.init;
begin
  graphics_misc_objects := TBitmap.Create;
  graphics_misc_objects_mask := TBitmap.Create;
  templates_other := TStringList.Create;
  load_templates_bin(false);
  load_builexp_bin(false);
  load_armour_bin(false);
  load_speed_bin(false);
  load_techpos_bin(false);
  load_tiledata_bin;
  load_colours_bin;
  load_data_r16;
  load_mis_ai_properties_ini;
  load_graphics_misc_objects;
  load_misc_objects_ini;
  load_players_ini;
  load_limits_ini;
  load_templates_other_txt;
end;

function TStructures.get_status: String;
begin
  result :=
    'Templates.bin file: '+templates_bin_filename+#13+
    'BUILEXP.BIN file: '+builexp_bin_filename+#13+
    'ARMOUR.BIN file: '+armour_bin_filename+#13+
    'SPEED.BIN file: '+speed_bin_filename+#13+
    'TILEDATA.BIN file: '+tiledata_bin_filename+#13+
    'COLOURS.BIN file: '+colours_bin_filename+#13+
    'DATA.R16 file: '+data_r16_filename+#13+
    'misc_objects.bmp file: '+graphics_misc_objects_filename+#13+
    'misc_objects.ini file: '+misc_objects_ini_filename+#13+
    'players.ini file: '+players_ini_filename+#13+
    'limits.ini file: '+limits_ini_filename+#13+
    'templates_other.txt file: '+templates_other_txt_filename+#13+
    'Structure images loaded: '+inttostr(structure_images_count)+#13+
    'House color pixel count: '+inttostr(house_color_pixel_count_total);
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
  compute_building_and_unit_side_versions;
  compute_building_type_mapping;

  // Register event in dispatcher
  Dispatcher.register_event(evFLTemplatesBin);
end;

procedure TStructures.save_templates_bin;
begin
  if templates_bin_filename = '' then
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
  for i := 0 to data.UnitArtCount-1 do
  begin
    unit_art_image_indexes[i] := index;
    inc(index, data.UnitArtAnimationFrames[i] * data.UnitArtDirectionFrames[i]);
  end;
  for i := 0 to data.BuildingArtCount-1 do
  begin
    building_art_image_indexes[i] := index;
    inc(index, data.BuildingArtDirections[i] * 2 + 1);
  end;
  for i := 0 to data.ProjectileArtCount-1 do
  begin
    projectile_art_image_indexes[i] := index;
    inc(index, data.ProjectileArtDirections[i]);
  end;
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

procedure TStructures.compute_building_and_unit_side_versions;
var
  data: ^TTemplatesBinFile;
  i, j: integer;
  index: integer;
begin
  data := Addr(templates);
  // Initialize
  for i := 0 to MAX_UNIT_TYPES - 1 do
    for j := 0 to CNT_PLAYERS - 1 do
      unit_side_versions[i,j] := -1;
  for i := 0 to MAX_BUILDING_TYPES - 1 do
    for j := 0 to CNT_PLAYERS - 1 do
      building_side_versions[i,j] := -1;
  // Compute unit side versions
  for i := 0 to MAX_UNIT_TYPES - 1 do
    for j := 0 to CNT_PLAYERS - 1 do
      for index := 0 to data.UnitCount - 1 do
        if data.UnitDefinitions[index].UnitType = i then
        begin
          if unit_side_versions[i,j] = -1 then
            unit_side_versions[i,j] := index;
          if (data.UnitDefinitions[index].OwnerSide and (1 shl j)) <> 0 then
            unit_side_versions[i,j] := index;
        end;
  // Compute building side versions
  for i := 0 to MAX_BUILDING_TYPES - 1 do
    for j := 0 to CNT_PLAYERS - 1 do
      for index := 0 to data.BuildingCount - 1 do
        if data.BuildingDefinitions[index].BuildingType = i then
        begin
          if building_side_versions[i,j] = -1 then
            building_side_versions[i,j] := index;
          if (data.BuildingDefinitions[index].OwnerSide and (1 shl j)) <> 0 then
            building_side_versions[i,j] := index;
        end;
end;

procedure TStructures.compute_building_type_mapping;
var
  i, j: integer;
  building_template: TBuildingTemplatePtr;
begin
  j := 0;
  for i := 0 to templates.BuildingTypeCount - 1 do
  begin
    building_template := get_building_template(i, 0);
    // If building is concrete, do not add it to list
    if (building_template <> nil) and (building_template.SpecialBehavior = 15) then
      continue;
    building_type_mapping[j] := i;
    inc(j);
  end;
  building_type_mapping_count := j;
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

function TStructures.get_unit_type_str(index: integer): String;
var
  row: integer;
begin
  if index < templates.UnitTypeCount then
  begin
    row := -1;
    if settings.TranslateStructureNames then
      row := StringTable.text_uib.IndexOfName(templates.UnitTypeStrings[index]);
    if row <> -1 then
      result := StringTable.text_uib.ValueFromIndex[row]
    else
      result := prettify_structure_name(templates.UnitTypeStrings[index])
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

function TStructures.get_building_type_str(index: integer): String;
var
  row: integer;
begin
  if index < templates.BuildingTypeCount then
  begin
    row := -1;
    if settings.TranslateStructureNames then
      row := StringTable.text_uib.IndexOfName(templates.BuildingTypeStrings[index]);
    if row <> -1 then
      result := StringTable.text_uib.ValueFromIndex[row]
    else
      result := prettify_structure_name(templates.BuildingTypeStrings[index]);
  end else
    result := 'UNDEFINED#' + inttostr(index);
end;

function TStructures.get_unit_template(unit_type, player: integer): TUnitTemplatePtr;
var
  unit_template_index: integer;
begin
  result := nil;
  unit_template_index := unit_side_versions[unit_type, player];
  if unit_template_index = -1 then
    exit;
  result := Addr(templates.UnitDefinitions[unit_template_index]);
end;

function TStructures.get_building_template(building_type, player: integer): TBuildingTemplatePtr;
var
  building_template_index: integer;
begin
  result := nil;
  building_template_index := building_side_versions[building_type, player];
  if building_template_index = -1 then
    exit;
  result := Addr(templates.BuildingDefinitions[building_template_index]);
end;

function TStructures.check_links_with_wall(special: word): boolean;
var
  tiledata_entry: TTiledataEntryPtr;
  building_template: TBuildingTemplatePtr;
begin
  result := false;
  tiledata_entry := get_tiledata_entry(special);
  if tiledata_entry.stype <> ST_BUILDING then
    exit;
  building_template := get_building_template(tiledata_entry.index, Mission.get_player_alloc_index(tiledata_entry.player));
  if building_template = nil then
    exit;
  result := (building_template.SpecialBehavior = 14) or (building_template.SpecialBehavior = 16);
end;

procedure TStructures.get_structure_size(special: word; var size_x, size_y: integer);
var
  tiledata_entry: TTiledataEntryPtr;
  building_template: TBuildingTemplatePtr;
  xx, yy: integer;
begin
  size_x := 0;
  size_y := 0;
  tiledata_entry := get_tiledata_entry(special);
  if tiledata_entry.stype = ST_BUILDING then
  begin
    building_template := get_building_template(tiledata_entry.index, Mission.get_player_alloc_index(tiledata_entry.player));
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
  if tiledata_entry.stype = ST_UNIT then
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
  if (tmp_filename = '') or (tmp_filename = builexp_bin_filename) then
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
  // Register event in dispatcher
  Dispatcher.register_event(evFLSpeedBin);
end;

procedure TStructures.save_speed_bin;
begin
  if speed_bin_filename = '' then
    exit;
  save_binary_file(speed_bin_filename, speed, sizeof(speed));
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
  // Register event in dispatcher
  Dispatcher.register_event(evFLTechposBin);
end;

procedure TStructures.save_techpos_bin;
begin
  if techpos_bin_filename = '' then
    exit;
  save_binary_file(techpos_bin_filename, techpos, sizeof(techpos));
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

procedure TStructures.load_colours_bin;
var
  tmp_filename, tmp_filename2: String;
  color: Cardinal;
  i: integer;
begin
  // Step 1 - editor's internal file
  tmp_filename := current_dir + 'config\COLOURS.BIN';
  if Settings.LoadCustomColoursBin then
  begin
    // Step 2 - game's internal file
    tmp_filename2 := Settings.GamePath + '\Data\bin\COLOURS.BIN';
    if FileExists(tmp_filename2) then
      tmp_filename := tmp_filename2;
    // Step 3 - file under CustomCampaignData folder
    tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\Colours\' + MissionDialog.cbColoursBin.Text;
    if FileExists(tmp_filename2) then
      tmp_filename := tmp_filename2;
  end;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file COLOURS.BIN', 'Error loading game file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // This file is already loaded - do not load it again
  if tmp_filename = colours_bin_filename then
    exit;
  colours_bin_filename := tmp_filename;
  // Load COLOURS.BIN file
  load_binary_file(tmp_filename, colours, sizeof(colours));
  for i := 0 to CNT_PLAYERS - 1 do
  begin
    color := 0;
    color := color or (((colours[i, 8] and $7C00) shr 10) shl 19) or (((colours[i, 8] and $7C00) shr 12) shl 16);
    color := color or (((colours[i, 8] and $03E0) shr  5) shl 11) or (((colours[i, 8] and $03E0) shr  7) shl  8);
    color := color or (((colours[i, 8] and $001F) shr  0) shl  3) or (((colours[i, 8] and $001F) shr  2) shl  0);
    player_info[i].color := color;
    player_info[i].color_inv := ((color and $FF0000) shr 16) or (color and $00FF00) or ((color and $0000FF) shl 16);
  end;
  // Invalidate all preloaded structure images
  for i := 0 to structure_images_count - 1 do
    structure_images[i].current_house_color := -1;
  // Register event in dispatcher
  Dispatcher.register_event(evFLColoursBin);
end;

procedure TStructures.load_data_r16;
var
  tmp_filename: String;
  data_r16_file: file of byte;
  data_r16_file_size: integer;
  index: integer;
  position: integer;
  first_byte: byte;
  header: TR16EntryHeaderPtr;
  image_data_size: integer;
  total_entry_size: integer;
begin
  tmp_filename := find_file('Data\DATA.R16', 'graphics');
  if (tmp_filename = '') or (tmp_filename = data_r16_filename) then
    exit;
  data_r16_filename := tmp_filename;

  // Clear all previously loaded data
  clear_structure_image_data;
  // Load file into buffer
  AssignFile(data_r16_file, tmp_filename);
  Reset(data_r16_file);
  data_r16_file_size := FileSize(data_r16_file);
  SetLength(data_r16_file_contents, data_r16_file_size);
  BlockRead(data_r16_file, data_r16_file_contents[0], data_r16_file_size);
  CloseFile(data_r16_file);
  // Parse file
  index := 0;
  position := 0;
  while position < data_r16_file_size do
  begin
    first_byte := data_r16_file_contents[position];
    // Reallocate data_r16_file_entry_positions capacity if needed
    if index = Length(data_r16_file_entry_positions) then
      SetLength(data_r16_file_entry_positions, Length(data_r16_file_entry_positions) + DATA_R16_FILE_ENTRY_POSITIONS_ALLOC_STEP);
    data_r16_file_entry_positions[index] := position;
    if first_byte = 0 then
    begin
      inc(position);
      inc(index);
      continue;
    end;
    header := Addr(data_r16_file_contents[position]);
    image_data_size := header.ImageWidth * header.ImageHeight * (header.BitsPerPixel div 8);
    total_entry_size := sizeof(TR16EntryHeader) + image_data_size;
    if (header.PaletteHandle <> 0) and (header.BitsPerPixel = 8) and (header.EntryType = 1) then
      inc(total_entry_size, sizeof(TR16Palette));
    inc(position, total_entry_size);
    inc(index);
  end;
  data_r16_file_entry_count := index;
  // Initialize entry mapping
  SetLength(structure_image_entry_mapping, data_r16_file_entry_count);
  for index := 0 to data_r16_file_entry_count - 1 do
  begin
    structure_image_entry_mapping[index].normal_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
    structure_image_entry_mapping[index].stealth_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
  end;
  // Register event in dispatcher
  Dispatcher.register_event(evFLDataR16);
end;

procedure TStructures.clear_structure_image_data;
var
  i: integer;
begin
  // Clean DATA.R16 file contents
  SetLength(data_r16_file_contents, 0);
  SetLength(data_r16_file_entry_positions, 0);
  data_r16_file_entry_count := 0;
  // Clean structure images
  for i := 0 to structure_images_count - 1 do
  begin
    with structure_images[i] do
    begin
      if bitmap <> nil then
        bitmap.Destroy;
      bitmap := nil;
      if bitmap_mask <> nil then
        bitmap_mask.Destroy;
      bitmap_mask := nil;
      bitmap_data := nil;
    end;
  end;
  SetLength(structure_images, 0);
  structure_images_count := 0;
  SetLength(structure_image_entry_mapping, 0);
  // Clean house color pixels
  SetLength(house_color_pixel_indexes, 0);
  SetLength(house_color_pixel_shades, 0);
  house_color_pixel_count_total := 0;
end;

procedure TStructures.clear_last_structure_image(entry_index: integer; is_stealth: boolean);
begin
  if structure_images_count = 0 then
    exit;
  dec(structure_images_count);
  with structure_images[structure_images_count] do
  begin
    if bitmap <> nil then
      bitmap.Destroy;
    bitmap := nil;
    if bitmap_mask <> nil then
      bitmap_mask.Destroy;
    bitmap_mask := nil;
    bitmap_data := nil;
    dec(house_color_pixel_count_total, house_color_pixel_count);
  end;
  if not is_stealth then
    structure_image_entry_mapping[entry_index].normal_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED
  else
    structure_image_entry_mapping[entry_index].stealth_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
end;

procedure TStructures.load_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean);
var
  already_loaded: boolean;
  header: TR16EntryHeaderPtr;
  image_data_8bpp: TByteArrayPtr;
  image_data_16bpp: TWordArrayPtr;
  palette_origin_index: integer;
  palette_origin_header: TR16EntryHeaderPtr;
  palette: TR16PalettePtr;
  width, height, padded_width: Integer;
  tmp_bitmap, tmp_bitmap_mask: TBitmap;
  tmp_bitmap_data: TWordArrayPtr;
  tmp_offset_x, tmp_offset_y: integer;
  tmp_house_color_pixel_count: integer;
  x, y, image_pos, bitmap_pos: integer;
  pixel: byte;
begin
  // Check if this entry is already loaded
  if not is_stealth then
    already_loaded := structure_image_entry_mapping[entry_index].normal_index <> STRUCTURE_IMAGE_ENTRY_NOT_LOADED
  else
    already_loaded := structure_image_entry_mapping[entry_index].stealth_index <> STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
  if already_loaded then
    exit;
  // Check if this entry is empty
  if data_r16_file_contents[data_r16_file_entry_positions[entry_index]] = 0 then
  begin
    // Empty entry
    if not is_stealth then
      structure_image_entry_mapping[entry_index].normal_index := STRUCTURE_IMAGE_ENTRY_EMPTY
    else
      structure_image_entry_mapping[entry_index].stealth_index := STRUCTURE_IMAGE_ENTRY_EMPTY;
    exit;
  end;
  // Load header and dimensions
  header := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index]]);
  image_data_8bpp := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index] + sizeof(TR16EntryHeader)]);
  image_data_16bpp := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index] + sizeof(TR16EntryHeader)]);
  width := header.ImageWidth;
  height := header.ImageHeight;
  padded_width := width + (width mod 2);
  // Get palette (go back to entry with referenced palette if needed)
  palette_origin_index := entry_index;
  while data_r16_file_contents[data_r16_file_entry_positions[palette_origin_index]] <> 1 do
    dec(palette_origin_index);
  palette_origin_header := Addr(data_r16_file_contents[data_r16_file_entry_positions[palette_origin_index]]);
  palette := Addr(data_r16_file_contents[data_r16_file_entry_positions[palette_origin_index] + sizeof(TR16EntryHeader) + palette_origin_header.ImageWidth * palette_origin_header.ImageHeight]);
  // Create bitmap for internal image storage
  tmp_bitmap := TBitmap.Create;
  tmp_bitmap.Width := width;
  tmp_bitmap.Height := height;
  tmp_bitmap.PixelFormat := pf15bit;
  tmp_bitmap_data := tmp_bitmap.ScanLine[height - 1];
  // Load image pixels
  tmp_house_color_pixel_count := 0;
  image_pos := 0;
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
    begin
      bitmap_pos := (height - y - 1) * padded_width + x;
      if header.BitsPerPixel = 8 then
      begin
        pixel := image_data_8bpp[image_pos];
        // Elliminate shadows
        if pixel = 1 then
          pixel := 0;
        // If unit is stealth then whole shape will be drawn in house color
        if is_stealth and (pixel <> 0) then
          pixel := 248;
        // House colox pixel
        if pixel >= 240 then
        begin
          dec(pixel, 240);
          // Reallocate house_color_pixel_indexes and house_color_pixel_shades capacity if needed
          if (house_color_pixel_count_total + tmp_house_color_pixel_count) = Length(house_color_pixel_indexes) then
          begin
            SetLength(house_color_pixel_indexes, Length(house_color_pixel_indexes) + HOUSE_COLOR_PIXEL_ALLOC_STEP);
            SetLength(house_color_pixel_shades, Length(house_color_pixel_shades) + HOUSE_COLOR_PIXEL_ALLOC_STEP);
          end;
          house_color_pixel_indexes[house_color_pixel_count_total + tmp_house_color_pixel_count] := bitmap_pos;
          house_color_pixel_shades[house_color_pixel_count_total + tmp_house_color_pixel_count] := pixel;
          inc(tmp_house_color_pixel_count);
          tmp_bitmap_data[bitmap_pos] := colours[house_index, pixel];
        end else
          tmp_bitmap_data[bitmap_pos] := palette.colors[pixel];
      end else
        tmp_bitmap_data[bitmap_pos] := image_data_16bpp[image_pos];
      inc(image_pos);
    end;
  // Create mask from image bitmap
  tmp_bitmap_mask := TBitmap.Create;
  tmp_bitmap_mask.Assign(tmp_bitmap);
  tmp_bitmap_mask.Mask(clFuchsia);
  // Change transparent image pixels to black color
  if header.BitsPerPixel = 8 then
  begin
    image_pos := 0;
    for y := 0 to height - 1 do
      for x := 0 to width - 1 do
      begin
        if image_data_8bpp[image_pos] <= 1 then
          tmp_bitmap_data[(height - y - 1) * padded_width + x] := 0;
        inc(image_pos);
      end;
  end;
  // Compute offsets
  if not is_unit then
  begin
    tmp_offset_x := header.ImageOffsetX * -1
  end else
    tmp_offset_x := (header.FrameWidth div 2 - header.ImageOffsetX) - (header.FrameWidth - 32) div 2;
  if not is_unit then
  begin
    tmp_offset_y := header.ImageOffsetY;
  end else
    tmp_offset_y := (header.FrameHeight div 2 - header.ImageOffsetY) - (header.FrameHeight - 32) div 2;
  // Save structure image
  if structure_images_count = Length(structure_images) then
    SetLength(structure_images, Length(structure_images) + STRUCTURE_IMAGES_ALLOC_STEP);
  with structure_images[structure_images_count] do
  begin
    bitmap := tmp_bitmap;
    bitmap_mask := tmp_bitmap_mask;
    bitmap_data := tmp_bitmap_data;
    offset_x := tmp_offset_x;
    offset_y := tmp_offset_y;
    house_color_pixel_first_index := house_color_pixel_count_total;
    house_color_pixel_count := tmp_house_color_pixel_count;
    current_house_color := house_index;
  end;
  inc(house_color_pixel_count_total, tmp_house_color_pixel_count);
  if not is_stealth then
    structure_image_entry_mapping[entry_index].normal_index := structure_images_count
  else
    structure_image_entry_mapping[entry_index].stealth_index := structure_images_count;
  inc(structure_images_count);
end;

procedure TStructures.recolor_structure_image(image_index, house_index: integer);
var
  i: integer;
begin
  with structure_images[image_index] do
  begin
    if current_house_color = house_index then
      exit;
    for i := 0 to house_color_pixel_count - 1 do
      bitmap_data[house_color_pixel_indexes[house_color_pixel_first_index + i]] := colours[house_index, house_color_pixel_shades[house_color_pixel_first_index + i]];
    current_house_color := house_index;
  end;
end;

function TStructures.get_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean; var was_already_loaded: boolean): TStructureImagePtr;
var
  image_index: integer;
begin
  was_already_loaded := false;
  result := nil;
  if data_r16_filename = '' then
    exit;
  if entry_index >= data_r16_file_entry_count then
    exit;
  // Check if image is already preloaded
  if not is_stealth then
    image_index := structure_image_entry_mapping[entry_index].normal_index
  else
    image_index := structure_image_entry_mapping[entry_index].stealth_index;
  // Image is already preloaded, but is empty
  if image_index = STRUCTURE_IMAGE_ENTRY_EMPTY then
    exit;
  // Image is already preloaded, recolor it and return it
  if image_index <> STRUCTURE_IMAGE_ENTRY_NOT_LOADED then
  begin
    recolor_structure_image(image_index, house_index);
    was_already_loaded := true;
    result := Addr(structure_images[image_index]);
    exit;
  end;
  // Image is not yet preloaded, load it now
  load_structure_image(entry_index, house_index, is_unit, is_stealth);
  if not is_stealth then
    image_index := structure_image_entry_mapping[entry_index].normal_index
  else
    image_index := structure_image_entry_mapping[entry_index].stealth_index;
  // Image is empty
  if image_index = STRUCTURE_IMAGE_ENTRY_EMPTY then
    exit;
  // Image is loaded now
  result := Addr(structure_images[image_index]);
end;

function TStructures.get_structure_image_header(entry_index: integer): TR16EntryHeaderPtr;
begin
  if entry_index < data_r16_file_entry_count then
    result := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index]])
  else
    result := nil;
end;

procedure TStructures.load_mis_ai_properties_ini;
var
  tmp_filename: String;
  i: integer;
  ini: TMemIniFile;
  tmp_strings: TStringList;
begin
  tmp_filename := find_file('config\mis_ai_properties.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load misai properties from ini file
  tmp_strings := TStringList.Create;
  ini := TMemIniFile.Create(tmp_filename);
  ini.ReadSection('AI',tmp_strings);
  SetLength(mis_ai_properties_template, tmp_strings.Count);
  SetLength(mis_ai_properties, tmp_strings.Count);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    mis_ai_properties_template[i].name := ini.ReadString('AI',tmp_strings[i],'');
    mis_ai_properties_template[i].data_type := tmp_strings[i][1];
    mis_ai_properties_template[i].position := strtoint(Copy(tmp_strings[i], 3, Length(tmp_strings[i]) - 2));
  end;
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TStructures.cache_mis_ai_properties;
var
  i, position, num: integer;
  name: String;
begin
  cnt_mis_ai_properties := 0;
  for i := 0 to Length(mis_ai_properties_template) - 1 do
  begin
    name := mis_ai_properties_template[i].name;
    // Replace building name
    position := Pos('B#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+2, 2), 100);
      if num < templates.BuildingTypeCount then
        name := Copy(name, 0, position-1) + templates.BuildingTypeStrings[num]
      else
        continue;
    end;
    // Replace building2 name
    position := Pos('B2#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+3, 2), 100);
      if num < templates.BuildingCount then
        name := Copy(name, 0, position-1) + prettify_structure_name(templates.BuildingNameStrings[num])
      else
        continue;
    end;
    // Replace unit name
    position := Pos('U#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+2, 2), 60);
      if num < templates.UnitCount then
        name := Copy(name, 0, position-1) + templates.UnitNameStrings[num]
      else
        continue;
    end;
    mis_ai_properties[cnt_mis_ai_properties].name := name;
    mis_ai_properties[cnt_mis_ai_properties].data_type := mis_ai_properties_template[i].data_type;
    mis_ai_properties[cnt_mis_ai_properties].position := mis_ai_properties_template[i].position;
    inc(cnt_mis_ai_properties);
  end;
end;

procedure TStructures.load_graphics_misc_objects;
var
  tmp_filename: String;
begin
  tmp_filename := find_file('graphics\misc_objects.bmp', 'graphics');
  if (tmp_filename = '') or (tmp_filename = graphics_misc_objects_filename) then
    exit;
  graphics_misc_objects_filename := tmp_filename;
  // Load graphics from files
  graphics_misc_objects.LoadFromFile(tmp_filename);
  // Create mask
  graphics_misc_objects_mask.Assign(graphics_misc_objects);
  graphics_misc_objects_mask.Mask(clBlack);
  // Register event in dispatcher
  Dispatcher.register_event(evFLMiscObjectsBmp);
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
      color := ini.ReadInteger(sname, 'color', $0);
      stats_group := ini.ReadInteger(sname, 'stats_group', 0);
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
      tiledata[value].index := i;
      tiledata[value].player := 0;
      tiledata[value].stype := ST_MISC_OBJECT;
    end;
  end;
end;

procedure TStructures.load_players_ini;
var
  tmp_filename, tmp_filename2: String;
  ini: TMemIniFile;
  i: integer;
begin
  // Step 1 - editor's internal file
  tmp_filename := current_dir + 'config\players.ini';
  // Step 2 - file under CustomCampaignData folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\' + MissionDialog.cbModsFolder.Text + '\config\players.ini';
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Step 3 - file under Players folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\Players\' + MissionDialog.cbPlayersIni.Text;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file config\players.ini', 'Error loading configuration file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // This file is already loaded - do not load it again
  if tmp_filename = players_ini_filename then
    exit;
  players_ini_filename := tmp_filename;
  // Read list of players
  ini := TMemIniFile.Create(tmp_filename);
  for i := 0 to cnt_players-1 do
  begin
    player_info[i].name := ini.ReadString('Player'+inttostr(i), 'name', 'Unnamed');
    player_info[i].shortname := ini.ReadString('Player'+inttostr(i), 'short', player_info[i].name);
  end;
  ini.Destroy;
  // Register event in dispatcher
  Dispatcher.register_event(evFLPlayersIni);
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
  limit_spice_blooms := ini.ReadInteger('Limits', 'spice_blooms', 30);
  limit_structures_total := ini.ReadInteger('Limits', 'structures_total', 1000);
  limit_refineries_per_player := ini.ReadInteger('Limits', 'refineries_per_player', 10);
  ini.Destroy;
end;

procedure TStructures.load_templates_other_txt;
var
  tmp_filename: String;
  i: integer;
  prefix: string;
begin
  tmp_filename := find_file('config\templates_other.txt', 'configuration');
  if (tmp_filename = '') or (tmp_filename = templates_other_txt_filename) then
    exit;
  templates_other_txt_filename := tmp_filename;
  templates_other.LoadFromFile(tmp_filename);
  for i := 0 to Length(templates_other_byte_types) - 1 do
  begin
    prefix := Copy(templates_other[i], 1, 2);
    if prefix = 'B ' then
      templates_other_byte_types[i] := tobtBuilding
    else if prefix = 'U ' then
      templates_other_byte_types[i] := tobtUnit
    else if prefix = 'W ' then
      templates_other_byte_types[i] := tobtWeapon
    else if prefix = 'E ' then
      templates_other_byte_types[i] := tobtExplosion
    else
      templates_other_byte_types[i] := tobtNone
  end;
end;

end.

