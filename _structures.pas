unit _structures;

interface

uses Windows, Classes, SysUtils, Math, Types, _utils;

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
// Templates other definitions
// *****************************************************************************

type
  TTemplatesOtherByteType = (tobtNone, tobtBuilding, tobtUnit, tobtWeapon, tobtExplosion);

// *****************************************************************************
// Auxiliary constants and types
// *****************************************************************************

const MAX_BUILDING_SIZE = 4;

type
  TItemNameList = array[0..0, 0..49] of char;
  TItemNameListPtr = ^TItemNameList;

// *****************************************************************************
// Data Export / Import structures
// *****************************************************************************

type
  TBuildingExportData = record
    building_name:             array[0..449] of char;
    building_template:         TBuildingTemplate;
    builexp_entry:             TBuilExpEntry;
    buildup_art_frames:        integer;
    ref_building_type:         array[0..49] of char;
    ref_prereq1_building_type: array[0..49] of char;
    ref_prereq2_building_type: array[0..49] of char;
    ref_armour_type:           array[0..49] of char;
    ref_primary_weapon:        array[0..49] of char;
    ref_secondary_weapon:      array[0..49] of char;
    ref_death_explosion:       array[0..49] of char;
    ref_firing_explosion:      array[0..49] of char;
    icon_data:                 array[0..3368] of byte;
  end;

  TBuildingExportDataPtr = ^TBuildingExportData;

type
  TUnitExportData = record
    unit_name:                 array[0..449] of char;
    unit_template:             TUnitTemplate;
    ref_unit_type:             array[0..49] of char;
    ref_prereq1_building_type: array[0..49] of char;
    ref_prereq2_building_type: array[0..49] of char;
    ref_armour_type:           array[0..49] of char;
    ref_primary_weapon:        array[0..49] of char;
    ref_secondary_weapon:      array[0..49] of char;
    ref_death_explosion:       array[0..49] of char;
    ref_firing_explosion:      array[0..49] of char;
    icon_data:                 array[0..3368] of byte;
  end;

  TUnitExportDataPtr = ^TUnitExportData;

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

    // Misc. objects related data
    misc_object_info: array of TMiscObjectInfo;
    cnt_misc_objects: integer;

    // Player related data
    player_names: array[0..CNT_PLAYERS-1] of string;
    player_names_short: array[0..CNT_PLAYERS-1] of string;

    // Limits related data
    limit_spice_blooms: integer;
    limit_structures_total: integer;
    limit_refineries_per_player: integer;

    // Templates other related data
    templates_other: TStringList;
    templates_other_byte_types: array[0..85] of TTemplatesOtherByteType;

    // Clipboard formats
    clipboard_format_building: cardinal;
    clipboard_format_unit: cardinal;
  public
    // General procedures
    procedure init;

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
    // Misc. objects related procedures
    procedure load_misc_objects_ini;
    procedure register_misc_objects_in_tiledata;
    // Players related procedures
    procedure load_players_ini;
    // Limits related procedures
    procedure load_limits_ini;
    // Templates other related procedures
    procedure load_templates_other_txt;

    // Data Export/Import, Copy/Paste and other manipulation procedures
    procedure store_item_reference(var data; item_index: integer; item_list: TItemNameListPtr);

    procedure store_building_data(index: integer; data: TBuildingExportDataPtr);
    procedure restore_building_data(index: integer; data: TBuildingExportDataPtr);
    procedure copy_building(index: integer);
    function paste_building(index: integer): boolean;
    procedure export_building(index: integer; filename: string);
    procedure import_building(index: integer; filename: string);
    procedure swap_buildings(index1, index2: integer);

    procedure store_unit_data(index: integer; data: TUnitExportDataPtr);
    procedure restore_unit_data(index: integer; data: TUnitExportDataPtr);
    procedure copy_unit(index: integer);
    function paste_unit(index: integer): boolean;
    procedure export_unit(index: integer; filename: string);
    procedure import_unit(index: integer; filename: string);
    procedure swap_units(index1, index2: integer);
  end;

var
  Structures: TStructures;

implementation

uses Forms, Clipbrd, IniFiles, _settings, _mission, _missionini, _graphics, _stringtable, _dispatcher;

procedure TStructures.init;
begin
  templates_other := TStringList.Create;
  load_templates_bin(false);
  load_builexp_bin(false);
  load_armour_bin(false);
  load_speed_bin(false);
  load_techpos_bin(false);
  load_tiledata_bin;
  load_misc_objects_ini;
  load_players_ini;
  load_limits_ini;
  load_templates_other_txt;
  clipboard_format_building := RegisterClipboardFormat('D2kEditorBuildingExportData');
  clipboard_format_unit := RegisterClipboardFormat('D2kEditorUnitExportData');
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
  if tmp_filename = players_ini_filename then
    exit;
  players_ini_filename := tmp_filename;
  // Read list of players
  ini := TMemIniFile.Create(tmp_filename);
  for i := 0 to cnt_players-1 do
  begin
    player_names[i] := ini.ReadString('Player'+inttostr(i), 'name', 'Unnamed');
    player_names_short[i] := ini.ReadString('Player'+inttostr(i), 'short', player_names[i]);
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

procedure TStructures.store_item_reference(var data; item_index: integer; item_list: TItemNameListPtr);
begin
  if item_index = -1 then
    FillChar(data, 50, 0)
  else
    Move(item_list[item_index], data, 50);
end;

procedure TStructures.store_building_data(index: integer; data: TBuildingExportDataPtr);
var
  icon_ptr: TR16EntryHeaderPtr;
begin
  // Actual building data
  Move(templates.BuildingNameStrings[index], data.building_name, Length(data.building_name));
  Move(templates.BuildingDefinitions[index], data.building_template, sizeof(TBuildingTemplate));
  Move(builexp[index], data.builexp_entry, sizeof(TBuilExpEntry));
  // Buildup art frames
  data.buildup_art_frames := templates.BuildupArtFrames[index];
  // References
  store_item_reference(data.ref_building_type, data.building_template.BuildingType, Addr(templates.BuildingTypeStrings));
  store_item_reference(data.ref_prereq1_building_type, data.building_template.Prereq1BuildingType, Addr(templates.BuildingTypeStrings));
  store_item_reference(data.ref_prereq2_building_type, data.building_template.Prereq2BuildingType, Addr(templates.BuildingTypeStrings));
  store_item_reference(data.ref_armour_type, data.building_template.ArmorType, Addr(armour.ArmourTypeStrings));
  store_item_reference(data.ref_primary_weapon, data.building_template.PrimaryWeapon, Addr(templates.WeaponStrings));
  store_item_reference(data.ref_secondary_weapon, data.building_template.SecondaryWeapon, Addr(templates.WeaponStrings));
  store_item_reference(data.ref_death_explosion, data.building_template.DeathExplosion, Addr(templates.ExplosionStrings));
  store_item_reference(data.ref_firing_explosion, data.building_template.FiringExplosion, Addr(templates.ExplosionStrings));
  // Building icon
  icon_ptr := StructGraphics.get_structure_image_header(first_building_icon_image_index + index);
  if icon_ptr.EntryType <> 0 then
    Move(icon_ptr.EntryType, data.icon_data, sizeof(data.icon_data))
  else
    FillChar(data.icon_data, sizeof(data.icon_data), 0);
end;

procedure TStructures.restore_building_data(index: integer; data: TBuildingExportDataPtr);
begin
  // Actual building data
  Move(data.building_name, templates.BuildingNameStrings[index], Length(data.building_name));
  Move(data.building_template, templates.BuildingDefinitions[index], sizeof(TBuildingTemplate));
  Move(data.builexp_entry, builexp[index], sizeof(TBuilExpEntry));
  // Buildup art frames
  if templates.BuildupArtFrames[index] <> data.buildup_art_frames then
  begin
    StructGraphics.change_image_entry_count(buildup_art_image_indexes[index], templates.BuildupArtFrames[index], data.buildup_art_frames);
    templates.BuildupArtFrames[index] := data.buildup_art_frames;
    compute_image_indexes;
  end;
  // Building icon
  if data.icon_data[0] <> 0 then
    StructGraphics.modify_image_data(first_building_icon_image_index + index, data.icon_data, sizeof(data.icon_data))
  else
    StructGraphics.modify_image_data(first_building_icon_image_index + index, data.icon_data, 1)
end;

procedure TStructures.copy_building(index: integer);
var
  handle: THandle;
  pointer: TBuildingExportDataPtr;
begin
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TBuildingExportData));
  pointer := GlobalLock(handle);

  store_building_data(index, pointer);

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format_building, handle);
  CloseClipboard;
end;

function TStructures.paste_building(index: integer): boolean;
var
  handle: THandle;
  pointer: TBuildingExportDataPtr;
begin
  result := false;
  if not Clipboard.HasFormat(clipboard_format_building) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format_building);
  pointer := GlobalLock(handle);

  restore_building_data(index, pointer);

  GlobalUnLock(handle);
  CloseClipboard;
  result := true;
end;

procedure TStructures.export_building(index: integer; filename: string);
var
  building_data: TBuildingExportData;
begin
  store_building_data(index, Addr(building_data));
  save_binary_file(filename, building_data, sizeof(building_data));
end;

procedure TStructures.import_building(index: integer; filename: string);
var
  building_data: TBuildingExportData;
begin
  load_binary_file(filename, building_data, sizeof(building_data));
  restore_building_data(index, Addr(building_data));
end;

procedure TStructures.swap_buildings(index1, index2: integer);
var
  tmp_building_name: array[0..449] of char;
  tmp_building_template: TBuildingTemplate;
  tmp_builexp_entry: TBuilExpEntry;
  tmp_buildup_art_frames: integer;
  tmp_building_animation_frames: integer;
  i: integer;
begin
  // Swap building data
  move(templates.BuildingNameStrings[index1,0], tmp_building_name[0], Length(tmp_building_name));
  move(templates.BuildingNameStrings[index2,0], templates.BuildingNameStrings[index1,0], Length(tmp_building_name));
  move(tmp_building_name[0], templates.BuildingNameStrings[index2,0], Length(tmp_building_name));
  tmp_building_template := templates.BuildingDefinitions[index1];
  templates.BuildingDefinitions[index1] := templates.BuildingDefinitions[index2];
  templates.BuildingDefinitions[index2] := tmp_building_template;
  tmp_builexp_entry := builexp[index1];
  builexp[index1] := builexp[index2];
  builexp[index2] := tmp_builexp_entry;
  StructGraphics.swap_image_entries(first_building_icon_image_index + index1, first_building_icon_image_index + index2, 1, 1);
  // Swap building animations
  StructGraphics.swap_image_entries(building_animation_image_indexes[index1], building_animation_image_indexes[index2], templates.BuildingAnimationFrames[index1], templates.BuildingAnimationFrames[index2]);
  tmp_building_animation_frames := templates.BuildingAnimationFrames[index1];
  templates.BuildingAnimationFrames[index1] := templates.BuildingAnimationFrames[index2];
  templates.BuildingAnimationFrames[index2] := tmp_building_animation_frames;
  for i := 0 to templates.BuildingCount - 1 do
  begin
    if (templates.BuildingDefinitions[i].Flags and BF_HAS_ANIMATION) = 0 then
      continue;
    if templates.BuildingDefinitions[i].BuildingAnimation = index1 then
      templates.BuildingDefinitions[i].BuildingAnimation := index2
    else if templates.BuildingDefinitions[i].BuildingAnimation = index2 then
      templates.BuildingDefinitions[i].BuildingAnimation := index1;
  end;
  // Swap buildup animations
  StructGraphics.swap_image_entries(buildup_art_image_indexes[index1], buildup_art_image_indexes[index2], templates.BuildupArtFrames[index1], templates.BuildupArtFrames[index2]);
  tmp_buildup_art_frames := templates.BuildupArtFrames[index1];
  templates.BuildupArtFrames[index1] := templates.BuildupArtFrames[index2];
  templates.BuildupArtFrames[index2] := tmp_buildup_art_frames;
  for i := 0 to templates.BuildingCount - 1 do
  begin
    if templates.BuildupArtFrames[i] = 0 then
      continue;
    if templates.BuildingDefinitions[i].BuildupArt = index1 then
      templates.BuildingDefinitions[i].BuildupArt := index2
    else if templates.BuildingDefinitions[i].BuildupArt = index2 then
      templates.BuildingDefinitions[i].BuildupArt := index1;
  end;
  compute_image_indexes;
end;

procedure TStructures.store_unit_data(index: integer; data: TUnitExportDataPtr);
var
  icon_ptr: TR16EntryHeaderPtr;
begin
  // Actual unit data
  Move(templates.UnitNameStrings[index], data.unit_name, Length(data.unit_name));
  Move(templates.UnitDefinitions[index], data.unit_template, sizeof(TUnitTemplate));
  // References
  store_item_reference(data.ref_unit_type, data.unit_template.UnitType, Addr(templates.UnitTypeStrings));
  store_item_reference(data.ref_prereq1_building_type, data.unit_template.Prereq1BuildingType, Addr(templates.BuildingTypeStrings));
  store_item_reference(data.ref_prereq2_building_type, data.unit_template.Prereq2BuildingType, Addr(templates.BuildingTypeStrings));
  store_item_reference(data.ref_armour_type, data.unit_template.ArmorType, Addr(armour.ArmourTypeStrings));
  store_item_reference(data.ref_primary_weapon, data.unit_template.PrimaryWeapon, Addr(templates.WeaponStrings));
  store_item_reference(data.ref_secondary_weapon, data.unit_template.SecondaryWeapon, Addr(templates.WeaponStrings));
  store_item_reference(data.ref_death_explosion, data.unit_template.DeathExplosion, Addr(templates.ExplosionStrings));
  store_item_reference(data.ref_firing_explosion, data.unit_template.FiringExplosion, Addr(templates.ExplosionStrings));
  // Unit icon
  icon_ptr := StructGraphics.get_structure_image_header(first_unit_icon_image_index + index);
  if icon_ptr.EntryType <> 0 then
    Move(icon_ptr.EntryType, data.icon_data, sizeof(data.icon_data))
  else
    FillChar(data.icon_data, sizeof(data.icon_data), 0);
end;

procedure TStructures.restore_unit_data(index: integer; data: TUnitExportDataPtr);
begin
  // Actual unit data
  Move(data.unit_name, templates.UnitNameStrings[index], Length(data.unit_name));
  Move(data.unit_template, templates.UnitDefinitions[index], sizeof(TUnitTemplate));
  // Unit icon
  if data.icon_data[0] <> 0 then
    StructGraphics.modify_image_data(first_unit_icon_image_index + index, data.icon_data, sizeof(data.icon_data))
  else
    StructGraphics.modify_image_data(first_unit_icon_image_index + index, data.icon_data, 1);
end;

procedure TStructures.copy_unit(index: integer);
var
  handle: THandle;
  pointer: TUnitExportDataPtr;
begin
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TUnitExportData));
  pointer := GlobalLock(handle);

  store_unit_data(index, pointer);

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format_unit, handle);
  CloseClipboard;
end;

function TStructures.paste_unit(index: integer): boolean;
var
  handle: THandle;
  pointer: TUnitExportDataPtr;
begin
  result := false;
  if not Clipboard.HasFormat(clipboard_format_unit) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format_unit);
  pointer := GlobalLock(handle);

  restore_unit_data(index, pointer);

  GlobalUnLock(handle);
  CloseClipboard;
  result := true;
end;

procedure TStructures.export_unit(index: integer; filename: string);
var
  unit_data: TUnitExportData;
begin
  store_unit_data(index, Addr(unit_data));
  save_binary_file(filename, unit_data, sizeof(unit_data));
end;

procedure TStructures.import_unit(index: integer; filename: string);
var
  unit_data: TUnitExportData;
begin
  load_binary_file(filename, unit_data, sizeof(unit_data));
  restore_unit_data(index, Addr(unit_data));
end;

procedure TStructures.swap_units(index1, index2: integer);
var
  tmp_unit_name: array[0..449] of char;
  tmp_unit_template: TUnitTemplate;
  i: integer;
begin
  // Swap unit data
  move(templates.UnitNameStrings[index1,0], tmp_unit_name[0], Length(tmp_unit_name));
  move(templates.UnitNameStrings[index2,0], templates.UnitNameStrings[index1,0], Length(tmp_unit_name));
  move(tmp_unit_name[0], templates.UnitNameStrings[index2,0], Length(tmp_unit_name));
  tmp_unit_template := templates.UnitDefinitions[index1];
  templates.UnitDefinitions[index1] := templates.UnitDefinitions[index2];
  templates.UnitDefinitions[index2] := tmp_unit_template;
  StructGraphics.swap_image_entries(first_unit_icon_image_index + index1, first_unit_icon_image_index + index2, 1, 1);
  // Adjust templates other
  for i := 0 to Length(templates.Other) - 1 do
    if templates_other_byte_types[i] = tobtUnit then
    begin
      if templates.Other[i] = index1 then
        templates.Other[i] := index2
      else if templates.Other[i] = index2 then
        templates.Other[i] := index1;
    end;
end;

end.

