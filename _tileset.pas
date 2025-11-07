unit _tileset;

interface

uses Windows, Graphics, Classes, IniFiles, _utils;

// Tileset constants
const min_tileset_tiles = 800;
const max_tileset_tiles = 4000;
const max_minimap_color_rules = 32;
const max_fill_area_rules = 16;
const cnt_paint_tile_groups = 8;
const cnt_block_preset_groups = 8;
const max_block_presets = 1024;
const max_block_preset_tiles = 4096;
const max_connection_points = 2048;     // Used for random map generator
const cnt_connection_point_types = 16;  // Used for random map generator
const cnt_block_groups = 512;           // Used for random map generator
const max_paint_tiles = 32;

// Constants for get_block_preset function
const bpNext = -1;
const bpRandom = -2;

// Constants for block presets
const block_preset_rows = 4;
const block_preset_cols = 10;
const cnt_block_preset_keys = block_preset_rows * block_preset_cols;
const block_preset_keys: array[0..cnt_block_preset_keys-1] of char =
  (
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
    'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P',
    'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':',
    'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?'
  );

// Constants for tileset image
const tileimage_width = 640;
const r16_tile_header_size = 29;
const r16_tile_data_size = 32 * 32 * 2;
const r16_tile_total_size = r16_tile_header_size + r16_tile_data_size;
const r8_tile_header_size = 29;
const r8_tile_data_size = 32 * 32;
const r8_tile_total_size = r8_tile_header_size + r8_tile_data_size;

// Tileset attributes (game)
const taConcrete = $800;
const taVehiclesPass = $2000;
const taInfantryPass = $4000;
const taAnyPass = $6000;
const taBuildable = $8000;
const taSand = $10000;

// Version constant
const CURRENT_TILESET_CONFIG_VERSION = 1;

// Tileset type definitions
type
  TileType = (ttPassable, ttImpassable, ttInfantryOnly, ttBuildable);

type
  SmoothPresetType = (
    spStraightLeft, spStraightUp, spStraightRight, spStraightDown,
    spCornerLeftUp, spCornerUpRight, spCornerRightDown, spCornerDownLeft,
    spTurnRightDown, spTurnDownLeft, spTurnLeftUp, spTurnUpRight,
    spCurveNegLeft, spCurveNegUp, spCurveNegRight, spCurveNegDown,
    spCurvePosLeft, spCurvePosUp, spCurvePosRight, spCurvePosDown
  );

type
  TTilesetListEntry = record
    name: string;
    fancy_name: string;
    author: string;
    num_tiles: integer;
    attributes: string;
    location: string;
  end;

type
  TTilesetHeader = record
    version_major: integer;
    version_minor: integer;
    reserved1: integer;
    reserved2: integer;
    custom_minimap_colors_allowed: byte;
    default_paint_group: shortint;
    reserved3: byte;
    reserved4: byte;
    rule_do_not_draw_rock_craters: byte;
    rule_do_not_draw_sand_craters: byte;
    reserved_rules: array[0..13] of byte;
    tileset_fancy_name: array[0..31] of char;
    author_name: array[0..31] of char;
  end;

type
  TTileAtrRule = record
    attr: int64;
    not_attr: int64;
  end;
  TTileAtrRulePtr = ^TTileAtrRule;

type
  TMinimapColorRule = record
    name: array[0..31] of char;
    rule: TTileAtrRule;
    color: cardinal;
    color_8bit: byte; // Used by Dune2000 game only
    color_16bit: word; // Used by Dune2000 game only
  end;

type
  TFillAreaRule = record
    name: array[0..31] of char;
    rule: TTileAtrRule;
  end;

type
  TPaintTileGroup = record
    name: array[0..31] of char;
    tile_index: word;
    restriction_rule: TTileAtrRule;
    smooth_preset_group: shortint;
    smooth_presets: array[0..31] of char;
    random_map_name: array[0..31] of char;
  end;

type
  TBlockPresetGroup = record
    name: array[0..31] of char;
    paint_group: integer;
  end;

type
  TBlockPreset = record
    width: byte;
    height: byte;
    num_connection_points: byte;  // Used for random map generator
    block_group: word;            // Used for random map generator
  end;

  PBlockPreset = ^TBlockPreset;

type // Used for random map generator
  TConnectionPoint = record
    type_and_direction: byte;
    offset: byte;
  end;

type // Used for random map generator
  TConnectionPointType = record
    paint_group: byte;
    connection_point_width: byte;
    connection_point_height: byte;
  end;

type // Used for random map generator
  TBlockGroup = record
    repeat_distance: byte;
    absolute_weight: word;
  end;

type
  TPaintTileList = record
    cnt_tiles: integer;
    tiles: array[0..max_paint_tiles-1] of word;
  end;

// Tileset class
type
  TTileset = class

  public
    // List of available tilesets
    cnt_tilesets: integer;
    tileset_list: array of TTilesetListEntry;
    tileset_index_mapping: array of integer;

    // Status variables
    tileset_name: String;
    tileatr_name: String;
    tileimage_filename: String;
    tls_filename: String;
    tileatr_filename: String;
    ini_filename: String;

    // Tileset image
    cnt_tiles: integer;
    tileimage: TBitmap;
    tileimage_modified: boolean;
    palette_loaded: Boolean;
    palette: PLogPalette;

    // Tileset data
    header: TTilesetHeader;

    attributes: array[0..max_tileset_tiles-1] of cardinal;
    attributes_extra: array[0..max_tileset_tiles-1] of cardinal;
    tile_hint_text: array[0..max_tileset_tiles-1] of integer;
    restrictions: array[0..max_tileset_tiles-1] of cardinal;

    extra_attribute_names: array[0..7, 0..31] of char;

    minimap_color_rules_used: integer;
    minimap_color_rules: array[0..max_minimap_color_rules-1] of TMinimapColorRule;

    fill_area_rules_used: integer;
    fill_area_rules: array[0..max_fill_area_rules-1] of TFillAreaRule;

    paint_tile_groups: array[-4..cnt_paint_tile_groups-1] of TPaintTileGroup;

    block_preset_groups: array[0..cnt_block_preset_groups-1] of TBlockPresetGroup;
    block_preset_key_variants: array[0..cnt_block_preset_groups-1, 0..cnt_block_preset_keys-1] of byte;
    block_presets: array[0..max_block_presets-1] of TBlockPreset;
    block_preset_tiles: array[0..max_block_preset_tiles-1] of word;

    connection_points: array[0..max_connection_points-1] of TConnectionPoint;               // Used for random map generator
    connection_point_types: array[0..cnt_connection_point_types-1] of TConnectionPointType; // Used for random map generator
    block_groups: array[0..cnt_block_groups] of TBlockGroup;                                // Used for random map generator

    // Internal editor data
    paint_tile_lists: array[0..cnt_paint_tile_groups-1] of TPaintTileList;
    paint_tile_random_maps: array[0..cnt_paint_tile_groups-1] of array of word;
    block_presets_used: integer;
    block_preset_tiles_used: integer;
    block_preset_key_variant_first_preset_indexes: array[0..cnt_block_preset_groups-1, 0..cnt_block_preset_keys-1] of word;
    block_preset_first_tile_indexes: array[0..max_block_presets-1] of word;
    connection_points_used: integer;                                                    // Used for random map generator
    block_preset_first_connection_point_indexes: array[0..max_block_presets-1] of word; // Used for random map generator

  private
    last_block_preset_variant: integer;
    last_block_preset_group: integer;
    last_block_preset_key: integer;

  public
    procedure init;
    // Load tileset list
    procedure load_tileset_list;
    // New tileset
    procedure new_tileset(p_tileset_name: String);
    // Change tileset
    procedure change_tileset_by_index(index: integer);
    procedure change_tileset_by_name(p_tileset_name, p_tileatr_name: String);
    procedure change_tileset_to_default;
    procedure change_tileset_next;
    // Load/save tileset
    procedure load_tileset(force_reload: boolean);
    procedure save_tileset;
    // Load/save tileset image
    procedure load_tileimage(p_tileset_name: string; force: boolean);
    procedure load_r16_image(filename: String);
    procedure load_r8_image(filename: String);
    procedure load_palette;
    procedure save_tileimage;
    procedure save_r16_image(filename: String);
    // Load/save tls
    function load_tls(p_tileset_name: string; force: boolean): boolean;
    procedure save_tls;
    // Load tile attributes
    procedure load_tileatr(p_tileatr_name: string; force: boolean);
    // Load ini configuration
    procedure load_ini(p_tileset_name: string; force: boolean);
    // Process internal data
    procedure process_internal_data;
    procedure process_paint_tile_lists;
    procedure process_paint_tile_random_maps;
    procedure process_block_presets;
    // Editing tileset image
    procedure import_tileimage_from_file(filename: String);
    procedure export_tileimage_to_file(filename: String);
    procedure import_tileimage_portion_from_file(filename: String; pos_x, pos_y: integer);
    procedure export_tileimage_portion_to_file(filename: String; pos_x, pos_y, size_x, size_y: integer);
    procedure copy_tileimage_portion(src_x, src_y, dest_x, dest_y, size_x, size_y: integer);
    procedure swap_tileimage_portion(src_x, src_y, dest_x, dest_y, size_x, size_y: integer);
    procedure erase_tileimage_portion(pos_x, pos_y, size_x, size_y: integer);
    procedure resize_tileset(new_cnt_tiles: integer);
    // Editing tile attributes
    procedure set_tile_attributes(tile: word; value: Int64);
    // Editing block presets
    procedure add_block_preset(group: integer; key_index: integer; width, height: integer; tiles: array of word);
    procedure delete_block_preset(preset_index: integer);
    // Miscellaneous functions
    function get_paint_tile_group_char(group: integer): char;
    function get_tile_attributes(tile, special: word; use_internal_attributes: boolean): Int64;
    function get_tile_type(tile: word): TileType;
    function get_tile_color(tile, special: word; var rule_index: integer): Cardinal;
    function get_fill_area_type(tile: word; special: word): integer;
    function get_tile_paint_group(tile: word): integer;
    function check_area_type(tile, special: word; area_type: integer): boolean;
    function check_paint_tile_restriction(tile, special: word; paint_tile_group: integer): boolean;
    function evaluate_rule(attr_value: int64; rule_ptr: TTileAtrRulePtr): boolean;
    function get_random_paint_tile(group, x, y: integer): integer;
    function get_block_preset_index(group: integer; key_index: integer; variant: integer): integer;
    function string_to_rule(rule: String; rule_ptr: TTileAtrRulePtr): boolean;
    function rule_to_string(rule_ptr: TTileAtrRulePtr): string;
  end;

var
  Tileset: TTileset;

implementation

uses Forms, SysUtils, StrUtils, Math, _map, _settings, _graphics, _dispatcher, _missionini, pngimage;

procedure TTileset.init;
begin
  // Initialize tileset image
  cnt_tiles := min_tileset_tiles;
  tileimage := Graphics.TBitmap.Create;
  tileimage_modified := false;
  palette_loaded := false;
  // Initialize empty block preset
  block_presets[0].width := 0;
  block_presets[0].height := 0;
  // Load tileset list
  load_tileset_list;
end;

procedure TTileset.load_tileset_list;
var
  SR: TSearchRec;
  tmp_strings: TStringList;
  tmp_inifile: TIniFile;
  tileimage_location: string;
  i: integer;
  tileset_file: file of byte;
  f: file of byte;
  header: TTilesetHeader;
  tileset_fancy_name_mapping: TStringList;
begin
  tmp_strings := TStringList.Create;
  tileset_fancy_name_mapping := TStringList.Create;
  // Find tileset INI files in editor folder
  if FindFirst(current_dir + 'tilesets\*.ini', 0, SR) = 0 then
  begin
    repeat
      if SR.Name <> 'template.ini' then
        tmp_strings.Values[ChangeFileExt(SR.Name, '')] := 'INI';
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  // Find tileset TLS files in game folder
  if FindFirst(Settings.GamePath + '\Data\Tilesets\*.TLS', 0, SR) = 0 then
  begin
    repeat
      tmp_strings.Values[ChangeFileExt(SR.Name, '')] := 'TLS';
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  // Find tileset TLS files in CustomCampaignData folder
  if (MissionIni.CampaignFolder <> '') and (MissionIni.ModsFolder <> '') and (FindFirst(Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\' + MissionIni.ModsFolder + '\Data\Tilesets\*.TLS', 0, SR) = 0) then
  begin
    repeat
      tmp_strings.Values[ChangeFileExt(SR.Name, '')] := 'TLS Mod';
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  // Initialize variables
  cnt_tilesets := tmp_strings.Count;
  SetLength(tileset_list, cnt_tilesets);
  SetLength(tileset_index_mapping, cnt_tilesets);
  // Process tilesets
  for i := 0 to cnt_tilesets -1 do
  begin
    tileset_list[i].name := tmp_strings.Names[i];
    tileset_list[i].location := tmp_strings.ValueFromIndex[i];
    tileimage_location := '';
    if tileset_list[i].location = 'INI' then
    begin
      tmp_inifile := TIniFile.Create(current_dir + '\tilesets\' + tileset_list[i].name + '.ini');
      tileimage_location := Settings.GamePath + '\Data';
      tileset_list[i].fancy_name := tmp_inifile.ReadString('Basic', 'name', tileset_list[i].name);
      tileset_list[i].author :=     tmp_inifile.ReadString('Basic', 'author', '');
      tileset_list[i].num_tiles :=  0;
      tileset_list[i].attributes := tmp_inifile.ReadString('Basic', 'tileatr', '');
      tmp_inifile.Destroy;
    end
    else if tileset_list[i].location = 'TLS' then
    begin
      AssignFile(tileset_file, Settings.GamePath + '\Data\Tilesets\' + tileset_list[i].name + '.TLS');
      Reset(tileset_file);
      BlockRead(tileset_file, header, sizeof(header));
      CloseFile(tileset_file);
      tileimage_location := Settings.GamePath + '\Data';
      tileset_list[i].fancy_name := header.tileset_fancy_name;
      tileset_list[i].author :=     header.author_name;
      tileset_list[i].num_tiles :=  0;
      tileset_list[i].attributes := '';
    end
    else if tileset_list[i].location = 'TLS Mod' then
    begin
      AssignFile(tileset_file, Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\' + MissionIni.ModsFolder + '\Data\Tilesets\' + tileset_list[i].name + '.TLS');
      Reset(tileset_file);
      BlockRead(tileset_file, header, sizeof(header));
      CloseFile(tileset_file);
      tileimage_location := Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\' + MissionIni.ModsFolder + '\Data';
      tileset_list[i].fancy_name := header.tileset_fancy_name;
      tileset_list[i].author :=     header.author_name;
      tileset_list[i].num_tiles :=  0;
      tileset_list[i].attributes := '';
    end;
    tileset_fancy_name_mapping.Add(tileset_list[i].fancy_name + '=' + IntToStr(i));
    // Get number of tiles by checking R8/R16 file size
    if FileExists(tileimage_location + '\' + tileset_list[i].name + '.R16') then
    begin
      AssignFile(f, tileimage_location + '\' + tileset_list[i].name + '.R16');
      FileMode := fmOpenRead;
      Reset(f);
      tileset_list[i].num_tiles := FileSize(f) div r16_tile_total_size;
      Close(f);
    end
    else if FileExists(tileimage_location + '\' + tileset_list[i].name + '.R8') then
    begin
      AssignFile(f, tileimage_location + '\' + tileset_list[i].name + '.R8');
      FileMode := fmOpenRead;
      Reset(f);
      tileset_list[i].num_tiles := FileSize(f) div r8_tile_total_size;
      Close(f);
    end
  end;
  // Process tileset index mapping
  tileset_fancy_name_mapping.Sort;
  for i := 0 to cnt_tilesets -1 do
    tileset_index_mapping[i] := StrToInt(tileset_fancy_name_mapping.ValueFromIndex[i]);
  // Clean up
  tmp_strings.Destroy;
  tileset_fancy_name_mapping.Destroy;
  // Register event in dispatcher
  Dispatcher.register_event(evFLLTilesetList);
end;

procedure TTileset.new_tileset(p_tileset_name: String);
var
  i: integer;
begin
  for i := 0 to cnt_tilesets - 1 do
    if AnsiCompareText(tileset_list[i].name, p_tileset_name) = 0 then
    begin
      Dispatcher.register_error('New tileset', 'Tileset with name ' + p_tileset_name + ' already exists.');
      exit;
    end;
  // Initialize status
  tileset_name := p_tileset_name;
  tileatr_name := '';
  tileimage_filename := '';
  tls_filename := '';
  tileatr_filename := '';
  ini_filename := '';
  // Initialize tileset image
  cnt_tiles := min_tileset_tiles;
  tileimage.Height := (cnt_tiles div 20) * 32;
  tileimage.Canvas.Pen.Color := clBlack;
  tileimage.Canvas.Brush.Color := clBlack;
  tileimage.Canvas.Brush.Style := bsSolid;
  tileimage.Canvas.Rectangle(0, 0, tileimage_width, tileimage.Height);
  tileimage_modified := true;
  // Initialize header
  FillChar(header, sizeof(header), 0);
  header.version_major := 1;
  header.version_minor := 0;
  // Initialize attributes and hints
  FillChar(attributes, sizeof(attributes), 0);
  FillChar(attributes_extra, sizeof(attributes_extra), 0);
  for i := 0 to cnt_tiles - 1 do
    tile_hint_text[i] := -1;
  FillChar(restrictions, sizeof(restrictions), 0);
  // Initialize configuration
  FillChar(extra_attribute_names, sizeof(extra_attribute_names), 0);
  minimap_color_rules_used := 1;
  FillChar(minimap_color_rules, sizeof(minimap_color_rules), 0);
  fill_area_rules_used := 1;
  FillChar(fill_area_rules, sizeof(fill_area_rules), 0);
  FillChar(paint_tile_groups, sizeof(paint_tile_groups), 0);
  for i := -4 to cnt_paint_tile_groups - 1 do
    paint_tile_groups[i].smooth_preset_group := -1;
  FillChar(block_preset_groups, sizeof(block_preset_groups), 0);
  FillChar(block_preset_key_variants, sizeof(block_preset_key_variants), 0);
  FillChar(block_presets, sizeof(block_presets), 0);
  FillChar(block_preset_tiles, sizeof(block_preset_tiles), 0);
  FillChar(connection_points, sizeof(connection_points), 0);
  FillChar(connection_point_types, sizeof(connection_point_types), 0);
  FillChar(block_groups, sizeof(block_groups), 0);
  process_internal_data;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTilesetFile);
end;

procedure TTileset.change_tileset_by_index(index: integer);
begin
  if (index >= cnt_tilesets) or (index < 0) then
    exit;
  tileset_name := tileset_list[index].name;
  tileatr_name := tileset_list[index].attributes;
  load_tileset(false);
end;

procedure TTileset.change_tileset_by_name(p_tileset_name, p_tileatr_name: String);
begin
  tileset_name := p_tileset_name;
  tileatr_name := p_tileatr_name;
  load_tileset(false);
end;

procedure TTileset.change_tileset_to_default;
var
  i: integer;
begin
  for i := 0 to cnt_tilesets - 1 do
    if tileset_list[i].name = Settings.DefaultTilesetName then
    begin
      change_tileset_by_index(i);
      break;
    end;
end;

procedure TTileset.change_tileset_next;
var
  i: integer;
begin
  for i := 0 to cnt_tilesets - 1 do
    if tileset_list[tileset_index_mapping[i]].name = tileset_name then
    begin
      change_tileset_by_index(tileset_index_mapping[(i + 1) mod cnt_tilesets]);
      break;
    end;
end;

procedure TTileset.load_tileset(force_reload: boolean);
var
  tileset_file_found: boolean;
begin
  load_tileimage(tileset_name, force_reload);
  tileset_file_found := load_tls(tileset_name, force_reload);
  // TLS file not found, fall back to loading TILEATRx.BIN file and tileset ini configuration file
  if not tileset_file_found then
  begin
    load_tileatr(tileatr_name, force_reload);
    load_ini(tileset_name, force_reload);
  end;
  // Failsafe to default tileset if tileset image/attributes could not be loaded
  if (tileimage_filename = '') and (tileset_name <> Settings.DefaultTilesetName) then
    load_tileimage(Settings.DefaultTilesetName, false);
  if (tls_filename = '') and (tileatr_filename = '') and (ini_filename = '') and (tileset_name <> Settings.DefaultTilesetName) then
    load_tls(Settings.DefaultTilesetName, false);
end;

procedure TTileset.save_tileset;
begin
  save_tileimage;
  save_tls;
end;

procedure TTileset.load_tileimage(p_tileset_name: string; force: boolean);
var
  tmp_filename: string;
begin
  // Try to find R16 file
  tmp_filename := find_file('Data\' + p_tileset_name + '.R16', '');
  if (tmp_filename <> '') and (tmp_filename = tileimage_filename) and not force then
    exit;
  if tmp_filename <> '' then
  begin
    load_r16_image(tmp_filename);
    tileimage_modified := false;
    exit;
  end;
  // Try to find R8 file
  tmp_filename := find_file('Data\' + p_tileset_name + '.R8', '');
  if (tmp_filename <> '') and (tmp_filename = tileimage_filename) and not force then
    exit;
  if tmp_filename <> '' then
  begin
    load_r8_image(tmp_filename);
    tileimage_modified := false;
    exit;
  end;
  Dispatcher.register_error('Error loading tileset graphics', 'Could not find image file for tileset ' + p_tileset_name);
end;

procedure TTileset.load_r16_image(filename: String);
var
  f: file of byte;
  file_size: integer;
  r16_file_buffer: array of byte;
  r16_tile_data_buffer: TWordArrayPtr;
  r16_tileset_data_buffer: TWordArrayPtr;
  i, x, y, offset: integer;
begin
  // Load file into buffer
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  file_size := filesize(f);
  if (file_size mod r16_tile_total_size) <> 0 then
  begin
    Dispatcher.register_error('Error loading tileset graphics', 'The file ' + filename + ' does not have expected size - must be a multiple of ' + inttostr(r16_tile_total_size) + ' bytes)');
    Close(f);
    exit;
  end;
  cnt_tiles := file_size div r16_tile_total_size;
  SetLength(r16_file_buffer, file_size);
  BlockRead(f, r16_file_buffer[0], file_size);
  Close(f);
  // Arrange image data into tileset image
  tileimage.Width := tileimage_width;
  tileimage.Height := (cnt_tiles div 20) * 32;
  tileimage.PixelFormat := pf15bit;
  r16_tileset_data_buffer := tileimage.ScanLine[tileimage.Height-1];
  for i := 0 to cnt_tiles-1 do
  begin
    offset := i * (r16_tile_header_size + r16_tile_data_size) + r16_tile_header_size;
    r16_tile_data_buffer := Addr(r16_file_buffer[offset]);
    for y := 0 to 31 do
      for x := 0 to 31 do
      begin
        r16_tileset_data_buffer[x + ((i mod 20) * 32) + ((tileimage.Height - (y + ((i div 20) * 32)) - 1) * tileimage_width)] := r16_tile_data_buffer[x + y*32];
      end;
  end;
  tileimage.PixelFormat := pf32bit;
  // Update file name
  tileimage_filename := filename;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTilesetImage);
end;

procedure TTileset.load_r8_image(filename: String);
var
  f: file of byte;
  file_size: integer;
  r8_file_buffer: array of byte;
  r8_tile_data_buffer: TByteArrayPtr;
  r8_tileset_data_buffer: TByteArrayPtr;
  i, x, y, offset: integer;
begin
  // Load file into buffer
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  file_size := filesize(f);
  if (file_size mod r8_tile_total_size) <> 0 then
  begin
    Dispatcher.register_error('Error loading tileset graphics', 'The file ' + filename + ' does not have expected size - must be a multiple of ' + inttostr(r8_tile_total_size) + ' bytes)');
    Close(f);
    exit;
  end;
  cnt_tiles := file_size div r8_tile_total_size;
  SetLength(r8_file_buffer, file_size);
  BlockRead(f, r8_file_buffer[0], file_size);
  Close(f);
  // Arrange image data into tileset image
  tileimage.Width := tileimage_width;
  tileimage.Height := (cnt_tiles div 20) * 32;
  tileimage.PixelFormat := pf8bit;
  load_palette;
  if palette_loaded then
    tileimage.Palette := CreatePalette(palette^);
  r8_tileset_data_buffer := tileimage.ScanLine[tileimage.Height-1];
  for i := 0 to cnt_tiles-1 do
  begin
    offset := i * (r8_tile_header_size + r8_tile_data_size) + r8_tile_header_size;
    r8_tile_data_buffer := Addr(r8_file_buffer[offset]);
    for y := 0 to 31 do
      for x := 0 to 31 do
      begin
        r8_tileset_data_buffer[x + ((i mod 20) * 32) + ((tileimage.Height - (y + ((i div 20) * 32)) - 1) * tileimage_width)] := r8_tile_data_buffer[x + y*32];
      end;
  end;
  tileimage.PixelFormat := pf32bit;
  // Update file name
  tileimage_filename := filename;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTilesetImage);
end;

procedure TTileset.load_palette;
var
  filename: String;
  f: file of byte;
  tmp_palette: array[0..255, 0..2] of byte;
  i: integer;
begin
  if palette_loaded then
    exit;
  filename := Settings.GamePath+'\Data\bin\PALETTE.BIN';
  if not FileExists(filename) then
  begin
    Dispatcher.register_error('Error loading tileset graphics', 'Cannot load palette. Could not find file ' + filename);
    exit;
  end;
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  BlockRead(f, tmp_palette[0,0], sizeof(tmp_palette));
  Close(f);
  GetMem(palette, Sizeof( TLogPalette ) + Sizeof( TPaletteEntry ) * 255);
  palette.palversion := $300;
  palette.palnumentries := 256;
  for i := 0 to 255 do
  begin
    palette.palPalEntry[i].peRed := tmp_palette[i,0] shl 2;
    palette.palPalEntry[i].peGreen := tmp_palette[i,1] shl 2;
    palette.palPalEntry[i].peBlue := tmp_palette[i,2] shl 2;
  end;
  palette_loaded := true;
end;

procedure TTileset.save_tileimage;
var
  name_pattern: string;
  tmp_filename: string;
begin
  if not tileimage_modified then
    exit;
  name_pattern := 'Data\' + tileset_name + '.R16';
  tmp_filename := Settings.GamePath + '\' + name_pattern;
  if not manage_filesave(tmp_filename, name_pattern, evTileimageFilenameChange) then
    exit;
  save_r16_image(tmp_filename);
  // Update filename
  if tileimage_filename <> tmp_filename then
  begin
    tileimage_filename := tmp_filename;
    Dispatcher.register_event(evTileimageFilenameChange);
  end;
  tileimage_modified := false;
end;

procedure TTileset.save_r16_image(filename: String);
var
  f: file of byte;
  r16_file_buffer: array of byte;
  r16_header: TR16EntryHeaderPtr;
  r16_tile_data_buffer: TWordArrayPtr;
  r16_tileset_data_buffer: TWordArrayPtr;
  i, x, y, header_offset, offset: integer;
  tmp_bitmap: TBitmap;
begin
  // Initialize buffer
  SetLength(r16_file_buffer, cnt_tiles * r16_tile_total_size);
  // Arrange image data into tileset image
  tmp_bitmap := TBitmap.Create;
  tmp_bitmap.Assign(tileimage);
  tmp_bitmap.PixelFormat := pf15bit;
  r16_tileset_data_buffer := tmp_bitmap.ScanLine[tileimage.Height-1];
  for i := 0 to cnt_tiles-1 do
  begin
    // Fill header
    header_offset := i * (r16_tile_header_size + r16_tile_data_size);
    r16_header := Addr(r16_file_buffer[header_offset]);
    r16_header.EntryType := 1;
    r16_header.ImageWidth := 32;
    r16_header.ImageHeight := 32;
    r16_header.ImageHandle := $01FE8514;
    r16_header.BitsPerPixel := 16;
    r16_header.FrameHeight := 32;
    r16_header.FrameWidth := 32;
    // Fill pixel data
    offset := header_offset + r16_tile_header_size;
    r16_tile_data_buffer := Addr(r16_file_buffer[offset]);
    for y := 0 to 31 do
      for x := 0 to 31 do
      begin
        r16_tile_data_buffer[x + y*32] := r16_tileset_data_buffer[x + ((i mod 20) * 32) + ((tileimage.Height - (y + ((i div 20) * 32)) - 1) * tileimage_width)];
      end;
  end;
  tmp_bitmap.Destroy;
  // Save buffer into file
  AssignFile(f, filename);
  ReWrite(f);
  BlockWrite(f, r16_file_buffer[0], Length(r16_file_buffer));
  Close(f);
  SetLength(r16_file_buffer, 0);
end;

function TTileset.load_tls(p_tileset_name: string; force: boolean): boolean;
var
  tmp_filename: string;
  tileset_file: file of byte;
begin
  result := true;
  // Try to find TLS file
  tmp_filename := find_file('Data\Tilesets\' + p_tileset_name + '.TLS', '');
  // TLS file not found
  if tmp_filename = '' then
  begin
    result := false;
    exit;
  end;
  // TLS file found and is same
  if (tmp_filename = tls_filename) and not force then
    exit;
  // Load tileset file
  AssignFile(tileset_file, tmp_filename);
  Reset(tileset_file);
  BlockRead(tileset_file, header,                     sizeof(header));
  BlockRead(tileset_file, attributes,                 sizeof(attributes));
  BlockRead(tileset_file, attributes_extra,           sizeof(attributes_extra));
  BlockRead(tileset_file, tile_hint_text,             sizeof(tile_hint_text));
  BlockRead(tileset_file, restrictions,               sizeof(restrictions));
  BlockRead(tileset_file, extra_attribute_names,      sizeof(extra_attribute_names));
  BlockRead(tileset_file, minimap_color_rules_used,   sizeof(minimap_color_rules_used));
  BlockRead(tileset_file, minimap_color_rules,        sizeof(minimap_color_rules));
  BlockRead(tileset_file, fill_area_rules_used,       sizeof(fill_area_rules_used));
  BlockRead(tileset_file, fill_area_rules,            sizeof(fill_area_rules));
  BlockRead(tileset_file, paint_tile_groups,          sizeof(paint_tile_groups));
  BlockRead(tileset_file, block_preset_groups,        sizeof(block_preset_groups));
  BlockRead(tileset_file, block_preset_key_variants,  sizeof(block_preset_key_variants));
  BlockRead(tileset_file, block_presets,              sizeof(block_presets));
  BlockRead(tileset_file, block_preset_tiles,         sizeof(block_preset_tiles));
  BlockRead(tileset_file, connection_points,          sizeof(connection_points));
  BlockRead(tileset_file, connection_point_types,     sizeof(connection_point_types));
  BlockRead(tileset_file, block_groups,               sizeof(block_groups));
  CloseFile(tileset_file);
  // Process internal data
  process_internal_data;
  // Update filename
  tls_filename := tmp_filename;
  // Unset TILEATRx.BIN and tileset.ini filename
  tileatr_name := '';
  tileatr_filename := '';
  ini_filename := '';
  // Register event in dispatcher
  Dispatcher.register_event(evFLTilesetFile);
end;

procedure TTileset.save_tls;
var
  name_pattern: string;
  tmp_filename: string;
  tileset_file: file of byte;
begin
  name_pattern := 'Data\Tilesets\' + tileset_name + '.TLS';
  tmp_filename := Settings.GamePath + '\' + name_pattern;
  if not manage_filesave(tmp_filename, name_pattern, evTlsFilenameChange) then
    exit;
  // Save tileset file
  AssignFile(tileset_file, tmp_filename);
  ReWrite(tileset_file);
  BlockWrite(tileset_file, header,                    sizeof(header));
  BlockWrite(tileset_file, attributes,                sizeof(attributes));
  BlockWrite(tileset_file, attributes_extra,          sizeof(attributes_extra));
  BlockWrite(tileset_file, tile_hint_text,            sizeof(tile_hint_text));
  BlockWrite(tileset_file, restrictions,              sizeof(restrictions));
  BlockWrite(tileset_file, extra_attribute_names,     sizeof(extra_attribute_names));
  BlockWrite(tileset_file, minimap_color_rules_used,  sizeof(minimap_color_rules_used));
  BlockWrite(tileset_file, minimap_color_rules,       sizeof(minimap_color_rules));
  BlockWrite(tileset_file, fill_area_rules_used,      sizeof(fill_area_rules_used));
  BlockWrite(tileset_file, fill_area_rules,           sizeof(fill_area_rules));
  BlockWrite(tileset_file, paint_tile_groups,         sizeof(paint_tile_groups));
  BlockWrite(tileset_file, block_preset_groups,       sizeof(block_preset_groups));
  BlockWrite(tileset_file, block_preset_key_variants, sizeof(block_preset_key_variants));
  BlockWrite(tileset_file, block_presets,             sizeof(block_presets));
  BlockWrite(tileset_file, block_preset_tiles,        sizeof(block_preset_tiles));
  BlockWrite(tileset_file, connection_points,         sizeof(connection_points));
  BlockWrite(tileset_file, connection_point_types,    sizeof(connection_point_types));
  BlockWrite(tileset_file, block_groups,              sizeof(block_groups));
  CloseFile(tileset_file);
  // Unset TILEATRx.BIN and tileset.ini filename
  tileatr_name := '';
  tileatr_filename := '';
  ini_filename := '';
  // Update filename
  if tls_filename <> tmp_filename then
  begin
    tls_filename := tmp_filename;
    load_tileset_list;
    Dispatcher.register_event(evTlsFilenameChange);
  end;
end;

procedure TTileset.load_tileatr(p_tileatr_name: string; force: boolean);
var
  tmp_filename: string;
  tileatr_file: file of cardinal;
begin
  tmp_filename := find_file('Data\bin\' + p_tileatr_name + '.BIN', 'tile attributes');
  if (tmp_filename = '') or ((tmp_filename = tileatr_filename) and not force) then
    exit;
  // Load TILEATR.BIN file
  AssignFile(tileatr_file, tmp_filename);
  Reset(tileatr_file);
  if FileSize(tileatr_file) <> (min_tileset_tiles * 2) then
  begin
    Dispatcher.register_error('Error loading tile attributes', 'The file ' + tmp_filename + ' does not have expected size (' + inttostr(min_tileset_tiles * 2 * 4) + ' bytes)');
    CloseFile(tileatr_file);
    exit;
  end;
  BlockRead(tileatr_file, attributes, min_tileset_tiles);
  BlockRead(tileatr_file, tile_hint_text, min_tileset_tiles);
  CloseFile(tileatr_file);
  // Update file name
  tileatr_filename := tmp_filename;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTileatrBin);
end;

procedure TTileset.load_ini(p_tileset_name: string; force: boolean);
var
  tmp_filename: string;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder, decoder2: TStringList;
  config_version: integer;
  i, j, k, l, x, y: integer;
  key: char;
  preset_index: integer;
  preset_tile_index: integer;
  connection_point_index: integer;
  tmp_block_preset: PBlockPreset;
  tile, index, width, height, pos_x, pos_y: integer;
  custom_tiles: boolean;
  group_name: String;
begin
  tmp_filename := find_file('tilesets\' + p_tileset_name + '.ini', '');
  if (tmp_filename <> '') and (tmp_filename = ini_filename) and not force then
    exit;
  if tmp_filename = '' then
  begin
    Application.MessageBox(PChar('Could not find tileset configuration ini file for tileset ' + p_tileset_name + '. Loading default tileset configuration instead.'), 'Error loading tileset configuration', MB_ICONWARNING or MB_OK);
    tmp_filename := find_file('tilesets\template.ini', 'tileset configuration');
    if (tmp_filename = '') or (tmp_filename = ini_filename) then
      exit;
  end;
  ini_filename := tmp_filename;
  tls_filename := '';

  ini := TMemIniFile.Create(tmp_filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder2 := TStringList.Create;
  decoder.Delimiter := ';';
  decoder2.Delimiter := '.';
  // Load basic information
  FillChar(header, sizeof(header), 0);
  header.version_major := 1;
  header.version_minor := 0;
  header.default_paint_group := ini.ReadInteger('Basic', 'default_paint_group', 1) - 1;
  store_c_string(ini.ReadString('Basic', 'name', ''), Addr(header.tileset_fancy_name), Length(header.tileset_fancy_name));
  store_c_string(ini.ReadString('Basic', 'author', ''), Addr(header.author_name), Length(header.author_name));
  config_version := ini.ReadInteger('Basic', 'version', 0);
  if config_version <> CURRENT_TILESET_CONFIG_VERSION then
    Application.MessageBox(PChar(
      'The tileset configuration file ''' + ini_filename + ''''#13 +
      'was made for different version of D2kEditor and may be incompatible with the version you use.'#13 +
      'Please get the latest available version of tileset configuration ini file or fix your file.'#13 +
      'You can get one or ask for help on FED2k forums.'),
      'Tileset configuration warning', MB_ICONWARNING + MB_OK);
  // Load extra attributes
  for i := 0 to max_tileset_tiles - 1 do
    attributes_extra[i] := 0;
  for i := 0 to 7 do
  begin
    decoder.DelimitedText := ini.ReadString('Editor_Tile_Attributes', 'Attribute'+inttostr(i+1), '');
    for j := 0 to decoder.Count-1 do
    begin
      tile := strtoint(decoder[j]);
      if (tile >= 0) and (tile < cnt_tiles) then
        attributes_extra[tile] := attributes_extra[tile] or (1 shl i)
    end;
  end;
  // Load extra attribute names
  for i := 0 to 7 do
    store_c_string(ini.ReadString('Editor_Tile_Attribute_Names', 'Attribute'+inttostr(i+1), 'Editor Attribute '+inttostr(i+1)), Addr(extra_attribute_names[i]), Length(extra_attribute_names[i]));
  // Load minimap color rules
  minimap_color_rules_used := 0;
  ini.ReadSection('Minimap_Color_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_minimap_color_rules then
      break;
    minimap_color_rules[i].name := '';
    minimap_color_rules[i].color := strtoint(tmp_strings[i]);
    string_to_rule(ini.ReadString('Minimap_Color_Rules', tmp_strings[i], '0'), Addr(minimap_color_rules[i].rule));
    inc(minimap_color_rules_used);
  end;
  // Load fill area rules
  fill_area_rules_used := 0;
  ini.ReadSection('Fill_Area_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_fill_area_rules then
      break;
    store_c_string(tmp_strings[i], Addr(fill_area_rules[i].name), Length(fill_area_rules[i].name));
    string_to_rule(ini.ReadString('Fill_Area_Rules', tmp_strings[i], '0'), Addr(fill_area_rules[i].rule));
    inc(fill_area_rules_used);
  end;
  // Load paint tile groups
  for i := -4 to cnt_paint_tile_groups - 1 do
  begin
    group_name := 'Group' + get_paint_tile_group_char(i);
    store_c_string(ini.ReadString('Paint_Tile_Groups', group_name+'.name', ''), Addr(paint_tile_groups[i].name), Length(paint_tile_groups[i].name));
    paint_tile_groups[i].tile_index := ini.ReadInteger('Paint_Tile_Groups', group_name+'.tile', 0);
    string_to_rule(ini.ReadString('Paint_Tile_Groups', group_name+'.restriction_rule', '0'), Addr(paint_tile_groups[i].restriction_rule));
    if i >= 0 then
    begin
      decoder.DelimitedText := ini.ReadString('Paint_Tile_Groups', group_name+'.paint_tiles', '');
      for j := 0 to decoder.Count-1 do
      begin
        tile := strtoint(decoder[j]);
        if (tile >= 0) and (tile < cnt_tiles) then
          attributes_extra[tile] := attributes_extra[tile] or (1 shl (i + 8));
      end;
      if (decoder.Count = 0) and (StrLen(paint_tile_groups[i].name) > 0) then
        attributes_extra[paint_tile_groups[i].tile_index] := attributes_extra[paint_tile_groups[i].tile_index] or (1 shl (i + 8));
      paint_tile_groups[i].smooth_preset_group := ini.ReadInteger('Paint_Tile_Groups', group_name+'.smooth_preset_group', 0) - 1;
      store_c_string(ini.ReadString('Paint_Tile_Groups', group_name+'.smooth_presets', ''), Addr(paint_tile_groups[i].smooth_presets), Length(paint_tile_groups[i].smooth_presets));
      store_c_string(ini.ReadString('Paint_Tile_Groups', group_name+'.random_map', ''), Addr(paint_tile_groups[i].random_map_name), Length(paint_tile_groups[i].random_map_name));
    end;
  end;
  // Load block preset groups
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    store_c_string(ini.ReadString('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.name', ''), Addr(block_preset_groups[i].name), Length(block_preset_groups[i].name));
    block_preset_groups[i].paint_group := ini.ReadInteger('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.paint', -4) - 1;
  end;
  // Load block presets
  preset_index := 1;
  preset_tile_index := 0;
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    for j := 0 to cnt_block_preset_keys - 1 do
    begin
      key := block_preset_keys[j];
      decoder.DelimitedText := ini.ReadString('Block_Preset_Group_'+(inttostr(i+1)), key, '');
      block_preset_key_variants[i, j] := decoder.Count;
      for k := 0 to decoder.Count - 1 do
      begin
        decoder2.DelimitedText := decoder[k];
        if (preset_index >= max_block_presets) then
          break;
        if (decoder2.Count < 3) then
          break;
        // Load width and height first
        custom_tiles := false;
        width := strtoint(decoder2[0]);
        height := strtoint(decoder2[1]);
        if width < 0 then
        begin
          width := width * -1;
          custom_tiles := true;
        end;
        if height < 0 then
        begin
          height := height * -1;
          custom_tiles := true;
        end;
        if (preset_tile_index + width * height >= max_block_preset_tiles) then
          continue;
        block_presets[preset_index].width := width;
        block_presets[preset_index].height := height;
        // Load preset tiles
        if not custom_tiles then
        begin
          // Continuous tiles
          if decoder2.Count = 4 then
          begin
            pos_x := strtoint(decoder2[2]);
            pos_y := strtoint(decoder2[3]);
            index := pos_x + pos_y * 20;
          end else
            index := strtoint(decoder2[2]);
          for y := 0 to height - 1 do
            for x := 0 to width - 1 do
              begin
                tile := index + x + (y * 20);
                block_preset_tiles[preset_tile_index + x + y * width] := tile;
              end;
        end else
        begin
          // Custom tiles
          if (decoder2.Count - 2 <> width * height) then
            continue;
          for y := 0 to height - 1 do
            for x := 0 to width - 1 do
            begin
              tile := strtoint(decoder2[x + y * width + 2]);
              block_preset_tiles[preset_tile_index + x + y * width] := tile;
            end;
        end;
        inc(preset_tile_index, width * height);
        inc(preset_index);
      end;
    end;
  end;

  {--// Load connection points
  connection_point_index := 0;
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    for j := 0 to cnt_block_preset_keys - 1 do
    begin
      key := ' ';
      if (j >= 0) and (j <= 9) then
        key := chr(j + ord('0'))
      else if (j >= 10) and (j <= 35) then
        key := chr(j + ord('A') - 10)
      else if j = 36 then
        key := '<'
      else if j = 37 then
        key := '>'
      else if j = 38 then
        key := ':'
      else if j = 39 then
        key := '?';
      for k := 0 to block_preset_key_variants[i, j].num_variants - 1 do
      begin
        tmp_block_preset := @block_presets[block_preset_key_variants[i, j].first_preset_index + k];
        decoder.DelimitedText := ini.ReadString('Connection_Points_For_Group_'+(inttostr(i+1)), key + inttostr(k+1), '');
        if decoder.Count = 0 then
        begin
          tmp_block_preset.block_group := 0;
          tmp_block_preset.num_connection_points := 0;
          tmp_block_preset.connection_point_index := 0;
          continue;
        end;
        tmp_block_preset.block_group := strtoint(decoder[0]);
        tmp_block_preset.num_connection_points := decoder.Count - 1;
        tmp_block_preset.connection_point_index := connection_point_index;
        for l := 1 to decoder.Count - 1 do
        begin
          decoder2.DelimitedText := decoder[l];
          if (connection_point_index >= max_connection_points) then
            break;
          if decoder2.Count = 3 then
          begin
            // 8 bytes: XTTTTDDD (16 types, 8 directions)
            connection_points[connection_point_index].type_and_direction := (strtoint(decoder2[0]) shl 3) or (strtoint(decoder2[1]) - 1);
            connection_points[connection_point_index].offset := strtoint(decoder2[2]);
          end;
          inc(connection_point_index);
        end;
      end;
    end;
  end;
  connection_points_used := connection_point_index;

  // Load block groups
  for i := 0 to cnt_block_groups - 1 do
  begin
    decoder2.DelimitedText := ini.ReadString('Block_Groups',inttostr(i),'');
    if decoder2.Count <> 2 then
    begin
      block_groups[i].repeat_distance := 0;
      block_groups[i].absolute_weight := 0;
    end else
    begin
      block_groups[i].repeat_distance := strtoint(decoder2[0]);
      block_groups[i].absolute_weight := strtoint(decoder2[1]);
    end;
  end;

  // Load connection point types
  for i := 0 to cnt_connection_point_types - 1 do
    begin
      decoder2.DelimitedText := ini.ReadString('Connection_Point_Types',inttostr(i), '1.1.1');
      connection_point_types[i].paint_group := strtoint(decoder2[0]) - 1;
      connection_point_types[i].connection_point_width := strtoint(decoder2[1]);
      connection_point_types[i].connection_point_height := strtoint(decoder2[2]);
    end;}

  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
  decoder2.Destroy;

  process_internal_data;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTilesetIni);
end;

procedure TTileset.process_internal_data;
begin
  process_paint_tile_lists;
  process_paint_tile_random_maps;
  process_block_presets;
end;

procedure TTileset.process_paint_tile_lists;
var
  i: integer;
  tile_paint_group: integer;
begin
  FillChar(paint_tile_lists, sizeof(paint_tile_lists), 0);
  for i := 0 to cnt_tiles - 1 do
  begin
    tile_paint_group := get_tile_paint_group(i);
    if (tile_paint_group >= 0) and (paint_tile_lists[tile_paint_group].cnt_tiles < max_paint_tiles) then
    begin
      paint_tile_lists[tile_paint_group].tiles[paint_tile_lists[tile_paint_group].cnt_tiles] := i;
      inc(paint_tile_lists[tile_paint_group].cnt_tiles);
    end;
  end;
end;

procedure TTileset.process_paint_tile_random_maps;
var
  i, j: integer;
  map_filename: String;
  map_file: file of word;
  map_file_buffer: array[0..max_map_width*max_map_height*2-1] of word;
  map_width, map_height: word;
begin
  for i := 0 to cnt_paint_tile_groups - 1 do
  begin
    SetLength(paint_tile_random_maps[i], 0);
    if StrLen(paint_tile_groups[i].random_map_name) > 0 then
    begin
      map_filename := current_dir + 'tilesets\' + paint_tile_groups[i].random_map_name + '.map';
      if FileExists(map_filename) then
      begin
        AssignFile(map_file, map_filename);
        Reset(map_file);
        Read(map_file, map_width);
        Read(map_file, map_height);
        if (map_width = max_map_width) and (map_height = max_map_height) then
        begin
          SetLength(paint_tile_random_maps[i], max_map_width * max_map_height);
          BlockRead(map_file, map_file_buffer[0], Length(map_file_buffer));
          for j := 0 to max_map_width * max_map_height - 1 do
            paint_tile_random_maps[i][j] := map_file_buffer[j*2];
        end;
        CloseFile(map_file);
      end;
    end;
  end;
end;

procedure TTileset.process_block_presets;
var
  i, j: integer;
begin
  block_presets_used := 1;
  block_preset_tiles_used := 0;
  for i := 0 to cnt_block_preset_groups - 1 do
    for j := 0 to cnt_block_preset_keys - 1 do
    begin
      block_preset_key_variant_first_preset_indexes[i,j] := block_presets_used;
      inc(block_presets_used, block_preset_key_variants[i,j]);
    end;
  for i := 0 to block_presets_used - 1 do
  begin
    block_preset_first_tile_indexes[i] := block_preset_tiles_used;
    inc(block_preset_tiles_used, block_presets[i].width * block_presets[i].height);
  end;
end;

procedure TTileset.import_tileimage_from_file(filename: String);
var
  tmp_bitmap: TGraphic;
  ext: String;
  error: String;
begin
  error := '';
  // Check for file existence
  if (filename = '') then
    exit;
  if not FileExists(filename) then
  begin
    Dispatcher.register_error('Error loading tileset image', 'The file ' + filename + ' does not exist.');
    exit;
  end;
  ext := UpperCase(ExtractFileExt(filename));
  if ext = '.R16' then
    load_r16_image(filename)
  else if ext = '.R8' then
    load_r8_image(filename)
  else
  begin
    if ext = '.BMP' then
      tmp_bitmap := TBitmap.Create
    else if ext = '.PNG' then
      tmp_bitmap := TPNGObject.Create
    else
    begin
      Dispatcher.register_error('Error loading tileset image', 'Unsupported image format ' + ext);
      exit;
    end;
    tmp_bitmap.LoadFromFile(filename);
    // Check the image
    if tmp_bitmap.Width <> tileimage_width then
      error := 'Tileset image width must be 640 pixels (20 tiles * 32 pixels)'
    else if (tmp_bitmap.Height mod 32) <> 0 then
      error := 'Tileset image height must be a multiple of 32 pixels'
    else if tmp_bitmap.Height < 1280 then
      error := 'Tileset image height must be at least 1280 pixels (40 tiles * 32 pixels)'
    else if tmp_bitmap.Height > 6400 then
      error := 'Tileset image height must be at most 6400 pixels (200 tiles * 32 pixels)';
    if error = '' then
    begin
      // Assign tile image
      tileimage.Assign(tmp_bitmap);
      tileimage.PixelFormat := pf32bit;
      // Update count of tiles
      cnt_tiles := (tmp_bitmap.Height div 32) * 20;
      // Update file name
      tileimage_filename := filename;
      // Register event in dispatcher
      Dispatcher.register_event(evFLTilesetImage);
    end else
      Dispatcher.register_error('Error loading tileset image', error);
    // Clean up
    tmp_bitmap.Destroy;
  end;
  tileimage_modified := true;
end;

procedure TTileset.export_tileimage_to_file(filename: String);
var
  PNG: TPNGObject;
  ext: String;
begin
  ext := UpperCase(ExtractFileExt(filename));
  if ext = '.R16' then
    save_r16_image(filename)
  else if ext = '.BMP' then
    tileimage.SaveToFile(filename)
  else if ext = '.PNG' then
  begin
    PNG := TPNGObject.Create;
    PNG.Assign(tileimage);
    PNG.SaveToFile(filename);
    PNG.Destroy;
  end;
end;

procedure TTileset.import_tileimage_portion_from_file(filename: String; pos_x, pos_y: integer);
var
  tmp_bitmap: TBitmap;
  PNG: TPNGObject;
  ext: String;
  error: String;
begin
  error := '';
  // Check for file existence
  if (filename = '') then
    exit;
  if not FileExists(filename) then
  begin
    Dispatcher.register_error('Error loading tileset portion image', 'The file ' + filename + ' does not exist.');
    exit;
  end;
  // Load file
  ext := UpperCase(ExtractFileExt(filename));
  tmp_bitmap := TBitmap.Create;
  if ext = '.BMP' then
    tmp_bitmap.LoadFromFile(filename)
  else if ext = '.PNG' then
  begin
    PNG := TPNGObject.Create;
    PNG.LoadFromFile(filename);
    tmp_bitmap.Assign(PNG);
    PNG.Destroy;
  end else
  begin
    Dispatcher.register_error('Error loading tileset portion image', 'Unsupported image format ' + ext);
    tmp_bitmap.Destroy;
    exit;
  end;
  // Check image dimensions
  if ((tmp_bitmap.Width mod 32) <> 0) or ((tmp_bitmap.Height mod 32) <> 0) then
  begin
    Dispatcher.register_error('Error loading tileset portion image', 'The image width and height must be divisible by 32.');
    tmp_bitmap.Destroy;
    exit;
  end;
  // Copy contents to tileimage
  tileimage.Canvas.CopyRect(Rect(pos_x * 32, pos_y * 32, pos_x * 32 + tmp_bitmap.Width, pos_y * 32 + tmp_bitmap.Height), tmp_bitmap.Canvas, Rect(0, 0, tmp_bitmap.Width, tmp_bitmap.Height));
  tileimage_modified := true;
  Dispatcher.register_event(evTilesetImageChange);
  tmp_bitmap.Destroy;
end;

procedure TTileset.export_tileimage_portion_to_file(filename: String; pos_x, pos_y, size_x, size_y: integer);
var
  tmp_bitmap: TBitmap;
  PNG: TPNGObject;
  ext: String;
begin
  // Prepare the portion
  tmp_bitmap := TBitmap.Create;
  tmp_bitmap.Width := size_x * 32;
  tmp_bitmap.Height := size_y * 32;
  tmp_bitmap.PixelFormat := pf32bit;
  tmp_bitmap.Canvas.CopyRect(Rect(0, 0, size_x * 32, size_y * 32), tileimage.Canvas, Rect(pos_x * 32, pos_y * 32, (pos_x + size_x) * 32, (pos_y + size_y) * 32));
  // Save to file
  ext := UpperCase(ExtractFileExt(filename));
  if ext = '.BMP' then
    tmp_bitmap.SaveToFile(filename)
  else if ext = '.PNG' then
  begin
    PNG := TPNGObject.Create;
    PNG.Assign(tmp_bitmap);
    PNG.SaveToFile(filename);
    PNG.Destroy;
  end;
  // Clean up
  tmp_bitmap.Destroy;
end;

procedure TTileset.copy_tileimage_portion(src_x, src_y, dest_x, dest_y, size_x, size_y: integer);
begin
  tileimage.Canvas.CopyRect(
    Rect(dest_x * 32, dest_y * 32, (dest_x + size_x) * 32, (dest_y + size_y) * 32),
    tileimage.Canvas,
    Rect(src_x * 32, src_y * 32, (src_x + size_x) * 32, (src_y + size_y) * 32)
  );
  tileimage_modified := true;
  Dispatcher.register_event(evTilesetImageChange);
end;

procedure TTileset.swap_tileimage_portion(src_x, src_y, dest_x, dest_y, size_x, size_y: integer);
var
  tmp_bitmap: TBitmap;
begin
  tmp_bitmap := TBitmap.Create;
  tmp_bitmap.Width := size_x * 32;
  tmp_bitmap.Height := size_y * 32;
  tmp_bitmap.PixelFormat := pf32bit;
  tmp_bitmap.Canvas.CopyRect(
    Rect(0, 0, size_x * 32, size_y * 32),
    tileimage.Canvas,
    Rect(dest_x * 32, dest_y * 32, (dest_x + size_x) * 32, (dest_y + size_y) * 32)
  );
  tileimage.Canvas.CopyRect(
    Rect(dest_x * 32, dest_y * 32, (dest_x + size_x) * 32, (dest_y + size_y) * 32),
    tileimage.Canvas,
    Rect(src_x * 32, src_y * 32, (src_x + size_x) * 32, (src_y + size_y) * 32)
  );
  tileimage.Canvas.CopyRect(
    Rect(src_x * 32, src_y * 32, (src_x + size_x) * 32, (src_y + size_y) * 32),
    tmp_bitmap.Canvas,
    Rect(0, 0, size_x * 32, size_y * 32)
  );
  tileimage_modified := true;
  Dispatcher.register_event(evTilesetImageChange);
end;

procedure TTileset.erase_tileimage_portion(pos_x, pos_y, size_x, size_y: integer);
begin
  tileimage.Canvas.Pen.Color := clBlack;
  tileimage.Canvas.Brush.Color := clBlack;
  tileimage.Canvas.Brush.Style := bsSolid;
  tileimage.Canvas.Rectangle(pos_x * 32, pos_y * 32, (pos_x + size_x) * 32, (pos_y + size_y) * 32);
  tileimage_modified := true;
  Dispatcher.register_event(evTilesetImageChange);
end;

procedure TTileset.resize_tileset(new_cnt_tiles: integer);
var
  old_cnt_tiles: integer;
  i: integer;
begin
  if cnt_tiles = new_cnt_tiles then
    exit;
  if (new_cnt_tiles < min_tileset_tiles) or (new_cnt_tiles > max_tileset_tiles) or ((new_cnt_tiles mod 20) <> 0) then
  begin
    Dispatcher.register_error('Resize tileset', Format('Number of tiles must be between %d and %d and must be divisible by 20.', [min_tileset_tiles, max_tileset_tiles]));
    exit;
  end;
  old_cnt_tiles := cnt_tiles;
  cnt_tiles := new_cnt_tiles;
  // Resize image
  tileimage.Height := (cnt_tiles div 20) * 32;
  tileimage.Canvas.Pen.Color := clBlack;
  tileimage.Canvas.Brush.Color := clBlack;
  tileimage.Canvas.Brush.Style := bsSolid;
  tileimage.Canvas.Rectangle(0, (old_cnt_tiles div 20) * 32, tileimage_width, (cnt_tiles div 20) * 32);
  // Initialize attributes and hints
  for i := old_cnt_tiles to cnt_tiles - 1 do
  begin
    attributes[i] := 0;
    attributes_extra[i] := 0;
    tile_hint_text[i] := -1;
    FillChar(restrictions[i], sizeof(restrictions[i]), 0);
  end;
  tileimage_modified := true;
  Dispatcher.register_event(evTilesetImageChange);
end;

procedure TTileset.set_tile_attributes(tile: word; value: Int64);
begin
  attributes[tile] := value and $FFFFFFFF;
  attributes_extra[tile] := value shr 32;
end;

procedure TTileset.add_block_preset(group, key_index, width, height: integer; tiles: array of word);
var
  i: integer;
  add_at_index: integer;
  tiles_to_add: integer;
begin
  tiles_to_add := width * height;
  if block_presets_used = max_block_presets then
  begin
    Dispatcher.register_error('Add block preset', 'Maximum number of block presets reached.');
    exit;
  end;
  if (block_preset_tiles_used + tiles_to_add) > max_block_preset_tiles then
  begin
    Dispatcher.register_error('Add block preset', 'Maximum number of block preset tiles reached.');
    exit;
  end;
  // Add block preset
  add_at_index := block_preset_key_variant_first_preset_indexes[group, key_index] + block_preset_key_variants[group, key_index];
  for i := block_presets_used downto add_at_index + 1 do
    block_presets[i] := block_presets[i - 1];
  block_presets[add_at_index].width := width;
  block_presets[add_at_index].height := height;
  inc(block_preset_key_variants[group, key_index]);
  // Add block preset tiles
  add_at_index := block_preset_first_tile_indexes[add_at_index - 1] + block_presets[add_at_index - 1].width * block_presets[add_at_index - 1].height;
  for i := block_preset_tiles_used + tiles_to_add - 1 downto add_at_index + tiles_to_add do
    block_preset_tiles[i] := block_preset_tiles[i - tiles_to_add];
  for i := 0 to tiles_to_add - 1 do
    block_preset_tiles[add_at_index + i] := tiles[i];
  process_block_presets;
  Dispatcher.register_event(evTilesetBlockPresetsChange);
end;

procedure TTileset.delete_block_preset(preset_index: integer);
var
  i, j, k: integer;
  num_variants, first_preset_index: integer;
  tiles_to_delete: integer;
begin
  for i := 0 to cnt_block_preset_groups - 1 do
    for j := 0 to cnt_block_preset_keys - 1 do
      begin
        num_variants := block_preset_key_variants[i, j];
        first_preset_index := block_preset_key_variant_first_preset_indexes[i, j];
        if (preset_index >= first_preset_index) and (preset_index < first_preset_index + num_variants) then
        begin
          tiles_to_delete := block_presets[preset_index].width * block_presets[preset_index].height;
          Dec(block_preset_key_variants[i, j]);
          for k := block_preset_first_tile_indexes[preset_index] to block_preset_tiles_used - tiles_to_delete - 1 do
            block_preset_tiles[k] := block_preset_tiles[k + tiles_to_delete];
          for k := preset_index to block_presets_used - 2 do
              block_presets[k] := block_presets[k + 1];
          preset_index := -1;
        end;
      end;
  process_block_presets;
  Dispatcher.register_event(evTilesetBlockPresetsChange);
end;

function TTileset.get_paint_tile_group_char(group: integer): char;
begin
  if group >= 0 then
    result := chr(ord('1') + group)
  else
    result := chr(ord('E') + group);
end;

function TTileset.get_tile_attributes(tile, special: word; use_internal_attributes: boolean): Int64;
var
  extra_attributes, void_attribute, tile_property_attributes: int64;
  spice_amount: integer;
begin
  result := attributes[tile and $0FFF];
  extra_attributes := attributes_extra[tile and $0FFF];
  extra_attributes := extra_attributes shl 32;
  result := result or extra_attributes;
  if use_internal_attributes then
  begin
    // Void attribute
    void_attribute := 0;
    if (tile and $0FFF) = paint_tile_groups[-1].tile_index then
      void_attribute := $00080000;
    void_attribute := void_attribute shl 32;
    result := result or void_attribute;
    // Thin spice, thick spice, concrete
    spice_amount := IfThen((tile and $1000) = 0, (tile shr 13) and 7, 0);
    tile_property_attributes := 0;
    if (special = 1) or (spice_amount = 1) or (spice_amount = 2) then
      tile_property_attributes := $00010000;
    if (special = 2) or (spice_amount >= 3) then
      tile_property_attributes := $00020000;
    if ((tile and $1000) <> 0) then
      tile_property_attributes := $00040000;
    tile_property_attributes := tile_property_attributes shl 32;
    result := result or tile_property_attributes;
  end;
end;

function TTileset.get_tile_type(tile: word): TileType;
var
  atr: cardinal;
begin
  atr := attributes[tile and $0FFF];
  if (atr and taAnyPass) = 0 then
    result := ttImpassable
  else if (atr and taAnyPass) = taInfantryPass then
    result := ttInfantryOnly
  else if (atr and taBuildable) = taBuildable then
    result := ttBuildable
  else
    result := ttPassable
end;

function TTileset.get_tile_color(tile, special: word; var rule_index: integer): Cardinal;
var
  i: integer;
  attr_value: int64;
begin
  rule_index := -1;
  attr_value := get_tile_attributes(tile, special, true);
  for i := 0 to minimap_color_rules_used - 1 do
  begin
    if evaluate_rule(attr_value, Addr(minimap_color_rules[i].rule)) then
    begin
      result := minimap_color_rules[i].color;
      rule_index := i;
      exit;
    end;
  end;
  result := $0;
end;

function TTileset.get_fill_area_type(tile: word; special: word): integer;
var
  i: integer;
  attr_value: int64;
begin
  attr_value := get_tile_attributes(tile, special, true);
  for i := 0 to fill_area_rules_used - 1 do
  begin
    if evaluate_rule(attr_value, Addr(fill_area_rules[i].rule)) then
    begin
      result := i;
      exit;
    end;
  end;
  result := fill_area_rules_used - 1;
end;

function TTileset.get_tile_paint_group(tile: word): integer;
var
  i: integer;
begin
  tile := tile and $0FFF;
  result := -128;
  if tile = paint_tile_groups[-1].tile_index then
  begin
    result := -1;
    exit;
  end;
  for i := 0 to cnt_paint_tile_groups - 1 do
  begin
    if (attributes_extra[tile] and (1 shl (i + 8))) <> 0 then
    begin
      result := i;
      break;
    end;
  end;
end;

function TTileset.check_area_type(tile, special: word; area_type: integer): boolean;
var
  attr_value: int64;
begin
  attr_value := get_tile_attributes(tile, special, true);
  result := evaluate_rule(attr_value, Addr(fill_area_rules[area_type].rule));
end;

function TTileset.check_paint_tile_restriction(tile, special: word; paint_tile_group: integer): boolean;
var
  attr_value: int64;
begin
  attr_value := get_tile_attributes(tile, special, true);
  result := evaluate_rule(attr_value, Addr(paint_tile_groups[paint_tile_group].restriction_rule));
end;

function TTileset.evaluate_rule(attr_value: int64; rule_ptr: TTileAtrRulePtr): boolean;
begin
  if (attr_value and rule_ptr.not_attr) <> 0 then
  begin
    result := false;
    exit;
  end;
  if rule_ptr.attr < 0 then
    // Any of attributes will match
    result := (attr_value and (rule_ptr.attr * -1)) <> 0
  else
    // All attributes will match
    result := (attr_value and rule_ptr.attr) = rule_ptr.attr;
end;

function TTileset.get_random_paint_tile(group, x, y: integer): integer;
begin
  if group >= cnt_paint_tile_groups then
  begin
    result := 0;
    exit;
  end;
  if Settings.UseRandomPaintMap and (Length(paint_tile_random_maps[group]) > 0) then
  begin
    result := paint_tile_random_maps[group][x + y * max_map_width];
    exit;
  end;
  if (group < 0) or (paint_tile_lists[group].cnt_tiles = 0) then
  begin
    result := paint_tile_groups[group].tile_index;
    exit;
  end;
  result := paint_tile_lists[group].tiles[random(paint_tile_lists[group].cnt_tiles)];
end;

function TTileset.get_block_preset_index(group: integer; key_index: integer; variant: integer): integer;
var
  num_variants: integer;
  preset_index: integer;
begin
  num_variants := block_preset_key_variants[group, key_index];
  // No block preset for this key
  if num_variants = 0 then
  begin
    result := 0;
    exit;
  end
  // Just one block preset variant for this key
  else if num_variants = 1 then
  begin
    variant := 0
  end else
  // Multiple block preset variants for this key
  begin
    if variant = bpNext then
    begin
      if (last_block_preset_group <> group) or (last_block_preset_key <> key_index) then
        last_block_preset_variant := 0
      else if last_block_preset_variant >= num_variants then
        last_block_preset_variant := 0;
      variant := last_block_preset_variant;
      inc(last_block_preset_variant);
    end
    else if variant = bpRandom then
    begin
      // Get random variant but different from the last one
      repeat
        variant := random(num_variants);
      until (variant <> last_block_preset_variant) or (last_block_preset_key <> key_index);
      last_block_preset_variant := variant;
    end;
  end;
  preset_index := block_preset_key_variant_first_preset_indexes[group, key_index] + variant;
  last_block_preset_group := group;
  last_block_preset_key := key_index;
  result := preset_index;
end;

function TTileset.string_to_rule(rule: String; rule_ptr: TTileAtrRulePtr): boolean;
var
  decoder: TStringList;
begin
  if rule = '' then
  begin
    result := false;
    exit;
  end;
  decoder := TStringList.Create;
  decoder.Delimiter := ';';
  decoder.DelimitedText := rule;
  rule_ptr.attr := strtoint64def(decoder[0], -1);
  if decoder.Count = 2 then
    rule_ptr.not_attr := strtoint64def(decoder[1], -1)
  else
    rule_ptr.not_attr := 0;
  result := (decoder.Count <= 2) and (rule_ptr.attr <> -1) and (rule_ptr.not_attr <> -1);
  decoder.Destroy;
end;

function TTileset.rule_to_string(rule_ptr: TTileAtrRulePtr): string;
begin
  if rule_ptr.attr >= 0 then
    result := '$' + IntToHex(rule_ptr.attr, 1)
  else
    result := '-$' + IntToHex(rule_ptr.attr * -1, 1);
  if rule_ptr.not_attr <> 0 then
    result := result + ';$' + IntToHex(rule_ptr.not_attr, 1);
end;

end.
