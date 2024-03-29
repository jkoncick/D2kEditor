unit _tileset;

interface

uses Windows, Graphics, Classes, IniFiles, _utils;

// Tileset constants
const cnt_tileset_tiles = 800;
const cnt_paint_tile_groups = 8;
const cnt_block_preset_groups = 8;
const cnt_block_preset_keys = 40; // 0-9, A-Z...
const max_minimap_color_rules = 32;
const max_fill_area_rules = 16;
const max_paint_tiles = 128;
const max_block_presets = 640;
const max_block_preset_tiles = 1024;
//--const max_connection_points = 512;
//--const cnt_connection_point_types = 16;
//--const cnt_block_groups = 256;

// Constants for get_block_preset function
const bpNext = -1;
const bpRandom = -2;

// Constants for tileset image
const tileimage_width = 640;
const tileimage_height = 1280;
const r16_file_size = 1661600;
const r16_tile_header_size = 29;
const r16_tile_data_size = 32 * 32 * 2;
const r8_file_size = 842400;
const r8_tile_header_size = 29;
const r8_tile_data_size = 32 * 32;

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
  TTileAtrRule = record
    attr: int64;
    not_attr: int64;
  end;
  TTileAtrRulePtr = ^TTileAtrRule;

type
  TMinimapColorRule = record
    rule: TTileAtrRule;
    color: cardinal;
    color_8bit: byte; // Used by Dune2000 game only
    color_16bit: word; // Used by Dune2000 game only
  end;

type
  TFillAreaRule = record
    name: string;
    rule: TTileAtrRule;
  end;

type
  TPaintTileGroup = record
    name: string;
    tile_index: word;
    smooth_preset_group: shortint;
    paint_tiles_cnt: byte;
    restriction_rule: TTileAtrRule;
    smooth_presets: string;
    random_map_used: boolean;
    random_map: array of word;
  end;

type
  TBlockPresetGroup = record
    name: string;
    paint_group: integer;
  end;

type
  TBlockPresetVariantList = record
    first_preset_index: word;
    num_variants: byte;
  end;

type
  TBlockPreset = record
    width: byte;
    height: byte;
    block_preset_tile_index: word;
    block_group: byte;
    num_connection_points: byte;
    connection_point_index: word;
  end;

  PBlockPreset = ^TBlockPreset;

type
  TConnectionPoint = record
    type_and_direction: byte;
    offset: byte;
  end;

type
  TBlockGroup = record
    repeat_distance: byte;
    absolute_weight: word;
  end;

type
  TConnectionPointType = record
    paint_group: byte;
    connection_point_width: byte;
    connection_point_height: byte;
  end;

// Tileset class
type
  TTileset = class

  private
    last_block_preset_variant: integer;
    last_block_preset_group: integer;
    last_block_preset_key: word;

  public
    // List of available tilesets
    cnt_tilesets: integer;
    tileset_list: TStringList;
    tileatr_mapping: THashedStringList;

    // Status variables
    tileset_index: integer;
    tileset_name: String;
    tileatr_name: String;
    tileimage_filename: String;
    tileatr_filename: String;
    config_filename: String;

    // Tileset image variables
    tileimage: TBitmap;
    palette_loaded: Boolean;
    palette: PLogPalette;

    // Tile attribute variables
    attributes: array[0..cnt_tileset_tiles-1] of cardinal;
    tile_hint_text: array[0..cnt_tileset_tiles-1] of integer;
    attributes_editor: array[0..cnt_tileset_tiles-1] of byte;
    block_preset_coverage: array[0..cnt_tileset_tiles-1] of byte;
    tile_paint_group: array[0..cnt_tileset_tiles-1] of shortint;

    // Tileset configuration
    matching_tileatr_name: string;
    default_paint_group: integer;

    minimap_color_rules: array[0..max_minimap_color_rules-1] of TMinimapColorRule;
    minimap_color_rules_used: integer;

    fill_area_rules: array[0..max_fill_area_rules-1] of TFillAreaRule;
    fill_area_rules_used: integer;

    editor_attribute_names: array[0..7] of string;

    paint_tile_groups: array[-4..cnt_paint_tile_groups-1] of TPaintTileGroup;
    paint_tiles: array[0..max_paint_tiles-1] of word; // Tile numbers of clean sand/rock/dunes
    paint_tiles_used: integer;

    block_preset_groups: array[0..cnt_block_preset_groups-1] of TBlockPresetGroup;
    block_preset_key_variants: array[0..cnt_block_preset_groups-1, 0..cnt_block_preset_keys-1] of TBlockPresetVariantList;
    block_presets: array[0..max_block_presets-1] of TBlockPreset;
    block_presets_used: integer;
    block_preset_tiles: array[0..max_block_preset_tiles] of word;
    block_preset_tiles_used: integer;

    {--// Tileset configuration specific for random map generator
    connection_points: array[0..max_connection_points-1] of TConnectionPoint;
    connection_points_used: integer;
    block_groups: array[0..cnt_block_groups] of TBlockGroup;
    connection_point_types: array[0..cnt_connection_point_types-1] of TConnectionPointType;}

  public
    procedure init;
    procedure load_tileset_list;
    // Dispatcher procedures
    procedure update_tileset_index;
    // Changing tileset
    procedure change_tileset_by_index(index: integer);
    procedure change_tileset_by_name(p_tileset_name, p_tileatr_name: String);
    procedure change_tileset_to_default;
    procedure change_tileset_next;
    // Loading tileset parts
    procedure load_tileset(force_reload: boolean);
    // Loading tileset image
    procedure load_tileimage(p_tileset_name: string; force: boolean);
    procedure load_tileimage_from_file(filename: String);
    procedure load_r16_image(filename: String);
    procedure load_r8_image(filename: String);
    procedure load_palette;
    procedure load_bmp_image(filename: String);
    // Loading tile attributes
    procedure load_tileatr(p_tileatr_name: string; force: boolean);
    procedure load_tileatr_from_file(filename: String);
    procedure load_tileatr_bin_file(filename: String);
    // Load tileset configuration
    procedure load_config(p_tileset_name: string; use_default, force: boolean);
    function load_rule(rule: String; rule_ptr: TTileAtrRulePtr): boolean;
    // Saving tile attributes
    procedure save_tileatr;
    procedure save_tileatr_to_file(filename: String);
    // Radar color file for Dune2000 game
    procedure produce_radar_color_file;

    // Functions related to tileset configuration
    function get_paint_tile_group_char(group: integer): char;
    function get_tile_attributes(tile, special: word; use_internal_attributes: boolean): Int64;
    procedure set_tile_attributes(tile: word; value: Int64);
    function get_tile_type(tile: word): TileType;
    function get_tile_color(tile, special: word): Cardinal;
    function get_fill_area_type(tile: word; special: word): integer;
    function check_area_type(tile, special: word; area_type: integer): boolean;
    function check_paint_tile_restriction(tile, special: word; paint_tile_group: integer): boolean;
    function evaluate_rule(attr_value: int64; rule_ptr: TTileAtrRulePtr): boolean;
    function block_key_to_index(key: word): integer;
    function get_random_paint_tile(group, x, y: integer): integer;
    function get_block_preset(group: integer; key: word; variant: integer): integer;
  end;

var
  Tileset: TTileset;

implementation

uses Forms, SysUtils, StrUtils, Math, _map, _settings, _dispatcher;

procedure TTileset.init;
begin
  tileset_index := -1;
  tileimage := Graphics.TBitmap.Create;
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
  tmp_tileset_pretty_name: string;
  tmp_tileset_name: string;
  tmp_tileatr_name: string;
  i: integer;
begin
  tmp_strings := TStringList.Create;
  if FindFirst(current_dir + 'tilesets\*.ini', 0, SR) = 0 then
  begin
    repeat
      if SR.Name <> 'template.ini' then
        tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  cnt_tilesets := tmp_strings.Count;
  tileset_list := TStringList.Create;
  tileatr_mapping := THashedStringList.Create;
  tileset_list.Capacity := cnt_tilesets;
  tileatr_mapping.Capacity := cnt_tilesets;
  for i := 0 to cnt_tilesets -1 do
  begin
    tmp_inifile := TIniFile.Create(current_dir+'\tilesets\'+tmp_strings[i]);
    tmp_tileset_name := ChangeFileExt(tmp_strings[i], '');
    tmp_tileset_pretty_name := tmp_inifile.ReadString('Basic','name', tmp_tileset_name);
    tmp_tileatr_name := tmp_inifile.ReadString('Basic','tileatr', '');
    tileset_list.Add(tmp_tileset_pretty_name + '=' + tmp_tileset_name);
    tileatr_mapping.Add(tmp_tileset_name + '=' + tmp_tileatr_name);
    tmp_inifile.Destroy;
  end;
  tmp_strings.Destroy;
  tileset_list.Sort;
  Dispatcher.register_event(evFLLTilesetList);
end;

procedure TTileset.update_tileset_index;
var
  i: integer;
begin
  tileset_index := -1;
  // Try to find the tileset in list of known tilesets
  for i := 0 to cnt_tilesets-1 do
    // Case-insensitive string compare
    if AnsiCompareText(tileset_name, tileset_list.ValueFromIndex[i]) = 0 then
    begin
      tileset_index := i;
      break;
    end;
  // Check if corresponding tileatr name matches
  if (tileset_index <> -1) and (AnsiCompareText(tileatr_mapping.Values[tileset_name], tileatr_name) <> 0) then
    tileset_index := -1;
end;

procedure TTileset.change_tileset_by_index(index: integer);
begin
  if (index >= cnt_tilesets) or (index < 0) then
    exit;
  tileset_name := tileset_list.ValueFromIndex[index];
  tileatr_name := tileatr_mapping.Values[tileset_name];
  load_tileset(false);
end;

procedure TTileset.change_tileset_by_name(p_tileset_name, p_tileatr_name: String);
begin
  tileset_name := p_tileset_name;
  tileatr_name := p_tileatr_name;
  load_tileset(false);
end;

procedure TTileset.change_tileset_to_default;
begin
  change_tileset_by_name(Settings.DefaultTilesetName, tileatr_mapping.Values[Settings.DefaultTilesetName]);
end;

procedure TTileset.change_tileset_next;
begin
  if cnt_tilesets = 0 then
    exit;
  change_tileset_by_index((tileset_index + 1) mod cnt_tilesets);
end;

procedure TTileset.load_tileset(force_reload: boolean);
begin
  load_tileimage(tileset_name, force_reload);
  load_tileatr(tileatr_name, force_reload);
  load_config(tileset_name, true, force_reload);
  // Failsafe to default tileset if tileset image/attributes could not be loaded
  if (tileimage_filename = '') and (tileset_name <> Settings.DefaultTilesetName) then
    load_tileimage(Settings.DefaultTilesetName, false);
  if (tileatr_filename = '') and (tileset_name <> Settings.DefaultTilesetName) then
    load_tileatr(tileatr_mapping.Values[Settings.DefaultTilesetName], false);
end;

procedure TTileset.load_tileimage(p_tileset_name: string; force: boolean);
var
  tmp_filename: string;
begin
  // Try to find R16 file
  if Settings.LoadR16Image then
  begin
    tmp_filename := find_file('Data\' + p_tileset_name + '.R16', '');
    if (tmp_filename <> '') and (tmp_filename = tileimage_filename) and not force then
      exit;
    if tmp_filename <> '' then
    begin
      load_r16_image(tmp_filename);
      exit;
    end;
  end;
  // Try to find R8 file
  if Settings.LoadR8Image then
  begin
    tmp_filename := find_file('Data\' + p_tileset_name + '.R8', '');
    if (tmp_filename <> '') and (tmp_filename = tileimage_filename) and not force then
      exit;
    if tmp_filename <> '' then
    begin
      load_r8_image(tmp_filename);
      exit;
    end;
  end;
  // Try to find bmp file
  tmp_filename := find_file('Data\' + p_tileset_name + '.bmp', '');
  if (tmp_filename <> '') and (tmp_filename = tileimage_filename) and not force then
    exit;
  if tmp_filename <> '' then
  begin
    load_bmp_image(tmp_filename);
    exit;
  end;
  Application.MessageBox(PChar('Could not find image file for tileset ' + p_tileset_name), 'Error loading tileset graphics', MB_OK or MB_ICONERROR);
end;

procedure TTileset.load_tileimage_from_file(filename: String);
var
  ext: String;
begin
  if (filename = '') or not FileExists(filename) then
    exit;
  Ext := UpperCase(ExtractFileExt(filename));
  if Ext = '.R16' then
    load_r16_image(filename)
  else if Ext = '.R8' then
    load_r8_image(filename)
  else if Ext = '.BMP' then
    load_bmp_image(filename);
  // Check if load was succesful
  if tileimage_filename <> filename then
    exit;
  // Load tileset configuration
  tileset_name := UpperCase(ChangeFileExt(ExtractFileName(filename), ''));
  load_config(tileset_name, false, false);
end;

procedure TTileset.load_r16_image(filename: String);
var
  f: file of byte;
  r16_file_buffer: array of byte;
  r16_tile_data_buffer: TWordArrayPtr;
  r16_tileset_data_buffer: TWordArrayPtr;
  i, x, y, offset: integer;
begin
  // Load file into buffer
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  if (filesize(f) <> r16_file_size) then
  begin
    Application.MessageBox(PChar('Error loading tileset image from ' + filename + ':'#13'File does not have expected size (' + inttostr(r16_file_size) + ' bytes)'), 'Error loading tileset graphics', MB_OK or MB_ICONERROR);
    Close(f);
    exit;
  end;
  SetLength(r16_file_buffer, r16_file_size);
  BlockRead(f, r16_file_buffer[0], r16_file_size);
  Close(f);
  // Arrange image data into tileset image
  tileimage.Width := tileimage_width;
  tileimage.Height := tileimage_height;
  tileimage.PixelFormat := pf15bit;
  r16_tileset_data_buffer := tileimage.ScanLine[tileimage_height-1];
  for i := 0 to cnt_tileset_tiles-1 do
  begin
    offset := i * (r16_tile_header_size + r16_tile_data_size) + r16_tile_header_size;
    r16_tile_data_buffer := Addr(r16_file_buffer[offset]);
    for y := 0 to 31 do
      for x := 0 to 31 do
      begin
        r16_tileset_data_buffer[x + ((i mod 20) * 32) + ((tileimage_height - (y + ((i div 20) * 32)) - 1) * tileimage_width)] := r16_tile_data_buffer[x + y*32];
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
  r8_file_buffer: array of byte;
  r8_tile_data_buffer: TByteArrayPtr;
  r8_tileset_data_buffer: TByteArrayPtr;
  i, x, y, offset: integer;
begin
  // Load file into buffer
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  if (filesize(f) <> r8_file_size) then
  begin
    Application.MessageBox(PChar('Error loading tileset image from ' + filename + ':'#13'File does not have expected size (' + inttostr(r8_file_size) + ' bytes)'), 'Error loading tileset graphics', MB_OK or MB_ICONERROR);
    Close(f);
    exit;
  end;
  SetLength(r8_file_buffer, r8_file_size);
  BlockRead(f, r8_file_buffer[0], r8_file_size);
  Close(f);
  // Arrange image data into tileset image
  tileimage.Width := tileimage_width;
  tileimage.Height := tileimage_height;
  tileimage.PixelFormat := pf8bit;
  load_palette;
  if palette_loaded then
    tileimage.Palette := CreatePalette(palette^);
  r8_tileset_data_buffer := tileimage.ScanLine[tileimage_height-1];
  for i := 0 to cnt_tileset_tiles-1 do
  begin
    offset := i * (r8_tile_header_size + r8_tile_data_size) + r8_tile_header_size;
    r8_tile_data_buffer := Addr(r8_file_buffer[offset]);
    for y := 0 to 31 do
      for x := 0 to 31 do
      begin
        r8_tileset_data_buffer[x + ((i mod 20) * 32) + ((tileimage_height - (y + ((i div 20) * 32)) - 1) * tileimage_width)] := r8_tile_data_buffer[x + y*32];
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
    Application.MessageBox(PChar('Error loading palette. Could not find file ' + filename), 'Error loading tileset graphics', MB_OK or MB_ICONERROR);
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

procedure TTileset.load_bmp_image(filename: String);
begin
  // Load bmp file
  tileimage.LoadFromFile(filename);
  tileimage.PixelFormat := pf32bit;
  // Update file name
  tileimage_filename := filename;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTilesetImage);
end;

procedure TTileset.load_tileatr(p_tileatr_name: string; force: boolean);
var
  tmp_filename: string;
begin
  tmp_filename := find_file('Data\bin\' + p_tileatr_name + '.BIN', 'tile attributes');
  if (tmp_filename = '') or ((tmp_filename = tileatr_filename) and not force) then
    exit;
  load_tileatr_bin_file(tmp_filename);
end;

procedure TTileset.load_tileatr_from_file(filename: String);
begin
  if (filename = '') or not FileExists(filename) then
    exit;
  tileatr_name := UpperCase(ChangeFileExt(ExtractFileName(filename), ''));
  load_tileatr_bin_file(filename);
end;

procedure TTileset.load_tileatr_bin_file(filename: String);
var
  tileatr_file: file of cardinal;
begin
  // Load TILEATR.BIN file
  AssignFile(tileatr_file, filename);
  Reset(tileatr_file);
  if FileSize(tileatr_file) <> (cnt_tileset_tiles * 2) then
  begin
    Application.MessageBox(PChar('Error loading tile attributes from ' + filename + ':'#13'File does not have expected size (' + inttostr(cnt_tileset_tiles * 2 * 4) + ' bytes)'), 'Error loading tile attributes', MB_OK or MB_ICONERROR);
    CloseFile(tileatr_file);
    exit;
  end;
  BlockRead(tileatr_file, attributes, cnt_tileset_tiles);
  BlockRead(tileatr_file, tile_hint_text, cnt_tileset_tiles);
  CloseFile(tileatr_file);
  // Update file name
  tileatr_filename := filename;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTileatrBin);
end;

procedure TTileset.load_config(p_tileset_name: string; use_default, force: boolean);
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
  random_map: String;
  map_filename: String;
  map_file: file of word;
  map_file_buffer: array[0..max_map_width*max_map_height*2-1] of word;
  map_width, map_height: word;
begin
  tmp_filename := find_file('tilesets\' + p_tileset_name + '.ini', '');
  if (tmp_filename <> '') and (tmp_filename = config_filename) and not force then
    exit;
  if tmp_filename = '' then
  begin
    if not use_default then
      exit;
    Application.MessageBox(PChar('Could not find tileset configuration ini file for tileset ' + p_tileset_name + '. Loading default tileset configuration instead.'), 'Error loading tileset configuration', MB_ICONWARNING or MB_OK);
    tmp_filename := find_file('tilesets\template.ini', 'tileset configuration');
    if (tmp_filename = '') or (tmp_filename = config_filename) then
      exit;
  end;
  config_filename := tmp_filename;

  ini := TMemIniFile.Create(tmp_filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder2 := TStringList.Create;
  decoder.Delimiter := ';';
  decoder2.Delimiter := '.';
  // Load basic information
  matching_tileatr_name := ini.ReadString('Basic', 'tileatr', '');
  default_paint_group := ini.ReadInteger('Basic', 'default_paint_group', 1) - 1;
  config_version := ini.ReadInteger('Basic', 'version', 0);
  if config_version <> CURRENT_TILESET_CONFIG_VERSION then
    Application.MessageBox(PChar(
      'The tileset configuration file ''' + config_filename + ''''#13 +
      'was made for different version of D2kEditor and may be incompatible with the version you use.'#13 +
      'Please get the latest available version of tileset configuration ini file or fix your file.'#13 +
      'You can get one or ask for help on FED2k forums.'),
      'Tileset configuration warning', MB_ICONWARNING + MB_OK);
  // Load minimap color rules
  minimap_color_rules_used := 0;
  ini.ReadSection('Minimap_Color_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_minimap_color_rules then
      break;
    minimap_color_rules[i].color := strtoint(tmp_strings[i]);
    load_rule(ini.ReadString('Minimap_Color_Rules', tmp_strings[i], '0'), Addr(minimap_color_rules[i].rule));
    inc(minimap_color_rules_used);
  end;
  // Load fill area rules
  fill_area_rules_used := 0;
  ini.ReadSection('Fill_Area_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_fill_area_rules then
      break;
    fill_area_rules[i].name := tmp_strings[i];
    load_rule(ini.ReadString('Fill_Area_Rules', tmp_strings[i], '0'), Addr(fill_area_rules[i].rule));
    inc(fill_area_rules_used);
  end;
  // Load editor attribute names
  for i := 0 to 7 do
    editor_attribute_names[i] := ini.ReadString('Editor_Tile_Attribute_Names', 'Attribute'+inttostr(i+1), 'Editor Attribute '+inttostr(i+1));
  // Reset editor attributes
  for i := 0 to cnt_tileset_tiles - 1 do
    attributes_editor[i] := 0;
  // Load editor attributes
  for i := 0 to 7 do
  begin
    decoder.DelimitedText := ini.ReadString('Editor_Tile_Attributes', 'Attribute'+inttostr(i+1), '');
    for j := 0 to decoder.Count-1 do
    begin
      tile := strtoint(decoder[j]);
      if (tile >= 0) and (tile < cnt_tileset_tiles) then
        attributes_editor[tile] := attributes_editor[tile] or (1 shl i)
    end;
  end;
  // Load paint tile groups
  paint_tiles_used := 0;
  for i := 0 to cnt_tileset_tiles - 1 do
    tile_paint_group[i] := -128;
  for i := -4 to cnt_paint_tile_groups - 1 do
  begin
    group_name := 'Group' + get_paint_tile_group_char(i);
    paint_tile_groups[i].name := ini.ReadString('Paint_Tile_Groups', group_name+'.name', '');
    paint_tile_groups[i].tile_index := ini.ReadInteger('Paint_Tile_Groups', group_name+'.tile', 0);
    paint_tile_groups[i].paint_tiles_cnt := 0;
    decoder.DelimitedText := ini.ReadString('Paint_Tile_Groups', group_name+'.paint_tiles', '');
    for j := 0 to decoder.Count-1 do
    begin
      if paint_tiles_used < max_paint_tiles then
      begin
        tile := strtoint(decoder[j]);
        paint_tiles[paint_tiles_used] := tile;
        inc(paint_tile_groups[i].paint_tiles_cnt);
        inc(paint_tiles_used);
        tile_paint_group[tile] := i;
      end;
    end;
    if (decoder.Count = 0) and (i >= -2) and (paint_tile_groups[i].name <> '') then
      tile_paint_group[paint_tile_groups[i].tile_index] := i;
    paint_tile_groups[i].smooth_preset_group := ini.ReadInteger('Paint_Tile_Groups', group_name+'.smooth_preset_group', 0) - 1;
    paint_tile_groups[i].smooth_presets := ini.ReadString('Paint_Tile_Groups', group_name+'.smooth_presets', '');
    load_rule(ini.ReadString('Paint_Tile_Groups', group_name+'.restriction_rule', '0'), Addr(paint_tile_groups[i].restriction_rule));
    // Load random paint background map
    paint_tile_groups[i].random_map_used := false;
    random_map := ini.ReadString('Paint_Tile_Groups', group_name+'.random_map', '');
    map_filename := current_dir + 'tilesets\' + random_map + '.map';
    if (random_map <> '') and FileExists(map_filename) then
    begin
      AssignFile(map_file, map_filename);
      Reset(map_file);
      Read(map_file, map_width);
      Read(map_file, map_height);
      if (map_width = max_map_width) and (map_height = max_map_height) then
      begin
        SetLength(paint_tile_groups[i].random_map, max_map_width * max_map_height);
        BlockRead(map_file, map_file_buffer[0], Length(map_file_buffer));
        for j := 0 to max_map_width * max_map_height - 1 do
          paint_tile_groups[i].random_map[j] := map_file_buffer[j*2];
        paint_tile_groups[i].random_map_used := true;
      end;
      CloseFile(map_file);
    end;
    if not paint_tile_groups[i].random_map_used then
      SetLength(paint_tile_groups[i].random_map, 0);
  end;
  // Load block preset groups
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    block_preset_groups[i].name := ini.ReadString('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.name', '');
    block_preset_groups[i].paint_group := ini.ReadInteger('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.paint', -4) - 1;
  end;
  // Load block presets
  preset_index := 1;
  preset_tile_index := 0;
  for i := 0 to cnt_tileset_tiles - 1 do
    block_preset_coverage[i] := 0;
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
      decoder.DelimitedText := ini.ReadString('Block_Preset_Group_'+(inttostr(i+1)), key, '');
      block_preset_key_variants[i, j].num_variants := decoder.Count;
      block_preset_key_variants[i, j].first_preset_index := preset_index;
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
        block_presets[preset_index].block_preset_tile_index := preset_tile_index;
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
                inc(block_preset_coverage[tile]);
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
              if tile <> -1 then
                inc(block_preset_coverage[tile]);
            end;
        end;
        inc(preset_tile_index, width * height);
        inc(preset_index);
      end;
    end;
  end;
  block_presets_used := preset_index;
  block_preset_tiles_used := preset_tile_index;

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

  // Register event in dispatcher
  Dispatcher.register_event(evFLTilesetIni);
end;

function TTileset.load_rule(rule: String; rule_ptr: TTileAtrRulePtr): boolean;
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

procedure TTileset.save_tileatr;
var
  tileatr_file: file of cardinal;
  lines_input, lines_output, encoder: TStringList;
  i, j, found_editor_attr_section, found_next_section: integer;
  line: String;
begin
  // Save TILEATR*.BIN file
  if tileatr_filename = '' then
    exit;
  if not manage_filesave(tileatr_filename, 'Data\bin\' + tileatr_name + '.BIN', evTileatrFilenameChange) then
    exit;
  AssignFile(tileatr_file, tileatr_filename);
  ReWrite(tileatr_file);
  BlockWrite(tileatr_file, attributes, cnt_tileset_tiles);
  BlockWrite(tileatr_file, tile_hint_text, cnt_tileset_tiles);
  CloseFile(tileatr_file);
  // Save editor attributes into tileset .ini file
  if (config_filename = '') or (AnsiCompareText(tileatr_filename, Settings.GamePath + '\Data\bin\' + matching_tileatr_name + '.BIN') <> 0) then
    exit;
  lines_input := TStringList.Create;
  lines_output := TStringList.Create;
  encoder := TStringList.Create;
  encoder.Delimiter := ';';
  lines_input.LoadFromFile(config_filename);
  // Find existing [Editor_Tile_Attributes] section and next section
  found_editor_attr_section := -1;
  found_next_section := -1;
  for i := 0 to lines_input.Count - 1 do
  begin
    if lines_input[i] = '[Editor_Tile_Attributes]' then
      found_editor_attr_section := i;
    if lines_input[i] = '[Paint_Tile_Groups]' then
      found_next_section := i;
  end;
  if found_next_section = -1 then
    found_next_section := lines_input.Count; // Append to end of file if not found
  if found_editor_attr_section = -1 then
    found_editor_attr_section := found_next_section; // Place before next section
  // Copy contents of input file to output till place where we should put [Editor_Tile_Attributes] section
  for i := 0 to found_editor_attr_section - 1 do
    lines_output.Add(lines_input[i]);
  // Write [Editor_Tile_Attributes] section
  lines_output.Add('[Editor_Tile_Attributes]');
  for i := 0 to 7 do
  begin
    encoder.Clear;
    for j := 0 to cnt_tileset_tiles-1 do
    begin
      if (attributes_editor[j] and (1 shl i)) <> 0 then
        encoder.Add(inttostr(j));
    end;
    if encoder.Count > 0 then
      lines_output.Add('Attribute' + inttostr(i+1) + '=' + encoder.DelimitedText);
  end;
  if found_editor_attr_section = found_next_section then
    lines_output.Add('');
  // Copy rest of input file contents while skipping old [Editor_Tile_Attributes] section
  for i := found_editor_attr_section to lines_input.Count - 1 do
    begin
      line := lines_input[i];
      if (line <> '[Editor_Tile_Attributes]') and not AnsiStartsStr('Attribute', line) then
        lines_output.Add(line);
    end;
  lines_output.SaveToFile(config_filename);
  lines_input.Destroy;
  lines_output.Destroy;
  encoder.Destroy;
end;

procedure TTileset.save_tileatr_to_file(filename: String);
begin
  tileatr_name := UpperCase(ChangeFileExt(ExtractFileName(filename), ''));
  tileatr_filename := filename;
  save_tileatr;
  // Register event in dispatcher
  Dispatcher.register_event(evTileatrFilenameChange);
end;

procedure TTileset.produce_radar_color_file;
var
  radar_color_file: file of byte;
  filename: string;
  extra_attributes: array[0..cnt_tileset_tiles-1] of cardinal;
  i: integer;
begin
  filename := ChangeFileExt(tileimage_filename, '.rcl');
  AssignFile(radar_color_file, filename);
  ReWrite(radar_color_file);
  BlockWrite(radar_color_file, minimap_color_rules_used, sizeof(minimap_color_rules_used));
  BlockWrite(radar_color_file, minimap_color_rules, sizeof(minimap_color_rules));
  for i := 0 to cnt_tileset_tiles - 1 do
  begin
    extra_attributes[i] := attributes_editor[i];
    if tile_paint_group[i] <> -128 then
    begin
      if tile_paint_group[i] >= 0 then
        extra_attributes[i] := extra_attributes[i] or (1 shl (tile_paint_group[i] + 8))
      else
        extra_attributes[i] := extra_attributes[i] or (1 shl (tile_paint_group[i] + 20));
    end;
  end;
  BlockWrite(radar_color_file, extra_attributes, sizeof(extra_attributes));
  CloseFile(radar_color_file);
  Application.MessageBox(PChar('Radar color rules were saved into ' + filename), 'Produce radar color file', MB_ICONINFORMATION);
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
  editor_attributes, paint_tile_group_attributes, tile_property_attributes: int64;
  spice_amount: integer;
begin
  // Standard game attributes: 32 bits
  result := attributes[tile and $0FFF];
  // Explicit editor attributes (Area type 1-8): 8 bits
  editor_attributes := attributes_editor[tile and $0FFF];
  editor_attributes := (editor_attributes shl 32) and $000000FF00000000;
  result := result or editor_attributes;
  if use_internal_attributes then
  begin
    // Internal attributes (paint tile group 1-8, A-D): 12 bits
    paint_tile_group_attributes := 0;
    if tile_paint_group[tile and $0FFF] <> -128 then
    begin
      if tile_paint_group[tile and $0FFF] >= 0 then
        paint_tile_group_attributes := paint_tile_group_attributes or (1 shl (tile_paint_group[tile and $0FFF] + 8))
      else
        paint_tile_group_attributes := paint_tile_group_attributes or (1 shl (tile_paint_group[tile and $0FFF] + 20))
    end;
    paint_tile_group_attributes := (paint_tile_group_attributes shl 32) and $000FFF0000000000;
    result := result or paint_tile_group_attributes;
    // Internal attributes (thin spice, thick spice, concrete): 3 bits
    spice_amount := IfThen((tile and $1000) = 0, (tile shr 13) and 7, 0);
    tile_property_attributes := 0;
    if (special = 1) or (spice_amount = 1) or (spice_amount = 2) then
      tile_property_attributes := $00010000;
    if (special = 2) or (spice_amount >= 3) then
      tile_property_attributes := $00020000;
    if ((tile and $1000) <> 0) then
      tile_property_attributes := $00040000;
    tile_property_attributes := (tile_property_attributes shl 32) and $0007000000000000;
    result := result or tile_property_attributes;
  end;
end;

procedure TTileset.set_tile_attributes(tile: word; value: Int64);
begin
  attributes[tile] := value and $FFFFFFFF;
  attributes_editor[tile] := (value shr 32) and $FF;
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

function TTileset.get_tile_color(tile, special: word): Cardinal;
var
  i: integer;
  attr_value: int64;
begin
  attr_value := get_tile_attributes(tile, special, true);
  for i := 0 to minimap_color_rules_used - 1 do
  begin
    if evaluate_rule(attr_value, Addr(minimap_color_rules[i].rule)) then
    begin
      result := minimap_color_rules[i].color;
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

function TTileset.block_key_to_index(key: word): integer;
begin
  if (key >= ord('0')) and (key <= ord('9')) then
    result := key - ord('0')
  else if (key >= ord('A')) and (key <= ord('Z')) then
    result := key - ord('A') + 10
  else if (key = 188) or (key = ord('<')) then
    result := 36
  else if (key = 190) or (key = ord('>')) then
    result := 37
  else if (key = 186) or (key = ord(':')) then
    result := 38
  else if (key = 191) or (key = ord('?')) then
    result := 39
  else
    result := -1;
end;

function TTileset.get_random_paint_tile(group, x, y: integer): integer;
var
  i: integer;
  start_index: integer;
begin
  if group >= cnt_paint_tile_groups then
  begin
    result := 0;
    exit;
  end;
  if Settings.UseRandomPaintMap and paint_tile_groups[group].random_map_used then
  begin
    result := paint_tile_groups[group].random_map[x + y * max_map_width];
    exit;
  end;
  if paint_tile_groups[group].paint_tiles_cnt = 0 then
  begin
    result := paint_tile_groups[group].tile_index;
    exit;
  end;
  start_index := 0;
  for i := -4 to group - 1 do
    inc(start_index, paint_tile_groups[i].paint_tiles_cnt);
  result := paint_tiles[random(paint_tile_groups[group].paint_tiles_cnt) + start_index];
end;

function TTileset.get_block_preset(group: integer; key: word; variant: integer): integer;
var
  num_variants: integer;
  key_index, preset_index: integer;
begin
  key_index := block_key_to_index(key);
  // Unknown key
  if key_index = -1 then
  begin
    result := 0;
    exit;
  end;
  num_variants := block_preset_key_variants[group, key_index].num_variants;
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
      if (last_block_preset_group <> group) or (last_block_preset_key <> key) then
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
      until (variant <> last_block_preset_variant) or (last_block_preset_key <> key);
      last_block_preset_variant := variant;
    end;
  end;
  preset_index := block_preset_key_variants[group, key_index].first_preset_index + variant;
  last_block_preset_group := group;
  last_block_preset_key := key;
  result := preset_index;
end;

end.
