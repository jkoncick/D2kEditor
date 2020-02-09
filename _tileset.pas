unit _tileset;

interface

uses Windows, Graphics;

// Tileset constants
const cnt_tileset_tiles = 800;
const cnt_paint_tile_groups = 4;
const cnt_block_preset_groups = 8;
const cnt_block_preset_keys = 40; // 0-9, A-Z...
const max_tile_color_rules = 32;
const max_fill_area_rules = 16;
const max_paint_tiles = 64;
const max_block_presets = 512;
const max_block_preset_tiles = 1024;
const max_connection_points = 512;
const cnt_connection_point_types = 16;
const cnt_block_groups = 256;

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

type
  WordArr = array[0..0] of word;
  PWordArr = ^WordArr;
  ByteArr = array[0..0] of byte;
  PByteArr = ^ByteArr;

// Tileset attributes (game)
const taVehiclesPass = $2000;
const taInfantryPass = $4000;
const taAnyPass = $6000;
const taBuildable = $8000;
const taSand = $10000;
const taRock = $20000000;
const taDunes = $40000000;
const taRoughRock = $80000000;

// Tileset type definitions
type
  TileType = (ttPassable, ttImpassable, ttInfantryOnly, ttBuildable);

type
  TTileColorRule = record
    color: TColor;
    attr: int64;
    not_attr: int64;
  end;

type
  TFillAreaRule = record
    attr: int64;
    not_attr: int64;
  end;

type
  TPaintTileGroup = record
    name: string;
    tile_index: integer;
    smooth_group: integer;
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

  public
    // List of available tilesets
    cnt_tilesets: integer;
    tileset_list: array of String;

    // Status variables
    current_tileset: integer;
    tileset_name: String;
    tileatr_name: String;
    tileimage_filename: String;
    tileatr_filename: String;
    config_filename: String;

    // Tileset image variables
    tileimage: TBitmap;
    palette_loaded: Boolean;
    palette: PLogPalette;

    // Tileset attribute variables
    attributes: array[0..(cnt_tileset_tiles*2)-1] of cardinal;
    attributes_editor: array[0..cnt_tileset_tiles-1] of byte;
    block_preset_coverage: array[0..cnt_tileset_tiles-1] of byte;

    // Tileset configuration
    tile_color_rules: array[0..max_tile_color_rules-1] of TTileColorRule;
    tile_color_rules_used: integer;

    fill_area_rules: array[0..max_tile_color_rules-1] of TFillAreaRule;
    fill_area_rules_used: integer;

    thin_spice_tile: integer;
    thin_spice_color: TColor;
    thin_spice_name: String;
    thick_spice_tile: integer;
    thick_spice_color: TColor;
    thick_spice_name: String;
    spice_restriction_attr: int64;
    spice_restriction_not_attr: int64;

    paint_tile_groups: array[0..cnt_paint_tile_groups-1] of TPaintTileGroup;
    paint_tiles: array[0..max_paint_tiles-1] of word; // Tile numbers of clean sand/rock/dunes
    paint_tiles_cnt: array[0..cnt_paint_tile_groups-1] of integer;
    paint_tiles_used: integer;

    block_preset_groups: array[0..cnt_block_preset_groups-1] of TBlockPresetGroup;
    block_preset_key_variants: array[0..cnt_block_preset_groups-1, 0..cnt_block_preset_keys-1] of TBlockPresetVariantList;
    block_presets: array[0..max_block_presets-1] of TBlockPreset;
    block_presets_used: integer;
    block_preset_tiles: array[0..max_block_preset_tiles] of word;
    block_preset_tiles_used: integer;

    // Tileset configuration specific for random map generator
    connection_points: array[0..max_connection_points-1] of TConnectionPoint;
    connection_points_used: integer;
    block_groups: array[0..cnt_block_groups] of TBlockGroup;
    connection_point_types: array[0..cnt_connection_point_types-1] of TConnectionPointType;

  public
    procedure init;
    procedure change_tileset(index: integer);
    procedure change_tileset_by_name(name: String);
    procedure next_tileset;

    procedure load_image_from_file(filename: String);
    procedure load_attributes_from_file(filename: String);
    procedure save_attributes_to_file(filename: String);
    procedure reload_attributes;
  private
    procedure load_tileimage;
    procedure load_r16_image(filename: String);
    procedure load_r8_image(filename: String);
    procedure load_palette;
    procedure load_attributes;
    procedure load_config;
  public
    procedure save_attributes;
    procedure convert_editor_attributes;
    procedure init_paint_tiles_from_editor_attributes;

    function get_tile_attributes(tile: word): Int64;
    procedure set_tile_attributes(tile: word; value: Int64);
    function get_tile_type(tile: word): TileType;
    function get_tile_color(tile: word): TColor;
    function get_fill_area_type(tile: word; special: word): integer;
    function check_spice_can_be_placed(tile: word): boolean;

    function block_key_to_index(key: word): integer;

    function get_random_paint_tile(group: integer): integer;
    function get_block_preset(group: integer; key: word; variant: integer): integer;

    function get_status: String;
  end;

var
  Tileset: TTileset;

implementation

uses Forms, SysUtils, main, _settings, IniFiles, Classes, StrUtils;

procedure TTileset.init;
var
  SR: TSearchRec;
  tmp_strings: TStringList;
  i: integer;
begin
  current_tileset := -1;
  tileset_name := 'BLOXBGBS';
  tileatr_name := 'TILEATR3';
  tileimage := Graphics.TBitmap.Create;
  palette_loaded := false;
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
  SetLength(tileset_list, cnt_tilesets);
  for i := 0 to cnt_tilesets -1 do
  begin
    tileset_list[i] := ChangeFileExt(tmp_strings[i], '');
  end;
  tmp_strings.Destroy;
  // Initialize empty block preset
  block_presets[0].width := 0;
  block_presets[0].height := 0;
end;

procedure TTileset.change_tileset(index: integer);
var
  fn_tileatr_editor, fn_tileatr_game: String;
begin
  if index = current_tileset then
    exit;
  if (index >= cnt_tilesets) or (index < 0) then
    exit;
  current_tileset := index;
  tileset_name := tileset_list[index];
  // Load tileset configuration
  config_filename := current_dir+'\tilesets\'+tileset_name+'.ini';
  load_config;
  // Load tileset attributes
  fn_tileatr_game := Settings.GamePath+'\Data\bin\'+tileatr_name+'.BIN';
  fn_tileatr_editor := current_dir+'\tilesets\'+tileatr_name+'.BIN';
  if FileExists(fn_tileatr_game) then
  begin
    tileatr_filename := fn_tileatr_game;
    load_attributes;
  end
  else if FileExists(fn_tileatr_editor) then
  begin
    tileatr_filename := fn_tileatr_editor;
    load_attributes;
  end else
    Application.MessageBox(PChar('Could not find tileset attributes file ' + tileatr_name + '.BIN'), 'Error loading tileset attributes', MB_OK or MB_ICONWARNING);
  // Load tileset image
  load_tileimage;
  // Redraw terrain editing GUI and dialogs
  MainWindow.tileset_changed;
  MainWindow.render_tileset;
end;

procedure TTileset.change_tileset_by_name(name: String);
var
  i: integer;
begin
  for i:= 0 to cnt_tilesets-1 do
  begin
    // Case-insensitive string compare
    if AnsiCompareText(name, tileset_list[i]) = 0 then
    begin
      change_tileset(i);
      exit;
    end;
  end;
  Application.MessageBox(PChar('Mission has unknown tileset: ' + name), 'Error loading tileset', MB_OK or MB_ICONWARNING);
  // Failsafe to default tileset if tileset is unknown
  if current_tileset = -1 then
  begin
    if name <> Settings.DefaultTilesetName then
      change_tileset_by_name(Settings.DefaultTilesetName)
    else
      change_tileset(0);
  end;
end;

procedure TTileset.next_tileset;
var
  new_tileset: integer;
begin
  new_tileset := current_tileset + 1;
  if new_tileset >= cnt_tilesets then
    new_tileset := 0;
  change_tileset(new_tileset);
end;

procedure TTileset.load_image_from_file(filename: String);
var
  ext: String;
begin
  Ext := UpperCase(ExtractFileExt(filename));
  if Ext = '.R16' then
    load_r16_image(filename)
  else if Ext = '.R8' then
    load_r8_image(filename)
  else begin
    tileimage.LoadFromFile(filename);
    tileimage_filename := filename;
  end;
  current_tileset := -1;
end;

procedure TTileset.load_attributes_from_file(filename: String);
begin
  tileatr_filename := filename;
  config_filename := '';
  current_tileset := -1;
  load_attributes;
end;

procedure TTileset.save_attributes_to_file(filename: String);
begin
  tileatr_filename := filename;
  config_filename := '';
  current_tileset := -1;
  save_attributes;
end;

procedure TTileset.reload_attributes;
begin
  load_attributes;
  load_config;
end;

procedure TTileset.load_tileimage();
var
  fn_r16_game, fn_r8_game, fn_r16_editor, fn_r8_editor, fn_bmp_editor: String;
begin
  fn_r16_game := Settings.GamePath+'\Data\'+tileset_name+'.r16';
  fn_r8_game := Settings.GamePath+'\Data\'+tileset_name+'.r8';
  fn_r16_editor := current_dir+'\tilesets\'+tileset_name+'.r16';
  fn_r8_editor := current_dir+'\tilesets\'+tileset_name+'.r8';
  fn_bmp_editor := current_dir+'\tilesets\'+tileset_name+'.bmp';
  // Try to load tileset image in given order
  if FileExists(fn_r16_game) and Settings.LoadR16Image then
    load_r16_image(fn_r16_game)
  else if FileExists(fn_r8_game) and Settings.LoadR8Image then
    load_r8_image(fn_r8_game)
  else if FileExists(fn_r16_editor) and Settings.LoadR16Image then
    load_r16_image(fn_r16_editor)
  else if FileExists(fn_r8_editor) and Settings.LoadR8Image then
    load_r8_image(fn_r8_editor)
  else if FileExists(fn_bmp_editor) then
  begin
    tileimage.LoadFromFile(fn_bmp_editor);
    tileimage_filename := fn_bmp_editor;
  end else
    Application.MessageBox(PChar('Could not find image file for tileset ' + tileset_name), 'Error loading tileset', MB_OK or MB_ICONWARNING);
end;

procedure TTileset.load_r16_image(filename: String);
var
  f: file of byte;
  r16_file_buffer: array of byte;
  r16_tile_data_buffer: PWordArr;
  r16_tileset_data_buffer: PWordArr;
  i, x, y, offset: integer;
begin
  // Load file into buffer
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  if (filesize(f) <> r16_file_size) then
  begin
    Application.MessageBox(PChar('Error loading tileset image from ' + filename + ':'#13'File does not have expected size (' + inttostr(r16_file_size) + ' bytes)'), 'Error loading tileset', MB_OK or MB_ICONERROR);
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
  tileimage_filename := filename;
end;

procedure TTileset.load_r8_image(filename: String);
var
  f: file of byte;
  r8_file_buffer: array of byte;
  r8_tile_data_buffer: PByteArr;
  r8_tileset_data_buffer: PByteArr;
  i, x, y, offset: integer;
begin
  // Load file into buffer
  AssignFile(f, filename);
  FileMode := fmOpenRead;
  Reset(f);
  if (filesize(f) <> r8_file_size) then
  begin
    Application.MessageBox(PChar('Error loading tileset image from ' + filename + ':'#13'File does not have expected size (' + inttostr(r8_file_size) + ' bytes)'), 'Error loading tileset', MB_OK or MB_ICONERROR);
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
  tileimage_filename := filename;
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
    Application.MessageBox(PChar('Error loading palette. Could not find file ' + filename), 'Error loading tileset', MB_OK or MB_ICONERROR);
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

procedure TTileset.load_attributes;
var
  tileatr_file: file of cardinal;
  ini: TMemIniFile;
  decoder: TStringList;
  i, j, tile: integer;
begin
  if (tileatr_filename = '') or not FileExists(tileatr_filename) then
    exit;
  // Load TILEATR file
  AssignFile(tileatr_file, tileatr_filename);
  Reset(tileatr_file);
  BlockRead(tileatr_file, attributes, cnt_tileset_tiles*2);
  CloseFile(tileatr_file);
  // Reset editor attributes
  for i := 0 to cnt_tileset_tiles - 1 do
    attributes_editor[i] := 0;
  // Load editor attributes from tileset .ini file
  if config_filename = '' then
    exit;
  ini := TMemIniFile.Create(config_filename);
  decoder := TStringList.Create;
  decoder.Delimiter := ';';
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
  init_paint_tiles_from_editor_attributes;
end;

procedure TTileset.load_config;
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder, decoder2: TStringList;
  i, j, k, l, x, y: integer;
  key: char;
  preset_index: integer;
  preset_tile_index: integer;
  connection_point_index: integer;
  tmp_block_preset: PBlockPreset;
  tile, index, width, height, pos_x, pos_y: integer;
begin
  if (config_filename = '') or not FileExists(config_filename) then
    exit;
  ini := TMemIniFile.Create(config_filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder2 := TStringList.Create;
  decoder.Delimiter := ';';
  decoder2.Delimiter := '.';
  // Load basic information
  tileatr_name := ini.ReadString('Basic', 'tileatr', '');
  // Load minimap color rules
  tile_color_rules_used := 0;
  ini.ReadSection('Minimap_Color_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_tile_color_rules then
      break;
    tile_color_rules[i].color := strtoint(tmp_strings[i]);
    decoder.DelimitedText := ini.ReadString('Minimap_Color_Rules', tmp_strings[i], '');
    tile_color_rules[i].attr := strtoint64(decoder[0]);
    if decoder.Count = 2 then
      tile_color_rules[i].not_attr := strtoint64(decoder[1])
    else
      tile_color_rules[i].not_attr := 0;
    inc(tile_color_rules_used);
  end;
  // Load fill area rules
  fill_area_rules_used := 0;
  ini.ReadSection('Fill_Area_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_fill_area_rules then
      break;
    decoder.DelimitedText := ini.ReadString('Fill_Area_Rules', tmp_strings[i], '');
    fill_area_rules[i].attr := strtoint64(decoder[0]);
    if decoder.Count = 2 then
      fill_area_rules[i].not_attr := strtoint64(decoder[1])
    else
      fill_area_rules[i].not_attr := 0;
    inc(fill_area_rules_used);
  end;
  // Load spice settings
  thin_spice_tile := ini.ReadInteger('Spice_Settings', 'ThinSpice.tile', 793);
  thin_spice_color := ini.ReadInteger('Spice_Settings', 'ThinSpice.color', 0);
  thin_spice_name := ini.ReadString('Spice_Settings', 'ThinSpice.name', 'Thin spice');
  thick_spice_tile := ini.ReadInteger('Spice_Settings', 'ThickSpice.tile', 301);
  thick_spice_color := ini.ReadInteger('Spice_Settings', 'ThickSpice.color', 0);
  thick_spice_name := ini.ReadString('Spice_Settings', 'ThickSpice.name', 'Thick spice');
  decoder.DelimitedText := ini.ReadString('Spice_Settings', 'SpiceRestrictionRule', '0');
  spice_restriction_attr := strtoint64(decoder[0]);
  if decoder.Count = 2 then
    spice_restriction_not_attr := strtoint64(decoder[1])
  else
    spice_restriction_not_attr := 0;
  // Load paint tile groups
  for i := 0 to cnt_paint_tile_groups - 1 do
  begin
    paint_tile_groups[i].name := ini.ReadString('Paint_Tile_Groups', 'Group'+inttostr(i+1)+'.name', '');
    paint_tile_groups[i].tile_index := ini.ReadInteger('Paint_Tile_Groups', 'Group'+inttostr(i+1)+'.tile', 0);
    paint_tile_groups[i].smooth_group := ini.ReadInteger('Paint_Tile_Groups', 'Group'+inttostr(i+1)+'.smoothgroup', 0) - 1;
  end;
  // Load block preset groups
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    block_preset_groups[i].name := ini.ReadString('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.name', '');
    block_preset_groups[i].paint_group := ini.ReadInteger('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.paint', 0) - 1;
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
        if decoder2.Count = 4 then
        begin
          // Normal block preset
          width := strtoint(decoder2[0]);
          height := strtoint(decoder2[1]);
          if (preset_tile_index + width * height >= max_block_preset_tiles) then
            continue;
          pos_x := strtoint(decoder2[2]);
          pos_y := strtoint(decoder2[3]);
          block_presets[preset_index].width := width;
          block_presets[preset_index].height := height;
          block_presets[preset_index].block_preset_tile_index := preset_tile_index;
          for y := 0 to height - 1 do
            for x := 0 to width - 1 do
              begin
                tile := x + pos_x + (y + pos_y) * 20;
                block_preset_tiles[preset_tile_index + x + y * width] := tile;
                inc(block_preset_coverage[tile]);
              end;
          inc(preset_tile_index, width * height);
        end else
        if decoder2.Count = 1 then
        begin
          // Custom block
          index := strtoint(decoder2[0]);
          decoder2.DelimitedText := ini.ReadString('Custom_Blocks', inttostr(index), '');
          width := strtoint(decoder2[0]);
          height := strtoint(decoder2[1]);
          if (preset_tile_index + width * height >= max_block_preset_tiles) then
            continue;
          if (decoder2.Count - 2 <> width * height) then
            continue;
          block_presets[preset_index].width := width;
          block_presets[preset_index].height := height;
          block_presets[preset_index].block_preset_tile_index := preset_tile_index;
          for y := 0 to height - 1 do
            for x := 0 to width - 1 do
            begin
              tile := strtoint(decoder2[x + y * width + 2]);
              block_preset_tiles[preset_tile_index + x + y * width] := tile;
              inc(block_preset_coverage[tile]);
            end;
          inc(preset_tile_index, width * height);
        end;
        inc(preset_index);
      end;
    end;
  end;
  block_presets_used := preset_index;
  block_preset_tiles_used := preset_tile_index;

  // Load connection points
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
    end;

  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
  decoder2.Destroy;
end;

procedure TTileset.save_attributes();
var
  tileatr_file: file of cardinal;
  lines_input, lines_output, encoder: TStringList;
  i, j, found_editor_attr_section, found_next_section: integer;
  line: String;
begin
  // Save TILEATR*.BIN file
  if tileatr_filename = '' then
    exit;
  AssignFile(tileatr_file, tileatr_filename);
  ReWrite(tileatr_file);
  BlockWrite(tileatr_file, attributes, cnt_tileset_tiles*2);
  CloseFile(tileatr_file);
  // Save editor attributes into tileset .ini file
  if config_filename = '' then
  begin
    // Reset editor attributes
    for i := 0 to cnt_tileset_tiles - 1 do
      attributes_editor[i] := 0;
    exit;
  end;
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
  // Re-init paint tiles
  init_paint_tiles_from_editor_attributes;
end;

procedure TTileset.convert_editor_attributes;
var
  i: integer;
begin
  for i := 0 to cnt_tileset_tiles-1 do
  begin
    attributes_editor[i] := attributes_editor[i] or (attributes[i] and $FF);
    attributes[i] := attributes[i] and $FFFFFF00;
  end;
  init_paint_tiles_from_editor_attributes;
end;

procedure TTileset.init_paint_tiles_from_editor_attributes;
var
  i, j: integer;
begin
  // Get all paint tiles (sand/rock/dunes) from editor attributes
  paint_tiles_used := 0;
  for i := 0 to Length(paint_tiles_cnt) - 1 do
    paint_tiles_cnt[i] := 0; // Initialize to zero
  for i := 0 to cnt_paint_tile_groups - 1 do
  begin
    for j := 0 to cnt_tileset_tiles - 1 do
    begin
      if ((attributes_editor[j] and (1 shl i)) <> 0) and (paint_tiles_used < max_paint_tiles) then
      begin
        paint_tiles[paint_tiles_used] := j;
        inc(paint_tiles_cnt[i]);
        inc(paint_tiles_used);
      end;
    end;
  end;
end;

function TTileset.get_tile_attributes(tile: word): Int64;
begin
  result := attributes_editor[tile];
  result := result shl 32;
  result := result or attributes[tile];
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
  atr := attributes[tile];
  if (atr and taAnyPass) = 0 then
    result := ttImpassable
  else if (atr and taAnyPass) = taInfantryPass then
    result := ttInfantryOnly
  else if (atr and taBuildable) = taBuildable then
    result := ttBuildable
  else
    result := ttPassable
end;

function TTileset.get_tile_color(tile: word): TColor;
var
  i: integer;
  attr_value: int64;
begin
  attr_value := get_tile_attributes(tile);
  for i := 0 to tile_color_rules_used - 1 do
  begin
    if ((attr_value and tile_color_rules[i].attr) = tile_color_rules[i].attr) and ((attr_value and tile_color_rules[i].not_attr) = 0) then
    begin
      result := tile_color_rules[i].color;
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
  if (special = 1) or (special = 2) then
  begin
    // Spice area
    result := 0;
    exit;
  end;
  attr_value := get_tile_attributes(tile);
  for i := 0 to fill_area_rules_used - 1 do
  begin
    if ((attr_value and fill_area_rules[i].attr) = fill_area_rules[i].attr) and ((attr_value and fill_area_rules[i].not_attr) = 0) then
    begin
      result := i + 2;
      exit;
    end;
  end;
  result := 1;
end;

function TTileset.check_spice_can_be_placed(tile: word): boolean;
var
  attr_value: int64;
begin
  attr_value := get_tile_attributes(tile);
  result := ((attr_value and spice_restriction_attr) = spice_restriction_attr) and ((attr_value and spice_restriction_not_attr) = 0);
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

function TTileset.get_random_paint_tile(group: integer): integer;
var
  i: integer;
  start_index: integer;
begin
  if (group >= cnt_paint_tile_groups) or (paint_tiles_cnt[group] = 0) then
  begin
    result := 0;
    exit;
  end;
  start_index := 0;
  for i := 0 to group - 1 do
    inc(start_index, paint_tiles_cnt[i]);
  result := paint_tiles[random(paint_tiles_cnt[group]) + start_index];
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
      if last_block_preset_variant >= num_variants then
        last_block_preset_variant := 0;
      variant := last_block_preset_variant;
      inc(last_block_preset_variant);
    end
    else if variant = bpRandom then
    begin
      // Get random variant but different from the last one
      repeat
        variant := random(num_variants);
      until variant <> last_block_preset_variant;
      last_block_preset_variant := variant;
    end;
  end;
  preset_index := block_preset_key_variants[group, key_index].first_preset_index + variant;
  result := preset_index;
end;

function TTileset.get_status: String;
begin
  result :=
    'Tileset index: '+inttostr(Tileset.current_tileset)+#13+
    'Tileset name: '+Tileset.tileset_name+#13+
    'Tileset attributes name: '+Tileset.tileatr_name+#13+
    'Tileset image file: '+Tileset.tileimage_filename+#13+
    'Tileset attributes file: '+Tileset.tileatr_filename+#13+
    'Tileset configuration file: '+Tileset.config_filename+#13+
    'Minimap color rules used: '+inttostr(Tileset.tile_color_rules_used)+#13+
    'Fill area rules used: '+inttostr(Tileset.fill_area_rules_used)+#13+
    'Paint tiles used: '+inttostr(Tileset.paint_tiles_used)+#13+
    'Block presets used: '+inttostr(Tileset.block_presets_used)+#13+
    'Preset tiles used: '+inttostr(Tileset.block_preset_tiles_used)+#13+
    'Connection points used: '+inttostr(Tileset.connection_points_used);
end;

end.
