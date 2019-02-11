unit _tileset;

interface

uses Windows, Graphics, Menus;

// Tileset constants
const cnt_tileset_tiles = 800;
const cnt_paint_tile_groups = 4;
const cnt_block_preset_groups = 8;
const cnt_block_preset_keys = 40; // 0-9, A-Z...
const max_tile_color_rules = 10;
const max_fill_area_rules = 10;
const max_paint_tiles = 80;
const max_block_presets = 64;
const max_custom_block_size = 8;
const max_custom_blocks = 40;

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
    and_attr: cardinal;
    check_attr: cardinal;
  end;

type
  TFillAreaRule = record
    and_attr: cardinal;
    check_attr: cardinal;
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
    num_variants: word;
  end;

type
  TBlockPreset = record
    width: word;
    height: word;
    pos_x: word;
    pos_y: word;
    custom_block_index: integer;
  end;

type
  TCustomBlock = record
    width: word;
    height: word;
    tiles: array[0..max_custom_block_size-1, 0..max_custom_block_size-1] of word;
  end;

const empty_block_preset: TBlockPreset = (width: 0; height: 0; pos_x: 0; pos_y: 0; custom_block_index: -1);

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

    // Auxiliary variables for loading tileset image
    palette_loaded: Boolean;
    palette: PLogPalette;

    // Tileset data and configuration
    tileimage: TBitmap;
    attributes: array[0..cnt_tileset_tiles-1] of cardinal;

    tile_color_rules: array[0..max_tile_color_rules-1] of TTileColorRule;
    tile_color_rules_cnt: integer;

    fill_area_rules: array[0..max_tile_color_rules-1] of TFillAreaRule;
    fill_area_rules_cnt: integer;

    thin_spice_tile: integer;
    thin_spice_color: TColor;
    thick_spice_tile: integer;
    thick_spice_color: TColor;
    spice_restriction_and_attr: cardinal;
    spice_restriction_check_attr: cardinal;

    paint_tile_groups: array[0..cnt_paint_tile_groups-1] of TPaintTileGroup;
    paint_tiles: array[0..cnt_paint_tile_groups-1, 0..max_paint_tiles-1] of integer; // Tile numbers of clean sand/rock/dunes
    paint_tiles_cnt: array[0..cnt_paint_tile_groups-1] of integer;

    block_preset_groups: array[0..cnt_block_preset_groups-1] of TBlockPresetGroup;
    block_preset_key_variants: array[0..cnt_block_preset_groups-1, 0..cnt_block_preset_keys-1] of TBlockPresetVariantList;
    block_presets: array[0..cnt_block_preset_groups-1, 0..max_block_presets-1] of TBlockPreset;

    custom_blocks: array[0..max_custom_blocks-1] of TCustomBlock;

  public
    procedure init;
    procedure change_tileset(index: integer);
    procedure change_tileset_by_name(name: String);
    procedure next_tileset;
    procedure use_custom_image(filename: String);
    procedure load_tileimage();
    procedure load_r16_image(filename: String);
    procedure load_r8_image(filename: String);
    procedure load_palette;
    procedure load_attributes(filename: String);
    procedure load_config(filename: String);

    function get_tile_type(tile: word): TileType;
    function get_tile_color(tile: word): TColor;
    function get_fill_area_type(tile: word; special: word): integer;
    function check_spice_can_be_placed(tile: word): boolean;

    function block_key_to_index(key: word): integer;

    function get_random_paint_tile(group: integer): integer;
    function get_block_preset(group: integer; key: word; variant: integer): TBlockPreset;

  end;

var
  Tileset: TTileset;

implementation

uses Forms, SysUtils, main, tileset_dialog, block_preset_dialog, _settings, IniFiles, Classes;

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
  MainWindow.tileset_menuitems[current_tileset].Checked := true;
  MainWindow.StatusBar.Panels[1].Text := tileset_name;
  // Load tileset configuration
  load_config(current_dir+'/tilesets/'+tileset_name+'.ini');
  // Load tileset attributes
  fn_tileatr_editor := current_dir+'/tilesets/'+tileatr_name+'.BIN';
  fn_tileatr_game := Settings.GamePath+'/Data/bin/'+tileatr_name+'.BIN';
  if FileExists(fn_tileatr_editor) then
    load_attributes(fn_tileatr_editor)
  else if FileExists(fn_tileatr_game) then
    load_attributes(fn_tileatr_game)
  else
    Application.MessageBox(PChar('Could not find tileset attributes file ' + tileatr_name + '.BIN'), 'Error loading tileset attributes', MB_OK or MB_ICONWARNING);
  // Load tileset image
  load_tileimage;
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
    change_tileset(Settings.DefaultTileset);
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

procedure TTileset.use_custom_image(filename: String);
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
  if (current_tileset < cnt_tilesets) and (current_tileset >= 0) then
    MainWindow.tileset_menuitems[current_tileset].Checked := False;
  MainWindow.StatusBar.Panels[1].Text := 'Custom image';
  MainWindow.render_tileset;
end;

procedure TTileset.load_tileimage();
var
  fn_r16_game, fn_r8_game, fn_r16_editor, fn_r8_editor, fn_bmp_editor: String;
begin
  fn_r16_game := Settings.GamePath+'/Data/'+tileset_name+'.r16';
  fn_r8_game := Settings.GamePath+'/Data/'+tileset_name+'.r8';
  fn_r16_editor := current_dir+'/tilesets/'+tileset_name+'.r16';
  fn_r8_editor := current_dir+'/tilesets/'+tileset_name+'.r8';
  fn_bmp_editor := current_dir+'/tilesets/'+tileset_name+'.bmp';
  // Try to load tileset image in given order
  if FileExists(fn_r16_game) then
    load_r16_image(fn_r16_game)
  else if FileExists(fn_r8_game) then
    load_r8_image(fn_r8_game)
  else if FileExists(fn_r16_editor) then
    load_r16_image(fn_r16_editor)
  else if FileExists(fn_r8_editor) then
    load_r8_image(fn_r8_editor)
  else if FileExists(fn_bmp_editor) then
  begin
    tileimage.LoadFromFile(fn_bmp_editor);
    tileimage_filename := fn_bmp_editor;
  end else
    Application.MessageBox(PChar('Could not find image file for tileset ' + tileset_name), 'Error loading tileset', MB_OK or MB_ICONWARNING);
  // Redraw terrain editing GUI and dialogs
  MainWindow.render_tileset;
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
  filename := Settings.GamePath+'/Data/bin/PALETTE.BIN';
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

procedure TTileset.load_attributes(filename: String);
var
  tileatr_file: file of cardinal;
  i, j: integer;
begin
  if not FileExists(filename) then
    exit;
  // Load TILEATR file
  AssignFile(tileatr_file, filename);
  Reset(tileatr_file);
  BlockRead(tileatr_file, attributes, cnt_tileset_tiles);
  CloseFile(tileatr_file);
  // Get all paint tiles (sand/rock/dunes) from editor attributes
  for i := 0 to Length(paint_tiles_cnt) - 1 do
    paint_tiles_cnt[i] := 0; // Initialize to zero
  for i := 0 to cnt_tileset_tiles - 1 do
    for j := 0 to cnt_paint_tile_groups - 1 do
    begin
      if ((attributes[i] and (1 shl j)) <> 0) and (paint_tiles_cnt[j] < max_paint_tiles) then
      begin
        paint_tiles[j, paint_tiles_cnt[j]] := i;
        inc(paint_tiles_cnt[j]);
      end;
    end;
  tileatr_filename := filename;
end;

procedure TTileset.load_config(filename: String);
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder, decoder2: TStringList;
  i, j, k: integer;
  key: char;
  preset_index: integer;
  index, width, height: integer;
begin
  if not FileExists(filename) then
    exit;
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder2 := TStringList.Create;
  decoder.Delimiter := ';';
  decoder2.Delimiter := '.';
  // Load basic information
  tileatr_name := ini.ReadString('Basic', 'tileatr', '');
  // Load minimap color rules
  tile_color_rules_cnt := 0;
  ini.ReadSection('Minimap_Color_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_tile_color_rules then
      break;
    tile_color_rules[i].color := strtoint(tmp_strings[i]);
    decoder.DelimitedText := ini.ReadString('Minimap_Color_Rules', tmp_strings[i], '');
    tile_color_rules[i].and_attr := strtoint(decoder[0]);
    if decoder.Count = 2 then
      tile_color_rules[i].check_attr := strtoint(decoder[1])
    else
      tile_color_rules[i].check_attr := strtoint(decoder[0]);
    inc(tile_color_rules_cnt);
  end;
  // Load fill area rules
  fill_area_rules_cnt := 0;
  ini.ReadSection('Fill_Area_Rules', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i >= max_fill_area_rules then
      break;
    decoder.DelimitedText := ini.ReadString('Fill_Area_Rules', tmp_strings[i], '');
    fill_area_rules[i].and_attr := strtoint(decoder[0]);
    if decoder.Count = 2 then
      fill_area_rules[i].check_attr := strtoint(decoder[1])
    else
      fill_area_rules[i].check_attr := strtoint(decoder[0]);
    inc(fill_area_rules_cnt);
  end;
  // Load spice settings
  thin_spice_tile := ini.ReadInteger('Spice_Settings', 'ThinSpice.tile', 0);
  thin_spice_color := ini.ReadInteger('Spice_Settings', 'ThinSpice.color', 0);
  thick_spice_tile := ini.ReadInteger('Spice_Settings', 'ThickSpice.tile', 0);
  thick_spice_color := ini.ReadInteger('Spice_Settings', 'ThickSpice.color', 0);
  decoder.DelimitedText := ini.ReadString('Spice_Settings', 'SpiceRestrictionRule', '0');
  spice_restriction_and_attr := strtoint(decoder[0]);
  if decoder.Count = 2 then
    spice_restriction_check_attr := strtoint(decoder[1])
  else
    spice_restriction_check_attr := strtoint(decoder[0]);
  // Load paint tile groups
  for i := 0 to cnt_paint_tile_groups - 1 do
  begin
    paint_tile_groups[i].name := ini.ReadString('Paint_Tile_Groups', 'Group'+inttostr(i+1)+'.name', '');
    MainWindow.paint_tile_select[i].Enabled := paint_tile_groups[i].name <> '';
    MainWindow.paint_tile_select[i].Hint := paint_tile_groups[i].name;
    paint_tile_groups[i].tile_index := ini.ReadInteger('Paint_Tile_Groups', 'Group'+inttostr(i+1)+'.tile', 0);
    paint_tile_groups[i].smooth_group := ini.ReadInteger('Paint_Tile_Groups', 'Group'+inttostr(i+1)+'.smoothgroup', 0) - 1;
  end;
  // Load block preset groups
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    block_preset_groups[i].name := ini.ReadString('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.name', '');
    block_preset_groups[i].paint_group := ini.ReadInteger('Block_Preset_Groups', 'Group'+inttostr(i+1)+'.paint', 0) - 1;
    MainWindow.block_preset_select[i].Enabled := block_preset_groups[i].name <> '';
    MainWindow.block_preset_select[i].Caption := block_preset_groups[i].name;
  end;
  // Load block presets
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    preset_index := 0;
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
          block_presets[i, preset_index].width := strtoint(decoder2[0]);
          block_presets[i, preset_index].height := strtoint(decoder2[1]);
          block_presets[i, preset_index].pos_x := strtoint(decoder2[2]);
          block_presets[i, preset_index].pos_y := strtoint(decoder2[3]);
          block_presets[i, preset_index].custom_block_index := -1;
        end else
        if decoder2.Count = 1 then
        begin
          // Custom block
          block_presets[i, preset_index].width := 0;
          block_presets[i, preset_index].height := 0;
          block_presets[i, preset_index].pos_x := 0;
          block_presets[i, preset_index].pos_y := 0;
          index := strtoint(decoder2[0]) - 1;
          if (index < max_custom_blocks) and (index >= 0)then
            block_presets[i, preset_index].custom_block_index := index
          else
            block_presets[i, preset_index].custom_block_index := -1;
        end;
        inc(preset_index);
      end;
    end;
  end;
  // Load custom blocks
  ini.ReadSection('Custom_Blocks', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    index := strtoint(tmp_strings[i]) - 1;
    if (index >= max_custom_blocks) or (index < 0) then
      continue;
    decoder2.DelimitedText := ini.ReadString('Custom_Blocks', tmp_strings[i], '');
    if decoder2.Count < 2 then
      continue;
    width := strtoint(decoder2[0]);
    height := strtoint(decoder2[1]);
    if (width > max_custom_block_size) or (height > max_custom_block_size) then
      continue;
    custom_blocks[index].width := width;
    custom_blocks[index].height := height;
    for j := 2 to decoder2.Count - 1 do
      custom_blocks[index].tiles[(j - 2) mod width, (j - 2) div width] := strtoint(decoder2[j]);
  end;

  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
  decoder2.Destroy;
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
begin
  for i := 0 to tile_color_rules_cnt - 1 do
  begin
    if (attributes[tile] and tile_color_rules[i].and_attr) = tile_color_rules[i].check_attr then
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
begin
  if (special = 1) or (special = 2) then
  begin
    // Spice area
    result := 0;
    exit;
  end;
  for i := 0 to fill_area_rules_cnt - 1 do
  begin
    if (attributes[tile] and fill_area_rules[i].and_attr) = fill_area_rules[i].check_attr then
    begin
      result := i + 2;
      exit;
    end;
  end;
  result := 1;
end;

function TTileset.check_spice_can_be_placed(tile: word): boolean;
begin
  result := (attributes[tile] and spice_restriction_and_attr) = spice_restriction_check_attr;
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
begin
  if (group >= cnt_paint_tile_groups) or (paint_tiles_cnt[group] = 0) then
  begin
    result := 0;
    exit;
  end;
  result := paint_tiles[group, random(paint_tiles_cnt[group])];
end;

function TTileset.get_block_preset(group: integer; key: word; variant: integer): TBlockPreset;
var
  num_variants: integer;
  key_index, preset_index: integer;
begin
  key_index := block_key_to_index(key);
  // Unknown key
  if key_index = -1 then
  begin
    result := empty_block_preset;
    exit;
  end;
  num_variants := block_preset_key_variants[group, key_index].num_variants;
  // No block preset for this key
  if num_variants = 0 then
  begin
    result := empty_block_preset;
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
  result := block_presets[group, preset_index];
end;

end.
