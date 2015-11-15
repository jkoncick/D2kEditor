unit _tileset;

interface

uses Graphics, Menus;

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

type
  TTilesetInfo = record
    name: String;
    image_name: String;
    tileatr_name: String;
  end;

const empty_block_preset: TBlockPreset = (width: 0; height: 0; pos_x: 0; pos_y: 0; custom_block_index: -1);

// Tileset class
type
  TTileset = class

  private
    menuitems: array of TMenuItem;
    last_block_preset_variant: integer;

  public
    tileimage_filename: String;
    attributes_filename: String;
    tileset_config_filename: String;
    tileimage: TBitmap;
    attributes: array[0..cnt_tileset_tiles-1] of cardinal;
    current_tileset: integer;
    cnt_tilesets: integer;
    tileset_info: array of TTilesetInfo;

    // Tileset configuration
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
    procedure load_image(filename: String);
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

uses Windows, Forms, SysUtils, main, tileset_dialog, block_preset_dialog, _mission, _settings, IniFiles, Classes, Dialogs;

procedure TTileset.init;
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i: integer;
begin
  tileimage := Graphics.TBitmap.Create;
  ini := TMemIniFile.Create(current_dir + 'config/tilesets.ini');
  tmp_strings := TStringList.Create;
  ini.ReadSections(tmp_strings);
  cnt_tilesets := tmp_strings.Count;
  SetLength(tileset_info, cnt_tilesets);
  SetLength(menuitems, cnt_tilesets);
  for i := 0 to cnt_tilesets -1 do
  begin
    tileset_info[i].name := tmp_strings[i];
    tileset_info[i].image_name := ini.ReadString(tmp_strings[i], 'image', '');
    tileset_info[i].tileatr_name := ini.ReadString(tmp_strings[i], 'tileatr', '');
    menuitems[i] := TMenuItem.Create(MainWindow.Selecttileset1);
    menuitems[i].Caption := tileset_info[i].name;
    menuitems[i].RadioItem := true;
    menuitems[i].GroupIndex := 1;
    menuitems[i].Tag := i;
    menuitems[i].OnClick := MainWindow.SelectTileset;
    MainWindow.Selecttileset1.Add(menuitems[i]);
  end;
  ini.Destroy;
  tmp_strings.Destroy;
  change_tileset(Settings.DefaultTileset);
end;

procedure TTileset.change_tileset(index: integer);
var
  tmp_filename: String;
begin
  if index = current_tileset then
    exit;
  current_tileset := index;
  menuitems[current_tileset].Checked := true;
  MainWindow.StatusBar.Panels[1].Text := tileset_info[current_tileset].name;
  // Set tileset in .mis file
  Move(tileset_info[current_tileset].name[1], Mission.mis_data.tileset, 8);
  Move(tileset_info[current_tileset].tileatr_name[1], Mission.mis_data.tileatr, 8);
  // Load tileset configuration if it is different
  tmp_filename := current_dir+'/tilesets/'+tileset_info[current_tileset].name+'.ini';
  load_config(tmp_filename);
  // Load tileset attributes
  load_attributes(current_dir+'/tilesets/'+tileset_info[current_tileset].tileatr_name+'.bin');
  // Load tileset image
  tmp_filename := current_dir+'/tilesets/'+tileset_info[current_tileset].image_name+'.bmp';
  if FileExists(tmp_filename) then
    load_image(tmp_filename);
end;

procedure TTileset.change_tileset_by_name(name: String);
var
  i: integer;
begin
  for i:= 0 to cnt_tilesets-1 do
  begin
    if name = tileset_info[i].name then
      Tileset.change_tileset(i);
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

procedure TTileset.use_custom_image(filename: String);
begin
  load_image(filename);
  menuitems[current_tileset].Checked := False;
  current_tileset := Settings.DefaultTileset;
  MainWindow.StatusBar.Panels[1].Text := 'Custom image';
end;

procedure TTileset.load_image(filename: String);
var
  i:integer;
begin
  tileimage_filename := filename;
  tileimage.LoadFromFile(tileimage_filename);
  // Draw glyphs in terrain editing GUI
  MainWindow.draw_paint_tile_select_glyph(-1, thin_spice_tile, tileimage.Canvas);
  MainWindow.draw_paint_tile_select_glyph(-2, thick_spice_tile, tileimage.Canvas);
  for i := 0 to cnt_paint_tile_groups-1 do
    MainWindow.draw_paint_tile_select_glyph(i, paint_tile_groups[i].tile_index, tileimage.Canvas);
  // Redraw dialogs
  MainWindow.render_tileset;
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
  attributes_filename := filename;
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
  tileset_config_filename := filename;
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
