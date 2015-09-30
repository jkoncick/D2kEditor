unit _tileset;

interface

uses Graphics, Menus;

// Tileset constants
const cnt_tileset_tiles = 800;
const cnt_paint_tile_groups = 3;
const cnt_block_preset_groups = 4;
const cnt_block_preset_keys = 38; // 0-9, A-Z, ',', '.'
const max_tile_color_rules = 10;
const max_fill_area_rules = 10;
const max_paint_tiles = 80;
const max_block_presets = 64;

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
// Tileset attributes (editor)
const taEdCleanSand = $01;
const taEdCleanRock = $02;
const taEdCleanDunes = $04;
const taEdDunesArea = $08;
const taEdRockArea = $10;
const taEdSandDecorations = $20;
const taEdIceArea = $40;

const paint_tiles_group_attrs: array[0..cnt_paint_tile_groups-1] of cardinal =
  (taEdCleanSand, taEdCleanRock, taEdCleanDunes);

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
  end;

type
  TTilesetInfo = record
    name: String;
    image_name: String;
    tileatr_name: String;
    config_name: String;
  end;

const empty_block_preset: TBlockPreset = (width: 0; height: 0; pos_x: 0; pos_y: 0);

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

    paint_tiles: array[0..cnt_paint_tile_groups-1, 0..max_paint_tiles-1] of integer; // Tile numbers of clean sand/rock/dunes
    paint_tiles_cnt: array[0..cnt_paint_tile_groups-1] of integer;

    block_preset_groups: array[0..cnt_block_preset_groups-1] of TBlockPresetGroup;
    block_preset_key_variants: array[0..cnt_block_preset_groups-1, 0..cnt_block_preset_keys-1] of TBlockPresetVariantList;
    block_presets: array[0..cnt_block_preset_groups-1, 0..max_block_presets-1] of TBlockPreset;

  public
    procedure init;
    procedure change_tileset(index: integer);
    procedure change_tileset_by_name(name: String);
    procedure next_tileset;
    procedure load_custom_image(filename: String);
    procedure load_tileatr(filename: String);
    procedure load_tileset_config(filename: String);

    function get_tile_type(tile: word): TileType;
    function get_tile_color(tile: word): TColor;
    function get_fill_area_type(tile: word; special: word): integer;

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
    tileset_info[i].config_name := ini.ReadString(tmp_strings[i], 'config', 'd2k_tilesets');
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
  current_tileset := index;
  menuitems[current_tileset].Checked := true;
  tmp_filename := current_dir+'/tilesets/'+tileset_info[current_tileset].image_name+'.bmp';
  // Set tileset in .mis file
  Move(tileset_info[current_tileset].name[1], Mission.mis_data.tileset, 8);
  Move(tileset_info[current_tileset].tileatr_name[1], Mission.mis_data.tileatr, 8);
  // Load tileset graphics
  if FileExists(tmp_filename) then
  begin
    tileimage_filename := tmp_filename;
    tileimage.LoadFromFile(tileimage_filename);
    // Copy spice tiles ftom tileset
    MainWindow.graphics_misc_objects.Canvas.CopyRect(Rect(0, 0, 32, 32), tileimage.Canvas, Rect(256, 1184, 288, 1216));
    MainWindow.graphics_misc_objects.Canvas.CopyRect(Rect(32, 0, 64, 32), tileimage.Canvas, Rect(32, 480, 64, 512));
  end;
  // Load tileset configuration if it is different
  tmp_filename := current_dir+'/tilesets/'+tileset_info[current_tileset].config_name+'.ini';
  if tmp_filename <> tileset_config_filename then
    load_tileset_config(tmp_filename);
  // Load tileset attributes
  load_tileatr(current_dir+'/tilesets/'+tileset_info[current_tileset].tileatr_name+'.bin');
  MainWindow.StatusBar.Panels[1].Text := tileset_info[current_tileset].name;
  // Redraw dialogs
  if (TilesetDialog <> nil) and TilesetDialog.Visible then
    TilesetDialog.DrawTileset(nil);
  if (BlockPresetDialog <> nil) then
    BlockPresetDialog.init_presets;
  // Re-render everything
  MainWindow.draw_cursor_image;
  MainWindow.render_minimap;
  MainWindow.render_map;
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
begin
  current_tileset := current_tileset + 1;
  if current_tileset >= cnt_tilesets then
    current_tileset := 0;
  change_tileset(current_tileset);
end;

procedure TTileset.load_custom_image(filename: String);
begin
  tileimage.LoadFromFile(filename);
  tileimage_filename := filename;
  menuitems[current_tileset].Checked := False;
  current_tileset := Settings.DefaultTileset;
  load_tileatr(current_dir+'/tilesets/'+tileset_info[current_tileset].tileatr_name+'.bin');
  MainWindow.StatusBar.Panels[1].Text := 'Tileset File';
  MainWindow.render_map;
end;

procedure TTileset.load_tileatr(filename: String);
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
      if ((attributes[i] and paint_tiles_group_attrs[j]) <> 0) and (paint_tiles_cnt[j] < max_paint_tiles) then
      begin
        paint_tiles[j, paint_tiles_cnt[j]] := i;
        inc(paint_tiles_cnt[j]);
      end;
    end;
end;

procedure TTileset.load_tileset_config(filename: String);
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder, decoder2: TStringList;
  i, j, k: integer;
  key: char;
  preset_index: integer;
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
  // Load paint tile groups
  for i := 0 to cnt_paint_tile_groups - 1 do
  begin
    MainWindow.paint_tile_select[i].Caption := ini.ReadString('Paint_Tile_Groups', 'Group'+inttostr(i+1)+'_name', '');
  end;
  // Load block preset groups
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    block_preset_groups[i].name := ini.ReadString('Block_Preset_Groups', 'Group'+inttostr(i+1)+'_name', '');
    block_preset_groups[i].paint_group := ini.ReadInteger('Block_Preset_Groups', 'Group'+inttostr(i+1)+'_paint', 0) - 1;
    MainWindow.BlockPresetGroupSelect.Items[i] := block_preset_groups[i].name;
  end;
  // Load block preset
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
        key := '>';
      decoder.DelimitedText := ini.ReadString('Block_Preset_Group_'+(inttostr(i+1)), key, '');
      block_preset_key_variants[i, j].num_variants := decoder.Count;
      block_preset_key_variants[i, j].first_preset_index := preset_index;
      for k := 0 to decoder.Count - 1 do
      begin
        decoder2.DelimitedText := decoder[k];
        if (preset_index >= max_block_presets) or (decoder2.Count <> 4) then
          break;
        block_presets[i, preset_index].width := strtoint(decoder2[0]);
        block_presets[i, preset_index].height := strtoint(decoder2[1]);
        block_presets[i, preset_index].pos_x := strtoint(decoder2[2]);
        block_presets[i, preset_index].pos_y := strtoint(decoder2[3]);
        inc(preset_index);
      end;
    end;
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

function TTileset.block_key_to_index(key: word): integer;
begin
  if (key >= ord('0')) and (key <= ord('9')) then
    result := key - ord('0')
  else if (key >= ord('A')) and (key <= ord('Z')) then
    result := key - ord('A') + 10
  else if key = 188 then
    result := 36
  else if key = 190 then
    result := 37
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
