unit _tileset;

interface

uses Graphics, Menus;

// Tileset constants
const cnt_tileset_tiles = 800;
const max_paint_tiles = 80;
const max_tile_color_rules = 10;

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

const paint_tiles_group_attrs: array[0..2] of cardinal = (taEdCleanSand, taEdCleanRock, taEdCleanDunes);

// Tileset type definitions
type
  TileType = (ttPassable, ttImpassable, ttInfantryOnly, ttBuildable);
  FillAreaType = (faSpice, faSand, faDunes, faSandDecorations, faIce, faOther);

type
  TTileColorRule = record
    color: TColor;
    and_attr: cardinal;
    check_attr: cardinal;
  end;

type
  TTilesetInfo = record
    name: String;
    image_name: String;
    tileatr_name: String;
    bassicconf_name: String;
  end;

// Tileset class
type
  TTileset = class

  private
    menuitems: array of TMenuItem;

  public
    tileimage_filename: String;
    attributes_filename: String;
    basicconf_filename: String;
    tileimage: TBitmap;
    attributes: array[0..cnt_tileset_tiles-1] of cardinal;
    current_tileset: integer;
    cnt_tilesets: integer;
    tileset_info: array of TTilesetInfo;

  private
    // Tileset configuration
    paint_tiles: array[0..2, 0..max_paint_tiles-1] of integer; // Tile numbers of clean sand/rock/dunes
    paint_tiles_cnt: array[0..2] of integer;

    tile_color_rules: array[0..max_tile_color_rules-1] of TTileColorRule;
    cnt_tile_color_rules: integer;

  public
    procedure init;
    procedure change_tileset(index: integer);
    procedure change_tileset_by_name(name: String);
    procedure next_tileset;
    procedure load_custom_image(filename: String);
    procedure load_tileatr(filename: String);
    procedure load_basicconf(filename: String);

    function get_tile_type(tile: word): TileType;
    function get_tile_color(tile: word): TColor;
    function get_fill_area_type(tile: word; special: word): FillAreaType;

    function get_random_paint_tile(group: integer): integer;

  end;

var
  Tileset: TTileset;

implementation

uses Windows, Forms, SysUtils, main, tileset_dialog, _mission, _settings, IniFiles, Classes;

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
    tileset_info[i].bassicconf_name := ini.ReadString(tmp_strings[i], 'basicconf', 'd2k_tilesets_basic');
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
  end;
  // Load tileset attributes
  load_tileatr(current_dir+'/tilesets/'+tileset_info[current_tileset].tileatr_name+'.bin');
  MainWindow.StatusBar.Panels[1].Text := tileset_info[current_tileset].name;
  if (TilesetDialog <> nil) and TilesetDialog.Visible then
    TilesetDialog.DrawTileset(nil);
  // Load tileset basic configuration if it is different
  tmp_filename := current_dir+'/tilesets/'+tileset_info[current_tileset].bassicconf_name+'.ini';
  if tmp_filename <> basicconf_filename then
    load_basicconf(tmp_filename);
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
    for j := 0 to Length(paint_tiles_group_attrs) - 1 do
    begin
      if ((attributes[i] and paint_tiles_group_attrs[j]) <> 0) and (paint_tiles_cnt[j] < max_paint_tiles) then
      begin
        paint_tiles[j, paint_tiles_cnt[j]] := i;
        inc(paint_tiles_cnt[j]);
      end;
    end;
end;

procedure TTileset.load_basicconf(filename: String);
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder: TStringList;
  i: integer;
begin
  if not FileExists(filename) then
    exit;
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  // Load minimap color rules
  cnt_tile_color_rules := 0;
  decoder.Delimiter := ';';
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
    inc(cnt_tile_color_rules);
  end;

  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
  basicconf_filename := filename;
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
  atr: cardinal;
  i: integer;
begin
  atr := attributes[tile];
  for i := 0 to Length(tile_color_rules) - 1 do
  begin
    if (atr and tile_color_rules[i].and_attr) = tile_color_rules[i].check_attr then
    begin
      result := tile_color_rules[i].color;
      exit;
    end;
  end;
  result := $0;
end;

function TTileset.get_fill_area_type(tile: word; special: word): FillAreaType;
var
  atr: cardinal;
begin
  atr := attributes[tile];
  if (special = 1) or (special = 2) then
    result := faSpice
  else if (atr and taEdCleanSand) = taEdCleanSand then
    result := faSand
  else if (atr and taEdDunesArea) = taEdDunesArea then
    result := faDunes
  else if (atr and taEdSandDecorations) = taEdSandDecorations then
    result := faSandDecorations
  else if (atr and taEdIceArea) = taEdIceArea then
    result := faIce
  else
    result := faOther;
end;

function TTileset.get_random_paint_tile(group: integer): integer;
begin
  if (group >= Length(paint_tiles_cnt)) or (paint_tiles_cnt[group] = 0) then
  begin
    result := 0;
    exit;
  end;
  result := paint_tiles[group, random(paint_tiles_cnt[group])];
end;

end.
