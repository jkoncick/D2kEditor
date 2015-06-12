unit tileset;

interface

uses Graphics, Menus;

// Tileset constants
const cnt_tilesets = 7;
const cnt_tileset_tiles = 800;
const default_tileset = 2;
const mmap_tile_type_colors: array[0..6] of TColor = ($8CDFEF,$29285A,$375582,$ACDFEF,$58A4E4,$509CDC,$F0827F);

// Tileset type definitions
type
  TileType = (ttNormal, ttImpassable, ttInfantryOnly, ttSlowdown, ttRock, ttBuildable, ttIce);
  FillAreaType = (faSpice, faSand, faDunes, faSandDecorations, faIce, faOther);

type
  TTilesetInfo = record
    name: String;
    image_name: String;
    tileatr_name: String;
  end;

// Tileset type definition constants
const tilesets: array[0..cnt_tilesets-1] of TTilesetInfo =
  (
    (name: 'BLOXBASE'; image_name: 'd2k_BLOXBASE'; tileatr_name: 'TILEATR2'),
    (name: 'BLOXBAT';  image_name: 'd2k_BLOXBAT';  tileatr_name: 'TILEATR6'),
    (name: 'BLOXBGBS'; image_name: 'd2k_BLOXBGBS'; tileatr_name: 'TILEATR3'),
    (name: 'BLOXICE';  image_name: 'd2k_BLOXICE' ; tileatr_name: 'TILEATR5'),
    (name: 'BLOXTREE'; image_name: 'd2k_BLOXTREE'; tileatr_name: 'TILEATR1'),
    (name: 'BLOXWAST'; image_name: 'd2k_BLOXWAST'; tileatr_name: 'TILEATR4'),
    (name: 'BLOXXMAS'; image_name: 'd2k_BLOXXMAS'; tileatr_name: 'TILEATR7')
  );

// Tileset variables
var
  tileset_image_filename: String;
  tileset_attributes_filename: String;
  tileset_image: TPicture;
  tileset_attributes: array[0..cnt_tileset_tiles-1] of cardinal;
  tileset_index: integer;
  tileset_menuitems: array[0..cnt_tilesets-1] of TMenuItem;


// Tileset functions and procedures
  procedure tileset_init;
  procedure tileset_change(index: integer);
  procedure tileset_next;
  procedure tileset_load_custom_image(filename: String);
  procedure load_tileatr(filename: String);
  function get_tile_type(tile: word): TileType;
  function get_fill_area_type(tile: word; special: word): FillAreaType;



implementation

uses Windows, Forms, SysUtils, main, tileset_dialog, mis_file;

procedure tileset_init;
var
  i: integer;
begin
  tileset_image := TPicture.Create;
  for i := 0 to cnt_tilesets -1 do
  begin
    tileset_menuitems[i] := TMenuItem.Create(MainWindow.Selecttileset1);
    tileset_menuitems[i].Caption := tilesets[i].name;
    tileset_menuitems[i].RadioItem := true;
    tileset_menuitems[i].GroupIndex := 1;
    tileset_menuitems[i].Tag := i;
    tileset_menuitems[i].OnClick := MainWindow.SelectTileset;
    MainWindow.Selecttileset1.Add(tileset_menuitems[i]);
  end;
end;

procedure tileset_change(index: integer);
var
  image_filename: String;
begin
  tileset_index := index;
  tileset_menuitems[tileset_index].Checked := true;
  image_filename := current_dir+'/tilesets/'+tilesets[tileset_index].image_name+'.bmp';
  // Set tileset in .mis file
  Move(tilesets[tileset_index].name[1], mis_data.tileset, 8);
  Move(tilesets[tileset_index].tileatr_name[1], mis_data.tileatr, 8);
  // Load tileset graphics
  if FileExists(image_filename) then
  begin
    tileset_image_filename := image_filename;
    tileset_image.LoadFromFile(tileset_image_filename);
  end;
  // Load tileset attributes
  load_tileatr(current_dir+'/tilesets/'+tilesets[tileset_index].tileatr_name+'.bin');
  MainWindow.StatusBar.Panels[1].Text := tilesets[tileset_index].name;
  if (TilesetDialog <> nil) and TilesetDialog.Visible then
    TilesetDialog.DrawTileset(nil);
  // Re-render everything
  MainWindow.draw_cursor_image;
  MainWindow.render_minimap;
  MainWindow.render_map;
end;

procedure tileset_next;
begin
  tileset_index := tileset_index + 1;
  if tileset_index >= cnt_tilesets then
    tileset_index := 0;
  tileset_change(tileset_index);
end;

procedure tileset_load_custom_image(filename: String);
begin
  tileset_image.LoadFromFile(filename);
  tileset_image_filename := filename;
  tileset_menuitems[tileset_index].Checked := False;
  tileset_index := default_tileset;
  load_tileatr(current_dir+'/tilesets/'+tilesets[tileset_index].tileatr_name+'.bin');
  MainWindow.StatusBar.Panels[1].Text := 'Tileset File';
  MainWindow.render_map;
end;

procedure load_tileatr(filename: String);
var
  tileatr_file: file of cardinal;
begin
  if not FileExists(filename) then
    exit;
  AssignFile(tileatr_file, filename);
  Reset(tileatr_file);
  BlockRead(tileatr_file, tileset_attributes, cnt_tileset_tiles);
  CloseFile(tileatr_file);
  tileset_attributes_filename := filename;
end;

function get_tile_type(tile: word): TileType;
var
  atr: cardinal;
begin
  atr := tileset_attributes[tile];
  if (atr and $00006000) = 0 then
    result := ttImpassable
  else if (atr and $00006000) = $00004000 then
    result := ttInfantryOnly
  else if (atr and $40000000) = $40000000 then
    result := ttSlowdown
  else if (atr and $8000) = $8000 then
    result := ttBuildable
  else if (atr and $20000000) = $20000000 then
    result := ttRock
  else if (atr and $40) = $40 then
    result := ttIce
  else
    result := ttNormal
end;

function get_fill_area_type(tile: word; special: word): FillAreaType;
var
  atr: cardinal;
begin
  atr := tileset_attributes[tile];
  if (special = 1) or (special = 2) then
    result := faSpice
  else if (atr and 1) = 1 then
    result := faSand
  else if (atr and 8) = 8 then
    result := faDunes
  else if (atr and 32) = 32 then
    result := faSandDecorations
  else if (atr and 64) = 64 then
    result := faIce
  else
    result := faOther;
end;

end.
