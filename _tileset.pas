unit _tileset;

interface

uses Graphics, Menus;

// Tileset constants
const cnt_tileset_tiles = 800;
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

// Tileset class
type
  TTileset = class

  private
    menuitems: array of TMenuItem;

  public
    tileimage_filename: String;
    attributes_filename: String;  
    tileimage: TBitmap;
    attributes: array[0..cnt_tileset_tiles-1] of cardinal;
    current_tileset: integer;
    cnt_tilesets: integer;
    tileset_info: array of TTilesetInfo;

  public
    procedure init;
    procedure change_tileset(index: integer);
    procedure next_tileset;
    procedure load_custom_image(filename: String);
    procedure load_tileatr(filename: String);
    function get_tile_type(tile: word): TileType;
    function get_fill_area_type(tile: word; special: word): FillAreaType;

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
  // Re-render everything
  MainWindow.draw_cursor_image;
  MainWindow.render_minimap;
  MainWindow.render_map;
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
begin
  if not FileExists(filename) then
    exit;
  AssignFile(tileatr_file, filename);
  Reset(tileatr_file);
  BlockRead(tileatr_file, attributes, cnt_tileset_tiles);
  CloseFile(tileatr_file);
  attributes_filename := filename;
end;

function TTileset.get_tile_type(tile: word): TileType;
var
  atr: cardinal;
begin
  atr := attributes[tile];
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

function TTileset.get_fill_area_type(tile: word; special: word): FillAreaType;
var
  atr: cardinal;
begin
  atr := attributes[tile];
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
