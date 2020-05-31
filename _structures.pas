unit _structures;

interface

uses Graphics, SysUtils, Math, Types;

const cnt_players = 8;
const cnt_tiledata_entries = 1000;

const max_building_width = 3;
const max_building_height = 4;

type StructureType = (stNothing, stMiscObject, stBuilding, stUnit);

type
  TTileDataEntry = record
    index: word;
    player: byte;
    stype: byte;
  end;

type
  TStructureInfo = record
    name: String;
    tiledataindex: word; // Index of structure in TILEDATA.BIN file
    pos_x: word; // X-position in structures image
    pos_y: word; // Y-position in structures image
    size_x: word; // Structure width
    size_y: word; // Structure height
    size_adjust: TRect;
    bottom_style: word; // Style of building's bottom
    linkwall: boolean;  // Structure links with wall
    not_on_buildable: boolean; // Building does not need to be placed on buildable tiles
    power: SmallInt; // Power the structure gives/needs
    values: array[0..cnt_players-1] of word; // Map special values
  end;

type
  TPlayerInfo = record
    name: String;
    shortname: String;
    color: TColor;
  end;

type TObjectStatsGroup = (sgNone, sgWormSpawners, sgPlayerStarts, sgSpiceBlooms);

type
  TMiscObjectInfo = record
    name: String;
    value: word;
    color: TColor;
    stats_group: word;
  end;

type
  TBottomStyleType = record
    size_x: integer;
    size_y: integer;
    rock_tile_x: integer;
    rock_tile_y: integer;
    conc_tile_x: integer;
    conc_tile_y: integer;
  end;

const bottom_style_types: array[0..3] of TBottomStyleType =
  (
    (size_x: 0; size_y: 0; rock_tile_x:  0; rock_tile_y:  0; conc_tile_x:  0; conc_tile_y:  0), // none
    (size_x: 2; size_y: 2; rock_tile_x: 17; rock_tile_y: 30; conc_tile_x:  9; conc_tile_y: 32), // 2x2
    (size_x: 3; size_y: 2; rock_tile_x: 11; rock_tile_y: 30; conc_tile_x:  3; conc_tile_y: 32), // 3x2
    (size_x: 3; size_y: 3; rock_tile_x: 14; rock_tile_y: 30; conc_tile_x:  6; conc_tile_y: 32)  // 3x3
  );

type
  TStructures = class

  public
    cnt_structures: integer;
    structure_info: array of TStructureInfo;
    first_unit_index: integer;

    player_info: array[0..cnt_players-1] of TPlayerInfo;

    cnt_misc_objects: integer;
    misc_object_info: array of TMiscObjectInfo;

    limit_spice_blooms: integer;
    limit_structures_total: integer;
    limit_refineries_per_player: integer;
  private
    tiledata: array[0..cnt_tiledata_entries-1] of TTileDataEntry;

  public
    procedure init;
    function special_value_is_valid(special: word): boolean;
    function special_value_to_params(special: word; var player: word; var index: word; var is_misc: boolean): boolean;
    function check_links_with_wall(special: word): boolean;

  end;

var
  Structures: TStructures;

implementation

uses Classes, IniFiles, main;

procedure TStructures.init;
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder: TStringList;
  tiledatafile: file of TTileDataEntry;
  sname : string;
  i,j,s,e: integer;
  found: boolean;
  colours_bin_file: file of byte;
  colours_bin_contents: array[0..127] of word;
  color: Cardinal;
begin
  // Read list of structures and their properties from structures.ini
  ini := TMemIniFile.Create(current_dir + 'config/structures.ini');
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder.Delimiter := '.';
  ini.ReadSections(tmp_strings);
  cnt_structures := tmp_strings.Count;
  SetLength(structure_info, cnt_structures);
  first_unit_index := cnt_structures;
  for i := 0 to cnt_structures - 1 do
  begin
    sname := tmp_strings[i];
    if ini.ReadBool(sname, 'first_unit', false) = true then
      first_unit_index := i;
    with structure_info[i] do
    begin
      name := sname;
      tiledataindex := ini.ReadInteger(sname, 'tiledataindex', 0);
      pos_x := ini.ReadInteger(sname, 'pos_x', 0);
      pos_y := ini.ReadInteger(sname, 'pos_y', 0);
      size_x := ini.ReadInteger(sname, 'size_x', 1);
      size_y := ini.ReadInteger(sname, 'size_y', 1);
      decoder.DelimitedText := ini.ReadString(sname, 'size_adjust', '');
      if decoder.Count = 4 then
      begin
        size_adjust.Top := strtoint(decoder[0]);
        size_adjust.Left := strtoint(decoder[1]);
        size_adjust.Bottom := strtoint(decoder[2]);
        size_adjust.Right := strtoint(decoder[3]);
      end;
      bottom_style := ini.ReadInteger(sname, 'bottom_style', 0);
      if (bottom_style >= Length(bottom_style_types)) then
        bottom_style := 0;
      linkwall := ini.ReadBool(sname, 'linkwall', false);
      not_on_buildable := ini.ReadBool(sname, 'not_on_buildable', false);
      power := ini.ReadInteger(sname, 'power', 0);
    end;
  end;
  ini.Destroy;

  // Read TILEDATA.BIN file and retrieve special values of structures
  AssignFile(tiledatafile, current_dir + 'config/TILEDATA.BIN');
  Reset(tiledatafile);
  BlockRead(tiledatafile, tiledata, cnt_tiledata_entries);
  CloseFile(tiledatafile);
  for i := 0 to cnt_tiledata_entries-1 do
  begin
    // Empty TILEDATA entry
    if (tiledata[i].index = 65535) and (tiledata[i].player = 255) and (tiledata[i].stype = 255) then
    begin
      tiledata[i].index := 0;
      tiledata[i].player := 0;
      tiledata[i].stype := byte(stNothing);
      continue;
    end;
    // Non-empty TILEDATA entry
    if tiledata[i].stype = $80 then
    begin
      // Building
      s := 0;
      e := first_unit_index - 1;
      tiledata[i].stype := byte(stBuilding);
    end else
    begin
      // Unit
      s := first_unit_index;
      e := cnt_structures -1;
      tiledata[i].stype := byte(stUnit);
    end;
    // Translate tiledata index to structure index and save special value
    found := false;
    for j := s to e do
      if structure_info[j].tiledataindex = tiledata[i].index then
      begin
        tiledata[i].index := j;
        structure_info[j].values[tiledata[i].player] := i;
        found := true;
        break;
      end;
    // Tiledata index not defined for any structure
    if not found then
    begin
      tiledata[i].index := 0;
      tiledata[i].player := 0;
      tiledata[i].stype := byte(stNothing);
    end;
  end;

  // Read list of players
  ini := TMemIniFile.Create(current_dir + 'config/players.ini');
  for i := 0 to cnt_players-1 do
  begin
    player_info[i].name := ini.ReadString('Player'+inttostr(i+1), 'name', 'Unnamed');
    player_info[i].shortname := ini.ReadString('Player'+inttostr(i+1), 'short', player_info[i].name);
  end;
  ini.Destroy;

  // Read list of miscellaneous objects
  ini := TMemIniFile.Create(current_dir + 'config/misc_objects.ini');
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
      tiledata[value].index := i;
      tiledata[value].player := 0;
      tiledata[value].stype := byte(stMiscObject);
    end;
  end;
  ini.Destroy;

  // Read limits from limits.ini
  ini := TMemIniFile.Create(current_dir + 'config/limits.ini');
  limit_spice_blooms := ini.ReadInteger('Limits', 'spice_blooms', 30);
  limit_structures_total := ini.ReadInteger('Limits', 'structures_total', 1000);
  limit_refineries_per_player := ini.ReadInteger('Limits', 'refineries_per_player', 10);
  ini.Destroy;

  // Read COLOURS.BIN file
  AssignFile(colours_bin_file, current_dir + 'config/COLOURS.BIN');
  Reset(colours_bin_file);
  BlockRead(colours_bin_file, colours_bin_contents[0], 256);
  CloseFile(colours_bin_file);
  for i := 0 to cnt_players-1 do
  begin
    j := i*16 + 8;
    color := 0;
    color := color or (((colours_bin_contents[j] and $7C00) shr 10) shl 3) or (((colours_bin_contents[j] and $7C00) shr 12) shl 0);
    color := color or (((colours_bin_contents[j] and $03E0) shr 5) shl 11) or (((colours_bin_contents[j] and $03E0) shr 7) shl 8);
    color := color or (((colours_bin_contents[j] and $001F) shr 0) shl 19) or (((colours_bin_contents[j] and $001F) shr 2) shl 16);
    player_info[i].color := color;
  end;

  tmp_strings.Destroy;
  decoder.Destroy;
end;

function TStructures.special_value_is_valid(special: word): boolean;
begin
  result := (special <> 0) and (special < cnt_tiledata_entries) and (tiledata[special].stype <> byte(stNothing));
end;

function TStructures.special_value_to_params(special: word; var player, index: word; var is_misc: boolean): boolean;
begin
  if not special_value_is_valid(special) then
  begin
    result := false;
    exit;
  end;
  player := tiledata[special].player;
  index := tiledata[special].index;
  is_misc := tiledata[special].stype = byte(stMiscObject);
  result := true;
end;

function TStructures.check_links_with_wall(special: word): boolean;
begin
  result := special_value_is_valid(special) and structure_info[tiledata[special].index].linkwall;
end;

end.

