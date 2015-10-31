unit _structures;

interface

uses Graphics, SysUtils, Math, Types;

const cnt_players = 8;
const cnt_tiledata_entries = 1000;

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
    power: SmallInt; // Power the structure gives/needs
    values: array[0..cnt_players-1] of word; // Map special values
  end;

type
  TMapPlayerInfo = record
    name: String;
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

const concrete_tiles: array[0..5] of word = (651, 657, 658, 659, 671, 691);

type
  TStructures = class

  public
    cnt_structures: integer;
    structure_info: array of TStructureInfo;
    first_unit_index: integer;

    cnt_map_players: integer;
    map_player_info: array of TMapPlayerInfo;

    cnt_misc_objects: integer;
    misc_object_info: array of TMiscObjectInfo;

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

  // Read list of map players
  ini := TMemIniFile.Create(current_dir + 'config/map_players.ini');
  ini.ReadSections(tmp_strings);
  cnt_map_players := IfThen(tmp_strings.Count <= cnt_players, tmp_strings.Count, cnt_players);
  SetLength(map_player_info, cnt_map_players);
  for i := 0 to cnt_map_players-1 do
  begin
    sname := tmp_strings[i];
    with map_player_info[i] do
    begin
      name := sname;
      color := ini.ReadInteger(sname, 'color', $0);
    end;
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

