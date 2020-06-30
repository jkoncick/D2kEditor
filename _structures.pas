unit _structures;

interface

uses Windows, Classes, Graphics, SysUtils, Math, Types;

const cnt_players = 8;
const cnt_tiledata_entries = 1000;

const max_building_width = 3;
const max_building_height = 4;

type
  TMisAIProperty = record
    name: String;
    data_type: char;
    position: integer;
  end;

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

  private
    graphics_structures_filename: String;
    graphics_misc_objects_filename: String;
    structures_ini_filename: String;
    tiledata_bin_filename: String;
    players_ini_filename: String;
    misc_objects_ini_filename: String;
    buildings_txt_filename: String;
    buildings2_txt_filename: String;
    units_txt_filename: String;
    colours_bin_filename: String;
    limits_ini_filename: String;

    pending_render_map: boolean;
    pending_render_minimap: boolean;
    pending_fill_events_and_conditions: boolean;
    pending_update_mis_ai_properties: boolean;

    tiledata: array[0..cnt_tiledata_entries-1] of TTileDataEntry;

    mis_ai_properties_template: array of TMisAIProperty;
  public
    // Mis AI properties data
    mis_ai_properties: array of TMisAIProperty;
    cnt_mis_ai_properties: integer;

    // Graphic data
    graphics_structures: TBitmap;
    graphics_structures_mask: TBitmap;
    graphics_misc_objects: TBitmap;
    graphics_misc_objects_mask: TBitmap;

    // Structures data
    cnt_structures: integer;
    structure_info: array of TStructureInfo;
    first_unit_index: integer;

    player_info: array[0..cnt_players-1] of TPlayerInfo;

    cnt_misc_objects: integer;
    misc_object_info: array of TMiscObjectInfo;

    building_names: TStringList;
    building2_names: TStringList;
    unit_names: TStringList;

    // Limits
    limit_spice_blooms: integer;
    limit_structures_total: integer;
    limit_refineries_per_player: integer;
  private


  public
    procedure init;
    function find_file(pattern: String): String;
    procedure load_mis_ai_properties_ini;
    procedure load_graphics_structures;
    procedure load_graphics_misc_objects;
    procedure load_structures_ini;
    procedure load_tiledata_bin;
    procedure load_players_ini;
    procedure load_misc_objects_ini;
    procedure load_buildings_txt;
    procedure load_buildings2_txt;
    procedure load_units_txt;
    procedure load_colours_bin;
    procedure load_limits_ini;
    procedure do_pending_actions(skip: boolean);
    procedure update_mis_ai_properties;
    function special_value_is_valid(special: word): boolean;
    function special_value_to_params(special: word; var player: word; var index: word; var is_misc: boolean): boolean;
    function check_links_with_wall(special: word): boolean;
    function get_building_name(index: integer): String;
    function get_unit_name(index: integer): String;
  end;

var
  Structures: TStructures;

implementation

uses Forms, IniFiles, main, set_dialog, test_map_dialog, map_stats_dialog, mission_dialog, event_dialog, _settings;

procedure TStructures.init;
begin
  graphics_structures := TBitmap.Create;
  graphics_structures_mask := TBitmap.Create;
  graphics_misc_objects := TBitmap.Create;
  graphics_misc_objects_mask := TBitmap.Create;
  building_names := TStringList.Create;
  building2_names := TStringList.Create;
  unit_names := TStringList.Create;
  load_mis_ai_properties_ini;
  load_graphics_structures;
  load_graphics_misc_objects;
  load_structures_ini;
  load_tiledata_bin;
  load_players_ini;
  load_misc_objects_ini;
  load_buildings_txt;
  load_buildings2_txt;
  load_units_txt;
  load_colours_bin;
  load_limits_ini;
  do_pending_actions(true);
end;

function TStructures.find_file(pattern: String): String;
var
  tmp_filename, tmp_filename2: String;
begin
  tmp_filename := current_dir + pattern;
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\' + MissionDialog.cbModsFolder.Text + '\' + pattern;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox(PChar('Could not find file ' + pattern), 'Error loading configuration or graphics file', MB_OK or MB_ICONERROR);
    tmp_filename := '';
  end;
  result := tmp_filename;
end;

procedure TStructures.load_mis_ai_properties_ini;
var
  tmp_filename: String;
  i: integer;
  ini: TMemIniFile;
  tmp_strings: TStringList;
begin
  tmp_filename := current_dir + 'config\mis_ai_properties.ini';
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file config\mis_ai_properties.ini', 'Error loading configuration file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // Load misai properties from ini file
  tmp_strings := TStringList.Create;
  ini := TMemIniFile.Create(tmp_filename);
  ini.ReadSection('AI',tmp_strings);
  SetLength(mis_ai_properties_template, tmp_strings.Count);
  SetLength(mis_ai_properties, tmp_strings.Count);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    mis_ai_properties_template[i].name := ini.ReadString('AI',tmp_strings[i],'');
    mis_ai_properties_template[i].data_type := tmp_strings[i][1];
    mis_ai_properties_template[i].position := strtoint(Copy(tmp_strings[i], 3, Length(tmp_strings[i]) - 2));
  end;
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TStructures.load_graphics_structures;
var
  tmp_filename, mask_filename: String;
begin
  tmp_filename := find_file('graphics\structures.bmp');
  if (tmp_filename = '') or (tmp_filename = graphics_structures_filename) then
    exit;
  graphics_structures_filename := tmp_filename;
  // Load graphics from files
  graphics_structures.LoadFromFile(tmp_filename);
  // Load mask if file exists, otherwise create mask and save it
  mask_filename := ChangeFileExt(tmp_filename, '') + '_mask.bmp';
  if not FileExists(mask_filename) then
  begin
    graphics_structures_mask.LoadFromFile(tmp_filename);
    graphics_structures_mask.Mask(clBlack);
    graphics_structures_mask.SaveToFile(mask_filename);
  end;
  graphics_structures_mask.LoadFromFile(mask_filename);
  // Update all occurences in editor
  pending_render_map := true;
end;

procedure TStructures.load_graphics_misc_objects;
var
  tmp_filename, mask_filename: String;
begin
  tmp_filename := find_file('graphics\misc_objects.bmp');
  if (tmp_filename = '') or (tmp_filename = graphics_misc_objects_filename) then
    exit;
  graphics_misc_objects_filename := tmp_filename;
  // Load graphics from files
  graphics_misc_objects.LoadFromFile(tmp_filename);
  // Load mask if file exists, otherwise create mask and save it
  mask_filename := ChangeFileExt(tmp_filename, '') + '_mask.bmp';
  if not FileExists(mask_filename) then
  begin
    graphics_misc_objects_mask.LoadFromFile(tmp_filename);
    graphics_misc_objects_mask.Mask(clBlack);
    graphics_misc_objects_mask.SaveToFile(mask_filename);
  end;
  graphics_misc_objects_mask.LoadFromFile(mask_filename);
  // Update all occurences in editor
  pending_render_map := true;
end;

procedure TStructures.load_structures_ini;
var
  tmp_filename: String;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder: TStringList;
  i: integer;
  sname : string;
begin
  tmp_filename := find_file('config\structures.ini');
  if (tmp_filename = '') or (tmp_filename = structures_ini_filename) then
    exit;
  structures_ini_filename := tmp_filename;
  // Read list of structures and their properties from structures.ini
  ini := TMemIniFile.Create(tmp_filename);
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
  tmp_strings.Destroy;
  decoder.Destroy;
  // Update all occurences in editor
  MainWindow.update_structures_list;
  MapStatsDialog.update_structures_list;
  pending_render_map := true;
  pending_render_minimap := true;
end;

procedure TStructures.load_tiledata_bin;
var
  tmp_filename, tmp_filename2: String;
  tiledatafile: file of TTileDataEntry;
  i,j,s,e: integer;
  found: boolean;
begin
  // Step 1 - editor's internal file
  tmp_filename := current_dir + 'config\TILEDATA.BIN';
  // Step 2 - game's internal file
  tmp_filename2 := Settings.GamePath + '\Data\bin\TILEDATA.BIN';
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Step 3 - file under CustomCampaignData folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\' + MissionDialog.cbModsFolder.Text + '\Data\bin\TILEDATA.BIN';
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file TILEDATA.BIN', 'Error loading game file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // This file is already loaded - do not load it again
  if tmp_filename = tiledata_bin_filename then
    exit;
  tiledata_bin_filename := tmp_filename;
  // Read TILEDATA.BIN file and retrieve special values of structures
  AssignFile(tiledatafile, tmp_filename);
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
  // Add misc objects into TILEDATA.BIN
  for i := 0 to cnt_misc_objects-1 do
  begin
    with misc_object_info[i] do
    begin
      tiledata[value].index := i;
      tiledata[value].player := 0;
      tiledata[value].stype := byte(stMiscObject);
    end;
  end;
  // Update all occurences in editor
  pending_render_map := true;
  pending_render_minimap := true;
end;

procedure TStructures.load_players_ini;
var
  tmp_filename, tmp_filename2: String;
  ini: TMemIniFile;
  player_list: TStringList;
  i: integer;
begin
  // Step 1 - editor's internal file
  tmp_filename := current_dir + 'config\players.ini';
  // Step 2 - file under CustomCampaignData folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\' + MissionDialog.cbModsFolder.Text + '\config\players.ini';
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Step 3 - file under Players folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\Players\' + MissionDialog.cbPlayersIni.Text;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file config\players.ini', 'Error loading configuration file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // This file is already loaded - do not load it again
  if tmp_filename = players_ini_filename then
    exit;
  players_ini_filename := tmp_filename;
  // Read list of players
  ini := TMemIniFile.Create(tmp_filename);
  player_list := TStringList.Create;
  for i := 0 to cnt_players-1 do
  begin
    player_info[i].name := ini.ReadString('Player'+inttostr(i), 'name', 'Unnamed');
    player_info[i].shortname := ini.ReadString('Player'+inttostr(i), 'short', player_info[i].name);
    player_list.Add(inttostr(i) + ' - ' + player_info[i].name);
  end;
  ini.Destroy;
  // Update all occurences in editor
  MainWindow.update_player_list(player_list);
  SetDialog.update_player_list(player_list);
  TestMapDialog.update_player_list(player_list);
  MapStatsDialog.update_player_list;
  MissionDialog.update_player_list(player_list);
  EventDialog.update_player_list(player_list);
  pending_fill_events_and_conditions := true;
  player_list.Destroy;
end;

procedure TStructures.load_misc_objects_ini;
var
  tmp_filename: String;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i: integer;
  sname : string;
begin
  tmp_filename := find_file('config\misc_objects.ini');
  if (tmp_filename = '') or (tmp_filename = misc_objects_ini_filename) then
    exit;
  misc_objects_ini_filename := tmp_filename;
  // Read list of miscellaneous objects
  ini := TMemIniFile.Create(tmp_filename);
  tmp_strings := TStringList.Create;
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
  // Update all occurences in editor
  MainWindow.update_misc_object_list;
  pending_render_map := true;
  pending_render_minimap := true;
end;

procedure TStructures.load_buildings_txt;
var
  tmp_filename: String;
begin
  tmp_filename := find_file('config\buildings.txt');
  if (tmp_filename = '') or (tmp_filename = buildings_txt_filename) then
    exit;
  buildings_txt_filename := tmp_filename;
  // Read list of buildings
  building_names.LoadFromFile(tmp_filename);
  // Update all occurences in editor
  pending_update_mis_ai_properties := true;
end;

procedure TStructures.load_buildings2_txt;
var
  tmp_filename: String;
begin
  tmp_filename := find_file('config\buildings2.txt');
  if (tmp_filename = '') or (tmp_filename = buildings2_txt_filename) then
    exit;
  buildings2_txt_filename := tmp_filename;
  // Read list of buildings
  building2_names.LoadFromFile(tmp_filename);
  // Update all occurences in editor
  EventDialog.update_building_list;
  pending_fill_events_and_conditions := true;
  pending_update_mis_ai_properties := true;
end;

procedure TStructures.load_units_txt;
var
  tmp_filename: String;
begin
  tmp_filename := find_file('config\units.txt');
  if (tmp_filename = '') or (tmp_filename = units_txt_filename) then
    exit;
  units_txt_filename := tmp_filename;
  // Read list of units
  unit_names.LoadFromFile(tmp_filename);
  // Update all occurences in editor
  EventDialog.update_unit_list;
  pending_fill_events_and_conditions := true;
  pending_update_mis_ai_properties := true;
end;

procedure TStructures.load_colours_bin;
var
  tmp_filename, tmp_filename2: String;
  colours_bin_file: file of byte;
  colours_bin_contents: array[0..127] of word;
  color: Cardinal;
  i, j: integer;
begin
  // Step 1 - editor's internal file
  tmp_filename := current_dir + 'config\COLOURS.BIN';
  if Settings.LoadCustomColoursBin then
  begin
    // Step 2 - game's internal file
    tmp_filename2 := Settings.GamePath + '\Data\bin\COLOURS.BIN';
    if FileExists(tmp_filename2) then
      tmp_filename := tmp_filename2;
    // Step 3 - file under CustomCampaignData folder
    tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\Colours\' + MissionDialog.cbColoursBin.Text;
    if FileExists(tmp_filename2) then
      tmp_filename := tmp_filename2;
  end;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file COLOURS.BIN', 'Error loading game file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // This file is already loaded - do not load it again
  if tmp_filename = colours_bin_filename then
    exit;
  colours_bin_filename := tmp_filename;
  // Load COLOURS.BIN file
  AssignFile(colours_bin_file, tmp_filename);
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
  // Update all occurences in editor
  MissionDialog.update_player_colors;
  pending_render_map := true;
  pending_render_minimap := true;
end;

procedure TStructures.load_limits_ini;
var
  tmp_filename: String;
  ini: TMemIniFile;
begin
  tmp_filename := find_file('config\limits.ini');
  if (tmp_filename = '') or (tmp_filename = limits_ini_filename) then
    exit;
  limits_ini_filename := tmp_filename;
  // Read limits from limits.ini
  ini := TMemIniFile.Create(tmp_filename);
  limit_spice_blooms := ini.ReadInteger('Limits', 'spice_blooms', 30);
  limit_structures_total := ini.ReadInteger('Limits', 'structures_total', 1000);
  limit_refineries_per_player := ini.ReadInteger('Limits', 'refineries_per_player', 10);
  ini.Destroy;
end;

procedure TStructures.do_pending_actions(skip: boolean);
begin
  if pending_update_mis_ai_properties then
    update_mis_ai_properties;
  if not skip then
  begin
    if pending_render_map then
      MainWindow.render_map;
    if pending_render_minimap then
      MainWindow.render_minimap;
    if pending_fill_events_and_conditions then
      EventDialog.update_contents;
    if pending_update_mis_ai_properties then
      MissionDialog.fill_ai_values;
  end;
  pending_render_map := false;
  pending_render_minimap := false;
  pending_fill_events_and_conditions := false;
  pending_update_mis_ai_properties := false;
end;

procedure TStructures.update_mis_ai_properties;
var
  i, position, num: integer;
  name: String;
begin
  cnt_mis_ai_properties := 0;
  for i := 0 to Length(mis_ai_properties_template) - 1 do
  begin
    name := mis_ai_properties_template[i].name;
    // Replace building name
    position := Pos('B#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+2, 2), 100);
      if num < building_names.Count then
        name := Copy(name, 0, position-1) + building_names[num]
      else
        continue;
    end;
    // Replace building2 name
    position := Pos('B2#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+3, 2), 100);
      if num < building2_names.Count then
        name := Copy(name, 0, position-1) + building2_names[num]
      else
        continue;
    end;
    // Replace unit name
    position := Pos('U#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+2, 2), 60);
      if num < unit_names.Count then
        name := Copy(name, 0, position-1) + unit_names[num]
      else
        continue;
    end;
    mis_ai_properties[cnt_mis_ai_properties].name := name;
    mis_ai_properties[cnt_mis_ai_properties].data_type := mis_ai_properties_template[i].data_type;
    mis_ai_properties[cnt_mis_ai_properties].position := mis_ai_properties_template[i].position;
    inc(cnt_mis_ai_properties);
  end;
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

function TStructures.get_building_name(index: integer): String;
begin
  if index < building2_names.Count then
    result := building2_names[index]
  else
    result := 'UNDEFINED#' + inttostr(index);
end;

function TStructures.get_unit_name(index: integer): String;
begin
  if index < unit_names.Count then
    result := unit_names[index]
  else
    result := 'UNDEFINED#' + inttostr(index);
end;

end.

