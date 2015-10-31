unit _map;

// This unit contains Map-related and statistics definitions, constants
// and several helper functions.

interface

uses _structures;

type
  TMapTile = record
    tile: word;
    special: word;
  end;

type
  TPlayerStats = record
    power_percent: word;
    power_output: word;
    power_need: word;
  end;

type
  TMapStats = record
    players: array[0..cnt_players-1] of TPlayerStats;
    objects: array[0..3] of integer;
  end;

var
  // Map variables
  map_loaded: boolean;
  map_filename: String;
  map_data: array[0..127, 0..127] of TMapTile;
  map_width: word;
  map_height: word;

  // Statistics variables
  map_stats: TMapStats;

  // Helper map-related functions and procedures
  procedure calculate_power_and_statistics;
  function check_map_errors: boolean;
  procedure load_map_file(filename: String);
  procedure save_map_file(filename: String);

implementation

uses Windows, Forms, SysUtils, _mission, main;

procedure calculate_power_and_statistics;
var
  output, need: array[0..cnt_players-1] of integer;
  i, x, y: integer;
  player, index: word;
  is_misc: boolean;
  tmp_power: integer;
  stats_group: integer;
begin
  for i := 1 to Length(map_stats.objects) do
    map_stats.objects[i] := 0;
  for x := 0 to cnt_players - 1 do
  begin
    output[x] := 0;
    need[x] := 0;
  end;
  // Process all map and compute power output/need and number of misc objects
  for y := 0 to map_height - 1 do
    for x := 0 to map_width -1 do
    begin
      if Structures.special_value_to_params(map_data[x,y].special,player,index,is_misc) then
      begin
        if is_misc then
        begin
          stats_group := Structures.misc_object_info[index].stats_group;
          if stats_group > 0 then
            inc(map_stats.objects[stats_group]);
        end else
        begin
          tmp_power := Structures.structure_info[index].power;
          if tmp_power > 0 then
            need[player] := need[player] + tmp_power
          else if tmp_power < 0 then
            output[player] := output[player] - tmp_power;
        end;
      end;
    end;
  // Calculate power value from output/need for each player
  for x := 0 to cnt_players - 1 do
  begin
    if (output[x] > 0) and (need[x] = 0) then
      map_stats.players[x].power_percent := output[x] div 2 + 100
    else if (output[x] = 0) then
      map_stats.players[x].power_percent := 0
    else
      map_stats.players[x].power_percent := round((ln(output[x] / need[x] + 1) / ln(2)) * 100);
    map_stats.players[x].power_output := output[x];
    map_stats.players[x].power_need := need[x];
  end;
  MainWindow.show_power_and_statistics;
end;

function check_map_errors: boolean;
var
  player_starts: integer;
begin
  result := true;
  // Check for number of worm spawners
  if map_stats.objects[ord(sgWormSpawners)] = 0 then
  begin
    Application.MessageBox('You must place at least one Worm Spawner.', 'Map error', MB_ICONWARNING);
    result := false;
  end;
  // Check for number of player starts
  player_starts := map_stats.objects[ord(sgPlayerStarts)];
  if (player_starts <> 0) and (player_starts <> 8) then
  begin
    Application.MessageBox('Invalid number of Player Starts. Must be either 0 (for campaign maps) or 8 (for multiplayer maps).', 'Map error', MB_ICONWARNING);
    result := false;
  end;
end;

procedure load_map_file(filename: String);
var
  map_file: file of word;
  x, y: integer;
begin
  // Reset map data
  for x := 0 to 127 do
    for y := 0 to 127 do
    begin
      map_data[x,y].tile := 0;
      map_data[x,y].special := 0;
    end;
  // Read map file
  AssignFile(map_file, filename);
  Reset(map_file);
  Read(map_file, map_width);
  Read(map_file, map_height);
  for y := 0 to map_height - 1 do
    for x := 0 to map_width - 1 do
    begin
      Read(map_file, map_data[x,y].tile);
      Read(map_file, map_data[x,y].special);
    end;
  CloseFile(map_file);
  map_loaded := true;
  map_filename := filename;
end;

procedure save_map_file(filename: String);
var
  map_file: file of word;
  x, y: integer;
begin
  AssignFile(map_file, filename);
  ReWrite(map_file);
  Write(map_file, map_width);
  Write(map_file, map_height);
  for y := 0 to map_height - 1 do
    for x := 0 to map_width - 1 do
    begin
      Write(map_file, map_data[x,y].tile);
      Write(map_file, map_data[x,y].special);
    end;
  CloseFile(map_file);
end;

end.
