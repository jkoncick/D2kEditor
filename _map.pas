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
    players: array[0..cnt_map_players-1] of TPlayerStats;
    objects: array[0..3] of integer;
  end;

const tiles_sand: array[0..9] of word = (48,49,50,51,52,68,69,70,71,72);
const tiles_rock: array[0..14] of word = (552,553,554,555,556,572,573,574,575,576,592,593,594,595,596);
const tiles_dunes: array[0..7] of word = (63,64,65,66,83,84,103,104);

const block_key_presets: array[1..53,0..3,0..3] of word = (
  // Up
  ((2,2,16,18), (2,2, 5, 4), (1,1, 8,14), (1,1, 0, 3)),
  ((2,3,16,22), (2,3, 6,10), (2,2,18,12), (2,2,16, 0)),
  ((2,2, 7,26), (2,2, 7, 4), (1,1,11,14), (1,1, 1, 3)),
  ((2,2, 9,26), (2,2, 0, 8), (1,1,12,14), (1,1, 1, 3)),
  ((2,2, 0,28), (2,2, 2, 8), (1,1,13,14), (1,1, 1, 3)),
  ((1,2,17,33), (1,2,15,33), (1,1,14,14), (1,1, 1, 3)),
  ((0,0, 0, 0), (0,0, 0, 0), (1,1,15,14), (1,1, 1, 3)),
  ((2,3,14,22), (2,3, 4,10), (2,2,16,12), (2,2,18, 0)),
  ((2,2,18,18), (2,2, 9, 4), (1,1,16,14), (1,1, 2, 3)),
  ((2,2,18,18), (2,2, 9, 4), (1,1,13,15), (1,1, 2, 3)),
  // Left
  ((3,2, 0,26), (3,2, 8,12), (2,2, 6,13), (2,2,10, 0)),
  ((2,2, 4,22), (2,2,18, 4), (1,1,14,16), (1,1, 0, 4)),
  ((2,2, 6,22), (2,2,18, 6), (1,1,15,16), (1,1, 0, 4)),
  ((2,2, 8,22), (2,2, 6, 8), (1,1,16,16), (1,1, 0, 4)),
  ((2,1,18,34), (2,1,12,34), (1,1,17,16), (1,1, 0, 4)),
  ((0,0, 0, 0), (0,0, 0, 0), (1,1,14,15), (1,1, 0, 4)),
  ((3,2, 0,24), (3,2, 8,10), (2,2, 4,13), (2,2, 4, 1)),
  // Right
  ((3,2,11,23), (3,2,11,12), (2,2, 0,13), (2,2, 6, 1)),
  ((2,2, 5,24), (2,2,11, 4), (1,1,11,16), (1,1, 2, 4)),
  ((2,2, 7,24), (2,2,13, 4), (1,1,12,16), (1,1, 2, 4)),
  ((2,2, 9,24), (2,2, 8, 8), (1,1,13,16), (1,1, 2, 4)),
  ((2,1,18,33), (2,1, 9,34), (1,1,15,15), (1,1, 2, 4)),
  ((3,2,11,25), (3,2,11,10), (2,2, 2,13), (2,2, 2, 1)),
  // Down
  ((2,2,16,20), (2,2, 0, 6), (1,1, 9,16), (1,1, 0, 5)),
  ((2,3, 2,20), (2,3, 0,10), (2,2,18, 8), (2,2,16, 2)),
  ((2,2, 4,20), (2,2, 2, 6), (1,1, 6,16), (1,1, 1, 5)),
  ((2,2, 6,20), (2,2,14, 6), (1,1, 7,16), (1,1, 1, 5)),
  ((2,2, 8,20), (2,2,16, 6), (1,1,17,15), (1,1, 1, 5)),
  ((1,2,16,33), (1,2,14,33), (0,0, 0, 0), (1,1, 1, 5)),
  ((2,3,10,20), (2,3, 2,10), (2,2,18,10), (2,2,18, 2)),
  ((2,2,18,20), (2,2, 4, 6), (1,1,18,15), (1,1, 2, 5)),
  // Inner curves
  ((2,2, 4,28), (2,2,12, 6), (1,1, 3,15), (1,1, 0, 1)),
  ((2,2,10,28), (2,2, 8, 6), (1,1, 3,16), (1,1, 0, 2)),
  ((2,2, 6,28), (2,2,10, 6), (1,1, 4,15), (1,1, 1, 1)),
  ((2,2, 8,28), (2,2, 6, 6), (1,1, 4,16), (1,1, 1, 2)),
  // Up
  ((2,2, 5,26), (2,2,14,10), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,18,24), (2,2,18,22), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,18,22), (2,2,18,24), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2, 2,28), (2,2,16,10), (0,0, 0, 0), (0,0, 0, 0)),
  // Left
  ((2,2, 0,22), (2,2,14, 8), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,12, 0), (2,2,14, 0), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,14, 0), (2,2,12, 0), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2, 3,24), (2,2,16, 8), (0,0, 0, 0), (0,0, 0, 0)),
  // Right
  ((2,1,12,22), (2,1,18,14), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,14, 2), (2,2,15, 4), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,15, 4), (2,2,14, 2), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2, 3,26), (2,2,14,12), (0,0, 0, 0), (0,0, 0, 0)),
  // Down
  ((2,2, 0,20), (2,2,10, 8), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,14,25), (2,2,16,25), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,16,25), (2,2,14,25), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,12,20), (2,2,12, 8), (0,0, 0, 0), (0,0, 0, 0)),
  // Others
  ((3,3, 0,30), (2,2, 3,30), (0,0, 0, 0), (0,0, 0, 0)),
  ((1,2,13, 2), (2,2, 5,30), (0,0, 0, 0), (0,0, 0, 0))
  );

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
  output, need: array[0..cnt_map_players-1] of integer;
  i, x, y: integer;
  player, index: word;
  is_misc: boolean;
  tmp_power: integer;
  stats_group: integer;
begin
  for i := 1 to Length(map_stats.objects) do
    map_stats.objects[i] := 0;
  for x := 0 to cnt_map_players - 1 do
  begin
    output[x] := 0;
    need[x] := 0;
  end;
  // Process all map and compute power output/need and number of misc objects
  for y := 0 to map_height - 1 do
    for x := 0 to map_width -1 do
    begin
      if special_value_to_params(map_data[x,y].special,player,index,is_misc) then
      begin
        if is_misc then
        begin
          stats_group := ord(misc_object_info[index].stats_group);
          if stats_group > 0 then
            inc(map_stats.objects[stats_group]);
        end else
        begin
          tmp_power := structure_info[index].power;
          if tmp_power > 0 then
            need[player] := need[player] + tmp_power
          else if tmp_power < 0 then
            output[player] := output[player] - tmp_power;
        end;
      end;
    end;
  // Calculate power value from output/need for each player
  for x := 0 to cnt_map_players - 1 do
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
