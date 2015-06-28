unit _map;

// This unit contains Map-related and statistics definitions, constants
// and several helper functions.

interface

uses Graphics;

const cnt_map_players = 7;

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
  TMapStructureInfo = record
    name: String;
    offs_x: word; // X-offset in image
    offs_y: word; // Y-offset in image
    size_x: word; // Structure width
    size_y: word; // Structure height
    overfl: word; // Sprite overflow (1 = up (for buildings), 2 = infantry, 3 = wide (harvester, MCV))
    lnwall: boolean;  // Structure links with wall
    power: SmallInt; // Power the structure gives/needs
    values: array[0..cnt_map_players-1] of word; // Map special values
  end;

const structure_params: array[0..31] of TMapStructureInfo =
  (
    (name: 'Wall';                  offs_x:  0; offs_y: 0; size_x: 1; size_y: 1; overfl:  0; lnwall:  true; power:   0; values:(  4,204,404,580,620,660,700)),
    (name: 'Wind Trap';             offs_x:  1; offs_y: 0; size_x: 2; size_y: 3; overfl:  1; lnwall: false; power:-200; values:(  5,205,405,581,621,661,701)),
    (name: 'Construction Yard';     offs_x:  3; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: -20; values:(  8,208,408,582,622,662,702)),
    (name: 'Barracks';              offs_x:  6; offs_y: 0; size_x: 2; size_y: 3; overfl:  1; lnwall: false; power:  30; values:( 11,211,411,583,623,663,703)),
    (name: 'Refinery';              offs_x:  8; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power:  75; values:( 14,214,414,584,624,664,704)),
    (name: 'Outpost';               offs_x: 11; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 125; values:( 17,217,417,585,625,665,705)),
    (name: 'Light Factory';         offs_x: 14; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 125; values:( 63,263,463,587,627,667,707)),
    (name: 'Silo';                  offs_x: 17; offs_y: 0; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:  15; values:( 69,269,469,589,629,668,708)),
    (name: 'Heavy Factory';         offs_x: 18; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power: 150; values:( 72,272,472,590,630,669,709)),
    (name: 'Repair Pad';            offs_x: 21; offs_y: 0; size_x: 3; size_y: 3; overfl:  0; lnwall: false; power:  50; values:( 75,275,475,591,631,670,710)),
    (name: 'Gun Turret';            offs_x: 24; offs_y: 0; size_x: 1; size_y: 1; overfl:  1; lnwall:  true; power:  50; values:( 78,278,478,592,632,671,711)),
    (name: 'High Tech Factory';     offs_x: 25; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power:  75; values:(120,320,520,593,633,672,712)),
    (name: 'Rocket Turret';         offs_x: 28; offs_y: 0; size_x: 1; size_y: 1; overfl:  1; lnwall:  true; power:  60; values:(123,323,523,594,634,673,713)),
    (name: 'IX Research Centre';    offs_x: 29; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power: 175; values:(126,326,526,595,635,674,714)),
    (name: 'Starport';              offs_x: 32; offs_y: 0; size_x: 3; size_y: 3; overfl:  0; lnwall: false; power: 150; values:(129,329,529,596,636,675,715)),
    (name: 'Palace';                offs_x: 35; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 200; values:(132,332,532,588,628,676,716)),
    (name: 'Sietch';                offs_x: 38; offs_y: 0; size_x: 2; size_y: 2; overfl:  0; lnwall: false; power:  50; values:(  0,  0,  0,597,637,  0,  0)),
    (name: 'Modified Outpost';      offs_x: 40; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 100; values:(  0,218,418,  0,  0,666,  0)),
    (name: 'Light Infantry';        offs_x: 43; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(180,360,560,598,638,677,717)),
    (name: 'Trooper';               offs_x: 44; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(181,361,561,599,639,678,718)),
    (name: 'St. Fremen / Saboteur'; offs_x: 45; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(182,362,562,600,640,679,719)),
    (name: 'Sardakaur / Fremen';    offs_x: 46; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(183,363,563,601,641,  0,  0)),
    (name: 'Engineer';              offs_x: 47; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(184,364,564,602,642,680,720)),
    (name: 'Harvester';             offs_x: 44; offs_y: 1; size_x: 1; size_y: 1; overfl:  3; lnwall: false; power:   0; values:(185,365,565,603,643,681,721)),
    (name: 'MCV';                   offs_x: 46; offs_y: 1; size_x: 1; size_y: 1; overfl:  3; lnwall: false; power:   0; values:(186,366,566,604,644,682,722)),
    (name: 'Trike / Raider';        offs_x: 43; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(187,367,567,605,645,683,723)),
    (name: 'Quad';                  offs_x: 44; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(188,368,568,606,646,684,724)),
    (name: 'Combat Tank';           offs_x: 45; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(189,369,569,607,647,685,725)),
    (name: 'Missile Tank';          offs_x: 46; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(190,370,570,608,648,686,726)),
    (name: 'Siege Tank';            offs_x: 47; offs_y: 3; size_x: 1; size_y: 1; overfl:  1; lnwall: false; power:   0; values:(191,371,571,609,649,687,727)),
    (name: 'Carryall';              offs_x: 17; offs_y: 1; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(192,372,572,610,650,688,728)),
    (name: 'House Special Tank';    offs_x: 47; offs_y: 1; size_x: 1; size_y: 1; overfl:  1; lnwall: false; power:   0; values:(194,374,574,  0,652,  0,  0))
  );

const misc_obj_values: array[0..9] of word = (0,1,2,20,23,45,41,42,43,44);

const mmap_player_colors: array[0..cnt_map_players-1] of TColor = ($84614A,$3231C6,$63824A,$6B0063,$747274,$00106B,$08728C);
const mmap_misc_objects_colors: array[1..9] of TColor = ($52AEF7,$2179E7,$FF00FF,$FFFF00,$0000FF,$0000B0,$0000C0,$0000D0,$0000E0);

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
  mstat_player: array[0..cnt_map_players-1] of TPlayerStats;
  mstat_num_worm_spawners: word;
  mstat_num_player_starts: word;
  mstat_num_spice_blooms: word;

  // Helper map-related functions and procedures
  function special_value_to_params(value: word; var player: word; var index: word; var is_misc: boolean): boolean;
  procedure calculate_power_and_statistics;
  function check_map_errors: boolean;
  procedure load_map_file(filename: String);
  procedure save_map_file(filename: String);

implementation

uses Windows, Forms, SysUtils, _mission, main;

function special_value_to_params(value: word; var player,
  index: word; var is_misc: boolean): boolean;
var i, j: integer;
begin
  if value = 0 then
  begin
    result := false;
    exit;
  end;
  for i:= 0 to 9 do
  begin
    if value = misc_obj_values[i] then
    begin
      index := i;
      is_misc := true;
      result := true;
      exit;
    end;
  end;
  for i := 0 to 31 do
  begin
    for j:= 0 to cnt_map_players - 1 do
    begin
      if value = structure_params[i].values[j] then
      begin
        player := j;
        index := i;
        is_misc := false;
        result := true;
        exit;
      end;
    end;
  end;
  result := false;
end;

procedure calculate_power_and_statistics;
var
  output, need: array[0..cnt_map_players-1] of integer;
  x, y: integer;
  player, index: word;
  is_misc: boolean;
  tmp_power: integer;
begin
  mstat_num_worm_spawners := 0;
  mstat_num_player_starts := 0;
  mstat_num_spice_blooms := 0;
  for x := 0 to cnt_map_players - 1 do
  begin
    output[x] := 0;
    need[x] := 0;
  end;
  for y := 0 to map_height - 1 do
    for x := 0 to map_width -1 do
    begin
      if special_value_to_params(map_data[x,y].special,player,index,is_misc) then
      begin
        if is_misc then
        begin
          if index = 3 then
            inc(mstat_num_worm_spawners)
          else if index = 4 then
            inc(mstat_num_player_starts)
          else if (index >= 5) and (index <= 9) then
            inc(mstat_num_spice_blooms)
        end else
        begin
          tmp_power := structure_params[index].power;
          if tmp_power > 0 then
            need[player] := need[player] + tmp_power
          else if tmp_power < 0 then
            output[player] := output[player] - tmp_power;
        end;
      end;
    end;
  for x := 0 to cnt_map_players - 1 do
  begin
    if (output[x] > 0) and (need[x] = 0) then
      mstat_player[x].power_percent := output[x] div 2 + 100
    else if (output[x] = 0) then
      mstat_player[x].power_percent := 0
    else
      mstat_player[x].power_percent := round((ln(output[x] / need[x] + 1) / ln(2)) * 100);
    mstat_player[x].power_output := output[x];
    mstat_player[x].power_need := need[x];
  end;
  MainWindow.show_power_and_statistics;
end;

function check_map_errors: boolean;
begin
  result := true;
  if mstat_num_worm_spawners = 0 then
  begin
    Application.MessageBox('You must place at least one Worm Spawner.', 'Map error', MB_ICONWARNING);
    result := false;
  end;
  if (mstat_num_player_starts <> 0) and (mstat_num_player_starts <> 8) then
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
