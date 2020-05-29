unit _map;

interface

uses _structures, _tileset;

const max_map_width = 128;
const max_map_height = 128;
const max_undo_steps = 32767;

type
  TMapTile = record
    tile: word;
    special: word;
  end;

type
  TMapData = array[0..max_map_width-1, 0..max_map_height-1] of TMapTile;
  TMapDataPtr = ^TMapData;

type
  TUndoEntry = record
    x, y: byte;
    is_first: boolean;
    data: TMapTile;
  end;

type
  TPlayerStats = record
    power_percent: word;
    power_output: word;
    power_need: word;
    num_refineries: word;
  end;

type
  TMapStats = record
    players: array[0..cnt_players-1] of TPlayerStats;
    objects: array[0..3] of integer;
    num_structures_total: integer;
  end;

type
  TMap = class

  private
    // Map variables
    map_loaded: boolean;
    map_filename: String;
    map_modified: boolean;
    map_data: TMapData;
    map_width: word;
    map_height: word;

    // Statistics variables
    map_stats: TMapStats;

    // Undo variables
    undo_history: array[0..max_undo_steps] of TUndoEntry;
    undo_start: integer;
    undo_max: integer;
    undo_pos: integer;
    undo_pos_last_saved: integer;
    undo_block_start: boolean;

    // Search variables
    search_last_special: word;
    search_last_x, search_last_y: integer;

    // Temporary variables
    tmp_paint_tile_group: integer;
    tmp_last_direction: boolean;
    tile_dirty: array[0..max_map_width-1, 0..max_map_height-1] of boolean;

  public
    Property loaded: boolean read map_loaded;
    Property filename: String read map_filename write map_filename;
    Property data: TMapData read map_data;
    Property width: word read map_width;
    Property height: word read map_height;
    Property stats: TMapStats read map_stats;

    // Basic map manipulation procedures
  private
    procedure modify_map_tile(x,y: integer; tile: word; special: word);
    procedure paint_tile(x,y: integer; paint_tile_group: integer);
  public
    procedure set_special_value(x,y: integer; special: word);
    procedure paint_rect(x,y, width, height: integer; paint_tile_group: integer);
    procedure copy_block(x,y, width, height: integer; block: TMapDataPtr; copy_terrain, copy_structures: boolean; area_type: integer; erase: boolean);
    procedure put_block(x,y, width, height: integer; block: TMapDataPtr);
    function check_structure_can_be_placed(x,y: integer; special: word): boolean;

    // Fill area procedures
  public
    procedure fill_area_start(x,y: integer; paint_tile_group: integer);
  private
    procedure fill_area_step(x,y: integer; area_type: integer);

    // Undo & Redo procedures
  public
    procedure do_undo;
    procedure do_redo;
  private
    procedure reset_undo_history;

    // Procedures related to auto-smoothing edges
  private
    function check_edge_tile(x,y: integer; exact: boolean): boolean;
    procedure put_edge_block(var xpos, ypos: integer; moveoff_x, moveoff_y, blockoff_x, blockoff_y: integer; smooth_preset_type: SmoothPresetType);
  public
    procedure smooth_edges(x, y: integer; paint_tile_group: integer);

    // Miscellaneous procedures
    procedure calculate_power_and_statistics;
    function check_errors: String;
    function check_map_modified: boolean;
    function search_special(special: word; var result_x, result_y: integer): boolean;

    // Load & Save procedures
    procedure load_map_file(filename: String);
    procedure save_map_file(filename: String);

    // Map actions
    procedure set_map_size(new_width, new_height: integer);
    procedure shift_map(direction, num_tiles: integer);
    procedure change_structure_owner(player_from, player_to: integer; swap: boolean);
    function remap_tiles(ini_filename: String): boolean;
    procedure new_map(new_width, new_height: integer);

  end;

var
  Map: TMap;


implementation

uses Windows, Forms, SysUtils, Math, IniFiles, Classes, _renderer, _mission, _settings, main;


// Modify map tile and save old values into undo history.
// Map data should not be modified outside this or undo/redo methods.
procedure TMap.modify_map_tile(x, y: integer; tile, special: word);
begin
  undo_history[undo_pos].x := x;
  undo_history[undo_pos].y := y;
  undo_history[undo_pos].data := map_data[x,y];
  undo_history[undo_pos].is_first := undo_block_start;
  undo_block_start := false;
  undo_pos := (undo_pos + 1) and max_undo_steps;
  if undo_start = undo_pos then
    undo_start := (undo_start + 1) and max_undo_steps;
  undo_max := undo_pos;
  MainWindow.Undo1.Enabled := true;
  MainWindow.Redo1.Enabled := false;
  if tile <> 65535 then
    map_data[x,y].tile := tile;
  if special <> 65535 then
    map_data[x,y].special := special;
  Renderer.invalidate_map_tile(x, y);
end;

// Apply "paint" operation to a single tile
procedure TMap.paint_tile(x, y: integer; paint_tile_group: integer);
begin
  if (Settings.RestrictPainting) and (not Tileset.check_paint_tile_restriction(map_data[x,y].tile, map_data[x,y].special, paint_tile_group)) then
    exit;
  if (paint_tile_group < -2) and (Tileset.paint_tile_groups[paint_tile_group].paint_tiles_cnt = 0) then
    // Paint spice
    modify_map_tile(x, y, map_data[x,y].tile, (paint_tile_group+5))
  else
    // Paint sand/rock/dunes etc.
    modify_map_tile(x, y, Tileset.get_random_paint_tile(paint_tile_group), 0);
end;

// Set special value of a tile (single undo-operation)
procedure TMap.set_special_value(x, y: integer; special: word);
begin
  undo_block_start := true;
  modify_map_tile(x, y, map_data[x,y].tile, special);
end;

procedure TMap.paint_rect(x, y, width, height: integer; paint_tile_group: integer);
var
  xx, yy: integer;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  for xx := x to x + width - 1 do
    for yy := y to y + height - 1 do
    begin
      if (xx >= map_width) or (xx < 0) or (yy >= map_height) or (yy < 0) then
        continue;
      paint_tile(xx, yy, paint_tile_group);
    end;
end;

procedure TMap.copy_block(x, y, width, height: integer; block: TMapDataPtr; copy_terrain, copy_structures: boolean; area_type: integer; erase: boolean);
var
  xx, yy: integer;
  value: TMapTile;
begin
  if erase then
  begin
    undo_block_start := true;
    Renderer.invalidate_init;
  end;
  for xx := 0 to width - 1 do
    for yy := 0 to height - 1 do
    begin
      if (x + xx < map_width) and (y + yy < map_height) then
      begin
        value := map_data[x + xx, y + yy];
        if (area_type = -1) or Tileset.check_area_type(value.tile, value.special, area_type) then
        begin
          if erase then
            modify_map_tile(x + xx, y + yy, IfThen(copy_terrain, Tileset.get_random_paint_tile(Tileset.default_paint_group), 65535), IfThen((not copy_terrain) and (value.special <= 2), 65535, 0));
          if (not copy_structures) and (value.special > 2) then
            value.special := 0;
        end else
        begin
          // Do not copy tile that does not match selected area type
          value.tile := 65535;
          value.special := 65535;
        end;
        if not copy_terrain then
          value.tile := 65535;
        if (not copy_terrain) and (value.special <= 2) then
          value.special := 65535;
      end else
      begin
        value.tile := 0;
        value.special := 0;
      end;
      block[xx,yy] := value;
    end;
end;

procedure TMap.put_block(x, y, width, height: integer; block: TMapDataPtr);
var
  xx, yy: integer;
begin
  undo_block_start := true;
  for xx := 0 to width - 1 do
    for yy := 0 to height - 1 do
      if (x + xx < map_width) and (x + xx >= 0) and (y + yy < map_height) and (y + yy >= 0) then
        modify_map_tile(x + xx, y + yy, block[xx,yy].tile, block[xx,yy].special);
end;

function TMap.check_structure_can_be_placed(x, y: integer; special: word): boolean;
var
  index, player: word; is_misc: boolean;
begin
  result := true;
  // Check if building exceeds the map bounds
  if Structures.special_value_to_params(special, player, index, is_misc) then
  begin
    if (not is_misc) and ((x + Structures.structure_info[index].size_x > map_width) or (y + Structures.structure_info[index].size_y > map_height)) then
    begin
      result := false;
      exit;
    end;
  end;
end;


procedure TMap.fill_area_start(x, y: integer; paint_tile_group: integer);
var
  tmp_pos: integer;
begin
  // Undo the action which was made by first click
  tmp_pos := undo_pos;
  repeat
    tmp_pos := (tmp_pos - 1) and max_undo_steps
  until undo_history[tmp_pos].is_first;
  if (undo_history[tmp_pos].x = x) and (undo_history[tmp_pos].y = y) then
    do_undo;
  // Reset dirty flags
  FillChar(tile_dirty, max_map_width * max_map_height, 0);
  // Fill area
  undo_block_start := true;
  Renderer.invalidate_init;
  tmp_paint_tile_group := paint_tile_group;
  fill_area_step(x, y, Tileset.get_fill_area_type(map_data[x, y].tile, map_data[x, y].special));
end;

procedure TMap.fill_area_step(x, y: integer; area_type: integer);
begin
  if (not Tileset.check_area_type(map_data[x,y].tile, map_data[x,y].special, area_type)) or tile_dirty[x, y] or ((Tileset.tile_paint_group[map_data[x,y].tile] = tmp_paint_tile_group) and (map_data[x,y].special = 0)) then
    exit;
  paint_tile(x, y, tmp_paint_tile_group);
  tile_dirty[x, y] := true;
  if x > 0 then
    fill_area_step(x-1, y, area_type);
  if x < (map_width - 1) then
    fill_area_step(x+1, y, area_type);
  if y > 0 then
    fill_area_step(x, y-1, area_type);
  if y < (map_height - 1) then
    fill_area_step(x, y+1, area_type);
end;


procedure TMap.do_undo;
var
  tmp_data: TMapTile;
  x, y: word;
begin
  if undo_pos = undo_start then
    exit;
  repeat
    undo_pos := (undo_pos - 1) and max_undo_steps;
    x := undo_history[undo_pos].x;
    y := undo_history[undo_pos].y;
    tmp_data := map_data[x, y];
    map_data[x, y] := undo_history[undo_pos].data;
    undo_history[undo_pos].data := tmp_data;
    Renderer.invalidate_map_tile(x, y);
  until undo_history[undo_pos].is_first or (undo_pos = undo_start);
  if undo_pos = undo_start then
    MainWindow.Undo1.Enabled := false;
  MainWindow.Redo1.Enabled := true;
  calculate_power_and_statistics;
end;

procedure TMap.do_redo;
var
  tmp_data: TMapTile;
  x, y: word;
begin
  if undo_pos = undo_max then
    exit;
  repeat
    x := undo_history[undo_pos].x;
    y := undo_history[undo_pos].y;
    tmp_data := map_data[x, y];
    map_data[x, y] := undo_history[undo_pos].data;
    undo_history[undo_pos].data := tmp_data;
    Renderer.invalidate_map_tile(x, y);
    undo_pos := (undo_pos + 1) and max_undo_steps;
  until undo_history[undo_pos].is_first or (undo_pos = undo_max);
  if undo_pos = undo_max then
    MainWindow.Redo1.Enabled := false;
  MainWindow.Undo1.Enabled := true;
  calculate_power_and_statistics;
end;

procedure TMap.reset_undo_history;
begin
  MainWindow.Undo1.Enabled := false;
  MainWindow.Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
  undo_pos_last_saved := 0;
  undo_history[0].is_first := true;
end;


function TMap.check_edge_tile(x, y: integer; exact: boolean): boolean;
begin
  if x < 0 then x := 0;
  if y < 0 then y := 0;
  if x >= map_width then x := map_width - 1;
  if y >= map_height then y := map_height - 1;
  result := Tileset.tile_paint_group[map_data[x,y].tile] = tmp_paint_tile_group;
  if not exact then
  begin
    if (tmp_paint_tile_group >= 0) then
      result := result or ((Tileset.attributes_editor[map_data[x,y].tile] and (1 shl tmp_paint_tile_group)) <> 0)
    else
      result := result or (Tileset.attributes_editor[map_data[x,y].tile] = 0);
  end;
end;

procedure TMap.put_edge_block(var xpos, ypos: integer; moveoff_x,
  moveoff_y, blockoff_x, blockoff_y: integer; smooth_preset_type: SmoothPresetType);
var
  block_preset: PBlockPreset;
  smooth_preset_group: integer;
  x, y: integer;
begin
  smooth_preset_group := Tileset.paint_tile_groups[tmp_paint_tile_group].smooth_preset_group;
  block_preset := @Tileset.block_presets[Tileset.get_block_preset(smooth_preset_group, ord(Tileset.paint_tile_groups[tmp_paint_tile_group].smooth_presets[ord(smooth_preset_type)+1]), bpRandom)];
  // Reuse already defined block-key-presets for this purpose
  // Place edge block (it can be either 1x1 or 2x2, so we use loops)
  for y := 0 to block_preset.height - 1 do
    for x := 0 to block_preset.width - 1 do
    begin
      // We cannot place the edge block immediately physically into map,
      // because it would interfere with checks for tiles around following tile.
      // Instead, we exploit undo feature for this purpose: we store all changes
      // into history and in the end we apply the changes (like doing redo)
      undo_history[undo_max].x := x + xpos + blockoff_x;
      undo_history[undo_max].y := y + ypos + blockoff_y;
      undo_history[undo_max].data.tile := Tileset.block_preset_tiles[block_preset.block_preset_tile_index + x + y * block_preset.width];
      undo_history[undo_max].data.special := 0;
      undo_history[undo_max].is_first := false;
      undo_max := (undo_max + 1) and max_undo_steps;
    end;
  // Finally move to next position (anticlockwise direction)
  xpos := xpos + moveoff_x;
  ypos := ypos + moveoff_y;
  tmp_last_direction := (moveoff_x > 0) or (moveoff_y > 0);
end;

procedure TMap.smooth_edges(x, y: integer; paint_tile_group: integer);
var
  start_x, start_y: integer;
  sum: integer;
  steps: integer;
  use_curves, use_cornerlink: boolean;
begin
  start_x := x;
  start_y := y;
  Renderer.invalidate_init;
  tmp_paint_tile_group := paint_tile_group;
  use_curves := length(Tileset.paint_tile_groups[tmp_paint_tile_group].smooth_presets) = 20;
  use_cornerlink := length(Tileset.paint_tile_groups[tmp_paint_tile_group].smooth_presets) = 14;
  undo_max := undo_pos;
  steps := 0;
  // Start smoothing edge from starting point (where user shift-clicked)
  while check_edge_tile(x, y, true) do
  begin
    // Check for all 8 tiles around current tile to determine the direction of edge
    sum := 0;
    if check_edge_tile(x,   y-1, false) then sum := sum + 1;   // 16 1 32
    if check_edge_tile(x-1, y  , false) then sum := sum + 2;   //  2 X 4
    if check_edge_tile(x+1, y  , false) then sum := sum + 4;   // 64 8 128
    if check_edge_tile(x  , y+1, false) then sum := sum + 8;
    if check_edge_tile(x-1, y-1, false) then sum := sum + 16;
    if check_edge_tile(x+1, y-1, false) then sum := sum + 32;
    if check_edge_tile(x-1, y+1, false) then sum := sum + 64;
    if check_edge_tile(x+1, y+1, false) then sum := sum + 128;
    // Transform current tile into edge tile and move to following tile
    case (sum and 15) of
       7: begin // down
          if use_curves and (sum and 128 > 0) and not check_edge_tile(x+1,y+2, false) then
            put_edge_block(x,y,2,1,0,0,spCurveNegDown)
          else
            put_edge_block(x,y,1,0,0,0,spStraightDown);
        end;
      11: begin // right
          if use_curves and (sum and 32 > 0) and not check_edge_tile(x+2,y-1, false) then
            put_edge_block(x,y,1,-2,0,-1,spCurveNegRight)
          else
            put_edge_block(x,y,0,-1,0,0,spStraightRight);
        end;
      14: begin // up
          if use_curves and (sum and 16 > 0) and not check_edge_tile(x-1,y-2, false) then
            put_edge_block(x,y,-2,-1,-1,-1,spCurveNegUp)
          else
            put_edge_block(x,y,-1,0,0,0,spStraightUp);
        end;
      13: begin // left
          if use_curves and (sum and 64 > 0) and not check_edge_tile(x-2,y+1, false) then
            put_edge_block(x,y,-1,2,-1,0,spCurveNegLeft)
          else
            put_edge_block(x,y,0,1,0,0,spStraightLeft);
        end;
       3: begin // down-right corner
          if use_curves and (sum and 32 > 0) and check_edge_tile(x+2,y-1, false) then
            put_edge_block(x,y,2,-1,0,-1,spCurvePosDown)
          else
            put_edge_block(x,y,0,-1,0,0,spCornerRightDown);
        end;
      10: begin // up-right corner
          if use_curves and (sum and 16 > 0) and check_edge_tile(x-1,y-2, false) then
            put_edge_block(x,y,-1,-2,-1,-1,spCurvePosRight)
          else
            put_edge_block(x,y,-1,0,0,0,spCornerUpRight);
        end;
      12: begin // up-left corner
          if use_curves and (sum and 64 > 0) and check_edge_tile(x-2,y+1, false) then
            put_edge_block(x,y,-2,1,-1,0,spCurvePosUp)
          else
            put_edge_block(x,y,0,1,0,0,spCornerLeftUp);
        end;
       5: begin // down-left corner
          if use_curves and (sum and 128 > 0) and check_edge_tile(x+1,y+2, false) then
            put_edge_block(x,y,1,2,0,0,spCurvePosLeft)
          else
            put_edge_block(x,y,1,0,0,0,spCornerDownLeft);
        end;
      15: begin // inner turns
        if use_cornerlink and (sum = 111) then
          put_edge_block(x,y,IfThen(tmp_last_direction,-1,1),0,0,0,spCurveNegLeft)
        else if use_cornerlink and (sum = 159) then
          put_edge_block(x,y,0,IfThen(tmp_last_direction,1,-1),0,0,spCurveNegUp)
        else
        case sum of
          239: put_edge_block(x,y,-1,0,0,0,spTurnLeftUp); // down-right turn
          191: put_edge_block(x,y,0,1,0,0,spTurnDownLeft);  // up-right turn
          127: put_edge_block(x,y,1,0,0,0,spTurnRightDown);  // up-left turn
          223: put_edge_block(x,y,0,-1,0,0,spTurnUpRight); // down-left turn
          else break; // Invalid combination - end
        end;
        end;
      else break; // Invalid combination - end
    end;
    // End if we got back into starting point
    if (x = start_x) and (y = start_y) then
      break;
    // End if we got outside the map
    if (x < 0) or (y < 0) or (x >= map_width) or (y >= map_height) then
      break;
    // Sometimes the algorithm may end up in infinite loop. This is to prevent it.
    inc(steps);
    if steps > 1000 then
      break;
  end;
  undo_history[undo_pos].is_first := true;
  // Finally put smoothed edges on map - apply all changes we stored into undo history
  do_redo;
end;


procedure TMap.calculate_power_and_statistics;
var
  output, need: array[0..cnt_players-1] of integer;
  i, x, y: integer;
  player, index: word;
  is_misc: boolean;
  tmp_power: integer;
  stats_group: integer;
begin
  for i := 0 to Length(map_stats.objects) - 1 do
    map_stats.objects[i] := 0;
  for x := 0 to cnt_players - 1 do
  begin
    output[x] := 0;
    need[x] := 0;
    map_stats.players[x].num_refineries := 0;
  end;
  map_stats.num_structures_total := 0;
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
          inc(map_stats.num_structures_total);
          if structures.structure_info[index].tiledataindex = 7 then
            inc(map_stats.players[player].num_refineries);
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

function TMap.check_errors: String;
var
  x, y: integer;
  index, player: word;
  is_misc: boolean;
  num_player_starts: integer;
  num_spice_blooms: integer;
  num_structures_total: integer;
  num_refineries: integer;
begin
  // Check for number of worm spawners
  if map_stats.objects[ord(sgWormSpawners)] = 0 then
  begin
    result := 'You must place at least one Worm Spawner.';
    exit;
  end;
  // Check for number of player starts
  num_player_starts := map_stats.objects[ord(sgPlayerStarts)];
  if (num_player_starts <> 0) and (num_player_starts <> 8) then
  begin
    result := format('Invalid number of Player Starts (%d). Must be either 0 (for campaign maps) or 8 (for multiplayer maps).', [num_player_starts]);
    exit;
  end;
  // Check for limit of spice blooms
  num_spice_blooms := map_stats.objects[ord(sgSpiceBlooms)];
  if num_spice_blooms > Structures.limit_spice_blooms then
  begin
    result := format('You placed %d spice blooms on map, which is more than maximum of %d supported by game.'#13'The spice blooms above the limit will not work.', [num_spice_blooms, Structures.limit_spice_blooms]);
    exit;
  end;
  // Check for limit of structures
  num_structures_total := map_stats.num_structures_total + map_stats.objects[ord(sgWormSpawners)];
  if num_structures_total > Structures.limit_structures_total then
  begin
    result := format('You placed %d structures on map, which is more than game''s limit of %d.'#13'Remove some buildings or units.', [num_structures_total, Structures.limit_structures_total]);
    exit;
  end;
  // Check for limit of refineries per player
  for player := 0 to cnt_players - 1 do
  begin
    num_refineries := map_stats.players[player].num_refineries;
    if num_refineries > Structures.limit_refineries_per_player then
    begin
      result := format('You placed %d refineries for player %s on map, which is more than game''s limit of %d.'#13'Remove some refineries.', [num_refineries, Structures.player_info[player].name, Structures.limit_refineries_per_player]);
      exit;
    end;
  end;
  // Check for buildings exceeding map bounds
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
    begin
      if (x < map_width - max_building_width) and (y < map_height - max_building_height) then
        continue;
      if Structures.special_value_to_params(map_data[x, y].special, player, index, is_misc) then
      begin
        if (not is_misc) and ((x + Structures.structure_info[index].size_x > map_width) or (y + Structures.structure_info[index].size_y > map_height)) then
        begin
          result := format('Building at %d,%d (%s) exceeds the map bounds.', [x, y, Structures.structure_info[index].name]);
          exit;
        end;
      end;
    end;
  if Mission.mis_assigned then
    result := Mission.check_errors
  else
    result := '';
end;

function TMap.check_map_modified: boolean;
begin
  if not map_loaded then
  begin
    result := false;
    exit;
  end;
  result := (undo_pos_last_saved <> undo_pos) or map_modified or Mission.mis_modified;
end;

function TMap.search_special(special: word; var result_x, result_y: integer): boolean;
var
  x, y: integer;
begin
  // Searching for a different special - reset starting position
  if special <> search_last_special then
  begin
    search_last_special := special;
    search_last_x := map_width - 1;
    search_last_y := map_height - 1;
  end;
  x := search_last_x;
  y := search_last_y;
  repeat
    Inc(x);
    if x = map_width then
    begin
      x := 0;
      Inc(y);
      if y = map_height then
        y := 0;
    end;
    // Found
    if map_data[x, y].special = special then
    begin
      result_x := x;
      result_y := y;
      search_last_x := x;
      search_last_y := y;
      result := true;
      exit;
    end;
  until (x = search_last_x) and (y = search_last_y);
  result := false;
end;


procedure TMap.load_map_file(filename: String);
var
  map_file: file of word;
  x, y: integer;
begin
  // Reset map data
  for x := 0 to max_map_width-1 do
    for y := 0 to max_map_height-1 do
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
  map_modified := false;
  reset_undo_history;
  calculate_power_and_statistics;
end;

procedure TMap.save_map_file(filename: String);
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
  if ExtractFileName(filename) <> 'TESTMAP.MAP' then
  begin
    map_modified := false;
    undo_pos_last_saved := undo_pos;
  end;
end;


procedure TMap.set_map_size(new_width, new_height: integer);
var
  i, j: integer;
begin
  if (map_width = new_width) and (map_height = new_height) then
    exit;
  // Fill additional area with clean sand
  for i := 0 to new_height - 1 do
    for j := 0 to new_width - 1 do
      if (i >= map_height) or (j >= map_width) then
      begin
        map_data[j,i].tile := Tileset.get_random_paint_tile(Tileset.default_paint_group);
        map_data[j,i].special := 0;
      end;
  // Set new map size
  map_width := new_width;
  map_height := new_height;
  // Adjust also mission
  Mission.adjust_event_positions_on_map_resize;
  reset_undo_history;
  map_modified := true;
  calculate_power_and_statistics;
end;

procedure TMap.shift_map(direction, num_tiles: integer);
var
  x, y: integer;
  src_x, src_y: integer;
begin
  case direction of
    1:  begin // Left
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_x := x + num_tiles;
              if (src_x < map_width) then
                map_data[x,y] := map_data[src_x,y]
              else
              begin
                map_data[x,y].tile := Tileset.get_random_paint_tile(Tileset.default_paint_group);
                map_data[x,y].special := 0;
              end;
            end;
          Mission.shift_event_positions(num_tiles * -1, 0);
        end;
    2:  begin // Up
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_y := y + num_tiles;
              if (src_y < map_height) then
                map_data[x,y] := map_data[x,src_y]
              else
              begin
                map_data[x,y].tile := Tileset.get_random_paint_tile(Tileset.default_paint_group);
                map_data[x,y].special := 0;
              end;
            end;
          Mission.shift_event_positions(0, num_tiles * -1);
        end;
    3:  begin // Right
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_x := x - num_tiles;
              if (src_x >= 0) then
                map_data[x,y] := map_data[src_x,y]
              else
              begin
                map_data[x,y].tile := Tileset.get_random_paint_tile(Tileset.default_paint_group);
                map_data[x,y].special := 0;
              end;
            end;
          Mission.shift_event_positions(num_tiles, 0);
        end;
    4:  begin
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_y := y - num_tiles;
              if (src_y >= 0) then
                map_data[x,y] := map_data[x,src_y]
              else
              begin
                map_data[x,y].tile := Tileset.get_random_paint_tile(Tileset.default_paint_group);
                map_data[x,y].special := 0;
              end;
            end;
          Mission.shift_event_positions(0, num_tiles);
        end;
  end;
  reset_undo_history;
  map_modified := true;
  calculate_power_and_statistics;
end;

procedure TMap.change_structure_owner(player_from,
  player_to: integer; swap: boolean);
var
  x,y: integer;
  player, index: word;
  is_misc: boolean;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  for y:= 0 to map_height - 1 do
    for x := 0 to map_width - 1 do
    begin
      if Structures.special_value_to_params(map_data[x,y].special, player, index, is_misc) and (not is_misc) then
      begin
        // Change from -> to
        if player = player_from then
        begin
          if player_to = cnt_players then
            modify_map_tile(x, y, map_data[x,y].tile, 0)
          else
            modify_map_tile(x, y, map_data[x,y].tile, Structures.structure_info[index].values[player_to]);
        end;
        // Swap is checked (change to -> from)
        if (player = player_to) and swap then
          modify_map_tile(x, y, map_data[x,y].tile, Structures.structure_info[index].values[player_from]);
      end;
    end;
  calculate_power_and_statistics;
end;

function TMap.remap_tiles(ini_filename: String): boolean;
var
  ini: TMemIniFile;
  strings: TStringList;
  from_tiles, to_tiles: array of word;
  count: integer;
  i, x, y: integer;
begin
  // Load ini file
  ini := TMemIniFile.Create(ini_filename);
  strings := TStringList.Create;
  ini.ReadSection('Remap_Tiles', strings);
  count := strings.Count;
  if count = 0 then
  begin
    result := false;
    exit;
  end;
  SetLength(from_tiles, count);
  SetLength(to_tiles, count);
  for i := 0 to count - 1 do
  begin
    from_tiles[i] := strtoint(strings[i]);
    to_tiles[i] := ini.ReadInteger('Remap_Tiles', strings[i], 0);
  end;
  // Remap the tiles
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
      for i := 0 to count - 1 do
        if map_data[x, y].tile = from_tiles[i] then
        begin
          map_data[x, y].tile := to_tiles[i];
          break;
        end;
  reset_undo_history;
  map_modified := true;
  result := true;
end;

procedure TMap.new_map(new_width, new_height: integer);
var
  x, y: integer;
begin
  // Reset map data
  for x := 0 to max_map_width-1 do
    for y := 0 to max_map_height-1 do
    begin
      map_data[x,y].tile := 0;
      map_data[x,y].special := 0;
    end;
  // Initialize map
  map_width := new_width;
  map_height := new_height;
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
    begin
      map_data[x,y].tile := Tileset.get_random_paint_tile(Tileset.default_paint_group);
      map_data[x,y].special := 0;
    end;
  if Settings.PreplaceWormSpawner then
    map_data[0,0].special := Structures.misc_object_info[1].value;
  map_filename := '';
  map_loaded := true;
  map_modified := false;
  reset_undo_history;
  calculate_power_and_statistics;
end;

end.
