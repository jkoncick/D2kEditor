unit _randomgen;

interface

uses _tileset, _map, Classes;

const max_active_connection_points = 64;
const max_block_candidates = 16;
const max_backtrack_steps = 2;
const max_steps = 4096;
const max_hist_used_block_entries = max_steps * (max_block_candidates div 2);
const max_hist_placed_block_entries = max_steps;
const max_hist_active_cp_change_entries = max_steps * 2;
const max_hist_distinct_area_entries = 64;
const max_hist_filled_area_entries = max_hist_distinct_area_entries * 8;

const constraint_type_connectionPoint = 8;

const max_rating_check_distance = 8;
//const distance_rating: array[0..max_rating_check_distance-1] of single = (0.25, 0.50, 0.75, 1.50, 3.00, 2.00, 1.5, 1.25);
const distance_rating: array[0..max_rating_check_distance-1] of single = (0.1, 0.2, 0.5, 0.75, 2.00, 2.50, 3.0, 3.5);

type
  TActiveConnectionPoint = record
    pos_x: smallint;
    pos_y: smallint;
    connection_point_index: word;
    last_block_preset_index: word; // To avoid repetition of two same blocks next to each other
    parent_step: word;
  end;

type
  THistUsedBlocksEntry = record
    step: word;
    block_group: byte;
  end;

type
  THistPlacedBlocksEntry = record
    pos_x: smallint;
    pos_y: shortint;
    block_preset_index: word;
    num_backtracked: byte;
  end;

type
  THistActiveCPChangeEntry = record
    active_cp_index: byte;
    change_type: byte; // 0 = replacement, 1 = addition, 2 = removal
    step: word;
    active_cp: TActiveConnectionPoint;
  end;

type
  THistFilledAreaEntry = record
    pos_x: byte;
    pos_y: byte;
    area_type: byte;
  end;

type
  THistDistinctAreaEntry = record
    first_placed_block: word;
    first_filled_area: word;
    base_area_type: byte;
    areas_filled: boolean;
  end;

type
  TConstraint = record
    c_type: byte;
    radius: shortint;
  end;

type
  TConstraintCheck = record
    step: integer;
    c_type: byte;
    distance: byte;
    violated: boolean;
  end;

type
  TTileRecord = record
    constraints: array[0..3] of TConstraint;
    constraint_checks: array[0..3] of TConstraintCheck;
  end;

type
  TBlockCandidate = record
    block_index: word;
    pos_x: smallint;
    pos_y: smallint;
    repeated: boolean;
    rating: single;
    cummulative_weight: integer;
  end;

// Random Generator class
type
  TRandomGen = class

  private
    log_target: TStrings;

  public
    active_connection_points: array [0..max_active_connection_points-1] of TActiveConnectionPoint;
    num_active_connection_points: integer;
    hist_used_blocks: array[0..max_hist_used_block_entries-1] of THistUsedBlocksEntry;
    num_hist_used_blocks: integer;
    hist_placed_blocks: array[0..max_hist_placed_block_entries-1] of THistPlacedBlocksEntry;
    current_step: integer;
    hist_active_cp_changes: array[0..max_hist_active_cp_change_entries-1] of THistActiveCPChangeEntry;
    num_hist_active_cp_changes: integer;
    hist_filled_areas: array[0..max_hist_filled_area_entries-1] of THistFilledAreaEntry;
    num_hist_filled_areas: integer;
    hist_distinct_areas: array[0..max_hist_distinct_area_entries-1] of THistDistinctAreaEntry;
    num_hist_distinct_areas: integer;
    tile_records: array[0..max_map_width-1, 0..max_map_height-1] of TTileRecord;
    constraint_check_step: integer;

  public
    procedure init(string_list: TStrings);
    procedure reset;
    procedure place_seed_block(block_preset_index, pos_x, pos_y: integer; do_loop: boolean);
    procedure manual_step(active_cp_index: integer);
    procedure manual_undo(whole_distinct_area: boolean);

  private
    procedure do_log(log_entry: String);
    procedure randomgen_loop(max_steps: integer);
    procedure randomgen_step(active_cp_index: integer);
    procedure undo_last_placed_block;
    procedure undo_last_distinct_area(preserve_first_block: boolean);
    function get_random_block_for_connection_point(active_cp_index: integer; var res_block_index, res_pos_x, res_pos_y: integer): boolean;
    function place_block(block_preset_index, pos_x, pos_y: integer; trial_only: boolean): single;
    function check_tile_is_free(pos_x, pos_y, base_area_type: integer): boolean;
    procedure check_constraint_violation(pos_x, pos_y, direction, check_type: integer; strict: boolean; var constraint_violated: boolean);
    procedure add_tile_constraint(pos_x, pos_y, direction, c_type, radius: integer);
    procedure remove_tile_constraint(pos_x, pos_y, direction: integer);
    function fill_enclosed_areas: boolean;
    function fill_enclosed_area_step(pos_x, pos_y, direction, base_area_type, target_area_type: integer): boolean;
    procedure undo_fill_enclosed_areas;
    procedure undo_fill_enclosed_area_step(pos_x, pos_y, base_area_type, target_area_type: integer);
  end;

var
  RandomGen: TRandomGen;

implementation

uses SysUtils, Math, main;

{ TRandomGen }

procedure TRandomGen.init(string_list: TStrings);
begin
  log_target := string_list;
  reset;
end;

procedure TRandomGen.reset;
begin
  num_active_connection_points := 0;
  num_hist_used_blocks := 0;
  current_step := 0;
  num_hist_active_cp_changes := 0;
  num_hist_filled_areas := 0;
  num_hist_distinct_areas := 0;
  constraint_check_step := 0;
  FillChar(tile_records[0,0], sizeof(tile_records), 0);
end;

procedure TRandomGen.place_seed_block(block_preset_index, pos_x, pos_y: integer; do_loop: boolean);
begin
  place_block(block_preset_index, pos_x, pos_y, false);
  if do_loop then
    randomgen_loop(10000);
end;

procedure TRandomGen.manual_step(active_cp_index: integer);
begin
  if (num_active_connection_points > 0) then
    randomgen_step(current_step mod num_active_connection_points)
    //randomgen_step(0)
  else if (num_hist_distinct_areas > 0) and (not hist_distinct_areas[num_hist_distinct_areas-1].areas_filled) then
  begin
    if not fill_enclosed_areas then
      undo_last_distinct_area(current_step > hist_distinct_areas[num_hist_distinct_areas-1].first_placed_block+1);
  end else
    beep;
end;

procedure TRandomGen.manual_undo(whole_distinct_area: boolean);
begin
  if num_hist_distinct_areas = 0 then
    exit;
  if whole_distinct_area then
  begin
    // Undo whole distinct area
    undo_last_distinct_area(false);
    MainWindow.render_minimap;
  end else
  begin
    // Undo single step
    if num_hist_filled_areas > hist_distinct_areas[num_hist_distinct_areas-1].first_filled_area then
      undo_fill_enclosed_areas
    else
      undo_last_placed_block;
  end;
end;

procedure TRandomGen.do_log(log_entry: String);
begin
  if log_target <> nil then
    log_target.Add(log_entry);
end;

procedure TRandomGen.randomgen_loop(max_steps: integer);
var
  num_steps: integer;
begin
  num_steps := 0;
  while (num_active_connection_points > 0) or (not hist_distinct_areas[num_hist_distinct_areas-1].areas_filled) do
  begin
    // Maximum number of steps reached, the algorithm probably got in wrong way
    if (num_steps = max_steps) then
    begin
      // Undo everything except the first block within distinct area
      exit;
      undo_last_distinct_area(true);
      // Try again
      num_steps := 0;
      beep;
    end;
    randomgen_step(current_step mod num_active_connection_points);
    //randomgen_step(0);
    inc(num_steps);
    // Random generation ended successfully by consuming all active connection points
    if (num_active_connection_points = 0) then
    begin
      if not fill_enclosed_areas then
      begin
        // Fill enclosed areas failed, undo everything except the first block within distinct area
        undo_last_distinct_area(current_step > hist_distinct_areas[num_hist_distinct_areas-1].first_placed_block+1);
        // Try again
        num_steps := 0;
        beep;
      end;
    end;
  end;
  // Random generation ended successfully by consuming all active connection points
  if (num_active_connection_points = 0) then
  begin
    MainWindow.render_map;
    MainWindow.render_minimap;
  end;
end;

procedure TRandomGen.randomgen_step(active_cp_index: integer);
var
  active_cp: ^TActiveConnectionPoint;
  res_block_index, res_pos_x, res_pos_y: integer;
  backtrack_limit_reached: boolean;
  block_placed: boolean;
  parent_step: integer;
begin
  if active_cp_index >= num_active_connection_points then
    exit;
  // Check if backtracking limit was reached for the last placed block
  backtrack_limit_reached := (current_step > 0) and (hist_placed_blocks[current_step-1].num_backtracked > max_backtrack_steps);
  block_placed := false;
  active_cp := @active_connection_points[active_cp_index];
  while (not backtrack_limit_reached) and get_random_block_for_connection_point(active_cp_index, res_block_index, res_pos_x, res_pos_y) do
  begin
    // Found a block - place it (it is already certain it can be placed)
    place_block(res_block_index, res_pos_x, res_pos_y, false);
    block_placed := true;
    break;
  end;
  if not block_placed then
  begin
    // Not found any blocks - undo the previous steps till the step where the connection point originated from (vacktracking)
    parent_step := active_cp.parent_step;
    while (current_step > parent_step) and ((current_step > hist_distinct_areas[num_hist_distinct_areas-1].first_placed_block + 1) or backtrack_limit_reached) do
      undo_last_placed_block;
    // Increase number of unsuccessful attempts for the previous block
    if (current_step > hist_distinct_areas[num_hist_distinct_areas-1].first_placed_block) then
      inc(hist_placed_blocks[current_step-1].num_backtracked);
  end;
end;

procedure TRandomGen.undo_last_placed_block;
var
  block: PBlockPreset;
  i: integer;
  x, y: integer;
  pos_x, pos_y: integer;
  map_tile: ^TMapTile;
  base_area_type: integer;
begin
  if current_step = 0 then
    exit;
  dec(current_step);
  base_area_type := hist_distinct_areas[num_hist_distinct_areas-1].base_area_type;
  // Remove block from map
  block := @Tileset.block_presets[hist_placed_blocks[current_step].block_preset_index];
  pos_x := hist_placed_blocks[current_step].pos_x;
  pos_y := hist_placed_blocks[current_step].pos_y;
  for y := 0 to block.height - 1 do
    for x := 0 to block.width - 1 do
    begin
      if (pos_x+x < 0) or (pos_x+x >= Map.width) or (pos_y+y < 0) or (pos_y+y >= Map.height) then
        continue;
      map_tile := @Map.data[pos_x+x,pos_y+y];
      map_tile.tile := Tileset.get_random_paint_tile(base_area_type, pos_x+x,pos_y+y);
      map_tile.special := 0;
    end;
  // Remove side constraints around this block
  // Down-direction
  if (pos_y + block.height-1 >= 0) and (pos_y + block.height-1 < Map.height) then
    for x := max(pos_x, 0) to min(pos_x + block.width-1, Map.width-1) do
      remove_tile_constraint(x, pos_y+block.height-1, 0);
  // Right-direction
  if (pos_x + block.width-1 >= 0) and (pos_x + block.width-1 < Map.width) then
    for y := max(pos_y, 0) to min(pos_y + block.height-1, Map.height-1) do
      remove_tile_constraint(pos_x+block.width-1, y, 1);
  // Up-direction
  if (pos_y >= 0) and (pos_y < Map.height) then
    for x := max(pos_x, 0) to min(pos_x + block.width-1, Map.width-1) do
      remove_tile_constraint(x, pos_y, 2);
  // Left-direction
  if (pos_x >= 0) and (pos_x < Map.width) then
    for y := max(pos_y, 0) to min(pos_y + block.height-1, Map.height-1) do
      remove_tile_constraint(pos_x, y, 3);
  // Remove list of used candidates for current step
  for i := num_hist_used_blocks - 1 downto 0 do
  begin
    if hist_used_blocks[i].step = (current_step + 1) then
      dec(num_hist_used_blocks)
    else
      break;
  end;
  // Return state of active connection points
  for i := num_hist_active_cp_changes - 1 downto 0 do
  begin
    if hist_active_cp_changes[i].step = (current_step) then
    begin
      case hist_active_cp_changes[i].change_type of
        // Undo replacement
        0: begin
            active_connection_points[hist_active_cp_changes[i].active_cp_index] := hist_active_cp_changes[i].active_cp;
          end;
        // Undo addition
        1: begin
            dec(num_active_connection_points);
          end;
        // Undo removal
        2: begin
            active_connection_points[num_active_connection_points] := active_connection_points[hist_active_cp_changes[i].active_cp_index];
            inc(num_active_connection_points);
            active_connection_points[hist_active_cp_changes[i].active_cp_index] := hist_active_cp_changes[i].active_cp;
          end;
      end;
      dec(num_hist_active_cp_changes);
    end else
      break;
  end;
  // If this was the first block within a distinct area, also decrease number of distinct areas
  if current_step = hist_distinct_areas[num_hist_distinct_areas-1].first_placed_block then
  begin
    dec(num_hist_distinct_areas);
    beep;
  end;
end;

procedure TRandomGen.undo_last_distinct_area(preserve_first_block: boolean);
var
  first_block: integer;
begin
  if num_hist_distinct_areas = 0 then
    exit;
  undo_fill_enclosed_areas;
  first_block := hist_distinct_areas[num_hist_distinct_areas-1].first_placed_block;
  if preserve_first_block then
  begin
    // Undo everything except the first block within distinct area
    while (current_step > first_block + 1) do
      undo_last_placed_block;
    // Clear number of backtracked steps
    hist_placed_blocks[current_step-1].num_backtracked := 0;
    // Clear list of blocks that were tried to place next to the first block
    while (num_hist_used_blocks > 0) and (hist_used_blocks[num_hist_used_blocks-1].step = current_step) do
      dec(num_hist_used_blocks);
  end else
  begin
    // Undo everything including the first block within distinct area
    while (current_step > first_block) do
      undo_last_placed_block;
  end;
end;

function TRandomGen.get_random_block_for_connection_point(active_cp_index: integer; var res_block_index, res_pos_x, res_pos_y: integer): boolean;
var
  i, j, k: integer;
  preset: PBlockPreset;
  active_cp: ^TActiveConnectionPoint;
  this_cp, that_cp: ^TConnectionPoint;
  search_for_type: integer;
  block_candidates: array[0..max_block_candidates-1] of TBlockCandidate;
  pos_x, pos_y: integer;
  rating: single;
  candidate_already_used: boolean;
  num_candidates: integer;
  random_number: integer;
  block_group_occurences: array[0..cnt_block_groups-1] of byte;
  block_group: integer;
  absolute_weight: integer;
  actual_weight: integer;
  weight_sum: integer;
begin
  // Find all candidate blocks for given connection point type
  active_cp := @active_connection_points[active_cp_index];
  this_cp := @tileset.connection_points[active_cp.connection_point_index];
  search_for_type := this_cp.type_and_direction xor 4;
  num_candidates := 0;
  FillChar(block_group_occurences[0], cnt_block_groups, 0);
  // Search through all available block presets
  for i := 0 to Tileset.block_presets_used - 1 do
  begin
    preset := @Tileset.block_presets[i];
    // Search through all connection points of the block
    for j := 0 to preset.num_connection_points - 1 do
    begin
      that_cp := @Tileset.connection_points[j + preset.connection_point_index];
      if that_cp.type_and_direction = search_for_type then
      begin
        // Found candidate block which has connection point we are looking for
        // Check if this candidate was already selected during current step
        candidate_already_used := false;
        for k := num_hist_used_blocks - 1 downto 0 do
        begin
          if hist_used_blocks[k].step <> current_step then
            break;
          if hist_used_blocks[k].block_group = preset.block_group then
          begin
            candidate_already_used := true;
            break;
          end;
        end;
        if candidate_already_used then
        begin
          // Reject this candidate, if it was already selected during current step
          do_log(format('Rejected candidate %d (already used during this step)', [i]));
          break;
        end;
        // Determine target position of the candidate block
        case (this_cp.type_and_direction and 7) + 1 of
          1: begin;
              pos_x := active_cp.pos_x;
              pos_y := active_cp.pos_y - that_cp.offset;
            end;
          2: begin
              pos_x := active_cp.pos_x - that_cp.offset;
              pos_y := active_cp.pos_y - (preset.height - 1);
            end;
          3: begin
              pos_x := active_cp.pos_x - (preset.width - 1);
              pos_y := active_cp.pos_y - that_cp.offset;
            end;
          4: begin
              pos_x := active_cp.pos_x - that_cp.offset;
              pos_y := active_cp.pos_y;
            end;
          5: begin;
              pos_x := active_cp.pos_x - (preset.width - 1);
              pos_y := active_cp.pos_y - that_cp.offset;
            end;
          6: begin
              pos_x := active_cp.pos_x - that_cp.offset;
              pos_y := active_cp.pos_y;
            end;
          7: begin
              pos_x := active_cp.pos_x;
              pos_y := active_cp.pos_y - that_cp.offset;
            end;
          8: begin
              pos_x := active_cp.pos_x - that_cp.offset;
              pos_y := active_cp.pos_y - (preset.height - 1);
            end;
          else begin
            pos_x := 0;
            pos_y := 0;
          end;
        end;
        // Try to place the target block and get its rating
        rating := place_block(i, pos_x, pos_y, true);
        if rating = 0.0 then
        begin
          // Block cannot be placed - reject candidate
          do_log(format('Rejected candidate %d (block cannot be placed)', [i]));
          break;
        end;
        // Add this block and its position into list of candidates
        block_candidates[num_candidates].block_index := i;
        block_candidates[num_candidates].pos_x := pos_x;
        block_candidates[num_candidates].pos_y := pos_y;
        block_candidates[num_candidates].rating := rating;
        // Check if this candidate is same as previous block
        if (hist_placed_blocks[active_cp.parent_step].block_preset_index = i) then
          block_candidates[num_candidates].repeated := true
        else
        begin
          block_candidates[num_candidates].repeated := false;
          inc(block_group_occurences[preset.block_group]);
        end;
        inc(num_candidates);
        break;
      end;
    end;
    // Limit reached, don't search further
    if num_candidates = max_block_candidates then
      break;
  end;
  // Assign weights to candidate blocks
  // Divide actual weight by number of occurences of blocks under same weight group
  weight_sum := 0;
  for i := 0 to num_candidates - 1 do
  begin
    block_group := Tileset.block_presets[block_candidates[i].block_index].block_group;
    absolute_weight := Tileset.block_groups[block_group].absolute_weight;
    if not block_candidates[i].repeated then
      actual_weight := ceil((absolute_weight / block_group_occurences[block_group]) * block_candidates[i].rating)
    else
      // Penalize block which is same as previous block
      actual_weight := 1;
    do_log(format('Found candidate %d (wg %d wgo %d absw %d rat %f actw %d)', [block_candidates[i].block_index, block_group, block_group_occurences[block_group], absolute_weight, block_candidates[i].rating, actual_weight]));
    block_candidates[i].cummulative_weight := weight_sum;
    inc(weight_sum, actual_weight);
  end;
  // Generate a random number and select a block from candidates
  random_number := random(weight_sum); // HERE is the core of random generation!!!
  for i := num_candidates - 1 downto 0 do
  begin
    if random_number >= block_candidates[i].cummulative_weight then
    begin
      // Candidate selected
      res_block_index := block_candidates[i].block_index;
      res_pos_x := block_candidates[i].pos_x;
      res_pos_y := block_candidates[i].pos_y;
      do_log(format('Selected candidate %d (num %d of %d)', [res_block_index, random_number, weight_sum]));
      // Mark this candidate as used, so that it won't be candidate in next try
      if (num_hist_used_blocks < max_hist_used_block_entries) then
      begin
        hist_used_blocks[num_hist_used_blocks].step := current_step;
        hist_used_blocks[num_hist_used_blocks].block_group := Tileset.block_presets[res_block_index].block_group;
        inc(num_hist_used_blocks);
      end;
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TRandomGen.place_block(block_preset_index, pos_x, pos_y: integer; trial_only: boolean): single;
var
  block: PBlockPreset;
  map_tile: ^TMapTile;
  i, j, x, y: integer;
  base_area_type: integer;
  direction: integer;
  active_cp: ^TActiveConnectionPoint;
  this_cp, that_cp: ^TConnectionPoint;
  this_cp_type, this_cp_x, this_cp_y, that_cp_type, that_cp_x, that_cp_y: integer;
  this_cp_direction: integer;
  constraint_type, cp_width, cp_height: byte;
  found_matching: boolean;
  active_cp_removed: array[0..3] of integer;
  active_cp_removed_cnt: integer;
  active_cp_added: array[0..3] of TActiveConnectionPoint;
  active_cp_added_cnt: integer;
  is_active_cp_removed: boolean;
  constraint_violated: boolean;
  side_constraints: cardinal; // 0xdrulDRUL, 1= connect_point, 2= outer, 4= inner
  side_constraint_type: array[0..3] of integer;
  side_cpcons_radius: array[0..3] of integer;
  side_cpcons_mincoord: array[0..3] of integer;
  side_cpcons_maxcoord: array[0..3] of integer;
  side_cpcons_orientation: array[0..3] of integer;
  rating: single;
  tile_record: ^TTileRecord;
  check_direction: integer;
  nearest_active_cp_index, distance, nearest_distance, old_distance, new_distance, distance_diff: integer;
begin
  // Determine base area type
  if num_active_connection_points = 0 then
  begin
    // This is new block within a distinct area - determine it from the area type on tile block is being placed
    base_area_type := 0;
    if (pos_x >= 0) and (pos_x < Map.width) and (pos_y >= 0) and (pos_y < Map.height) and (Tileset.tile_paint_group[Map.data[pos_x, pos_y].tile] <> -128) then
      base_area_type := Tileset.tile_paint_group[Map.data[pos_x, pos_y].tile];
  end else
    // Base area type is same as for the current distinct area
    base_area_type := hist_distinct_areas[num_hist_distinct_areas - 1].base_area_type;

  block := @Tileset.block_presets[block_preset_index];
  // Check if block itself can be placed
  for y := 0 to block.height - 1 do
    for x := 0 to block.width - 1 do
    begin
      if not check_tile_is_free(pos_x + x, pos_y + y, base_area_type) then
      begin
        // Block cannot be placed
        result := 0.0;
        exit;
      end;
    end;
  // Handle connection points
  active_cp_removed_cnt := 0;
  active_cp_added_cnt := 0;
  side_constraints := 0;
  FillChar(side_constraint_type[0], sizeof(side_constraint_type), 0);
  for i := 0 to block.num_connection_points - 1 do
  begin
    this_cp := @tileset.connection_points[block.connection_point_index + i];
    direction := this_cp.type_and_direction and 7;
    this_cp_type := this_cp.type_and_direction;
    that_cp_type := this_cp_type xor 4; // Look for opposite direction and same type
    constraint_type := Tileset.connection_point_types[this_cp_type shr 3].paint_group;
    cp_width := Tileset.connection_point_types[this_cp_type shr 3].connection_point_width;
    cp_height := Tileset.connection_point_types[this_cp_type shr 3].connection_point_height;
    // Determine position of this connection point and counterpart connection point
    // Also define constraints on all 4 sides around the block
    case (direction + 1) of
    1: begin
        this_cp_x := pos_x + block.width;
        this_cp_y := pos_y + this_cp.offset;
        this_cp_direction := 1;
        that_cp_x := this_cp_x - 1;
        that_cp_y := this_cp_y;
        side_constraints := side_constraints or $2140;
        side_constraint_type[1] := constraint_type or side_constraint_type[1];
        side_constraint_type[2] := constraint_type or side_constraint_type[2];
        side_cpcons_radius[1] := cp_width;
        side_cpcons_mincoord[1] := this_cp_y - (cp_height-1);
        side_cpcons_maxcoord[1] := this_cp_y;
        side_cpcons_orientation[1] := 0;
      end;
    2: begin
        this_cp_x := pos_x + this_cp.offset;
        this_cp_y := pos_y - 1;
        this_cp_direction := 2;
        that_cp_x := this_cp_x;
        that_cp_y := this_cp_y + 1;
        side_constraints := side_constraints or $0214;
        side_constraint_type[2] := constraint_type or side_constraint_type[2];
        side_constraint_type[3] := constraint_type or side_constraint_type[3];
        side_cpcons_radius[2] := cp_height;
        side_cpcons_mincoord[2] := this_cp_x - (cp_width-1);
        side_cpcons_maxcoord[2] := this_cp_x;
        side_cpcons_orientation[2] := 0;
      end;
    3: begin
        this_cp_x := pos_x - 1;
        this_cp_y := pos_y + this_cp.offset;
        this_cp_direction := 3;
        that_cp_x := this_cp_x + 1;
        that_cp_y := this_cp_y;
        side_constraints := side_constraints or $4021;
        side_constraint_type[3] := constraint_type or side_constraint_type[3];
        side_constraint_type[0] := constraint_type or side_constraint_type[0];
        side_cpcons_radius[3] := cp_width;
        side_cpcons_mincoord[3] := this_cp_y;
        side_cpcons_maxcoord[3] := this_cp_y + (cp_height-1);
        side_cpcons_orientation[3] := 1;
      end;
    4: begin
        this_cp_x := pos_x + this_cp.offset;
        this_cp_y := pos_y + block.height;
        this_cp_direction := 0;
        that_cp_x := this_cp_x;
        that_cp_y := this_cp_y - 1;
        side_constraints := side_constraints or $1402;
        side_constraint_type[0] := constraint_type or side_constraint_type[0];
        side_constraint_type[1] := constraint_type or side_constraint_type[1];
        side_cpcons_radius[0] := cp_height;
        side_cpcons_mincoord[0] := this_cp_x;
        side_cpcons_maxcoord[0] := this_cp_x + (cp_width-1);
        side_cpcons_orientation[0] := 1;
      end;
    5: begin
        this_cp_x := pos_x - 1;
        this_cp_y := pos_y + this_cp.offset;
        this_cp_direction := 3;
        that_cp_x := this_cp_x + 1;
        that_cp_y := this_cp_y;
        side_constraints := side_constraints or $2041;
        side_constraint_type[2] := constraint_type or side_constraint_type[2];
        side_constraint_type[3] := constraint_type or side_constraint_type[3];
        side_cpcons_radius[3] := cp_width;
        side_cpcons_mincoord[3] := this_cp_y - (cp_height-1);
        side_cpcons_maxcoord[3] := this_cp_y;
        side_cpcons_orientation[3] := 0;
      end;
    6: begin
        this_cp_x := pos_x + this_cp.offset;
        this_cp_y := pos_y + block.height;
        this_cp_direction := 0;
        that_cp_x := this_cp_x;
        that_cp_y := this_cp_y - 1;
        side_constraints := side_constraints or $1204;
        side_constraint_type[3] := constraint_type or side_constraint_type[3];
        side_constraint_type[0] := constraint_type or side_constraint_type[0];
        side_cpcons_radius[0] := cp_height;
        side_cpcons_mincoord[0] := this_cp_x - (cp_width-1);
        side_cpcons_maxcoord[0] := this_cp_x;
        side_cpcons_orientation[0] := 0;
      end;
    7: begin
        this_cp_x := pos_x + block.width;
        this_cp_y := pos_y + this_cp.offset;
        this_cp_direction := 1;
        that_cp_x := this_cp_x - 1;
        that_cp_y := this_cp_y;
        side_constraints := side_constraints or $4120;
        side_constraint_type[0] := constraint_type or side_constraint_type[0];
        side_constraint_type[1] := constraint_type or side_constraint_type[1];
        side_cpcons_radius[1] := cp_width;
        side_cpcons_mincoord[1] := this_cp_y;
        side_cpcons_maxcoord[1] := this_cp_y + (cp_height-1);
        side_cpcons_orientation[1] := 1;
      end;
    8: begin
        this_cp_x := pos_x + this_cp.offset;
        this_cp_y := pos_y - 1;
        this_cp_direction := 2;
        that_cp_x := this_cp_x;
        that_cp_y := this_cp_y + 1;
        side_constraints := side_constraints or $0412;
        side_constraint_type[1] := constraint_type or side_constraint_type[1];
        side_constraint_type[2] := constraint_type or side_constraint_type[2];
        side_cpcons_radius[2] := cp_height;
        side_cpcons_mincoord[2] := this_cp_x;
        side_cpcons_maxcoord[2] := this_cp_x + (cp_width-1);
        side_cpcons_orientation[2] := 1;
      end;
    else begin // Suppress warnings
        this_cp_x := 0;
        this_cp_y := 0;
        that_cp_x := 0;
        that_cp_y := 0;
      end;
    end;
    // Look for matching active connection point which can be paired this connection point
    found_matching := false;
    for j := 0 to num_active_connection_points - 1 do
    begin
      active_cp := @active_connection_points[j];
      that_cp := @tileset.connection_points[active_cp.connection_point_index];
      if (active_cp.pos_x = that_cp_x) and (active_cp.pos_y = that_cp_y) and ((that_cp.type_and_direction and 7) = (that_cp_type and 7)) then
      begin
        // Found a connection point on matching position and matching direction
        if that_cp.type_and_direction = that_cp_type then
        begin
          // Types match too - matching connection point found
          // Remove that connection point from active list (pending)
          active_cp_removed[active_cp_removed_cnt] := j;
          inc(active_cp_removed_cnt);
          found_matching := true;
          break;
        end else
        begin
          // Types don't match
          // Don't allow block to be placed
          result := 0.0;
          exit;
        end;
      end;
    end;
    if not found_matching then
    begin
      // Matching connection point not found
      // Before adding to active list, check also if tile is free on connection point position
      if not check_tile_is_free(this_cp_x, this_cp_y, base_area_type) then
      begin
        // Don't allow block to be placed
        result := 0.0;
        exit;
      end;
      // Add this connection point to active list (pending)
      // Place connection point outside of map bounds only if connection point direction does not point directly outside map and
      // of block is at least partially present on map
      if ((this_cp_x >= 0) or (this_cp_direction <> 3)) and
        ((this_cp_x < Map.width) or (this_cp_direction <> 1)) and
        ((this_cp_y >= 0) or (this_cp_direction <> 2)) and
        ((this_cp_y < Map.height) or (this_cp_direction <> 0)) and
        (pos_x < Map.width) and (pos_y < Map.height) and (pos_x + block.width-1 >= 0) and (pos_y + block.height-1 >= 0) then
      begin
        active_cp := @active_cp_added[active_cp_added_cnt];
        active_cp.pos_x := this_cp_x;
        active_cp.pos_y := this_cp_y;
        active_cp.connection_point_index := block.connection_point_index + i;
        active_cp.last_block_preset_index := block_preset_index;
        active_cp.parent_step := current_step;
        inc(active_cp_added_cnt);
      end;
    end;
  end;
  // Check if limit of active connection points would be exceeded
  if (active_cp_added_cnt > active_cp_removed_cnt) and (num_active_connection_points + active_cp_added_cnt - active_cp_removed_cnt > max_active_connection_points) then
  begin
    // Limit exceeded - don't allow block to be placed
    result := 0.0;
    exit;
  end;
  // Check for any constraint violation
  constraint_violated := false;
  // Down-direction
  if (pos_y + block.height < Map.height) then
    for x := max(pos_x-1, 0) to min(pos_x + block.width, Map.width-1) do
      if (side_constraints and $5000) = $0000 then
        check_constraint_violation(x, pos_y+block.height, 0, 0, (x >= pos_x) and (x < pos_x + block.width), constraint_violated)
      else if (side_constraints and $5000) = $4000 then
        check_constraint_violation(x, pos_y+block.height, 0, side_constraint_type[0], (x >= pos_x) and (x < pos_x + block.width), constraint_violated)
      else if ((x < side_cpcons_mincoord[0]) and (side_cpcons_orientation[0] = 1)) or ((x > side_cpcons_maxcoord[0]) and (side_cpcons_orientation[0] = 0)) then
        check_constraint_violation(x, pos_y+block.height, 0, 0, (x >= pos_x) and (x < pos_x + block.width), constraint_violated)
      else if ((x < side_cpcons_mincoord[0]) and (side_cpcons_orientation[0] = 0)) or ((x > side_cpcons_maxcoord[0]) and (side_cpcons_orientation[0] = 1)) then
        check_constraint_violation(x, pos_y+block.height, 0, side_constraint_type[0], (x >= pos_x) and (x < pos_x + block.width), constraint_violated);
      //else
      //  check_constraint_violation(x, pos_y+block.height, 0, constraint_type_connectionPoint, true, constraint_violated);
  // Right-direction
  if (pos_x + block.width < Map.width) then
    for y := max(pos_y-1, 0) to min(pos_y + block.height, Map.height-1) do
      if (side_constraints and $0500) = $0000 then
        check_constraint_violation(pos_x+block.width, y, 1, 0, (y >= pos_y) and (y < pos_y + block.height), constraint_violated)
      else if (side_constraints and $0500) = $0400 then
        check_constraint_violation(pos_x+block.width, y, 1, side_constraint_type[1], (y >= pos_y) and (y < pos_y + block.height), constraint_violated)
      else if ((y < side_cpcons_mincoord[1]) and (side_cpcons_orientation[1] = 1)) or ((y > side_cpcons_maxcoord[1]) and (side_cpcons_orientation[1] = 0)) then
        check_constraint_violation(pos_x+block.width, y, 1, 0, (y >= pos_y) and (y < pos_y + block.height), constraint_violated)
      else if ((y < side_cpcons_mincoord[1]) and (side_cpcons_orientation[1] = 0)) or ((y > side_cpcons_maxcoord[1]) and (side_cpcons_orientation[1] = 1)) then
        check_constraint_violation(pos_x+block.width, y, 1, side_constraint_type[1], (y >= pos_y) and (y < pos_y + block.height), constraint_violated);
      //else
      //  check_constraint_violation(pos_x+block.width, y, 1, constraint_type_connectionPoint, true, constraint_violated);
  // Up-direction
  if (pos_y-1 >= 0) then
    for x := max(pos_x-1, 0) to min(pos_x + block.width, Map.width) do
      if (side_constraints and $0050) = $0000 then
        check_constraint_violation(x, pos_y-1, 2, 0, (x >= pos_x) and (x < pos_x + block.width), constraint_violated)
      else if (side_constraints and $0050) = $0040 then
        check_constraint_violation(x, pos_y-1, 2, side_constraint_type[2], (x >= pos_x) and (x < pos_x + block.width), constraint_violated)
      else if ((x < side_cpcons_mincoord[2]) and (side_cpcons_orientation[2] = 1)) or ((x > side_cpcons_maxcoord[2]) and (side_cpcons_orientation[2] = 0)) then
        check_constraint_violation(x, pos_y-1, 2, 0, (x >= pos_x) and (x < pos_x + block.width), constraint_violated)
      else if ((x < side_cpcons_mincoord[2]) and (side_cpcons_orientation[2] = 0)) or ((x > side_cpcons_maxcoord[2]) and (side_cpcons_orientation[2] = 1)) then
        check_constraint_violation(x, pos_y-1, 2, side_constraint_type[2], (x >= pos_x) and (x < pos_x + block.width), constraint_violated);
      //else
      //  check_constraint_violation(x, pos_y-1, 2, constraint_type_connectionPoint, true, constraint_violated);
  // Left-direction
  if (pos_x-1 >= 0) then
    for y := max(pos_y-1, 0) to min(pos_y + block.height, Map.height-1) do
      if (side_constraints and $0005) = $0000 then
        check_constraint_violation(pos_x-1, y, 3, 0, (y >= pos_y) and (y < pos_y + block.height), constraint_violated)
      else if (side_constraints and $0005) = $0004 then
        check_constraint_violation(pos_x-1, y, 3, side_constraint_type[3], (y >= pos_y) and (y < pos_y + block.height), constraint_violated)
      else if ((y < side_cpcons_mincoord[3]) and (side_cpcons_orientation[3] = 1)) or ((y > side_cpcons_maxcoord[3]) and (side_cpcons_orientation[3] = 0)) then
        check_constraint_violation(pos_x-1, y, 3, 0, (y >= pos_y) and (y < pos_y + block.height), constraint_violated)
      else if ((y < side_cpcons_mincoord[3]) and (side_cpcons_orientation[3] = 0)) or ((y > side_cpcons_maxcoord[3]) and (side_cpcons_orientation[3] = 1)) then
        check_constraint_violation(pos_x-1, y, 3, side_constraint_type[3], (y >= pos_y) and (y < pos_y + block.height), constraint_violated);
      //else
      //  check_constraint_violation(pos_x-1, y, 3, constraint_type_connectionPoint, true, constraint_violated);

  inc(constraint_check_step);
  if constraint_violated then
  begin
    result := 0.0;
    exit;
  end;
  // Check if block would interfere with any active connection point which is not removed
  for i := 0 to num_active_connection_points - 1 do
  begin
    active_cp := @active_connection_points[i];
    if (active_cp.pos_x >= pos_x) and (active_cp.pos_x < (pos_x + block.width)) and (active_cp.pos_y >= pos_y) and (active_cp.pos_y < (pos_y + block.height)) then
    begin
      // Active CP interferes block dimensions, check if this active CP is being removed
      is_active_cp_removed := false;
      for j := 0 to active_cp_removed_cnt -1 do
      begin
        if active_cp_removed[j] = i then
        begin
          is_active_cp_removed := true;
          break;
        end;
      end;
      if not is_active_cp_removed then
      begin
        // Don't allow block to be placed
        result := 0.0;
        exit;
      end;
    end;
  end;

  // *** RATING COMPUTATION ***
  rating := 1.0;
  for i := 0 to active_cp_added_cnt -1 do
  begin
    //active_cp := @active_cp_added[i];
    {x := active_cp.pos_x;
    y := active_cp.pos_y;
    direction := (Tileset.connection_points[active_cp.connection_point_index].type_and_direction and 3);
    check_direction := direction xor 2;
    for j := 0 to 7 do
    begin
      case direction of
        0: inc(y);
        1: inc(x);
        2: dec(y);
        3: dec(x);
      end;
      tile_record := @tile_records[x, y];
      tile_record.constraint_checks[check_direction].step := constraint_check_step + 1;
      tile_record.constraint_checks[check_direction].c_type := 4;
      tile_record.constraint_checks[check_direction].violated := false;
      tile_record.constraint_checks[check_direction].distance := j+1;
      if tile_record.constraints[check_direction].radius > 0 then
      begin
        tile_record.constraint_checks[check_direction].violated := true;
        rating := rating * distance_rating[j] * distance_rating[j];
        break;
      end;
    end;

    x := active_cp.pos_x;
    y := active_cp.pos_y;
    direction := (Tileset.connection_points[active_cp.connection_point_index].type_and_direction and 3);
    check_direction := direction;
    for j := 0 to 7 do
    begin
      case direction of
        0: dec(y);
        1: dec(x);
        2: inc(y);
        3: inc(x);
      end;
      tile_record := @tile_records[x, y];
      tile_record.constraint_checks[check_direction].step := constraint_check_step + 1;
      tile_record.constraint_checks[check_direction].c_type := 5;
      tile_record.constraint_checks[check_direction].violated := false;
      tile_record.constraint_checks[check_direction].distance := j+1;
      if tile_record.constraints[check_direction].radius > 0 then
      begin
        tile_record.constraint_checks[check_direction].violated := true;
        rating := rating * distance_rating[j] * distance_rating[j];
        break;
      end;
    end;}

    {x := active_cp.pos_x;
    y := active_cp.pos_y;
    direction := (Tileset.connection_points[active_cp.connection_point_index].type_and_direction and 7);
    check_direction := 0;
    case direction + 1 of
      1: check_direction := 3;
      2: check_direction := 0;
      3: check_direction := 1;
      4: check_direction := 2;
      5: check_direction := 1;
      6: check_direction := 2;
      7: check_direction := 3;
      8: check_direction := 0;
    end;
    for j := 0 to 7 do
    begin
      case direction of
        0: inc(x);
        1: dec(y);
        2: dec(x);
        3: inc(y);
        4: dec(x);
        5: inc(y);
        6: inc(x);
        7: dec(y);
      end;
      tile_record := @tile_records[x, y];
      tile_record.constraint_checks[check_direction].step := constraint_check_step + 1;
      tile_record.constraint_checks[check_direction].c_type := 8;
      tile_record.constraint_checks[check_direction].violated := false;
      tile_record.constraint_checks[check_direction].distance := j+1;
      if tile_record.constraints[check_direction].radius > 0 then
      begin
        tile_record.constraint_checks[check_direction].violated := true;
        rating := rating * distance_rating[j] * distance_rating[j];
        break;
      end;
    end;}

  end;
  inc(constraint_check_step);

  // If more than one active connection point is removed, then give the biggest rating
  if (active_cp_removed_cnt > 1) then
  begin
    rating := rating * 100.0;
    //beep;
  end;

  // Rating according to how distance from the nearest active connection point was changed
  if (active_cp_removed_cnt = 1) and (active_cp_added_cnt > 0) and (num_active_connection_points > 1) then
  begin
    active_cp := @active_connection_points[active_cp_removed[0]];
    // Find the nearest active connection point to the connection point we removed
    nearest_active_cp_index := -1;
    nearest_distance := 256;
    for i := 0 to num_active_connection_points - 1 do
    begin
      // Skip the active connection point we are removing
      if active_cp_removed[0] = i then
        continue;
      distance := abs(active_cp.pos_x - active_connection_points[i].pos_x) + abs(active_cp.pos_y - active_connection_points[i].pos_y);
      if distance < nearest_distance then
      begin
        nearest_distance := distance;
        nearest_active_cp_index := i;
      end;
    end;
    do_log(Format('Nearest connection point %d distance %d', [nearest_active_cp_index, nearest_distance]));
    old_distance := nearest_distance;
    // Compare how the distance was changed for the newly added active connection point
    active_cp := @active_cp_added[0];
    new_distance := abs(active_cp.pos_x - active_connection_points[nearest_active_cp_index].pos_x) + abs(active_cp.pos_y - active_connection_points[nearest_active_cp_index].pos_y);
    distance_diff := new_distance - old_distance;
    if (distance_diff < 0) then
    begin
      // We're closer to the nearest active connection point
      do_log(Format('We''re closer by %d tiles', [distance_diff * -1]));
      rating := rating * 10.0;
    end;
    if (distance_diff > 0) then
    begin
      // We're farther to the nearest active connection point
      do_log(Format('We''re farther by %d tiles', [distance_diff]));
      //rating := rating * 0.1;
    end;
  end;


  // *** ACTUAL CHANGE ***
  result := rating;
  if trial_only then
    exit;
  // If this is the first block within a distinct area (no active connection points existed before placing this block),
  // Add new entry into history of distinct areas
  if num_active_connection_points = 0 then
  begin
    hist_distinct_areas[num_hist_distinct_areas].first_placed_block := current_step;
    hist_distinct_areas[num_hist_distinct_areas].first_filled_area := num_hist_filled_areas;
    hist_distinct_areas[num_hist_distinct_areas].base_area_type := base_area_type;
    hist_distinct_areas[num_hist_distinct_areas].areas_filled := false;
    inc(num_hist_distinct_areas);
  end;

  // Now perform the pending changes in active connection points
  for i := 0 to 3 do
  begin
    if (active_cp_added_cnt > i) and (active_cp_removed_cnt > i) then
    begin
      // Replace removed active connection point with added one
      // Record this change into history first
      hist_active_cp_changes[num_hist_active_cp_changes].active_cp_index := active_cp_removed[i];
      hist_active_cp_changes[num_hist_active_cp_changes].change_type := 0;
      hist_active_cp_changes[num_hist_active_cp_changes].step := current_step;
      hist_active_cp_changes[num_hist_active_cp_changes].active_cp := active_connection_points[active_cp_removed[i]];
      inc(num_hist_active_cp_changes);
      // Do the actual change
      active_connection_points[active_cp_removed[i]] := active_cp_added[i];
    end
    else if (active_cp_added_cnt > i) then
    begin
      // More active CPs were added than removed - add one
      // Record this change into history first
      hist_active_cp_changes[num_hist_active_cp_changes].change_type := 1;
      hist_active_cp_changes[num_hist_active_cp_changes].step := current_step;
      inc(num_hist_active_cp_changes);
      // Do the actual change
      active_connection_points[num_active_connection_points] := active_cp_added[i];
      inc(num_active_connection_points);
    end
    else if (active_cp_removed_cnt > i) then
    begin
      // More active CPs were removed than added - remove one
      // Record this change into history first
      hist_active_cp_changes[num_hist_active_cp_changes].active_cp_index := active_cp_removed[i];
      hist_active_cp_changes[num_hist_active_cp_changes].change_type := 2;
      hist_active_cp_changes[num_hist_active_cp_changes].step := current_step;
      hist_active_cp_changes[num_hist_active_cp_changes].active_cp := active_connection_points[active_cp_removed[i]];
      inc(num_hist_active_cp_changes);
      // Do the actual change
      active_connection_points[active_cp_removed[i]] := active_connection_points[num_active_connection_points - 1];
      for j := 0 to active_cp_removed_cnt - 1 do
        if active_cp_removed[j] = num_active_connection_points - 1 then
          active_cp_removed[j] := active_cp_removed[i];
      dec(num_active_connection_points);
    end;
  end;
  // Place block itself
  for y := 0 to block.height - 1 do
    for x := 0 to block.width - 1 do
    begin
      if (pos_x+x < 0) or (pos_x+x >= Map.width) or (pos_y+y < 0) or (pos_y+y >= Map.height) then
        continue;
      map_tile := @Map.data[pos_x+x,pos_y+y];
      map_tile.tile := Tileset.block_preset_tiles[block.block_preset_tile_index + x + y * block.width];
      map_tile.special := 0;
    end;
  // Add this placed block into history
  hist_placed_blocks[current_step].pos_x := pos_x;
  hist_placed_blocks[current_step].pos_y := pos_y;
  hist_placed_blocks[current_step].block_preset_index := block_preset_index;
  hist_placed_blocks[current_step].num_backtracked := 0;
  inc(current_step);

  // Mark constraints around the block
  // Down-direction
  if (pos_y + block.height-1 >= 0) and (pos_y + block.height-1 < Map.height) then
    for x := max(pos_x, 0) to min(pos_x + block.width-1, Map.width-1) do
      if (side_constraints and $5000) = $0000 then
        add_tile_constraint(x, pos_y+block.height-1, 0, 0, 1)
      else if (side_constraints and $5000) = $4000 then
        add_tile_constraint(x, pos_y+block.height-1, 0, side_constraint_type[0], 1)
      else if ((x < side_cpcons_mincoord[0]) and (side_cpcons_orientation[0] = 1)) or ((x > side_cpcons_maxcoord[0]) and (side_cpcons_orientation[0] = 0)) then
        add_tile_constraint(x, pos_y+block.height-1, 0, 0, 1)
      else if ((x < side_cpcons_mincoord[0]) and (side_cpcons_orientation[0] = 0)) or ((x > side_cpcons_maxcoord[0]) and (side_cpcons_orientation[0] = 1)) then
        add_tile_constraint(x, pos_y+block.height-1, 0, side_constraint_type[0], 1)
      else
        add_tile_constraint(x, pos_y+block.height-1, 0, constraint_type_connectionPoint, side_cpcons_radius[0]);
  // Right-direction
  if (pos_x + block.width-1 >= 0) and (pos_x + block.width-1 < Map.width) then
    for y := max(pos_y, 0) to min(pos_y + block.height-1, Map.height-1) do
      if (side_constraints and $0500) = $0000 then
        add_tile_constraint(pos_x+block.width-1, y, 1, 0, 1)
      else if (side_constraints and $0500) = $0400 then
        add_tile_constraint(pos_x+block.width-1, y, 1, side_constraint_type[1], 1)
      else if ((y < side_cpcons_mincoord[1]) and (side_cpcons_orientation[1] = 1)) or ((y > side_cpcons_maxcoord[1]) and (side_cpcons_orientation[1] = 0)) then
        add_tile_constraint(pos_x+block.width-1, y, 1, 0, 1)
      else if ((y < side_cpcons_mincoord[1]) and (side_cpcons_orientation[1] = 0)) or ((y > side_cpcons_maxcoord[1]) and (side_cpcons_orientation[1] = 1)) then
        add_tile_constraint(pos_x+block.width-1, y, 1, side_constraint_type[1], 1)
      else
        add_tile_constraint(pos_x+block.width-1, y, 1, constraint_type_connectionPoint, side_cpcons_radius[1]);
  // Up-direction
  if (pos_y >= 0) and (pos_y < Map.height) then
    for x := max(pos_x, 0) to min(pos_x + block.width-1, Map.width-1) do
      if (side_constraints and $0050) = $0000 then
        add_tile_constraint(x, pos_y, 2, 0, 1)
      else if (side_constraints and $0050) = $0040 then
        add_tile_constraint(x, pos_y, 2, side_constraint_type[2], 1)
      else if ((x < side_cpcons_mincoord[2]) and (side_cpcons_orientation[2] = 1)) or ((x > side_cpcons_maxcoord[2]) and (side_cpcons_orientation[2] = 0)) then
        add_tile_constraint(x, pos_y, 2, 0, 1)
      else if ((x < side_cpcons_mincoord[2]) and (side_cpcons_orientation[2] = 0)) or ((x > side_cpcons_maxcoord[2]) and (side_cpcons_orientation[2] = 1)) then
        add_tile_constraint(x, pos_y, 2, side_constraint_type[2], 1)
      else
        add_tile_constraint(x, pos_y, 2, constraint_type_connectionPoint, side_cpcons_radius[2]);
  // Left-direction
  if (pos_x >= 0) and (pos_x < Map.width) then
    for y := max(pos_y, 0) to min(pos_y + block.height-1, Map.height-1) do
      if (side_constraints and $0005) = $0000 then
        add_tile_constraint(pos_x, y, 3, 0, 1)
      else if (side_constraints and $0005) = $0004 then
        add_tile_constraint(pos_x, y, 3, side_constraint_type[3], 1)
      else if ((y < side_cpcons_mincoord[3]) and (side_cpcons_orientation[3] = 1)) or ((y > side_cpcons_maxcoord[3]) and (side_cpcons_orientation[3] = 0)) then
        add_tile_constraint(pos_x, y, 3, 0, 1)
      else if ((y < side_cpcons_mincoord[3]) and (side_cpcons_orientation[3] = 0)) or ((y > side_cpcons_maxcoord[3]) and (side_cpcons_orientation[3] = 1)) then
        add_tile_constraint(pos_x, y, 3, side_constraint_type[3], 1)
      else
        add_tile_constraint(pos_x, y, 3, constraint_type_connectionPoint, side_cpcons_radius[3]);
end;

function TRandomGen.check_tile_is_free(pos_x, pos_y, base_area_type: integer): boolean;
var
  tile_index: word;
begin
  if (pos_x < 0) or (pos_x >= Map.width) or (pos_y < 0) or (pos_y >= Map.height) then
  begin
    // No restrictions outside of map
    result := true;
    exit;
  end;
  // Check if tile has base area type
  tile_index := Map.data[pos_x, pos_y].tile;
  result := Tileset.tile_paint_group[tile_index] = base_area_type;
end;

procedure TRandomGen.check_constraint_violation(pos_x, pos_y, direction, check_type: integer; strict: boolean; var constraint_violated: boolean);
var
  check_direction: integer;
  c_type, c_radius: integer;
  distance, dist_diff: integer;
  tile_record: ^TTileRecord;
  violation: boolean;
begin
  check_direction := direction xor 2;
  distance := 1;
  while (pos_x >= 0) and (pos_x < Map.width) and (pos_y >= 0) and (pos_y < Map.height) and (distance <= 2) do
  begin
    tile_record := @tile_records[pos_x, pos_y];
    // Check for violation
    c_type := tile_record.constraints[check_direction].c_type;
    c_radius := tile_record.constraints[check_direction].radius;
    dist_diff := c_radius - distance;
    violation := (c_radius <> 0) and (dist_diff >= 0) and ((check_type <> c_type) or (strict and (dist_diff > 0))) and not ((not strict) and (c_type = constraint_type_connectionPoint));
    constraint_violated := constraint_violated or violation;
    // Record this constraint check for rendering
    //if (distance = 1) or violation then
    begin
      tile_record.constraint_checks[check_direction].step := constraint_check_step + 1;
      if strict then
        tile_record.constraint_checks[check_direction].c_type := check_type
      else
        tile_record.constraint_checks[check_direction].c_type := check_type + 4;
      tile_record.constraint_checks[check_direction].violated := violation;
      tile_record.constraint_checks[check_direction].distance := distance;
    end;
    // Move by one step
    if not strict then
      break;
    if c_radius <> 0 then
      break; // Already hit a constraint, do not move further
    inc(distance);
    case direction of
      0: inc(pos_y);
      1: inc(pos_x);
      2: dec(pos_y);
      3: dec(pos_x);
    end;
  end;
end;

procedure TRandomGen.add_tile_constraint(pos_x, pos_y, direction, c_type, radius: integer);
begin
  tile_records[pos_x, pos_y].constraints[direction].c_type := c_type;
  tile_records[pos_x, pos_y].constraints[direction].radius := radius;
end;

procedure TRandomGen.remove_tile_constraint(pos_x, pos_y, direction: integer);
begin
  tile_records[pos_x, pos_y].constraints[direction].radius := 0;
end;

function TRandomGen.fill_enclosed_areas: boolean;
var
  x, y, direction: integer;
  pos_x, pos_y: integer;
  tile_record: ^TTileRecord;
  target_area_type, base_area_type: integer;
  success: boolean;
begin
  for y := 0 to Map.height - 1 do
    for x := 0 to Map.width - 1 do
    begin
      tile_record := @tile_records[x, y];
      // Check constraints on each side of tile
      for direction := 0 to 3 do
      begin
        if tile_record.constraints[direction].radius <= 0 then
          continue; // No constraint
        target_area_type := tile_record.constraints[direction].c_type;
        base_area_type := hist_distinct_areas[num_hist_distinct_areas-1].base_area_type;
        if (target_area_type = base_area_type) or (target_area_type > 3) then
          continue; // Target area type we are not interested in
        // Determine position of adjacent tile we are going to use as flood fill seed
        pos_x := x;
        pos_y := y;
        case direction of
          0: inc(pos_y);
          1: inc(pos_x);
          2: dec(pos_y);
          3: dec(pos_x);
        end;
        if (pos_x >= 0) and (pos_x < Map.width) and (pos_y >= 0) and (pos_y < Map.height) and (Tileset.tile_paint_group[Map.data[pos_x, pos_y].tile] = base_area_type) then
        begin
          success := fill_enclosed_area_step(pos_x, pos_y, direction, base_area_type, target_area_type);
          if success then
          begin
            // Add this filled area to history
            hist_filled_areas[num_hist_filled_areas].pos_x := pos_x;
            hist_filled_areas[num_hist_filled_areas].pos_y := pos_y;
            hist_filled_areas[num_hist_filled_areas].area_type := target_area_type;
            inc(num_hist_filled_areas);
          end else
          begin
            // Undo what was done before area type conflict was found
            undo_fill_enclosed_area_step(pos_x, pos_y, base_area_type, target_area_type);
            result := false;
            exit;
          end;
        end;
      end;
    end;
  hist_distinct_areas[num_hist_distinct_areas-1].areas_filled := true;
  result := true;
end;

function TRandomGen.fill_enclosed_area_step(pos_x, pos_y, direction, base_area_type, target_area_type: integer): boolean;
var
  map_tile: ^TMapTile;
  side_constraint: ^TConstraint;
begin
  result := true;
  if (pos_x < 0) or (pos_x >= Map.width) or (pos_y < 0) or (pos_y >= Map.height) then
    exit; // Outside map
  map_tile := @Map.data[pos_x, pos_y];
  if Tileset.tile_paint_group[map_tile.tile] <> base_area_type then
  begin
    // Tile is not of base area type - check for constraint
    side_constraint := @tile_records[pos_x, pos_y].constraints[direction xor 2];
    if side_constraint.radius <> 0 then
      // Check for area type conflict
      result := side_constraint.c_type = target_area_type;
    exit;
  end;
  // Do actual change
  map_tile.tile := Tileset.get_random_paint_tile(target_area_type, pos_x, pos_y);
  map_tile.special := 0;
  // Step to adjacent tile
  result := fill_enclosed_area_step(pos_x, pos_y+1, 0, base_area_type, target_area_type);
  if result = false then
    exit;
  result := fill_enclosed_area_step(pos_x+1, pos_y, 1, base_area_type, target_area_type);
  if result = false then
    exit;
  result := fill_enclosed_area_step(pos_x, pos_y-1, 2, base_area_type, target_area_type);
  if result = false then
    exit;
  result := fill_enclosed_area_step(pos_x-1, pos_y, 3, base_area_type, target_area_type);
end;

procedure TRandomGen.undo_fill_enclosed_areas;
begin
  if (num_hist_distinct_areas = 0) or (num_hist_filled_areas = 0) then
    exit; // Nothing to undo
  while num_hist_filled_areas > hist_distinct_areas[num_hist_distinct_areas-1].first_filled_area do
  begin
    dec(num_hist_filled_areas);
    undo_fill_enclosed_area_step(hist_filled_areas[num_hist_filled_areas].pos_x, hist_filled_areas[num_hist_filled_areas].pos_y, hist_distinct_areas[num_hist_distinct_areas-1].base_area_type, hist_filled_areas[num_hist_filled_areas].area_type);
  end;
  hist_distinct_areas[num_hist_distinct_areas-1].areas_filled := false;
end;

procedure TRandomGen.undo_fill_enclosed_area_step(pos_x, pos_y, base_area_type, target_area_type: integer);
var
  map_tile: ^TMapTile;
begin
  if (pos_x < 0) or (pos_x >= Map.width) or (pos_y < 0) or (pos_y >= Map.height) then
    exit;
  map_tile := @Map.data[pos_x, pos_y];
  if Tileset.tile_paint_group[map_tile.tile] <> target_area_type then
    exit;
  // Do actual change
  map_tile.tile := Tileset.get_random_paint_tile(base_area_type, pos_x, pos_y);
  map_tile.special := 0;
  // Step to adjacent tile
  undo_fill_enclosed_area_step(pos_x, pos_y+1, base_area_type, target_area_type);
  undo_fill_enclosed_area_step(pos_x+1, pos_y, base_area_type, target_area_type);
  undo_fill_enclosed_area_step(pos_x, pos_y-1, base_area_type, target_area_type);
  undo_fill_enclosed_area_step(pos_x-1, pos_y, base_area_type, target_area_type);
end;

end.
