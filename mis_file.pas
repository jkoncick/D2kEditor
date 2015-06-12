unit mis_file;

interface

// Mis file constants
const cnt_mis_players = 8;

const player_names: array[0..7] of string =
  ('Atreides', 'Harkonnen', 'Ordos', 'Emperor', 'Fremen', 'Smugglers', 'Mercenaries', 'Sandworm');

const flag_value: array[0..1] of string = ('False', 'True');
const deploy_action: array[0..2] of string = ('None', 'Hunt', 'Free');
const allegiance_type: array[0..2] of string = ('Ally', 'Enemy', 'Neutral');
const comparison_function: array[0..3] of string = ('>', '<', '=', '%');

const unit_names: array[0..29] of string = (
  'Light infantry',
  'Trooper',
  'Engineer',
  'Thumper infantry',
  'Sardaukar',
  'Trike',
  'Raider',
  'Quad',
  'Harvester',
  'Combat tank (A)',
  'Combat tank (H)',
  'Combat tank (O)',
  'MCV',
  'Missile tank',
  'Deviator',
  'Siege tank',
  'Sonic tank',
  'Devastator',
  'Carryall',
  'Carryall (A)',
  'Ornithropter',
  'Stealth Fremen',
  'Fremen',
  'Saboteur',
  'Death Hand Missile',
  'Sandworm',
  'Frigate',
  'Grenadier',
  'Stealth Raider',
  'MP Sardaukar'
  );

const building_names: array[0..61] of string = (
  'Construction Yard (A)',
  'Construction Yard (H)',
  'Construction Yard (O)',
  'Construction Yard (E)',
  'Concrete (A)',
  'Concrete (H)',
  'Concrete (O)',
  'Large Concrete (A)',
  'Large Concrete (H)',
  'Large Concrete (O)',
  'Wind Trap (A)',
  'Wind Trap (H)',
  'Wind Trap (O)',
  'Barracks (A)',
  'Barracks (H)',
  'Barracks (O)',
  'Sietch',
  'Wall (A)',
  'Wall (H)',
  'Wall (O)',
  'Refinery (A)',
  'Refinery (H)',
  'Refinery (O)',
  'Gun Turret (A)',
  'Gun Turret (H)',
  'Gun Turret (O)',
  'Outpost (A)',
  'Outpost (H)',
  'Outpost (O)',
  'Rocket Turret (A)',
  'Rocket Turret (H)',
  'Rocket Turret (O)',
  'High Tech Factory (A)',
  'High Tech Factory (H)',
  'High Tech Factory (O)',
  'Light Factory (A)',
  'Light Factory (H)',
  'Light Factory (O)',
  'Silo (A)',
  'Silo (H)',
  'Silo (O)',
  'Heavy Factory (A)',
  'Heavy Factory (H)',
  'Heavy Factory (O)',
  'Heavy Factory (M)',
  'Heavy Factory (E)',
  'Starport (A)',
  'Starport (H)',
  'Starport (O)',
  'Starport (S)',
  'Repair Pad (A)',
  'Repair Pad (H)',
  'Repair Pad (O)',
  'IX Research Centre (A)',
  'IX Research Centre (H)',
  'IX Research Centre (O)',
  'Palace (A)',
  'Palace (H)',
  'Palace (O)',
  'Palace (E)',
  'Modified Outpost (H)',
  'Modified Outpost (O)'
  );

// Mis file type definitions
type
  EventType = (etReinforcement, etStarportDelivery, etAllegiance, etLeave,
    etBerserk, etPlaySound, etSetBuildRate, etSetAttackBuildingRate, etSetCash,
    etSetTech, etMissionWin, etMissionFail, etBloxFile, etAttribFile,
    etRevealMap, etShowTimer, etHideTimer, etShowMessage, etUnitSpawn, etSetFlag);

type
  ConditionType = (ctBuildingExists, ctUnitExists, ctInterval, ctTimer,
    ctCasualties, ctBaseDestroyed, ctUnitsDestroyed, ctTileRevealed,
    ctSpiceHarvested, ctFlag);

type
   EventMarkerType = (emNone, emReinforcement, emHarvester, emUnitSpawn, emTileRevealed, emRevealMap);

type
  TEvent = packed record
    map_pos_x: cardinal;
    map_pos_y: cardinal;
    value: cardinal;      // Flag value, Message unknown value
    num_conditions: byte;
    event_type: byte;
    num_units: byte;      // Also Reveal Map radius
    player: byte;         // Also Set Flag flag number
    allegiance_target: byte;
    allegiance_type: byte;
    deploy_action: byte;
    condition_index: array[0..13] of byte;
    condition_not: array[0..13] of byte;
    units: array[0..20] of byte;
    message_index: cardinal;
  end;

type
  TCondition = packed record
    time_amount: cardinal;
    start_delay: cardinal;
    value: cardinal; // Interval run count, Casualty threshold, Base/Units destroyed, tile revealed unknown, Cash amount
    map_pos_x: cardinal;
    map_pos_y: cardinal;
    casualty_flags: cardinal;
    player: byte;
    condition_type: byte;
    building_type: byte;
    unit_type_or_comparison_function: byte;
  end;

type
  TMisFile = packed record
    tech_level:       array[0..7] of byte;
    starting_money:   array[0..7] of cardinal;
    unknown1:         array[0..39] of byte;
    allocation_index: array[0..7] of byte;
    ai_segments:      array[0..7, 0..7607] of byte;
    allegiance:       array[0..7, 0..7] of byte;
    events:           array[0..63] of TEvent;
    conditions:       array[0..47] of TCondition;
    tileset:          array[0..199] of char;
    tileatr:          array[0..199] of char;
    num_events:       byte;
    num_conditions:   byte;
    time_limit:       cardinal;
    unknown2:         array[0..691] of byte;
  end;

type
  TEventMarker = record
    emtype: EventMarkerType;
    side: word;
    index: word;
    moved: boolean;
  end;

type
  TEventTypeInfo = record
    name: String;
    use_map_position: boolean;
    use_player_index: boolean;
    use_unit_list: boolean;
    value_name: string;
  end;

type
  TConditionTypeInfo = record
    name: String;
    use_player_index: boolean;
    value_name: string;
  end;

type
  TEventMarkerTypeInfo = record
    letter: char;
    player_related: boolean;
  end;

// Mis file type definition constants
const event_type_info: array[0..19] of TEventTypeInfo =
  (
    (name: 'Reinforcement';             use_map_position: true;  use_player_index: true;  use_unit_list: true;  value_name: '';),
    (name: 'Starport Delivery';         use_map_position: false; use_player_index: true;  use_unit_list: true;  value_name: '';),
    (name: 'Allegiance';                use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: '(unsupported)';             use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Berserk';                   use_map_position: false; use_player_index: true;  use_unit_list: false; value_name: '';),
    (name: '(unsupported)';             use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Set Build Rate';            use_map_position: false; use_player_index: true;  use_unit_list: false; value_name: 'Unknown';),
    (name: 'Set Attack Building Rate';  use_map_position: false; use_player_index: true;  use_unit_list: false; value_name: 'Unknown';),
    (name: 'Set Cash';                  use_map_position: false; use_player_index: true ; use_unit_list: false; value_name: 'Cash';),
    (name: 'Set Tech';                  use_map_position: false; use_player_index: true ; use_unit_list: false; value_name: 'Tech level';),
    (name: 'Mission Win';               use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Mission Fail';              use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: '(unsupported)';             use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: '(unsupported)';             use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Reveal Map';                use_map_position: true;  use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Show Timer';                use_map_position: false; use_player_index: false; use_unit_list: false; value_name: 'Time';),
    (name: 'Hide Timer';                use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Show Message';              use_map_position: false; use_player_index: false; use_unit_list: false; value_name: 'Unknown';),
    (name: 'Unit Spawn';                use_map_position: true;  use_player_index: true;  use_unit_list: true;  value_name: '';),
    (name: 'Set Flag';                  use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';)
  );

const condition_type_info: array[0..9] of TConditionTypeInfo =
  (
    (name: 'Building Exists'; use_player_index: true;  value_name: '';),
    (name: 'Unit Exists';     use_player_index: true;  value_name: '';),
    (name: 'Interval';        use_player_index: false; value_name: 'Run count';),
    (name: 'Timer';           use_player_index: false; value_name: '';),
    (name: 'Casualties';      use_player_index: true;  value_name: 'Threshold';),
    (name: 'Base Destroyed';  use_player_index: true;  value_name: 'Unknown';),
    (name: 'Units Destroyed'; use_player_index: true;  value_name: 'Unknown';),
    (name: 'Tile Revealed';   use_player_index: false; value_name: 'Unknown';),
    (name: 'Spice Harvested'; use_player_index: false; value_name: 'Credits';),
    (name: 'Flag';            use_player_index: false; value_name: '';)
  );

const event_marker_type_info: array[0..5] of TEventMarkerTypeInfo =
  (
    (letter: ' '; player_related: false),
    (letter: 'R'; player_related: true),
    (letter: 'H'; player_related: true),
    (letter: 'S'; player_related: true),
    (letter: 'T'; player_related: false),
    (letter: 'M'; player_related: false)
  );

// Mis file variables
var
  mis_filename: String;
  mis_assigned: boolean;
  mis_data: TMisFile;
  mis_map_markers: array[0..127, 0..127] of TEventMarker;

// Mis file functions and procedures
  function get_mis_filename(filename: String): String;
  procedure load_mis_file(filename: String);
  procedure save_mis_file(filename: String);
  procedure process_event_markers;
  procedure set_default_mis_values;
  function get_event_contents(index: integer): String;
  function get_event_conditions(index: integer): String;
  function get_condition_contents(index: integer; show_player: boolean): String;
  procedure delete_condition(deleted_index: integer);



implementation

uses Windows, Forms, SysUtils, main, tileset, map_defs, stringtable, event_dialog;

function get_mis_filename(filename: String): String;
var
  tmp_filename: String;
begin
  tmp_filename := ExtractFileDir(filename)+'\_'+UpperCase(ExtractFileName(filename));
  tmp_filename[length(tmp_filename)-1]:= 'I';
  tmp_filename[length(tmp_filename)]:= 'S';
  result := tmp_filename;
end;

procedure load_mis_file(filename: String);
var
  mis_file: file of TMisFile;
  tileset_name: String;
  i: integer;
begin
  mis_assigned := FileExists(filename);
  if mis_assigned then
  begin
    MainWindow.StatusBar.Panels[4].Text := 'MIS';
    mis_filename := filename;
  end else
  begin
    MainWindow.StatusBar.Panels[4].Text := '';
    mis_filename := '';
    set_default_mis_values;
    exit;
  end;
  AssignFile(mis_file, filename);
  Reset(mis_file);
  Read(mis_file, mis_data);
  CloseFile(mis_file);

  tileset_name := String(mis_data.tileset);
  for i:= 0 to cnt_tilesets-1 do
  begin
    if tileset_name = tilesets[i].name then
      tileset_change(i);
  end;
  process_event_markers;
  EventDialog.update_contents;
end;

procedure save_mis_file(filename: String);
var
  mis_file: file of TMisFile;
begin
  AssignFile(mis_file, filename);
  ReWrite(mis_file);
  Write(mis_file, mis_data);
  CloseFile(mis_file);
end;

procedure process_event_markers;
var
  event: ^TEvent;
  condition: ^TCondition;
  event_type: EventType;
  x, y: byte;
  i: integer;
  moved: boolean;
begin
  for x := 0 to 127 do
    for y := 0 to 127 do
      mis_map_markers[x,y].emtype := emNone;
  for i:= 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    event_type := EventType(event.event_type);
    if (event_type = etReinforcement) or (event_type = etUnitSpawn) or (event_type = etRevealMap) then
    begin
      // Reinforcement, spawn, Reveal map
      x := event.map_pos_x;
      y := event.map_pos_y;
      // Move event marker one tile to right if this tile has already an event
      moved := false;
      while mis_map_markers[x][y].emtype <> emNone do
      begin
        x := (x + 1) mod map_width;
        moved := true;
      end;
      if event_type = etUnitSpawn then
        mis_map_markers[x][y].emtype := emUnitSpawn
      else if event_type = etRevealMap then
         mis_map_markers[x][y].emtype := emRevealMap
      else if (event.num_units = 1) and (event.units[0] = 8) then
        mis_map_markers[x][y].emtype := emHarvester
      else
        mis_map_markers[x][y].emtype := emReinforcement;
      mis_map_markers[x][y].side := event.player;
      mis_map_markers[x][y].index := i;
      mis_map_markers[x][y].moved := moved;
    end;
  end;
  for i:= 0 to mis_data.num_conditions - 1 do
  begin
    condition := Addr(mis_data.conditions[i]);
    if condition.condition_type = byte(ctTileRevealed) then
    begin
      // Unit in tile
      x := condition.map_pos_x;
      y := condition.map_pos_y;
      mis_map_markers[x][y].emtype := emTileRevealed;
      mis_map_markers[x][y].index := i;
    end;
  end;
end;

procedure set_default_mis_values;
var
  i, j: integer;
begin
  FillChar(mis_data, sizeof(mis_data), 0);
  // Write tileset name
  Move(tilesets[tileset_index].name[1], mis_data.tileset, 8);
  Move(tilesets[tileset_index].tileatr_name[1], mis_data.tileatr, 8);
  // Player properties
  for i := 0 to 7 do
  begin
    mis_data.tech_level[i] := 8;
    mis_data.starting_money[i] := 5000;
    mis_data.allocation_index[i] := i;
  end;
  // Allegiance
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      if i = j then
        mis_data.allegiance[i,j] := 0
      else
        mis_data.allegiance[i,j] := 1;
    end;
end;

function get_event_contents(index: integer): String;
var
  event: ^TEvent;
  event_type: EventType;
  contents: string;
  i: integer;
begin
  event := Addr(mis_data.events[index]);
  event_type := EventType(event.event_type);
  contents := '';
  if event_type_info[event.event_type].use_unit_list then
  begin
    contents := deploy_action[event.deploy_action] + ' (' + inttostr(event.num_units) + ') ';
    for i := 0 to event.num_units - 1 do
    begin
      if i > 0 then
        contents := contents + ', ';
      contents := contents + unit_names[event.units[i]];
    end;
  end;
  case event_type of
    etAllegiance:   contents := player_names[event.player] + ' -> ' + player_names[event.allegiance_target] + ' (' + allegiance_type[event.allegiance_type] + ')';
    etSetBuildRate: contents := inttostr(event.value);
    etSetAttackBuildingRate: contents := inttostr(event.value);
    etSetCash:      contents := inttostr(event.value);
    etSetTech:      contents := inttostr(event.value);
    etRevealMap:    contents := inttostr(event.num_units);
    etShowTimer:    contents := inttostr(event.value);
    etShowMessage:
    begin
      contents := '(' + inttostr(event.message_index) + ') ';
      if event.message_index < cardinal(string_table_size) then
      contents := contents + string_table[event.message_index].text;
    end;
    etSetFlag:      contents := inttostr(event.player) + ' = ' + flag_value[event.value];
  end;
  {contents := inttostr(event.map_pos_x) + ' ' + inttostr(event.map_pos_y) + ' ' + inttostr(event.value) + ' ' + inttostr(event.num_units)  + ' ' + inttostr(event.player) + ' ' + inttostr(event.allegiance_target) + ' ' + inttostr(event.allegiance_type) + ' ' + inttostr(event.deploy_action);
  for i := 0 to 20 do
    contents := contents + ' ' + inttostr(event.units[i]);
  contents := contents + ' ' + inttostr(event.message_index);}
  result := contents;
end;

function get_event_conditions(index: integer): String;
var
  event: ^TEvent;
  conditions: String;
  cond_index: integer;
  i: integer;
begin
  event := Addr(mis_data.events[index]);
  conditions := '';
  for i := 0 to event.num_conditions - 1 do
  begin
    if i > 0 then
      conditions := conditions + ',   ';
    conditions := conditions + '[';
    if event.condition_not[i] = 1 then
      conditions := conditions + 'x';
    cond_index := event.condition_index[i];
    conditions := conditions + inttostr(cond_index) + ']' + condition_type_info[mis_data.conditions[cond_index].condition_type].name + '(' + get_condition_contents(cond_index, true) + ')';
  end;
  result := conditions;
end;

function get_condition_contents(index: integer; show_player: boolean): String;
var
  cond: ^TCondition;
  cond_type: ConditionType;
  contents: String;
  space: String;
begin
  cond := Addr(mis_data.conditions[index]);
  cond_type := ConditionType(cond.condition_type);
  space := '';
  if condition_type_info[cond.condition_type].use_player_index and show_player then
  begin
    contents := contents + player_names[cond.player];
    space := ' ';
  end;
  case cond_type of
    ctBuildingExists: contents := contents + space + building_names[cond.building_type];
    ctUnitExists:     contents := contents + space + unit_names[cond.unit_type_or_comparison_function];
    ctInterval:       contents := contents + inttostr(cond.time_amount) + ' ' + inttostr(cond.start_delay) + ' ' + inttostr(cond.value);
    ctTimer:          contents := contents + comparison_function[cond.unit_type_or_comparison_function] + inttostr(cond.time_amount);
    ctCasualties:     contents := contents + space + inttostr(cond.value) + ' ' + IntToHex(cond.casualty_flags, 8);
    ctTileRevealed:   contents := contents + inttostr(cond.map_pos_x) + ' ' + inttostr(cond.map_pos_y);
    ctSpiceHarvested: contents := contents + inttostr(cond.value);
  end;
  //contents := contents + inttostr(cond.time_amount) + ' ' + inttostr(cond.start_delay) + ' ' + inttostr(cond.more_uses) + ' ' + inttostr(cond.map_pos_x) + ' ' + inttostr(cond.map_pos_y) + ' ' + inttostr(cond.casualty_flags) + ' ' + inttostr(cond.side) + ' ' + inttostr(cond.building_type) + ' ' + inttostr(cond.unit_type_or_comparison_function);
  result := contents;
end;

procedure delete_condition(deleted_index: integer);
var
  i, j, k: integer;
begin
  if deleted_index >= mis_data.num_conditions then
    exit;
  // Delete condition and shift all conditions up
  for i := deleted_index to mis_data.num_conditions - 2 do
    mis_data.conditions[i] := mis_data.conditions[i+1];
  FillChar(mis_data.conditions[mis_data.num_conditions - 1], sizeof(TCondition), 0);
  // Modify all event's conditions
  for i := 0 to mis_data.num_events - 1 do
  begin
    // Go through all event's conditions
    for j := 0 to mis_data.events[i].num_conditions - 1 do
    begin
      // Decrease condition index if greater than deleted condition
      if mis_data.events[i].condition_index[j] > deleted_index then
        dec(mis_data.events[i].condition_index[j])
      // Delete condition from event if it is the deleted condition
      else if mis_data.events[i].condition_index[j] = deleted_index then
      begin
        for k := j to mis_data.events[i].num_conditions - 2 do
        begin
          mis_data.events[i].condition_index[k] := mis_data.events[i].condition_index[k+1];
          mis_data.events[i].condition_not[k] := mis_data.events[i].condition_not[k+1];
        end;
        mis_data.events[i].condition_index[mis_data.events[i].num_conditions - 1] := 0;
        mis_data.events[i].condition_not[mis_data.events[i].num_conditions - 1] := 0;
        dec(mis_data.events[i].num_conditions)
      end;
    end;
    // Modify flag number if event is Set Flag type
    if mis_data.events[i].event_type = Byte(etSetFlag) then
    begin
      if mis_data.events[i].player > deleted_index then
        dec(mis_data.events[i].player)
      else if mis_data.events[i].player = deleted_index then
        mis_data.events[i].player := 0;
    end;
  end;
  // Finally decrease number of conditions and fill event dialog grids
  dec(mis_data.num_conditions);
  EventDialog.fill_grids;
end;

end.
