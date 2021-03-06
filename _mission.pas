unit _mission;

interface

uses Graphics, IniFiles, _map, _misai, _utils;

// Mis file constants
const player_annihilated_msgid: array[0..7] of integer = (602, 600, 601, 606, 605, 603, 604, 0);

const flag_value: array[0..1] of string = ('False', 'True');
const deploy_action: array[0..3] of string = ('Guard', 'Attack', 'Retreat', 'Stay');
const allegiance_type: array[0..2] of string = ('Ally', 'Enemy', 'Neutral');
const allegiance_type_color: array[0..2] of TColor = (clGreen, clRed, clOlive);
const comparison_function: array[0..3] of string = ('>', '<', '=', '%');

// Mis file type definitions
type
  EventType = (etReinforcement, etStarportDelivery, etAllegiance, etLeave,
    etBerserk, etPlaySound, etSetBuildRate, etSetAttackBuildingRate, etSetCash,
    etSetTech, etMissionWin, etMissionFail, etBloxFile, etAttribFile,
    etRevealMap, etShowTimer, etHideTimer, etShowMessage, etUnitSpawn, etSetFlag,
    etUnused, etPlayMusic);

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
    casualties_ratio: single;
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
    ai_segments:      array[0..7] of TMisAISegment;
    allegiance:       array[0..7, 0..7] of byte;
    events:           array[0..63] of TEvent;
    conditions:       array[0..47] of TCondition;
    tileset:          array[0..199] of char;
    tileatr:          array[0..199] of char;
    num_events:       byte;
    num_conditions:   byte;
    time_limit:       integer;
    unknown2:         array[0..691] of byte;
  end;

type
  TEventMarker = record
    emtype: EventMarkerType;
    side: byte;
    index: byte;
    moved: boolean;
  end;

type
  TEventTypeInfo = record
    name: String;
    key: word;
    use_map_position: boolean;
    use_player_index: boolean;
    use_unit_list: boolean;
    value_name: string;
  end;

type
  TConditionTypeInfo = record
    name: String;
    key: word;
    use_player_index: boolean;
    value_name: string;
  end;

type
  TEventMarkerTypeInfo = record
    letter: char;
    player_related: boolean;
  end;

// Mis file type definition constants
const event_type_info: array[0..21] of TEventTypeInfo =
  (
    (name: 'Reinforcement';             key: ord('R'); use_map_position: true;  use_player_index: true;  use_unit_list: true;  value_name: '';),
    (name: 'Starport Delivery';         key: ord('D'); use_map_position: false; use_player_index: true;  use_unit_list: true;  value_name: '';),
    (name: 'Allegiance';                key: ord('A'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Leave';                     key: ord('L'); use_map_position: false; use_player_index: true;  use_unit_list: false; value_name: '';),
    (name: 'Berserk';                   key: ord('B'); use_map_position: false; use_player_index: true;  use_unit_list: false; value_name: '';),
    (name: 'Play Sound';                key: ord('O'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Set Build Rate';            key: ord('G'); use_map_position: false; use_player_index: true;  use_unit_list: false; value_name: 'Build rate';),
    (name: 'Set Attack Building Rate';  key: ord('J'); use_map_position: false; use_player_index: true;  use_unit_list: false; value_name: 'Attack rate';),
    (name: 'Set Cash';                  key: ord('Y'); use_map_position: false; use_player_index: true ; use_unit_list: false; value_name: 'Cash';),
    (name: 'Set Tech';                  key: ord('T'); use_map_position: false; use_player_index: true ; use_unit_list: false; value_name: 'Tech level';),
    (name: 'Mission Win';               key: ord('W'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Mission Fail';              key: ord('F'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: '(unsupported)';             key: ord(' '); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: '(unsupported)';             key: ord(' '); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Reveal Map';                key: ord('V'); use_map_position: true;  use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Show Timer';                key: ord('I'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: 'Time';),
    (name: 'Hide Timer';                key: ord('H'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Show Message';              key: ord('M'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: 'Unknown';),
    (name: 'Unit Spawn';                key: ord('S'); use_map_position: true;  use_player_index: true;  use_unit_list: true;  value_name: '';),
    (name: 'Set Flag';                  key: ord('E'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: '(unused)';                  key: ord(' '); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';),
    (name: 'Play Music';                key: ord('U'); use_map_position: false; use_player_index: false; use_unit_list: false; value_name: '';)
  );

const condition_type_info: array[0..9] of TConditionTypeInfo =
  (
    (name: 'Building Exists'; key: ord('B'); use_player_index: true;  value_name: '';),
    (name: 'Unit Exists';     key: ord('U'); use_player_index: true;  value_name: '';),
    (name: 'Interval';        key: ord('I'); use_player_index: false; value_name: 'Run count';),
    (name: 'Timer';           key: ord('T'); use_player_index: false; value_name: '';),
    (name: 'Casualties';      key: ord('C'); use_player_index: true;  value_name: 'Threshold';),
    (name: 'Base Destroyed';  key: ord('A'); use_player_index: true;  value_name: 'Unknown';),
    (name: 'Units Destroyed'; key: ord('E'); use_player_index: true;  value_name: 'Unknown';),
    (name: 'Tile Revealed';   key: ord('R'); use_player_index: false; value_name: 'Unknown';),
    (name: 'Spice Harvested'; key: ord('H'); use_player_index: false; value_name: 'Credits';),
    (name: 'Flag';            key: ord('F'); use_player_index: false; value_name: '';)
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

// Mission class
type
  TMission = class

  public
    // MIS file related data
    mis_filename: String;
    mis_assigned: boolean;
    mis_modified: boolean;
    mis_data: TMisFile;
    // Other data
    event_markers: array[0..max_map_width-1, 0..max_map_height-1] of TEventMarker;

  public
    procedure init;
    // Loading and saving procedures
    function get_mis_filename(map_filename: String): String;
    procedure load_mission(map_filename: String);
    procedure save_mission(map_filename: String; is_testmap: boolean);
    procedure assign_mission;
    procedure new_mission;
    procedure unload_mission;
    procedure reset_mission_data;
    // Dispatcher procedures
    procedure cache_event_markers;
    // Getting text descriptions
    function get_event_contents(index: integer): String;
    function get_event_conditions(index: integer): String;
    function get_condition_contents(index: integer; show_player: boolean): String;
    function check_event_has_condition(index: integer; condition_index: integer): boolean;
    // Creating/deleting events/conditions
    function add_event(position: integer): boolean;
    function add_condition: boolean;
    procedure delete_event(deleted_index: integer);
    procedure delete_condition(deleted_index: integer);
    function condition_is_used(index: integer): boolean;
    procedure swap_events(e1, e2: integer);
    procedure swap_conditions(c1, c2: integer);
    // Auto-creating common events
    function get_or_create_condition(condition_type: ConditionType; player: integer; time_amount: cardinal; unit_type_or_comp_func: byte): integer;
    procedure create_unit_spawn(player: integer; num_events: integer);
    procedure create_harvester_replacement(player: integer);
    procedure create_annihilated_message(player: integer; use_alloc_index: boolean; alloc_index: integer);
    procedure add_run_once_flag(event_num: integer);
    // Miscellaneous
    procedure shift_event_positions(shift_x: integer; shift_y: integer);
    procedure adjust_event_positions_on_map_resize;
    function check_errors: String;
    function get_player_alloc_index(player: integer): integer;
  end;

var
  Mission: TMission;

implementation

uses SysUtils, Math, _missionini, _tileset, _stringtable, _settings, _structures, _dispatcher;

procedure TMission.init;
begin
  reset_mission_data;
end;

function TMission.get_mis_filename(map_filename: String): String;
begin
  result := ChangeFileExt(ExtractFileDir(map_filename)+'\_'+UpperCase(ExtractFileName(map_filename)), '.MIS');
end;

procedure TMission.load_mission(map_filename: String);
var
  tmp_filename: string;
begin
  tmp_filename := get_mis_filename(map_filename);
  if not FileExists(tmp_filename) then
  begin
    MissionIni.unload_mission_ini(false);
    unload_mission;
    // Change tileset to default or currently loaded one
    if (Tileset.tileset_index = -1) then
      Tileset.change_tileset_to_default
    else
      Tileset.load_tileset(false);
    exit;
  end;

  mis_filename := tmp_filename;
  mis_assigned := true;
  mis_modified := false;

  // Load actual data
  load_binary_file(tmp_filename, mis_data, sizeof(mis_data));
  // Load mission ini file
  MissionIni.load_mission_ini(map_filename);
  // Change tileset according to mission's tileset
  Tileset.change_tileset_by_name(mis_data.tileset, mis_data.tileatr);
  // Do needed actions
  Dispatcher.register_event(evMisLoad);
end;

procedure TMission.save_mission(map_filename: String; is_testmap: boolean);
var
  tmp_filename: string;
begin
  tmp_filename := get_mis_filename(map_filename);
  if not mis_assigned then
  begin
    if FileExists(tmp_filename) then
      DeleteFile(tmp_filename);
    MissionIni.save_mission_ini(map_filename, is_testmap);
    exit;
  end;
  if not is_testmap then
  begin
    mis_filename := tmp_filename;
    mis_modified := false;
  end;

  // Save actual data
  save_binary_file(tmp_filename, mis_data, sizeof(mis_data));
  // Save mission ini file
  MissionIni.save_mission_ini(map_filename, is_testmap);
end;

procedure TMission.assign_mission;
begin
  mis_assigned := true;
  Dispatcher.register_event(evMisLoad);
end;

procedure TMission.new_mission;
begin
  unload_mission;
  if Settings.AssignMisFileToNewMap then
    assign_mission;
end;

procedure TMission.unload_mission;
begin
  if not mis_assigned then
    exit;
  mis_filename := '';
  mis_assigned := false;
  reset_mission_data;
  // Uload mission ini file
  MissionIni.unload_mission_ini(true);
  // Do needed actions
  Dispatcher.register_event(evMisLoad);
end;

procedure TMission.reset_mission_data;
var
  i, j: integer;
begin
  FillChar(mis_data, sizeof(mis_data), 0);
  // Write tileset name
  Move(Tileset.tileset_name[1], mis_data.tileset, Length(Tileset.tileset_name));
  Move(Tileset.tileatr_name[1], mis_data.tileatr, Length(Tileset.tileatr_name));
  // Player properties and AI
  for i := 0 to 7 do
  begin
    mis_data.tech_level[i] := Settings.DefaultMisTechLevel;
    mis_data.starting_money[i] := Settings.DefaultMisStartingMoney;
    mis_data.allocation_index[i] := i;
    MisAI.init_misai_segment(mis_data.ai_segments[i], i);
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
  // Time limit
  mis_data.time_limit := -1;
  // Clear mofication flag
  mis_modified := false;
end;

procedure TMission.cache_event_markers;
var
  event: ^TEvent;
  condition: ^TCondition;
  event_type: EventType;
  x, y: integer;
  i: integer;
  moved: boolean;
  attempts: integer;
begin
  // Clear event markers
  for x := 0 to max_map_width - 1 do
    for y := 0 to max_map_height - 1 do
      event_markers[x,y].emtype := emNone;
  // Process events
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
      attempts := 0;
      while (event_markers[x][y].emtype <> emNone) and (attempts < Map.width) do
      begin
        x := (x + 1) mod (Map.width);
        moved := true;
        // Prevent infinite loop
        Inc(attempts);
      end;
      if attempts = Map.width then
        break;
      if event_type = etUnitSpawn then
        event_markers[x][y].emtype := emUnitSpawn
      else if event_type = etRevealMap then
         event_markers[x][y].emtype := emRevealMap
      else if (event.num_units = 1) and (event.units[0] = 8) then
        event_markers[x][y].emtype := emHarvester
      else
        event_markers[x][y].emtype := emReinforcement;
      event_markers[x][y].side := event.player;
      event_markers[x][y].index := i;
      event_markers[x][y].moved := moved;
    end;
  end;
  // Process conditions
  for i:= 0 to mis_data.num_conditions - 1 do
  begin
    condition := Addr(mis_data.conditions[i]);
    if condition.condition_type = byte(ctTileRevealed) then
    begin
      // Unit in tile
      x := condition.map_pos_x;
      y := condition.map_pos_y;
      event_markers[x][y].emtype := emTileRevealed;
      event_markers[x][y].index := i;
    end;
  end;
end;

function TMission.get_event_contents(index: integer): String;
var
  event: ^TEvent;
  event_type: EventType;
  contents: string;
  i: integer;
  dummy: boolean;
  tmp_unit_count: array[0..MAX_UNIT_TYPES-1] of byte;
begin
  event := Addr(mis_data.events[index]);
  event_type := EventType(event.event_type);
  contents := '';
  if event_type_info[event.event_type].use_unit_list then
  begin
    if event_type <> etStarportDelivery then
      contents := deploy_action[event.deploy_action] + ' ';
    contents := contents + '(' + inttostr(event.num_units) + '): ';
    dummy := false;
    for i := 0 to Length(tmp_unit_count) - 1 do
      tmp_unit_count[i] := 0;
    for i := 0 to event.num_units - 1 do
    begin
      if event.units[i] < Length(tmp_unit_count) then
        Inc(tmp_unit_count[event.units[i]]);
    end;
    for i := 0 to Length(tmp_unit_count) - 1 do
    begin
      if tmp_unit_count[i] > 0 then
      begin
        if dummy then
          contents := contents + ',  ';
        contents := contents + inttostr(tmp_unit_count[i]) + 'x ' + Structures.get_unit_name_str(i);
        dummy := true;
      end;
    end;
  end;
  case event_type of
    etAllegiance:   contents := Structures.player_names[event.player] + ' -> ' + Structures.player_names[event.allegiance_target] + ' (' + allegiance_type[event.allegiance_type] + ')';
    etPlaySound:    contents := inttostr(event.value) + ' - ' + StringTable.samples_uib.ValueFromIndex[event.value];
    etSetBuildRate: contents := inttostr(event.value);
    etSetAttackBuildingRate: contents := inttostr(event.value);
    etSetCash:      contents := inttostr(event.value);
    etSetTech:      contents := inttostr(event.value);
    etRevealMap:    contents := inttostr(event.num_units);
    etShowTimer:    contents := inttostr(event.value);
    etShowMessage:
    begin
      contents := '(' + inttostr(event.message_index) + ') ';
      contents := contents + StringTable.get_text(event.message_index, true, dummy);
    end;
    etSetFlag:      contents := inttostr(event.player) + ' = ' + flag_value[event.value];
    etPlayMusic:    SetString(contents, PChar(Addr(event.units[0])), StrLen(PChar(Addr(event.units[0]))));
  end;
  {contents := inttostr(event.map_pos_x) + ' ' + inttostr(event.map_pos_y) + ' ' + inttostr(event.value) + ' ' + inttostr(event.num_units)  + ' ' + inttostr(event.player) + ' ' + inttostr(event.allegiance_target) + ' ' + inttostr(event.allegiance_type) + ' ' + inttostr(event.deploy_action);
  for i := 0 to 20 do
    contents := contents + ' ' + inttostr(event.units[i]);
  contents := contents + ' ' + inttostr(event.message_index);}
  result := contents;
end;

function TMission.get_event_conditions(index: integer): String;
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

function TMission.get_condition_contents(index: integer; show_player: boolean): String;
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
    contents := contents + Structures.player_names[cond.player];
    space := ' ';
  end;
  case cond_type of
    ctBuildingExists: contents := contents + space + Structures.get_building_name_str(cond.building_type);
    ctUnitExists:     contents := contents + space + Structures.get_unit_name_str(cond.unit_type_or_comparison_function);
    ctInterval:       contents := contents + inttostr(cond.start_delay) + ' ' + inttostr(cond.time_amount) + ' ' + inttostr(cond.value);
    ctTimer:          contents := contents + comparison_function[cond.unit_type_or_comparison_function] + inttostr(cond.time_amount);
    ctCasualties:     contents := contents + space + inttostr(cond.value) + '  ' + floattostrf(cond.casualties_ratio, ffFixed, 8, 3);
    ctTileRevealed:   contents := contents + inttostr(cond.map_pos_x) + ' ' + inttostr(cond.map_pos_y);
    ctSpiceHarvested: contents := contents + inttostr(cond.value);
    ctFlag:           contents := contents + MissionIni.condition_notes[index];
  end;
  //contents := contents + inttostr(cond.time_amount) + ' ' + inttostr(cond.start_delay) + ' ' + inttostr(cond.more_uses) + ' ' + inttostr(cond.map_pos_x) + ' ' + inttostr(cond.map_pos_y) + ' ' + inttostr(cond.casualty_flags) + ' ' + inttostr(cond.side) + ' ' + inttostr(cond.building_type) + ' ' + inttostr(cond.unit_type_or_comparison_function);
  result := contents;
end;

function TMission.check_event_has_condition(index, condition_index: integer): boolean;
var
  event: ^TEvent;
  i: integer;
begin
  event := Addr(mis_data.events[index]);
  for i := 0 to event.num_conditions - 1 do
  begin
    if event.condition_index[i] = condition_index then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TMission.add_event(position: integer): boolean;
var
  i: integer;
begin
  if (mis_data.num_events = Length(mis_data.events)) or (position > mis_data.num_events) then
  begin
    result := false;
    exit;
  end;
  for i := mis_data.num_events downto position + 1 do
  begin
    mis_data.events[i] := mis_data.events[i-1];
    MissionIni.event_notes[i] := MissionIni.event_notes[i-1];
  end;
  FillChar(mis_data.events[position], sizeof(TEvent), 0);
  MissionIni.event_notes[position] := '';
  inc(mis_data.num_events);
  mis_modified := true;
  result := true;
end;

function TMission.add_condition: boolean;
begin
  if mis_data.num_conditions = Length(mis_data.conditions) then
  begin
    result := false;
    exit;
  end;
  FillChar(mis_data.conditions[mis_data.num_conditions], sizeof(TCondition), 0);
  MissionIni.condition_notes[mis_data.num_conditions] := '';
  inc(mis_data.num_conditions);
  mis_modified := true;
  result := true;
end;

procedure TMission.delete_event(deleted_index: integer);
var
  event_used_position: boolean;
  i: integer;
begin
  if deleted_index >= mis_data.num_events then
    exit;
  event_used_position := event_type_info[mis_data.events[deleted_index].event_type].use_map_position;
  // Delete event and shift all events up
  for i := deleted_index to mis_data.num_events - 2 do
  begin
    mis_data.events[i] := mis_data.events[i+1];
    MissionIni.event_notes[i] := MissionIni.event_notes[i+1];
  end;
  FillChar(mis_data.events[mis_data.num_events - 1], sizeof(TEvent), 0);
  MissionIni.event_notes[mis_data.num_events - 1] := '';
  dec(mis_data.num_events);
  mis_modified := true;
  // Update event markers on map if event had position
  if event_used_position then
    Dispatcher.register_event(evMisEventPositionChange);
end;

procedure TMission.delete_condition(deleted_index: integer);
var
  condition_used_position: boolean;
  i, j, k, m: integer;
  event: ^TEvent;
begin
  if deleted_index >= mis_data.num_conditions then
    exit;
  condition_used_position := mis_data.conditions[deleted_index].condition_type = Byte(ctTileRevealed);
  // Delete condition and shift all conditions up
  for i := deleted_index to mis_data.num_conditions - 2 do
  begin
    mis_data.conditions[i] := mis_data.conditions[i+1];
    MissionIni.condition_notes[i] := MissionIni.condition_notes[i+1];
  end;
  FillChar(mis_data.conditions[mis_data.num_conditions - 1], sizeof(TCondition), 0);
  MissionIni.condition_notes[mis_data.num_conditions - 1] := '';
  // Go through all events
  for i := 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    // Go through all event's conditions
    m := 0;
    for j := 0 to event.num_conditions - 1 do
    begin
      // Decrease condition index if greater than deleted condition
      if event.condition_index[j+m] > deleted_index then
        dec(event.condition_index[j+m])
      // Delete condition from event if it is the deleted condition
      else if event.condition_index[j] = deleted_index then
      begin
        for k := j to event.num_conditions - 2 do
        begin
          event.condition_index[k] := event.condition_index[k+1];
          event.condition_not[k] := event.condition_not[k+1];
        end;
        event.condition_index[event.num_conditions - 1] := 0;
        event.condition_not[event.num_conditions - 1] := 0;
        dec(event.num_conditions);
        m := -1;
      end;
    end;
    // Modify flag number if event is Set Flag type
    if event.event_type = Byte(etSetFlag) then
    begin
      if event.player > deleted_index then
        dec(event.player)
      else if event.player = deleted_index then
        event.player := 0;
    end;
  end;
  // Finally decrease number of conditions and fill event dialog grids
  dec(mis_data.num_conditions);
  mis_modified := true;
  // Update event markers on map if condition had position
  if condition_used_position then
    Dispatcher.register_event(evMisEventPositionChange);
end;

function TMission.condition_is_used(index: integer): boolean;
var
  i, j: integer;
  event: ^TEvent;
begin
  result := false;
  if index >= mis_data.num_conditions then
    exit;
  for i := 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    for j := 0 to event.num_conditions - 1 do
      if event.condition_index[j] = index then
      begin
        result := true;
        exit;
      end;
  end;
end;

procedure TMission.swap_events(e1, e2: integer);
var
  event_used_position: boolean;
  tmp_event: TEvent;
  tmp_note: String;
begin
  tmp_event := mis_data.events[e1];
  mis_data.events[e1] := mis_data.events[e2];
  mis_data.events[e2] := tmp_event;
  tmp_note := MissionIni.event_notes[e1];
  MissionIni.event_notes[e1] := MissionIni.event_notes[e2];
  MissionIni.event_notes[e2] := tmp_note;
  mis_modified := true;
  // Update event markers on map if event had position
  event_used_position := (event_type_info[mis_data.events[e1].event_type].use_map_position) or (event_type_info[mis_data.events[e2].event_type].use_map_position);
  if event_used_position then
    Dispatcher.register_event(evMisEventPositionChange);
end;

procedure TMission.swap_conditions(c1, c2: integer);
var
  condition_used_position: boolean;
  i, j: integer;
  tmp_condition: TCondition;
  tmp_note: String;
  event: ^TEvent;
begin
  if (c1 >= mis_data.num_conditions) or (c2 >= mis_data.num_conditions) then
    exit;
  condition_used_position := (mis_data.conditions[c1].condition_type = Byte(ctTileRevealed)) or (mis_data.conditions[c2].condition_type = Byte(ctTileRevealed));
  // Swap conditions
  tmp_condition := mis_data.conditions[c1];
  mis_data.conditions[c1] := mis_data.conditions[c2];
  mis_data.conditions[c2] := tmp_condition;
  // Swap condition notes
  tmp_note := MissionIni.condition_notes[c1];
  MissionIni.condition_notes[c1] := MissionIni.condition_notes[c2];
  MissionIni.condition_notes[c2] := tmp_note;
  // Go through all events
  for i := 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    // Go through all event's conditions
    for j := 0 to event.num_conditions - 1 do
    begin
      // Decrease condition index if greater than deleted condition
      if event.condition_index[j] = c1 then
        event.condition_index[j] := c2
      else if event.condition_index[j] = c2 then
        event.condition_index[j] := c1;
    end;
    // Modify flag number if event is Set Flag type
    if event.event_type = Byte(etSetFlag) then
    begin
      if event.player = c1 then
        event.player := c2
      else if event.player = c2 then
        event.player := c1;
    end;
  end;
  mis_modified := true;
  // Update event markers on map if condition had position
  if condition_used_position then
    Dispatcher.register_event(evMisEventPositionChange);
end;

function TMission.get_or_create_condition(condition_type: ConditionType;
  player: integer; time_amount: cardinal;
  unit_type_or_comp_func: byte): integer;
var
  i: integer;
  cond: ^TCondition;
begin
  // Try to find condition if it already exists
  for i := 0 to mis_data.num_conditions - 1 do
  begin
    cond := Addr(mis_data.conditions[i]);
    if  (cond.condition_type = Byte(condition_type)) and
        (cond.player = player) and
        (cond.time_amount = time_amount) and
        (cond.unit_type_or_comparison_function = unit_type_or_comp_func) and
        (condition_type <> ctFlag) then // Always create new flag
    begin
      result := i;
      exit;
    end;
  end;
  // Condition does not exist, must create one
  if not add_condition then
  begin
    result := -1;
    exit;
  end;
  result := mis_data.num_conditions - 1;
  cond := Addr(mis_data.conditions[result]);
  cond.condition_type := Byte(condition_type);
  cond.player := player;
  cond.time_amount := time_amount;
  cond.unit_type_or_comparison_function := unit_type_or_comp_func;
  if (condition_type = ctBaseDestroyed) or (condition_type = ctUnitsDestroyed) then
    cond.value := 1;
end;

procedure TMission.create_unit_spawn(player, num_events: integer);
var
  i: integer;
  cond_index: integer;
  event: ^TEvent;
begin
  cond_index := get_or_create_condition(ctTimer,0,1,2);
  if cond_index = -1 then
    exit;
  for i := 0 to num_events - 1 do
  begin
    if not add_event(mis_data.num_events) then
      exit;
    event := Addr(mis_data.events[mis_data.num_events-1]);
    event.event_type := Byte(etUnitSpawn);
    event.player := player;
    event.deploy_action := 2;
    event.num_conditions := 1;
    event.condition_index[0] := cond_index;
  end;
  Dispatcher.register_event(evMisEventPositionChange);
end;

procedure TMission.create_harvester_replacement(player: integer);
var
  event: array[1..3] of ^TEvent;
  cond_index: array[1..4] of integer;
  i: integer;
begin
  // Create all needed conditions
  cond_index[1] := get_or_create_condition(ctBaseDestroyed, player, 0, 0);
  cond_index[2] := get_or_create_condition(ctUnitExists, player, 0, 8);
  cond_index[3] := get_or_create_condition(ctTimer, 0, 500, 3);
  cond_index[4] := get_or_create_condition(ctFlag, 0, 0, 0);
  for i := 1 to 4 do
    if cond_index[i] = -1 then
      exit;
  // Create all needed events
  for i := 1 to 3 do
  begin
    if not add_event(mis_data.num_events) then
      exit;
    event[i] := Addr(mis_data.events[mis_data.num_events-1]);
  end;
  // Fill all event contents
  event[1].event_type := Byte(etReinforcement);
  event[1].player := player;
  event[1].num_units := 1;
  event[1].units[0] := 8;
  event[2].event_type := Byte(etSetFlag);
  event[2].player := cond_index[4];
  event[2].value := 0;
  event[3].event_type := Byte(etSetFlag);
  event[3].player := cond_index[4];
  event[3].value := 1;
  // Fill all event conditions
  event[1].num_conditions := 4;
  event[2].num_conditions := 4;
  event[3].num_conditions := 2;
  for i := 1 to 4 do
  begin
    event[1].condition_index[i-1] := cond_index[i];
    event[2].condition_index[i-1] := cond_index[i];
  end;
  event[1].condition_not[0] := 1;
  event[1].condition_not[1] := 1;
  event[2].condition_not[0] := 1;
  event[2].condition_not[1] := 1;
  event[3].condition_index[0] := cond_index[2];
  event[3].condition_index[1] := cond_index[4];
  event[3].condition_not[1] := 1;
  Dispatcher.register_event(evMisEventPositionChange);
end;

procedure TMission.create_annihilated_message(player: integer; use_alloc_index: boolean; alloc_index: integer);
var
  players: array[0..cnt_players-1] of integer;
  num_players: integer;
  event: ^TEvent;
  cond1_index, cond2_index: integer;
  i: integer;
begin
  num_players := 0;
  // Get list of players to make base/units destroyed conditions for
  if use_alloc_index then
  begin
    // All players having given allocation index
    for i := 0 to cnt_players-1 do
      if mis_data.allocation_index[i] = alloc_index then
      begin
        players[num_players] := i;
        inc(num_players);
      end;
  end else
  begin
    // Just the player itself
    players[0] := player;
    num_players := 1;
  end;
  if num_players = 0 then
    exit;
  // Create message event
  if not add_event(mis_data.num_events) then
    exit;
  event := Addr(mis_data.events[mis_data.num_events - 1]);
  event.event_type := Byte(etShowMessage);
  event.message_index := player_annihilated_msgid[player];
  event.value := 300;
  // Create base/units destroyed conditions
  for i := 0 to num_players - 1 do
  begin
    cond1_index := get_or_create_condition(ctBaseDestroyed, players[i], 0, 0);
    cond2_index := get_or_create_condition(ctUnitsDestroyed, players[i], 0, 0);
    if (cond1_index = -1) or (cond2_index = -1) then
      exit;
    event.condition_index[i*2] := cond1_index;
    event.condition_index[i*2+1] := cond2_index;
    event.num_conditions := event.num_conditions + 2;
  end;
  // Finally create run-once flag
  add_run_once_flag(mis_data.num_events - 1);
end;

procedure TMission.add_run_once_flag(event_num: integer);
var
  flag_number: integer;
  new_condition: integer;
  i: integer;
begin
  if event_num >= mis_data.num_events then
    exit;
  // Check if this event is Set flag (can be ignored)
  if mis_data.events[event_num].event_type = Byte(etSetFlag) then
    exit;
  // Check if this event has already flag in list of conditions
  for i := 0 to mis_data.events[event_num].num_conditions do
  begin
    if (mis_data.events[event_num].condition_not[i] = 1) and
      (mis_data.conditions[mis_data.events[event_num].condition_index[i]].condition_type = Byte(ctFlag)) then
      exit;
  end;
  // Try to create new event (Set flag) and condition (Flag)
  if not add_event(event_num+1) or not add_condition then
    exit;
  flag_number := mis_data.num_conditions - 1;
  new_condition := mis_data.events[event_num].num_conditions;
  // Add new flag to event's conditions
  mis_data.events[event_num].num_conditions := new_condition + 1;
  mis_data.events[event_num].condition_index[new_condition] := flag_number;
  mis_data.events[event_num].condition_not[new_condition] := 1;
  // Fill new event
  mis_data.events[event_num+1].event_type := Byte(etSetFlag);
  mis_data.events[event_num+1].player := flag_number;
  mis_data.events[event_num+1].value := 1;
  mis_data.events[event_num+1].num_conditions := new_condition + 1;
  Move(mis_data.events[event_num].condition_index, mis_data.events[event_num+1].condition_index, Length(mis_data.events[0].condition_index));
  Move(mis_data.events[event_num].condition_not, mis_data.events[event_num+1].condition_not, Length(mis_data.events[0].condition_not));
  // Fill new condition
  mis_data.conditions[flag_number].condition_type := Byte(ctFlag);
end;

procedure TMission.shift_event_positions(shift_x, shift_y: integer);
var
  i: integer;
  event: ^TEvent;
  condition: ^TCondition;
  new_pos_x, new_pos_y: integer;
begin
  for i := 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    if event_type_info[event.event_type].use_map_position then
    begin
      new_pos_x := Integer(event.map_pos_x) + shift_x;
      new_pos_y := Integer(event.map_pos_y) + shift_y;
      event.map_pos_x := min(Map.width - 1, max(0, new_pos_x));
      event.map_pos_y := min(Map.height - 1, max(0, new_pos_y));
    end;
  end;
  for i := 0 to mis_data.num_conditions - 1 do
  begin
    condition := Addr(mis_data.conditions[i]);
    if condition.condition_type = Byte(ctTileRevealed) then
    begin
      new_pos_x := Integer(condition.map_pos_x) + shift_x;
      new_pos_y := Integer(condition.map_pos_y) + shift_y;
      condition.map_pos_x := min(Map.width - 1, max(0, new_pos_x));
      condition.map_pos_y := min(Map.height - 1, max(0, new_pos_y));
    end;
  end;
end;

procedure TMission.adjust_event_positions_on_map_resize;
var
  i: integer;
  event: ^TEvent;
  condition: ^TCondition;
begin
  for i := 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    if event_type_info[event.event_type].use_map_position then
    begin
      event.map_pos_x := min(Map.width - 1, event.map_pos_x);
      event.map_pos_y := min(Map.height - 1, event.map_pos_y);
    end;
  end;
  for i := 0 to mis_data.num_conditions - 1 do
  begin
    condition := Addr(mis_data.conditions[i]);
    if condition.condition_type = Byte(ctTileRevealed) then
    begin
      condition.map_pos_x := min(Map.width - 1, condition.map_pos_x);
      condition.map_pos_y := min(Map.height - 1, condition.map_pos_y);
    end;
  end;
end;

function TMission.check_errors: String;
var
  i: integer;
begin
  // Check if players with active AI have non-zero tech and credits
  for i := 0 to 7 do
  begin
    if (mis_data.ai_segments[i, 1] = 1) and ((mis_data.tech_level[i] = 0) or (mis_data.starting_money[i] = 0)) then
    begin
      result := format('Players with active AI must have non-zero tech level and credits. Player ''%s'' does not meet this requirement.', [Structures.player_names[i]]);
      exit;
    end;
  end;
  // Check if Reinforcement or Starport Delivery events have non-zero units.
  for i := 0 to mis_data.num_events - 1 do
  begin
    if ((mis_data.events[i].event_type = Byte(etReinforcement)) or (mis_data.events[i].event_type = Byte(etStarportDelivery))) and (mis_data.events[i].num_units = 0) then
    begin
      result := format('Event #%d of type %s has zero units to deliver.', [i, event_type_info[mis_data.events[i].event_type].name]);
      exit;
    end;
  end;
  result := '';
end;

function TMission.get_player_alloc_index(player: integer): integer;
begin
  result := player;
  if not mis_assigned then
    exit;
  result := Min(mis_data.allocation_index[player], CNT_PLAYERS - 1);
end;

end.
