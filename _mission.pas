unit _mission;

interface

uses Graphics, Classes, IniFiles, _map, _misai, _utils, _eventconfig;

// Mis file constants
const player_annihilated_msgid: array[0..7] of integer = (602, 600, 601, 606, 605, 603, 604, 0);
const allegiance_type: array[0..2] of string = ('Ally', 'Enemy', 'Neutral');
const allegiance_type_color: array[0..2] of TColor = (clGreen, clRed, clOlive);

// *****************************************************************************
// Event definition
// *****************************************************************************
type
  TEvent = packed record
    coord_x: array[0..3] of byte;          // 0
    coord_y: array[0..3] of byte;          // 4
    value: cardinal;                       // 8
    num_conditions: byte;                  // 12
    event_type: byte;                      // 13
    amount: byte;                          // 14
    player: byte;                          // 15
    arg2: byte;                            // 16
    arg3: byte;                            // 17
    arg4: byte;                            // 18
    condition_index: array[0..11] of byte; // 19
    coord_var_flags: byte;                 // 31
    arg_var_flags: byte;                   // 32
    condition_not: array[0..11] of byte;   // 33
    conditions_and_or: byte;               // 45
    blocked_flags: byte;                   // 46
    data: array[0..24] of byte;            // 47
  end;                                     // 72

const event_args_struct_members: array[0..5] of TStructMemberDefinition =
  (
    (pos: 15; bytes: 1), // player
    (pos: 14; bytes: 1), // amount
    (pos: 16; bytes: 1), // arg2
    (pos: 17; bytes: 1), // arg3
    (pos: 18; bytes: 1), // arg4
    (pos: 8;  bytes: 4)  // value
  );

// *****************************************************************************
// Condition definition
// *****************************************************************************

type
  TCondition = packed record
    time_amount: cardinal;        // 0
    start_delay: cardinal;        // 4
    value: cardinal;              // 8
    coord_x: array[0..3] of byte; // 12
    coord_y: array[0..3] of byte; // 16
    casualties_ratio: single;     // 20
    player: byte;                 // 24
    condition_type: byte;         // 25
    arg1: byte;                   // 26 cpBuildingType          (building type)
    arg2: byte;                   // 27 cpUnitType, cpTimer     (unit type, comparison function)
  end;                            // 28

const condition_args_struct_members: array[0..6] of TStructMemberDefinition =
  (
    (pos: 24; bytes: 1), // player
    (pos: 26; bytes: 1), // arg1
    (pos: 27; bytes: 1), // arg2
    (pos: 4;  bytes: 4), // start_delay
    (pos: 0;  bytes: 4), // time_amount
    (pos: 8;  bytes: 4), // value
    (pos: 20; bytes: 4)  // casualties_ratio
  );

const comparison_function: array[0..3] of string = ('>', '<', '=', '%');

// *****************************************************************************
// Mis file definitions
// *****************************************************************************

const MAX_EVENTS = 64;
const MAX_CONDITIONS = 48;

type
  TMisFile = packed record
    tech_level:       array[0..7] of byte;
    starting_money:   array[0..7] of cardinal;
    unknown1:         array[0..39] of byte;
    allocation_index: array[0..7] of byte;
    ai_segments:      array[0..7] of TMisAISegment;
    allegiance:       array[0..7, 0..7] of byte;
    events:           array[0..MAX_EVENTS-1] of TEvent;
    conditions:       array[0..MAX_CONDITIONS-1] of TCondition;
    tileset:          array[0..199] of char;
    tileatr:          array[0..199] of char;
    num_events:       byte;
    num_conditions:   byte;
    time_limit:       integer;
    unknown2:         array[0..691] of byte;
  end;

// *****************************************************************************
// Event markers definitions
// *****************************************************************************

type
  TEventMarkerType = (emNone, emEvent, emCondition);

type
  TEventMarker = record
    emtype: TEventMarkerType;
    index: byte;
    coord: byte;
    side: shortint;
    marker: char;
    moved: boolean;
  end;

// *****************************************************************************
// Mission class
// *****************************************************************************
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
    function get_coords_contents(x, y: integer): String;
    function get_argument_contents(value: integer; argdef: TArgDefinitionPtr): String;
    function get_event_data_contents(event_id: integer): string;
    function check_event_has_condition(index: integer; condition_index: integer): boolean;
    // Creating/deleting events/conditions
    function add_event(position, event_type: integer): integer;
    function add_condition(condition_type: integer): boolean;
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
    procedure adjust_event_positions(shift_x: integer; shift_y: integer);
    function check_errors: String;
    function get_player_alloc_index(player: integer): integer;
  end;

var
  Mission: TMission;

implementation

uses SysUtils, Math, _missionini, _tileset, _stringtable, _settings, _structures, _dispatcher, event_dialog,
  StrUtils;

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
  EventDialog.apply_changes;
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
  et: TEventTypeDefinitionPtr;
  ct: TConditionTypeDefinitionPtr;
  x, y: integer;
  i, j: integer;
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
    et := Addr(EventConfig.event_types[event.event_type]);
    for j := 0 to Length(et.coords) - 1 do
      if (et.coords[j].marker <> ' ') and evaluate_show_if(Addr(et.coords[j].show_if), event, Addr(event_args_struct_members)) then
      begin
        x := event.coord_x[j];
        y := event.coord_y[j];
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
          continue;
        // Fill event marker data
        event_markers[x][y].emtype := emEvent;
        event_markers[x][y].index := i;
        event_markers[x][y].coord := j;
        event_markers[x][y].side := IfThen(et.has_player, event.player, -1);
        event_markers[x][y].marker := et.coords[j].marker;
        if (event.event_type = 0) and (event.amount = 1) and (event.data[0] = Byte(Structures.templates.GroupIDs[16])) then
          event_markers[x][y].marker := 'H';
        event_markers[x][y].moved := moved;
    end;
  end;
  // Process conditions
  for i:= 0 to mis_data.num_conditions - 1 do
  begin
    condition := Addr(mis_data.conditions[i]);
    ct := Addr(EventConfig.condition_types[condition.condition_type]);
    for j := 0 to Length(ct.coords) - 1 do
      if (ct.coords[j].marker <> ' ') and evaluate_show_if(Addr(ct.coords[j].show_if), condition, Addr(condition_args_struct_members)) then
      begin
        x := condition.coord_x[j];
        y := condition.coord_y[j];
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
          continue;
        // Fill event marker data
        event_markers[x][y].emtype := emCondition;
        event_markers[x][y].index := i;
        event_markers[x][y].coord := j;
        event_markers[x][y].side := IfThen(ct.has_player, condition.player, -1);
        event_markers[x][y].marker := ct.coords[j].marker;
        event_markers[x][y].moved := moved;
    end;
  end;
end;

function TMission.get_event_contents(index: integer): String;
var
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  contents: string;
  i: integer;
  start: integer;
  idx: integer;
begin
  event := Addr(mis_data.events[index]);
  et := Addr(EventConfig.event_types[event.event_type]);
  contents := '';
  i := 0;
  start := 1;
  while i < Length(et.contents) do
  begin
    inc(i);
    if et.contents[i] <> '%' then
      continue;
    contents := contents + copy(et.contents, start, i - start);
    if (et.contents[i+1]) = 'c' then
    begin
      idx := (Ord(et.contents[i+2]) - Ord('0'));
      if evaluate_show_if(Addr(et.coords[idx].show_if), event, Addr(event_args_struct_members)) then
        contents := contents + get_coords_contents(event.coord_x[idx], event.coord_y[idx]);
      start := i + 3;
    end else
    if (et.contents[i+1]) = 'a' then
    begin
      idx := (Ord(et.contents[i+2]) - Ord('0'));
      if evaluate_show_if(Addr(et.args[idx].show_if), event, Addr(event_args_struct_members)) then
      begin
        if et.args[idx].arg_type = atFloat then
          contents := contents + floattostrf(get_float_struct_member(event, Addr(event_args_struct_members), idx), ffFixed, 8, 3)
        else
          contents := contents + get_argument_contents(get_integer_struct_member(event, Addr(event_args_struct_members), idx), Addr(et.args[idx]));
      end;
      start := i + 3;
    end else
    if (et.contents[i+1]) = 's' then
    begin
      contents := contents + get_event_data_contents(index);
      start := i + 2;
    end;
  end;
  contents := contents + copy(et.contents, start, Length(et.contents) + 1 - start);
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
    conditions := conditions + inttostr(cond_index) + ']' + EventConfig.condition_types[mis_data.conditions[cond_index].condition_type].name + '(' + get_condition_contents(cond_index, true) + ')';
  end;
  result := conditions;
end;

function TMission.get_condition_contents(index: integer; show_player: boolean): String;
var
  cond: ^TCondition;
  ct: TConditionTypeDefinitionPtr;
  contents: String;
  i: integer;
  start: integer;
  idx: integer;
begin
  cond := Addr(mis_data.conditions[index]);
  ct := Addr(EventConfig.condition_types[cond.condition_type]);
  if (ct.has_player) and show_player then
  begin
    contents := contents + Structures.player_names[cond.player];
    if Length(ct.contents) > 0 then
      contents := contents + ' ';
  end;
  i := 0;
  start := 1;
  while i < Length(ct.contents) do
  begin
    inc(i);
    if ct.contents[i] <> '%' then
      continue;
    contents := contents + copy(ct.contents, start, i - start);
    if (ct.contents[i+1]) = 'c' then
    begin
      idx := (Ord(ct.contents[i+2]) - Ord('0'));
      if evaluate_show_if(Addr(ct.coords[idx].show_if), cond, Addr(condition_args_struct_members)) then
        contents := contents + get_coords_contents(cond.coord_x[idx], cond.coord_y[idx]);
      start := i + 3;
    end else
    if (ct.contents[i+1]) = 'a' then
    begin
      idx := (Ord(ct.contents[i+2]) - Ord('0'));
      if evaluate_show_if(Addr(ct.args[idx].show_if), cond, Addr(condition_args_struct_members)) then
      begin
        if ct.args[idx].arg_type = atFloat then
          contents := contents + floattostrf(get_float_struct_member(cond, Addr(condition_args_struct_members), idx), ffFixed, 8, 3)
        else
          contents := contents + get_argument_contents(get_integer_struct_member(cond, Addr(condition_args_struct_members), idx), Addr(ct.args[idx]));
      end;
      start := i + 3;
    end;
  end;
  contents := contents + copy(ct.contents, start, Length(ct.contents) + 1 - start);
  {case cond_type of
    ctBuildingExists: contents := contents + space + Structures.get_building_name_str(cond.arg1);
    ctUnitExists:     contents := contents + space + Structures.get_unit_name_str(cond.arg2);
    ctInterval:       contents := contents + inttostr(cond.start_delay) + ' ' + inttostr(cond.time_amount) + ' ' + inttostr(cond.value);
    ctTimer:          contents := contents + comparison_function[cond.arg2] + inttostr(cond.time_amount);
    ctCasualties:     contents := contents + space + inttostr(cond.value) + '  ' + floattostrf(cond.casualties_ratio, ffFixed, 8, 3);
    ctTileRevealed:   contents := contents + inttostr(cond.coord_x[0]) + ' ' + inttostr(cond.coord_y[0]);
    ctSpiceHarvested: contents := contents + inttostr(cond.value);
    ctFlag:           contents := contents + MissionIni.condition_notes[index];
  end;}
  //contents := contents + inttostr(cond.time_amount) + ' ' + inttostr(cond.start_delay) + ' ' + inttostr(cond.more_uses) + ' ' + inttostr(cond.map_pos_x) + ' ' + inttostr(cond.map_pos_y) + ' ' + inttostr(cond.casualty_flags) + ' ' + inttostr(cond.side) + ' ' + inttostr(cond.building_type) + ' ' + inttostr(cond.unit_type_or_comparison_function);
  result := contents;
end;

function TMission.get_coords_contents(x, y: integer): String;
begin
  result := Format('%d , %d', [x, y]);
end;

function TMission.get_argument_contents(value: integer; argdef: TArgDefinitionPtr): String;
begin
  result := '';
  case argdef.arg_type of
    atBigNumber: result := inttostr(value);
    atNumber: result := inttostr(value);
    atHexNumber: result := IntToHex(value, 8);
    atList:
      begin
        case argdef.list_type of
          ltCustom: result := argdef.values[value];
          ltPlayers: result := Structures.player_names[value];
          ltSounds: result := StringTable.samples_uib.ValueFromIndex[value];
          ltBuildings: result := Structures.get_building_name_str(value);
          ltUnits: result := Structures.get_unit_name_str(value);
          ltWeapons: result := Structures.templates.WeaponStrings[value];
          ltExplosions: result := Structures.templates.ExplosionStrings[value];
        end;
      end;
    atBool: if (value <> 0) then result := '(' + argdef.name + ')';
    atSwitch: result := argdef.values[IfThen(value <> 0, 1, 0)];
  end;
end;

function TMission.get_event_data_contents(event_id: integer): string;
var
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  contents: string;
  i: integer;
  dummy: boolean;
  tmp_unit_count: array[0..MAX_UNIT_TYPES-1] of byte;
  message_index: integer;
begin
  event := Addr(mis_data.events[event_id]);
  et := Addr(EventConfig.event_types[event.event_type]);
  contents := '';
  if et.event_data = edUnitList then
  begin
    dummy := false;
    for i := 0 to Length(tmp_unit_count) - 1 do
      tmp_unit_count[i] := 0;
    for i := 0 to event.amount - 1 do
    begin
      if event.data[i] < Length(tmp_unit_count) then
        Inc(tmp_unit_count[event.data[i]]);
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
  if et.event_data = edMessage then
  begin
    message_index := get_integer_value(Addr(event.data), 21, 4);
    contents := '(' + inttostr(message_index) + ') ';
    contents := contents + StringTable.get_text(message_index, true, dummy);
  end;
  if et.event_data = edMusic then
    SetString(contents, PChar(Addr(event.data[0])), StrLen(PChar(Addr(event.data[0]))));
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

function TMission.add_event(position, event_type: integer): integer;
var
  i, j: integer;
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  event_reference: integer;
begin
  if mis_data.num_events = Length(mis_data.events) then
  begin
    result := -1;
    exit;
  end;
  position := Min(position, mis_data.num_events);
  for i := mis_data.num_events downto position + 1 do
  begin
    mis_data.events[i] := mis_data.events[i-1];
    MissionIni.event_notes[i] := MissionIni.event_notes[i-1];
  end;
  // Initialize new event with default values
  FillChar(mis_data.events[position], sizeof(TEvent), 0);
  mis_data.events[position].event_type := event_type;
  et := Addr(EventConfig.event_types[event_type]);
  for i := 0 to High(et.coords) do
  begin
    mis_data.events[position].coord_x[i] := et.coords[i].default;
    mis_data.events[position].coord_y[i] := et.coords[i].default;
  end;
  for i := 0 to High(et.args) do
    set_integer_struct_member(Addr(mis_data.events[position]), Addr(event_args_struct_members), i, et.args[i].default);
  MissionIni.event_notes[position] := '';
  // Go through all events and update event references
  for i := 0 to mis_data.num_events do
  begin
    event := Addr(mis_data.events[i]);
    et := Addr(EventConfig.event_types[event.event_type]);
    // Modify argument value for condition references
    for j := 0 to High(et.args) do
    begin
      if et.args[j].reference = rtEvent then
      begin
        event_reference := get_integer_struct_member(event, Addr(event_args_struct_members), j);
        if event_reference >= position then
          inc(event_reference);
        set_integer_struct_member(event, Addr(event_args_struct_members), j, event_reference);
      end;
    end;
  end;
  // Increase number of events
  inc(mis_data.num_events);
  mis_modified := true;
  result := position;
end;

function TMission.add_condition(condition_type: integer): boolean;
var
  i: integer;
  ct: TConditionTypeDefinitionPtr;
begin
  if mis_data.num_conditions = Length(mis_data.conditions) then
  begin
    result := false;
    exit;
  end;
  // Initialize new event with default values
  FillChar(mis_data.conditions[mis_data.num_conditions], sizeof(TCondition), 0);
  mis_data.conditions[mis_data.num_conditions].condition_type := condition_type;
  ct := Addr(EventConfig.condition_types[condition_type]);
  for i := 0 to High(ct.coords) do
  begin
    mis_data.conditions[mis_data.num_conditions].coord_x[i] := ct.coords[i].default;
    mis_data.conditions[mis_data.num_conditions].coord_y[i] := ct.coords[i].default;
  end;
  for i := 0 to High(ct.args) do
    set_integer_struct_member(Addr(mis_data.conditions[mis_data.num_conditions]), Addr(condition_args_struct_members), i, ct.args[i].default);
  MissionIni.condition_notes[mis_data.num_conditions] := '';
  // Increase number of conditions
  inc(mis_data.num_conditions);
  mis_modified := true;
  result := true;
end;

procedure TMission.delete_event(deleted_index: integer);
var
  event_used_position: boolean;
  i, j: integer;
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  event_reference: integer;
begin
  if deleted_index >= mis_data.num_events then
    exit;
  event_used_position := EventConfig.event_types[mis_data.events[deleted_index].event_type].has_map_pos;
  // Delete event and shift all events up
  for i := deleted_index to mis_data.num_events - 2 do
  begin
    mis_data.events[i] := mis_data.events[i+1];
    MissionIni.event_notes[i] := MissionIni.event_notes[i+1];
  end;
  FillChar(mis_data.events[mis_data.num_events - 1], sizeof(TEvent), 0);
  MissionIni.event_notes[mis_data.num_events - 1] := '';
  // Go through all events and fix references
  for i := 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    et := Addr(EventConfig.event_types[event.event_type]);
    // Modify argument value for condition references
    for j := 0 to High(et.args) do
    begin
      if et.args[j].reference = rtEvent then
      begin
        event_reference := get_integer_struct_member(event, Addr(event_args_struct_members), j);
        if event_reference > deleted_index then
          dec(event_reference)
        else if event_reference = deleted_index then
          event_reference := -1;
        set_integer_struct_member(event, Addr(event_args_struct_members), j, event_reference);
      end;
    end;
  end;
  // Decrease number of events
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
  et: TEventTypeDefinitionPtr;
  condition_reference: integer;
begin
  if deleted_index >= mis_data.num_conditions then
    exit;
  condition_used_position := EventConfig.condition_types[mis_data.conditions[deleted_index].condition_type].has_map_pos;
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
    et := Addr(EventConfig.event_types[event.event_type]);
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
    // Modify argument value for condition references
    for k := 0 to High(et.args) do
    begin
      if et.args[k].reference = rtCondition then
      begin
        condition_reference := get_integer_struct_member(event, Addr(event_args_struct_members), k);
        if condition_reference > deleted_index then
          dec(condition_reference)
        else if condition_reference = deleted_index then
          condition_reference := 0;
        set_integer_struct_member(event, Addr(event_args_struct_members), k, condition_reference);
      end;
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
  i, j: integer;
  tmp_event: TEvent;
  tmp_note: String;
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  event_reference: integer;
begin
  tmp_event := mis_data.events[e1];
  mis_data.events[e1] := mis_data.events[e2];
  mis_data.events[e2] := tmp_event;
  tmp_note := MissionIni.event_notes[e1];
  MissionIni.event_notes[e1] := MissionIni.event_notes[e2];
  MissionIni.event_notes[e2] := tmp_note;
  mis_modified := true;
  // Go through all events and update event references
  for i := 0 to mis_data.num_events do
  begin
    event := Addr(mis_data.events[i]);
    et := Addr(EventConfig.event_types[event.event_type]);
    // Modify argument value for condition references
    for j := 0 to High(et.args) do
    begin
      if et.args[j].reference = rtEvent then
      begin
        event_reference := get_integer_struct_member(event, Addr(event_args_struct_members), j);
        if event_reference = e1 then
          event_reference := e2
        else if event_reference = e2 then
          event_reference := e1;
        set_integer_struct_member(event, Addr(event_args_struct_members), j, event_reference);
      end;
    end;
  end;
  // Update event markers on map if event had position
  if (EventConfig.event_types[mis_data.events[e1].event_type].has_map_pos) or (EventConfig.event_types[mis_data.events[e2].event_type].has_map_pos) then
    Dispatcher.register_event(evMisEventPositionChange);
end;

procedure TMission.swap_conditions(c1, c2: integer);
var
  condition_used_position: boolean;
  i, j: integer;
  tmp_condition: TCondition;
  tmp_note: String;
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  condition_reference: integer;
begin
  if (c1 >= mis_data.num_conditions) or (c2 >= mis_data.num_conditions) then
    exit;
  condition_used_position := (EventConfig.condition_types[mis_data.conditions[c1].condition_type].has_map_pos) or (EventConfig.condition_types[mis_data.conditions[c2].condition_type].has_map_pos);
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
    et := Addr(EventConfig.event_types[event.event_type]);
    // Go through all event's conditions
    for j := 0 to event.num_conditions - 1 do
    begin
      // Decrease condition index if greater than deleted condition
      if event.condition_index[j] = c1 then
        event.condition_index[j] := c2
      else if event.condition_index[j] = c2 then
        event.condition_index[j] := c1;
    end;
    // Modify condition number for condition references
    for j := 0 to High(et.args) do
    begin
      if et.args[j].reference = rtCondition then
      begin
        condition_reference := get_integer_struct_member(event, Addr(event_args_struct_members), j);
        if condition_reference = c1 then
          condition_reference := c2
        else if condition_reference = c2 then
          condition_reference := c1;
        set_integer_struct_member(event, Addr(event_args_struct_members), j, condition_reference);
      end;
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
        (cond.arg2 = unit_type_or_comp_func) and
        (condition_type <> ctFlag) then // Always create new flag
    begin
      result := i;
      exit;
    end;
  end;
  // Condition does not exist, must create one
  if not add_condition(Byte(condition_type)) then
  begin
    result := -1;
    exit;
  end;
  result := mis_data.num_conditions - 1;
  cond := Addr(mis_data.conditions[result]);
  cond.player := player;
  cond.time_amount := time_amount;
  cond.arg2 := unit_type_or_comp_func;
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
    if add_event(mis_data.num_events, 18) = -1 then
      exit;
    event := Addr(mis_data.events[mis_data.num_events-1]);
    event.player := player;
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
  cond_index[2] := get_or_create_condition(ctUnitExists, player, 0, Structures.templates.GroupIDs[16]);
  cond_index[3] := get_or_create_condition(ctTimer, 0, 500, 3);
  cond_index[4] := get_or_create_condition(ctFlag, 0, 0, 0);
  for i := 1 to 4 do
    if cond_index[i] = -1 then
      exit;
  // Create all needed events
  for i := 1 to 3 do
  begin
    if add_event(mis_data.num_events, 0) = -1 then
      exit;
    event[i] := Addr(mis_data.events[mis_data.num_events-1]);
  end;
  // Fill all event contents
  event[1].event_type := 0;
  event[1].player := player;
  event[1].amount := 1;
  event[1].data[0] := Structures.templates.GroupIDs[16];
  event[2].event_type := 19;
  event[2].player := cond_index[4];
  event[2].value := 0;
  event[3].event_type := 19;
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
  if add_event(mis_data.num_events, 17) = -1 then
    exit;
  event := Addr(mis_data.events[mis_data.num_events - 1]);
  set_integer_value(Addr(event.data), 21, 4, player_annihilated_msgid[player]);
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
  if mis_data.events[event_num].event_type = 19 then
    exit;
  // Check if this event has already flag in list of conditions
  for i := 0 to mis_data.events[event_num].num_conditions do
  begin
    if (mis_data.events[event_num].condition_not[i] = 1) and
      (mis_data.conditions[mis_data.events[event_num].condition_index[i]].condition_type = Byte(ctFlag)) then
      exit;
  end;
  // Try to create new event (Set flag) and condition (Flag)
  if (add_event(event_num+1, 0) = -1) or not add_condition(-1) then
    exit;
  flag_number := mis_data.num_conditions - 1;
  new_condition := mis_data.events[event_num].num_conditions;
  // Add new flag to event's conditions
  mis_data.events[event_num].num_conditions := new_condition + 1;
  mis_data.events[event_num].condition_index[new_condition] := flag_number;
  mis_data.events[event_num].condition_not[new_condition] := 1;
  // Fill new event
  mis_data.events[event_num+1].event_type := 19;
  mis_data.events[event_num+1].player := flag_number;
  mis_data.events[event_num+1].value := 1;
  mis_data.events[event_num+1].num_conditions := new_condition + 1;
  Move(mis_data.events[event_num].condition_index, mis_data.events[event_num+1].condition_index, Length(mis_data.events[0].condition_index));
  Move(mis_data.events[event_num].condition_not, mis_data.events[event_num+1].condition_not, Length(mis_data.events[0].condition_not));
  // Fill new condition
  mis_data.conditions[flag_number].condition_type := Byte(ctFlag);
end;

procedure TMission.adjust_event_positions(shift_x, shift_y: integer);
var
  i, j: integer;
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  condition: ^TCondition;
  ct: TConditionTypeDefinitionPtr;
begin
  for i := 0 to mis_data.num_events - 1 do
  begin
    event := Addr(mis_data.events[i]);
    et := Addr(EventConfig.event_types[event.event_type]);
    for j := 0 to High(et.coords) do
      if et.coords[j].coord_type <> ctNone then
      begin
        event.coord_x[j] := min(Map.width - 1, max(0, Integer(event.coord_x[j]) + shift_x));
        event.coord_y[j] := min(Map.height - 1, max(0, Integer(event.coord_y[j]) + shift_y));
        if et.coords[j].coord_type = ctArea then
        begin
          event.coord_x[j+1] := min(Map.width - event.coord_x[j], event.coord_x[j+1]);
          event.coord_y[j+1] := min(Map.height - event.coord_y[j], event.coord_y[j+1]);
        end;
      end;
  end;
  for i := 0 to mis_data.num_conditions - 1 do
  begin
    condition := Addr(mis_data.conditions[i]);
    ct := Addr(EventConfig.condition_types[condition.condition_type]);
    for j := 0 to High(ct.coords) do
      if ct.coords[j].coord_type <> ctNone then
      begin
        condition.coord_x[j] := min(Map.width - 1, max(0, Integer(condition.coord_x[j]) + shift_x));
        condition.coord_y[j] := min(Map.height - 1, max(0, Integer(condition.coord_y[j]) + shift_y));
        if ct.coords[j].coord_type = ctArea then
        begin
          condition.coord_x[j+1] := min(Map.width - condition.coord_x[j], condition.coord_x[j+1]);
          condition.coord_y[j+1] := min(Map.height - condition.coord_y[j], condition.coord_y[j+1]);
        end;
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
    if ((mis_data.events[i].event_type = 0) or (mis_data.events[i].event_type = 1)) and (mis_data.events[i].amount = 0) then
    begin
      result := format('Event #%d of type %s has zero units to deliver.', [i, EventConfig.event_types[mis_data.events[i].event_type].name]);
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
