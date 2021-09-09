unit _eventconfig;

interface

uses Classes, IniFiles, _utils;

type  CoordType =                           (ctNone, ctPoint, ctArea);
const CoordTypeStr: array[0..2] of string = ('None', 'Point', 'Area');

type  ArgType =                           (atNone, atNumber, atBigNumber, atHexNumber, atFloat, atList, atBool, atSwitch);
const ArgTypeStr: array[0..7] of String = ('None', 'Number', 'BigNumber', 'HexNumber', 'Float', 'List', 'Bool', 'Switch');

type  ListType =                           (ltNone, ltCustom, ltPlayers, ltSounds, ltBuildings, ltUnits, ltWeapons, ltExplosions);
const ListTypeStr: array[0..7] of String = ('None', 'Custom', 'Players', 'Sounds', 'Buildings', 'Units', 'Weapons', 'Explosions');

type  ReferenceType =                           (rtNone, rtEvent, rtCondition);
const ReferenceTypeStr: array[0..2] of String = ('None', 'Event', 'Condition');

type  EventData =                           (edNone, edUnitList, edByteValues, edMessage, edMusic, edTileBlock, edTilePairs);
const EventDataStr: array[0..6] of String = ('None', 'UnitList', 'ByteValues', 'Message', 'Music', 'TileBlock', 'TilePairs');

type
  TShowIfDefinition = record
    arg_ref: shortint;
    comp_func: char;
    value: integer;
  end;

  TShowIfDefinitionPtr = ^TShowIfDefinition;

type
  TCoordDefinition = record
    name: String;
    show_if: TShowIfDefinition;
    coord_type: CoordType;
    marker: char;
    default: integer;
    maxval: integer;
    readonly: boolean;
  end;

  TCoordDefinitionPtr = ^TCoordDefinition;

type
  TArgDefinition = record
    name: String;
    show_if: TShowIfDefinition;
    arg_type: ArgType;
    list_type: ListType;
    reference: ReferenceType;
    default: integer;
    maxval: integer;
    readonly: boolean;
    values: TStringList;
  end;

  TArgDefinitionPtr = ^TArgDefinition;

type
  TEventTypeDefinition = record
    name: String;
    coords: array[0..3] of TCoordDefinition;
    args: array[0..5] of TArgDefinition;
    event_data: EventData;
    contents: String;
    has_map_pos: boolean;
    has_player: boolean;
  end;

  TEventTypeDefinitionPtr = ^TEventTypeDefinition;

type
  TConditionTypeDefinition = record
    name: String;
    coords: array[0..3] of TCoordDefinition;
    args: array[0..6] of TArgDefinition;
    contents: String;
    has_map_pos: boolean;
    has_player: boolean;
  end;

  TConditionTypeDefinitionPtr = ^TConditionTypeDefinition;

type
  ConditionType = (ctBuildingExists, ctUnitExists, ctInterval, ctTimer,
    ctCasualties, ctBaseDestroyed, ctUnitsDestroyed, ctTileRevealed,
    ctSpiceHarvested, ctFlag);

type
  TEventConfig = class

  public
    // Event configuration
    event_types: array[-1..255] of TEventTypeDefinition;
    event_type_mapping: array[0..255] of byte;
    cnt_valid_event_types: integer;
    // Condition configuration
    condition_types: array[-1..255] of TConditionTypeDefinition;
    condition_type_mapping: array[0..255] of byte;
    cnt_valid_condition_types: integer;

  public
    procedure init;
  private
    procedure load_event_types_ini;
    procedure load_condition_types_ini;
    procedure load_show_if_definition(show_if: TShowIfDefinitionPtr; string_def: string);
    procedure load_coord_definition(coord: TCoordDefinitionPtr; ini: TMemIniFile; ini_sect: string; index: integer);
    procedure load_argument_definition(arg: TArgDefinitionPtr; ini: TMemIniFile; ini_sect: string; index: integer);
  end;

function evaluate_show_if(show_if: TShowIfDefinitionPtr; data_ptr: Pointer; struct_def: TStructDefinitionPtr): boolean;

var
  EventConfig: TEventConfig;

implementation

uses SysUtils, _dispatcher;

procedure TEventConfig.init;
begin
  load_event_types_ini;
  load_condition_types_ini;
  Dispatcher.register_event(evLoadEventTypeConfiguration);
end;

procedure TEventConfig.load_event_types_ini;
var
  tmp_filename: String;
  i, j: integer;
  s: string;
  ini: TMemIniFile;
  tmp_strings, decoder: TStringList;
begin
  tmp_filename := find_file('config\event_types.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load event types from ini file
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder.Delimiter := ';';
  ini := TMemIniFile.Create(tmp_filename);
  ini.ReadSections(tmp_strings);
  cnt_valid_event_types := 0;
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i = Length(event_types) then
      break;
    // Skipped event type
    if tmp_strings[i][1] = '#' then
      continue;
    // Set up mapping
    event_type_mapping[cnt_valid_event_types] := i;
    inc(cnt_valid_event_types);
    // Load name
    event_types[i].name := tmp_strings[i];
    // Load coords
    for j := 0 to Length(event_types[i].coords) - 1 do
      load_coord_definition(Addr(event_types[i].coords[j]), ini, tmp_strings[i], j);
    // Load args
    for j := 0 to Length(event_types[i].args) - 1 do
      load_argument_definition(Addr(event_types[i].args[j]), ini, tmp_strings[i], j);
    // Load event data
    s := ini.ReadString(tmp_strings[i], 'data', 'None');
    for j := 0 to High(EventDataStr) do
      if s = EventDataStr[j] then
      begin
        event_types[i].event_data := EventData(j);
        break;
      end;
    // Load contents
    event_types[i].contents := ini.ReadString(tmp_strings[i], 'contents', '');
    // Fill auxiliary properties
    event_types[i].has_map_pos := event_types[i].coords[0].marker <> ' ';
    event_types[i].has_player := event_types[i].args[0].name = 'Player';
  end;
  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
end;

procedure TEventConfig.load_condition_types_ini;
var
  tmp_filename: String;
  i, j: integer;
  ini: TMemIniFile;
  tmp_strings, decoder: TStringList;
begin
  tmp_filename := find_file('config\condition_types.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load condition types from ini file
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder.Delimiter := ';';
  ini := TMemIniFile.Create(tmp_filename);
  ini.ReadSections(tmp_strings);
  cnt_valid_condition_types := 0;
  for i := 0 to tmp_strings.Count - 1 do
  begin
    if i = Length(condition_types) then
      break;
    // Skipped condition type
    if tmp_strings[i][1] = '#' then
      continue;
    // Set up mapping
    condition_type_mapping[cnt_valid_condition_types] := i;
    inc(cnt_valid_condition_types);
    // Load name
    condition_types[i].name := tmp_strings[i];
    // Load coords
    for j := 0 to Length(condition_types[i].coords) - 1 do
      load_coord_definition(Addr(condition_types[i].coords[j]), ini, tmp_strings[i], j);
    // Load args
    for j := 0 to Length(condition_types[i].args) - 1 do
      load_argument_definition(Addr(condition_types[i].args[j]), ini, tmp_strings[i], j);
    // Load contents
    condition_types[i].contents := ini.ReadString(tmp_strings[i], 'contents', '');
    // Fill auxiliary properties
    condition_types[i].has_map_pos := condition_types[i].coords[0].marker <> ' ';
    condition_types[i].has_player := condition_types[i].args[0].name = 'Player';
  end;
  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
end;

procedure TEventConfig.load_show_if_definition(show_if: TShowIfDefinitionPtr; string_def: string);
begin
  if string_def = '' then
  begin
    show_if.arg_ref := -1;
    exit;
  end;
  show_if.arg_ref := ord(string_def[1]) - ord('0');
  show_if.comp_func := string_def[2];
  show_if.value := StrToInt(Copy(string_def, 3, Length(string_def) - 2));
end;

procedure TEventConfig.load_coord_definition(coord: TCoordDefinitionPtr; ini: TMemIniFile; ini_sect: string; index: integer);
var
  s, n: string;
  i: integer;
begin
  n := 'coord' + inttostr(index);
  coord.name := ini.ReadString(ini_sect, n + '.name', '');
  load_show_if_definition(Addr(coord.show_if), ini.ReadString(ini_sect, n + '.showif', ''));
  // Load coordinate type
  coord.coord_type := ctNone;
  s := ini.ReadString(ini_sect, n + '.type', 'None');
  for i := 0 to High(CoordTypeStr) do
    if s = CoordTypeStr[i] then
    begin
      coord.coord_type := CoordType(i);
      break;
    end;
  // Load others
  s := ini.ReadString(ini_sect, n + '.marker', ' ');
  coord.marker := s[1];
  coord.default := ini.ReadInteger(ini_sect, n + '.default', 0);
  coord.maxval := ini.ReadInteger(ini_sect, n + '.maxval', 127);
  coord.readonly := ini.ReadBool(ini_sect, n + '.readonly', false);
end;

procedure TEventConfig.load_argument_definition(arg: TArgDefinitionPtr; ini: TMemIniFile; ini_sect: string; index: integer);
var
  s, n: string;
  i, start: integer;
begin
  n := 'arg' + inttostr(index);
  arg.name := ini.ReadString(ini_sect, n + '.name', '');
  load_show_if_definition(Addr(arg.show_if), ini.ReadString(ini_sect, n + '.showif', ''));
  // Load argument type
  arg.arg_type := atNone;
  s := ini.ReadString(ini_sect, n + '.type', 'None');
  for i := 0 to High(ArgTypeStr) do
    if s = ArgTypeStr[i] then
    begin
      arg.arg_type := ArgType(i);
      break;
    end;
  // Load list type
  arg.list_type := ltNone;
  s := ini.ReadString(ini_sect, n + '.list', 'None');
  for i := 0 to High(ListTypeStr) do
    if s = ListTypeStr[i] then
    begin
      arg.list_type := ListType(i);
      break;
    end;
  // Load reference type
  arg.reference := rtNone;
  s := ini.ReadString(ini_sect, n + '.reference', 'None');
  for i := 0 to High(ReferenceTypeStr) do
    if s = ReferenceTypeStr[i] then
    begin
      arg.reference := ReferenceType(i);
      break;
    end;
  // Load others
  arg.default := ini.ReadInteger(ini_sect, n + '.default', 0);
  arg.maxval := ini.ReadInteger(ini_sect, n + '.maxval', 0);
  arg.readonly := ini.ReadBool(ini_sect, n + '.readonly', false);
  // Load list of values
  if (arg.list_type = ltCustom) or (arg.arg_type = atSwitch) then
  begin
    arg.values := TStringList.Create;
    s := ini.ReadString(ini_sect, n + '.values', '');
    start := 1;
    for i := 1 to Length(s) do
      if s[i] = ';' then
      begin
        arg.values.Add(Copy(s, start, i - start));
        start := i + 1;
      end;
    arg.values.Add(Copy(s, start, Length(s) - start + 1));
  end;
end;

function evaluate_show_if(show_if: TShowIfDefinitionPtr; data_ptr: Pointer; struct_def: TStructDefinitionPtr): boolean;
var
  value: integer;
begin
  result := true;
  if show_if.arg_ref = -1 then
    exit;
  value := get_integer_struct_member(data_ptr, struct_def, show_if.arg_ref);
  case show_if.comp_func of
    '=': result := value = show_if.value;
    '>': result := value > show_if.value;
    '<': result := value < show_if.value;
  end;
end;

end.
