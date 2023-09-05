unit _eventconfig;

interface

uses Classes, IniFiles, _utils, _gamelists;

type  CoordType =                           (ctNone, ctPoint, ctArea, ctPointAndSize);
const CoordTypeStr: array[0..3] of string = ('None', 'Point', 'Area', 'PointAndSize');

type  ArgType =                           (atNone, atNumber, atBigNumber, atHexNumber, atFloat, atList, atBool, atSwitch, atVariable);
const ArgTypeStr: array[0..8] of String = ('None', 'Number', 'BigNumber', 'HexNumber', 'Float', 'List', 'Bool', 'Switch', 'Variable');

type  ListType =                           (ltNone, ltCustom, ltGame, ltItem);
const ListTypeStr: array[0..3] of String = ('None', 'Custom', 'Game', 'Item');

type  ItemListType =                            (ilNone, ilSides, ilSidesAny, ilSounds, ilUnits, ilUnitGroups, ilBuildings, ilBuildingGroups, ilWeapons, ilExplosions, ilArmourTypes, ilSpeedTypes);
const ItemListTypeStr: array[0..11] of String = ('None', 'Sides', 'SidesAny', 'Sounds', 'Units', 'UnitGroups', 'Buildings', 'BuildingGroups', 'Weapons', 'Explosions', 'ArmourTypes', 'SpeedTypes');

type  ReferenceType =                           (rtNone, rtEvent, rtCondition);
const ReferenceTypeStr: array[0..2] of String = ('None', 'Event', 'Condition');

type  EventData =                            (edNone, edUnitList, edValueList, edCoordList, edAreaList, edByteValues, edMessage, edMusic, edTileBlock, edTilePairs, edCondExpr, edUnitFilter, edBuildingFilter, edCrateFilter, edTileFilter, edUnitTypeFilter, edBuildingTypeFilter);
const EventDataStr: array[0..16] of String = ('None', 'UnitList', 'ValueList', 'CoordList', 'AreaList', 'ByteValues', 'Message', 'Music', 'TileBlock', 'TilePairs', 'CondExpr', 'UnitFilter', 'BuildingFilter', 'CrateFilter', 'TileFilter', 'UnitTypeFilter', 'BuildingTypeFilter');

type  ConditionData =                           (cdNone, cdUnitFilter, cdBuildingFilter, cdCrateFilter, cdTileFilter);
const ConditionDataStr: array[0..4] of String = ('None', 'UnitFilter', 'BuildingFilter', 'CrateFilter', 'TileFilter');

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
    game_list_type: integer;
    item_list_type: ItemListType;
    reference: ReferenceType;
    default: integer;
    maxval: integer;
    readonly: boolean;
    is_gamestruct_arg: boolean;
    values: TStringList;
  end;

  TArgDefinitionPtr = ^TArgDefinition;

type
  TEventTypeDefinition = record
    name: String;
    coords: array[0..3] of TCoordDefinition;
    args: array[0..6] of TArgDefinition;
    gamestruct_index: integer;
    gamestruct_datatype_arg: integer;
    gamestruct_offset_arg: integer;
    gamestruct_value_arg: integer;
    event_data: EventData;
    contents: String;
    color: integer;
    is_start_block: boolean;
    allow_obj_index: boolean;
    has_map_pos: boolean;
    has_side: boolean;
  end;

  TEventTypeDefinitionPtr = ^TEventTypeDefinition;

type
  TConditionTypeDefinition = record
    name: String;
    coords: array[0..1] of TCoordDefinition;
    args: array[0..6] of TArgDefinition;
    gamestruct_index: integer;
    gamestruct_datatype_arg: integer;
    gamestruct_offset_arg: integer;
    gamestruct_value_arg: integer;
    condition_data: ConditionData;
    contents: String;
    has_map_pos: boolean;
    has_side: boolean;
  end;

  TConditionTypeDefinitionPtr = ^TConditionTypeDefinition;

type
  ConditionType = (ctBuildingExists, ctUnitExists, ctInterval, ctTimer,
    ctCasualties, ctBaseDestroyed, ctUnitsDestroyed, ctTileRevealed,
    ctSpiceHarvested, ctFlag);

type
  TFilterCriteriaDefinition = record
    name: String;
    list_type: ListType;
    game_list_type: integer;
    item_list_type: ItemListType;
    is_flag: boolean;
    default: integer;
    maxval: integer;
    values: TStringList;
  end;

  TFilterCriteriaDefinitionPtr = ^TFilterCriteriaDefinition;

  TFilterCriteriaDefinitionArr = array of TFilterCriteriaDefinition;

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
    // Filter criteria configuration
    filter_criteria: array[0..5] of TFilterCriteriaDefinitionArr;

  public
    procedure init;
  private
    procedure load_event_types_ini;
    procedure load_condition_types_ini;
    procedure load_filter_criteria_ini;
    procedure load_show_if_definition(show_if: TShowIfDefinitionPtr; string_def: string);
    procedure load_coord_definition(coord: TCoordDefinitionPtr; ini: TMemIniFile; ini_sect: string; index: integer);
    procedure load_argument_definition(arg: TArgDefinitionPtr; ini: TMemIniFile; ini_sect: string; index: integer);
    procedure load_filter_criteria(ini: TMemIniFile; index: integer; object_name: string);
  end;

function evaluate_show_if(show_if: TShowIfDefinitionPtr; data_ptr: Pointer; struct_def: TStructDefinitionPtr): boolean;

var
  EventConfig: TEventConfig;

implementation

uses SysUtils, _dispatcher, _gamestructs;

procedure TEventConfig.init;
begin
  load_event_types_ini;
  load_condition_types_ini;
  load_filter_criteria_ini;
  Dispatcher.register_event(evLoadEventTypeConfiguration);
end;

procedure TEventConfig.load_event_types_ini;
var
  tmp_filename: String;
  i, j: integer;
  s: string;
  ini: TMemIniFile;
  tmp_strings: TStringList;
begin
  event_types[-1].gamestruct_index := -1;
  tmp_filename := find_file('config\event_types.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load event types from ini file
  tmp_strings := TStringList.Create;
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
    // Load gamestruct properties
    event_types[i].gamestruct_index := GameStructs.struct_name_to_index(ini.ReadString(tmp_strings[i], 'gamestruct_name', ''));
    event_types[i].gamestruct_datatype_arg := ini.ReadInteger(tmp_strings[i], 'gamestruct_datatype_arg', 1);
    event_types[i].gamestruct_offset_arg := ini.ReadInteger(tmp_strings[i], 'gamestruct_offset_arg', 2);
    event_types[i].gamestruct_value_arg := ini.ReadInteger(tmp_strings[i], 'gamestruct_value_arg', 5);
    if event_types[i].gamestruct_index <> -1 then
    begin
      event_types[i].args[event_types[i].gamestruct_datatype_arg].is_gamestruct_arg := true;
      event_types[i].args[event_types[i].gamestruct_offset_arg].is_gamestruct_arg := true;
    end;
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
    // Load miscellaneous properties
    event_types[i].color := ini.ReadInteger(tmp_strings[i], 'color', -1);
    event_types[i].is_start_block := ini.ReadBool(tmp_strings[i], 'is_start_block', False);
    event_types[i].allow_obj_index := ini.ReadBool(tmp_strings[i], 'allow_obj_index', False);
    // Fill auxiliary properties
    event_types[i].has_map_pos := event_types[i].coords[0].marker <> ' ';
    event_types[i].has_side := event_types[i].args[0].name = 'Side';
  end;
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TEventConfig.load_condition_types_ini;
var
  tmp_filename: String;
  i, j: integer;
  s: string;
  ini: TMemIniFile;
  tmp_strings: TStringList;
begin
  condition_types[-1].gamestruct_index := -1;
  tmp_filename := find_file('config\condition_types.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load condition types from ini file
  tmp_strings := TStringList.Create;
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
    // Load gamestruct properties
    condition_types[i].gamestruct_index := GameStructs.struct_name_to_index(ini.ReadString(tmp_strings[i], 'gamestruct_name', ''));
    condition_types[i].gamestruct_datatype_arg := ini.ReadInteger(tmp_strings[i], 'gamestruct_datatype_arg', 1);
    condition_types[i].gamestruct_offset_arg := ini.ReadInteger(tmp_strings[i], 'gamestruct_offset_arg', 2);
    condition_types[i].gamestruct_value_arg := ini.ReadInteger(tmp_strings[i], 'gamestruct_value_arg', 5);
    if condition_types[i].gamestruct_index <> -1 then
    begin
      condition_types[i].args[condition_types[i].gamestruct_datatype_arg].is_gamestruct_arg := true;
      condition_types[i].args[condition_types[i].gamestruct_offset_arg].is_gamestruct_arg := true;
    end;
    // Load condition data
    s := ini.ReadString(tmp_strings[i], 'data', 'None');
    for j := 0 to High(ConditionDataStr) do
      if s = ConditionDataStr[j] then
      begin
        condition_types[i].condition_data := ConditionData(j);
        break;
      end;
    // Load contents
    condition_types[i].contents := ini.ReadString(tmp_strings[i], 'contents', '');
    // Fill auxiliary properties
    condition_types[i].has_map_pos := condition_types[i].coords[0].marker <> ' ';
    condition_types[i].has_side := condition_types[i].args[0].name = 'Side';
  end;
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TEventConfig.load_filter_criteria_ini;
var
  tmp_filename: String;
  ini: TMemIniFile;
begin
  tmp_filename := find_file('config\filter_criteria.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load filter criteria from ini file
  ini := TMemIniFile.Create(tmp_filename);
  load_filter_criteria(ini, 0, 'Unit');
  load_filter_criteria(ini, 1, 'Building');
  load_filter_criteria(ini, 2, 'Crate');
  load_filter_criteria(ini, 3, 'Tile');
  load_filter_criteria(ini, 4, 'UnitType');
  load_filter_criteria(ini, 5, 'BuildingType');
  ini.Destroy;
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
  // Load game list type
  if (arg.list_type = ltGame) then
    arg.game_list_type := GameLists.get_list_index(ini.ReadString(ini_sect, n + '.list_type', 'None'));
  // Load item list type
  if (arg.list_type = ltItem) then
  begin
    arg.item_list_type := ilNone;
    s := ini.ReadString(ini_sect, n + '.list_type', 'None');
    for i := 0 to High(ItemListTypeStr) do
      if s = ItemListTypeStr[i] then
      begin
        arg.item_list_type := ItemListType(i);
        break;
      end;
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

procedure TEventConfig.load_filter_criteria(ini: TMemIniFile; index: integer; object_name: string);
var
  s: String;
  i, j, start: integer;
  tmp_strings, decoder: TStringList;
begin
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder.Delimiter := ';';
  ini.ReadSection(object_name, tmp_strings);
  SetLength(filter_criteria[index], tmp_strings.Count);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    filter_criteria[index, i].name := tmp_strings[i];
    decoder.DelimitedText := ini.ReadString(object_name, tmp_strings[i], '');
    // Load list type
    filter_criteria[index, i].list_type := ltNone;
    if decoder.Count > 0 then
      for j := 0 to High(ListTypeStr) do
        if decoder[0] = ListTypeStr[j] then
        begin
          filter_criteria[index, i].list_type := ListType(j);
          break;
        end;
    // Load game list type
    if (filter_criteria[index, i].list_type = ltGame) then
      filter_criteria[index, i].game_list_type := GameLists.get_list_index(decoder[1]);
    // Load item list type
    if (filter_criteria[index, i].list_type = ltItem) then
    begin
      filter_criteria[index, i].item_list_type := ilNone;
      for j := 0 to High(ItemListTypeStr) do
        if decoder[1] = ItemListTypeStr[j] then
        begin
          filter_criteria[index, i].item_list_type := ItemListType(j);
          break;
        end;
    end;
    // Load list of values
    if (filter_criteria[index, i].list_type = ltCustom) then
    begin
      filter_criteria[index, i].values := TStringList.Create;
      s := decoder[1];
      start := 1;
      for j := 1 to Length(s) do
        if s[j] = ',' then
        begin
          filter_criteria[index, i].values.Add(Copy(s, start, j - start));
          start := j + 1;
        end;
      filter_criteria[index, i].values.Add(Copy(s, start, Length(s) - start + 1));
    end;
    // Load properties
    if (filter_criteria[index, i].list_type = ltNone) then
    begin
      filter_criteria[index, i].is_flag := (decoder.Count > 3) and (decoder[3] = '1');
      filter_criteria[index, i].default := 0;
      filter_criteria[index, i].maxval := 255;
      if decoder.Count > 1 then
        filter_criteria[index, i].default := StrToInt(decoder[1]);
      if decoder.Count > 2 then
        filter_criteria[index, i].maxval := StrToInt(decoder[2]);
    end else
      filter_criteria[index, i].is_flag := (decoder.Count > 2) and (decoder[2] = '1');
  end;
  tmp_strings.Destroy;
  decoder.Destroy;
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
