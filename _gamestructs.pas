unit _gamestructs;

interface

uses Classes, _eventconfig;

type
  GameStructDataType = (dtByte, dtWord, dtDword, dtFloat);

const game_struct_data_type_size: array[0..3] of word = (1, 2, 4, 4);

type
  TGameStructMember = record
    name_template: String;
    data_type: GameStructDataType;
    offset: integer;
    list_type: ListType;
    game_list_type: integer;
    item_list_type: ItemListType;
  end;

  TGameStructMemberPtr = ^TGameStructMember;

type
  TGameStructs = class

  public
    struct_names: TStringList;
    struct_members: array of array of TGameStructMember;
    struct_member_names: array of TStringList;
    struct_member_mapping: array of array of integer;
    struct_member_count: array of integer;
    ai_struct_index: integer;
  public
    procedure init;
    procedure load_game_structs_ini;
    procedure cache_struct_member_names;
    function get_misai_property(index: integer): TGameStructMemberPtr;
    function struct_name_to_index(name: string): integer;
    function get_struct_member_name_list(struct_index: integer): TStringList;
    function get_struct_member(struct_index: integer; member_index: integer): TGameStructMemberPtr; overload;
    function get_struct_member(struct_index: integer; data_type, offset: integer): TGameStructMemberPtr; overload;
    function get_struct_member_index(struct_index: integer; data_type, offset: integer): integer;
    function get_struct_member_name(struct_index: integer; member_index: integer): string;
  end;

var
  GameStructs: TGameStructs;

implementation

uses SysUtils, IniFiles, _utils, _structures, _gamelists;

procedure TGameStructs.init;
begin
  struct_names := TStringList.Create;
  ai_struct_index := -1;
  load_game_structs_ini;
end;

procedure TGameStructs.load_game_structs_ini;
var
  tmp_filename: String;
  i, j, k, l, start, order: integer;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  c: char;
  s, ss: string;
begin
  tmp_filename := find_file('config\game_structs.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load game structs from ini file
  tmp_strings := TStringList.Create;
  ini := TMemIniFile.Create(tmp_filename);
  ini.ReadSections(struct_names);
  SetLength(struct_members, struct_names.Count);
  SetLength(struct_member_names, struct_names.Count);
  SetLength(struct_member_mapping, struct_names.Count);
  SetLength(struct_member_count, struct_names.Count);
  for i := 0 to struct_names.Count - 1 do
  begin
    ini.ReadSection(struct_names[i], tmp_strings);
    SetLength(struct_members[i], tmp_strings.Count);
    struct_member_names[i] := TStringList.Create;
    SetLength(struct_member_mapping[i], tmp_strings.Count);
    if struct_names[i] = 'AI' then
      ai_struct_index := i;
    for j := 0 to tmp_strings.Count - 1 do
    begin
      // Parse name
      s := ini.ReadString(struct_names[i],tmp_strings[j],'');
      order := 0;
      start := 1;
      for k := 1 to Length(s) + 1 do
        if (k = Length(s) + 1) or (s[k] = ';') then
        begin
          ss := Copy(s, start, k - start);
          case order of
            // Member name
            0: struct_members[i, j].name_template := ss;
            // Member list type
            1:
              for l := 0 to High(ListTypeStr) do
                if ss = ListTypeStr[l] then
                begin
                  struct_members[i, j].list_type := ListType(l);
                  break;
                end;
            // Member game or item list type
            2:
              begin
                if struct_members[i, j].list_type = ltGame then
                  struct_members[i, j].game_list_type := GameLists.get_list_index(ss);
                if struct_members[i, j].list_type = ltItem then
                begin
                  struct_members[i, j].item_list_type := ilNone;
                  for l := 0 to High(ItemListTypeStr) do
                    if ss = ItemListTypeStr[l] then
                    begin
                      struct_members[i, j].item_list_type := ItemListType(l);
                      break;
                    end;
                end;
              end;
          end;
          start := k + 1;
          inc(order);
        end;
      // Parse data type and offset
      c := tmp_strings[j][1];
      case c of
        'w': struct_members[i, j].data_type := dtWord;
        'd': struct_members[i, j].data_type := dtDword;
        'f': struct_members[i, j].data_type := dtFloat;
        else struct_members[i, j].data_type := dtByte;
      end;
      struct_members[i, j].offset := strtoint(Copy(tmp_strings[j], 3, Length(tmp_strings[j]) - 2));
      // Default mapping
      struct_member_mapping[i, j] := j;
    end;
    struct_member_count[i] := tmp_strings.Count;
  end;
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TGameStructs.cache_struct_member_names;
var
  i, j, position, num: integer;
  name: String;
begin
  for i := 0 to Length(struct_members) - 1 do
  begin
    struct_member_names[i].Clear;
    struct_member_count[i] := 0;
    for j := 0 to Length(struct_members[i]) - 1 do
    begin
      name := struct_members[i, j].name_template;
      // Replace building group
      position := Pos('B#', name);
      if position > 0 then
      begin
        num := strtointdef(Copy(name, position+2, 2), MAX_BUILDING_TYPES);
        if num < Structures.templates.BuildingGroupCount then
          name := Copy(name, 0, position-1) + Structures.templates.BuildingGroupStrings[num]
        else
          continue;
      end;
      // Replace building name
      position := Pos('B2#', name);
      if position > 0 then
      begin
        num := strtointdef(Copy(name, position+3, 2), MAX_BUILDING_TYPES);
        if num < Structures.templates.BuildingCount then
          name := Copy(name, 0, position-1) + Structures.prettify_structure_name(Structures.templates.BuildingNameStrings[num])
        else
          continue;
      end;
      // Replace unit name
      position := Pos('U#', name);
      if position > 0 then
      begin
        num := strtointdef(Copy(name, position+2, 2), MAX_UNIT_TYPES);
        if num < Structures.templates.UnitCount then
          name := Copy(name, 0, position-1) + Structures.templates.UnitNameStrings[num]
        else
          continue;
      end;
      struct_member_names[i].Add(name);
      struct_member_mapping[i, struct_member_count[i]] := j;
      inc(struct_member_count[i]);
    end;
  end;
end;

function TGameStructs.get_misai_property(index: integer): TGameStructMemberPtr;
begin
  result := Addr(struct_members[ai_struct_index, struct_member_mapping[ai_struct_index, index]]);
end;

function TGameStructs.struct_name_to_index(name: string): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to struct_names.Count - 1 do
    if name = struct_names[i] then
    begin
      result := i;
      exit;
    end;
end;

function TGameStructs.get_struct_member_name_list(struct_index: integer): TStringList;
begin
  if struct_index = -1 then
    result := nil
  else
    result := struct_member_names[struct_index];
end;

function TGameStructs.get_struct_member(struct_index: integer; member_index: integer): TGameStructMemberPtr;
begin
  result := nil;
  if member_index = -1 then
    exit;
  if struct_index <> -1 then
    result := Addr(struct_members[struct_index, struct_member_mapping[struct_index, member_index]]);
end;

function TGameStructs.get_struct_member(struct_index: integer; data_type, offset: integer): TGameStructMemberPtr;
begin
  result := get_struct_member(struct_index, get_struct_member_index(struct_index, data_type, offset));
end;

function TGameStructs.get_struct_member_index(struct_index: integer; data_type, offset: integer): integer;
var
  i: integer;
begin
  result := -1;
  if struct_index = -1 then
    exit;
  for i := 0 to struct_member_count[struct_index] - 1 do
    if struct_members[struct_index, struct_member_mapping[struct_index, i]].offset = offset then
    begin
      if ord(struct_members[struct_index, struct_member_mapping[struct_index, i]].data_type) = data_type then
        result := i;
      exit;
    end;
end;

function TGameStructs.get_struct_member_name(struct_index: integer; member_index: integer): string;
begin
  result := '';
  if member_index = -1 then
    exit;
  if struct_index <> -1 then
    result := struct_member_names[struct_index][member_index];
end;

end.
