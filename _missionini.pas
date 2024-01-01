unit _missionini;

interface

uses
  StdCtrls, ValEdit, IniFiles, Classes, _mission;

const MAX_VARIABLES = 256;

type
  TRuleDefinition = record
    name: String;
    default_value: String;
  end;

type
  TMissionIni = class

  public
    // Rules
    rules_ini_filename: string;
    rules: array of TRuleDefinition;

    // Mission ini file related data
    mission_ini_filename: string;
    mission_ini_assigned: boolean;
    // Basic section
    Name: string;
    Author: string;
    Music: string;
    SideId: integer;
    MissionNumber: integer;
    TextUib: string;
    BriefingMemo: TMemo;
    // Data section
    CampaignFolder: string;
    ModsFolder: string;
    ColoursFile: string;
    PlayersFile: string;
    IntelId: string;
    // Vars section
    RuleValueList: TValueListEditor;
    // Text section
    TextValueList: TValueListEditor;
    // Notes section
    event_notes: array[0..MAX_EVENTS-1] of String;
    condition_notes: array[0..MAX_CONDITIONS-1] of String;
    // Variables section
    variable_names: array[0..MAX_VARIABLES] of String;

  public
    procedure init;
    procedure load_rules_ini;
    procedure init_controls(briefing_memo: TMemo; rule_value_list, text_value_list: TValueListEditor);
    // Loading and saving procedures
    procedure load_mission_ini(map_filename: String);
    procedure save_mission_ini(map_filename: String; is_testmap: boolean);
    procedure assign_mission_ini;
    procedure unload_mission_ini(load_tilesets: boolean);
    procedure reset_mission_ini_data;
    procedure reset_rules_to_defaults;
    // Export and import
    procedure export_events(first_event, last_event: integer; filename: string);
    procedure import_events(first_event: integer; filename: string);
    // Custom text related procedures
    function get_custom_text(index: integer; var text: String): boolean;
    procedure set_custom_text(index: integer; text: String);
    procedure remove_custom_text(index: integer);
    // Variable name related procedures
    function get_variable_name(index: integer; brackets: integer): string;
    function set_variable_name(index: integer; name: string): boolean;
    // Data manipulation procedures
    procedure set_side_id(value: integer);
    procedure set_mission_number(value: integer);
    procedure set_campaign_folder(value: string);
    procedure set_mods_folder(value: string);
    procedure set_colours_file(value: string);
    procedure set_players_file(value: string);
    procedure set_text_uib(value: string);
  private
    procedure load_custom_campaign_data_files(load_tilesets: boolean);
  end;

var
  MissionIni: TMissionIni;

implementation

uses Windows, SysUtils, StrUtils, Math, Forms, _utils, _dispatcher, _tileset, _structures, _graphics, _sounds, _stringtable, _launcher, _eventconfig;

{ TMissionIni }

procedure TMissionIni.init;

begin
  // Load rule definitions from ini file
  load_rules_ini;
  // Initialize with default mission ini file data
  reset_mission_ini_data;
end;

procedure TMissionIni.load_rules_ini;
var
  i: integer;
  tmp_filename: string;
  ini: TMemIniFile;
  tmp_strings: TStringList;
begin
  // Find rules.ini file
  tmp_filename := find_file('config\rules.ini', 'configuration');
  if (tmp_filename = '') or (tmp_filename = rules_ini_filename) then
    exit;
  rules_ini_filename := tmp_filename;
  // Load rules.ini file
  tmp_strings := TStringList.Create;
  ini := TMemIniFile.Create(tmp_filename);
  ini.ReadSection('Vars',tmp_strings);
  SetLength(rules, tmp_strings.Count);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    rules[i].name := tmp_strings[i];
    rules[i].default_value := ini.ReadString('Vars',tmp_strings[i],'');
  end;
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TMissionIni.init_controls(briefing_memo: TMemo; rule_value_list, text_value_list: TValueListEditor);
begin
  BriefingMemo := briefing_memo;
  RuleValueList := rule_value_list;
  TextValueList := text_value_list;
end;

procedure TMissionIni.load_mission_ini(map_filename: String);
var
  tmp_filename: string;
  i: integer;
  tmp_strings: TStringList;
  ini: TMemIniFile;
begin
  tmp_filename := ChangeFileExt(map_filename,'.ini');
  if not FileExists(tmp_filename) then
  begin
    unload_mission_ini(false);
    exit;
  end;

  mission_ini_filename := tmp_filename;
  mission_ini_assigned := true;

  // Load actual data
  ini := TMemIniFile.Create(tmp_filename);
  tmp_strings := TStringList.Create;
  // Load Basic section
  Name :=          ini.ReadString ('Basic','Name','');
  Author :=        ini.ReadString ('Basic','Author','');
  Music :=         ini.ReadString ('Basic','Music','');
  SideId :=        ini.ReadInteger('Basic','SideId',-1);
  MissionNumber := ini.ReadInteger('Basic','MissionNumber',0);
  TextUib :=       ini.ReadString ('Basic','TextUib','');
  // Load briefing
  tmp_strings.Delimiter := '_';
  tmp_strings.DelimitedText := StringReplace(ini.ReadString('Basic','Briefing',''),' ','^',[rfReplaceAll]);
  for i := 0 to tmp_strings.Count-1 do
    tmp_strings[i] := StringReplace(tmp_strings[i],'^',' ',[rfReplaceAll]);
  BriefingMemo.Lines := tmp_strings;
  // Remove additional trailing newline from the Memo
  BriefingMemo.Text := Copy(BriefingMemo.Text, 1, Length(BriefingMemo.Text) - 2);
  // Load Data section
  CampaignFolder := ini.ReadString('Data','CampaignFolder','');
  ModsFolder :=     ini.ReadString('Data','ModsFolder','');
  ColoursFile :=    ini.ReadString('Data','ColoursFile','');
  PlayersFile :=    ini.ReadString('Data','PlayersFile','');
  IntelId :=        ini.ReadString('Data','IntelId','');
  // Load Vars section
  tmp_strings.Clear;
  for i := 0 to Length(rules) - 1 do
    tmp_strings.Add(rules[i].name+'='+ini.ReadString('Vars',rules[i].name,rules[i].default_value));
  RuleValueList.Strings := tmp_strings;
  // Load Text section
  tmp_strings.Clear;
  ini.ReadSection('Text', tmp_strings);
  for i := 0 to tmp_strings.Count-1 do
    tmp_strings[i] := tmp_strings[i]+'='+ini.ReadString('Text',tmp_strings[i],'');
  TextValueList.Strings := tmp_strings;
  // Load Notes section
  for i := 0 to Length(event_notes) - 1 do
    event_notes[i] := ini.ReadString('Notes', 'event'+inttostr(i), '');
  for i := 0 to Length(condition_notes) - 1 do
    condition_notes[i] := ini.ReadString('Notes', 'condition'+inttostr(i), '');
  // Load Variables section
  for i := 0 to Length(variable_names) - 1 do
    variable_names[i] := ini.ReadString('Variables', inttostr(i), '');
  // Loading is done - clean up
  tmp_strings.Destroy;
  ini.Destroy;

  // Do needed actions
  if SideId <> -1 then
    Launcher.MySideID := SideID;
  Launcher.MissionNumber := MissionNumber;
  load_custom_campaign_data_files(false);
  Dispatcher.register_event(evMissionIniLoad);
end;

procedure TMissionIni.save_mission_ini(map_filename: String; is_testmap: boolean);
var
  tmp_filename: string;
  i: integer;
  ini: TMemIniFile;
begin
  tmp_filename := ChangeFileExt(map_filename,'.ini');
  if not mission_ini_assigned then
  begin
    if FileExists(tmp_filename) then
      DeleteFile(tmp_filename);
    exit;
  end;
  if not is_testmap then
    mission_ini_filename := tmp_filename;

  // Save actual data
  ini := TMemIniFile.Create(tmp_filename);
  // Save Basic section
  if is_testmap then
    ini.WriteString('Basic','Name','TESTMAP')
  else
    if Name = ''       then ini.DeleteKey('Basic','Name')          else ini.WriteString ('Basic','Name',Name);
  if Author = ''       then ini.DeleteKey('Basic','Author')        else ini.WriteString ('Basic','Author',Author);
  if Music = ''        then ini.DeleteKey('Basic','Music')         else ini.WriteString ('Basic','Music',Music);
  if SideId = -1       then ini.DeleteKey('Basic','SideId')        else ini.WriteInteger('Basic','SideId',SideId);
  if MissionNumber = 0 then ini.DeleteKey('Basic','MissionNumber') else ini.WriteInteger('Basic','MissionNumber',MissionNumber);
  if TextUib = ''      then ini.DeleteKey('Basic','TextUib')       else ini.WriteString ('Basic','TextUib',TextUib);
  // Save briefing
  if BriefingMemo.Lines.Count = 0 then
    ini.DeleteKey('Basic','Briefing')
  else
    ini.WriteString('Basic','Briefing',StringReplace(BriefingMemo.Text,chr(13)+chr(10),'_',[rfReplaceAll]));
  // Save Data section
  if CampaignFolder = '' then ini.DeleteKey('Data','CampaignFolder') else ini.WriteString('Data','CampaignFolder',CampaignFolder);
  if ModsFolder = ''     then ini.DeleteKey('Data','ModsFolder')     else ini.WriteString('Data','ModsFolder',ModsFolder);
  if ColoursFile = ''    then ini.DeleteKey('Data','ColoursFile')    else ini.WriteString('Data','ColoursFile',ColoursFile);
  if PlayersFile = ''    then ini.DeleteKey('Data','PlayersFile')    else ini.WriteString('Data','PlayersFile',PlayersFile);
  if IntelId = ''        then ini.DeleteKey('Data','IntelId')        else ini.WriteString('Data','IntelId',IntelId);
  // Save Vars section
  ini.EraseSection('Vars');
  for i := 0 to Length(rules) - 1 do
    if RuleValueList.Cells[1,i+1] <> rules[i].default_value then
      ini.WriteString('Vars',rules[i].name,RuleValueList.Cells[1,i+1]);
  // Save Text section
  ini.EraseSection('Text');
  for i := 0 to TextValueList.Strings.Count - 1 do
    ini.WriteString('Text', TextValueList.Cells[0,i+1], TextValueList.Cells[1,i+1]);
  // Save Notes section
  ini.EraseSection('Notes');
  for i := 0 to Length(event_notes) - 1 do
    if event_notes[i] <> '' then
      ini.WriteString('Notes', 'event'+inttostr(i), event_notes[i]);
  for i := 0 to Length(condition_notes) - 1 do
    if condition_notes[i] <> '' then
      ini.WriteString('Notes', 'condition'+inttostr(i), condition_notes[i]);
  // Save Variables section
  for i := 0 to Length(variable_names) - 1 do
    if variable_names[i] <> '' then
      ini.WriteString('Variables', inttostr(i), variable_names[i]);
  // Saving is done
  ini.UpdateFile;
  ini.Destroy;
end;

procedure TMissionIni.assign_mission_ini;
begin
  mission_ini_assigned := true;
  Dispatcher.register_event(evMissionIniLoad);
end;

procedure TMissionIni.unload_mission_ini(load_tilesets: boolean);
begin
  if not mission_ini_assigned then
    exit;
  mission_ini_filename := '';
  mission_ini_assigned := false;
  reset_mission_ini_data;
  // Do needed actions
  load_custom_campaign_data_files(load_tilesets);
  Dispatcher.register_event(evMissionIniLoad);
end;

procedure TMissionIni.reset_mission_ini_data;
var
  i: integer;
begin
  // Clear Basic section
  Name := '';
  Author := '';
  Music := '';
  SideId := -1;
  MissionNumber := 0;
  TextUib := '';
  BriefingMemo.Lines.Clear;
  // Clear Data section
  CampaignFolder := '';
  ModsFolder := '';
  ColoursFile := '';
  PlayersFile := '';
  IntelId := '';
  // Clear Vars section
  reset_rules_to_defaults;
  // Clear Text section
  TextValueList.Strings.Clear;
  // Clear Notes section
  for i := 0 to Length(event_notes) - 1 do
    event_notes[i] := '';
  for i := 0 to Length(condition_notes) - 1 do
    condition_notes[i] := '';
  // Clear variable names
  for i := 0 to Length(variable_names) - 1 do
    variable_names[i] := '';
end;

procedure TMissionIni.reset_rules_to_defaults;
var
  i: integer;
  tmp_strings: TStringList;
begin
  tmp_strings := TStringList.Create;
  for i := 0 to Length(rules) - 1 do
    tmp_strings.Add(rules[i].name+'='+rules[i].default_value);
  RuleValueList.Strings := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TMissionIni.export_events(first_event, last_event: integer; filename: string);
var
  tmp_filename: string;
  i, j: integer;
  ini: TMemIniFile;
  textstring_used: array of boolean;
  variable_used: array[0..255] of boolean;
  text_id: String;
  event: ^TEvent;
  event_type: ^TEventTypeDefinition;
  filter: TObjectFilterPtr;
  cond_expr: TCondExprPtr;
begin
  tmp_filename := ChangeFileExt(filename,'.ini');
  if not mission_ini_assigned then
  begin
    if FileExists(tmp_filename) then
      DeleteFile(tmp_filename);
    exit;
  end;

  // Save actual data
  ini := TMemIniFile.Create(tmp_filename);

  // Find out which text strings are used in selected events
  SetLength(textstring_used, TextValueList.Strings.Count);
  FillChar(textstring_used[0], TextValueList.Strings.Count, 0);
  for i := first_event to last_event do
    if EventConfig.event_types[Mission.event_data[i].event_type].event_data = edMessage then
    begin
      text_id := IntToStr(get_integer_value(Addr(Mission.event_data[i].data), 21, 4));
      for j := 0 to TextValueList.Strings.Count - 1 do
        if TextValueList.Cells[0,j+1] = text_id then
          textstring_used[j] := true;
    end;
  // Save Text section
  ini.EraseSection('Text');
  for i := 0 to TextValueList.Strings.Count - 1 do
    if textstring_used[i] then
      ini.WriteString('Text', TextValueList.Cells[0,i+1], TextValueList.Cells[1,i+1]);

  // Save Notes section
  ini.EraseSection('Notes');
  for i := first_event to last_event do
    if event_notes[i] <> '' then
      ini.WriteString('Notes', 'event'+inttostr(i - first_event), event_notes[i]);

  // Find out which variables are used in selected events
  FillChar(variable_used[0], sizeof(variable_used), 0);
  for i := first_event to last_event do
  begin
    event := Addr(Mission.event_data[i]);
    event_type := Addr(EventConfig.event_types[event.event_type]);
    // Variable-type event coords
    for j := 0 to Length(event.coord_x) - 1 do
    begin
      if (event.coord_var_flags and (1 shl (j * 2))) <> 0 then
        variable_used[event.coord_x[j]] := true;
      if (event.coord_var_flags and (1 shl (j * 2 + 1))) <> 0 then
        variable_used[event.coord_y[j]] := true;
    end;
    // Variable-type event args
    for j := 0 to Length(event_args_struct_members) - 1 do
      if ((event.arg_var_flags and (1 shl j)) <> 0) or ((event_type.args[j].arg_type = atVariable) and evaluate_show_if(Addr(event_type.args[j].show_if), event, Addr(event_args_struct_members[0]))) then
        variable_used[get_integer_struct_member(event, Addr(event_args_struct_members[0]), j)] := true;
    // Variables used in object filter
    if ((Ord(event_type.event_data) >= Ord(edUnitFilter)) and ((event.event_flags and 8) = 0)) or ((event_type.event_data = edCondExpr) and (event.amount > 0)) then
    begin
      filter := Addr(event.data[1]);
      for j := 0 to Length(filter.criteria_value) - 1 do
        if (filter.pos_and_var_flags and (1 shl (j + 8)) <> 0) then
          variable_used[filter.criteria_value[j]] := true;
      for j := 0 to IfThen((filter.pos_and_var_flags and 12) <> 0, 2, 3) do
        if (filter.pos_and_var_flags and (1 shl (j + 4)) <> 0) then
          variable_used[filter.pos_values[j]] := true;
    end;
    // Variables used in conditional expression
    if (event_type.event_data = edCondExpr) and (event.amount = 0) then
    begin
      cond_expr := Addr(event.data[1]);
      for j := 0 to cond_expr.num_operations - 1 do
      begin
        variable_used[cond_expr.variable[j]] := true;
        if (cond_expr.value_var_flags and (1 shl j)) <> 0 then
          variable_used[cond_expr.value[j]] := true;
      end;
    end;
    // Variables used in message
    if (event_type.event_data = edMessage) then
      for j := 0 to 7 do
        if event.data[5 + j] > 0 then
          variable_used[event.data[13 + j]] := true;
    // Variable used in object index or filter skip
    if (event.event_flags and 24) <> 0 then
       variable_used[event.filter_skip] := true;
    if ((event.event_flags and 32) <> 0) and ((event.event_flags and 8) = 0) then
       variable_used[event.data[0]] := true;
  end;
  // Save Variables section
  ini.EraseSection('Variables');
  for i := 0 to Length(variable_names) - 1 do
    if variable_used[i] and (variable_names[i] <> '') then
      ini.WriteString('Variables', inttostr(i), variable_names[i]);

  // Saving is done
  ini.UpdateFile;
  ini.Destroy;
end;

procedure TMissionIni.import_events(first_event: integer; filename: string);
var
  tmp_filename: string;
  i: integer;
  tmp_strings: TStringList;
  ini: TMemIniFile;
  num: integer;
  old_str, new_str: string;
begin
  tmp_filename := ChangeFileExt(filename,'.ini');
  if not FileExists(tmp_filename) then
    exit;
  if not mission_ini_assigned then
    exit;

  // Load actual data
  ini := TMemIniFile.Create(tmp_filename);
  tmp_strings := TStringList.Create;
  // Load Text section
  ini.ReadSection('Text', tmp_strings);
  for i := 0 to tmp_strings.Count-1 do
  begin
    num := TextValueList.Strings.IndexOfName(tmp_strings[i]);
    new_str := ini.ReadString('Text',tmp_strings[i],'');
    if num = -1 then
      TextValueList.Strings.Add(tmp_strings[i]+'='+ini.ReadString('Text',tmp_strings[i],''))
    else
    begin
      old_str := TextValueList.Strings.Values[tmp_strings[i]];
      if (old_str <> new_str) and (Application.MessageBox(PChar(Format('Conflict when importing text string %s.'#13'Current text: %s'#13'Imported text: %s'#13'Replace text?', [tmp_strings[i], old_str, new_str])), 'Import events', MB_YESNO or MB_ICONWARNING) = ID_YES) then
        TextValueList.Strings.Strings[num] := tmp_strings[i]+'='+ini.ReadString('Text',tmp_strings[i],'');
    end;
  end;
  // Load Notes section
  tmp_strings.Clear;
  ini.ReadSection('Notes', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    num := StrToInt(Copy(tmp_strings[i], 6, Length(tmp_strings[i]) - 5)) + first_event;
    if num < Length(event_notes) then
      event_notes[num] := ini.ReadString('Notes', tmp_strings[i], '');
  end;
  // Load Variables section
  tmp_strings.Clear;
  ini.ReadSection('Variables', tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    num := StrToInt(tmp_strings[i]);
    old_str := variable_names[num];
    new_str := ini.ReadString('Variables', tmp_strings[i], '');
    if (old_str <> '') and (old_str <> new_str) and (Application.MessageBox(PChar(Format('Conflict when importing name of variable %s.'#13'Current name: %s'#13'Imported name: %s'#13'Replace name?', [tmp_strings[i], old_str, new_str])), 'Import events', MB_YESNO or MB_ICONWARNING) = ID_NO) then
      continue;
    variable_names[num] := new_str;
  end;
  // Loading is done - clean up
  tmp_strings.Destroy;
  ini.Destroy;

  // Do needed actions
  Dispatcher.register_event(evMissionIniLoad);
end;

function TMissionIni.get_custom_text(index: integer; var text: String): boolean;
var
  row: integer;
begin
  result := false;
  if (TextValueList <> nil) and TextValueList.FindRow(inttostr(index), row) then
  begin
    text := TextValueList.Cells[1,row];
    result := true;
  end;
end;

procedure TMissionIni.set_custom_text(index: integer; text: String);
begin
  TextValueList.Values[inttostr(index)] := text;
end;

procedure TMissionIni.remove_custom_text(index: integer);
var
  row: integer;
begin
   if TextValueList.FindRow(inttostr(index), row) then
     TextValueList.DeleteRow(row);
end;

function TMissionIni.get_variable_name(index: integer; brackets: integer): string;
var
  i: integer;
begin
  if index >= MAX_VARIABLES then
    result := ''
  else if variable_names[index] = '' then
    result := 'Var' + IntToStr(index)
  else
    result := variable_names[index];
  for i := 1 to brackets do
    result := '[' + result + ']';
end;

function TMissionIni.set_variable_name(index: integer; name: string): boolean;
begin
  result := false;
  if index < MAX_VARIABLES then
  begin
    result := variable_names[index] <> name;
    variable_names[index] := name;
  end;
end;

procedure TMissionIni.set_side_id(value: integer);
begin
  SideId := value;
  Launcher.MySideID := SideId;
end;

procedure TMissionIni.set_mission_number(value: integer);
begin
  MissionNumber := value;
  Launcher.MissionNumber := MissionNumber;
end;

procedure TMissionIni.set_campaign_folder(value: string);
begin
  CampaignFolder := value;
  load_custom_campaign_data_files(true);
end;

procedure TMissionIni.set_mods_folder(value: string);
begin
  ModsFolder := value;
  load_custom_campaign_data_files(true);
end;

procedure TMissionIni.set_colours_file(value: string);
begin
  ColoursFile := value;
  StructGraphics.load_colours_bin;
end;

procedure TMissionIni.set_players_file(value: string);
begin
  PlayersFile := value;
  Structures.load_sides_ini;
end;

procedure TMissionIni.set_text_uib(value: string);
begin
  TextUib := value;
  StringTable.load_text_uib(TextUib);
end;

procedure TMissionIni.load_custom_campaign_data_files(load_tilesets: boolean);
begin
  if load_tilesets then
    Tileset.load_tileset(false);
  Structures.load_templates_bin(false);
  Structures.load_builexp_bin(false);
  Structures.load_armour_bin(false);
  Structures.load_speed_bin(false);
  Structures.load_techpos_bin(false);
  Structures.load_tiledata_bin;
  Structures.load_misc_objects_ini;
  Structures.load_sides_ini;
  Structures.load_limits_ini;
  Structures.load_group_ids;
  StructGraphics.load_colours_bin;
  StructGraphics.load_data_r16(false);
  StructGraphics.load_graphics_misc_objects;
  Sounds.load_sound_rs(false);
  StringTable.load_text_uib(TextUib);
  StringTable.load_samples_uib(false);
end;

end.
