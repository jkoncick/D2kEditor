unit _missionini;

interface

uses
  StdCtrls, ValEdit, IniFiles, Classes;

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
    event_notes: array[0..63] of String;
    condition_notes: array[0..47] of String;

  public
    procedure init;
    procedure load_rules_ini;
    procedure init_controls(briefing_memo: TMemo; rule_value_list, text_value_list: TValueListEditor);
    // Loading and saving procedures
    function get_mission_ini_filename(map_filename: String): String;
    procedure load_mission_ini(map_filename: String);
    procedure save_mission_ini(map_filename: String; is_testmap: boolean);
    procedure assign_mission_ini;
    procedure unload_mission_ini(load_tilesets: boolean);
    procedure reset_mission_ini_data;
    procedure reset_rules_to_defaults;
    // Custom text related procedures
    function get_custom_text(index: integer; var text: String): boolean;
    procedure set_custom_text(index: integer; text: String);
    procedure remove_custom_text(index: integer);
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

uses SysUtils, StrUtils, _utils, _dispatcher, _tileset, _structures, _graphics, _sounds, _stringtable, _launcher;

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

function TMissionIni.get_mission_ini_filename(map_filename: String): String;
begin
  result := ChangeFileExt(map_filename,'.ini');
end;

procedure TMissionIni.load_mission_ini(map_filename: String);
var
  tmp_filename: string;
  i: integer;
  tmp_strings: TStringList;
  ini: TMemIniFile;
begin
  tmp_filename := get_mission_ini_filename(map_filename);
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
  tmp_filename := get_mission_ini_filename(map_filename);
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
  Structures.load_players_ini;
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
  Structures.load_players_ini;
  Structures.load_limits_ini;
  Structures.load_group_ids_txt;
  StructGraphics.load_colours_bin;
  StructGraphics.load_data_r16(false);
  StructGraphics.load_graphics_misc_objects;
  Sounds.load_sound_rs(false);
  StringTable.load_text_uib(TextUib);
  StringTable.load_samples_uib(false);
end;

end.
