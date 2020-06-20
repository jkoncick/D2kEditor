unit _launcher;

interface

uses
  Classes;

type
  TMissionInfo = record
    filename: String;
    mission_name: String;
    author: String;
    briefing: String;
    text_uib: String;
    mission_number: integer;
    side_id: integer;
  end;

type
  TLauncher = class

  public
    // Mission data
    missions_loaded: boolean;
    mission_list: TStringList;
    mission_data: Array of TMissionInfo;

    // Test map settings
    MySideID: integer;
    MissionNumber: integer;
    DifficultyLevel: integer;
    Seed: integer;
    TextUib: String;
    TestMapParameters: String;

  public
    procedure load_all_missions;
    procedure launch_mission(mission_index: integer; difficulty_level: integer);
    procedure get_map_test_settings(map_filename: String);
    procedure launch_current_mission;

  end;

var
  Launcher: TLauncher;

implementation

uses
  _settings, Windows, SysUtils, IniFiles, ShellApi, mission_dialog, main;

{ TLauncher }

procedure TLauncher.load_all_missions;
var
  tmp_strings: TStringList;
  ini: TMemIniFile;
  SR: TSearchRec;
  i: integer;
begin
  if missions_loaded then
    exit;
  mission_list := TStringList.Create;
  tmp_strings := TStringList.Create;
  if FindFirst(Settings.MissionsPath + '\*.INI', 0, SR) = 0 then
  begin
    repeat
      tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  SetLength(Mission_data, tmp_strings.Count);
  mission_list.Capacity := tmp_strings.Count;
  for i := 0 to tmp_strings.Count - 1 do
  begin
    ini := TMemIniFile.Create(Settings.MissionsPath + '\' + tmp_strings[i]);
    mission_data[i].filename := ChangeFileExt(tmp_strings[i],'');
    mission_data[i].mission_name := ini.ReadString('Basic', 'Name', mission_data[i].filename);
    mission_data[i].author := ini.ReadString('Basic', 'Author', '(Unnamed)');
    mission_data[i].briefing := ini.ReadString('Basic', 'Briefing', '');
    mission_data[i].text_uib := ini.ReadString('Basic', 'TextUib', '');
    mission_data[i].mission_number := ini.ReadInteger('Basic', 'MissionNumber', 0);
    mission_data[i].side_id := ini.ReadInteger('Basic', 'SideId', 0);
    mission_list.Add(mission_data[i].mission_name + '=' + inttostr(i));
    ini.Destroy;
  end;
  mission_list.Sort;
  tmp_strings.Destroy;
  missions_loaded := true;
end;

procedure TLauncher.launch_mission(mission_index, difficulty_level: integer);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Settings.GamePath + '\spawn.ini');
  ini.WriteString('Settings', 'Scenario', mission_data[mission_index].filename);
  ini.WriteInteger('Settings', 'MySideID', mission_data[mission_index].side_id);
  ini.WriteInteger('Settings', 'MissionNumber', mission_data[mission_index].mission_number);
  ini.WriteInteger('Settings', 'DifficultyLevel', difficulty_level);
  ini.WriteInteger('Settings', 'Seed', Seed);
  if mission_data[mission_index].text_uib <> '' then
    ini.WriteString('Settings', 'TextUib', mission_data[mission_index].text_uib)
  else
    ini.DeleteKey('Settings', 'TextUib');
  ini.Destroy;
  ShellExecuteA(0, 'open', PChar(Settings.GameExecutable), PChar('-SPAWN ' + TestMapParameters), PChar(Settings.GamePath), SW_SHOWNORMAL);
end;

procedure TLauncher.get_map_test_settings(map_filename: String);
var
  ini: TIniFile;
  map_name: String;
  house, mission: char;
  house_num, mission_num: integer;
begin
  ini := TIniFile.Create(Settings.GamePath + '\spawn.ini');
  // Try to detect MissionNumber and MySideID from map file name
  if map_filename <> '' then
  begin
    map_name := ChangeFileExt(ExtractFileName(map_filename),'');
    house := map_name[Length(map_name)-3];
    mission := map_name[Length(map_name)-2];
  end else
  begin
    house := 'X';
    mission := 'X';
  end;
  house_num := ini.ReadInteger('Settings', 'MySideID', 0);
  case house of
  'A': house_num := 0;
  'H': house_num := 1;
  'O': house_num := 2;
  end;
  if (ord(mission) >= ord('1')) and (ord(mission) <= ord('9')) then
    mission_num := strtoint(mission)
  else
    mission_num := ini.ReadInteger('Settings', 'MissionNumber', 1);
  // Fill test map settings
  MySideID := house_num;
  MissionNumber := mission_num;
  DifficultyLevel := ini.ReadInteger('Settings', 'DifficultyLevel', 1);
  Seed := ini.ReadInteger('Settings', 'Seed', random(2000000000));
  TextUib := ini.ReadString('Settings', 'TextUib', '');
  ini.Destroy;
end;

procedure TLauncher.launch_current_mission;
var
  ini: TIniFile;
  temp_map_name: String;
begin
  if random(9001) = 1337 then
  begin
    ShellExecute(0, 'open', PChar('https://www.youtube.com/watch?v=oHg5SJYRHA0'), nil, nil, SW_SHOWNORMAL);
    exit;
  end;
  // Write test map settings to ini file
  ini := TIniFile.Create(Settings.GamePath + '\spawn.ini');
  ini.WriteString('Settings', 'Scenario', 'TESTMAP');
  ini.WriteInteger('Settings', 'MySideID', MySideID);
  ini.WriteInteger('Settings', 'MissionNumber', MissionNumber);
  ini.WriteInteger('Settings', 'DifficultyLevel', DifficultyLevel);
  ini.WriteInteger('Settings', 'Seed', Seed);
  if TextUib <> '' then
    ini.WriteString('Settings', 'TextUib', TextUib)
  else
    ini.DeleteKey('Settings', 'TextUib');
  ini.Destroy;
  // Save current mission as TESTMAP.MAP
  temp_map_name := Missiondialog.edMapName.Text;
  Missiondialog.edMapName.Text := 'TESTMAP';
  MainWindow.save_map(Settings.MissionsPath + '\TESTMAP.MAP');
  Missiondialog.edMapName.Text := temp_map_name;
  if not MissionDialog.cbUseINI.Checked then
    DeleteFile(Settings.MissionsPath + '\TESTMAP.INI');
  // Launch game
  ShellExecuteA(0, 'open', PChar(Settings.GameExecutable), PChar('-SPAWN ' + TestMapParameters), PChar(Settings.GamePath), SW_SHOWNORMAL);
end;

end.
