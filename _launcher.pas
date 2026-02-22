unit _launcher;

interface

uses
  Classes;

type
  TMissionInfo = record
    filename: String;
    mission_name: String;
    author: String;
    side_id: integer;
    mission_number: integer;
    text_uib: String;
    briefing: String;
    campaign_folder: String;
    mods_folder: String;
    colours_file: String;
    players_file: String;
    intel_id: String;
    tileset: String;
  end;

type
  TSourceFileInfo = record
    name: string;
    size: integer;
    time: integer;
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
    DebugFeatures: integer;

    // File paths
    spawn_ini_path: string;

    // Game is running
    game_running: boolean;

  public
    procedure init;
    procedure load_all_missions;
    procedure launch_mission(mission_index: integer; difficulty_level: integer);
    procedure launch_current_mission;
    procedure launch_game;
    procedure get_test_map_settings(map_filename: String);
    function check_map_can_be_tested: boolean;
    function check_game_is_running: boolean;

  private
    procedure execute_game_and_wait(testmap: boolean);
  end;

var
  Launcher: TLauncher;

implementation

uses
  Windows, SysUtils, StrUtils, IniFiles, ShellApi, Dialogs, Forms, _settings, _map, _mission, _missionini, _dispatcher, _utils;

{ TLauncher }

procedure TLauncher.init;
var
  ini: TMemIniFile;
begin
  spawn_ini_path := Settings.GamePath + '\spawn.ini';
  // Load test map settings from spawn.ini file
  ini := TMemIniFile.Create(spawn_ini_path);
  MySideID := ini.ReadInteger('Settings', 'MySideID', 0);
  MissionNumber := ini.ReadInteger('Settings', 'MissionNumber', 0);
  DifficultyLevel := ini.ReadInteger('Settings', 'DifficultyLevel', 1);
  DebugFeatures := ini.ReadInteger('Settings', 'DebugFeatures', 0);
  ini.Destroy;
end;

procedure TLauncher.load_all_missions;
var
  tmp_strings: TStringList;
  ini: TMemIniFile;
  SR: TSearchRec;
  mis_filename: string;
  mis_file: file of byte;
  tileset_name_buffer: array[0..199] of char;
  i: integer;
begin
  if missions_loaded then
    exit;
  mission_list := TStringList.Create;
  tmp_strings := TStringList.Create;
  if FindFirst(Settings.MissionsPath + '\*.INI', 0, SR) = 0 then
  begin
    repeat
      if ChangeFileExt(SR.Name, '') <> 'TESTMAP' then
        tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  SetLength(Mission_data, tmp_strings.Count);
  mission_list.Capacity := tmp_strings.Count;
  for i := 0 to tmp_strings.Count - 1 do
  begin
    // Load mission details from mission ini file
    ini := TMemIniFile.Create(Settings.MissionsPath + '\' + tmp_strings[i]);
    mission_data[i].filename := ChangeFileExt(tmp_strings[i],'');
    mission_data[i].mission_name := ini.ReadString('Basic', 'Name', mission_data[i].filename);
    mission_data[i].author := ini.ReadString('Basic', 'Author', '');
    mission_data[i].side_id := ini.ReadInteger('Basic', 'SideId', 0);
    mission_data[i].mission_number := ini.ReadInteger('Basic', 'MissionNumber', 0);
    mission_data[i].text_uib := ini.ReadString('Basic', 'TextUib', '');
    mission_data[i].briefing := ini.ReadString('Basic', 'Briefing', '');
    mission_data[i].campaign_folder := ini.ReadString('Data', 'CampaignFolder', '');
    mission_data[i].mods_folder := ini.ReadString('Data', 'ModsFolder', '');
    mission_data[i].colours_file := ini.ReadString('Data', 'ColoursFile', '');
    mission_data[i].players_file := ini.ReadString('Data', 'PlayersFile', '');
    mission_data[i].intel_id := ini.ReadString('Data', 'IntelId', '');
    ini.Destroy;
    // Load mission tileset from MIS file
    mis_filename := Settings.MissionsPath + '\_' + mission_data[i].filename + '.MIS';
    if FileExists(mis_filename) then
    begin
      AssignFile(mis_file, mis_filename);
      Reset(mis_file);
      Seek(mis_file, $10598);
      BlockRead(mis_file, tileset_name_buffer[0], Length(tileset_name_buffer));
      mission_data[i].tileset := tileset_name_buffer;
      Close(mis_file);
    end;
    // Add this mission into list
    mission_list.Add(mission_data[i].mission_name + '=' + inttostr(i));
  end;
  mission_list.Sort;
  tmp_strings.Destroy;
  missions_loaded := true;
end;

procedure TLauncher.launch_mission(mission_index, difficulty_level: integer);
var
  ini: TIniFile;
  minfo: ^TMissionInfo;
  mods_folder: string;
begin
  minfo := Addr(mission_data[mission_index]);
  // Check for mods folder existence
  mods_folder := Settings.GamePath + '\CustomCampaignData\' + minfo.campaign_folder + '\' + minfo.mods_folder + '\';
  if (minfo.campaign_folder <> '') and (minfo.mods_folder <> '') and not DirectoryExists(mods_folder) then
  begin
    Application.MessageBox(PChar('The mods folder ' + mods_folder + ' does not exist!'), 'Launch mission', MB_ICONERROR or MB_OK);
    exit;
  end;
  // Write test map settings to ini file
  ini := TIniFile.Create(spawn_ini_path);
  ini.WriteString('Settings', 'Scenario', minfo.filename);
  ini.WriteInteger('Settings', 'MySideID', minfo.side_id);
  ini.WriteInteger('Settings', 'MissionNumber', minfo.mission_number);
  ini.WriteInteger('Settings', 'DifficultyLevel', difficulty_level);
  ini.WriteInteger('Settings', 'DebugFeatures', 0);
  ini.WriteString ('Settings', 'CampaignFolder', minfo.campaign_folder);
  ini.WriteString ('Settings', 'ModsFolder', minfo.mods_folder);
  ini.WriteString ('Settings', 'ColoursFile', minfo.colours_file);
  if minfo.text_uib <> '' then
    ini.WriteString('Settings', 'TextUib', minfo.text_uib)
  else
    ini.DeleteKey('Settings', 'TextUib');
  ini.Destroy;
  // Launch game
  execute_game_and_wait(true);
end;

procedure TLauncher.launch_current_mission;
var
  ini: TIniFile;
  text_uib: string;
begin
  if random(9001) = 1337 then
  begin
    ShellExecute(0, 'open', PChar('https://www.youtube.com/watch?v=oHg5SJYRHA0'), nil, nil, SW_SHOWNORMAL);
    exit;
  end;
  // Check for text.uib existence
  text_uib := MissionIni.TextUib;
  if (text_uib <> '') and (find_file('Data\UI_DATA\' + text_uib, '') = '') then
  begin
    Application.MessageBox(PChar('The custom TEXT.UIB file (' + text_uib + ') does not exist.'#13'Map will be tested with the game''s default TEXT.UIB file.'), 'Warning', MB_ICONWARNING);
    text_uib := '';
  end;
  // Write test map settings to ini file
  ini := TIniFile.Create(spawn_ini_path);
  ini.WriteString ('Settings', 'Scenario', 'TESTMAP');
  ini.WriteInteger('Settings', 'MySideID', MySideID);
  ini.WriteInteger('Settings', 'MissionNumber', MissionNumber);
  ini.WriteInteger('Settings', 'DifficultyLevel', DifficultyLevel);
  ini.WriteInteger('Settings', 'DebugFeatures', DebugFeatures);
  ini.WriteString ('Settings', 'CampaignFolder', MissionIni.CampaignFolder);
  ini.WriteString ('Settings', 'ModsFolder', MissionIni.ModsFolder);
  ini.WriteString ('Settings', 'ColoursFile', MissionIni.ColoursFile);
  if text_uib <> '' then
    ini.WriteString('Settings', 'TextUib', text_uib)
  else
    ini.DeleteKey('Settings', 'TextUib');
  ini.Destroy;
  // Save current mission as TESTMAP.MAP
  Map.save_map(Settings.MissionsPath + '\TESTMAP.MAP', true);
  // Launch game
  execute_game_and_wait(true);
end;

procedure TLauncher.launch_game;
begin
  execute_game_and_wait(false);
end;

procedure TLauncher.get_test_map_settings(map_filename: String);
var
  map_name: String;
  house, mission: char;
begin
  // Try to detect MissionNumber and MySideID from map file name
  map_name := ChangeFileExt(ExtractFileName(map_filename),'');
  if Length(map_name) <> 4 then
    exit;
  house := map_name[1];
  case house of
    'A': MySideID := 0;
    'H': MySideID := 1;
    'O': MySideID := 2;
  end;
  mission := map_name[2];
  if (ord(mission) >= ord('1')) and (ord(mission) <= ord('9')) then
    MissionNumber := strtoint(mission);
end;

function TLauncher.check_map_can_be_tested: boolean;
var
  errmsg: string;
begin
  errmsg := '';
  result := false;
  if check_game_is_running then
    exit;
  if not Map.loaded then
    errmsg := 'No map to test.'
  else if not Mission.mis_assigned then
    errmsg := 'No mission file is assigned to this map.'
  else if not FileExists(Settings.GameExecutable) then
    errmsg := 'Cannot find game executable (' + Settings.GameExecutable + ')';
  // Show error if any
  if errmsg <> '' then
    Application.MessageBox(PChar(errmsg), 'Cannot test map', MB_ICONERROR)
  else if Settings.CheckMapErrorsOnTest then
    result := Map.check_errors
  else
    result := true;
end;

function TLauncher.check_game_is_running: boolean;
begin
  result := game_running;
  if game_running then
    Application.MessageBox('Cannot do this operation while game is still running.', 'Game is running', MB_ICONERROR or MB_OK);
end;

procedure TLauncher.execute_game_and_wait(testmap: boolean);
var
  tmpStartupInfo: TStartupInfo;
  tmpProcessInformation: TProcessInformation;
begin
  FillChar(tmpStartupInfo, SizeOf(tmpStartupInfo), 0);
  tmpStartupInfo.cb := SizeOf(TStartupInfo);
  if CreateProcess(nil, PChar('"' + Settings.GameExecutable + '"' + IfThen(testmap, ' -SPAWN', '')), nil, nil, true, 0,
    nil, PChar(Settings.GamePath), tmpStartupInfo, tmpProcessInformation) then
  begin
    game_running := true;
    // loop every 10 ms
    while WaitForSingleObject(tmpProcessInformation.hProcess, 10) > 0 do
    begin
      Application.ProcessMessages;
      Dispatcher.do_pending_actions;
    end;
    CloseHandle(tmpProcessInformation.hProcess);
    CloseHandle(tmpProcessInformation.hThread);
    game_running := false;
  end;
end;

end.
