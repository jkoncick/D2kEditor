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
    Seed: integer;
    TextUib: String;
    TestMapParameters: String;

    // Game is running
    game_running: boolean;

  public
    procedure load_all_missions;
    procedure launch_mission(mission_index: integer; difficulty_level: integer);
    procedure get_map_test_settings(map_filename: String);
    procedure launch_current_mission;
    procedure launch_game;
    function check_game_is_running: boolean;
    procedure restore_backup_files_from_last_run;

  private
    function get_journal_path: string;
    function replace_files_from_mods_folder(campaign_folder, mods_folder, colours_file: string): boolean;
    procedure execute_game_and_wait(testmap: boolean);
    procedure restore_backup_files;
  end;

var
  Launcher: TLauncher;

implementation

uses
  Windows, SysUtils, StrUtils, IniFiles, ShellApi, Dialogs, Forms, _settings, _map, _missionini, _dispatcher;

{ TLauncher }

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
  files_replaced: boolean;
  mods_folder: string;
begin
  minfo := Addr(mission_data[mission_index]);
  mods_folder := Settings.GamePath + '\CustomCampaignData\' + minfo.campaign_folder + '\' + minfo.mods_folder + '\';
  if (minfo.campaign_folder <> '') and (minfo.mods_folder <> '') and not DirectoryExists(mods_folder) then
  begin
    Application.MessageBox(PChar('The mods folder ' + mods_folder + ' does not exist!'), 'Launch mission', MB_ICONERROR or MB_OK);
    exit;
  end;
  // Write test map settings to ini file
  ini := TIniFile.Create(Settings.GamePath + '\spawn.ini');
  ini.WriteString('Settings', 'Scenario', minfo.filename);
  ini.WriteInteger('Settings', 'MySideID', minfo.side_id);
  ini.WriteInteger('Settings', 'MissionNumber', minfo.mission_number);
  ini.WriteInteger('Settings', 'DifficultyLevel', difficulty_level);
  ini.WriteInteger('Settings', 'Seed', Seed);
  if mission_data[mission_index].text_uib <> '' then
    ini.WriteString('Settings', 'TextUib', minfo.text_uib)
  else
    ini.DeleteKey('Settings', 'TextUib');
  ini.Destroy;
  // Launch game
  files_replaced := replace_files_from_mods_folder(minfo.campaign_folder, minfo.mods_folder, minfo.colours_file);
  execute_game_and_wait(true);
  if files_replaced then
    restore_backup_files;
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
  files_replaced: boolean;
begin
  if random(9001) = 1337 then
  begin
    ShellExecute(0, 'open', PChar('https://www.youtube.com/watch?v=oHg5SJYRHA0'), nil, nil, SW_SHOWNORMAL);
    exit;
  end;
  // Write test map settings to ini file
  ini := TIniFile.Create(Settings.GamePath + '\spawn.ini');
  ini.WriteString ('Settings', 'Scenario', 'TESTMAP');
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
  Map.save_map(Settings.MissionsPath + '\TESTMAP.MAP', true);
  // Launch game
  files_replaced := replace_files_from_mods_folder(MissionIni.CampaignFolder, MissionIni.ModsFolder, MissionIni.ColoursFile);
  execute_game_and_wait(true);
  if files_replaced then
    restore_backup_files;
end;

procedure TLauncher.launch_game;
var
  files_replaced: boolean;
begin
  files_replaced := replace_files_from_mods_folder(MissionIni.CampaignFolder, MissionIni.ModsFolder, MissionIni.ColoursFile);
  execute_game_and_wait(false);
  if files_replaced then
    restore_backup_files;
end;

function TLauncher.check_game_is_running: boolean;
begin
  result := game_running;
  if game_running then
    Application.MessageBox('Cannot do this operation while game is still running.', 'Game is running', MB_ICONERROR or MB_OK);
end;

procedure TLauncher.restore_backup_files_from_last_run;
begin
  if not FileExists(get_journal_path) then
    exit;
  Application.MessageBox('The original game files were not restored from backup during last run. They will be restored now.', 'Restore backup files', MB_ICONINFORMATION or MB_OK);
  restore_backup_files;
end;

function TLauncher.get_journal_path: string;
begin
  result := Settings.GamePath + '\journal.ini';
end;

function TLauncher.replace_files_from_mods_folder(campaign_folder, mods_folder, colours_file: string): boolean;
var
  source_folder: string;
  SearchRec: TSearchRec;
  folders: TStringList;
  folder: string;
  i: integer;
  source_files: array of TSourceFileInfo;
  source_files_count: integer;
  source_file, target_file, backup_file: string;
  journal_path: string;
  journal: TMemIniFile;
  colours_bin_file: string;
begin
  result := false;
  if (campaign_folder = '') or (mods_folder = '') then
    exit;
  source_folder := Settings.GamePath + '\CustomCampaignData\' + campaign_folder + '\' + mods_folder + '\';
  if not DirectoryExists(source_folder) then
    exit;
  // Search recursively through folder structure in the mod folder and find all files
  folders := TStringList.Create;
  folders.Add('');
  i := 0;
  source_files_count := 0;
  while (i < folders.Count) do
  begin
    folder := folders[i];
    inc(i);
    // Collect child folders first
    if (FindFirst(source_folder + folder + '*', faDirectory, SearchRec) = 0) then
    begin
      repeat
        if ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          folders.Add(IncludeTrailingPathDelimiter(folder + SearchRec.Name));
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);
    end;
    // Collect files
    if (FindFirst(source_folder + folder + '*', faAnyFile - faDirectory, SearchRec) = 0) then
    begin
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          if source_files_count = Length(source_files) then
            SetLength(source_files, Length(source_files) + 16);
          source_files[source_files_count].name := folder + SearchRec.Name;
          source_files[source_files_count].size := SearchRec.Size;
          source_files[source_files_count].time := SearchRec.Time;
          inc(source_files_count);
        end;
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);
    end;
  end;
  folders.Destroy;
  // Process found source files
  journal_path := get_journal_path;
  if FileExists(journal_path) then
    DeleteFile(journal_path);
  journal := TMemIniFile.Create(journal_path);
  for i := 0 to source_files_count - 1 do
  begin
    source_file := source_folder + source_files[i].name;
    target_file := Settings.GamePath + '\' + source_files[i].name;
    backup_file := Settings.GamePath + '\BackupData\' + source_files[i].name;
    if (FindFirst(target_file, faAnyFile - faDirectory, SearchRec) = 0) then
    begin
      // Target file exists
      if (source_files[i].size = SearchRec.Size) and (source_files[i].time = SearchRec.Time) then
      begin
        // Target file is same as source file
        journal.WriteString('journal', source_files[i].name, 'Same');
      end else
      begin
        // Target file is different
        journal.WriteString('journal', source_files[i].name, 'Different');
        if not DirectoryExists(ExtractFileDir(backup_file)) then
          ForceDirectories(ExtractFileDir(backup_file));
        MoveFile(PChar(target_file), PChar(backup_file));
        CopyFile(PChar(source_file), PChar(target_file), false);
      end;
      FindClose(SearchRec);
    end else
    begin
      // Target file does not exist
      journal.WriteString('journal', source_files[i].name, 'Notexists');
      if not DirectoryExists(ExtractFileDir(target_file)) then
        ForceDirectories(ExtractFileDir(target_file));
      CopyFile(PChar(source_file), PChar(target_file), false);
    end;
  end;
  // Deal with COLOURS.BIN
  colours_bin_file := Settings.GamePath + '\CustomCampaignData\' + campaign_folder + '\Colours\' + colours_file;
  if FileExists(colours_bin_file) then
  begin
    journal.WriteString('journal', 'data\bin\COLOURS.BIN', 'Different');
    source_file := colours_bin_file;
    target_file := Settings.GamePath + '\' + 'data\bin\COLOURS.BIN';
    backup_file := Settings.GamePath + '\BackupData\' + 'data\bin\COLOURS.BIN';
    MoveFile(PChar(target_file), PChar(backup_file));
    CopyFile(PChar(source_file), PChar(target_file), false);
  end;
  // Write journal file
  journal.UpdateFile;
  journal.Destroy;
  if Settings.Debug_ShowReplaceFilesFromModsFolderLog then
    ShellExecute(0, 'open', PChar(journal_path), nil, nil, SW_SHOWNORMAL);
  SetLength(source_files, 0);
  result := true;
end;

procedure TLauncher.execute_game_and_wait(testmap: boolean);
var
  tmpStartupInfo: TStartupInfo;
  tmpProcessInformation: TProcessInformation;
begin
  FillChar(tmpStartupInfo, SizeOf(tmpStartupInfo), 0);
  tmpStartupInfo.cb := SizeOf(TStartupInfo);
  if CreateProcess(nil, PChar('"' + Settings.GameExecutable + '" ' + IfThen(testmap, '-SPAWN ', '') + TestMapParameters), nil, nil, true, 0,
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

procedure TLauncher.restore_backup_files;
var
  journal_path: string;
  journal: TMemIniFile;
  files: TStringList;
  i: integer;
  original_file, backup_file: string;
begin
  journal_path := get_journal_path;
  journal := TMemIniFile.Create(journal_path);
  files := TStringList.Create;
  journal.ReadSection('journal', files);
  for i := 0 to files.Count - 1 do
  begin
    original_file := Settings.GamePath + '\' + files[i];
    if journal.ReadString('journal', files[i], '') = 'Different' then
    begin
      backup_file := Settings.GamePath + '\BackupData\' + files[i];
      DeleteFile(original_file);
      MoveFile(PChar(backup_file), PChar(original_file));
    end else
    if journal.ReadString('journal', files[i], '') = 'Notexists' then
    begin
      if Settings.CleanUpExtraModFilesAfterLaunch then
        DeleteFile(original_file);
    end;
  end;
  journal.Destroy;
  files.Destroy;
  DeleteFile(journal_path);
end;

end.
