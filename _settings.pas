unit _settings;

interface

uses
  IniFiles, Types, Forms;

const cnt_recent_files = 9;

type
  TSettings = class

  public
    // Preferences
    PreserveGUISettings: boolean;
    RestrictSpiceToSand: boolean;
    EnableEventNotes: boolean;
    HidePresetWindow: boolean;
    CheckMapErrorsOnSave: boolean;
    CheckMapErrorsOnTest: boolean;
    AlwaysAskOnQuit: boolean;
    DrawBuildingMarker: boolean;
    DrawObjectBrush: boolean;
    DrawPaintBrush: boolean;
    LoadR16Image: boolean;
    LoadR8Image: boolean;
    UseAllocationIndexes: boolean;
    ShowEventMarkers: boolean;
    MarkDefenceAreas: boolean;
    GridColor: Cardinal;

    // Default values
    DefaultMapWidth: integer;
    DefaultMapHeight: integer;
    DefaultMisTechLevel: integer;
    DefaultMisStartingMoney: integer;
    DefaultTileset: integer;
    AssignMisFileToNewMap: boolean;
    PreplaceWormSpawner: boolean;

    // File paths
    GameExecutable: String;
    GamePath: String;
    MissionsPath: String;
    TextUIBPath: String;

    // Recent files
    RecentFiles: Array[1..cnt_recent_files] of String;

    // Test map settings
    MySideID: integer;
    MissionNumber: integer;
    DifficultyLevel: integer;
    Seed: integer;
    TextUib: String;
    TestMapParameters: String;

  private
    tmp_ini: TMemIniFile;

  public
    procedure load_precreate_editor_settings;
    procedure load_postcreate_editor_settings;
    procedure save_editor_settings;
    procedure determine_game_paths_from_path(path: String);
    procedure load_window_position(ini: TMemIniFile; window: TForm; window_name: String);
    procedure save_window_position(ini: TMemIniFile; window: TForm; window_name: String);
    procedure update_recent_files(filename: String);
    procedure get_map_test_settings;
    procedure save_map_test_settings;

  end;

var
  Settings: TSettings;

implementation

uses
  SysUtils, main, tileset_dialog, block_preset_dialog, set_dialog, test_map_dialog,
  mission_dialog, event_dialog, map_stats_dialog, _map, _stringtable;

procedure TSettings.load_precreate_editor_settings;
var
  ini: TMemIniFile;
  i: integer;
begin
  ini := TMemIniFile.Create(current_dir + 'D2kEditor.ini');
  tmp_ini := ini;
  // Load preferences
  PreserveGUISettings := ini.ReadBool('Preferences', 'PreserveGUISettings', true);
  RestrictSpiceToSand := ini.ReadBool('Preferences', 'RestrictSpiceToSand', true);
  EnableEventNotes := ini.ReadBool('Preferences', 'EnableEventNotes', true);
  HidePresetWindow := ini.ReadBool('Preferences', 'HidePresetWindow', true);
  CheckMapErrorsOnSave := ini.ReadBool('Preferences', 'CheckMapErrorsOnSave', true);
  CheckMapErrorsOnTest := ini.ReadBool('Preferences', 'CheckMapErrorsOnTest', true);
  AlwaysAskOnQuit := ini.ReadBool('Preferences', 'AlwaysAskOnQuit', true);
  DrawBuildingMarker := ini.ReadBool('Preferences', 'DrawBuildingMarker', true);
  DrawObjectBrush := ini.ReadBool('Preferences', 'DrawObjectBrush', true);
  DrawPaintBrush := ini.ReadBool('Preferences', 'DrawPaintBrush', true);
  LoadR16Image := ini.ReadBool('Preferences', 'LoadR16Image', true);
  LoadR8Image := ini.ReadBool('Preferences', 'LoadR8Image', true);
  UseAllocationIndexes := ini.ReadBool('Preferences', 'UseAllocationIndexes', false);
  ShowEventMarkers := ini.ReadBool('Preferences', 'ShowEventMarkers', true);
  MarkDefenceAreas := ini.ReadBool('Preferences', 'MarkDefenceAreas', true);
  GridColor := ini.ReadInteger('Preferences', 'GridColor', $000000);
  // Load default values
  DefaultMapWidth := ini.ReadInteger('Defaults', 'DefaultMapWidth', 64);
  DefaultMapHeight := ini.ReadInteger('Defaults', 'DefaultMapHeight', 64);
  DefaultMisTechLevel := ini.ReadInteger('Defaults', 'DefaultMisTechLevel', 1);
  DefaultMisStartingMoney := ini.ReadInteger('Defaults', 'DefaultMisStartingMoney', 3000);
  DefaultTileset := ini.ReadInteger('Defaults', 'DefaultTileset', 2);
  AssignMisFileToNewMap := ini.ReadBool('Defaults', 'AssignMisFileToNewMap', true);
  PreplaceWormSpawner := ini.ReadBool('Defaults', 'PreplaceWormSpawner', false);
  // Load file paths
  GamePath := ini.ReadString('Paths','GamePath','');
  GameExecutable := ini.ReadString('Paths','GameExecutable','');
  MissionsPath := ini.ReadString('Paths','MissionsPath','');
  TextUIBPath := ini.ReadString('Paths','TextUIBPath','');
  determine_game_paths_from_path(current_dir);
  // Load recent files
  for i := 1 to cnt_recent_files do
  begin
    RecentFiles[i] := ini.ReadString('RecentFiles', 'file' + inttostr(i), '');
  end;
  // Load MainWindow GUI setings
  if not PreserveGUISettings then
    exit;
  load_window_position(ini, MainWindow, 'MainWindow');
  MainWindow.CbSelectStructures.Checked := ini.ReadBool('GUI','MainWindow.CbSelectStructures.Checked',MainWindow.CbSelectStructures.Checked);
end;

procedure TSettings.load_postcreate_editor_settings;
var
  ini: TMemIniFile;
begin
  ini := tmp_ini;
  // Load GUI settings for all other dialogs
  if not PreserveGUISettings then
  begin
    ini.Destroy;
    exit;
  end;
  load_window_position(ini, TilesetDialog, 'TilesetDialog');
  load_window_position(ini, BlockPresetDialog, 'BlockPresetDialog');
  load_window_position(ini, SetDialog, 'SetDialog');
  load_window_position(ini, TestMapDialog, 'TestMapDialog');
  load_window_position(ini, MissionDialog, 'MissionDialog');
  MissionDialog.StringValueList.Height := ini.ReadInteger('GUI','MissionDialog.StringValueList.Height',MissionDialog.StringValueList.Height);
  load_window_position(ini, EventDialog, 'EventDialog');
  EventDialog.LowerPanel.Height := ini.ReadInteger('GUI','EventDialog.LowerPanel.Height',EventDialog.LowerPanel.Height);
  EventDialog.EventGrid.ColWidths[4] := ini.ReadInteger('GUI','EventDialog.EventGrid.ColWidths[4]',EventDialog.EventGrid.ColWidths[4]);
  EventDialog.EventGrid.ColWidths[5] := ini.ReadInteger('GUI','EventDialog.EventGrid.ColWidths[5]',EventDialog.EventGrid.ColWidths[5]);
  load_window_position(ini, MapStatsDialog, 'MapStatsDialog');
  ini.Destroy;
end;


procedure TSettings.save_editor_settings;
var
  ini: TMemIniFile;
  i: integer;
begin
  ini := TMemIniFile.Create(current_dir + 'D2kEditor.ini');
  // Save preferences
  ini.WriteBool('Preferences', 'PreserveGUISettings', PreserveGUISettings);
  ini.WriteBool('Preferences', 'RestrictSpiceToSand', RestrictSpiceToSand);
  ini.WriteBool('Preferences', 'EnableEventNotes', EnableEventNotes);
  ini.WriteBool('Preferences', 'HidePresetWindow', HidePresetWindow);
  ini.WriteBool('Preferences', 'CheckMapErrorsOnSave', CheckMapErrorsOnSave);
  ini.WriteBool('Preferences', 'CheckMapErrorsOnTest', CheckMapErrorsOnTest);
  ini.WriteBool('Preferences', 'AlwaysAskOnQuit', AlwaysAskOnQuit);
  ini.WriteBool('Preferences', 'DrawBuildingMarker', DrawBuildingMarker);
  ini.WriteBool('Preferences', 'DrawObjectBrush', DrawObjectBrush);
  ini.WriteBool('Preferences', 'DrawPaintBrush', DrawPaintBrush);
  ini.WriteBool('Preferences', 'LoadR16Image', LoadR16Image);
  ini.WriteBool('Preferences', 'LoadR8Image', LoadR8Image);
  ini.WriteBool('Preferences', 'UseAllocationIndexes', UseAllocationIndexes);
  ini.WriteBool('Preferences', 'ShowEventMarkers', ShowEventMarkers);
  ini.WriteBool('Preferences', 'MarkDefenceAreas', MarkDefenceAreas);
  ini.WriteInteger('Preferences', 'Gridcolor', GridColor);
  // Save default values
  ini.WriteInteger('Defaults', 'DefaultMapWidth', DefaultMapWidth);
  ini.WriteInteger('Defaults', 'DefaultMapHeight', DefaultMapHeight);
  ini.WriteInteger('Defaults', 'DefaultMisTechLevel', DefaultMisTechLevel);
  ini.WriteInteger('Defaults', 'DefaultMisStartingMoney', DefaultMisStartingMoney);
  ini.WriteInteger('Defaults', 'DefaultTileset', DefaultTileset);
  ini.WriteBool('Defaults', 'AssignMisFileToNewMap', AssignMisFileToNewMap);
  ini.WriteBool('Defaults', 'PreplaceWormSpawner', PreplaceWormSpawner);
  // Save file paths
  ini.WriteString('Paths','GamePath',GamePath);
  ini.WriteString('Paths','GameExecutable',GameExecutable);
  ini.WriteString('Paths','MissionsPath',MissionsPath);
  ini.WriteString('Paths','TextUIBPath',TextUIBPath);
  // Save recent files
  for i := 1 to cnt_recent_files do
  begin
    if RecentFiles[i] <> '' then
      ini.WriteString('RecentFiles', 'file' + inttostr(i), RecentFiles[i]);
  end;
  // Save GUI settings
  save_window_position(ini, MainWindow, 'MainWindow');
  ini.WriteBool('GUI','MainWindow.CbSelectStructures.Checked',MainWindow.CbSelectStructures.Checked);
  save_window_position(ini, TilesetDialog, 'TilesetDialog');
  save_window_position(ini, BlockPresetDialog, 'BlockPresetDialog');
  save_window_position(ini, SetDialog, 'SetDialog');
  save_window_position(ini, TestMapDialog, 'TestMapDialog');
  save_window_position(ini, MissionDialog, 'MissionDialog');
  ini.WriteInteger('GUI','MissionDialog.StringValueList.Height',MissionDialog.StringValueList.Height);
  save_window_position(ini, EventDialog, 'EventDialog');
  ini.WriteInteger('GUI','EventDialog.LowerPanel.Height',EventDialog.LowerPanel.Height);
  ini.WriteInteger('GUI','EventDialog.EventGrid.ColWidths[4]',EventDialog.EventGrid.ColWidths[4]);
  ini.WriteInteger('GUI','EventDialog.EventGrid.ColWidths[5]',EventDialog.EventGrid.ColWidths[5]);
  save_window_position(ini, MapStatsDialog, 'MapStatsDialog');
  ini.UpdateFile;
  ini.Destroy;
end;

procedure TSettings.determine_game_paths_from_path(path: String);
var
  temp_path: String;
begin
  // Get Game path and game executable from map filename
  if (GameExecutable = '') or (not FileExists(GameExecutable)) then
  begin
    temp_path := path;
    repeat
      temp_path := ExcludeTrailingPathDelimiter(ExtractFilePath(temp_path));
      if FileExists(temp_path + '\dune2000.exe') then
      begin
        GamePath := temp_path;
        GameExecutable := temp_path + '\dune2000.exe';
        break;
      end;
    until
      Length(temp_path) <= 2;
  end;
  // Attempt to find game path failed, end now
  if (GameExecutable = '') or (not FileExists(GameExecutable)) then
    exit;
  // Get Missions path
  if (MissionsPath = '') or (not DirectoryExists(MissionsPath)) then
  begin
    temp_path := GamePath + '\Data\Missions';
    if DirectoryExists(temp_path) then
      MissionsPath := temp_path
    else
      MissionsPath := GamePath + '\Missions';
  end;
  // Get TEXT.UIB filename and load it
  if (TextUIBPath = '') or (not FileExists(TextUIBPath)) then
  begin
    TextUIBPath := GamePath + '\Data\UI_DATA\TEXT.UIB';
    StringTable.load_from_file(TextUIBPath);
  end;
end;

procedure TSettings.load_window_position(ini: TMemIniFile; window: TForm; window_name: String);
var
  i, left, top: integer;
  windowRect, dummyRect: TRect;
  onAnyMonitor: boolean;
begin
  left := ini.ReadInteger('GUI', window_name + '.Left', window.Left);
  top := ini.ReadInteger('GUI', window_name + '.Top', window.Top);
  // Check if the window is shown on any monitor
  windowRect := Rect(left, top, left + window.Width, top + window.Height);
  onAnyMonitor := false;
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    onAnyMonitor := onAnyMonitor or IntersectRect(dummyRect, windowRect, Screen.Monitors[i].WorkareaRect);
  end;
  // Set window position according to last position saved in ini file
  if (onAnyMonitor) then
  begin
    window.Left := left;
    window.Top := top;
  end;
  // Load window size
  if window.BorderStyle <> bsSizeable then
    exit;
  if not ((window.Constraints.MinWidth = window.Constraints.MaxWidth) and (window.Constraints.MinWidth <> 0)) then
    window.Width := ini.ReadInteger('GUI', window_name + '.Width', window.Width);
  if not ((window.Constraints.MinHeight = window.Constraints.MaxHeight) and (window.Constraints.MinHeight <> 0)) then
    window.Height := ini.ReadInteger('GUI', window_name + '.Height', window.Height);
end;

procedure TSettings.save_window_position(ini: TMemIniFile; window: TForm; window_name: String);
begin
  ini.WriteInteger('GUI', window_name + '.Left', window.Left);
  ini.WriteInteger('GUI', window_name + '.Top', window.Top);
  if window.BorderStyle <> bsSizeable then
    exit;
  if not ((window.Constraints.MinWidth = window.Constraints.MaxWidth) and (window.Constraints.MinWidth <> 0)) then
    ini.WriteInteger('GUI', window_name + '.Width', window.Width);
  if not ((window.Constraints.MinHeight = window.Constraints.MaxHeight) and (window.Constraints.MinHeight <> 0)) then
    ini.WriteInteger('GUI', window_name + '.Height', window.Height);
end;

procedure TSettings.update_recent_files(filename: String);
var
  i: integer;
  found_pos: integer;
begin
  // Try to find if the file exists in the list
  found_pos := cnt_recent_files + 1;
  for i := 1 to cnt_recent_files do
  begin
    if RecentFiles[i] = filename then
    begin
      found_pos := i;
      break;
    end;
  end;
  // Save current file into the first position and shift other files to next position
  for i := found_pos downto 2 do
  begin
    if i > cnt_recent_files then
      continue;
    RecentFiles[i] := RecentFiles[i-1];
  end;
  RecentFiles[1] := filename;
end;

procedure TSettings.get_map_test_settings;
var
  ini: TIniFile;
  map_name: String;
  house, mission: char;
  house_num, mission_num: integer;
begin
  ini := TIniFile.Create(GamePath + '\spawn.ini');
  // Try to detect MissionNumber and MySideID from map file name
  if Map.filename <> '' then
  begin
    map_name := ChangeFileExt(ExtractFileName(Map.filename),'');
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

procedure TSettings.save_map_test_settings;
var
  ini: TIniFile;
begin
  // Write settings to ini file
  ini := TIniFile.Create(GamePath + '\spawn.ini');
  ini.WriteString('Settings', 'Scenario', 'TESTMAP');
  ini.WriteInteger('Settings', 'MySideID', MySideID);
  ini.WriteInteger('Settings', 'MissionNumber', MissionNumber);
  ini.WriteInteger('Settings', 'DifficultyLevel', DifficultyLevel);
  ini.WriteInteger('Settings', 'Seed', Seed);
  if TextUib <> '' then
    ini.WriteString('Settings', 'TextUib', TextUib);
  ini.Destroy;
end;

end.
