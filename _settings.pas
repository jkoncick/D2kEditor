unit _settings;

interface

uses
  IniFiles;

type
  TSettings = class

  public
    // Preferences
    AssignMisFileToNewMap: boolean;
    PreserveGUISettings: boolean;
    RestrictSpiceToSand: boolean;
    EnableEventNotes: boolean;
    HidePresetWindow: boolean;

    // Default values
    DefaultMapWidth: integer;
    DefaultMapHeight: integer;
    DefaultMisTechLevel: integer;
    DefaultMisStartingMoney: integer;
    DefaultTileset: integer;

    // File paths
    GameExecutable: String;
    GamePath: String;
    TextUIBPath: String;

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
    procedure get_file_paths_from_map_filename;
    procedure get_map_test_settings;
    procedure save_map_test_settings;

  end;

var
  Settings: TSettings;

implementation

uses
  SysUtils, main, tileset_dialog, block_preset_dialog, set_dialog, test_map_dialog,
  mission_dialog, event_dialog, _map, _stringtable;

procedure TSettings.load_precreate_editor_settings;
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(current_dir + 'D2kEditor.ini');
  tmp_ini := ini;
  // Load preferences
  AssignMisFileToNewMap := ini.ReadBool('Preferences', 'AssignMisFileToNewMap', true);
  PreserveGUISettings := ini.ReadBool('Preferences', 'PreserveGUISettings', true);
  RestrictSpiceToSand := ini.ReadBool('Preferences', 'RestrictSpiceToSand', true);
  EnableEventNotes := ini.ReadBool('Preferences', 'EnableEventNotes', true);
  HidePresetWindow := ini.ReadBool('Preferences', 'HidePresetWindow', true);
  // Load default values
  DefaultMapWidth := ini.ReadInteger('Defaults', 'DefaultMapWidth', 64);
  DefaultMapHeight := ini.ReadInteger('Defaults', 'DefaultMapHeight', 64);
  DefaultMisTechLevel := ini.ReadInteger('Defaults', 'DefaultMisTechLevel', 1);
  DefaultMisStartingMoney := ini.ReadInteger('Defaults', 'DefaultMisStartingMoney', 3000);
  DefaultTileset := ini.ReadInteger('Defaults', 'DefaultTileset', 2);
  // Load file paths
  GamePath := ini.ReadString('Paths','GamePath', current_dir + '..\');
  GameExecutable := ini.ReadString('Paths','GameExecutable', GamePath + 'dune2000.exe');
  TextUIBPath := ini.ReadString('Paths','TextUIBPath', GamePath + 'Data\UI_DATA\TEXT.UIB');
  // Load MainWindow GUI setings
  if not PreserveGUISettings then
    exit;
  MainWindow.Left := ini.ReadInteger('GUI','MainWindow.Left',MainWindow.Left);
  MainWindow.Top := ini.ReadInteger('GUI','MainWindow.Top',MainWindow.Top);
  MainWindow.Width := ini.ReadInteger('GUI','MainWindow.Width',MainWindow.Width);
  MainWindow.Height := ini.ReadInteger('GUI','MainWindow.Height',MainWindow.Height);
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
  TilesetDialog.Left := ini.ReadInteger('GUI','TilesetDialog.Left',TilesetDialog.Left);
  TilesetDialog.Top := ini.ReadInteger('GUI','TilesetDialog.Top',TilesetDialog.Top);
  TilesetDialog.Height := ini.ReadInteger('GUI','TilesetDialog.Height',TilesetDialog.Height);
  BlockPresetDialog.Left := ini.ReadInteger('GUI','BlockPresetDialog.Left',BlockPresetDialog.Left);
  BlockPresetDialog.Top := ini.ReadInteger('GUI','BlockPresetDialog.Top',BlockPresetDialog.Top);
  SetDialog.Left := ini.ReadInteger('GUI','SetDialog.Left',SetDialog.Left);
  SetDialog.Top := ini.ReadInteger('GUI','SetDialog.Top',SetDialog.Top);
  TestMapDialog.Left := ini.ReadInteger('GUI','TestMapDialog.Left',TestMapDialog.Left);
  TestMapDialog.Top := ini.ReadInteger('GUI','TestMapDialog.Top',TestMapDialog.Top);
  MissionDialog.Left := ini.ReadInteger('GUI','MissionDialog.Left',MissionDialog.Left);
  MissionDialog.Top := ini.ReadInteger('GUI','MissionDialog.Top',MissionDialog.Top);
  MissionDialog.Height := ini.ReadInteger('GUI','MissionDialog.Height',MissionDialog.Height);
  MissionDialog.StringValueList.Height := ini.ReadInteger('GUI','MissionDialog.StringValueList.Height',MissionDialog.StringValueList.Height);
  EventDialog.Left := ini.ReadInteger('GUI','EventDialog.Left',EventDialog.Left);
  EventDialog.Top := ini.ReadInteger('GUI','EventDialog.Top',EventDialog.Top);
  EventDialog.Width := ini.ReadInteger('GUI','EventDialog.Width',EventDialog.Width);
  EventDialog.Height := ini.ReadInteger('GUI','EventDialog.Height',EventDialog.Height);
  EventDialog.LowerPanel.Height := ini.ReadInteger('GUI','EventDialog.LowerPanel.Height',EventDialog.LowerPanel.Height);
  ini.Destroy;
end;


procedure TSettings.save_editor_settings;
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(current_dir + 'D2kEditor.ini');
  // Save preferences
  ini.WriteBool('Preferences', 'AssignMisFileToNewMap', AssignMisFileToNewMap);
  ini.WriteBool('Preferences', 'PreserveGUISettings', PreserveGUISettings);
  ini.WriteBool('Preferences', 'RestrictSpiceToSand', RestrictSpiceToSand);
  ini.WriteBool('Preferences', 'EnableEventNotes', EnableEventNotes);
  ini.WriteBool('Preferences', 'HidePresetWindow', HidePresetWindow);
  // Save default values
  ini.WriteInteger('Defaults', 'DefaultMapWidth', DefaultMapWidth);
  ini.WriteInteger('Defaults', 'DefaultMapHeight', DefaultMapHeight);
  ini.WriteInteger('Defaults', 'DefaultMisTechLevel', DefaultMisTechLevel);
  ini.WriteInteger('Defaults', 'DefaultMisStartingMoney', DefaultMisStartingMoney);
  ini.WriteInteger('Defaults', 'DefaultTileset', DefaultTileset);
  // Save file paths
  ini.WriteString('Paths','GamePath',GamePath);
  ini.WriteString('Paths','GameExecutable',GameExecutable);
  ini.WriteString('Paths','TextUIBPath',TextUIBPath);
  // Save GUI settings
  ini.WriteInteger('GUI','MainWindow.Left',MainWindow.Left);
  ini.WriteInteger('GUI','MainWindow.Top',MainWindow.Top);
  ini.WriteInteger('GUI','MainWindow.Width',MainWindow.Width);
  ini.WriteInteger('GUI','MainWindow.Height',MainWindow.Height);
  ini.WriteInteger('GUI','TilesetDialog.Left',TilesetDialog.Left);
  ini.WriteInteger('GUI','TilesetDialog.Top',TilesetDialog.Top);
  ini.WriteInteger('GUI','TilesetDialog.Height',TilesetDialog.Height);
  ini.WriteInteger('GUI','BlockPresetDialog.Left',BlockPresetDialog .Left);
  ini.WriteInteger('GUI','BlockPresetDialog.Top',BlockPresetDialog.Top);
  ini.WriteInteger('GUI','SetDialog.Left',SetDialog.Left);
  ini.WriteInteger('GUI','SetDialog.Top',SetDialog.Top);
  ini.WriteInteger('GUI','TestMapDialog.Left',TestMapDialog.Left);
  ini.WriteInteger('GUI','TestMapDialog.Top',TestMapDialog.Top);
  ini.WriteInteger('GUI','MissionDialog.Left',MissionDialog.Left);
  ini.WriteInteger('GUI','MissionDialog.Top',MissionDialog.Top);
  ini.WriteInteger('GUI','MissionDialog.Height',MissionDialog.Height);
  ini.WriteInteger('GUI','MissionDialog.StringValueList.Height',MissionDialog.StringValueList.Height);
  ini.WriteInteger('GUI','EventDialog.Left',EventDialog.Left);
  ini.WriteInteger('GUI','EventDialog.Top',EventDialog.Top);
  ini.WriteInteger('GUI','EventDialog.Width',EventDialog.Width);
  ini.WriteInteger('GUI','EventDialog.Height',EventDialog.Height);
  ini.WriteInteger('GUI','EventDialog.LowerPanel.Height',EventDialog.LowerPanel.Height);
  ini.UpdateFile;
  ini.Destroy;
end;

procedure TSettings.get_file_paths_from_map_filename;
begin
  // Get Game path and game executable from map filename
  if (GameExecutable = '') or (not FileExists(GameExecutable)) then
  begin
    GamePath := ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(map_filename)));
    GameExecutable := GamePath + 'dune2000.exe';
  end;
  // Get TEXT.UIB filename and load it
  if (TextUIBPath = '') or (not FileExists(TextUIBPath)) then
  begin
    TextUIBPath := GamePath + 'Data\UI_DATA\TEXT.UIB';
    StringTable.load_from_file(TextUIBPath);
  end;
end;

procedure TSettings.get_map_test_settings;
var
  ini: TIniFile;
  map_name: String;
  house, mission: char;
  house_num, mission_num: integer;
begin
  ini := TIniFile.Create(GamePath + 'spawn.ini');
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

procedure TSettings.save_map_test_settings;
var
  ini: TIniFile;
begin
  // Write settings to ini file
  ini := TIniFile.Create(GamePath + 'spawn.ini');
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
