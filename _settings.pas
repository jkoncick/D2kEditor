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
    RestrictPainting: boolean;
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
    LoadCustomColoursBin: boolean;
    UseAllocationIndexes: boolean;
    ShowEventMarkers: boolean;
    MarkDefenceAreas: boolean;
    ShowUnknownSpecials: boolean;
    UseRandomPaintMap: boolean;
    GridColor: Cardinal;

    // Default values
    DefaultMapWidth: integer;
    DefaultMapHeight: integer;
    DefaultMisTechLevel: integer;
    DefaultMisStartingMoney: integer;
    DefaultTilesetName: String;
    AssignMisFileToNewMap: boolean;
    PreplaceWormSpawner: boolean;

    // File paths
    GameExecutable: String;
    GamePath: String;
    MissionsPath: String;

    // Recent files
    RecentFiles: Array[1..cnt_recent_files] of String;

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

  end;

var
  Settings: TSettings;

implementation

uses
  SysUtils, StdCtrls, main, tileset_dialog, block_preset_dialog, set_dialog, test_map_dialog,
  mission_dialog, event_dialog, map_stats_dialog, tileatr_editor;

procedure TSettings.load_precreate_editor_settings;
var
  ini: TMemIniFile;
  i: integer;
begin
  ini := TMemIniFile.Create(current_dir + 'D2kEditor.ini');
  tmp_ini := ini;
  // Load preferences
  PreserveGUISettings := ini.ReadBool('Preferences', 'PreserveGUISettings', true);
  RestrictPainting := ini.ReadBool('Preferences', 'RestrictPainting', true);
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
  LoadCustomColoursBin := ini.ReadBool('Preferences', 'LoadCustomColoursBin', true);
  UseAllocationIndexes := ini.ReadBool('Preferences', 'UseAllocationIndexes', false);
  ShowEventMarkers := ini.ReadBool('Preferences', 'ShowEventMarkers', true);
  MarkDefenceAreas := ini.ReadBool('Preferences', 'MarkDefenceAreas', true);
  ShowUnknownSpecials := ini.ReadBool('Preferences', 'ShowUnknownSpecials', false);
  UseRandomPaintMap := ini.ReadBool('Preferences', 'UseRandomPaintMap', false);
  GridColor := ini.ReadInteger('Preferences', 'GridColor', $000000);
  // Load default values
  DefaultMapWidth := ini.ReadInteger('Defaults', 'DefaultMapWidth', 64);
  DefaultMapHeight := ini.ReadInteger('Defaults', 'DefaultMapHeight', 64);
  DefaultMisTechLevel := ini.ReadInteger('Defaults', 'DefaultMisTechLevel', 1);
  DefaultMisStartingMoney := ini.ReadInteger('Defaults', 'DefaultMisStartingMoney', 3000);
  DefaultTilesetName := ini.ReadString('Defaults', 'DefaultTilesetName', 'BLOXBGBS');
  AssignMisFileToNewMap := ini.ReadBool('Defaults', 'AssignMisFileToNewMap', true);
  PreplaceWormSpawner := ini.ReadBool('Defaults', 'PreplaceWormSpawner', false);
  // Load file paths
  GamePath := ini.ReadString('Paths','GamePath','');
  GameExecutable := ini.ReadString('Paths','GameExecutable','');
  MissionsPath := ini.ReadString('Paths','MissionsPath','');
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
  MainWindow.CbSelectStructures.State := TCheckBoxState(ini.ReadInteger('GUI','MainWindow.CbSelectStructures.State',Ord(MainWindow.CbSelectStructures.State)));
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
  load_window_position(ini, TileAtrEditor, 'TileAtrEditor');
  TileAtrEditor.cbAlwaysOnTop.Checked := ini.ReadBool('GUI','TileAtrEditor.cbAlwaysOnTop.Checked',TileAtrEditor.cbAlwaysOnTop.Checked);
  // Load file dialog paths
  MainWindow.MapOpenDialog.InitialDir := ini.ReadString('FileDialogPaths', 'MapOpenDialog', MissionsPath);
  MainWindow.MapSaveDialog.InitialDir := ini.ReadString('FileDialogPaths', 'MapSaveDialog', MissionsPath);
  MainWindow.TilesetOpenDialog.InitialDir := ini.ReadString('FileDialogPaths', 'TilesetOpenDialog', GamePath + '\Data');
  MainWindow.TileatrOpenDialog.InitialDir := ini.ReadString('FileDialogPaths', 'TileatrOpenDialog', GamePath + '\Data\bin');
  MainWindow.MapImageSaveDialog.InitialDir := ini.ReadString('FileDialogPaths', 'MapImageSaveDialog', '');
  MainWindow.RemapTilesOpenDialog.InitialDir := ini.ReadString('FileDialogPaths', 'RemapTilesOpenDialog', current_dir);
  MissionDialog.ExportAIDialog.InitialDir := ini.ReadString('FileDialogPaths', 'ExportAIDialog', current_dir + 'AI_templates');
  MissionDialog.ImportAIDialog.InitialDir := ini.ReadString('FileDialogPaths', 'ImportAIDialog', current_dir + 'AI_templates');
  TileAtrEditor.SaveTileAtrDialog.InitialDir := ini.ReadString('FileDialogPaths', 'SaveTileAtrDialog', GamePath + '\Data\bin');

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
  ini.WriteBool('Preferences', 'RestrictPainting', RestrictPainting);
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
  ini.WriteBool('Preferences', 'LoadCustomColoursBin', LoadCustomColoursBin);
  ini.WriteBool('Preferences', 'UseAllocationIndexes', UseAllocationIndexes);
  ini.WriteBool('Preferences', 'ShowEventMarkers', ShowEventMarkers);
  ini.WriteBool('Preferences', 'MarkDefenceAreas', MarkDefenceAreas);
  ini.WriteBool('Preferences', 'ShowUnknownSpecials', ShowUnknownSpecials);
  ini.WriteBool('Preferences', 'UseRandomPaintMap', UseRandomPaintMap);
  ini.WriteInteger('Preferences', 'Gridcolor', GridColor);
  // Save default values
  ini.WriteInteger('Defaults', 'DefaultMapWidth', DefaultMapWidth);
  ini.WriteInteger('Defaults', 'DefaultMapHeight', DefaultMapHeight);
  ini.WriteInteger('Defaults', 'DefaultMisTechLevel', DefaultMisTechLevel);
  ini.WriteInteger('Defaults', 'DefaultMisStartingMoney', DefaultMisStartingMoney);
  ini.WriteString('Defaults', 'DefaultTilesetName', DefaultTilesetName);
  ini.WriteBool('Defaults', 'AssignMisFileToNewMap', AssignMisFileToNewMap);
  ini.WriteBool('Defaults', 'PreplaceWormSpawner', PreplaceWormSpawner);
  // Save file paths
  ini.WriteString('Paths','GamePath',GamePath);
  ini.WriteString('Paths','GameExecutable',GameExecutable);
  ini.WriteString('Paths','MissionsPath',MissionsPath);
  // Save recent files
  for i := 1 to cnt_recent_files do
  begin
    if RecentFiles[i] <> '' then
      ini.WriteString('RecentFiles', 'file' + inttostr(i), RecentFiles[i]);
  end;
  // Save GUI settings
  save_window_position(ini, MainWindow, 'MainWindow');
  ini.WriteInteger('GUI','MainWindow.CbSelectStructures.State',Ord(MainWindow.CbSelectStructures.State));
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
  save_window_position(ini, TileAtrEditor, 'TileAtrEditor');
  ini.WriteBool('GUI','TileAtrEditor.cbAlwaysOnTop.Checked',TileAtrEditor.cbAlwaysOnTop.Checked);
  // Save file dialog paths
  if MainWindow.MapOpenDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'MapOpenDialog', ExtractFilePath(MainWindow.MapOpenDialog.FileName));
  if MainWindow.MapSaveDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'MapSaveDialog', ExtractFilePath(MainWindow.MapSaveDialog.FileName));
  if MainWindow.TilesetOpenDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'TilesetOpenDialog', ExtractFilePath(MainWindow.TilesetOpenDialog.FileName));
  if MainWindow.TileatrOpenDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'TileatrOpenDialog', ExtractFilePath(MainWindow.TileatrOpenDialog.FileName));
  if MainWindow.MapImageSaveDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'MapImageSaveDialog', ExtractFilePath(MainWindow.MapImageSaveDialog.FileName));
  if MainWindow.RemapTilesOpenDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'RemapTilesOpenDialog', ExtractFilePath(MainWindow.RemapTilesOpenDialog.FileName));
  if MissionDialog.ExportAIDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'ExportAIDialog', ExtractFilePath(MissionDialog.ExportAIDialog.FileName));
  if MissionDialog.ImportAIDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'ImportAIDialog', ExtractFilePath(MissionDialog.ImportAIDialog.FileName));
  if TileAtrEditor.SaveTileAtrDialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', 'SaveTileAtrDialog', ExtractFilePath(TileAtrEditor.SaveTileAtrDialog.FileName));

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
  if ini.ReadBool('GUI', window_name + '.Maximized', false) then
    window.WindowState := wsMaximized;
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
  ini.WriteBool('GUI', window_name + '.Maximized', window.WindowState = wsMaximized);
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

end.
