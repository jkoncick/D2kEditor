unit _settings;

interface

uses
  IniFiles, Types, Forms, Controls, Dialogs;

const cnt_recent_files = 9;

type
  TSettings = class

  public
    // Preferences
    PreserveGUISettings: boolean;
    RestrictPainting: boolean;
    HidePresetWindow: boolean;
    CheckMapErrorsOnSave: boolean;
    CheckMapErrorsOnTest: boolean;
    AlwaysAskOnQuit: boolean;
    LoadR16Image: boolean;
    LoadR8Image: boolean;
    LoadCustomColoursBin: boolean;
    UseAllocationIndexes: boolean;
    ShowEventMarkers: boolean;
    MarkDefenceAreas: boolean;
    ShowUnknownSpecials: boolean;
    UseRandomPaintMap: boolean;
    TranslateStructureNames: boolean;
    CleanUpExtraModFilesAfterLaunch: boolean;
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

    // Debug settings
    Debug_ShowRenderTime: boolean;
    Debug_ShowDifferentialRendering: boolean;
    Debug_ShowReplaceFilesFromModsFolderLog: boolean;

  public
    ini: TMemIniFile;

  public
    procedure load_precreate_editor_settings;
    procedure load_postcreate_editor_settings;
    procedure save_editor_settings;
    procedure determine_game_paths_from_path(path: String);
    procedure load_window_position(window: TForm);
    procedure save_window_position(window: TForm);
    function  load_control_property_int(control: TControl; property_name: String; property_value: integer): integer;
    procedure save_control_property_int(control: TControl; property_name: String; property_value: integer);
    procedure load_file_dialog_initial_dir(dialog: TOpenDialog; default_dir: String);
    procedure save_file_dialog_initial_dir(dialog: TOpenDialog);
    procedure update_recent_files(filename: String);

  end;

var
  Settings: TSettings;

implementation

uses
  SysUtils, StdCtrls, main, tileset_dialog, block_preset_dialog, set_dialog, test_map_dialog,
  mission_dialog, event_dialog, map_stats_dialog, mission_launcher, tileatr_editor, structures_editor, debug_window, _utils,
  Grids;

procedure TSettings.load_precreate_editor_settings;
var
  i: integer;
begin
  ini := TMemIniFile.Create(current_dir + 'D2kEditor.ini');
  // Load preferences
  PreserveGUISettings := ini.ReadBool('Preferences', 'PreserveGUISettings', true);
  RestrictPainting := ini.ReadBool('Preferences', 'RestrictPainting', true);
  HidePresetWindow := ini.ReadBool('Preferences', 'HidePresetWindow', true);
  CheckMapErrorsOnSave := ini.ReadBool('Preferences', 'CheckMapErrorsOnSave', true);
  CheckMapErrorsOnTest := ini.ReadBool('Preferences', 'CheckMapErrorsOnTest', true);
  AlwaysAskOnQuit := ini.ReadBool('Preferences', 'AlwaysAskOnQuit', true);
  LoadR16Image := ini.ReadBool('Preferences', 'LoadR16Image', true);
  LoadR8Image := ini.ReadBool('Preferences', 'LoadR8Image', true);
  LoadCustomColoursBin := ini.ReadBool('Preferences', 'LoadCustomColoursBin', true);
  UseAllocationIndexes := ini.ReadBool('Preferences', 'UseAllocationIndexes', false);
  ShowEventMarkers := ini.ReadBool('Preferences', 'ShowEventMarkers', true);
  MarkDefenceAreas := ini.ReadBool('Preferences', 'MarkDefenceAreas', true);
  ShowUnknownSpecials := ini.ReadBool('Preferences', 'ShowUnknownSpecials', false);
  UseRandomPaintMap := ini.ReadBool('Preferences', 'UseRandomPaintMap', false);
  TranslateStructureNames := ini.ReadBool('Preferences', 'TranslateStructureNames', false);
  CleanUpExtraModFilesAfterLaunch := ini.ReadBool('Preferences', 'CleanUpExtraModFilesAfterLaunch', false);
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
    RecentFiles[i] := ini.ReadString('RecentFiles', 'file' + inttostr(i), '');
end;

procedure TSettings.load_postcreate_editor_settings;
var
  i: integer;
begin
  // Load GUI settings for all other dialogs
  load_window_position(TilesetDialog);
  load_window_position(BlockPresetDialog);
  load_window_position(SetDialog);
  load_window_position(TestMapDialog);
  load_window_position(MissionDialog);
  MissionDialog.StringValueList.Height := load_control_property_int(MissionDialog.StringValueList, 'Height', MissionDialog.StringValueList.Height);
  load_window_position(EventDialog);
  EventDialog.LowerPanel.Height := load_control_property_int(EventDialog.LowerPanel, 'Height', EventDialog.LowerPanel.Height);
  for i := EventDialog.EventGrid.FixedCols to EventDialog.EventGrid.ColCount - 1 do
    EventDialog.EventGrid.ColWidths[i] := load_control_property_int(EventDialog.EventGrid, Format('ColWidths[%d]', [i]), EventDialog.EventGrid.ColWidths[i]);
  load_window_position(MapStatsDialog);
  load_window_position(MissionLauncher);
  load_window_position(TileAtrEditor);
  TileAtrEditor.cbAlwaysOnTop.State := TCheckBoxState(load_control_property_int(TileAtrEditor.cbAlwaysOnTop, 'State', Ord(TileAtrEditor.cbAlwaysOnTop.State)));
  load_window_position(StructuresEditor);
  load_window_position(DebugWindow);
  // Load file dialog paths
  load_file_dialog_initial_dir(MainWindow.MapOpenDialog,           MissionsPath);
  load_file_dialog_initial_dir(MainWindow.MapSaveDialog,           MissionsPath);
  load_file_dialog_initial_dir(MainWindow.TilesetOpenDialog,       GamePath + '\Data');
  load_file_dialog_initial_dir(MainWindow.TileatrOpenDialog,       GamePath + '\Data\bin');
  load_file_dialog_initial_dir(MainWindow.MapImageSaveDialog,      '');
  load_file_dialog_initial_dir(MainWindow.RemapTilesOpenDialog,    current_dir);
  load_file_dialog_initial_dir(MissionDialog.ExportAIDialog,       current_dir + 'AI_templates');
  load_file_dialog_initial_dir(MissionDialog.ImportAIDialog,       current_dir + 'AI_templates');
  load_file_dialog_initial_dir(EventDialog.ExportEventsDialog,     '');
  load_file_dialog_initial_dir(EventDialog.ImportEventsDialog,     '');
  load_file_dialog_initial_dir(TileAtrEditor.SaveTileAtrDialog,    GamePath + '\Data\bin');
  load_file_dialog_initial_dir(StructuresEditor.ItemExportDialog,  '');
  load_file_dialog_initial_dir(StructuresEditor.ItemImportDialog,  '');
  load_file_dialog_initial_dir(StructuresEditor.ArtExportDialog,   '');
  load_file_dialog_initial_dir(StructuresEditor.ArtImportDialog,   '');
  load_file_dialog_initial_dir(StructuresEditor.ImageExportDialog, '');
  load_file_dialog_initial_dir(StructuresEditor.ImageImportDialog, '');
  load_file_dialog_initial_dir(StructuresEditor.SoundExportDialog, '');
  load_file_dialog_initial_dir(StructuresEditor.SoundImportDialog, '');

  ini.Destroy;
  ini := nil;
end;


procedure TSettings.save_editor_settings;
var
  i: integer;
begin
  ini := TMemIniFile.Create(current_dir + 'D2kEditor.ini');
  // Save preferences
  ini.WriteBool('Preferences', 'PreserveGUISettings', PreserveGUISettings);
  ini.WriteBool('Preferences', 'RestrictPainting', RestrictPainting);
  ini.WriteBool('Preferences', 'HidePresetWindow', HidePresetWindow);
  ini.WriteBool('Preferences', 'CheckMapErrorsOnSave', CheckMapErrorsOnSave);
  ini.WriteBool('Preferences', 'CheckMapErrorsOnTest', CheckMapErrorsOnTest);
  ini.WriteBool('Preferences', 'AlwaysAskOnQuit', AlwaysAskOnQuit);
  ini.WriteBool('Preferences', 'LoadR16Image', LoadR16Image);
  ini.WriteBool('Preferences', 'LoadR8Image', LoadR8Image);
  ini.WriteBool('Preferences', 'LoadCustomColoursBin', LoadCustomColoursBin);
  ini.WriteBool('Preferences', 'UseAllocationIndexes', UseAllocationIndexes);
  ini.WriteBool('Preferences', 'ShowEventMarkers', ShowEventMarkers);
  ini.WriteBool('Preferences', 'MarkDefenceAreas', MarkDefenceAreas);
  ini.WriteBool('Preferences', 'ShowUnknownSpecials', ShowUnknownSpecials);
  ini.WriteBool('Preferences', 'UseRandomPaintMap', UseRandomPaintMap);
  ini.WriteBool('Preferences', 'TranslateStructureNames', TranslateStructureNames);
  ini.WriteBool('Preferences', 'CleanUpExtraModFilesAfterLaunch', CleanUpExtraModFilesAfterLaunch);
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
    if RecentFiles[i] <> '' then
      ini.WriteString('RecentFiles', 'file' + inttostr(i), RecentFiles[i]);
  // Save GUI settings
  save_window_position(MainWindow);
  save_control_property_int(MainWindow.CbSelectStructures, 'State', Ord(MainWindow.CbSelectStructures.State));
  save_window_position(TilesetDialog);
  save_window_position(BlockPresetDialog);
  save_window_position(SetDialog);
  save_window_position(TestMapDialog);
  save_window_position(MissionDialog);
  save_control_property_int(MissionDialog.StringValueList, 'Height', MissionDialog.StringValueList.Height);
  save_window_position(EventDialog);
  save_control_property_int(EventDialog.LowerPanel, 'Height', EventDialog.LowerPanel.Height);
  for i := EventDialog.EventGrid.FixedCols to EventDialog.EventGrid.ColCount - 1 do
    save_control_property_int(EventDialog.EventGrid, Format('ColWidths[%d]', [i]), EventDialog.EventGrid.ColWidths[i]);
  save_window_position(MapStatsDialog);
  save_window_position(MissionLauncher);
  MissionLauncher.save_mission_grid_column_states;
  save_window_position(TileAtrEditor);
  save_control_property_int(TileAtrEditor.cbAlwaysOnTop, 'State', Ord(TileAtrEditor.cbAlwaysOnTop.State));
  save_window_position(StructuresEditor);
  save_window_position(DebugWindow);
  // Save file dialog paths
  save_file_dialog_initial_dir(MainWindow.MapOpenDialog);
  save_file_dialog_initial_dir(MainWindow.MapSaveDialog);
  save_file_dialog_initial_dir(MainWindow.TilesetOpenDialog);
  save_file_dialog_initial_dir(MainWindow.TileatrOpenDialog);
  save_file_dialog_initial_dir(MainWindow.MapImageSaveDialog);
  save_file_dialog_initial_dir(MainWindow.RemapTilesOpenDialog);
  save_file_dialog_initial_dir(MissionDialog.ExportAIDialog);
  save_file_dialog_initial_dir(MissionDialog.ImportAIDialog);
  save_file_dialog_initial_dir(EventDialog.ExportEventsDialog);
  save_file_dialog_initial_dir(EventDialog.ImportEventsDialog);
  save_file_dialog_initial_dir(TileAtrEditor.SaveTileAtrDialog);
  save_file_dialog_initial_dir(StructuresEditor.ItemExportDialog);
  save_file_dialog_initial_dir(StructuresEditor.ItemImportDialog);
  save_file_dialog_initial_dir(StructuresEditor.ArtExportDialog);
  save_file_dialog_initial_dir(StructuresEditor.ArtImportDialog);
  save_file_dialog_initial_dir(StructuresEditor.ImageExportDialog);
  save_file_dialog_initial_dir(StructuresEditor.ImageImportDialog);
  save_file_dialog_initial_dir(StructuresEditor.SoundExportDialog);
  save_file_dialog_initial_dir(StructuresEditor.SoundImportDialog);

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

procedure TSettings.load_window_position(window: TForm);
var
  i, left, top: integer;
  windowRect, dummyRect: TRect;
  onAnyMonitor: boolean;
begin
  if not PreserveGUISettings then
    exit;
  left := ini.ReadInteger('GUI', window.Name + '.Left', window.Left);
  top := ini.ReadInteger('GUI', window.Name + '.Top', window.Top);
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
    window.Width := ini.ReadInteger('GUI', window.Name + '.Width', window.Width);
  if not ((window.Constraints.MinHeight = window.Constraints.MaxHeight) and (window.Constraints.MinHeight <> 0)) then
    window.Height := ini.ReadInteger('GUI', window.Name + '.Height', window.Height);
  if ini.ReadBool('GUI', window.Name + '.Maximized', false) then
    window.WindowState := wsMaximized;
end;

procedure TSettings.save_window_position(window: TForm);
begin
  if not PreserveGUISettings then
    exit;
  ini.WriteInteger('GUI', window.Name + '.Left', window.Left);
  ini.WriteInteger('GUI', window.Name + '.Top', window.Top);
  if window.BorderStyle <> bsSizeable then
    exit;
  if not ((window.Constraints.MinWidth = window.Constraints.MaxWidth) and (window.Constraints.MinWidth <> 0)) then
    ini.WriteInteger('GUI', window.Name + '.Width', window.Width);
  if not ((window.Constraints.MinHeight = window.Constraints.MaxHeight) and (window.Constraints.MinHeight <> 0)) then
    ini.WriteInteger('GUI', window.Name + '.Height', window.Height);
  ini.WriteBool('GUI', window.Name + '.Maximized', window.WindowState = wsMaximized);
end;

function TSettings.load_control_property_int(control: TControl; property_name: String; property_value: integer): integer;
begin
  result := property_value;
  if PreserveGUISettings then
    result := ini.ReadInteger('GUI', control.Owner.Name + '.' + control.Name + '.' + property_name, property_value);
end;

procedure TSettings.save_control_property_int(control: TControl; property_name: String; property_value: integer);
begin
  if PreserveGUISettings then
    ini.WriteInteger('GUI', control.Owner.Name + '.' + control.Name + '.' + property_name, property_value);
end;

procedure TSettings.load_file_dialog_initial_dir(dialog: TOpenDialog; default_dir: String);
begin
  dialog.InitialDir := ini.ReadString('FileDialogPaths', dialog.Name, default_dir);
end;

procedure TSettings.save_file_dialog_initial_dir(dialog: TOpenDialog);
begin
  if dialog.FileName <> '' then
    ini.WriteString('FileDialogPaths', dialog.Name, ExtractFilePath(dialog.FileName));
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
