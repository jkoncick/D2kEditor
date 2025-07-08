program D2kEditor;

uses
  Windows,
  Forms,
  SysUtils,
  main in 'main.pas' {MainWindow},
  settings_dialog in 'settings_dialog.pas' {SettingsDialog},
  set_dialog in 'set_dialog.pas' {SetDialog},
  tileset_dialog in 'tileset_dialog.pas' {TilesetDialog},
  block_preset_dialog in 'block_preset_dialog.pas' {BlockPresetDialog},
  test_map_dialog in 'test_map_dialog.pas' {TestMapDialog},
  event_dialog in 'event_dialog.pas' {EventDialog},
  mission_dialog in 'mission_dialog.pas' {MissionDialog},
  map_stats_dialog in 'map_stats_dialog.pas' {MapStatsDialog},
  mission_launcher in 'mission_launcher.pas' {MissionLauncher},
  tileatr_editor in 'tileatr_editor.pas' {TileAtrEditor},
  structures_editor in 'structures_editor.pas' {StructuresEditor},
  debug_window in 'debug_window.pas' {DebugWindow},
  _utils in '_utils.pas',
  _settings in '_settings.pas',
  _dispatcher in '_dispatcher.pas',
  _renderer in '_renderer.pas',
  _map in '_map.pas',
  _misai in '_misai.pas',
  _mission in '_mission.pas',
  _missionini in '_missionini.pas',
  _tileset in '_tileset.pas',
  _structures in '_structures.pas',
  _graphics in '_graphics.pas',
  _sounds in '_sounds.pas',
  _stringtable in '_stringtable.pas',
  _randomgen in '_randomgen.pas',
  _launcher in '_launcher.pas',
  _gamelists in '_gamelists.pas',
  _gamestructs in '_gamestructs.pas',
  _eventconfig in '_eventconfig.pas',
  pngimage in 'pngdelphi\pngimage.pas',
  zlibpas in 'pngdelphi\zlibpas.pas',
  pnglang in 'pngdelphi\pnglang.pas';

{$R *.res}

begin
  // Miscellaneous initializations
  randomize;
  current_dir := ExtractFilePath(Application.ExeName);
  QueryPerformanceFrequency(performance_frequency);
  Application.Initialize;
  Application.Title := 'D2kEditor';
  Application.HintPause := 500;
  Application.HintHidePause:= 100000;
  // Load settings
  Settings := TSettings.Create;
  Settings.load_precreate_editor_settings;
  // Create program modules
  Dispatcher := TDispatcher.Create;
  Renderer := TRenderer.Create;
  Map := TMap.Create;
  MisAI := TMisAI.Create;
  Mission := TMission.Create;
  MissionIni := TMissionIni.Create;
  Tileset := TTileset.Create;
  Structures := TStructures.Create;
  StructGraphics := TStructGraphics.Create;
  Sounds := TSounds.Create;
  StringTable := TStringTable.Create;
  //--RandomGen := TRandomGen.Create;
  Launcher := TLauncher.Create;
  GameLists := TGameLists.Create;
  GameStructs := TGameStructs.Create;
  EventConfig := TEventConfig.Create;
  // Create forms
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TSettingsDialog, SettingsDialog);
  Application.CreateForm(TSetDialog, SetDialog);
  Application.CreateForm(TTilesetDialog, TilesetDialog);
  Application.CreateForm(TBlockPresetDialog, BlockPresetDialog);
  Application.CreateForm(TTestMapDialog, TestMapDialog);
  Application.CreateForm(TEventDialog, EventDialog);
  Application.CreateForm(TMissionDialog, MissionDialog);
  Application.CreateForm(TMapStatsDialog, MapStatsDialog);
  Application.CreateForm(TMissionLauncher, MissionLauncher);
  Application.CreateForm(TTileAtrEditor, TileAtrEditor);
  Application.CreateForm(TStructuresEditor, StructuresEditor);
  Application.CreateForm(TDebugWindow, DebugWindow);
  // All GUI settings must be loaded after all dialogs are created.
  Settings.load_postcreate_editor_settings;
  // Initialize program modules
  Renderer.init;
  MisAI.init;
  Mission.init;
  MissionIni.init;
  Tileset.init;
  GameLists.init;
  GameStructs.init;
  Structures.init;
  StructGraphics.init;
  Sounds.init;
  StringTable.init;
  Launcher.init;
  EventConfig.init;
  // Load map given as first parameter
  if ParamCount > 0 then
    Map.load_map(ParamStr(1));
  // Main program loop
  repeat
    try
      Application.HandleMessage;
      Dispatcher.do_pending_actions;
    except
      Application.HandleException(Application);
    end;
  until Application.Terminated;
end.
