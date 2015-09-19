program D2kEditor;

uses
  Forms,
  main in 'main.pas' {MainWindow},
  set_dialog in 'set_dialog.pas' {SetDialog},
  tileset_dialog in 'tileset_dialog.pas' {TilesetDialog},
  test_map_dialog in 'test_map_dialog.pas' {TestMapDialog},
  event_dialog in 'event_dialog.pas' {EventDialog},
  mission_dialog in 'mission_dialog.pas' {MissionDialog},
  _map in '_map.pas',
  _mission in '_mission.pas',
  _tileset in '_tileset.pas',
  _stringtable in '_stringtable.pas',
  _settings in '_settings.pas',
  _structures in '_structures.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D2kEditor';
  Mission := TMission.Create;
  Tileset := TTileset.Create;
  StringTable := TStringTable.Create;
  Settings := TSettings.Create;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TSetDialog, SetDialog);
  Application.CreateForm(TTilesetDialog, TilesetDialog);
  Application.CreateForm(TTestMapDialog, TestMapDialog);
  Application.CreateForm(TEventDialog, EventDialog);
  Application.CreateForm(TMissionDialog, MissionDialog);
  // All GUI settings must be loaded after all dialogs are created.
  Settings.load_postcreate_editor_settings;
  // Load map given as first parameter
  if ParamCount > 0 then
    MainWindow.load_map(ParamStr(1));
  Application.Run;
end.
