program D2kEditor;

uses
  Forms,
  main in 'main.pas' {MainWindow},
  set_dialog in 'set_dialog.pas' {SetDialog},
  tileset_dialog in 'tileset_dialog.pas' {TilesetDialog},
  test_map_dialog in 'test_map_dialog.pas' {TestMapDialog},
  mis_file in 'mis_file.pas',
  tileset in 'tileset.pas',
  map_defs in 'map_defs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D2K+ map Editor';
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TSetDialog, SetDialog);
  Application.CreateForm(TTilesetDialog, TilesetDialog);
  Application.CreateForm(TTestMapDialog, TestMapDialog);
  Application.Run;
end.
