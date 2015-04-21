program D2kEditor;

uses
  Forms,
  main in 'main.pas' {MainWindow},
  set_dialog in 'set_dialog.pas' {SetDialog},
  tileset_dialog in 'tileset_dialog.pas' {TilesetDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D2K+ map Editor';
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TSetDialog, SetDialog);
  Application.CreateForm(TTilesetDialog, TilesetDialog);
  Application.Run;
end.
