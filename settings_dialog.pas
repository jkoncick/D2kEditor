unit settings_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TSettingsDialog = class(TForm)
    gbUISettings: TGroupBox;
    cbPreserveGUISettings: TCheckBox;
    cbHidePresetWindow: TCheckBox;
    cbAlwaysAskOnQuit: TCheckBox;
    cbTranslateStructureNames: TCheckBox;
    cbEventGridShowEmptyLines: TCheckBox;
    gbEditingSettings: TGroupBox;
    cbRestrictPainting: TCheckBox;
    cbUseRandomPaintMap: TCheckBox;
    cbLoadCustomColoursBin: TCheckBox;
    cbCheckMapErrorsOnSave: TCheckBox;
    cbCheckMapErrorsOnTest: TCheckBox;
    gbDefaultValues: TGroupBox;
    lblDefaultMapWidth: TLabel;
    seDefaultMapWidth: TSpinEdit;
    lblDefaultMapHeight: TLabel;
    seDefaultMapHeight: TSpinEdit;
    lblDefaultMisTechLevel: TLabel;
    seDefaultMisTechLevel: TSpinEdit;
    lblDefaultMisStartingMoney: TLabel;
    edDefaultMisStartingMoney: TEdit;
    lblDefaultTilesetName: TLabel;
    edDefaultTilesetName: TEdit;
    gbPaths: TGroupBox;
    lblGamePath: TLabel;
    lblGameExecutable: TLabel;
    lblMissionsPath: TLabel;
    edGamePath: TEdit;
    edGameExecutable: TEdit;
    edMissionsPath: TEdit;
    btnGamePath: TButton;
    btnGameExecutable: TButton;
    btnMissionsPath: TButton;
    OpenDialog: TOpenDialog;
    btnSave: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnGamePathClick(Sender: TObject);
    procedure btnGameExecutableClick(Sender: TObject);
    procedure btnMissionsPathClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingsDialog: TSettingsDialog;

implementation

{$R *.dfm}

{$WARN UNIT_PLATFORM OFF}

uses FileCtrl, _settings;

procedure TSettingsDialog.FormShow(Sender: TObject);
begin
  cbPreserveGUISettings.Checked := Settings.PreserveGUISettings;
  cbRestrictPainting.Checked := Settings.RestrictPainting;
  cbHidePresetWindow.Checked := Settings.HidePresetWindow;
  cbCheckMapErrorsOnSave.Checked := Settings.CheckMapErrorsOnSave;
  cbCheckMapErrorsOnTest.Checked := Settings.CheckMapErrorsOnTest;
  cbAlwaysAskOnQuit.Checked := Settings.AlwaysAskOnQuit;
  cbLoadCustomColoursBin.Checked := Settings.LoadCustomColoursBin;
  cbUseRandomPaintMap.Checked := Settings.UseRandomPaintMap;
  cbTranslateStructureNames.Checked := Settings.TranslateStructureNames;
  cbEventGridShowEmptyLines.Checked := Settings.EventGridShowEmptyLines;
  seDefaultMapWidth.Value := Settings.DefaultMapWidth;
  seDefaultMapHeight.Value := Settings.DefaultMapHeight;
  seDefaultMisTechLevel.Value := Settings.DefaultMisTechLevel;
  edDefaultMisStartingMoney.Text := IntToStr(Settings.DefaultMisStartingMoney);
  edDefaultTilesetName.Text := Settings.DefaultTilesetName;
  edGamePath.Text := Settings.GamePath;
  edGameExecutable.Text := Settings.GameExecutable;
  edMissionsPath.Text := Settings.MissionsPath;
end;

procedure TSettingsDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    btnSaveClick(Sender);
    Key := 0;
  end;
  if Key = 27 then
  begin
    btnCancelClick(Sender);
    Key := 0;
  end;
end;

procedure TSettingsDialog.btnGamePathClick(Sender: TObject);
var
  s: String;
begin
  s := edGamePath.Text;
  if SelectDirectory(s, [], 0) then
    edGamePath.Text := s;
end;

procedure TSettingsDialog.btnGameExecutableClick(Sender: TObject);
begin
  OpenDialog.FileName := edGameExecutable.Text;
  if OpenDialog.Execute then
    edGameExecutable.Text := OpenDialog.FileName;
end;

procedure TSettingsDialog.btnMissionsPathClick(Sender: TObject);
var
  s: String;
begin
  s := edMissionsPath.Text;
  if SelectDirectory(s, [], 0) then
    edMissionsPath.Text := s;
end;

procedure TSettingsDialog.btnSaveClick(Sender: TObject);
begin
  Settings.PreserveGUISettings := cbPreserveGUISettings.Checked;
  Settings.RestrictPainting := cbRestrictPainting.Checked;
  Settings.HidePresetWindow := cbHidePresetWindow.Checked;
  Settings.CheckMapErrorsOnSave := cbCheckMapErrorsOnSave.Checked;
  Settings.CheckMapErrorsOnTest := cbCheckMapErrorsOnTest.Checked;
  Settings.AlwaysAskOnQuit := cbAlwaysAskOnQuit.Checked;
  Settings.LoadCustomColoursBin := cbLoadCustomColoursBin.Checked;
  Settings.UseRandomPaintMap := cbUseRandomPaintMap.Checked;
  Settings.TranslateStructureNames := cbTranslateStructureNames.Checked;
  Settings.EventGridShowEmptyLines := cbEventGridShowEmptyLines.Checked;
  Settings.DefaultMapWidth := seDefaultMapWidth.Value;
  Settings.DefaultMapHeight := seDefaultMapHeight.Value;
  Settings.DefaultMisTechLevel := seDefaultMisTechLevel.Value;
  Settings.DefaultMisStartingMoney := StrToInt(edDefaultMisStartingMoney.Text);
  Settings.DefaultTilesetName := edDefaultTilesetName.Text;
  Settings.GamePath := edGamePath.Text;
  Settings.GameExecutable := edGameExecutable.Text;
  Settings.MissionsPath := edMissionsPath.Text;
  Close;
end;

procedure TSettingsDialog.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.

