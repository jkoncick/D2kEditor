unit test_map_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ShellApi, IniFiles, CheckLst;

type
  TTestMapDialog = class(TForm)
    eMySideID: TComboBox;
    lblMySideID: TLabel;
    lblDifficultyLevel: TLabel;
    eDifficultyLevel: TComboBox;
    btnCancel: TButton;
    btnLaunch: TButton;
    eDebugFeatures: TCheckListBox;
    lblDebugFeatures: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
  private
    procedure store_data;
  public
    function invoke: TModalResult;
    procedure update_side_list(side_list: TStringList);
  end;

var
  TestMapDialog: TTestMapDialog;

implementation

uses
  main, _mission, _structures, _launcher;

{$R *.dfm}

procedure TTestMapDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 13 then
  begin
    key := 0;
    btnLaunchClick(Sender);
  end;
  if key = 27 then
  begin
    key := 0;
    btnCancelClick(Sender);
  end;
end;

procedure TTestMapDialog.btnCancelClick(Sender: TObject);
begin
  store_data;
  ModalResult := mrCancel;
end;

procedure TTestMapDialog.btnLaunchClick(Sender: TObject);
begin
  store_data;
  ModalResult := mrOk;
end;

procedure TTestMapDialog.store_data;
var
  i, value: integer;
begin
  Launcher.MySideId := eMySideID.ItemIndex;
  Launcher.DifficultyLevel := eDifficultyLevel.ItemIndex;
  value := 0;
  for i := 0 to eDebugFeatures.Items.Count - 1 do
    if eDebugFeatures.Checked[i] then
      value := value or (1 shl i);
  Launcher.DebugFeatures := value;
end;

function TTestMapDialog.invoke: TModalResult;
var
  i: integer;
begin
  eMySideID.ItemIndex := Launcher.MySideID;
  eDifficultyLevel.ItemIndex := Launcher.DifficultyLevel;
  for i := 0 to eDebugFeatures.Items.Count - 1 do
    eDebugFeatures.Checked[i] := ((1 shl i) and Launcher.DebugFeatures) <> 0;
  result := ShowModal;
end;

procedure TTestMapDialog.update_side_list(side_list: TStringList);
begin
  eMySideID.Items := side_list;
end;

end.
