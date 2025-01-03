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
    { Private declarations }
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

procedure TTestMapDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    close;
  end;
end;

procedure TTestMapDialog.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTestMapDialog.btnLaunchClick(Sender: TObject);
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
  ModalResult := mrOk;
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
