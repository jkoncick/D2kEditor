unit test_map_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ShellApi, IniFiles;

type
  TTestMapDialog = class(TForm)
    eMySideID: TComboBox;
    lblMySideID: TLabel;
    lblMissionNumber: TLabel;
    eMissionNumber: TSpinEdit;
    lblDifficultyLevel: TLabel;
    eDifficultyLevel: TComboBox;
    lblSeed: TLabel;
    eSeed: TEdit;
    btnCancel: TButton;
    btnLaunch: TButton;
    btnRandomSeed: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnRandomSeedClick(Sender: TObject);
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

procedure TTestMapDialog.btnRandomSeedClick(Sender: TObject);
begin
  eSeed.Text := inttostr(random(2000000000));
end;

procedure TTestMapDialog.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTestMapDialog.btnLaunchClick(Sender: TObject);
begin
  with Launcher do
  begin
    MySideId := eMySideID.ItemIndex;
    MissionNumber := eMissionNumber.Value;
    DifficultyLevel := eDifficultyLevel.ItemIndex;
    Seed := strtoint(eSeed.Text);
  end;
  ModalResult := mrOk;
end;

function TTestMapDialog.invoke: TModalResult;
begin
  with Launcher do
  begin
    eMySideID.ItemIndex := MySideID;
    eMissionNumber.Value := MissionNumber;
    eDifficultyLevel.ItemIndex := DifficultyLevel;
    eSeed.Text := inttostr(Seed);
  end;
  result := ShowModal;
end;

procedure TTestMapDialog.update_side_list(side_list: TStringList);
begin
  eMySideID.Items := side_list;
end;

end.
