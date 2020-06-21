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
    lblTextUib: TLabel;
    eTextUib: TEdit;
    lblParameters: TLabel;
    eParameters: TEdit;
    btnCancel: TButton;
    btnLaunch: TButton;
    btnRandomSeed: TButton;
    btnDefaultTextUib: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnRandomSeedClick(Sender: TObject);
    procedure btnDefaultTextUibClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure invoke;
    procedure update_player_list(player_list: TStringList);
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

procedure TTestMapDialog.btnDefaultTextUibClick(Sender: TObject);
begin
  eTextUib.Text := '';
end;

procedure TTestMapDialog.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTestMapDialog.btnLaunchClick(Sender: TObject);
begin
  with Launcher do
  begin
    MySideId := eMySideID.ItemIndex;
    MissionNumber := eMissionNumber.Value;
    DifficultyLevel := eDifficultyLevel.ItemIndex;
    Seed := strtoint(eSeed.Text);
    TextUib := eTextUib.Text;
    TestMapParameters := eParameters.Text;
  end;
  Launcher.launch_current_mission;
  close;
end;

procedure TTestMapDialog.invoke;
begin
  with Launcher do
  begin
    eMySideID.ItemIndex := MySideID;
    eMissionNumber.Value := MissionNumber;
    eDifficultyLevel.ItemIndex := DifficultyLevel;
    eSeed.Text := inttostr(Seed);
    eTextUib.Text := TextUib;
    eParameters.Text := TestMapParameters;
  end;
  ShowModal;
end;

procedure TTestMapDialog.update_player_list(player_list: TStringList);
begin
  eMySideID.Items := player_list;
  if eMySideID.ItemIndex = -1 then
    eMySideID.ItemIndex := 0;
end;

end.
