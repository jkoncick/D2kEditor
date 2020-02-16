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
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnRandomSeedClick(Sender: TObject);
    procedure btnDefaultTextUibClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure invoke;
  end;

var
  TestMapDialog: TTestMapDialog;

implementation

uses
  main, _mission, _settings;

{$R *.dfm}

procedure TTestMapDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cnt_mis_players - 1 do
    eMySideID.Items.Add(inttostr(i) + ' - ' + player_names[i]);
  eMySideID.ItemIndex := 0;
end;

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
  with Settings do
  begin
    MySideId := eMySideID.ItemIndex;
    MissionNumber := eMissionNumber.Value;
    DifficultyLevel := eDifficultyLevel.ItemIndex;
    Seed := strtoint(eSeed.Text);
    TextUib := eTextUib.Text;
    TestMapParameters := eParameters.Text;
  end;
  MainWindow.launch_game;
  close;
end;

procedure TTestMapDialog.invoke;
begin
  with Settings do
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

end.
