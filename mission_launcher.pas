unit mission_launcher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids;

type
  TMissionLauncher = class(TForm)
    mMissionBriefing: TMemo;
    lbMissionFileName: TLabel;
    edMissionFileName: TEdit;
    btnLaunchGame: TButton;
    btnOpenMissionInEditor: TButton;
    cbDifficultyLevel: TComboBox;
    lbDifficultyLevel: TLabel;
    sgMissionList: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgMissionListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgMissionListDblClick(Sender: TObject);
    procedure sgMissionListMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgMissionListMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure btnLaunchGameClick(Sender: TObject);
    procedure btnOpenMissionInEditorClick(Sender: TObject);
  private
    mission_index: integer;
  public
    { Public declarations }
  end;

var
  MissionLauncher: TMissionLauncher;

implementation

uses
  ShellApi, Math, _launcher, _settings, _stringtable, main;

{$R *.dfm}

procedure TMissionLauncher.FormCreate(Sender: TObject);
begin
  sgMissionList.ColWidths[0] := 260;
  sgMissionList.Cells[0,0] := 'Mission name';
  sgMissionList.ColWidths[1] := 76;
  sgMissionList.Cells[1,0] := 'Author';
  sgMissionList.ColWidths[2] := 16;
  sgMissionList.Cells[2,0] := 'S.';
  sgMissionList.ColWidths[3] := 100;
  sgMissionList.Cells[3,0] := 'Campaign folder';
  sgMissionList.ColWidths[4] := 100;
  sgMissionList.Cells[4,0] := 'Mods folder';
  sgMissionList.ColWidths[5] := 100;
  sgMissionList.Cells[5,0] := 'Text.uib';
end;

procedure TMissionLauncher.FormShow(Sender: TObject);
var
  i : integer;
  dummy: boolean;
begin
  if Launcher.missions_loaded then
    exit;
  Launcher.load_all_missions;
  mission_index := -1;
  sgMissionList.RowCount := Launcher.mission_list.Count + 1;
  for i := 0 to Launcher.mission_list.Count - 1 do
    with Launcher.mission_data[strtoint(Launcher.mission_list.ValueFromIndex[i])] do
    begin
      sgMissionList.Cells[0, i + 1] := mission_name;
      sgMissionList.Cells[1, i + 1] := author;
      sgMissionList.Cells[2, i + 1] := inttostr(side_id);
      sgMissionList.Cells[3, i + 1] := campaign_folder;
      sgMissionList.Cells[4, i + 1] := mods_folder;
      sgMissionList.Cells[5, i + 1] := text_uib;
    end;
  sgMissionListSelectCell(nil, sgMissionList.Col, sgMissionList.Row, dummy);
end;

procedure TMissionLauncher.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    27: close;
    13: btnLaunchGameClick(nil);
    32: btnOpenMissionInEditorClick(nil);
  end;
end;

procedure TMissionLauncher.sgMissionListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  side_letter: char;
begin
  if ARow <= 0 then
    exit;
  mission_index := strtoint(Launcher.mission_list.ValueFromIndex[ARow - 1]);
  with (Launcher.mission_data[mission_index]) do
  begin
    edMissionFileName.Text := filename;
    if (briefing = '') and (mission_number > 0) and (side_id < 3) then
    begin
      // In-game briefing from text.uib
      side_letter := ' ';
      case side_id of
        0: side_letter := 'A';
        1: side_letter := 'H';
        2: side_letter := 'O';
      end;
      mMissionBriefing.Text := StringReplace(StringTable.text_uib.Values[side_letter + 'M' + inttostr(mission_number) + 'Text2'], #172, #13#10, [rfReplaceAll]);;
    end else
      // Briefing from ini file
      mMissionBriefing.Text := StringReplace(briefing, '_', #13#10, [rfReplaceAll]);
  end;
end;

procedure TMissionLauncher.sgMissionListDblClick(Sender: TObject);
begin
  btnOpenMissionInEditorClick(nil);
end;

procedure TMissionLauncher.sgMissionListMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  sgMissionList.TopRow := Max(Min(sgMissionList.TopRow + 2, sgMissionList.RowCount - sgMissionList.VisibleRowCount), 1);
  handled := true;
end;

procedure TMissionLauncher.sgMissionListMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  sgMissionList.TopRow := Max(sgMissionList.TopRow - 2, 1);
  handled := true;
end;

procedure TMissionLauncher.btnLaunchGameClick(Sender: TObject);
begin
  if mission_index <> -1 then
    Launcher.launch_mission(mission_index, cbDifficultyLevel.ItemIndex);
end;

procedure TMissionLauncher.btnOpenMissionInEditorClick(Sender: TObject);
begin
  if mission_index = -1 then
    exit;
  if sender = btnOpenMissionInEditor then
    close;
  MainWindow.load_map(Settings.MissionsPath + '\' + Launcher.mission_data[mission_index].filename + '.MAP');
end;

end.
