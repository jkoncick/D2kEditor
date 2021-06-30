unit mission_launcher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, Menus;

type
  TMissionGridColumnDefinition = record
    name: string;
    short: string;
    width: integer;
    shown: boolean;
  end;

const mission_grid_column_definitions: array[0..11] of TMissionGridColumnDefinition =
  (
    (name: 'File name';       short: '';   width: 160; shown: false),
    (name: 'Mission name';    short: '';   width: 256; shown: true),
    (name: 'Author';          short: '';   width: 68;  shown: true),
    (name: 'Side ID';         short: 'S.'; width: 16;  shown: true),
    (name: 'Mission number';  short: '#';  width: 16;  shown: false),
    (name: 'Campaign folder'; short: '';   width: 80;  shown: true),
    (name: 'Mods folder';     short: '';   width: 80;  shown: true),
    (name: 'Colours.bin';     short: '';   width: 80;  shown: false),
    (name: 'Players.ini';     short: '';   width: 80;  shown: false),
    (name: 'Text.uib';        short: '';   width: 80;  shown: true),
    (name: 'Intel ID';        short: '';   width: 80;  shown: false),
    (name: 'Tileset';         short: '';   width: 64;  shown: true)
  );

type
  TMissionGridColumnState = record
    width: integer;
    shown: boolean;
  end;

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
    pnMissionDetails: TPanel;
    pnMissionDetailsControls: TPanel;
    pmMissionList: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PopupMenuitemClick(Sender: TObject);
    procedure sgMissionListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgMissionListDblClick(Sender: TObject);
    procedure sgMissionListMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgMissionListMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure btnLaunchGameClick(Sender: TObject);
    procedure btnOpenMissionInEditorClick(Sender: TObject);
  public
    mission_grid_column_states: array of TMissionGridColumnState;
    popup_menuitems: array of TMenuItem;
    mission_index: integer;
  private
    // Fill data procedures
    procedure fill_mission_grid_columns;
    procedure fill_mission_grid_data;
    procedure fill_mission_details(index: integer);
    // Store data procedures
    procedure store_mission_grid_column_states;
  public
    // Save settings
    procedure save_mission_grid_column_states;
  end;

var
  MissionLauncher: TMissionLauncher;

implementation

uses
  ShellApi, Math, StrUtils, _map, _launcher, _settings, _stringtable;

{$R *.dfm}

procedure TMissionLauncher.FormCreate(Sender: TObject);
var
  i: integer;
begin
  SetLength(mission_grid_column_states, Length(mission_grid_column_definitions));
  SetLength(popup_menuitems, Length(mission_grid_column_definitions));
  // Create column seletion menu items
  for i := 0 to Length(mission_grid_column_definitions) - 1 do
  begin
    mission_grid_column_states[i].shown := Settings.ini.ReadBool('MissionLauncherGrid', Format('Column%d.Shown', [i]), mission_grid_column_definitions[i].shown);
    mission_grid_column_states[i].width := Settings.ini.ReadInteger('MissionLauncherGrid', Format('Column%d.Width', [i]), mission_grid_column_definitions[i].width);
    popup_menuitems[i] := TMenuItem.Create(pmMissionList);
    popup_menuitems[i].Caption := mission_grid_column_definitions[i].name;
    popup_menuitems[i].AutoCheck := true;
    popup_menuitems[i].Checked := mission_grid_column_states[i].shown;
    popup_menuitems[i].OnClick := PopupMenuitemClick;
    pmMissionList.Items.Add(popup_menuitems[i]);
  end;
  fill_mission_grid_columns;
  // Reset mission index
  mission_index := -1;
end;

procedure TMissionLauncher.FormShow(Sender: TObject);
begin
  if Launcher.missions_loaded then
    exit;
  Launcher.load_all_missions;
  fill_mission_grid_data;
  fill_mission_details(sgMissionList.Row - 1);
end;

procedure TMissionLauncher.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    27: close;
    13: btnLaunchGameClick(nil);
    32: btnOpenMissionInEditorClick(nil);
  end;
end;

procedure TMissionLauncher.PopupMenuitemClick(Sender: TObject);
begin
  store_mission_grid_column_states;
  fill_mission_grid_columns;
  fill_mission_grid_data;
end;

procedure TMissionLauncher.sgMissionListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  fill_mission_details(ARow - 1);
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
  if (mission_index <> -1) and not Launcher.check_game_is_running then
    Launcher.launch_mission(mission_index, cbDifficultyLevel.ItemIndex);
end;

procedure TMissionLauncher.btnOpenMissionInEditorClick(Sender: TObject);
begin
  if mission_index = -1 then
    exit;
  if sender = btnOpenMissionInEditor then
    close;
  Map.load_map(Settings.MissionsPath + '\' + Launcher.mission_data[mission_index].filename + '.MAP');
end;

procedure TMissionLauncher.fill_mission_grid_columns;
var
  i: integer;
  col: integer;
begin
  // Update number of columns
  col := 0;
  for i := 0 to Length(mission_grid_column_definitions) - 1 do
    if mission_grid_column_states[i].shown then
      inc(col);
  sgMissionList.ColCount := col;
  // Update column names
  col := 0;
  for i := 0 to Length(mission_grid_column_definitions) - 1 do
  begin
    if not mission_grid_column_states[i].shown then
      continue;
    sgMissionList.ColWidths[col] := mission_grid_column_states[i].width;
    sgMissionList.Cells[col,0] := IfThen(mission_grid_column_definitions[i].short <> '', mission_grid_column_definitions[i].short, mission_grid_column_definitions[i].name);
    inc(col);
  end;
end;

procedure TMissionLauncher.fill_mission_grid_data;
var
  i, j: integer;
  col: integer;
begin
  sgMissionList.RowCount := Launcher.mission_list.Count + 1;
  for i := 0 to Launcher.mission_list.Count - 1 do
  begin
    col := 0;
    for j := 0 to Length(mission_grid_column_definitions) - 1 do
    begin
      if not mission_grid_column_states[j].shown then
        continue;
      with Launcher.mission_data[strtoint(Launcher.mission_list.ValueFromIndex[i])] do
        case j of
          0: sgMissionList.Cells[col, i + 1] := filename;
          1: sgMissionList.Cells[col, i + 1] := mission_name;
          2: sgMissionList.Cells[col, i + 1] := author;
          3: sgMissionList.Cells[col, i + 1] := inttostr(side_id);
          4: sgMissionList.Cells[col, i + 1] := inttostr(mission_number);
          5: sgMissionList.Cells[col, i + 1] := campaign_folder;
          6: sgMissionList.Cells[col, i + 1] := mods_folder;
          7: sgMissionList.Cells[col, i + 1] := colours_file;
          8: sgMissionList.Cells[col, i + 1] := players_file;
          9: sgMissionList.Cells[col, i + 1] := text_uib;
          10: sgMissionList.Cells[col, i + 1] := intel_id;
          11: sgMissionList.Cells[col, i + 1] := tileset;
        end;
      inc(col);
    end;
  end;
end;

procedure TMissionLauncher.fill_mission_details(index: integer);
var
  side_letter: char;
begin
  if index < 0 then
    exit;
  mission_index := strtoint(Launcher.mission_list.ValueFromIndex[index]);
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

procedure TMissionLauncher.store_mission_grid_column_states;
var
  i: integer;
  col: integer;
begin
  col := 0;
  for i := 0 to Length(mission_grid_column_definitions) - 1 do
  begin
    if mission_grid_column_states[i].shown then
    begin
      mission_grid_column_states[i].width := sgMissionList.ColWidths[col];
      inc(col);
    end;
    mission_grid_column_states[i].shown := popup_menuitems[i].Checked;
  end;
end;

procedure TMissionLauncher.save_mission_grid_column_states;
var
  i: integer;
begin
  store_mission_grid_column_states;
  for i := 0 to Length(mission_grid_column_definitions) - 1 do
  begin
    Settings.ini.WriteBool('MissionLauncherGrid', Format('Column%d.Shown', [i]), mission_grid_column_states[i].shown);
    Settings.ini.WriteInteger('MissionLauncherGrid', Format('Column%d.Width', [i]), mission_grid_column_states[i].width);
  end;
end;

end.
