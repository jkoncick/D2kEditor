unit mission_launcher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMissionLauncher = class(TForm)
    lbMissionList: TListBox;
    mMissionBriefing: TMemo;
    edMissionName: TEdit;
    edMissionAuthor: TEdit;
    lbMissionName: TLabel;
    lbMissionAuthor: TLabel;
    lbMissionNumber: TLabel;
    edMissionNumber: TEdit;
    lbMissionFileName: TLabel;
    edMissionFileName: TEdit;
    btnLaunchGame: TButton;
    btnOpenMissionInEditor: TButton;
    procedure FormShow(Sender: TObject);
    procedure lbMissionListClick(Sender: TObject);
    procedure btnLaunchGameClick(Sender: TObject);
    procedure btnOpenMissionInEditorClick(Sender: TObject);
  private
    loaded: boolean;
    mission_index: integer;
  public
    { Public declarations }
  end;

var
  MissionLauncher: TMissionLauncher;

implementation

uses
  _launcher, _settings, main;

{$R *.dfm}

procedure TMissionLauncher.FormShow(Sender: TObject);
var
  tmp_strings: TStringList;
  i : integer;
begin
  if loaded then
    exit;
  Launcher.load_all_missions;
  tmp_strings := TStringList.Create;
  for i := 0 to Launcher.mission_list.Count - 1 do
    tmp_strings.Add(Launcher.mission_list.Names[i]);
  lbMissionList.Items := tmp_strings;
  tmp_strings.Destroy;
  loaded := True;
end;

procedure TMissionLauncher.lbMissionListClick(Sender: TObject);
begin
  mission_index := strtoint(Launcher.mission_list.ValueFromIndex[lbMissionList.ItemIndex]);
  edMissionName.Text := Launcher.mission_data[mission_index].mission_name;
  edMissionAuthor.Text := Launcher.mission_data[mission_index].author;
  edMissionNumber.Text := inttostr(Launcher.mission_data[mission_index].mission_number);
  edMissionFileName.Text := Launcher.mission_data[mission_index].filename;
  mMissionBriefing.Text := StringReplace(Launcher.mission_data[mission_index].briefing, '_', #13#10, [rfReplaceAll]);
end;

procedure TMissionLauncher.btnLaunchGameClick(Sender: TObject);
begin
  Launcher.launch_mission(mission_index, 0);
end;

procedure TMissionLauncher.btnOpenMissionInEditorClick(Sender: TObject);
begin
  MainWindow.load_map(Settings.MissionsPath + '\' + Launcher.mission_data[mission_index].filename + '.MAP');
end;

end.
