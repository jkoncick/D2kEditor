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
    cbDifficultyLevel: TComboBox;
    lbDifficultyLevel: TLabel;
    lbMissionCampaignFolder: TLabel;
    edMissionCampaignFolder: TEdit;
    lbMissionModsFolder: TLabel;
    edMissionModsFolder: TEdit;
    btnOpenCampaignFolder: TButton;
    btnOpenModsFolder: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbMissionListClick(Sender: TObject);
    procedure btnLaunchGameClick(Sender: TObject);
    procedure btnOpenMissionInEditorClick(Sender: TObject);
    procedure btnOpenCampaignFolderClick(Sender: TObject);
    procedure btnOpenModsFolderClick(Sender: TObject);
  private
    mission_index: integer;
  public
    { Public declarations }
  end;

var
  MissionLauncher: TMissionLauncher;

implementation

uses
  _launcher, _settings, main, ShellApi;

{$R *.dfm}

procedure TMissionLauncher.FormShow(Sender: TObject);
var
  tmp_strings: TStringList;
  i : integer;
begin
  if Launcher.missions_loaded then
    exit;
  Launcher.load_all_missions;
  tmp_strings := TStringList.Create;
  for i := 0 to Launcher.mission_list.Count - 1 do
    tmp_strings.Add(Launcher.mission_list.Names[i]);
  lbMissionList.Items := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TMissionLauncher.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27: close;
    13: btnLaunchGameClick(nil);
  end;
end;

procedure TMissionLauncher.lbMissionListClick(Sender: TObject);
begin
  if lbMissionList.ItemIndex = -1 then
    exit;
  mission_index := strtoint(Launcher.mission_list.ValueFromIndex[lbMissionList.ItemIndex]);
  with (Launcher.mission_data[mission_index]) do
  begin
    edMissionName.Text := mission_name;
    edMissionAuthor.Text := author;
    edMissionNumber.Text := inttostr(mission_number);
    edMissionFileName.Text := filename;
    edMissionCampaignFolder.Text := campaign_folder;
    if (campaign_folder <> '') and not DirectoryExists(Settings.GamePath + '\CustomCampaignData\' + campaign_folder) then
      edMissionCampaignFolder.Color := clRed
    else
      edMissionCampaignFolder.Color := clWhite;
    edMissionModsFolder.Text := mods_folder;
    if (mods_folder <> '') and not DirectoryExists(Settings.GamePath + '\CustomCampaignData\' + campaign_folder + '\' + mods_folder) then
      edMissionModsFolder.Color := clRed
    else
      edMissionModsFolder.Color := clWhite;
    mMissionBriefing.Text := StringReplace(briefing, '_', #13#10, [rfReplaceAll]);
  end;
end;

procedure TMissionLauncher.btnLaunchGameClick(Sender: TObject);
begin
  if lbMissionList.ItemIndex = -1 then
    exit;
  Launcher.launch_mission(mission_index, cbDifficultyLevel.ItemIndex);
end;

procedure TMissionLauncher.btnOpenMissionInEditorClick(Sender: TObject);
begin
  if lbMissionList.ItemIndex = -1 then
    exit;
  MainWindow.load_map(Settings.MissionsPath + '\' + Launcher.mission_data[mission_index].filename + '.MAP');
  if sender = btnOpenMissionInEditor then
    close;
end;

procedure TMissionLauncher.btnOpenCampaignFolderClick(Sender: TObject);
begin
  if edMissionCampaignFolder.Text = '' then
    exit;
  ShellExecute(0, 'open', PChar(Settings.GamePath + '\CustomCampaignData\' + edMissionCampaignFolder.Text), nil, nil, SW_SHOWNORMAL);
end;

procedure TMissionLauncher.btnOpenModsFolderClick(Sender: TObject);
begin
  if (edMissionCampaignFolder.Text = '') or (edMissionModsFolder.Text = '') then
    exit;
  ShellExecute(0, 'open', PChar(Settings.GamePath + '\CustomCampaignData\' + edMissionCampaignFolder.Text + '\' + edMissionModsFolder.Text), nil, nil, SW_SHOWNORMAL);
end;

end.
