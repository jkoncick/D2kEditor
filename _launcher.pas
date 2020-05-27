unit _launcher;

interface

uses
  Classes;

type
  TMissionInfo = record
    filename: String;
    mission_name: String;
    author: String;
    briefing: String;
    text_uib: String;
    mission_number: integer;
    side_id: integer;
  end;

type
  TLauncher = class

  public
    mission_list: TStringList;
    mission_data: Array of TMissionInfo;

  private
    missions_loaded: boolean;

  public
    constructor Create;
    procedure load_all_missions;
    procedure launch_mission(mission_index: integer; difficulty_level: integer);

  end;

var
  Launcher: TLauncher;

implementation

uses
  _settings, Windows, SysUtils, IniFiles, ShellApi;

{ TLauncher }

constructor TLauncher.Create;
begin
  mission_list := TStringList.Create;
end;

procedure TLauncher.load_all_missions;
var
  tmp_strings: TStringList;
  ini: TMemIniFile;
  SR: TSearchRec;
  i: integer;
begin
  if missions_loaded then
    exit;
  tmp_strings := TStringList.Create;
  if FindFirst(Settings.MissionsPath + '\*.INI', 0, SR) = 0 then
  begin
    repeat
      tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  SetLength(Mission_data, tmp_strings.Count);
  mission_list.Capacity := tmp_strings.Count;
  for i := 0 to tmp_strings.Count - 1 do
  begin
    ini := TMemIniFile.Create(Settings.MissionsPath + '\' + tmp_strings[i]);
    mission_data[i].filename := ChangeFileExt(tmp_strings[i],'');
    mission_data[i].mission_name := ini.ReadString('Basic', 'Name', mission_data[i].filename);
    mission_data[i].author := ini.ReadString('Basic', 'Author', '(Unnamed)');
    mission_data[i].briefing := ini.ReadString('Basic', 'Briefing', '');
    mission_data[i].text_uib := ini.ReadString('Basic', 'TextUib', '');
    mission_data[i].mission_number := ini.ReadInteger('Basic', 'MissionNumber', 0);
    mission_data[i].side_id := ini.ReadInteger('Basic', 'SideId', 0);
    mission_list.Add(mission_data[i].mission_name + '=' + inttostr(i));
    ini.Destroy;
  end;
  mission_list.Sort;
  tmp_strings.Destroy;
  missions_loaded := true;
end;

procedure TLauncher.launch_mission(mission_index, difficulty_level: integer);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Settings.GamePath + '\spawn.ini');
  ini.WriteString('Settings', 'Scenario', mission_data[mission_index].filename);
  ini.WriteInteger('Settings', 'MySideID', mission_data[mission_index].side_id);
  ini.WriteInteger('Settings', 'MissionNumber', mission_data[mission_index].mission_number);
  ini.WriteInteger('Settings', 'DifficultyLevel', difficulty_level);
  ini.WriteInteger('Settings', 'Seed', Settings.Seed);
  if mission_data[mission_index].text_uib <> '' then
    ini.WriteString('Settings', 'TextUib', mission_data[mission_index].text_uib)
  else
    ini.DeleteKey('Settings', 'TextUib');
  ini.Destroy;
  ShellExecuteA(0, 'open', PChar(Settings.GameExecutable), PChar('-SPAWN ' + Settings.TestMapParameters), PChar(Settings.GamePath), SW_SHOWNORMAL);
end;

end.
