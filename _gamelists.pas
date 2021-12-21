unit _gamelists;

interface

uses Classes;

type  GameListType =                           (glNone, glUnitBehavior, glUnitFlag, glUnitState, glBuildingBehavior, glBuildingFlag, glBuildingState, glCrateType, glCrateImage, glTileAtr);
const GameListTypeStr: array[0..9] of String = ('None', 'UnitBehavior', 'UnitFlag', 'UnitState', 'BuildingBehavior', 'BuildingFlag', 'BuildingState', 'CrateType', 'CrateImage', 'TileAtr');

type
  TGameLists = class

  public
    lists: array[0..9] of TStringList;

  public
    procedure init;
  private
    procedure load_game_lists_txt;
  public
    function get_game_list_type(name: string): integer;
    function get_numbered_list(list_type: GameListType): TStringList;
  end;

var
  GameLists: TGameLists;

implementation

uses SysUtils, _dispatcher, _utils;

procedure TGameLists.init;
begin
  load_game_lists_txt;
  Dispatcher.register_event(evLoadGameLists);
end;

procedure TGameLists.load_game_lists_txt;
var
  tmp_filename: String;
  i: integer;
  f: TextFile;
  line, section: string;
  game_list_type: integer;
begin
  tmp_filename := find_file('config\game_lists.txt', 'configuration');
  if tmp_filename = '' then
    exit;
  for i := 0 to High(lists) do
    lists[i] := TStringList.Create;
  AssignFile(f, tmp_filename);
  Reset(f);
  game_list_type := 0;
  while not Eof(f) do
  begin
    Readln(f, line);
    if Length(line) = 0 then
      continue;
    if line[1] = '[' then
    begin
      section := copy(line, 2, Length(line) - 2);
      game_list_type := get_game_list_type(section);
      continue;
    end;
    if game_list_type = 0 then
      continue;
    lists[game_list_type].Add(line);
  end;
  CloseFile(f);
end;

function TGameLists.get_game_list_type(name: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to High(GameListTypeStr) do
    if name = GameListTypeStr[i] then
    begin
      result := i;
      exit;
    end;
end;

function TGameLists.get_numbered_list(list_type: GameListType): TStringList;
var
  i: integer;
begin
  result := TStringList.Create;
  for i := 0 to lists[Ord(list_type)].Count - 1 do
    result.Add(IntToStr(i) + ' - ' + lists[Ord(list_type)][i]);
end;

end.
