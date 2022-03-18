unit _gamelists;

interface

uses Classes;

type
  TGameLists = class

  private
    list_index: TStringList;
    lists: array of TStringList;

  public
    procedure init;
  private
    procedure load_game_lists_ini;
  public
    function get_list_index(name: string): integer;
    function get_list_ref(index: integer): TStringList; overload;
    function get_list_ref(name: string): TStringList; overload;
    function get_numbered_list(index: integer): TStringList; overload;
    function get_numbered_list(name: string): TStringList; overload;
  end;

var
  GameLists: TGameLists;

implementation

uses SysUtils, _dispatcher, _utils;

procedure TGameLists.init;
begin
  list_index := TStringList.Create;
  list_index.Add('None');
  SetLength(lists, 1);
  lists[0] := TStringList.Create;
  load_game_lists_ini;
  Dispatcher.register_event(evLoadGameLists);
end;

procedure TGameLists.load_game_lists_ini;
var
  tmp_filename: String;
  f: TextFile;
  line, section: string;
begin
  tmp_filename := find_file('config\game_lists.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  AssignFile(f, tmp_filename);
  Reset(f);
  while not Eof(f) do
  begin
    Readln(f, line);
    if Length(line) = 0 then
      continue;
    if line[1] = '[' then
    begin
      section := copy(line, 2, Length(line) - 2);
      list_index.Add(section);
      SetLength(lists, list_index.Count);
      lists[list_index.Count-1] := TStringList.Create;
      continue;
    end;
    lists[list_index.Count-1].Add(line);
  end;
  CloseFile(f);
end;

function TGameLists.get_list_index(name: string): integer;
var
  i: integer;
begin
  for i := 0 to list_index.Count - 1 do
    if list_index[i] = name then
      begin
        result := i;
        exit;
      end;
  result := 0;
end;

function TGameLists.get_list_ref(index: integer): TStringList;
begin
  result := lists[index];
end;

function TGameLists.get_list_ref(name: string): TStringList;
begin
  result := lists[get_list_index(name)];
end;

function TGameLists.get_numbered_list(index: integer): TStringList;
var
  i: integer;
begin
  result := TStringList.Create;
  for i := 0 to lists[index].Count - 1 do
    result.Add(IntToStr(i) + ' - ' + lists[index][i]);
end;

function TGameLists.get_numbered_list(name: string): TStringList;
begin
  result := get_numbered_list(get_list_index(name));
end;

end.
