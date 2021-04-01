unit _sounds;

interface

type
  TSoundEntry = record
    name: string;
    pos: integer;
    size: integer;
  end;

type
  TSounds = class

  public
    sound_rs_filename: String;
    sound_rs_directory: array of TSoundEntry;
    sound_rs_data_offset: integer;
    sound_rs_data: array of byte;
    sound_rs_modified: boolean;

    procedure init;
    procedure load_sound_rs(force: boolean);
    procedure save_sound_rs;
    procedure cache_sound_rs_data;
    function find_sound(name: string): integer;
    procedure play_sound(index: integer);
    function check_sound_size(size: integer): boolean;
    procedure export_sound(index: integer; filename: string);
    procedure replace_sound(index: integer; filename: string);
    procedure add_new_sound(filename: string);
    procedure remove_last_sound;
    procedure rename_sound(index: integer; name: string);

  end;

var
  Sounds: TSounds;

implementation

uses Forms, Windows, SysUtils, MMSystem, _utils, _dispatcher;

{ TSounds }

procedure TSounds.init;
begin
  load_sound_rs(false);
end;

procedure TSounds.load_sound_rs(force: boolean);
var
  tmp_filename: String;
  f: file of byte;
  directory_buffer: array of byte;
  directory_size: integer;
  pos, numrecs: integer;
  srec: TDummyStringRecPtr;
  slen: integer;
begin
  tmp_filename := find_file('Data\GAMESFX\SOUND.RS', 'game');
  if (tmp_filename = '') or ((tmp_filename = sound_rs_filename) and not force) then
    exit;
  sound_rs_filename := tmp_filename;
  // Clean up previously cached data
  SetLength(sound_rs_data, 0);
  // Load directory data from file
  AssignFile(f, tmp_filename);
  Reset(f);
  BlockRead(f, directory_size, 4);
  SetLength(directory_buffer, directory_size);
  BlockRead(f, directory_buffer[0], directory_size);
  CloseFile(f);
  // Determine number of entries
  pos := 0;
  numrecs := 0;
  while pos < directory_size do
  begin
    srec := Addr(directory_buffer[pos]);
    Inc(pos, Length(String(srec.str)) + 9);
    Inc(numrecs);
  end;
  // Load directory
  SetLength(sound_rs_directory, numrecs);
  pos := 0;
  numrecs := 0;
  sound_rs_data_offset := directory_size + 4;
  while pos < directory_size do
  begin
    srec := Addr(directory_buffer[pos]);
    sound_rs_directory[numrecs].name := srec.str;
    slen := Length(sound_rs_directory[numrecs].name);
    sound_rs_directory[numrecs].pos := get_integer_value(directory_buffer, pos + slen + 1, 4) - sound_rs_data_offset;
    sound_rs_directory[numrecs].size := get_integer_value(directory_buffer, pos + slen + 5, 4);
    Inc(pos, slen + 9);
    Inc(numrecs);
  end;
  SetLength(directory_buffer, 0);
  sound_rs_modified := false;
  // Register event in dispatcher
  Dispatcher.register_event(evFLTemplatesBin);
end;

procedure TSounds.save_sound_rs;
var
  f: file of byte;
  directory_buffer: array of byte;
  directory_size: integer;
  i, pos, slen: integer;
begin
  if (sound_rs_filename = '') or not sound_rs_modified then
    exit;
  if not manage_filesave(sound_rs_filename, 'Data\GAMESFX\SOUND.RS', evStructuresFilenameChange) then
    exit;
  // Determine directory size
  directory_size := 0;
  for i := 0 to Length(sound_rs_directory) - 1 do
    Inc(directory_size, Length(sound_rs_directory[i].name) + 9);
  SetLength(directory_buffer, directory_size);
  sound_rs_data_offset := directory_size + 4;
  // Fill directory buffer
  pos := 0;
  for i := 0 to Length(sound_rs_directory) - 1 do
  begin
    slen := Length(sound_rs_directory[i].name);
    store_c_string(sound_rs_directory[i].name, Addr(directory_buffer[pos]), slen + 1);
    set_integer_value(directory_buffer, pos + slen + 1, 4, sound_rs_directory[i].pos + sound_rs_data_offset);
    set_integer_value(directory_buffer, pos + slen + 5, 4, sound_rs_directory[i].size);
    inc(pos, slen + 9);
  end;
  // Write file
  AssignFile(f, sound_rs_filename);
  Rewrite(f);
  BlockWrite(f, directory_size, 4);
  BlockWrite(f, directory_buffer[0], directory_size);
  BlockWrite(f, sound_rs_data[0], Length(sound_rs_data));
  CloseFile(f);
  SetLength(directory_buffer, 0);
  sound_rs_modified := false;
end;

procedure TSounds.cache_sound_rs_data;
var
  f: file of byte;
  file_size: integer;
begin
  // Check if data are already cached
  if Length(sound_rs_data) > 0 then
    exit;
  // Load data from file
  AssignFile(f, sound_rs_filename);
  Reset(f);
  file_size := FileSize(f);
  SetLength(sound_rs_data, file_size - sound_rs_data_offset);
  Seek(f, sound_rs_data_offset);
  BlockRead(f, sound_rs_data[0], file_size - sound_rs_data_offset);
  CloseFile(f);
end;

function TSounds.find_sound(name: string): integer;
var
  cmp_str: string;
  i: integer;
begin
  result := -1;
  cmp_str := name + '.WAV';
  for i := 0 to Length(sound_rs_directory) - 1 do
    if AnsiCompareText(cmp_str, sound_rs_directory[i].name) = 0 then
    begin
      result := i;
      exit;
    end;
end;

procedure TSounds.play_sound(index: integer);
begin
  if (index < 0) or (index >= Length(sound_rs_directory)) then
    exit;
  cache_sound_rs_data;
  PlaySound(Addr(sound_rs_data[sound_rs_directory[index].pos]), 0, SND_MEMORY or SND_ASYNC);
end;

function TSounds.check_sound_size(size: integer): boolean;
begin
  result := size <= 65536;
  if not result then
    Application.MessageBox(PChar(Format('Cannot import sound bigger than 65536 bytes (actual %d bytes)', [size])), 'Import sound', MB_ICONERROR or MB_OK);
end;

procedure TSounds.export_sound(index: integer; filename: string);
begin
  if (index < 0) or (index >= Length(sound_rs_directory)) then
    exit;
  cache_sound_rs_data;
  save_binary_file(filename, sound_rs_data[sound_rs_directory[index].pos], sound_rs_directory[index].size);
end;

procedure TSounds.replace_sound(index: integer; filename: string);
var
  f: file of byte;
  pos, old_size, new_size, old_total_size: integer;
  i: integer;
begin
  if (index < 0) or (index >= Length(sound_rs_directory)) then
    exit;
  cache_sound_rs_data;
  AssignFile(f, filename);
  Reset(f);
  pos := sound_rs_directory[index].pos;
  old_size := sound_rs_directory[index].size;
  new_size := FileSize(f);
  old_total_size := Length(sound_rs_data);
  if not check_sound_size(new_size) then
  begin
    CloseFile(f);
    exit;
  end;
  // Shift data
  if new_size > old_size then
  begin
    SetLength(sound_rs_data, Length(sound_rs_data) + new_size - old_size);
    Move(sound_rs_data[pos], sound_rs_data[pos + new_size - old_size], old_total_size - pos);
  end
  else if new_size < old_size then
  begin
    Move(sound_rs_data[pos + old_size - new_size], sound_rs_data[pos], old_total_size - (pos + old_size - new_size));
    SetLength(sound_rs_data, Length(sound_rs_data) + new_size - old_size);
  end;
  // Shift positions of following entries
  for i := index + 1 to Length(sound_rs_directory) - 1 do
    Inc(sound_rs_directory[i].pos, new_size - old_size);
  // Import sound data
  BlockRead(f, sound_rs_data[pos], new_size);
  sound_rs_directory[index].size := new_size;
  CloseFile(f);
  sound_rs_modified := true;
end;

procedure TSounds.add_new_sound(filename: string);
var
  f: file of byte;
  new_index, new_pos, new_size: integer;
begin
  cache_sound_rs_data;
  AssignFile(f, filename);
  Reset(f);
  new_index := Length(sound_rs_directory);
  new_pos := Length(sound_rs_data);
  new_size := FileSize(f);
  if not check_sound_size(new_size) then
  begin
    CloseFile(f);
    exit;
  end;
  // Add new entry to directory
  SetLength(sound_rs_directory, Length(sound_rs_directory) + 1);
  sound_rs_directory[new_index].name := ExtractFileName(filename);
  sound_rs_directory[new_index].pos := new_pos;
  sound_rs_directory[new_index].size := new_size;
  // Import sound data
  SetLength(sound_rs_data, Length(sound_rs_data) + new_size);
  BlockRead(f, sound_rs_data[new_pos], new_size);
  CloseFile(f);
  sound_rs_modified := true;
end;

procedure TSounds.remove_last_sound;
begin
  if Length(sound_rs_directory) = 0 then
    exit;
  cache_sound_rs_data;
  SetLength(sound_rs_data, Length(sound_rs_data) - sound_rs_directory[Length(sound_rs_directory) - 1].size);
  SetLength(sound_rs_directory, Length(sound_rs_directory) - 1);
  sound_rs_modified := true;
end;

procedure TSounds.rename_sound(index: integer; name: string);
begin
  sound_rs_directory[index].name := name;
  sound_rs_modified := true;
end;

end.
