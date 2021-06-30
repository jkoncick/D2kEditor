unit _misai;

interface

// Internal configuration types
type
  TMisAIProperty = record
    name: String;
    data_type: char;
    position: integer;
  end;

  TMisAIPropertyPtr = ^TMisAIProperty;

// Game types and constants
type
  TMisAISegment = array[0..7607] of byte;

type
  TDefenceArea = record
    MinX: byte;
    MaxX: byte;
    MinY: byte;
    MaxY: byte;
    Unknown1: Cardinal;
    Strength: Cardinal;
    StartDelay: Cardinal;
    Unknown: Cardinal;
  end;

  TDefenceAreaPtr = ^TDefenceArea;

const CNT_DEFENCE_AREAS = 5;
const DEFENCE_AREA_SIZE = SizeOf(TDefenceArea);
const DEFENCE_AREAS_COUNT_BYTE = 7505;
const DEFENCE_AREAS_START_BYTE = 7508;
const DEFENCE_AREAS_END_BYTE = DEFENCE_AREAS_START_BYTE + DEFENCE_AREA_SIZE * CNT_DEFENCE_AREAS - 1;

type
  TMisAI = class

  private
    mis_ai_properties_template: array of TMisAIProperty;
    mis_ai_properties: array of TMisAIProperty;
  public
    cnt_mis_ai_properties: integer;
    default_ai: TMisAISegment;
  private
    misai_clipboard_format: cardinal;
  public
    // MisAI configuration related procedures
    procedure init;
    procedure load_mis_ai_properties_ini;
    procedure cache_mis_ai_property_list;
    procedure load_default_ai;
    function get_misai_property(index: integer): TMisAIPropertyPtr;
    // MisAI segment manipulation procedures
    procedure init_misai_segment(var misai_segment: TMisAISegment; player_number: integer);
    procedure load_misai_segment(filename: String; var misai_segment: TMisAISegment);
    procedure save_misai_segment(filename: String; var misai_segment: TMisAISegment);
    procedure copy_misai_segment_to_clipboard(var misai_segment: TMisAISegment);
    function paste_misai_segment_from_clipboard(var misai_segment: TMisAISegment): boolean;
    function get_defence_area(var misai_segment: TMisAISegment; defence_area_num: integer): TDefenceAreaPtr;
  end;

var
  MisAI: TMisAI;

implementation

uses Windows, Forms, SysUtils, Classes, IniFiles, Clipbrd, _utils, _structures;

procedure TMisAI.init;
begin
  load_mis_ai_properties_ini;
  load_default_ai;
  // Register AI clipboard format
  misai_clipboard_format := RegisterClipboardFormat('D2kEditorMisAISegment');
end;

procedure TMisAI.load_mis_ai_properties_ini;
var
  tmp_filename: String;
  i: integer;
  ini: TMemIniFile;
  tmp_strings: TStringList;
begin
  tmp_filename := find_file('config\mis_ai_properties.ini', 'configuration');
  if tmp_filename = '' then
    exit;
  // Load misai properties from ini file
  tmp_strings := TStringList.Create;
  ini := TMemIniFile.Create(tmp_filename);
  ini.ReadSection('AI',tmp_strings);
  SetLength(mis_ai_properties_template, tmp_strings.Count);
  SetLength(mis_ai_properties, tmp_strings.Count);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    mis_ai_properties_template[i].name := ini.ReadString('AI',tmp_strings[i],'');
    mis_ai_properties_template[i].data_type := tmp_strings[i][1];
    mis_ai_properties_template[i].position := strtoint(Copy(tmp_strings[i], 3, Length(tmp_strings[i]) - 2));
  end;
  ini.Destroy;
  tmp_strings.Destroy;
end;

procedure TMisAI.cache_mis_ai_property_list;
var
  i, position, num: integer;
  name: String;
begin
  cnt_mis_ai_properties := 0;
  for i := 0 to Length(mis_ai_properties_template) - 1 do
  begin
    name := mis_ai_properties_template[i].name;
    // Replace building group
    position := Pos('B#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+2, 2), MAX_BUILDING_TYPES);
      if num < Structures.templates.BuildingGroupCount then
        name := Copy(name, 0, position-1) + Structures.templates.BuildingGroupStrings[num]
      else
        continue;
    end;
    // Replace building name
    position := Pos('B2#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+3, 2), MAX_BUILDING_TYPES);
      if num < Structures.templates.BuildingCount then
        name := Copy(name, 0, position-1) + Structures.prettify_structure_name(Structures.templates.BuildingNameStrings[num])
      else
        continue;
    end;
    // Replace unit name
    position := Pos('U#', name);
    if position > 0 then
    begin
      num := strtointdef(Copy(name, position+2, 2), MAX_UNIT_TYPES);
      if num < Structures.templates.UnitCount then
        name := Copy(name, 0, position-1) + Structures.templates.UnitNameStrings[num]
      else
        continue;
    end;
    mis_ai_properties[cnt_mis_ai_properties].name := name;
    mis_ai_properties[cnt_mis_ai_properties].data_type := mis_ai_properties_template[i].data_type;
    mis_ai_properties[cnt_mis_ai_properties].position := mis_ai_properties_template[i].position;
    inc(cnt_mis_ai_properties);
  end;
end;

procedure TMisAI.load_default_ai;
var
  tmp_filename: String;
begin
  tmp_filename := find_file('config\default_ai.misai', 'default mission AI');
  if tmp_filename = '' then
    exit;
  load_misai_segment(tmp_filename, default_ai);
end;

function TMisAI.get_misai_property(index: integer): TMisAIPropertyPtr;
begin
  result := Addr(mis_ai_properties[index]);
end;

procedure TMisAI.init_misai_segment(var misai_segment: TMisAISegment; player_number: integer);
begin
  misai_segment[0] := player_number;
  Move(default_ai[1], misai_segment[1], Length(misai_segment)-1);
end;

procedure TMisAI.load_misai_segment(filename: String; var misai_segment: TMisAISegment);
begin
  load_binary_file(filename, misai_segment[1], Length(misai_segment)-1);
end;

procedure TMisAI.save_misai_segment(filename: String; var misai_segment: TMisAISegment);
begin
  save_binary_file(filename, misai_segment[1], Length(misai_segment)-1);
end;

procedure TMisAI.copy_misai_segment_to_clipboard(var misai_segment: TMisAISegment);
var
  handle: THandle;
  pointer: ^TMisAISegment;
begin
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, sizeof(TMisAISegment));
  pointer := GlobalLock(handle);

  Move(misai_segment[1], pointer[1], Length(pointer^) - 1);

  GlobalUnLock(handle);
  SetClipboardData(misai_clipboard_format, handle);
  CloseClipboard;
end;

function TMisAI.paste_misai_segment_from_clipboard(var misai_segment: TMisAISegment): boolean;
var
  handle: THandle;
  pointer: ^TMisAISegment;
begin
  result := false;
  if not Clipboard.HasFormat(misai_clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(misai_clipboard_format);
  pointer := GlobalLock(handle);

  Move(pointer[1], misai_segment[1], Length(pointer^) - 1);

  GlobalUnLock(handle);
  CloseClipboard;
  result := true;
end;

function TMisAI.get_defence_area(var misai_segment: TMisAISegment; defence_area_num: integer): TDefenceAreaPtr;
begin
  result := Addr(misai_segment[DEFENCE_AREAS_START_BYTE + (defence_area_num * DEFENCE_AREA_SIZE)]);
end;

end.
