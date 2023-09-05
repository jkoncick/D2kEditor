unit _misai;

interface

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

  public
    default_ai: TMisAISegment;
  private
    misai_clipboard_format: cardinal;
  public
    // MisAI configuration related procedures
    procedure init;
    procedure load_default_ai;
    // MisAI segment manipulation procedures
    procedure init_misai_segment(var misai_segment: TMisAISegment; side_number: integer);
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
  load_default_ai;
  // Register AI clipboard format
  misai_clipboard_format := RegisterClipboardFormat('D2kEditorMisAISegment');
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

procedure TMisAI.init_misai_segment(var misai_segment: TMisAISegment; side_number: integer);
begin
  misai_segment[0] := side_number;
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
