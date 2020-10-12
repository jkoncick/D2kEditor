unit _utils;

interface

uses
  Controls, Messages, ExtCtrls, SysUtils;

const CNT_PLAYERS = 8;

type
  TByteArray = array[0..0] of byte;
  TByteArrayPtr = ^TByteArray;
  TWordArray = array[0..0] of word;
  TWordArrayPtr = ^TWordArray;
  TCardinalArray = array[0..0] of cardinal;
  TCardinalArrayPtr = ^TCardinalArray;

type
  TImage = class(ExtCtrls.TImage)
    protected
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  end;

procedure store_c_string(source: String; target_ptr: TByteArrayPtr; target_size: integer);
function IntToBin(val: integer; width: integer): string;
function BinToInt(bin: string): integer;

implementation

uses
  main;

procedure TImage.CMMouseLeave(var Message: TMessage);
begin
  MainWindow.ImageMouseLeave(self);
end;

procedure store_c_string(source: String; target_ptr: TByteArrayPtr; target_size: integer);
begin
  FillChar(target_ptr^, target_size, 0);
  Move(source[1], target_ptr^, Length(source));
end;

function IntToBin(val: integer; width: integer): string;
begin
  result := '';
  while val > 0 do
    begin
      result := IntToStr(val and 1) + result;
      val := val shr 1;
    end;
  while Length(result) < width do
    result := '0' + result;
end;

function BinToInt(bin: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(bin) do
    begin
      result := result shl 1;
      result := result or StrToInt(bin[i]);
    end;
end;

end.
 