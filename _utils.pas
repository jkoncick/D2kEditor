unit _utils;

interface

uses
  Controls, Messages, ExtCtrls;

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

end.
 