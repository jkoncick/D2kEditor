unit tileset_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TTilesetDialog = class(TForm)
    TilesetImage: TImage;
    TilesetScroll: TScrollBar;
    TilesetGrid: TCheckBox;
    Block11: TButton;
    Block22: TButton;
    Block33: TButton;
    Block44: TButton;
    Block21: TButton;
    Block12: TButton;
    Block32: TButton;
    Block23: TButton;
    procedure DrawTileset(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure TilesetImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetBlockSize(Sender: TObject);
    procedure TilesetImageDblClick(Sender: TObject);
  private
    { Private declarations }
    tileset_top: word;
    block_width: word;
    block_height: word;
    block_left: word;
    block_top: word;
  public
    { Public declarations }
  end;

var
  TilesetDialog: TTilesetDialog;

implementation

uses
  main;

{$R *.dfm}

{ TTilesetDialog }

procedure TTilesetDialog.DrawTileset(Sender: TObject);
var
  x,y: integer;
begin
  tileset_top := TilesetScroll.Position;
  TilesetImage.Canvas.CopyRect(rect(0,0,640,512),MainWindow.graphics_tileset.Bitmap.Canvas,rect(0,tileset_top*32,640,tileset_top*32+512));
  if TilesetGrid.Checked then
  begin
    TilesetImage.Canvas.Pen.Color:= clBlack;
    TilesetImage.Canvas.Pen.Width := 1;
    for x:= 0 to 20-1 do
    begin
      TilesetImage.Canvas.MoveTo(x*32,0);
      TilesetImage.Canvas.LineTo(x*32,512);
    end;
    for y:= 0 to 16-1 do
    begin
      TilesetImage.Canvas.MoveTo(0,y*32);
      TilesetImage.Canvas.LineTo(640,y*32);
    end;
  end;
  TilesetImage.Canvas.Pen.Color := clRed;
  TilesetImage.Canvas.Pen.Width := 2;
  TilesetImage.Canvas.Brush.Style := bsClear;
  TilesetImage.Canvas.Rectangle(block_left*32+1,(block_top-tileset_top)*32+1,(block_left+block_width)*32+1,(block_top+block_height-tileset_top)*32+1);
end;

procedure TTilesetDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
  27: Close;
  71: TilesetGrid.Checked := not TilesetGrid.Checked;
  49: SetBlockSize(Block11);
  50: SetBlockSize(Block22);
  51: SetBlockSize(Block33);
  52: SetBlockSize(Block44);
  53: SetBlockSize(Block21);
  54: SetBlockSize(Block12);
  55: SetBlockSize(Block32);
  56: SetBlockSize(Block23);
  end;
end;

procedure TTilesetDialog.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScroll.Position := TilesetScroll.Position+1;
end;

procedure TTilesetDialog.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScroll.Position := TilesetScroll.Position-1;
end;

procedure TTilesetDialog.TilesetImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tile_x, tile_y: word;
  block_x, block_y: integer;
  border_x, border_y: integer;
begin
  tile_x := x div 32;
  tile_y := y div 32 + tileset_top;
  if (tile_x + block_width > 20) or (tile_y + block_height > 40) then
    exit;
  block_width := MainWindow.BlockWidth.Value;
  block_height := MainWindow.BlockHeight.Value;
  MainWindow.block_width := block_width;
  MainWindow.block_height := block_height;
  block_left := tile_x;
  block_top := tile_y;
  border_x := (128 - block_width * 32) div 2;
  border_y := (128 - block_height * 32) div 2;
  MainWindow.BlockImage.Canvas.Brush.Color := clBtnFace;
  MainWindow.BlockImage.Canvas.Rectangle(0,0,128,128);
  for block_x:= 0 to block_width-1 do
    for block_y := 0 to block_height-1 do
    begin
      MainWindow.block_data[block_x,block_y] := (block_top+block_y)*20+block_left+block_x;
      MainWindow.BlockImage.Canvas.CopyRect(rect(block_x*32+border_x,block_y*32+border_y,block_x*32+32+border_x,block_y*32+32+border_y),MainWindow.graphics_tileset.Bitmap.Canvas,rect((block_left+block_x)*32,(block_top+block_y)*32,(block_left+block_x)*32+32,(block_top+block_y)*32+32));
    end;
  DrawTileset(nil);
end;

procedure TTilesetDialog.SetBlockSize(Sender: TObject);
begin
  MainWindow.SetBlockSize(sender);
end;

procedure TTilesetDialog.TilesetImageDblClick(Sender: TObject);
begin
  MainWindow.RbCustomBlock.Checked := True;
  close;
end;

end.
