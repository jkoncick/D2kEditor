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
    TilesetMarkTiles: TCheckBox;
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
  private
    { Private declarations }
    tileset_top: word;
  public
    { Public declarations }
    block_width: word;
    block_height: word;
    block_left: word;
    block_top: word;
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
  tile_attr: TileType;
begin
  tileset_top := TilesetScroll.Position;
  TilesetImage.Canvas.CopyRect(rect(0,0,640,576),MainWindow.graphics_tileset.Bitmap.Canvas,rect(0,tileset_top*32,640,tileset_top*32+576));
  if TilesetMarkTiles.Checked then
    for x := 0 to 20 - 1 do
      for y := tileset_top to tileset_top + 18 - 1 do
      begin
        tile_attr := MainWindow.get_tile_type(MainWindow.tileset_attributes[x + y * 20]);
        if (tile_attr = ttImpassable) or (tile_attr = ttInfantryOnly) then
        begin
          if (tile_attr = ttImpassable) then
            TilesetImage.Canvas.Pen.Color := clRed
          else if (tile_attr = ttInfantryOnly) then
            TilesetImage.Canvas.Pen.Color := $4080FF;
          TilesetImage.Canvas.Pen.Width := 2;
          TilesetImage.Canvas.MoveTo(x*32, (y-tileset_top)*32);
          TilesetImage.Canvas.LineTo(x*32+31, (y-tileset_top)*32+31);
          TilesetImage.Canvas.MoveTo(x*32+31, (y-tileset_top)*32);
          TilesetImage.Canvas.LineTo(x*32, (y-tileset_top)*32+31);
        end;
      end;
  if TilesetGrid.Checked then
  begin
    TilesetImage.Canvas.Pen.Color:= clBlack;
    TilesetImage.Canvas.Pen.Width := 1;
    for x:= 0 to 20-1 do
    begin
      TilesetImage.Canvas.MoveTo(x*32,0);
      TilesetImage.Canvas.LineTo(x*32,576);
    end;
    for y:= 0 to 18-1 do
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
  77: TilesetMarkTiles.Checked := not TilesetMarkTiles.Checked;
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
  TilesetScroll.Position := TilesetScroll.Position + 2;
  Handled := true;
end;

procedure TTilesetDialog.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScroll.Position := TilesetScroll.Position - 2;
  Handled := true;
end;

procedure TTilesetDialog.TilesetImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  b_left, b_top: word;
  b_width, b_height: word;
begin
  b_left := X div 32;
  b_top := Y div 32 + tileset_top;
  b_width := MainWindow.BlockWidth.Value;
  b_height := MainWindow.BlockHeight.Value;
  if (b_left + b_width > 20) or (b_top + b_height > 40) then
    exit;
  MainWindow.select_block_from_tileset(b_width, b_height, b_left, b_top);
  DrawTileset(nil);
  if Button = mbLeft then
  begin
    close;
    MainWindow.RbTileBlock.Checked := True;
  end;
end;

procedure TTilesetDialog.SetBlockSize(Sender: TObject);
begin
  MainWindow.SetBlockSize(sender);
end;

end.
