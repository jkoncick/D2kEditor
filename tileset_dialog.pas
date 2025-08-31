unit tileset_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Math, _tileset, Spin;

type
  TTilesetDialog = class(TForm)
    TilesetImage: TImage;
    TilesetScroll: TScrollBar;
    TilesetGrid: TCheckBox;
    TilesetMarkTiles: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DrawTileset(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure TilesetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TilesetImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TilesetImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  private
    tileset_top: integer;
    tileset_height: integer;

    select_started: boolean;
    select_start_x: integer;
    select_start_y: integer;
    select_end_x: integer;
    select_end_y: integer;

    block_width: word;
    block_height: word;
    block_left: word;
    block_top: word;

    pending_update_tileset: boolean;
  public
    // Dispatcher events
    procedure update_tileset;
    procedure update_grid_color;
  end;

var
  TilesetDialog: TTilesetDialog;

implementation

uses
  Clipbrd, main, block_preset_dialog, _settings;

{$R *.dfm}

{ TTilesetDialog }

procedure TTilesetDialog.FormCreate(Sender: TObject);
begin
  TilesetImage.Picture.Bitmap.Width := tileimage_width;
end;

procedure TTilesetDialog.FormShow(Sender: TObject);
begin
  if pending_update_tileset then
    update_tileset;
end;

procedure TTilesetDialog.FormHide(Sender: TObject);
begin
  MainWindow.block_preset_dialog_opened := false;
end;

procedure TTilesetDialog.FormResize(Sender: TObject);
begin
  tileset_height := (ClientHeight - 32) div 32;
  TilesetImage.Height := tileset_height * 32;
  TilesetImage.Picture.Bitmap.Height := tileset_height * 32;
  TilesetScroll.Height := tileset_height * 32;
  TilesetScroll.PageSize := tileset_height;
  DrawTileset(nil);
end;

procedure TTilesetDialog.DrawTileset(Sender: TObject);
var
  x,y: integer;
  tile_attr: TileType;
begin
  if TilesetScroll.Position > (Tileset.cnt_tiles div 20 - tileset_height) then
  begin
    TilesetScroll.Position := Tileset.cnt_tiles div 20 - tileset_height;
    exit;
  end;
  if Tileset.tileimage = nil then
    exit;
  tileset_top := TilesetScroll.Position;
  TilesetImage.Canvas.CopyRect(rect(0,0,tileimage_width,tileset_height*32),Tileset.tileimage.Canvas,rect(0,tileset_top*32,tileimage_width,tileset_top*32+tileset_height*32));
  if TilesetMarkTiles.Checked then
    for x := 0 to 20 - 1 do
      for y := tileset_top to tileset_top + tileset_height - 1 do
      begin
        tile_attr := Tileset.get_tile_type(x + y * 20);
        if (tile_attr = ttImpassable) or (tile_attr = ttInfantryOnly) or (tile_attr = ttBuildable) then
        begin
          if (tile_attr = ttImpassable) then
            TilesetImage.Canvas.Pen.Color := clRed
          else if (tile_attr = ttInfantryOnly) then
            TilesetImage.Canvas.Pen.Color := $4080FF
          else if (tile_attr = ttBuildable) then
            TilesetImage.Canvas.Pen.Color := $40FF80;
          TilesetImage.Canvas.Pen.Width := 2;
          TilesetImage.Canvas.MoveTo(x*32, (y-tileset_top)*32);
          TilesetImage.Canvas.LineTo(x*32+31, (y-tileset_top)*32+31);
          TilesetImage.Canvas.MoveTo(x*32+31, (y-tileset_top)*32);
          TilesetImage.Canvas.LineTo(x*32, (y-tileset_top)*32+31);
        end;
      end;
  if TilesetGrid.Checked then
  begin
    TilesetImage.Canvas.Pen.Color:= Settings.GridColor;
    TilesetImage.Canvas.Pen.Width := 1;
    for x:= 0 to 20-1 do
    begin
      TilesetImage.Canvas.MoveTo(x*32,0);
      TilesetImage.Canvas.LineTo(x*32,tileset_height*32);
    end;
    for y:= 0 to tileset_height-1 do
    begin
      TilesetImage.Canvas.MoveTo(0,y*32);
      TilesetImage.Canvas.LineTo(tileimage_width,y*32);
    end;
  end;
  TilesetImage.Canvas.Pen.Color := clRed;
  TilesetImage.Canvas.Pen.Width := 2;
  TilesetImage.Canvas.Brush.Style := bsClear;
  TilesetImage.Canvas.Rectangle(block_left*32+1,(block_top-tileset_top)*32+1,(block_left+block_width)*32+1,(block_top+block_height-tileset_top)*32+1);
end;

procedure TTilesetDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    27: Close;
    32: begin BlockPresetDialog.Show; Hide; key := 0; end;
    71: TilesetGrid.Checked := not TilesetGrid.Checked;
    77: TilesetMarkTiles.Checked := not TilesetMarkTiles.Checked;
  end;
end;

procedure TTilesetDialog.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScroll.Position := TilesetScroll.Position + 2;
  Handled := true;
end;

procedure TTilesetDialog.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScroll.Position := TilesetScroll.Position - 2;
  Handled := true;
end;

procedure TTilesetDialog.TilesetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  select_started := true;
  select_start_x := pos_x;
  select_start_y := pos_y;
  TilesetImageMouseMove(Sender, Shift, X, Y);
end;

procedure TTilesetDialog.TilesetImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  if select_started and ((pos_x <> select_end_x) or (pos_y <> select_end_y)) and (pos_x >= 0) and (pos_y >=0) and (pos_x < 20) and (pos_y < (Tileset.cnt_tiles div 20)) then
  begin
    select_end_x := pos_x;
    select_end_y := pos_y;
    block_left := min(select_start_x, select_end_x);
    block_width := max(select_start_x, select_end_x) - block_left + 1;
    block_top := min(select_start_y, select_end_y);
    block_height := max(select_start_y, select_end_y) - block_top + 1;
    DrawTileset(nil);
  end;
end;

procedure TTilesetDialog.TilesetImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not select_started then
    exit;
  select_started := false;
  MainWindow.select_block_from_tileset(block_width, block_height, block_left, block_top);
  if Button = mbLeft then
  begin
    close;
    MainWindow.RbBlockMode.Checked := True;
  end;
end;

procedure TTilesetDialog.update_tileset;
begin
  if not Visible then
  begin
    pending_update_tileset := true;
    exit;
  end;
  pending_update_tileset := false;
  TilesetScroll.Max := Tileset.cnt_tiles div 20 - 1;
  DrawTileset(nil);
end;

procedure TTilesetDialog.update_grid_color;
begin
  if TilesetGrid.Checked then
    DrawTileset(nil);
end;

end.
