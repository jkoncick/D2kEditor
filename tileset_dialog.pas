unit tileset_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Math, _tileset;

type
  TTilesetDialog = class(TForm)
    TilesetImage: TImage;
    TilesetScroll: TScrollBar;
    TilesetGrid: TCheckBox;
    TilesetMarkTiles: TCheckBox;
    sbCustomSize: TSpeedButton;
    sbPreset11: TSpeedButton;
    sbPreset22: TSpeedButton;
    sbPreset33: TSpeedButton;
    sbPreset44: TSpeedButton;
    sbPreset21: TSpeedButton;
    sbPreset12: TSpeedButton;
    sbPreset32: TSpeedButton;
    sbPreset23: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DrawTileset(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure TilesetImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TilesetImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TilesetImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetBlockSize(Sender: TObject);
  private
    { Private declarations }
    tileset_top: integer;
    tileset_height: integer;

    preset_buttons: array[0..8] of TSpeedButton;
    preset_width: word;
    preset_height: word;

    select_started: boolean;
    select_start_x: integer;
    select_start_y: integer;
    select_end_x: integer;
    select_end_y: integer;
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

procedure TTilesetDialog.FormCreate(Sender: TObject);
begin
  preset_buttons[0] := sbCustomSize;
  preset_buttons[1] := sbPreset11;
  preset_buttons[2] := sbPreset22;
  preset_buttons[3] := sbPreset33;
  preset_buttons[4] := sbPreset44;
  preset_buttons[5] := sbPreset21;
  preset_buttons[6] := sbPreset12;
  preset_buttons[7] := sbPreset32;
  preset_buttons[8] := sbPreset23;
  TilesetImage.Picture.Bitmap.Width := 640;
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
  if TilesetScroll.Position > (40 - tileset_height) then
  begin
    TilesetScroll.Position := 40 - tileset_height;
    exit;
  end;
  tileset_top := TilesetScroll.Position;
  TilesetImage.Canvas.CopyRect(rect(0,0,640,tileset_height*32),Tileset.tileimage.Canvas,rect(0,tileset_top*32,640,tileset_top*32+tileset_height*32));
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
    TilesetImage.Canvas.Pen.Color:= clBlack;
    TilesetImage.Canvas.Pen.Width := 1;
    for x:= 0 to 20-1 do
    begin
      TilesetImage.Canvas.MoveTo(x*32,0);
      TilesetImage.Canvas.LineTo(x*32,tileset_height*32);
    end;
    for y:= 0 to tileset_height-1 do
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
  192: sbCustomSize.Down := true;
  end;
  if (key >= ord('1')) and (key <= ord('8')) then
  begin
    preset_buttons[key - ord('0')].Down := true;
    SetBlockSize(preset_buttons[key - ord('0')]);
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
  if sbCustomSize.Down then
  begin
    // Custom block size
    select_started := true;
    select_start_x := b_left;
    select_start_y := b_top;
    TilesetImageMouseMove(Sender, Shift, X, Y);
    exit;
  end;
  b_width := preset_width;
  b_height := preset_height;
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

procedure TTilesetDialog.TilesetImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  if select_started and ((pos_x <> select_end_x) or (pos_y <> select_end_y)) and (pos_x >= 0) and (pos_y >=0) and (pos_x < 20) and (pos_y < 40) then
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

procedure TTilesetDialog.TilesetImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if select_started then
  begin
    select_started := false;
    MainWindow.select_block_from_tileset(block_width, block_height, block_left, block_top);
    if Button = mbLeft then
    begin
      close;
      MainWindow.RbTileBlock.Checked := True;
    end;
  end;
end;

procedure TTilesetDialog.SetBlockSize(Sender: TObject);
var
  tag: integer;
begin
  tag := (Sender as TSpeedButton).Tag;
  preset_width := block_size_presets[tag][1];
  preset_height := block_size_presets[tag][2];
end;

end.
