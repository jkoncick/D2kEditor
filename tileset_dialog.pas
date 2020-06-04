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
    sbCustomSize: TSpeedButton;
    sbPreset11: TSpeedButton;
    sbPreset22: TSpeedButton;
    sbPreset33: TSpeedButton;
    sbPreset44: TSpeedButton;
    sbPreset21: TSpeedButton;
    sbPreset12: TSpeedButton;
    sbPreset32: TSpeedButton;
    sbPreset23: TSpeedButton;
    cbPresetHelper: TCheckBox;
    rbContinuousTiles: TRadioButton;
    rbCustomTiles: TRadioButton;
    sePresetWidth: TSpinEdit;
    sePresetHeight: TSpinEdit;
    lbPresetSize: TLabel;
    lbPresetSizeX: TLabel;
    mmPresetDefinition: TMemo;
    Bevel1: TBevel;
    PresetImage: TImage;
    btnClearPreset: TButton;
    lbPresetCode: TLabel;
    btnCopyPresetCode: TButton;
    btnUsePreset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
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
    procedure cbPresetHelperClick(Sender: TObject);
    procedure PresetTypeChange(Sender: TObject);
    procedure SetCustomPresetSize(Sender: TObject);
    procedure ClearPreset(Sender: TObject);
    procedure PresetImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnCopyPresetCodeClick(Sender: TObject);
    procedure btnUsePresetClick(Sender: TObject);
  private
    procedure set_continuous_preset_tiles;
    procedure block_preset_to_text;
    procedure draw_block_preset;
  public
    procedure tileset_changed;
  private
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

    block_width: word;
    block_height: word;
    block_left: word;
    block_top: word;
    block_tiles: array[0..63] of smallint;
    block_tile_current: integer;
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
  block_width := 1;
  block_height := 1;
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
      TilesetImage.Canvas.LineTo(640,y*32);
    end;
  end;
  if not (cbPresetHelper.Checked and rbCustomTiles.Checked) then
  begin
    TilesetImage.Canvas.Pen.Color := clRed;
    TilesetImage.Canvas.Pen.Width := 2;
    TilesetImage.Canvas.Brush.Style := bsClear;
    TilesetImage.Canvas.Rectangle(block_left*32+1,(block_top-tileset_top)*32+1,(block_left+block_width)*32+1,(block_top+block_height-tileset_top)*32+1);
  end;
end;

procedure TTilesetDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
  27: Close;
  32: begin BlockPresetDialog.Show; Hide; key := 0; end;
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
  pos_x, pos_y: integer;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  if cbPresetHelper.Checked and rbCustomTiles.Checked then
  begin
    // Select next tile for custom preset
    block_tiles[block_tile_current] := pos_x + pos_y * 20;
    block_tile_current := (block_tile_current + 1) mod (block_width * block_height);
    block_preset_to_text;
    draw_block_preset;
    exit;
  end;
  if sbCustomSize.Down then
  begin
    // Custom block size
    select_started := true;
    select_start_x := pos_x;
    select_start_y := pos_y;
    TilesetImageMouseMove(Sender, Shift, X, Y);
    exit;
  end;
  // Predefined block size
  if (pos_x + preset_width > 20) or (pos_y + preset_height > 40) then
    exit;
  block_width := preset_width;
  block_height := preset_height;
  block_left := pos_x;
  block_top := pos_y;
  DrawTileset(nil);
  select_started := true;
  TilesetImageMouseUp(Sender, Button, Shift, X, Y);
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
  if not select_started then
    exit;
  select_started := false;
  if not cbPresetHelper.Checked then
  begin
    MainWindow.select_block_from_tileset(block_width, block_height, block_left, block_top);
    if Button = mbLeft then
    begin
      close;
      MainWindow.RbBlockMode.Checked := True;
    end;
  end;
  block_width := min(block_width, 8);
  block_height := min(block_height, 8);
  sePresetWidth.Value := block_width;
  sePresetHeight.Value := block_height;
  set_continuous_preset_tiles;
  if cbPresetHelper.Checked then
  begin
    block_preset_to_text;
    draw_block_preset;
  end;
end;

procedure TTilesetDialog.SetBlockSize(Sender: TObject);
var
  preset_num: integer;
begin
  preset_num := (Sender as TSpeedButton).Tag - 1;
  preset_width := brush_size_presets[preset_num][1];
  preset_height := brush_size_presets[preset_num][2];
  if cbPresetHelper.Checked and rbCustomTiles.Checked then
  begin
    sePresetWidth.Value := preset_width;
    sePresetHeight.Value := preset_height;
  end;
end;

procedure TTilesetDialog.cbPresetHelperClick(Sender: TObject);
begin
  if cbPresetHelper.Checked then
  begin
    Constraints.MinWidth := 892;
    Constraints.MaxWidth := 892;
    block_preset_to_text;
    draw_block_preset;
  end else
  begin
    Constraints.MinWidth := 682;
    Constraints.MaxWidth := 682;
  end;
  DrawTileset(nil);
end;

procedure TTilesetDialog.PresetTypeChange(Sender: TObject);
var
  custom_tiles: boolean;
begin
  custom_tiles := rbCustomTiles.Checked;
  lbPresetSize.Visible := custom_tiles;
  lbPresetSizeX.Visible := custom_tiles;
  sePresetWidth.Visible := custom_tiles;
  sePresetHeight.Visible := custom_tiles;
  btnClearPreset.Visible := custom_tiles;
  block_tile_current := 0;
  if rbContinuousTiles.Checked then
    set_continuous_preset_tiles;
  block_preset_to_text;
  draw_block_preset;
  DrawTileset(nil);
end;

procedure TTilesetDialog.SetCustomPresetSize(Sender: TObject);
begin
  if not (cbPresetHelper.Checked and rbCustomTiles.Checked) then
    exit;
  block_width := sePresetWidth.Value;
  block_height := sePresetHeight.Value;
  ClearPreset(Sender);
end;

procedure TTilesetDialog.ClearPreset(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 63 do
    block_tiles[i] := -1;
  block_tile_current := 0;
  block_preset_to_text;
  draw_block_preset;
end;

procedure TTilesetDialog.PresetImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pos_left, pos_top: integer;
  pos_x, pos_y: integer;
begin
  if not rbCustomTiles.Checked then
    exit;
  pos_left := 96 - (block_width * 16);
  pos_top := 96 - (block_height * 16);
  if (X < pos_left) or (Y < pos_top) then
    exit;
  pos_x := (X - pos_left) div 32;
  pos_y := (Y - pos_top) div 32;
  if (pos_x >= block_width) or (pos_y >= block_height) then
    exit;
  if Button = mbLeft then
    block_tile_current := pos_x + pos_y * block_width
  else
  begin
    block_tiles[pos_x + pos_y * block_width] := -1;
    block_preset_to_text;
  end;
  draw_block_preset;
end;

procedure TTilesetDialog.btnCopyPresetCodeClick(Sender: TObject);
var
  str: string;
begin
  str := mmPresetDefinition.Lines.GetText;
  Clipboard.AsText := StringReplace(StringReplace(str, #10, '', [rfReplaceAll]), #13, '', [rfReplaceAll]);
end;

procedure TTilesetDialog.btnUsePresetClick(Sender: TObject);
var
  x, y: integer;
begin
  MainWindow.block_width := block_width;
  MainWindow.block_height := block_height;
  for y := 0 to block_height-1 do
    for x := 0 to block_width-1 do
    begin
      MainWindow.block_data[x, y].tile := block_tiles[x + y * block_width];
      MainWindow.block_data[x, y].special := IfThen(block_tiles[x + y * block_width] = -1, 65535, 0);
    end;
  MainWindow.draw_cursor_image;
  close;
end;

procedure TTilesetDialog.set_continuous_preset_tiles;
var
  x, y: integer;
begin
  for y := 0 to block_height-1 do
    for x := 0 to block_width-1 do
      block_tiles[x + y*block_width] := block_left + x + (block_top + y) * 20;
end;

procedure TTilesetDialog.block_preset_to_text;
var
  str: string;
  i: integer;
begin
  if rbContinuousTiles.Checked then
    str := inttostr(block_width) + '.' + inttostr(block_height) + '.' + inttostr(block_left + block_top * 20)
  else
  begin
    str := inttostr(block_width * -1) + '.' + inttostr(block_height * -1);
    for i := 0 to (block_width*block_height)-1 do
      str := str + '.' + inttostr(block_tiles[i]);
  end;
  mmPresetDefinition.Lines.Clear;
  mmPresetDefinition.Lines.Add(str);
end;

procedure TTilesetDialog.draw_block_preset;
var
  pos_left, pos_top: integer;
  x, y: integer;
  tile_to_draw: integer;
  tile_to_draw_x, tile_to_draw_y: integer;
  block_tile_current_x, block_tile_current_y: integer;
begin
  PresetImage.Canvas.Pen.Color := clBtnFace;
  PresetImage.Canvas.Brush.Color := clBtnFace;
  PresetImage.Canvas.Brush.Style := bsSolid;
  PresetImage.Canvas.Rectangle(0, 0, 192, 192);
  pos_left := 96 - (block_width * 16);
  pos_top := 96 - (block_height * 16);
  for y := 0 to block_height - 1 do
    for x := 0 to block_width - 1 do
    begin
      tile_to_draw := block_tiles[x + y * block_width];
      tile_to_draw_x := tile_to_draw mod 20;
      tile_to_draw_y := tile_to_draw div 20;
      PresetImage.Canvas.CopyRect(Rect(pos_left + x*32, pos_top + y*32, pos_left + x*32 + 32, pos_top + y*32 + 32), Tileset.tileimage.Canvas, Rect(tile_to_draw_x*32, tile_to_draw_y*32, tile_to_draw_x*32+32, tile_to_draw_y*32+32));
    end;
  PresetImage.Canvas.Brush.Style := bsClear;
  if rbCustomTiles.Checked then
  begin
    block_tile_current_x := block_tile_current mod block_width;
    block_tile_current_y := block_tile_current div block_width;
    PresetImage.Canvas.Pen.Color := clRed;
    PresetImage.Canvas.Rectangle(pos_left + block_tile_current_x*32, pos_top + block_tile_current_y*32, pos_left + block_tile_current_x*32 + 32, pos_top + block_tile_current_y*32 + 32);
  end;
  PresetImage.Canvas.Pen.Color := clBlue;
  PresetImage.Canvas.Rectangle(pos_left-1, pos_top-1, pos_left + block_width * 32 + 1, pos_top + block_height * 32 + 1);
end;

procedure TTilesetDialog.tileset_changed;
begin
  DrawTileset(nil);
  draw_block_preset;
end;

end.
