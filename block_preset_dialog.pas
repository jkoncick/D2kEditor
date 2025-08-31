unit block_preset_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, _tileset;

type
  TBlockPresetDialog = class(TForm)
    BlockPresetImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CMDialogKey(var AMessage: TCMDialogKey); message CM_DIALOGKEY;
    procedure BlockPresetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    variants_current: array[0..cnt_block_preset_keys-1] of integer;
    render_letters: boolean;
    // Pending action flags
    pending_update_tileset: boolean;

  public
    // Dispatcher procedures
    procedure update_tileset;
    // Other procedures
    procedure init_presets;
    procedure next_variant_all;
    procedure next_variant(key_index: integer);
    procedure select_preset(key_index: integer);
    procedure draw_all;
    procedure draw_block_preset(key_index: integer);
  end;

var
  BlockPresetDialog: TBlockPresetDialog;

implementation

uses _settings, _graphics, main, tileset_dialog;

{$R *.dfm}

procedure TBlockPresetDialog.FormCreate(Sender: TObject);
begin
  ClientWidth := 960;
  ClientHeight := 384;
  init_presets;
end;

procedure TBlockPresetDialog.FormShow(Sender: TObject);
begin
  if pending_update_tileset then
    update_tileset;
end;

procedure TBlockPresetDialog.FormHide(Sender: TObject);
begin
  MainWindow.block_preset_dialog_opened := true;
end;

procedure TBlockPresetDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: integer;
begin
  case key of
    27: Close;
    32: begin TilesetDialog.Show; Hide; end;
  end;
  // F1-F4: Change block preset group
  if (key >= 112) and (key <= 115) then
    MainWindow.FormKeyDown(Sender, Key, Shift);
  // Number/letter: Select preset
  if ((key >= ord('0')) and (key <= ord('9'))) or ((key >= ord('A')) and (key <= ord('Z'))) or (key = 186) or (key = 188) or (key = 190) or (key = 191) then
  begin
    if key = 188 then
      key := ord('<');
    if key = 190 then
      key := ord('>');
    if key = 186 then
      key := ord(':');
    if key = 191 then
      key := ord('?');
    for i := 0 to cnt_block_preset_keys - 1 do
      if ord(block_preset_keys[i]) = key then
      begin
        if ssShift in Shift then
          next_variant(i)
        else
          select_preset(i);
        break;
      end;
  end;
end;

procedure TBlockPresetDialog.CMDialogKey(var AMessage: TCMDialogKey);
begin
  if AMessage.CharCode = VK_TAB then
  begin
    next_variant_all;
    AMessage.Result := 1;
  end else
    inherited;
end;

procedure TBlockPresetDialog.BlockPresetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row, col: integer;
begin
  col := X div 96;
  row := Y div 96;
  if Button = mbRight then
  begin
    next_variant(row * block_preset_cols + col);
  end
  else if Button = mbLeft then
  begin
    select_preset(row * block_preset_cols + col);
  end
  else if Button = mbMiddle then
  begin
    render_letters := not render_letters;
    draw_all;
  end;
end;

procedure TBlockPresetDialog.update_tileset;
begin
  if not Visible then
  begin
    pending_update_tileset := true;
    exit;
  end;
  pending_update_tileset := false;
  init_presets;
end;

procedure TBlockPresetDialog.init_presets;
var
  i: integer;
begin
  Caption := 'Block preset selection - ' + Tileset.block_preset_groups[MainWindow.block_preset_group].name;
  for i := 0 to cnt_block_preset_keys - 1 do
    variants_current[i] := 0;
  draw_all;
end;

procedure TBlockPresetDialog.next_variant_all;
var
  i: integer;
begin
  for i := 0 to cnt_block_preset_keys - 1 do
    next_variant(i);
end;

procedure TBlockPresetDialog.next_variant(key_index: integer);
var
  num_variants: integer;
begin
  num_variants := Tileset.block_preset_key_variants[MainWindow.block_preset_group, key_index];
  if num_variants > 1 then
  begin
    variants_current[key_index] := (variants_current[key_index] + 1) mod num_variants;
    draw_block_preset(key_index);
  end;
end;

procedure TBlockPresetDialog.select_preset(key_index: integer);
begin
  MainWindow.select_block_preset(tileset.get_block_preset_index(MainWindow.block_preset_group, key_index, variants_current[key_index]));
  if settings.HidePresetWindow then
    Hide;
end;

procedure TBlockPresetDialog.draw_all;
var
  i: integer;
begin
  for i := 0 to cnt_block_preset_keys - 1 do
    draw_block_preset(i);
end;

procedure TBlockPresetDialog.draw_block_preset(key_index: integer);
var
  row, col: integer;
  preset_index: integer;
  preset: PBlockPreset;
  size_x, size_y: integer;
  src_x, src_y: integer;
  off_x, off_y: integer;
  min_x, min_y: integer;
  src_rect, dest_rect: TRect;
  x, y: integer;
  tile: word;
  tile_x, tile_y: integer;
  tile_attr: Cardinal;
  side: integer;
  num_variants: integer;
begin
  row := key_index div block_preset_cols;
  col := key_index mod block_preset_cols;
  preset_index := tileset.get_block_preset_index(MainWindow.block_preset_group, key_index, variants_current[key_index]);
  preset := @Tileset.block_presets[preset_index];
  BlockPresetImage.Canvas.Pen.Color := clBtnFace;
  BlockPresetImage.Canvas.Brush.Color := clBtnFace;
  BlockPresetImage.Canvas.Rectangle(col*96, row*96, col*96+96, row*96+96);
  min_x := col * 96;
  min_y := row * 96;
  size_x := preset.width*32;
  size_y := preset.height*32;
  off_x := ((96 - size_x) div 2) + col * 96;
  off_y := ((96 - size_y) div 2) + row * 96;
  for x := 0 to preset.width - 1 do
    for y := 0 to preset.height - 1 do
    begin
      tile := Tileset.block_preset_tiles[Tileset.block_preset_first_tile_indexes[preset_index] + x + y * preset.width];
      if tile = 65535 then
        continue;
      tile_x := tile mod 20;
      tile_y := tile div 20;
      src_rect := Rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32);
      dest_rect := Rect(off_x + x*32, off_y + y*32, off_x + x*32 + 32, off_y + y*32 + 32);
      if dest_rect.Left < min_x then
      begin
        src_rect.Left := src_rect.Left + (min_x - dest_rect.Left);
        dest_rect.Left := min_x;
      end;
      if dest_rect.Top < min_y then
      begin
        src_rect.Top := src_rect.Top + (min_y - dest_rect.Top);
        dest_rect.Top := min_y;
      end;
      if (src_rect.Left < src_rect.Right) and (src_rect.Top < src_rect.Bottom) then
        BlockPresetImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      // Draw concrete owner marker
      tile_attr := Tileset.attributes[tile];
      if (tile_attr and $8800) = $8800 then
      begin
        side := (tile_attr shr 17) and 7;
        BlockPresetImage.Canvas.Pen.Color := StructGraphics.house_colors_inv[side];
        BlockPresetImage.Canvas.Brush.Color := BlockPresetImage.Canvas.Pen.Color;
        BlockPresetImage.Canvas.Brush.Style := bsSolid;
        BlockPresetImage.Canvas.Ellipse(off_x + x*32 + 8, off_y + y*32 + 8, off_x + x*32 + 24, off_y + y*32 + 24);
        BlockPresetImage.Canvas.Brush.Style := bsClear;
      end;
    end;

  num_variants := Tileset.block_preset_key_variants[MainWindow.block_preset_group, key_index];
  if num_variants > 1 then
    BlockPresetImage.Canvas.TextOut(col * 96 + 64, row * 96 + 81, inttostr(variants_current[key_index] + 1) + ' of ' + inttostr(num_variants));
  if render_letters then
    BlockPresetImage.Canvas.TextOut(col * 96 + 4, row * 96 + 2, block_preset_keys[key_index]);

  BlockPresetImage.Canvas.Pen.Color := clBlack;
  BlockPresetImage.Canvas.MoveTo(col*96, row*96);
  BlockPresetImage.Canvas.LineTo(col*96+96, row*96);
  BlockPresetImage.Canvas.MoveTo(col*96, row*96);
  BlockPresetImage.Canvas.LineTo(col*96, row*96+96);
end;

end.
