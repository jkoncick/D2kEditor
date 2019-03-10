unit block_preset_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

const num_rows = 4;
const num_cols = 10;
const block_preset_keys: array[0..num_rows-1, 0..num_cols-1] of char =
  (
    ('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'),
    ('Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P'),
    ('A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':'),
    ('Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?')
  );

type
  TBlockPresetDialog = class(TForm)
    BlockPresetImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CMDialogKey(var AMessage: TCMDialogKey); message CM_DIALOGKEY;
    procedure BlockPresetImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    variants_cnt: array[0..num_rows-1, 0..num_cols-1] of integer;
    variants_current: array[0..num_rows-1, 0..num_cols-1] of integer;
    render_letters: boolean;

  public
    procedure init_presets;
    procedure next_variant_all;
    procedure next_variant(row, col: integer);
    procedure select_preset(row, col: integer);
    procedure draw_all;
    procedure draw_block_preset(row, col: integer);
  end;

var
  BlockPresetDialog: TBlockPresetDialog;

implementation

uses _tileset, _settings, main, tileset_dialog;

{$R *.dfm}

procedure TBlockPresetDialog.FormCreate(Sender: TObject);
begin
  ClientWidth := 960;
  ClientHeight := 384;
  init_presets;
end;

procedure TBlockPresetDialog.FormHide(Sender: TObject);
begin
  MainWindow.block_preset_dialog_opened := true;
end;

procedure TBlockPresetDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i, j: integer;
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
    for i := 0 to num_rows -1 do
      for j:= 0 to num_cols -1 do
      begin
        if ord(block_preset_keys[i,j]) = key then
        begin
          if ssShift in Shift then
            next_variant(i, j)
          else
            select_preset(i, j);
        end;
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

procedure TBlockPresetDialog.BlockPresetImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row, col: integer;
begin
  col := X div 96;
  row := Y div 96;
  if Button = mbRight then
  begin
    next_variant(row, col);
  end
  else if Button = mbLeft then
  begin
    select_preset(row, col);
  end
  else if Button = mbMiddle then
  begin
    render_letters := not render_letters;
    draw_all;
  end;
end;

procedure TBlockPresetDialog.init_presets;
var
  i, j: integer;
  key_index: integer;
begin
  Caption := 'Block preset selection - ' + Tileset.block_preset_groups[MainWindow.block_preset_group].name;
  for i := 0 to num_rows -1 do
    for j:= 0 to num_cols -1 do
    begin
      variants_current[i,j] := 0;
      key_index := Tileset.block_key_to_index(ord(block_preset_keys[i,j]));
      if key_index = -1 then
        variants_cnt[i,j] := 0
      else
        variants_cnt[i,j] := Tileset.block_preset_key_variants[MainWindow.block_preset_group, key_index].num_variants;
    end;
  draw_all;
end;

procedure TBlockPresetDialog.next_variant_all;
var
  i, j: integer;
begin
  for i := 0 to num_rows -1 do
    for j:= 0 to num_cols -1 do
      next_variant(i,j);
end;

procedure TBlockPresetDialog.next_variant(row, col: integer);
begin
  if variants_cnt[row,col] > 1 then
  begin
    variants_current[row,col] := (variants_current[row,col] + 1) mod variants_cnt[row,col];
    draw_block_preset(row, col);
  end;
end;

procedure TBlockPresetDialog.select_preset(row, col: integer);
var
  key: word;
  preset: PBlockPreset;
begin
  key := ord(block_preset_keys[row, col]);
  preset := tileset.get_block_preset(MainWindow.block_preset_group, key, variants_current[row, col]);
  MainWindow.select_block_preset(preset);
  if settings.HidePresetWindow then
    Hide;
end;

procedure TBlockPresetDialog.draw_all;
var
  i, j: integer;
begin
  for i := 0 to num_rows -1 do
    for j:= 0 to num_cols -1 do
      draw_block_preset(i,j);
end;

procedure TBlockPresetDialog.draw_block_preset(row, col: integer);
var
  key: word;
  preset: PBlockPreset;
  size_x, size_y: integer;
  src_x, src_y: integer;
  off_x, off_y: integer;
  min_x, min_y: integer;
  src_rect, dest_rect: TRect;
  x, y: integer;
  tile: word;
  tile_x, tile_y: integer;
begin
  key := ord(block_preset_keys[row, col]);
  preset := tileset.get_block_preset(MainWindow.block_preset_group, key, variants_current[row,col]);
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
      tile := Tileset.block_preset_tiles[preset.block_preset_tile_index + x + y * preset.width];
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
    end;

  if variants_cnt[row,col] > 1 then
    BlockPresetImage.Canvas.TextOut(col*96+64, row*96+81, inttostr(variants_current[row,col]+1) + ' of ' + inttostr(variants_cnt[row,col]));
  if render_letters then
    BlockPresetImage.Canvas.TextOut(col * 96 + 4, row * 96 + 2, block_preset_keys[row,col]);

  BlockPresetImage.Canvas.Pen.Color := clBlack;
  BlockPresetImage.Canvas.MoveTo(col*96, row*96);
  BlockPresetImage.Canvas.LineTo(col*96+96, row*96);
  BlockPresetImage.Canvas.MoveTo(col*96, row*96);
  BlockPresetImage.Canvas.LineTo(col*96, row*96+96);
end;

end.
