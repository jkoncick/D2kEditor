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
    ('A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ' '),
    ('Z', 'X', 'C', 'V', 'B', 'N', 'M', chr(188), chr(190), ' ')
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
    procedure BlockPresetImageMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
  private
    variants_cnt: array[0..num_rows-1, 0..num_cols-1] of integer;
    variants_current: array[0..num_rows-1, 0..num_cols-1] of integer;

  public
    procedure init_presets;
    procedure next_variant_all;
    procedure next_variant(row, col: integer);
    procedure draw_block_preset(row, col: integer);
  end;

var
  BlockPresetDialog: TBlockPresetDialog;

implementation

uses _tileset, main, tileset_dialog;

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
begin
  case key of
    27: Close;
    32: begin TilesetDialog.Show; Hide; end;
  end;
  if (key >= 112) and (key <= 115) then
    MainWindow.FormKeyDown(Sender, Key, Shift);
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
  key: word;
  preset: TBlockPreset;
begin
  col := X div 96;
  row := Y div 96;
  if Button = mbRight then
  begin
    next_variant(row, col);
  end
  else if Button = mbLeft then
  begin
    key := ord(block_preset_keys[row, col]);
    preset := tileset.get_block_preset(MainWindow.BlockPresetGroupSelect.ItemIndex, key, variants_current[row, col]);
    MainWindow.select_block_from_tileset(preset.width, preset.height, preset.pos_x, preset.pos_y);
    Hide;
  end
  else if Button = mbMiddle then
  begin
    for row := 0 to num_rows -1 do
      for col := 0 to num_cols -1 do
        BlockPresetImage.Canvas.TextOut(col * 96 + 4, row * 96 + 2, block_preset_keys[row,col]);
  end;
end;

procedure TBlockPresetDialog.BlockPresetImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MainWindow.CursorImage.Visible := false;
end;

procedure TBlockPresetDialog.init_presets;
var
  i, j: integer;
  key_index: integer;
begin
  for i := 0 to num_rows -1 do
    for j:= 0 to num_cols -1 do
    begin
      variants_current[i,j] := 0;
      key_index := Tileset.block_key_to_index(ord(block_preset_keys[i,j]));
      variants_cnt[i,j] := Tileset.block_preset_key_variants[MainWindow.BlockPresetGroupSelect.ItemIndex, key_index].num_variants;
      draw_block_preset(i, j);
    end;
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

procedure TBlockPresetDialog.draw_block_preset(row, col: integer);
var
  key: word;
  preset: TBlockPreset;
  size_x, size_y: integer;
  src_x, src_y: integer;
  off_x, off_y: integer;
begin
  key := ord(block_preset_keys[row, col]);
  preset := tileset.get_block_preset(MainWindow.BlockPresetGroupSelect.ItemIndex, key, variants_current[row,col]);
  BlockPresetImage.Canvas.Pen.Color := clBtnFace;
  BlockPresetImage.Canvas.Brush.Color := clBtnFace;
  BlockPresetImage.Canvas.Rectangle(col*96, row*96, col*96+96, row*96+96);

  size_x := preset.width*32;
  size_y := preset.height*32;
  src_x := preset.pos_x*32;
  src_y := preset.pos_y*32;
  if size_x > 96 then
  begin
    src_x := src_x + (size_x - 96) div 2;
    size_x := 96;
  end;
  if size_y > 96 then
  begin
    src_y := src_y + (size_y - 96) div 2;
    size_y := 96;
  end;
  off_x := ((96 - size_x) div 2) + col * 96;
  off_y := ((96 - size_y) div 2) + row * 96;
  BlockPresetImage.Canvas.CopyRect(Rect(off_x, off_y, off_x+size_x, off_y+size_y), Tileset.tileimage.Canvas, Rect(src_x, src_y, src_x+size_x, src_y+size_y));

  if variants_cnt[row,col] > 1 then
    BlockPresetImage.Canvas.TextOut(col*96+64, row*96+81, inttostr(variants_current[row,col]+1) + ' of ' + inttostr(variants_cnt[row,col]));

  BlockPresetImage.Canvas.Pen.Color := clBlack;
  BlockPresetImage.Canvas.MoveTo(col*96, row*96);
  BlockPresetImage.Canvas.LineTo(col*96+96, row*96);
  BlockPresetImage.Canvas.MoveTo(col*96, row*96);
  BlockPresetImage.Canvas.LineTo(col*96, row*96+96);
end;

end.
