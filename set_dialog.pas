unit set_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Grids;

type
  TSetDialog = class(TForm)
    SetMapSize_Menu: TPanel;
    SetMapSize_LbWidth: TLabel;
    SetMapSize_LbHeight: TLabel;
    SetMapSize_Width: TSpinEdit;
    SetMapSize_Height: TSpinEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    ShiftMap_Menu: TPanel;
    ShiftMap_RbUp: TRadioButton;
    ShiftMap_RbDown: TRadioButton;
    ShiftMap_RbLeft: TRadioButton;
    ShiftMap_RbRight: TRadioButton;
    ShiftMap_NumTiles: TSpinEdit;
    ShiftMap_LbNumTiles: TLabel;
    ChStrOwn_Menu: TPanel;
    ChStrOwn_SideFrom: TComboBox;
    ChStrOwn_SideTo: TComboBox;
    ChStrOwn_Swap: TCheckBox;
    ChStrOwn_LbSideFrom: TLabel;
    ChStrOwn_LbSideTo: TLabel;
    Tileset_Menu: TPanel;
    pnButtons: TPanel;
    Tileset_List: TStringGrid;
    NewTileset_Menu: TPanel;
    NewTileset_edTilesetName: TEdit;
    NewTileset_lblTilesetName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);  
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure ShiftMap_SelectDirection(Sender: TObject);
  private
    current_menu: integer;
    shift_map_direction: integer;
  public
    function select_menu(menu: integer): integer;
    // Dispatcher procedures
    procedure update_tileset_list;
    procedure update_side_list(side_list: TStringList);
    procedure update_tileset;
  private
    function check_map_dimensions: boolean;
  end;

var
  SetDialog: TSetDialog;

implementation

uses
  Math, _settings, _map, _tileset;

{$R *.dfm}

{ TSetDialog }

procedure TSetDialog.FormCreate(Sender: TObject);
begin
  SetMapSize_Width.MaxValue := max_map_width;
  SetMapSize_Width.Value := Settings.DefaultMapWidth;
  SetMapSize_Height.MaxValue := max_map_height;
  SetMapSize_Height.Value := Settings.DefaultMapHeight;
  Tileset_List.Cells[0,0] := 'Name';
  Tileset_List.Cells[1,0] := 'Fancy name';
  Tileset_List.Cells[2,0] := 'Author';
  Tileset_List.Cells[3,0] := 'Tiles';
  Tileset_List.Cells[4,0] := 'Attributes';
  Tileset_List.Cells[5,0] := 'Location';
  Tileset_List.ColWidths[0] := 72;
  Tileset_List.ColWidths[1] := 192;
  Tileset_List.ColWidths[2] := 128;
  Tileset_List.ColWidths[3] := 40;
  Tileset_List.ColWidths[4] := 72;
  Tileset_List.ColWidths[5] := 65;
end;

procedure TSetDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then
    BtnOKClick(nil);
  if key = 27 then
    BtnCancelClick(nil);
end;

procedure TSetDialog.FormShow(Sender: TObject);
begin
  if current_menu = 5 then
    Tileset_List.SetFocus
  else if current_menu = 6 then
    NewTileset_edTilesetName.SetFocus
  else
    BtnOK.SetFocus;
end;

function TSetDialog.select_menu(menu: integer): integer;
begin
  SetMapSize_Menu.Visible := False;
  ShiftMap_Menu.Visible := False;
  ChStrOwn_Menu.Visible := False;
  Tileset_Menu.Visible := False;
  NewTileset_Menu.Visible := False;
  Height := 172;
  Width := 176;
  case menu of
    1:  begin
          Caption := 'Set map size';
          SetMapSize_Menu.Visible := True;
        end;
    2:  begin
          ShiftMap_Menu.Visible := True;
          Caption := 'Shift map';
        end;
    3:  begin
          ChStrOwn_Menu.Visible := True;
          Caption := 'Change structure owner';
        end;
    4:  begin
          SetMapSize_Menu.Visible := True;
          Caption := 'Set map size';
        end;
    5:  begin
          Tileset_Menu.Visible := True;
          Caption := 'Select tileset';
          Height := 516;
          Width := 600;
        end;
    6:  begin
          NewTileset_Menu.Visible := True;
          Caption := 'New tileset';
          NewTileset_edTilesetName.Text := '';
        end;
  end;
  current_menu := menu;
  result := ShowModal;
end;

procedure TSetDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSetDialog.BtnOKClick(Sender: TObject);
begin
  case current_menu of
    1:  begin
          if check_map_dimensions then
          begin
            ModalResult := mrOk;
            Map.set_map_size(SetMapSize_Width.Value,SetMapSize_Height.Value);
          end;
        end;
    2:  begin
          if shift_map_direction > 0 then
          begin
            ModalResult := mrOk;
            Map.shift_map(shift_map_direction,ShiftMap_NumTiles.Value);
          end;
        end;
    3:  begin
          Map.change_structure_owner(ChStrOwn_SideFrom.ItemIndex,ChStrOwn_SideTo.ItemIndex,ChStrOwn_Swap.Checked);
          ModalResult := mrOk;
        end;
    4:  begin
          if check_map_dimensions then
          begin
            ModalResult := mrOk;
            Map.new_map(SetMapSize_Width.Value,SetMapSize_Height.Value);
          end;
        end;
    5:  begin
          if Tileset_List.Row > 0 then
          begin
            ModalResult := mrOk;
            Tileset.change_tileset_by_index(Tileset.tileset_index_mapping[Tileset_List.Row - 1]);
          end;
        end;
    6:  begin
          if NewTileset_edTilesetName.Text <> '' then
          begin
            ModalResult := mrOk;
            Tileset.new_tileset(NewTileset_edTilesetName.Text);
          end;
        end;
  end;
end;

procedure TSetDialog.ShiftMap_SelectDirection(Sender: TObject);
begin
  shift_map_direction := (Sender as TRadioButton).Tag;
end;

procedure TSetDialog.update_tileset_list;
var
  i, index: integer;
begin
  Tileset_List.RowCount := Tileset.cnt_tilesets + 1;
  for i := 0 to Tileset.cnt_tilesets - 1 do
  begin
    index := Tileset.tileset_index_mapping[i];
    Tileset_List.Cells[0, i+1] := Tileset.tileset_list[index].name;
    Tileset_List.Cells[1, i+1] := Tileset.tileset_list[index].fancy_name;
    Tileset_List.Cells[2, i+1] := Tileset.tileset_list[index].author;
    Tileset_List.Cells[3, i+1] := IntToStr(Tileset.tileset_list[index].num_tiles);
    Tileset_List.Cells[4, i+1] := Tileset.tileset_list[index].attributes;
    Tileset_List.Cells[5, i+1] := Tileset.tileset_list[index].location;
    if Tileset.tileset_list[index].name = Settings.DefaultTilesetName then
      Tileset_List.Row := i + 1;
  end;
end;

procedure TSetDialog.update_side_list(side_list: TStringList);
var
  prev_index: integer;
begin
  prev_index := ChStrOwn_SideFrom.ItemIndex;
  ChStrOwn_SideFrom.Items := side_list;
  ChStrOwn_SideFrom.ItemIndex := Max(prev_index, 0);
  prev_index := ChStrOwn_SideTo.ItemIndex;
  ChStrOwn_SideTo.Items := side_list;
  ChStrOwn_SideTo.Items.Add('None (delete)');
  ChStrOwn_SideTo.ItemIndex := Max(prev_index, 0);
end;

procedure TSetDialog.update_tileset;
var
  i: integer;
begin
  for i := 0 to Tileset.cnt_tilesets - 1 do
    if Tileset.tileset_name = Tileset.tileset_list[Tileset.tileset_index_mapping[i]].name then
      Tileset_List.Row := i + 1;
end;

function TSetDialog.check_map_dimensions: boolean;
begin
  result := false;
  if ((SetMapSize_Width.Value mod 2) = 1) or ((SetMapSize_Height.Value mod 2) = 1) then
    ShowMessage('Map size must be even.')
  else
    result := true;
end;

end.
