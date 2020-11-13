unit set_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls;

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
    ChStrOwn_PlayerFrom: TComboBox;
    ChStrOwn_PlayerTo: TComboBox;
    ChStrOwn_Swap: TCheckBox;
    ChStrOwn_LbPlayerFrom: TLabel;
    ChStrOwn_LbPlayerTo: TLabel;
    Tileset_Menu: TPanel;
    Tileset_List: TListBox;
    pnButtons: TPanel;
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
    procedure update_player_list(player_list: TStringList);
    procedure update_tileset;
  private
    function check_map_dimensions: boolean;
  end;

var
  SetDialog: TSetDialog;

implementation

uses
  Math, main, _settings, _map, _tileset;

{$R *.dfm}

{ TSetDialog }

procedure TSetDialog.FormCreate(Sender: TObject);
begin
  SetMapSize_Width.MaxValue := max_map_width;
  SetMapSize_Width.Value := Settings.DefaultMapWidth;
  SetMapSize_Height.MaxValue := max_map_height;
  SetMapSize_Height.Value := Settings.DefaultMapHeight;
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
  else
    BtnOK.SetFocus;
end;

function TSetDialog.select_menu(menu: integer): integer;
begin
  SetMapSize_Menu.Visible := False;
  ShiftMap_Menu.Visible := False;
  ChStrOwn_Menu.Visible := False;
  Tileset_Menu.Visible := False;
  Height := 172;
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
          Height := 350;
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
          Map.change_structure_owner(ChStrOwn_PlayerFrom.ItemIndex,ChStrOwn_PlayerTo.ItemIndex,ChStrOwn_Swap.Checked);
          ModalResult := mrOk;
        end;
    4:  begin
          if check_map_dimensions then
          begin
            ModalResult := mrOk;
            MainWindow.new_map(SetMapSize_Width.Value,SetMapSize_Height.Value);
          end;
        end;
    5:  begin
          ModalResult := mrOk;
          Tileset.change_tileset(Tileset_List.ItemIndex);
        end;
  end;
end;

procedure TSetDialog.ShiftMap_SelectDirection(Sender: TObject);
begin
  shift_map_direction := (Sender as TRadioButton).Tag;
end;

procedure TSetDialog.update_tileset_list;
var
  tilesets: TStringList;
  i: integer;
  default_tileset: integer;
begin
  tilesets := TStringList.Create;
  default_tileset := 0;
  for i := 0 to Tileset.cnt_tilesets - 1 do
  begin
    tilesets.Add(Tileset.tileset_list.Names[i]);
    if Tileset.tileset_list.ValueFromIndex[i] = Settings.DefaultTilesetName then
      default_tileset := i;
  end;
  Tileset_List.Items := tilesets;
  tilesets.Destroy;
  Tileset_List.ItemIndex := default_tileset;
end;

procedure TSetDialog.update_player_list(player_list: TStringList);
var
  prev_index: integer;
begin
  prev_index := ChStrOwn_PlayerFrom.ItemIndex;
  ChStrOwn_PlayerFrom.Items := player_list;
  ChStrOwn_PlayerFrom.ItemIndex := Max(prev_index, 0);
  prev_index := ChStrOwn_PlayerTo.ItemIndex;
  ChStrOwn_PlayerTo.Items := player_list;
  ChStrOwn_PlayerTo.Items.Add('None (delete)');
  ChStrOwn_PlayerTo.ItemIndex := Max(prev_index, 0);
end;

procedure TSetDialog.update_tileset;
begin
  if Tileset.current_tileset <> -1 then
    Tileset_List.ItemIndex := Tileset.current_tileset;
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
