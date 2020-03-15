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
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);  
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure ShiftMap_SelectDirection(Sender: TObject);
  private
    { Private declarations }
    current_menu: integer;
    shift_map_direction: integer;
  public
    { Public declarations }
    procedure select_menu(menu: integer);
  end;

var
  SetDialog: TSetDialog;

implementation

uses
  main, _settings, _map, _tileset;

{$R *.dfm}

{ TSetDialog }

procedure TSetDialog.FormCreate(Sender: TObject);
var
  tilesets: TStringList;
  i: integer;
  default_tileset: integer;
begin
  ChStrOwn_PlayerFrom.Items := MainWindow.PlayerSelect.Items;
  ChStrOwn_PlayerFrom.ItemIndex := 0;
  ChStrOwn_PlayerTo.Items := MainWindow.PlayerSelect.Items;
  ChStrOwn_PlayerTo.Items.Add('None (delete)');
  ChStrOwn_PlayerTo.ItemIndex := 0;
  SetMapSize_Width.MaxValue := max_map_width;
  SetMapSize_Width.Value := Settings.DefaultMapWidth;
  SetMapSize_Height.MaxValue := max_map_height;
  SetMapSize_Height.Value := Settings.DefaultMapHeight;
  tilesets := TStringList.Create;
  default_tileset := 0;
  for i := 0 to Tileset.cnt_tilesets - 1 do
  begin
    tilesets.Add(Tileset.tileset_list[i]);
    if Tileset.tileset_list[i] = Settings.DefaultTilesetName then
      default_tileset := i;
  end;
  Tileset_List.Items := tilesets;
  tilesets.Destroy;
  Tileset_List.ItemIndex := default_tileset;
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
  BtnOK.SetFocus;
end;

procedure TSetDialog.select_menu(menu: integer);
begin
  SetMapSize_Menu.Visible := False;
  ShiftMap_Menu.Visible := False;
  ChStrOwn_Menu.Visible := False;
  Tileset_Menu.Visible := False;
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
        end;
  end;
  current_menu := menu;
  ShowModal;
end;

procedure TSetDialog.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TSetDialog.BtnOKClick(Sender: TObject);
begin
  case current_menu of
    1:  begin
          if ((SetMapSize_Width.Value mod 2) = 1) or ((SetMapSize_Height.Value mod 2) = 1) then
            ShowMessage('Map size must be even.')
          else
          begin
            close;
            MainWindow.set_map_size(SetMapSize_Width.Value,SetMapSize_Height.Value);
          end;
        end;
    2:  begin
          if shift_map_direction > 0 then
          begin
            close;
            MainWindow.shift_map(shift_map_direction,ShiftMap_NumTiles.Value);
          end;
        end;
    3:  begin
          MainWindow.change_structure_owner(ChStrOwn_PlayerFrom.ItemIndex,ChStrOwn_PlayerTo.ItemIndex,ChStrOwn_Swap.Checked);
          close;
        end;
    4:  begin
          if ((SetMapSize_Width.Value mod 2) = 1) or ((SetMapSize_Height.Value mod 2) = 1) then
            ShowMessage('Map size must be even.')
          else
          begin
            close;
            MainWindow.new_map(SetMapSize_Width.Value,SetMapSize_Height.Value);
          end;
        end;
    5:  begin
          close;
          MainWindow.change_tileset(Tileset_List.ItemIndex);
        end;
  end;
end;

procedure TSetDialog.ShiftMap_SelectDirection(Sender: TObject);
begin
  shift_map_direction := (Sender as TRadioButton).Tag;
end;

end.
