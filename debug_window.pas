unit debug_window;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, ValEdit, StdCtrls;

type
  TDebugWindow = class(TForm)
    vleDebugValueList: TValueListEditor;
    pnDebugControls: TPanel;
    gbRenderingPerformance: TGroupBox;
    cbShowRenderTime: TCheckBox;
    cbShowDifferentialRendering: TCheckBox;
    gbMissionLauncher: TGroupBox;
    cbShowReplaceFilesFromModsFolderLog: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DebugSettingChange(Sender: TObject);
  private
    // Pending action flags
    pending_update_debug_values: boolean;
  public
    // Dispatcher procedures
    procedure update_debug_values;
  private
    procedure add_debug_value(tmp_strings: TStringList; module: TObject; key, value: String);
  end;

var
  DebugWindow: TDebugWindow;

implementation

uses _settings, _structures, _graphics, _sounds, _tileset, _stringtable;

{$R *.dfm}

procedure TDebugWindow.FormShow(Sender: TObject);
begin
  if pending_update_debug_values then
    update_debug_values;
end;

procedure TDebugWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
    Close;
end;

procedure TDebugWindow.DebugSettingChange(Sender: TObject);
begin
  Settings.Debug_ShowRenderTime := cbShowRenderTime.Checked;
  Settings.Debug_ShowDifferentialRendering := cbShowDifferentialRendering.Checked;
  Settings.Debug_ShowReplaceFilesFromModsFolderLog := cbShowReplaceFilesFromModsFolderLog.Checked;
end;

procedure TDebugWindow.update_debug_values;
var
  s: TStringList;
begin
  if not Visible then
  begin
    pending_update_debug_values := true;
    exit;
  end;
  pending_update_debug_values := false;

  s := TStringList.Create;

  add_debug_value(s, Tileset, 'Tileset name', Tileset.tileset_name);
  add_debug_value(s, Tileset, 'Attributes name', Tileset.tileatr_name);
  add_debug_value(s, Tileset, 'Image file', Tileset.tileimage_filename);
  add_debug_value(s, Tileset, 'TLS file', Tileset.tls_filename);
  add_debug_value(s, Tileset, 'Attributes file', Tileset.tileatr_filename);
  add_debug_value(s, Tileset, 'INI file', Tileset.ini_filename);
  add_debug_value(s, Tileset, 'Minimap color rules used', Format('%d / %d', [Tileset.minimap_color_rules_used, max_minimap_color_rules]));
  add_debug_value(s, Tileset, 'Fill area rules used', Format('%d / %d', [Tileset.fill_area_rules_used, max_fill_area_rules]));
  add_debug_value(s, Tileset, 'Block presets used', Format('%d / %d', [Tileset.block_presets_used, max_block_presets]));
  add_debug_value(s, Tileset, 'Block preset tiles used', Format('%d / %d', [Tileset.block_preset_tiles_used, max_block_preset_tiles]));
  //--add_debug_value(s, Tileset, 'Connection points used', Format('%d / %d', [Tileset.connection_points_used, max_connection_points]));

  add_debug_value(s, Structures, 'Templates.bin file', Structures.templates_bin_filename);
  add_debug_value(s, Structures, 'BUILEXP.BIN file', Structures.builexp_bin_filename);
  add_debug_value(s, Structures, 'ARMOUR.BIN file', Structures.armour_bin_filename);
  add_debug_value(s, Structures, 'SPEED.BIN file', Structures.speed_bin_filename);
  add_debug_value(s, Structures, 'TECHPOS.BIN file', Structures.techpos_bin_filename);
  add_debug_value(s, Structures, 'TILEDATA.BIN file', Structures.tiledata_bin_filename);
  add_debug_value(s, Structures, 'misc_objects.ini file', Structures.misc_objects_ini_filename);
  add_debug_value(s, Structures, 'players.ini file', Structures.sides_ini_filename);
  add_debug_value(s, Structures, 'limits.ini file', Structures.limits_ini_filename);

  add_debug_value(s, StructGraphics, 'COLOURS.BIN file', StructGraphics.colours_bin_filename);
  add_debug_value(s, StructGraphics, 'DATA.R16 file', StructGraphics.data_r16_filename);
  add_debug_value(s, StructGraphics, 'misc_objects.bmp file', StructGraphics.graphics_misc_objects_filename);
  add_debug_value(s, StructGraphics, 'Structure images loaded', Format('%d / %d', [StructGraphics.structure_images_count, Length(StructGraphics.structure_images)]));
  add_debug_value(s, StructGraphics, 'House color pixel count', Format('%d / %d', [StructGraphics.house_color_pixel_count_total, Length(StructGraphics.house_color_pixel_indexes)]));

  add_debug_value(s, Sounds, 'SOUND.RS file', Sounds.sound_rs_filename);

  add_debug_value(s, StringTable, 'text.uib file', StringTable.text_uib_filename);
  add_debug_value(s, StringTable, 'samples.uib file', StringTable.samples_uib_filename);

  vleDebugValueList.Strings := s;
  s.Destroy;
end;

procedure TDebugWindow.add_debug_value(tmp_strings: TStringList; module: TObject; key, value: String);
begin
  tmp_strings.Add(Format('%s: %s=%s', [Copy(module.ClassName, 2, 100), key, value]));
end;

end.
