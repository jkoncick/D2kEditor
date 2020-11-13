unit map_stats_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, _structures, _map, _utils;

const CNT_FIXED_ROWS = 7;

type OverallStats = (osAllStructures, osBuildingsNoWalls, osBuildings, osUnits, osPowerOutput, osPowerNeed, osPowerPercent);

type
  TMapStatsDialog = class(TForm)
    StatsGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StatsGridMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure StatsGridMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    tmp_stats: Array[0..CNT_FIXED_ROWS + MAX_BUILDING_TYPES + MAX_UNIT_TYPES -1, 0..CNT_PLAYERS-1] of integer;
    pending_update_map_stats: boolean;
  public
    // Dispatcher procedures
    procedure update_map_stats;
    procedure update_structures_list(building_list, unit_list: TStringList);
    procedure update_player_list;
  end;

var
  MapStatsDialog: TMapStatsDialog;

implementation

uses _mission;

{$R *.dfm}

procedure TMapStatsDialog.FormCreate(Sender: TObject);
begin
  StatsGrid.ColWidths[0] := 128;
  StatsGrid.Cells[0,1] := 'All structures';
  StatsGrid.Cells[0,2] := 'Buildings (without walls)';
  StatsGrid.Cells[0,3] := 'Buildings';
  StatsGrid.Cells[0,4] := 'Units';
  StatsGrid.Cells[0,5] := 'Power output';
  StatsGrid.Cells[0,6] := 'Power need';
  StatsGrid.Cells[0,7] := 'Power percent';
  StatsGrid.Cells[9,0] := 'Total';
end;

procedure TMapStatsDialog.FormShow(Sender: TObject);
begin
  if pending_update_map_stats then
    update_map_stats;
end;

procedure TMapStatsDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
    close;
end;

procedure TMapStatsDialog.StatsGridMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if StatsGrid.TopRow < (StatsGrid.RowCount - StatsGrid.VisibleRowCount) then
    StatsGrid.TopRow := StatsGrid.TopRow + 1;
  Handled := true;
end;

procedure TMapStatsDialog.StatsGridMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if StatsGrid.TopRow > 1 then
    StatsGrid.TopRow := StatsGrid.TopRow - 1;
  Handled := true;
end;

procedure TMapStatsDialog.update_map_stats;
var
  i,j,k: integer;
  index: integer;
  tiledata_entry: TTileDataEntryPtr;
  building_template: TBuildingTemplatePtr;
  total_value: integer;
begin
  if not Visible then
  begin
    pending_update_map_stats := true;
    exit;
  end;
  pending_update_map_stats := false;
  if not Map.loaded then
    exit;
  // Reset statistics
  for i := 0 to Length(tmp_stats) - 1 do
    for j := 0 to CNT_PLAYERS - 1 do
      tmp_stats[i,j] := 0;
  // Copy already computed power statistics
  for j := 0 to CNT_PLAYERS - 1 do
  begin
    tmp_stats[Byte(osPowerOutput),j] := Map.stats.players[j].power_output;
    tmp_stats[Byte(osPowerNeed),j] := Map.stats.players[j].power_need;
    tmp_stats[Byte(osPowerPercent),j] := Map.stats.players[j].power_percent;
  end;
  // Compute map statistics
  for i := 0 to Map.width-1 do
    for j := 0 to Map.height-1 do
    begin
      tiledata_entry := Structures.get_tiledata_entry(Map.data[i,j].special);
      if (tiledata_entry.stype = ST_BUILDING) and (tiledata_entry.index < MAX_BUILDING_TYPES) then
      begin
        index := -1;
        for k := 0 to MAX_BUILDING_TYPES-1 do
          if Structures.building_type_mapping[k] = Integer(tiledata_entry.index) then
          begin
            index := k;
            break;
          end;
        if index <> -1 then
          Inc(tmp_stats[CNT_FIXED_ROWS + index, tiledata_entry.player]);
        Inc(tmp_stats[Byte(osAllStructures), tiledata_entry.player]);
        Inc(tmp_stats[Byte(osBuildings), tiledata_entry.player]);
        building_template := Structures.get_building_template(tiledata_entry.index, Mission.get_player_alloc_index(tiledata_entry.player));
        if not ((building_template <> nil) and (building_template.SpecialBehavior = 14)) then
          Inc(tmp_stats[Byte(osBuildingsNoWalls), tiledata_entry.player]);
      end else
      if (tiledata_entry.stype = ST_UNIT) and (tiledata_entry.index < MAX_UNIT_TYPES) then
      begin
        Inc(tmp_stats[CNT_FIXED_ROWS + Structures.building_type_mapping_count + tiledata_entry.index, tiledata_entry.player]);
        Inc(tmp_stats[Byte(osAllStructures), tiledata_entry.player]);
        Inc(tmp_stats[Byte(osUnits), tiledata_entry.player]);
      end;
    end;
  // Show statistics on grid
  for i := 0 to cnt_fixed_rows + Structures.building_type_mapping_count + Structures.templates.UnitTypeCount - 1 do
  begin
    total_value := 0;
    for j := 0 to cnt_players - 1 do
    begin
      total_value := total_value + tmp_stats[i,j];
      if (i < cnt_fixed_rows) or (tmp_stats[i,j] > 0) then
        StatsGrid.Cells[j+1,i+1] := inttostr(tmp_stats[i,j])
      else
        StatsGrid.Cells[j+1,i+1] := '';
    end;
    if ((i < cnt_fixed_rows) or (total_value > 0)) and (i <> Byte(osPowerPercent)) then
      StatsGrid.Cells[9,i+1] := inttostr(total_value)
    else
      StatsGrid.Cells[9,i+1] := '';
  end;
end;

procedure TMapStatsDialog.update_structures_list(building_list, unit_list: TStringList);
var
  i: integer;
begin
  StatsGrid.RowCount := 1 + cnt_fixed_rows + Structures.building_type_mapping_count + Structures.templates.UnitTypeCount;
  for i := 0 to Structures.building_type_mapping_count - 1 do
    StatsGrid.Cells[0,1+cnt_fixed_rows+i] := building_list[i];
  for i := 0 to Structures.templates.UnitTypeCount - 1 do
    StatsGrid.Cells[0,1+cnt_fixed_rows+Structures.building_type_mapping_count+i] := unit_list[i];
end;

procedure TMapStatsDialog.update_player_list;
var
  i: integer;
begin
  for i := 0 to cnt_players - 1 do
    StatsGrid.Cells[i+1,0] := Structures.player_info[i].name;
end;

end.
