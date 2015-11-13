unit map_stats_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, _structures, _map;

const cnt_fixed_rows = 7;

type OverallStats = (osAllStructures, osBuildingsNoWalls, osBuildings, osUnits, osPowerOutput, osPowerNeed, osPowerPercent);

type
  TMapStatsDialog = class(TForm)
    StatsGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    tmp_stats: Array of Array[0..cnt_players-1] of integer;
  public
    procedure update_stats;
  end;

var
  MapStatsDialog: TMapStatsDialog;

implementation

{$R *.dfm}

procedure TMapStatsDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  StatsGrid.ColWidths[0] := 128;
  StatsGrid.Cells[0,1] := 'All structures';
  StatsGrid.Cells[0,2] := 'Buildings (without walls)';
  StatsGrid.Cells[0,3] := 'Buildings';
  StatsGrid.Cells[0,4] := 'Units';
  StatsGrid.Cells[0,5] := 'Power output';
  StatsGrid.Cells[0,6] := 'Power need';
  StatsGrid.Cells[0,7] := 'Power percent';
  for i := 0 to Structures.cnt_map_players - 1 do
    StatsGrid.Cells[i+1,0] := Structures.map_player_info[i].name;
  StatsGrid.RowCount := 1 + cnt_fixed_rows + Structures.cnt_structures;
  SetLength(tmp_stats, cnt_fixed_rows+Structures.cnt_structures);
  for i := 0 to Structures.cnt_structures - 1 do
    StatsGrid.Cells[0,1+i+cnt_fixed_rows] := Structures.structure_info[i].name;
end;

procedure TMapStatsDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
    close;
end;

procedure TMapStatsDialog.update_stats;
var
  i,j: integer;
  is_misc: boolean;
  player, index: word;
begin
  if not Map.loaded then
    exit;
  // Reset statistics
  for i := 0 to cnt_fixed_rows + Structures.cnt_structures - 1 do
    for j := 0 to cnt_players - 1 do
      tmp_stats[i,j] := 0;
  // Copy already computed power statistics
  for j := 0 to cnt_players - 1 do
  begin
    tmp_stats[Byte(osPowerOutput),j] := Map.stats.players[j].power_output;
    tmp_stats[Byte(osPowerNeed),j] := Map.stats.players[j].power_need;
    tmp_stats[Byte(osPowerPercent),j] := Map.stats.players[j].power_percent;
  end;
  // Compute map statistics
  for i := 0 to Map.width-1 do
    for j := 0 to Map.height-1 do
    begin
      if not Structures.special_value_to_params(Map.data[i,j].special,player,index,is_misc) then
        continue;
      if is_misc then
        continue;
      Inc(tmp_stats[index+cnt_fixed_rows,player]);
      Inc(tmp_stats[Byte(osAllStructures),player]);
      if index < Structures.first_unit_index then
      begin
        Inc(tmp_stats[Byte(osBuildings),player]);
        if index > 0 then
          Inc(tmp_stats[Byte(osBuildingsNoWalls),player]);
      end else
        Inc(tmp_stats[Byte(osUnits),player]);
    end;
  // Show statistics on grid
  for i := 0 to cnt_fixed_rows + Structures.cnt_structures - 1 do
    for j := 0 to cnt_players - 1 do
      if (i < cnt_fixed_rows) or (tmp_stats[i,j] > 0) then
        StatsGrid.Cells[j+1,i+1] := inttostr(tmp_stats[i,j])
      else
        StatsGrid.Cells[j+1,i+1] := '';
end;

end.
