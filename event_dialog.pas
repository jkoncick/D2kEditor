unit event_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls;

type
  TEventDialog = class(TForm)
    EventGrid: TStringGrid;
    LowerPanel: TPanel;
    ConditionGrid: TStringGrid;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure fill_grids;
  end;

var
  EventDialog: TEventDialog;

implementation

uses mis_file;

{$R *.dfm}

{ TEventDialog }

procedure TEventDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  EventGrid.Cells[0,0] := '#';
  EventGrid.ColWidths[0] := 20;
  EventGrid.Cells[1,0] := 'Event type';
  EventGrid.ColWidths[1] := 100;
  EventGrid.Cells[2,0] := 'Position';
  EventGrid.ColWidths[2] := 50;
  EventGrid.Cells[3,0] := 'Player';
  EventGrid.ColWidths[3] := 64;
  EventGrid.Cells[4,0] := 'Contents';
  EventGrid.ColWidths[4] := 500;
  EventGrid.Cells[5,0] := 'Conditions';
  //EventGrid.ColWidths[5] := ClientWidth-752;
  EventGrid.ColWidths[5] := 1140;
  for i:= 1 to 64 do
  begin
    EventGrid.Cells[0,i] := inttostr(i-1);
  end;
  ConditionGrid.Cells[0,0] := '#';
  ConditionGrid.ColWidths[0] := 20;
  ConditionGrid.Cells[1,0] := 'Condition type';
  ConditionGrid.ColWidths[1] := 92;
  ConditionGrid.Cells[2,0] := 'Player';
  ConditionGrid.ColWidths[2] := 64;
  ConditionGrid.Cells[3,0] := 'Contents';
  ConditionGrid.ColWidths[3] := 120;
  for i:= 1 to 48 do
  begin
    ConditionGrid.Cells[0,i] := inttostr(i-1);
  end;
end;

procedure TEventDialog.FormShow(Sender: TObject);
begin
  fill_grids;
end;

procedure TEventDialog.fill_grids;
var
  i: integer;
  event: ^TEvent;
  et_info: ^TEventTypeInfo;
  cond: ^TCondition;
  ct_info: ^TConditionTypeInfo;
begin
  // Fill events
  for i := 1 to mis_data.num_events do
  begin
    event := Addr(mis_data.events[i-1]);
    et_info := Addr(event_type_info[event.event_type]);
    // Basic information
    EventGrid.Cells[1,i] := et_info.name;
    if et_info.use_map_position then
      EventGrid.Cells[2,i] := inttostr(event.map_pos_x) + ' , ' + inttostr(event.map_pos_y)
    else
      EventGrid.Cells[2,i] := '';
    if et_info.use_player_index then
      EventGrid.Cells[3,i] := player_names[event.player]
    else
      EventGrid.Cells[3,i] := '';
    // Contents
    EventGrid.Cells[4,i] := get_event_contents(i-1);
    // Conditions
    EventGrid.Cells[5,i] := get_event_conditions(i-1);
  end;
  // Clear unused event rows
  for i := mis_data.num_events + 1 to 64 do
  begin
    EventGrid.Rows[i].Clear;
    EventGrid.Cells[0,i] := inttostr(i-1);
  end;
  // Fill conditions
  for i := 1 to mis_data.num_conditions do
  begin
    cond := Addr(mis_data.conditions[i-1]);
    ct_info := Addr(condition_type_info[cond.condition_type]);
    // Basic information
    ConditionGrid.Cells[1,i] := ct_info.name;
    if ct_info.use_player_index then
      ConditionGrid.Cells[2,i] := player_names[cond.player]
    else
      ConditionGrid.Cells[2,i] := '';
    // Contents
    ConditionGrid.Cells[3,i] := get_condition_contents(i-1, false);
  end;
  // Clear unused condition rows
  for i := mis_data.num_conditions + 1 to 48 do
  begin
    ConditionGrid.Rows[i].Clear;
    ConditionGrid.Cells[0,i] := inttostr(i-1);
  end;
end;

end.
