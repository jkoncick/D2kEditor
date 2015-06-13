unit mission_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, mis_file, Buttons, ComCtrls, Grids,
  ValEdit;

type
  TMissionDialog = class(TForm)
    lblTechLevel: TLabel;
    lblStartingMoney: TLabel;
    SettingsPanel: TPanel;
    lblAllocIndex: TLabel;
    PlayerSettingsPanel: TPanel;
    RulesAndStringsPanel: TPanel;
    AITabControl: TTabControl;
    lblSetToAll: TLabel;
    seTechLevelAll: TSpinEdit;
    edStartingMoneyAll: TEdit;
    btnAllocIndexReset: TButton;
    btnAllegianceReset: TButton;
    lblTimeLimit: TLabel;
    edTimeLimit: TEdit;
    cbSetBothSides: TCheckBox;
    AIValueList: TValueListEditor;
    AIOptionsPanel: TPanel;
    btnImportAI: TButton;
    btnExportAI: TButton;
    btnCopyAI: TButton;
    cbCopyAITo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure seTechLevelAllChange(Sender: TObject);
    procedure edStartingMoneyAllChange(Sender: TObject);
    procedure btnAllocIndexResetClick(Sender: TObject);
    procedure btnAllegianceResetClick(Sender: TObject);
    procedure tech_level_change(Sender: TObject);
    procedure starting_money_change(Sender: TObject);
    procedure alloc_index_change(Sender: TObject);
    procedure allegiance_btn_click(Sender: TObject);
    procedure time_limit_change(Sender: TObject);
    procedure AITabControlChange(Sender: TObject);
    procedure AIValueListStringsChange(Sender: TObject);
    procedure btnCopyAIClick(Sender: TObject);
  private
    player_label: array[0..cnt_mis_players-1] of TLabel;
    tech_level: array[0..cnt_mis_players-1] of TSpinEdit;
    starting_money: array[0..cnt_mis_players-1] of TEdit;
    alloc_index: array[0..cnt_mis_players-1] of TSpinEdit;
    player_label_alleg: array[0..cnt_mis_players-1] of TLabel;
    allegiance_btn: array[0..cnt_mis_players-1, 0..cnt_mis_players-1] of TBitBtn;
  public
    procedure fill_data;
    procedure fill_ai_values;
  end;

var
  MissionDialog: TMissionDialog;

implementation

{$R *.dfm}

procedure TMissionDialog.FormCreate(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to cnt_mis_players-1 do
  begin
    // Initialize player labels
    player_label[i] := TLabel.Create(self);
    player_label[i].Left := 8;
    player_label[i].Top := 28 + i * 24;
    player_label[i].Caption := player_names[i];
    player_label[i].Parent := PlayerSettingsPanel;
    // Initialize tech levels
    tech_level[i] := TSpinEdit.Create(self);
    tech_level[i].Left := 72;
    tech_level[i].Top := 24 + i * 24;
    tech_level[i].Width := 56;
    tech_level[i].Height := 22;
    tech_level[i].MinValue := 0;
    tech_level[i].MaxValue := 255;
    tech_level[i].Value := 0;
    tech_level[i].Tag := i;
    tech_level[i].Parent := PlayerSettingsPanel;
    tech_level[i].OnChange := tech_level_change;
    // Initialize Starting money
    starting_money[i] := TEdit.Create(self);
    starting_money[i].Left := 136;
    starting_money[i].Top := 24 + i * 24;
    starting_money[i].Width := 80;
    starting_money[i].Height := 22;
    starting_money[i].Text := '0';
    starting_money[i].Tag := i;
    starting_money[i].Parent := PlayerSettingsPanel;
    starting_money[i].OnChange := starting_money_change;
    // Initialize allocation indexes
    alloc_index[i] := TSpinEdit.Create(self);
    alloc_index[i].Left := 224;
    alloc_index[i].Top := 24 + i * 24;
    alloc_index[i].Width := 56;
    alloc_index[i].Height := 22;
    alloc_index[i].MinValue := 0;
    alloc_index[i].MaxValue := 255;
    alloc_index[i].Value := 0;
    alloc_index[i].Tag := i;
    alloc_index[i].Parent := PlayerSettingsPanel;
    alloc_index[i].OnChange := alloc_index_change;
    // Initialize allegiance labels
    player_label_alleg[i] := TLabel.Create(self);
    player_label_alleg[i].Left := 290 + i * 52;
    player_label_alleg[i].Top := 8;
    player_label_alleg[i].Caption := player_names_short[i];
    player_label_alleg[i].Parent := PlayerSettingsPanel;
    // Initialize allegiance buttons
    for j := 0 to cnt_mis_players-1 do
    begin
      allegiance_btn[i,j] := TBitBtn.Create(self);
      allegiance_btn[i,j].Left := 288 + j * 52;
      allegiance_btn[i,j].Top := 24 + i * 24;
      allegiance_btn[i,j].Width := 52;
      allegiance_btn[i,j].Height := 22;
      allegiance_btn[i,j].Font.Style := [fsBold];
      allegiance_btn[i,j].Tag := i * 8 + j;
      allegiance_btn[i,j].Parent := PlayerSettingsPanel;
      allegiance_btn[i,j].OnClick := allegiance_btn_click;
    end;
    // Initialize AI PageControl
    AITabControl.Tabs.Add(player_names[i]);
    cbCopyAITo.Items.Add(inttostr(i) + ' - ' + player_names[i]);
  end;
  AIValueList.ColWidths[0] := 160;
  cbCopyAITo.ItemIndex := 0;
end;

procedure TMissionDialog.fill_data;
var
  i, j: integer;
begin
  for i := 0 to cnt_mis_players-1 do
  begin
    tech_level[i].Value := mis_data.tech_level[i];
    starting_money[i].Text := inttostr(mis_data.starting_money[i]);
    alloc_index[i].Value := mis_data.allocation_index[i];
    for j := 0 to cnt_mis_players-1 do
    begin
      allegiance_btn[i,j].Caption := allegiance_type[mis_data.allegiance[i,j]];
      allegiance_btn[i,j].Font.Color := allegiance_type_color[mis_data.allegiance[i,j]];
    end;
  end;
  edTimeLimit.Text := inttostr(mis_data.time_limit);
  fill_ai_values;
end;

procedure TMissionDialog.fill_ai_values;
var
  i: integer;
  tmp_strings: TValueListStrings;
begin
  tmp_strings := TValueListStrings.Create(nil);
  for i := 0 to Length(mis_data.ai_segments[0]) -1 do
  begin
    tmp_strings.Add('Byte ' + inttostr(i)+ '=' + inttostr(mis_data.ai_segments[AITabControl.TabIndex, i]));
  end;
  AIValueList.Strings := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TMissionDialog.seTechLevelAllChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    tech_level[i].Value := StrToIntDef(seTechLevelAll.Text,0);
end;

procedure TMissionDialog.edStartingMoneyAllChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    starting_money[i].Text := IntToStr(StrToIntDef(edStartingMoneyAll.Text,0));
end;

procedure TMissionDialog.btnAllocIndexResetClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    alloc_index[i].Value := i;
end;

procedure TMissionDialog.btnAllegianceResetClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    for j := 0 to cnt_mis_players-1 do
    begin
      if i = j then
        mis_data.allegiance[i,j] := 0
      else
        mis_data.allegiance[i,j] := 1;
      allegiance_btn[i,j].Caption := allegiance_type[mis_data.allegiance[i,j]];
      allegiance_btn[i,j].Font.Color := allegiance_type_color[mis_data.allegiance[i,j]];
    end;
end;

procedure TMissionDialog.tech_level_change(Sender: TObject);
begin
  mis_data.tech_level[(Sender as TSpinEdit).Tag] := StrToIntDef((Sender as TSpinEdit).Text, 0);
end;

procedure TMissionDialog.starting_money_change(Sender: TObject);
begin
  mis_data.starting_money[(Sender as TEdit).Tag] := StrToIntDef((Sender as TEdit).Text, 0);
end;

procedure TMissionDialog.alloc_index_change(Sender: TObject);
begin
  mis_data.allocation_index[(Sender as TSpinEdit).Tag] := StrToIntDef((Sender as TSpinEdit).Text, 0);
end;

procedure TMissionDialog.allegiance_btn_click(Sender: TObject);
var
  i, j: integer;
begin
  i := (Sender as TBitBtn).Tag div cnt_mis_players;
  j := (Sender as TBitBtn).Tag mod cnt_mis_players;
  mis_data.allegiance[i,j] := (mis_data.allegiance[i,j] + 1) mod Length(allegiance_type);
  allegiance_btn[i,j].Caption := allegiance_type[mis_data.allegiance[i,j]];
  allegiance_btn[i,j].Font.Color := allegiance_type_color[mis_data.allegiance[i,j]];
  if cbSetBothSides.Checked and (i <> j) then
  begin
    mis_data.allegiance[j,i] := (mis_data.allegiance[j,i] + 1) mod Length(allegiance_type);
    allegiance_btn[j,i].Caption := allegiance_type[mis_data.allegiance[j,i]];
    allegiance_btn[j,i].Font.Color := allegiance_type_color[mis_data.allegiance[j,i]];
  end;
end;

procedure TMissionDialog.time_limit_change(Sender: TObject);
begin
  mis_data.time_limit := StrToIntDef(edTimeLimit.Text, -1);
end;

procedure TMissionDialog.AITabControlChange(Sender: TObject);
begin
  fill_ai_values;
end;

procedure TMissionDialog.AIValueListStringsChange(Sender: TObject);
begin
  mis_data.ai_segments[AITabControl.TabIndex, AIValueList.Row-1] := strtointdef(AIValueList.Cells[1,AIValueList.Row],0);
end;

procedure TMissionDialog.btnCopyAIClick(Sender: TObject);
begin
  Move(mis_data.ai_segments[AITabControl.TabIndex], mis_data.ai_segments[cbCopyAITo.ItemIndex], Length(mis_data.ai_segments[0]));
  AITabControl.TabIndex := cbCopyAITo.ItemIndex;
end;

end.
