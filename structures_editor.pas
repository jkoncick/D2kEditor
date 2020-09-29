unit structures_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, CheckLst, Spin, _structures,
  Grids, ValEdit, _utils;

const side_names: array[0..CNT_PLAYERS-1] of string = ('Atreides', 'Harkonnen', 'Ordos', 'Emperor', 'Fremen', 'Smugglers', 'Mercenaries', 'Sandworm');
const unit_voices: array[0..8] of string = ('A1', 'A2', 'A3', 'H1', 'H2', 'H3', 'O1', 'O2', 'O3');

type
  TStructuresEditor = class(TForm)
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    PageControl: TPageControl;
    PageBuildings: TTabSheet;
    PageUnits: TTabSheet;
    pnBuildingTypeList: TPanel;
    lblBuildingTypeList: TLabel;
    lbBuildingTypeList: TListBox;
    pnBuildingList: TPanel;
    lblBuildingList: TLabel;
    lbBuildingList: TListBox;
    gbBuildingBasic: TGroupBox;
    gbBuildingBuildRequirements: TGroupBox;
    gbBuildingSpaceRequirements: TGroupBox;
    gbBuildingProperties: TGroupBox;
    gbBuildingTurret: TGroupBox;
    gbBuildingVisuals: TGroupBox;
    clbBuildingOwnerSide: TCheckListBox;
    lblBuildingOwnerSide: TLabel;
    edBuildingName: TEdit;
    lblBuildingName: TLabel;
    cbxBuildingType: TComboBox;
    lblBuildingType: TLabel;
    imgBuildingIcon: TImage;
    lblBuildingRequirementsTech: TLabel;
    lblBuildingRequirementsCost: TLabel;
    lblBuildingRequirementsSpeed: TLabel;
    seBuildingTechLevelBuild: TSpinEdit;
    seBuildingTechLevelUpgrade1: TSpinEdit;
    seBuildingTechLevelUpgrade2: TSpinEdit;
    seBuildingTechLevelUpgrade3: TSpinEdit;
    lblBuildingRequirementsBuild: TLabel;
    lblBuildingRequirementsUpgrade1: TLabel;
    lblBuildingRequirementsUpgrade2: TLabel;
    lblBuildingRequirementsUpgrade3: TLabel;
    edBuildingCostBuild: TEdit;
    edBuildingCostUpgrade1: TEdit;
    edBuildingCostUpgrade2: TEdit;
    edBuildingCostUpgrade3: TEdit;
    edBuildingBuildSpeedBuild: TEdit;
    edBuildingBuildSpeedUpgrade1: TEdit;
    edBuildingBuildSpeedUpgrade2: TEdit;
    edBuildingBuildSpeedUpgrade3: TEdit;
    lblBuildingPrereq1BuildingType: TLabel;
    lblBuildingPrereq2BuildingType: TLabel;
    clbBuildingPrereq1OwnerSide: TCheckListBox;
    clbBuildingPrereq2OwnerSide: TCheckListBox;
    cbxBuildingPrereq1BuildingType: TComboBox;
    cbxBuildingPrereq2BuildingType: TComboBox;
    seBuildingPrereq1UpgradesNeeded: TSpinEdit;
    seBuildingPrereq2UpgradesNeeded: TSpinEdit;
    lblBuildingPrereq1UpgradesNeeded: TLabel;
    lblBuildingPrereq2UpgradesNeeded: TLabel;
    lblBuildingPrereq1OwnerSide: TLabel;
    lblBuildingPrereq2OwnerSide: TLabel;
    lblBuildingHitPoints: TLabel;
    lblBuildingArmorType: TLabel;
    edBuildingHitPoints: TEdit;
    cbxBuildingArmorType: TComboBox;
    lblBuildingPowerConsumption: TLabel;
    lblBuildingHealthBarSize: TLabel;
    lblBuildingSpecialBehavior: TLabel;
    edBuildingPowerConsumption: TEdit;
    cbxBuildingHealthBarSize: TComboBox;
    cbxBuildingSpecialBehavior: TComboBox;
    cbBuildingFlagSELECT_REPAIR: TCheckBox;
    cbBuildingFlagCAN_CAPTURE: TCheckBox;
    cbBuildingFlagALWAYS_DECAY: TCheckBox;
    cbBuildingFlagCANNOT_SELL: TCheckBox;
    lblBuildingSightRadius: TLabel;
    seBuildingSightRadius: TSpinEdit;
    cbBuildingActLikeTurret: TCheckBox;
    cbBuildingRequireEnoughPower: TCheckBox;
    lblBuildingPrimaryWeapon: TLabel;
    lblBuildingSecondaryWeapon: TLabel;
    cbxBuildingPrimaryWeapon: TComboBox;
    cbxBuildingSecondaryWeapon: TComboBox;
    lblBuildingRateOfFire: TLabel;
    lblBuildingBarrelRotationSpeed: TLabel;
    seBuildingRateOfFire: TSpinEdit;
    seBuildingBarrelRotationSpeed: TSpinEdit;
    imgBuildingImage: TImage;
    lblBuildingBuildingArt: TLabel;
    cbxBuildingBuildingArt: TComboBox;
    lblBuildingBarrelArt: TLabel;
    cbxBuildingBarrelArt: TComboBox;
    lblBuildingArtWidth: TLabel;
    lblBuildingArtHeight: TLabel;
    edBuildingArtWidth: TEdit;
    edBuildingArtHeight: TEdit;
    cbxBuildingBuildingAnimation: TComboBox;
    cbBuildingFlagHAS_ANIMATION: TCheckBox;
    cbBuildingFlagANIM_PERMANENT: TCheckBox;
    lblBuildingBuildingAnimation: TLabel;
    lblBuildingAnimationSpeed: TLabel;
    seBuildingAnimationSpeed: TSpinEdit;
    lblBuildingBuildupArt: TLabel;
    cbxBuildingBuildupArt: TComboBox;
    lblBuildingBuildupFramesToShow: TLabel;
    seBuildingBuildupFramesToShow: TSpinEdit;
    imgBuildingTilesOccupiedAll: TImage;
    imgBuildingTilesOccupiedSolid: TImage;
    lblBuildingTilesOccupiedAll: TLabel;
    lblBuildingTilesOccupiedSolid: TLabel;
    lblBuildingExitPoint1X: TLabel;
    lblBuildingExitPoint1Y: TLabel;
    lblBuildingExitPoint2X: TLabel;
    lblBuildingExitPoint2Y: TLabel;
    seBuildingExitPoint1X: TSpinEdit;
    seBuildingExitPoint1Y: TSpinEdit;
    seBuildingExitPoint2X: TSpinEdit;
    seBuildingExitPoint2Y: TSpinEdit;
    cbBuildingFlagHAS_SKIRT: TCheckBox;
    cbBuildingFlagNO_CONCRETE: TCheckBox;
    lblBuildingDeathExplosion: TLabel;
    lblBuildingFiringExplosion: TLabel;
    cbxBuildingDeathExplosion: TComboBox;
    cbxBuildingFiringExplosion: TComboBox;
    sgBuildingDirectionFrames: TStringGrid;
    lblBuildingDirectionFrames: TLabel;
    gbBuildingOtherUnknown: TGroupBox;
    lblBuildingUnknown93: TLabel;
    seBuildingUnknown93: TSpinEdit;
    lblBuildingFlags: TLabel;
    edBuildingFlags: TEdit;
    cbBuildingFlagAUTOREPAIR: TCheckBox;
    cbBuildingFlagUNKNOWN9: TCheckBox;
    cbBuildingFlagANIM_ALPHA: TCheckBox;
    seBuildingUnknown8: TSpinEdit;
    lblBuildingUnknown8: TLabel;
    btnBuildingDirectionFrames0: TButton;
    btnBuildingDirectionFrames8: TButton;
    btnBuildingDirectionFrames32: TButton;
    btnBuildingBuildingAnimationPlay: TButton;
    tmBuildingBuildingAnimation: TTimer;
    btnBuildingBuildupArtPlay: TButton;
    tmBuildingBuildupArt: TTimer;
    pnUnitTypeList: TPanel;
    lblUnitTypeList: TLabel;
    lbUnitTypeList: TListBox;
    pnUnitList: TPanel;
    lblUnitList: TLabel;
    lbUnitList: TListBox;
    gbUnitBasic: TGroupBox;
    lblUnitOwnerSide: TLabel;
    lblUnitName: TLabel;
    lblUnitType: TLabel;
    imgUnitIcon: TImage;
    clbUnitOwnerSide: TCheckListBox;
    edUnitName: TEdit;
    cbxUnitType: TComboBox;
    gbUnitBuildRequirements: TGroupBox;
    lblUnitTechLevel: TLabel;
    lblUnitCost: TLabel;
    lblUnitBuildSpeed: TLabel;
    lblUnitPrereq1BuildingType: TLabel;
    lblUnitPrereq2BuildingType: TLabel;
    lblUnitPrereq1UpgradesNeeded: TLabel;
    lblUnitPrereq1OwnerSide: TLabel;
    seUnitTechLevel: TSpinEdit;
    edUnitCost: TEdit;
    edUnitBuildSpeed: TEdit;
    clbUnitPrereq1OwnerSide: TCheckListBox;
    cbxUnitPrereq1BuildingType: TComboBox;
    cbxUnitPrereq2BuildingType: TComboBox;
    seUnitPrereq1UpgradesNeeded: TSpinEdit;
    cbUnitAvailableInStarport: TCheckBox;
    cbUnitMultiplayerOnly: TCheckBox;
    gbUnitVoices: TGroupBox;
    gbUnitProperties: TGroupBox;
    lblUnitHitPoints: TLabel;
    lblUnitArmorType: TLabel;
    lblUnitHealthBarSize: TLabel;
    lblUnitSpecialBehavior: TLabel;
    lblUnitSightRadius: TLabel;
    edUnitHitPoints: TEdit;
    cbxUnitArmorType: TComboBox;
    cbxUnitHealthBarSize: TComboBox;
    cbxUnitSpecialBehavior: TComboBox;
    cbUnitIsInfantry: TCheckBox;
    cbUnitFlagUF_STEALTH: TCheckBox;
    cbUnitFlagUF_SELFHEALING: TCheckBox;
    seUnitSightRadius: TSpinEdit;
    gbUnitMovement: TGroupBox;
    gbUnitWeapons: TGroupBox;
    lblUnitPrimaryWeapon: TLabel;
    lblUnitSecondaryWeapon: TLabel;
    lblUnitRateOfFire: TLabel;
    lblUnitBarrelRotationSpeed: TLabel;
    cbUnitHasBarrel: TCheckBox;
    cbUnitFlagUF_FIXED_BARREL: TCheckBox;
    cbxUnitPrimaryWeapon: TComboBox;
    cbxUnitSecondaryWeapon: TComboBox;
    seUnitRateOfFire: TSpinEdit;
    seUnitBarrelRotationSpeed: TSpinEdit;
    edUnitSpeed: TEdit;
    lblUnitSpeed: TLabel;
    lblUnitSpeedType: TLabel;
    cbxUnitSpeedType: TComboBox;
    lblUnitUnitRotationSpeed: TLabel;
    seUnitUnitRotationSpeed: TSpinEdit;
    gbUnitVisuals: TGroupBox;
    imgUnitImage: TImage;
    lblUnitUnitArt: TLabel;
    lblUnitBarrelArt: TLabel;
    lblUnitDeathExplosion: TLabel;
    lblUnitFiringExplosion: TLabel;
    lblUnitDirectionFrames: TLabel;
    cbxUnitUnitArt: TComboBox;
    cbxUnitBarrelArt: TComboBox;
    cbxUnitDeathExplosion: TComboBox;
    cbxUnitFiringExplosion: TComboBox;
    sgUnitDirectionFrames: TStringGrid;
    btnUnitDirectionFrames0: TButton;
    btnUnitDirectionFrames8: TButton;
    btnUnitDirectionFrames32: TButton;
    gbUnitOtherUnknown: TGroupBox;
    lblUnitUnknown52: TLabel;
    lblUnitFlags: TLabel;
    lblUnitUnknown46: TLabel;
    seUnitUnknown52: TSpinEdit;
    edUnitFlags: TEdit;
    seUnitUnknown46: TSpinEdit;
    lblUnitUnknown55: TLabel;
    seUnitUnknown55: TSpinEdit;
    seUnitUnknown164: TSpinEdit;
    lblUnitUnknown164: TLabel;
    cbUnitCanCrushInfantry: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    cbUnitFlagUF_NO_AI: TCheckBox;
    PageWeapons: TTabSheet;
    PageExplosions: TTabSheet;
    PageArmor: TTabSheet;
    PageSpeed: TTabSheet;
    sgSpeedValues: TStringGrid;
    btnBuildingTypeAdd: TButton;
    btnBuildingTypeRemove: TButton;
    edBuildingTypeName: TEdit;
    btnBuildingTypeRename: TButton;
    btnBuildingAdd: TButton;
    btnBuildingRemove: TButton;
    btnBuildingCopy: TButton;
    btnBuildingPaste: TButton;
    Applychanges1: TMenuItem;
    Savetofiles1: TMenuItem;
    Saveandtest1: TMenuItem;
    Reloadfiles1: TMenuItem;
    CopyfilestoModsfolder1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Applychanges1Click(Sender: TObject);
    procedure Savetofiles1Click(Sender: TObject);
    procedure Saveandtest1Click(Sender: TObject);
    procedure Reloadfiles1Click(Sender: TObject);
    procedure CopyfilestoModsfolder1Click(Sender: TObject);
    procedure lbBuildingTypeListClick(Sender: TObject);
    procedure btnBuildingTypeAddClick(Sender: TObject);
    procedure btnBuildingTypeRemoveClick(Sender: TObject);
    procedure btnBuildingTypeRenameClick(Sender: TObject);
    procedure lbBuildingListClick(Sender: TObject);
    procedure btnBuildingAddClick(Sender: TObject);
    procedure btnBuildingRemoveClick(Sender: TObject);
    procedure btnBuildingCopyClick(Sender: TObject);
    procedure btnBuildingPasteClick(Sender: TObject);
    procedure imgBuildingTilesOccupiedAllMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgBuildingTilesOccupiedSolidMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure edBuildingFlagsChange(Sender: TObject);
    procedure BuildingFlagCheckboxChange(Sender: TObject);
    procedure btnBuildingDirectionFramesClick(Sender: TObject);
    procedure RedrawBuildingPreview(Sender: TObject);
    procedure btnBuildingBuildingAnimationPlayClick(Sender: TObject);
    procedure tmBuildingBuildingAnimationTimer(Sender: TObject);
    procedure btnBuildingBuildupArtPlayClick(Sender: TObject);
    procedure tmBuildingBuildupArtTimer(Sender: TObject);
    procedure lbUnitTypeListClick(Sender: TObject);
    procedure lbUnitListClick(Sender: TObject);
    procedure edUnitFlagsChange(Sender: TObject);
    procedure UnitFlagCheckboxChange(Sender: TObject);
    procedure btnUnitDirectionFramesClick(Sender: TObject);
    procedure RedrawUnitPreview(Sender: TObject);
  private
    cbxUnitVoices: array[0..17] of TComboBox;
    lblUnitVoices: array[0..17] of TLabel;

    clipboard_format_building: cardinal;
    clipboard_format_unit: cardinal;
    loading: boolean;
    last_building_type_index: integer;
    last_building_index: integer;
    last_unit_type_index: integer;
    last_unit_index: integer;
    tmp_building_tiles_occupied_all: cardinal;
    tmp_building_tiles_occupied_solid: cardinal;
  public
    procedure fill_data;
    procedure fill_building_data;
    procedure fill_unit_data;
    procedure set_owner_side_field_value(control: TCheckListBox; value: byte);
    procedure draw_icon_no_image_sign(img_target: TImage);
    procedure draw_building_tile_map(img_target: TImage; value: cardinal);
    procedure draw_building_preview(draw_building: boolean);
    procedure draw_building_frame(image_index: integer; alpha, animation: boolean);
    procedure draw_unit_preview;
    procedure draw_unit_frame(image_index, side: integer; is_stealth: boolean);
    procedure store_data;
    procedure store_building_data;
    procedure store_unit_data;
    function get_owner_side_field_value(control: TCheckListBox): byte;
    procedure apply_changes;
    procedure save_to_files;
  end;

var
  StructuresEditor: TStructuresEditor;

implementation

uses _tileset, _stringtable, Clipbrd, map_stats_dialog, event_dialog,
  mission_dialog, main, _launcher, Math;

{$R *.dfm}

{ TStructuresEditor }

procedure TStructuresEditor.FormCreate(Sender: TObject);
var
  tmp_strings: TStringList;
  dummy: boolean;
  i: integer;
  c1, c2, f: Int64;
begin
  clipboard_format_building := RegisterClipboardFormat('D2kEditorBuildingTemplate');
  clipboard_format_unit := RegisterClipboardFormat('D2kEditorUnitTemplate');
  QueryPerformanceFrequency(f);
  QueryPerformanceCounter(c1);
  tmp_strings := TStringList.Create;
  // Side names
  for i := 0 to CNT_PLAYERS - 1 do
    tmp_strings.Add(side_names[i]);
  clbBuildingOwnerSide.Items := tmp_strings;
  clbBuildingPrereq1OwnerSide.Items := tmp_strings;
  clbBuildingPrereq2OwnerSide.Items := tmp_strings;
  clbUnitOwnerSide.Items := tmp_strings;
  clbUnitPrereq1OwnerSide.Items := tmp_strings;
  // Unit voices combo boxes
  tmp_strings.Clear;
  for i := 0 to SoundStringTable.get_table_size - 1 do
    tmp_strings.Add(inttostr(i) + ' - ' + SoundStringTable.get_text(i, false, dummy));
  for i := 0 to 17 do
  begin
    cbxUnitVoices[i] := TComboBox.Create(self);
    cbxUnitVoices[i].Style := csDropDownList;
    cbxUnitVoices[i].Parent := gbUnitVoices;
    cbxUnitVoices[i].Width := 109;
    cbxUnitVoices[i].Left := 32 + 140 * (i div 9);
    cbxUnitVoices[i].Top := 36 + 24 * (i mod 9);
    cbxUnitVoices[i].Items := tmp_strings;
    lblUnitVoices[i] := TLabel.Create(self);
    lblUnitVoices[i].Parent := gbUnitVoices;
    lblUnitVoices[i].Left := 8 + 140 * (i div 9);
    lblUnitVoices[i].Top := 36 + 24 * (i mod 9);
    lblUnitVoices[i].Caption := unit_voices[i mod 9] + ':';
  end;

  tmp_strings.Destroy;
  QueryPerformanceCounter(c2);
  //Caption := floattostr((c2-c1)/f);
  // Speed modifier values
  sgSpeedValues.Cells[0, 0] := 'Modifier val.';
  for i := 0 to Length(Structures.speed.Values) - 1 do
    sgSpeedValues.Cells[0, i+1] := inttostr(i);
end;

procedure TStructuresEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
    Close;
end;

procedure TStructuresEditor.Applychanges1Click(Sender: TObject);
begin
  apply_changes;
end;

procedure TStructuresEditor.Savetofiles1Click(Sender: TObject);
begin
  apply_changes;
  save_to_files;
end;

procedure TStructuresEditor.Saveandtest1Click(Sender: TObject);
begin
  if not MainWindow.check_map_can_be_tested then
    exit;
  apply_changes;
  save_to_files;
  Launcher.launch_current_mission;
end;

procedure TStructuresEditor.Reloadfiles1Click(Sender: TObject);
begin
  Structures.load_templates_bin(true);
  Structures.load_armour_bin(true);
  Structures.load_speed_bin(true);
  fill_data;
  apply_changes;
end;

procedure TStructuresEditor.CopyfilestoModsfolder1Click(Sender: TObject);
begin
  beep;
end;

procedure TStructuresEditor.lbBuildingTypeListClick(Sender: TObject);
begin
  last_building_type_index := lbBuildingTypeList.ItemIndex;
  edBuildingTypeName.Text := Structures.templates.BuildingTypeStrings[lbBuildingTypeList.ItemIndex];
end;

procedure TStructuresEditor.btnBuildingTypeAddClick(Sender: TObject);
var
  index: integer;
begin
  if Structures.templates.BuildingTypeCount = MAX_BUILDING_TYPES then
    exit;
  store_data;
  index := Structures.templates.BuildingTypeCount;
  store_c_string(edBuildingTypeName.Text, Addr(Structures.templates.BuildingTypeStrings[index]), Length(Structures.templates.BuildingTypeStrings[index]));
  Inc(Structures.templates.BuildingTypeCount);
  last_building_type_index := index;
  fill_data;
end;

procedure TStructuresEditor.btnBuildingTypeRemoveClick(Sender: TObject);
var
  index: integer;
begin
  if Structures.templates.BuildingTypeCount = 0 then
    exit;
  store_data;
  index := Structures.templates.BuildingTypeCount - 1;
  FillChar(Structures.templates.BuildingTypeStrings[index], Length(Structures.templates.BuildingTypeStrings[index]), 0);
  Dec(Structures.templates.BuildingTypeCount);
  if last_building_type_index = index then
    Dec(last_building_type_index);
  fill_data;
end;

procedure TStructuresEditor.btnBuildingTypeRenameClick(Sender: TObject);
var
  index: integer;
begin
  store_data;
  index := lbBuildingTypeList.ItemIndex;
  store_c_string(edBuildingTypeName.Text, Addr(Structures.templates.BuildingTypeStrings[index]), Length(Structures.templates.BuildingTypeStrings[index]));
  last_building_type_index := index;
  fill_data;
end;

procedure TStructuresEditor.lbBuildingListClick(Sender: TObject);
begin
  if not loading then
    store_building_data;
  fill_building_data;
end;

procedure TStructuresEditor.btnBuildingAddClick(Sender: TObject);
begin
  if Structures.templates.BuildingCount = MAX_BUILDING_TYPES then
    exit;
  store_data;
  last_building_index := Structures.templates.BuildingCount;
  Inc(Structures.templates.BuildingCount);
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.btnBuildingRemoveClick(Sender: TObject);
var
  index: integer;
begin
  if Structures.templates.BuildingCount = 0 then
    exit;
  store_data;
  index := Structures.templates.BuildingCount - 1;
  if last_building_index = index then
    Dec(last_building_index);
  Dec(Structures.templates.BuildingCount);
  FillChar(Structures.templates.BuildingNameStrings[index], Length(Structures.templates.BuildingNameStrings[index]), 0);
  FillChar(Structures.templates.BuildingDefinitions[index], Sizeof(TBuildingTemplate), 0);
  Structures.compute_image_indexes;
  fill_data;
end;

procedure TStructuresEditor.btnBuildingCopyClick(Sender: TObject);
var
  handle: THandle;
  pointer: TBuildingTemplatePtr;
begin
  if lbBuildingList.ItemIndex < 0 then
    exit;
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TBuildingTemplate));
  pointer := GlobalLock(handle);

  store_data;
  Move(Structures.templates.BuildingDefinitions[lbBuildingList.ItemIndex], pointer^, sizeof(TBuildingTemplate));

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format_building, handle);
  CloseClipboard;
end;

procedure TStructuresEditor.btnBuildingPasteClick(Sender: TObject);
var
  handle: THandle;
  pointer: TBuildingTemplatePtr;
begin
  if (lbBuildingList.ItemIndex < 0) or not Clipboard.HasFormat(clipboard_format_building) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format_building);
  pointer := GlobalLock(handle);

  Move(pointer^, Structures.templates.BuildingDefinitions[lbBuildingList.ItemIndex], sizeof(TBuildingTemplate));

  fill_building_data;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TStructuresEditor.imgBuildingTilesOccupiedAllMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X div 17;
  Y := Y div 17;
  if Button = mbLeft then
    tmp_building_tiles_occupied_all := tmp_building_tiles_occupied_all or (1 shl (Y * MAX_BUILDING_SIZE + x))
  else if Button = mbRight then
    tmp_building_tiles_occupied_all := tmp_building_tiles_occupied_all and (not (1 shl (Y * MAX_BUILDING_SIZE + x)));
  draw_building_tile_map(imgBuildingTilesOccupiedAll, tmp_building_tiles_occupied_all);
  draw_building_preview(true);
end;

procedure TStructuresEditor.imgBuildingTilesOccupiedSolidMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X div 17;
  Y := Y div 17;
  if Button = mbLeft then
    tmp_building_tiles_occupied_solid := tmp_building_tiles_occupied_solid or (1 shl (Y * MAX_BUILDING_SIZE + x))
  else if Button = mbRight then
    tmp_building_tiles_occupied_solid := tmp_building_tiles_occupied_solid and (not (1 shl (Y * MAX_BUILDING_SIZE + x)));
  draw_building_tile_map(imgBuildingTilesOccupiedSolid, tmp_building_tiles_occupied_solid);
end;

procedure TStructuresEditor.edBuildingFlagsChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edBuildingFlags.Text);
  loading := true;
  cbBuildingFlagSELECT_REPAIR.Checked := (value and BF_SELECT_REPAIR) <> 0;
  cbBuildingFlagCAN_CAPTURE.Checked := (value and BF_CAN_CAPTURE) <> 0;
  cbBuildingFlagALWAYS_DECAY.Checked := (value and BF_ALWAYS_DECAY) <> 0;
  cbBuildingFlagCANNOT_SELL.Checked := (value and BF_CANNOT_SELL) <> 0;
  cbBuildingFlagHAS_ANIMATION.Checked := (value and BF_HAS_ANIMATION) <> 0;
  cbBuildingFlagANIM_PERMANENT.Checked := (value and BF_ANIM_PERMANENT) <> 0;
  cbBuildingFlagANIM_ALPHA.Checked := (value and BF_ANIM_ALPHA) <> 0;
  cbBuildingFlagHAS_SKIRT.Checked := (value and BF_HAS_SKIRT) <> 0;
  cbBuildingFlagNO_CONCRETE.Checked := (value and BF_NO_CONCRETE) <> 0;
  cbBuildingFlagAUTOREPAIR.Checked := (value and BF_AUTOREPAIR) <> 0;
  cbBuildingFlagUNKNOWN9.Checked := (value and BF_UNKNOWN9) <> 0;
  loading := false;
  draw_building_preview(true);
end;

procedure TStructuresEditor.BuildingFlagCheckboxChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edBuildingFlags.Text);
  if (Sender as TCheckBox).Checked then
    value := value or  Cardinal((Sender as TCheckBox).Tag)
  else
    value := value and (not Cardinal((Sender as TCheckBox).Tag));
  loading := true;
  edBuildingFlags.Text := IntToHex(value, 8);
  loading := false;
  draw_building_preview(true);
end;

procedure TStructuresEditor.btnBuildingDirectionFramesClick(Sender: TObject);
var
  i: integer;
  count: integer;
  row, col: integer;
begin
  count := (Sender as TButton).Tag;
  for i := 0 to 31 do
  begin
    row := i div 8;
    col := i mod 8;
    if count = 0 then
      sgBuildingDirectionFrames.Cells[col, row] := inttostr(0)
    else if count = 32 then
      sgBuildingDirectionFrames.Cells[col, row] := inttostr(i)
    else if count = 8 then
      sgBuildingDirectionFrames.Cells[col, row] := inttostr(((i + 2) div 4) mod 8);
  end;
end;

procedure TStructuresEditor.RedrawBuildingPreview(Sender: TObject);
begin
  if loading then
    exit;
  draw_building_preview(true);
end;

procedure TStructuresEditor.btnBuildingBuildingAnimationPlayClick(Sender: TObject);
begin
  if tmBuildingBuildingAnimation.Enabled then
  begin
    tmBuildingBuildingAnimation.Enabled := false;
    btnBuildingBuildingAnimationPlay.Caption := 'Play';
    draw_building_preview(true);
  end else
  begin
    tmBuildingBuildingAnimation.Enabled := true;
    tmBuildingBuildingAnimation.Tag := 0;
    btnBuildingBuildingAnimationPlay.Caption := 'Stop';
  end;
end;

procedure TStructuresEditor.tmBuildingBuildingAnimationTimer(Sender: TObject);
var
  num_frames: integer;
  anim_speed: integer;
  current_frame: integer;
begin
  if tmBuildingBuildupArt.Enabled or (not cbBuildingFlagHAS_ANIMATION.Checked) then
    exit;
  num_frames := Structures.templates.BuildingAnimationFrames[cbxBuildingBuildingAnimation.ItemIndex];
  anim_speed := seBuildingAnimationSpeed.Value;
  if (num_frames = 0) or (anim_speed = 0) then
    exit;
  if tmBuildingBuildingAnimation.Tag mod anim_speed = 0 then
  begin
    current_frame := (tmBuildingBuildingAnimation.Tag div anim_speed) mod num_frames;
    draw_building_preview(true);
    draw_building_frame(Structures.building_animation_image_indexes[cbxBuildingBuildingAnimation.ItemIndex] + current_frame, cbBuildingFlagANIM_ALPHA.Checked, true);
  end;
  tmBuildingBuildingAnimation.Tag := tmBuildingBuildingAnimation.Tag + 1;
end;

procedure TStructuresEditor.btnBuildingBuildupArtPlayClick(Sender: TObject);
begin
  tmBuildingBuildupArt.Enabled := true;
  tmBuildingBuildupArt.Tag := 0;
end;

procedure TStructuresEditor.tmBuildingBuildupArtTimer(Sender: TObject);
var
  num_frames: integer;
  buildup_frames_to_show: integer;
begin
  num_frames := Structures.templates.BuildupArtFrames[cbxBuildingBuildupArt.ItemIndex];
  buildup_frames_to_show := seBuildingBuildupFramesToShow.Value;
  if tmBuildingBuildupArt.Tag >= num_frames then
  begin
    // Whole animation was played
    tmBuildingBuildupArt.Enabled := false;
    draw_building_preview(true);
    exit;
  end;
  draw_building_preview(tmBuildingBuildupArt.Tag >= buildup_frames_to_show);
  draw_building_frame(Structures.buildup_art_image_indexes[cbxBuildingBuildupArt.ItemIndex] + tmBuildingBuildupArt.Tag, false, true);
  tmBuildingBuildupArt.Tag := tmBuildingBuildupArt.Tag + 1;
end;

procedure TStructuresEditor.lbUnitTypeListClick(Sender: TObject);
begin
  loading := loading;
end;

procedure TStructuresEditor.lbUnitListClick(Sender: TObject);
begin
  if not loading then
    store_unit_data;
  fill_unit_data;
end;

procedure TStructuresEditor.edUnitFlagsChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edUnitFlags.Text);
  loading := true;
  cbUnitFlagUF_STEALTH.Checked := (value and UF_STEALTH) <> 0;
  cbUnitFlagUF_NO_AI.Checked := (value and UF_NO_AI) <> 0;
  cbUnitFlagUF_SELFHEALING.Checked := (value and UF_SELFHEALING) <> 0;
  cbUnitFlagUF_FIXED_BARREL.Checked := (value and UF_FIXED_BARREL) <> 0;
  loading := false;
  draw_unit_preview;
end;

procedure TStructuresEditor.UnitFlagCheckboxChange(Sender: TObject);
var
  value: cardinal;
begin
  if loading then
    exit;
  value := strtoint('$' + edUnitFlags.Text);
  if (Sender as TCheckBox).Checked then
    value := value or  Cardinal((Sender as TCheckBox).Tag)
  else
    value := value and (not Cardinal((Sender as TCheckBox).Tag));
  loading := true;
  edUnitFlags.Text := IntToHex(value, 8);
  loading := false;
  draw_unit_preview;
end;

procedure TStructuresEditor.btnUnitDirectionFramesClick(Sender: TObject);
var
  i: integer;
  count: integer;
  row, col: integer;
begin
  count := (Sender as TButton).Tag;
  for i := 0 to 31 do
  begin
    row := i div 8;
    col := i mod 8;
    if count = 0 then
      sgUnitDirectionFrames.Cells[col, row] := inttostr(0)
    else if count = 32 then
      sgUnitDirectionFrames.Cells[col, row] := inttostr(i)
    else if count = 8 then
      sgUnitDirectionFrames.Cells[col, row] := inttostr(((i + 2) div 4) mod 8);
  end;
end;

procedure TStructuresEditor.RedrawUnitPreview(Sender: TObject);
begin
  if loading then
    exit;
  draw_unit_preview;
end;

procedure TStructuresEditor.fill_data;
var
  tmp_strings: TStringList;
  i: integer;
  j: integer;
begin
  tmp_strings := TStringList.Create;

  // Building type list
  for i := 0 to Structures.templates.BuildingTypeCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.BuildingTypeStrings[i]]));
  lbBuildingTypeList.Items := tmp_strings;
  cbxBuildingType.Items := tmp_strings;
  // Building name list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.BuildingNameStrings[i]]));
  lbBuildingList.Items := tmp_strings;
  // Unit type list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.UnitTypeCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.UnitTypeStrings[i]]));
  lbUnitTypeList.Items := tmp_strings;
  cbxUnitType.Items := tmp_strings;
  // Unit name list
  tmp_strings.Clear;
  for i := 0 to Structures.templates.UnitCount - 1 do
    tmp_strings.Add(Format('%.*d %s', [2, i, Structures.templates.UnitNameStrings[i]]));
  lbUnitList.Items := tmp_strings;

  // Building type selection
  tmp_strings.Clear;
  tmp_strings.Add('(none)');
  for i := 0 to Structures.templates.BuildingTypeCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.templates.BuildingTypeStrings[i]]));
  cbxBuildingPrereq1BuildingType.Items := tmp_strings;
  cbxBuildingPrereq2BuildingType.Items := tmp_strings;
  cbxUnitPrereq1BuildingType.Items := tmp_strings;
  cbxUnitPrereq2BuildingType.Items := tmp_strings;
  // Weapon selection
  tmp_strings.Clear;
  tmp_strings.Add('(none)');
  for i := 0 to Structures.templates.WeaponCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.templates.WeaponStrings[i]]));
  cbxBuildingPrimaryWeapon.Items := tmp_strings;
  cbxBuildingSecondaryWeapon.Items := tmp_strings;
  cbxUnitPrimaryWeapon.Items := tmp_strings;
  cbxUnitSecondaryWeapon.Items := tmp_strings;
  // Explosion selection
  tmp_strings.Clear;
  tmp_strings.Add('(none)');
  for i := 0 to Structures.templates.ExplosionCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.templates.ExplosionStrings[i]]));
  cbxBuildingDeathExplosion.Items := tmp_strings;
  cbxBuildingFiringExplosion.Items := tmp_strings;
  cbxUnitDeathExplosion.Items := tmp_strings;
  cbxUnitFiringExplosion.Items := tmp_strings;
  // Building art selection
  tmp_strings.Clear;
  tmp_strings.Add('(none)');
  for i := 0 to Structures.templates.BuildingArtCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%dd)', [2, i, Structures.building_art_image_indexes[i], Structures.templates.BuildingArtDirections[i]]));
  cbxBuildingBuildingArt.Items := tmp_strings;
  cbxBuildingBarrelArt.Items := tmp_strings;
  // Building animation selection
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%df)', [2, i, Structures.building_animation_image_indexes[i], Structures.templates.BuildingAnimationFrames[i]]));
  cbxBuildingBuildingAnimation.Items := tmp_strings;
  // Buildup art selection
  tmp_strings.Clear;
  for i := 0 to Structures.templates.BuildingCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%df)', [2, i, Structures.buildup_art_image_indexes[i], Structures.templates.BuildupArtFrames[i]]));
  cbxBuildingBuildupArt.Items := tmp_strings;
  // Unit art selection
  tmp_strings.Clear;
  tmp_strings.Add('(none)');
  for i := 0 to Structures.templates.UnitArtCount - 1 do
    tmp_strings.Add(Format('%.*d - %4d (%dd %df)', [2, i, Structures.unit_art_image_indexes[i], Structures.templates.UnitArtDirectionFrames[i], Structures.templates.UnitArtAnimationFrames[i]]));
  cbxUnitUnitArt.Items := tmp_strings;
  cbxUnitBarrelArt.Items := tmp_strings;
  // Armour type selection
  tmp_strings.Clear;
  for i := 0 to Structures.armour.ArmourTypeCount - 1 do
    tmp_strings.Add(Format('%.*d - %s', [2, i, Structures.armour.ArmourTypeStrings[i]]));
  cbxBuildingArmorType.Items := tmp_strings;
  cbxUnitArmorType.Items := tmp_strings;
  // Speed type selection
  tmp_strings.Clear;
  for i := 0 to Length(Structures.speed.SpeedNameStrings) - 1 do
    tmp_strings.Add(Format('%.*d - %s', [1, i, Structures.speed.SpeedNameStrings[i]]));
  cbxUnitSpeedType.Items := tmp_strings;

  // Speeds
  for i := 0 to Length(Structures.speed.SpeedNameStrings) - 1 do
    sgSpeedValues.Cells[i+1, 0] := Structures.speed.SpeedNameStrings[i];
  for i := 0 to Length(Structures.speed.Values) - 1 do
    for j := 0 to Length(Structures.speed.Values[i]) - 1 do
      sgSpeedValues.Cells[j+1, i+1] := floattostr(Round(Structures.speed.Values[i, j] * 100)/100);

  tmp_strings.Destroy;

  // Last indexes
  lbBuildingTypeList.ItemIndex := last_building_type_index;
  lbBuildingTypeListClick(nil);
  lbBuildingList.ItemIndex := last_building_index;
  loading := true;
  lbBuildingListClick(nil);
  lbUnitTypeList.ItemIndex := last_unit_type_index;
  lbUnitTypeListClick(nil);
  lbUnitList.ItemIndex := last_unit_index;
  loading := true;
  lbUnitListClick(nil);
  loading := false;

  // Status bar
  StatusBar.Panels[0].Text := Structures.templates_bin_filename;
end;

procedure TStructuresEditor.fill_building_data;
var
  index: integer;
  i: integer;
  bld: TBuildingTemplatePtr;
  icon: TStructureImagePtr;
  was_already_loaded: boolean;
begin
  index := lbBuildingList.ItemIndex;
  if index < 0 then
    exit;
  loading := true;
  last_building_index := index;
  bld := Addr(Structures.templates.BuildingDefinitions[index]);
  // Basic group box
  edBuildingName.Text := Structures.templates.BuildingNameStrings[index];
  cbxBuildingType.ItemIndex := bld.BuildingType;
  icon := Structures.get_structure_image(Structures.first_building_icon_image_index + index, 0, false, false, was_already_loaded);
  if icon <> nil then
  begin
    imgBuildingIcon.Picture.Bitmap.Assign(icon.bitmap);
    Structures.clear_last_structure_image(Structures.first_building_icon_image_index + index, false);
  end else
    draw_icon_no_image_sign(imgBuildingIcon);
  set_owner_side_field_value(clbBuildingOwnerSide, bld.OwnerSide);
  // Build requirements group box
  seBuildingTechLevelBuild.Value := bld.TechLevelBuild;
  seBuildingTechLevelUpgrade1.Value := bld.TechLevelUpgrade1;
  seBuildingTechLevelUpgrade2.Value := bld.TechLevelUpgrade2;
  seBuildingTechLevelUpgrade3.Value := bld.TechLevelUpgrade3;
  edBuildingCostBuild.Text := inttostr(bld.CostBuild);
  edBuildingCostUpgrade1.Text := inttostr(bld.CostUpgrade1);
  edBuildingCostUpgrade2.Text := inttostr(bld.CostUpgrade2);
  edBuildingCostUpgrade3.Text := inttostr(bld.CostUpgrade3);
  edBuildingBuildSpeedBuild.Text := inttostr(bld.BuildSpeedBuild);
  edBuildingBuildSpeedUpgrade1.Text := inttostr(bld.BuildSpeedUpgrade1);
  edBuildingBuildSpeedUpgrade2.Text := inttostr(bld.BuildSpeedUpgrade2);
  edBuildingBuildSpeedUpgrade3.Text := inttostr(bld.BuildSpeedUpgrade3);
  cbxBuildingPrereq1BuildingType.ItemIndex := bld.Prereq1BuildingType + 1;
  set_owner_side_field_value(clbBuildingPrereq1OwnerSide, bld.Prereq1OwnerSide);
  seBuildingPrereq1UpgradesNeeded.Value := bld.Prereq1UpgradesNeeded;
  cbxBuildingPrereq2BuildingType.ItemIndex := bld.Prereq2BuildingType + 1;
  set_owner_side_field_value(clbBuildingPrereq2OwnerSide, bld.Prereq2OwnerSide);
  seBuildingPrereq2UpgradesNeeded.Value := bld.Prereq2UpgradesNeeded;
  // Properties and behavior group box
  edBuildingHitPoints.Text := inttostr(bld.HitPoints);
  cbxBuildingArmorType.ItemIndex := bld.ArmorType;
  edBuildingPowerConsumption.Text := inttostr(bld.PowerConsumption);
  cbxBuildingHealthBarSize.ItemIndex := bld.HealthBarSize;
  seBuildingSightRadius.Value := bld.SightRadius;
  cbxBuildingSpecialBehavior.ItemIndex := bld.SpecialBehavior;
  // Turret properties group box
  cbBuildingActLikeTurret.Checked := bld.ActLikeTurret <> 0;
  cbBuildingRequireEnoughPower.Checked := bld.RequireEnoughPower <> 0;
  cbxBuildingPrimaryWeapon.ItemIndex := bld.PrimaryWeapon + 1;
  cbxBuildingSecondaryWeapon.ItemIndex := bld.SecondaryWeapon + 1;
  seBuildingRateOfFire.Value := bld.RateOfFire;
  seBuildingBarrelRotationSpeed.Value := bld.BarrelRotationSpeed;
  // Visuals and animations group box
  cbxBuildingBuildingArt.ItemIndex := bld.BuildingArt + 1;
  cbxBuildingBarrelArt.ItemIndex := bld.BarrelArt + 1;
  edBuildingArtWidth.Text := inttostr(bld.ArtWidth);
  edBuildingArtHeight.Text := inttostr(bld.ArtHeight);
  cbxBuildingBuildingAnimation.ItemIndex := bld.BuildingAnimation;
  seBuildingAnimationSpeed.Value := bld.AnimationSpeed;
  cbxBuildingBuildupArt.ItemIndex := bld.BuildupArt;
  seBuildingBuildupFramesToShow.Value := bld.BuildupFramesToShow;
  cbxBuildingDeathExplosion.ItemIndex := bld.DeathExplosion + 1;
  cbxBuildingFiringExplosion.ItemIndex := bld.FiringExplosion + 1;
  for i := 0 to 31 do
    sgBuildingDirectionFrames.Cells[i mod 8, i div 8] := inttostr(bld.DirectionFrames[i]);
  // Space requirements group box
  tmp_building_tiles_occupied_all := bld.TilesOccupiedAll;
  draw_building_tile_map(imgBuildingTilesOccupiedAll, bld.TilesOccupiedAll);
  tmp_building_tiles_occupied_solid := bld.TilesOccupiedSolid;
  draw_building_tile_map(imgBuildingTilesOccupiedSolid, bld.TilesOccupiedSolid);
  seBuildingExitPoint1X.Value := bld.ExitPoint1X;
  seBuildingExitPoint1Y.Value := bld.ExitPoint1Y;
  seBuildingExitPoint2X.Value := bld.ExitPoint2X;
  seBuildingExitPoint2Y.Value := bld.ExitPoint2Y;
  // Others and unknown group box
  seBuildingUnknown8.Value := bld.Unknown8;
  seBuildingUnknown93.Value := bld.Unknown93;
  edBuildingFlags.Text := IntToHex(bld.Flags, 8);

  loading := false;
  edBuildingFlagsChange(nil);
end;

procedure TStructuresEditor.fill_unit_data;
var
  index: integer;
  i: integer;
  unt: TUnitTemplatePtr;
  icon: TStructureImagePtr;
  was_already_loaded: boolean;
begin
  index := lbUnitList.ItemIndex;
  if index < 0 then
    exit;
  last_unit_index := index;
  loading := true;
  unt := Addr(Structures.templates.UnitDefinitions[index]);
  // Basic group box
  edUnitName.Text := Structures.templates.UnitNameStrings[index];
  cbxUnitType.ItemIndex := unt.UnitType;
  icon := Structures.get_structure_image(Structures.first_unit_icon_image_index + index, 0, false, false, was_already_loaded);
  if icon <> nil then
  begin
    imgUnitIcon.Picture.Bitmap.Assign(icon.bitmap);
    Structures.clear_last_structure_image(Structures.first_unit_icon_image_index + index, false);
  end else
    draw_icon_no_image_sign(imgUnitIcon);
  set_owner_side_field_value(clbUnitOwnerSide, unt.OwnerSide);
  // Build requirements group box
  seUnitTechLevel.Value := unt.TechLevel;
  edUnitCost.Text := inttostr(unt.Cost);
  edUnitBuildSpeed.Text := inttostr(unt.BuildSpeed);
  cbxUnitPrereq1BuildingType.ItemIndex := unt.Prereq1BuildingType + 1;
  set_owner_side_field_value(clbUnitPrereq1OwnerSide, unt.Prereq1OwnerSide);
  seUnitPrereq1UpgradesNeeded.Value := unt.Prereq1UpgradesNeeded;
  cbxUnitPrereq2BuildingType.ItemIndex := unt.Prereq2BuildingType + 1;
  cbUnitAvailableInStarport.Checked := unt.AvailableInStarport <> 0;
  cbUnitMultiplayerOnly.Checked := unt.MultiplayerOnly <> 0;
  // Voices group box
  for i := 0 to Length(unt.Voices) - 1 do
    cbxUnitVoices[i].ItemIndex := unt.Voices[i];
  // Properties and behavior group box
  edUnitHitPoints.Text := inttostr(unt.HitPoints);
  cbxUnitArmorType.ItemIndex := unt.ArmorType;
  seUnitSightRadius.Value := unt.SightRadius;
  cbxUnitHealthBarSize.ItemIndex := unt.HealthBarSize;
  cbUnitIsInfantry.Checked := unt.IsInfantry <> 0;
  cbxUnitSpecialBehavior.ItemIndex := unt.SpecialBehavior;
  cbUnitCanCrushInfantry.Checked := unt.CanCrushInfantry <> 0;
  // Movement group box
  edUnitSpeed.Text := inttostr(unt.Speed shr 12);
  cbxUnitSpeedType.ItemIndex := unt.SpeedType;
  seUnitUnitRotationSpeed.Value := unt.UnitRotationSpeed;
  // Weapons group box
  cbxUnitPrimaryWeapon.ItemIndex := unt.PrimaryWeapon + 1;
  cbxUnitSecondaryWeapon.ItemIndex := unt.SecondaryWeapon + 1;
  cbUnitHasBarrel.Checked := unt.HasBarrel <> 0;
  seUnitRateOfFire.Value := unt.RateOfFire;
  seUnitBarrelRotationSpeed.Value := unt.BarrelRotationSpeed;
  // Visuals and animations group box
  cbxUnitUnitArt.ItemIndex := unt.UnitArt + 1;
  cbxUnitBarrelArt.ItemIndex := unt.BarrelArt + 1;
  cbxUnitDeathExplosion.ItemIndex := unt.DeathExplosion + 1;
  cbxUnitFiringExplosion.ItemIndex := unt.FiringExplosion + 1;
  for i := 0 to 31 do
    sgUnitDirectionFrames.Cells[i mod 8, i div 8] := inttostr(unt.DirectionFrames[i]);
  // Others and unknown group box
  seUnitUnknown46.Value := unt.Unknown46;
  seUnitUnknown52.Value := unt.Unknown52;
  seUnitUnknown55.Value := unt.Unknown55;
  seUnitUnknown164.Value := unt.Unknown164;
  edUnitFlags.Text := IntToHex(unt.Flags, 8);

  loading := false;
  edUnitFlagsChange(nil);
end;

procedure TStructuresEditor.set_owner_side_field_value(control: TCheckListBox; value: byte);
var
  i: integer;
begin
  for i := 0 to CNT_PLAYERS - 1 do
    control.Checked[i] := (value and (1 shl i)) <> 0;
end;

procedure TStructuresEditor.draw_icon_no_image_sign(img_target: TImage);
begin
  img_target.Canvas.Pen.Color := clGray;
  img_target.Canvas.Brush.Color := clGray;
  img_target.Canvas.Brush.Style := bsSolid;
  img_target.Canvas.Rectangle(0, 0, imgUnitIcon.Width, imgUnitIcon.Height);
  img_target.Canvas.Font.Color := clWhite;
  img_target.Canvas.TextOut(7, 17, 'No Image');
end;

procedure TStructuresEditor.draw_building_tile_map(img_target: TImage; value: cardinal);
var
  x, y: integer;
begin
  img_target.Canvas.Pen.Color := clBlack;
  img_target.Canvas.Brush.Style := bsSolid;
  for y := 0 to MAX_BUILDING_SIZE - 1 do
    for x := 0 to MAX_BUILDING_SIZE - 1 do
    begin
      if (value and (1 shl (y * MAX_BUILDING_SIZE + x))) <> 0 then
        img_target.Canvas.Brush.Color := clGray
      else
        img_target.Canvas.Brush.Color := clWhite;
      img_target.Canvas.Rectangle(x*17, y*17, x*17+16, y*17+16);
    end;
end;

procedure TStructuresEditor.draw_building_preview(draw_building: boolean);
var
  art_width, art_height: integer;
  i, x, y: integer;
  paint_tile_group, start_index, tile_index, tile_x, tile_y: integer;
  src_rect, dest_rect: TRect;
  building_skirt: ^TBuildingSkirt;
  bottom_offset: integer;
begin
  art_width := strtointdef(edBuildingArtWidth.Text, 0);
  art_height := strtointdef(edBuildingArtHeight.Text, 0);
  imgBuildingImage.Canvas.CopyMode := cmSrcCopy;
  // Find proper paint tile group with buildable tiles
  paint_tile_group := 0;
  for i := 0 to cnt_paint_tile_groups - 1 do
    if Tileset.get_tile_type(Tileset.paint_tile_groups[i].tile_index) = ttBuildable then
    begin
      paint_tile_group := i;
      break;
    end;
  // Find starting tile index for paint tile group
  start_index := 0;
  for i := -4 to paint_tile_group - 1 do
    inc(start_index, Tileset.paint_tile_groups[i].paint_tiles_cnt);
  // Draw terrain
  for y := 0 to 4 do
    for x := 0 to 3 do
    begin
      if Tileset.paint_tile_groups[paint_tile_group].paint_tiles_cnt > 0 then
        tile_index := Tileset.paint_tiles[start_index + (y * 4 + x) mod Tileset.paint_tile_groups[paint_tile_group].paint_tiles_cnt]
      else
        tile_index := Tileset.paint_tile_groups[paint_tile_group].tile_index;
      tile_x := tile_index mod 20;
      tile_y := tile_index div 20;
      dest_rect := Rect((x)*32, (y)*32, (x)*32 + 32, (y)*32 + 32);
      src_rect := Rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32);
      imgBuildingImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
    end;
  // Draw concrete under building
  if not cbBuildingFlagNO_CONCRETE.Checked then
  begin
    for x := 0 to MAX_BUILDING_SIZE - 1 do
      for y := 0 to MAX_BUILDING_SIZE - 1 do
      begin
        // Check if tile is occupied by building
        if (tmp_building_tiles_occupied_all and (1 shl (y * MAX_BUILDING_SIZE + x))) = 0 then
          continue;
        tile_index := CONCRETE_TILES[(x + y) mod Length(CONCRETE_TILES)];
        tile_x := tile_index mod 20;
        tile_y := tile_index div 20;
        dest_rect := Rect((x)*32, (y+1)*32, (x)*32 + 32, (y+1)*32 + 32);
        src_rect := Rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32);
        imgBuildingImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      end;
  end;
  // Draw building's skirt
  if cbBuildingFlagHAS_SKIRT.Checked then
  begin
    building_skirt := nil;
    if art_width = 64 then
      building_skirt := Addr(BUILDING_SKIRT_2x2);
    if art_width = 96 then
      building_skirt := Addr(BUILDING_SKIRT_3x2);
    if building_skirt <> nil then
    begin
      bottom_offset := (art_height div 32) - building_skirt.size_y;
      for y := 0 to building_skirt.size_y - 1 do
        for x := 0 to building_skirt.size_x - 1 do
        begin
          dest_rect := Rect((x)*32, (y + 1 + bottom_offset)*32, (x)*32 + 32, (y + 1 + bottom_offset)*32 + 32);
          if (not cbBuildingFlagNO_CONCRETE.Checked) and ((tmp_building_tiles_occupied_all and (1 shl ((y + bottom_offset) * MAX_BUILDING_SIZE + x))) <> 0) then
            src_rect := Rect((x + building_skirt.conc_tile_x)*32, (y + building_skirt.conc_tile_y)*32, (x + building_skirt.conc_tile_x)*32+32, (y + building_skirt.conc_tile_y)*32+32)
          else
            src_rect := Rect((x + building_skirt.rock_tile_x)*32, (y + building_skirt.rock_tile_y)*32, (x + building_skirt.rock_tile_x)*32+32, (y + building_skirt.rock_tile_y)*32+32);
          imgBuildingImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
        end;
    end;
  end;
  // Draw building frames
  if not draw_building then
    exit;
  if cbxBuildingBuildingArt.ItemIndex <> 0 then
  begin
    draw_building_frame(Structures.building_art_image_indexes[cbxBuildingBuildingArt.ItemIndex-1], false, false);
    draw_building_frame(Structures.building_art_image_indexes[cbxBuildingBuildingArt.ItemIndex-1] + 1, false, false);
  end;
  if cbxBuildingBarrelArt.ItemIndex <> 0 then
  begin
    draw_building_frame(Structures.building_art_image_indexes[cbxBuildingBarrelArt.ItemIndex-1] + 1, false, false);
  end;
end;

procedure TStructuresEditor.draw_building_frame(image_index: integer; alpha, animation: boolean);
var
  side: integer;
  i: integer;
  art_height: integer;
  structure_image: TStructureImagePtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
  turret_offset_x: integer;
begin
  side := 0;
  for i := 0 to CNT_PLAYERS - 1 do
    if clbBuildingOwnerSide.Checked[i] then
    begin
      side := i;
      break;
    end;
  structure_image := Structures.get_structure_image(image_index, side, false, false, was_already_loaded);
  if structure_image <> nil then
  begin
    art_height := strtointdef(edBuildingArtHeight.Text, 0);
    turret_offset_x := 0;
    if (cbxBuildingSpecialBehavior.ItemIndex = 16) and (not animation) then
      turret_offset_x := seBuildingExitPoint1X.Value;
    src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
    dest_rect := Rect(structure_image.offset_x + turret_offset_x, art_height - structure_image.offset_y + 32, structure_image.offset_x + turret_offset_x + structure_image.bitmap.Width, art_height - structure_image.offset_y + structure_image.bitmap.Height + 32);
    if not alpha then
    begin
      imgBuildingImage.Canvas.CopyMode := cmSrcAnd;
      imgBuildingImage.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
    end;
    imgBuildingImage.Canvas.CopyMode := cmSrcPaint;
    imgBuildingImage.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
    if not was_already_loaded then
      Structures.clear_last_structure_image(image_index, false);
  end;
end;

procedure TStructuresEditor.draw_unit_preview;
var
  i: integer;
  side: integer;
  is_stealth: boolean;
begin
  // Get side
  side := 0;
  for i := 0 to CNT_PLAYERS - 1 do
    if clbUnitOwnerSide.Checked[i] then
    begin
      side := i;
      break;
    end;
  // Draw background
  imgUnitImage.Canvas.Brush.Style := bsSolid;
  imgUnitImage.Canvas.Pen.Color := clWhite;
  imgUnitImage.Canvas.Brush.Color := clWhite;
  imgUnitImage.Canvas.Rectangle(0, 0, 80, 80);
  imgUnitImage.Canvas.Pen.Color := $E0E0E0;
  imgUnitImage.Canvas.Brush.Color := $E0E0E0;
  imgUnitImage.Canvas.Rectangle(24, 24, 56, 56);
  imgUnitImage.Canvas.Brush.Color := clWhite;
  // Draw unit frames
  is_stealth := cbUnitFlagUF_STEALTH.Checked or (cbxUnitSpecialBehavior.ItemIndex = 12) or (edUnitName.Text = 'STEALTH RAIDER');
  if cbxUnitUnitArt.ItemIndex <> 0 then
  begin
    draw_unit_frame(Structures.unit_art_image_indexes[cbxUnitUnitArt.ItemIndex-1], side, is_stealth);
  end;
  if cbxUnitBarrelArt.ItemIndex <> 0 then
  begin
    draw_unit_frame(Structures.unit_art_image_indexes[cbxUnitBarrelArt.ItemIndex-1], side, is_stealth);
  end;
end;

procedure TStructuresEditor.draw_unit_frame(image_index, side: integer; is_stealth: boolean);
var
  structure_image: TStructureImagePtr;
  was_already_loaded: boolean;
  src_rect, dest_rect: TRect;
begin
  structure_image := Structures.get_structure_image(image_index, side, true, is_stealth, was_already_loaded);
  if structure_image <> nil then
  begin
    src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
    dest_rect := Rect(structure_image.offset_x + 24, structure_image.offset_y + 24, structure_image.offset_x + 24 + structure_image.bitmap.Width, structure_image.offset_y + 24 + structure_image.bitmap.Height);
    imgUnitImage.Canvas.CopyMode := cmSrcAnd;
    imgUnitImage.Canvas.CopyRect(dest_rect, structure_image.bitmap_mask.Canvas, src_rect);
    imgUnitImage.Canvas.CopyMode := cmSrcPaint;
    imgUnitImage.Canvas.CopyRect(dest_rect, structure_image.bitmap.Canvas, src_rect);
    if not was_already_loaded then
      Structures.clear_last_structure_image(image_index, is_stealth);
  end;
end;

procedure TStructuresEditor.store_data;
begin
  store_building_data;
  store_unit_data;
end;

procedure TStructuresEditor.store_building_data;
var
  index: integer;
  i: integer;
  bld: TBuildingTemplatePtr;
  str: String;
begin
  index := last_building_index;
  if index < 0 then
    exit;
  bld := Addr(Structures.templates.BuildingDefinitions[index]);
  // Basic group box
  store_c_string(edBuildingName.Text, Addr(Structures.templates.BuildingNameStrings[index]), Length(Structures.templates.BuildingNameStrings[index]));
  str := Format('%.*d %s', [2, index, Structures.templates.BuildingNameStrings[index]]);
  if lbBuildingList.Items[index] <> str then
    lbBuildingList.Items[index] := str;
  bld.BuildingType := cbxBuildingType.ItemIndex;
  bld.OwnerSide := get_owner_side_field_value(clbBuildingOwnerSide);
  // Build requirements group box
  bld.TechLevelBuild := seBuildingTechLevelBuild.Value;
  bld.TechLevelUpgrade1 := seBuildingTechLevelUpgrade1.Value;
  bld.TechLevelUpgrade2 := seBuildingTechLevelUpgrade2.Value;
  bld.TechLevelUpgrade3 := seBuildingTechLevelUpgrade3.Value;
  bld.CostBuild := strtointdef(edBuildingCostBuild.Text, 0);
  bld.CostUpgrade1 := strtointdef(edBuildingCostUpgrade1.Text, 0);
  bld.CostUpgrade2 := strtointdef(edBuildingCostUpgrade2.Text, 0);
  bld.CostUpgrade3 := strtointdef(edBuildingCostUpgrade3.Text, 0);
  bld.BuildSpeedBuild := strtointdef(edBuildingBuildSpeedBuild.Text, 0);
  bld.BuildSpeedUpgrade1 := strtointdef(edBuildingBuildSpeedUpgrade1.Text, 0);
  bld.BuildSpeedUpgrade2 := strtointdef(edBuildingBuildSpeedUpgrade2.Text, 0);
  bld.BuildSpeedUpgrade3 := strtointdef(edBuildingBuildSpeedUpgrade3.Text, 0);
  bld.Prereq1BuildingType := cbxBuildingPrereq1BuildingType.ItemIndex - 1;
  bld.Prereq1OwnerSide := get_owner_side_field_value(clbBuildingPrereq1OwnerSide);
  bld.Prereq1UpgradesNeeded := seBuildingPrereq1UpgradesNeeded.Value;
  bld.Prereq2BuildingType := cbxBuildingPrereq2BuildingType.ItemIndex - 1;
  bld.Prereq2OwnerSide := get_owner_side_field_value(clbBuildingPrereq2OwnerSide);
  bld.Prereq2UpgradesNeeded := seBuildingPrereq2UpgradesNeeded.Value;
  // Properties and behavior group box
  bld.HitPoints := strtointdef(edBuildingHitPoints.Text, 0);
  bld.ArmorType := cbxBuildingArmorType.ItemIndex;
  bld.PowerConsumption := strtointdef(edBuildingPowerConsumption.Text, 0);
  bld.HealthBarSize := cbxBuildingHealthBarSize.ItemIndex;
  bld.SightRadius := seBuildingSightRadius.Value;
  bld.SpecialBehavior := cbxBuildingSpecialBehavior.ItemIndex;
  // Turret properties group box
  bld.ActLikeTurret := IfThen(cbBuildingActLikeTurret.Checked, 1, 0);
  bld.RequireEnoughPower := IfThen(cbBuildingRequireEnoughPower.Checked, 1, 0);
  bld.PrimaryWeapon := cbxBuildingPrimaryWeapon.ItemIndex - 1;
  bld.SecondaryWeapon := cbxBuildingSecondaryWeapon.ItemIndex - 1;
  bld.RateOfFire := seBuildingRateOfFire.Value;
  bld.BarrelRotationSpeed := seBuildingBarrelRotationSpeed.Value;
  // Visuals and animations group box
  bld.BuildingArt := cbxBuildingBuildingArt.ItemIndex - 1;
  bld.BarrelArt := cbxBuildingBarrelArt.ItemIndex - 1;
  bld.ArtWidth := strtointdef(edBuildingArtWidth.Text, 0);
  bld.ArtHeight := strtointdef(edBuildingArtHeight.Text, 0);
  bld.BuildingAnimation := cbxBuildingBuildingAnimation.ItemIndex;
  bld.AnimationSpeed := seBuildingAnimationSpeed.Value;
  bld.BuildupArt := cbxBuildingBuildupArt.ItemIndex;
  bld.BuildupFramesToShow := seBuildingBuildupFramesToShow.Value;
  bld.DeathExplosion := cbxBuildingDeathExplosion.ItemIndex - 1;
  bld.FiringExplosion := cbxBuildingFiringExplosion.ItemIndex - 1;
  for i := 0 to 31 do
    bld.DirectionFrames[i] := strtointdef(sgBuildingDirectionFrames.Cells[i mod 8, i div 8], 0);
  // Space requirements group box
  bld.TilesOccupiedAll := tmp_building_tiles_occupied_all;
  bld.TilesOccupiedSolid := tmp_building_tiles_occupied_solid;
  bld.ExitPoint1X := seBuildingExitPoint1X.Value;
  bld.ExitPoint1Y := seBuildingExitPoint1Y.Value;
  bld.ExitPoint2X := seBuildingExitPoint2X.Value;
  bld.ExitPoint2Y := seBuildingExitPoint2Y.Value;
  // Others and unknown group box
  bld.Unknown8 := seBuildingUnknown8.Value;
  bld.Unknown93 := seBuildingUnknown93.Value;
  bld.Flags := strtointdef('$' + edBuildingFlags.Text, 0);
end;

procedure TStructuresEditor.store_unit_data;
var
  index: integer;
  i: integer;
  unt: TUnitTemplatePtr;
  str: string;
begin
  index := last_unit_index;
  if index < 0 then
    exit;
  unt := Addr(Structures.templates.UnitDefinitions[index]);
  // Basic group box
  store_c_string(edUnitName.Text, Addr(Structures.templates.UnitNameStrings[index]), Length(Structures.templates.UnitNameStrings[index]));
  str := Format('%.*d %s', [2, index, Structures.templates.UnitNameStrings[index]]);
  if lbUnitList.Items[index] <> str then
    lbUnitList.Items[index] := str;
  unt.UnitType := cbxUnitType.ItemIndex;
  unt.OwnerSide := get_owner_side_field_value(clbUnitOwnerSide);
  // Build requirements group box
  unt.TechLevel := seUnitTechLevel.Value;
  unt.Cost := strtointdef(edUnitCost.Text, 0);
  unt.BuildSpeed := strtointdef(edUnitBuildSpeed.Text, 0);
  unt.Prereq1BuildingType := cbxUnitPrereq1BuildingType.ItemIndex - 1;
  unt.Prereq1OwnerSide := get_owner_side_field_value(clbUnitPrereq1OwnerSide);
  unt.Prereq1UpgradesNeeded := seUnitPrereq1UpgradesNeeded.Value;
  unt.Prereq2BuildingType := cbxUnitPrereq2BuildingType.ItemIndex - 1;
  unt.AvailableInStarport := IfThen(cbUnitAvailableInStarport.Checked, 1, 0);
  unt.MultiplayerOnly := IfThen(cbUnitMultiplayerOnly.Checked, 1, 0);
  // Voices group box
  for i := 0 to Length(unt.Voices) - 1 do
    unt.Voices[i] := cbxUnitVoices[i].ItemIndex;
  // Properties and behavior group box
  unt.HitPoints := strtointdef(edUnitHitPoints.Text, 0);
  unt.ArmorType := cbxUnitArmorType.ItemIndex;
  unt.SightRadius := seUnitSightRadius.Value;
  unt.HealthBarSize := cbxUnitHealthBarSize.ItemIndex;
  unt.IsInfantry := IfThen(cbUnitIsInfantry.Checked, 1, 0);
  unt.SpecialBehavior := cbxUnitSpecialBehavior.ItemIndex;
  // Movement group box
  unt.Speed := strtointdef(edUnitSpeed.Text, 0) shl 12;
  unt.SpeedType := cbxUnitSpeedType.ItemIndex;
  unt.UnitRotationSpeed := seUnitUnitRotationSpeed.Value;
  unt.CanCrushInfantry := IfThen(cbUnitCanCrushInfantry.Checked, 1, 0);
  // Weapons group box
  unt.PrimaryWeapon := cbxUnitPrimaryWeapon.ItemIndex - 1;
  unt.SecondaryWeapon := cbxUnitSecondaryWeapon.ItemIndex - 1;
  unt.HasBarrel := IfThen(cbUnitHasBarrel.Checked, 1, 0);
  unt.RateOfFire := seUnitRateOfFire.Value;
  unt.BarrelRotationSpeed := seUnitBarrelRotationSpeed.Value;
  // Visuals and animations group box
  unt.UnitArt := cbxUnitUnitArt.ItemIndex - 1;
  unt.BarrelArt := cbxUnitBarrelArt.ItemIndex - 1;
  unt.DeathExplosion := cbxUnitDeathExplosion.ItemIndex - 1;
  unt.FiringExplosion := cbxUnitFiringExplosion.ItemIndex - 1;
  for i := 0 to 31 do
    unt.DirectionFrames[i] := strtointdef(sgUnitDirectionFrames.Cells[i mod 8, i div 8], 0);
  // Others and unknown group box
  unt.Unknown46 := seUnitUnknown46.Value;
  unt.Unknown52 := seUnitUnknown52.Value;
  unt.Unknown55 := seUnitUnknown55.Value;
  unt.Unknown164 := seUnitUnknown164.Value;
  unt.Flags := strtointdef('$' + edUnitFlags.Text, 0);
end;

function TStructuresEditor.get_owner_side_field_value(control: TCheckListBox): byte;
var
  i: integer;
begin
  result := 0;
  for i := 0 to CNT_PLAYERS - 1 do
    if control.Checked[i] then
      result := result or (1 shl i);
end;

procedure TStructuresEditor.apply_changes;
begin
  store_data;
  Structures.compute_building_and_unit_side_versions;
  Structures.compute_building_type_mapping;
  MainWindow.update_structures_list;
  MapStatsDialog.update_structures_list;
  EventDialog.update_structures_list;
  Structures.update_mis_ai_properties;
  MainWindow.render_map;
  MainWindow.render_minimap;
  EventDialog.update_contents;
  MissionDialog.fill_ai_values;
end;

procedure TStructuresEditor.save_to_files;
begin
  Structures.save_templates_bin;
end;

end.
