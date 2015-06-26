unit vars;

interface

type
  TRuleDefinition = record
    name: String;
    default_value: String;
    help_text: String;
  end;

const rule_definitions: array[0..27] of TRuleDefinition =
  (
    (name:'harvestUnloadDelay'; default_value:'60'; help_text:'';),
    (name:'harvestBlobValue'; default_value:'100'; help_text:'The value of each blob of spice (a harvester can carry up to 7)';),
    (name:'harvestLoadSpiceDelay'; default_value:'60'; help_text:'';),
    (name:'starportUpdateDelay'; default_value:'1500'; help_text:'';),
    (name:'starportStockIncreaseDelay'; default_value:'1500'; help_text:'';),
    (name:'starportStockIncreaseProb'; default_value:'10'; help_text:'';),
    (name:'starportCostVariationPercent'; default_value:'25'; help_text:'';),
    (name:'starportFrigateDelay'; default_value:'1500'; help_text:'';),
    (name:'refineryExplosionOffsetX'; default_value:'62'; help_text:'';),
    (name:'refineryExplosionOffsetY'; default_value:'-79'; help_text:'';),
    (name:'HarvesterDriveDistance'; default_value:'64'; help_text:'';),
    (name:'RepairDriveDistance'; default_value:'64'; help_text:'';),
    (name:'BuildingRepairValue'; default_value:'12'; help_text:'';),
    (name:'UnitRepairValue'; default_value:'8'; help_text:'';),
    (name:'SinglePlayerDelay'; default_value:'2'; help_text:'';),
    (name:'NumberOfFremen'; default_value:'2'; help_text:'';),
    (name:'SandWormAppetite'; default_value:'3'; help_text:'';),
    (name:'SandWormInitialSleep'; default_value:'50'; help_text:'';),
    (name:'SandWormFedSleep'; default_value:'40'; help_text:'';),
    (name:'SandWormShotSleep'; default_value:'90'; help_text:'';),
    (name:'NumberOfCrates'; default_value:'1'; help_text:'';),
    (name:'CratesPerPlayer'; default_value:'No'; help_text:'';),
    (name:'DevastatorExplodeDelay'; default_value:'100'; help_text:'';),
    (name:'IgnoreDistance'; default_value:'2'; help_text:'';),
    (name:'CrateCash'; default_value:'1000'; help_text:'';),
    (name:'ShowWarnings'; default_value:'Yes'; help_text:'Debug feature for game crashes';),
    (name:'DeathHandAccuracy'; default_value:'3'; help_text:'';),
    (name:'InfiniteSpice'; default_value:'No'; help_text:'';)
  );

implementation

end.
