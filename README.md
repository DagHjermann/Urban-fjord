# Urban-fjord
R scripts for the Urban Fjord and Milfersk projects  

## Project portal  
* 210135 - Miljøgifter i en urban fjord (2021-2026)   
* 210136 - Overvåking av miljøgifter i ferskvannsnæringsnett  
    - Data 2014-2020: folder 17197_MILFERSK > 17197_Milfersk VANNMILJØ
    - Data 2021-2025: other folders

## NOTE:  
There are two other related repos/projects: 
- This repo was started for Urban Fjord but at some point I continued in Milkys, script 992  
- A repo called Milfersk was started once but not much was added (?)  

## Data flow   

1. Read excel data from researchers   
- Script name format: 300_Excel_[program]_[year]     
- combining NIVA data (should be in Nivabase, but is sometimes stuck in Labware)   
- custom code per file / project  
- save as standardized files (csv + rds)  

2. Read export files from Vannmiljø   
- Script name format: 600_Check_vmexport_[program]_[exportdate]     
- for checking what has been delivered already  
- match stations + matrix with 

3a. Read samples from Labware LIMS   
- Script name format: 100_Labware_[program]_[year]     
- function get_labware_samples    
- for comparing samples in Labware with samples in report, and (later) with othe dat 

3b. Read data from Nivabase   
- Script name format: 200_Nivabase_[program]_[year]     
- functions  
    - get_stations_from_project_id  
    - get_specimens_from_stationdata   
    - get_biota_chemistry  
    - get_water_chemistry_from_stations  
    - get_sediment_chemistry_from_stations  

4. Check export files for Vannmiljø created in Aquamonitor  
- Script name format: 500_Check_QqMexport_[program]_year  
   
5. Compare data     
- (no scripts yet?)
    - what has been delivered to VM? (1 vs 2)  
    - what is not delivered, but is in Nivabase, ready to deliver to VM? (3b - 2)    
    - which parameters needs to be added to existing samples that are already in Nivabase? (1 - [3b - 2])    
    - what is not in Nivabase, but in Labware and metadata needs to be added in Labware? (3a - 3b; to be added, then repeat the above)    
    
* NOTE: water samples from later years (from 2021?) have water and particle phase. The particle phase data should be given by changing parameter name (parameter name in METHOD_DEFINITIONS) to [NAME] + "-P".  



## Vannmiljø reporting

* Vannprøve, partikkelfase  
    - are given in Labware with "(P)"" or "(partikler)"" in the Description field   
    - are given in Milkys as a separate parameter, by adding "-P" to the parameter name  
    - In Vannmiljø use sampling method as below:  
    "SamplingMethodID": "NS-ISO 5667-17:2008D",
    "Name": "Partikler: Filtrering volumbasert",
    "Description": "Prøvetaking - Del 17: Veiledning i prøvetaking av partikulært materiale i vann (ISO 5667-17:2008)",
    "SortOrder": null
    - Medium = PA
    
* Vannprøve, vannfase  
    - are given in Labware with "(W)"" or "(vann)"" in the Description field   
    - are given in Milkys without "-P" added to the parameter name  
    - In Vannmiljø use sampling method = UKJENT  
    - Medium = PA
    - Provetakmetode_id = UKJENT
    
* Vannprøve, ufiltrert  
    - Medium = VAR (Avløpsvann renset)
    - Medium = VF (Ferskvann)
    - Provetakmetode_id = NS-EN ISO 5667-6:2016	(Ferskvann (elver og bekker))

* Sediment, grab (Van Veen etc.)  
    - Medium = PA
    - Provetakmetode_id = NS-EN ISO 5667-19B
    
* Sediment, grab (Van Veen etc.)  
    - Medium = SS
    - Provetakmetode_id = NS-EN ISO 5667-19B

* Blue mussel
    - Medium = BA
    - Provetakmetode_id = NS 9434:2017-SE (not transplanted)
    - Provetakmetode_id = NS 9434:2017-UP (transplanted)  
    
    




