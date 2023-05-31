# Urban-fjord
R scripts for the Urban Fjord and Milfersk projects  

## NOTE:  
There are two other related repos/projects: 
- This repo was started for Urban Fjord but at some point I continued in Milkys, script 992  
- A repo called Milfersk was started once but not much was added (?)  

## Data flow   

1. Read excel data from researchers    
- combining NIVA data (should be in Nivabase, but is sometimes stuck in Labware)   
- custom code per file / project  

2. Read export files from Vannmiljø   
- checking what has been delivered already    

3a. Read samples from Labware LIMS   
- function get_labware_samples    
- for comparing samples in Labware with samples in report, and (later) with othe dat 

3b. Read data from Nivabase   
- functions  
    - get_stations_from_project_id  
    - get_specimens_from_stationdata   
    - get_biota_chemistry  
    - get_water_chemistry_from_stations  
    - get_sediment_chemistry_from_stations  
    
4. Compare data     
    - what has been delivered to VM? (1 vs 2)  
    - what is not delivered, but is in Nivabase, ready to deliver to VM? (3b - 2)    
    - which parameters needs to be added to existing samples that are already in Nivabase? (1 - [3b - 2])    
    - what is not in Nivabase, but in Labware and metadata needs to be added in Labware? (3a - 3b; to be added, then repeat the above)    
    
* NOTE: water samples from later years (from 2021?) have water and particle phase. The particle phase data should be given by changing parameter name (parameter name in METHOD_DEFINITIONS) to [NAME] + "-P".  




