#WELI data preparation
#Process ODK excel output to fit requirements of dataprep.do stata syntax file

#08/04/2020

rm(list=ls())

library(openxlsx)
library(tidyverse)
library(data.table, warn.conflicts = FALSE)
library(haven)
#Adjust working directory
setwd("C:/Users/Ken/Dropbox/Research Grant Projects/The GIVE Project/Data Management/WELI Baseline Data/Shevac Project WELI do files")
#Adjust name of downloaded data
weliWB         <- loadWorkbook("weli_2019_SVAC_KE3_2019_12_06_10_30_29.xlsx")

####Data import####
##Sheet with main survey table
MainTable <- as_tibble(read.xlsx(weliWB, sheet = "WELI_ILRI2019_shevac4")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%    #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))   #keep last bit of var
  

##Sheet on hh information 
HhInfo <- as_tibble(read.xlsx(weliWB, sheet = "hh_information")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%    #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  
##Sheet on hh roster
HhRoster <- as_tibble(read.xlsx(weliWB, sheet = "household_roster")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%    #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var

##G1 Sheet on basic interview information
IntrvwInfo <- as_tibble(read.xlsx(weliWB, sheet = "module_G1")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%    #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))   #keep last bit of var
  
##G2a Sheet on input in productive decisions (crops)
InputProdDec <- as_tibble(read.xlsx(weliWB, sheet = "module_G2")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  select(!matches("labels")) %>%                                                        #drop "labels" vars
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE)) #keep last bit of var
  
##Sheet on livestock herd
LvstHerd <- as_tibble(read.xlsx(weliWB, sheet = "we_livestock")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  select(-matches("\\/(\\d){1,2}$")) %>% 
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on livestock species, important for hh
LvstSpecImpHh  <- as_tibble(read.xlsx(weliWB, sheet = "we_livestock_hh_important_lives")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on species, important for woman 
LvstSpecImpWe <- as_tibble(read.xlsx(weliWB, sheet = "we_livestock_we_important_lives")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

###G2b Sheet on livestock activities
LvstAct  <- as_tibble(read.xlsx(weliWB, sheet = "we_livestock_livestock_activiti")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  select(-matches("labels")) %>% 
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

###Sheet on ownership of assets (G3a)
OwnAssets <- as_tibble(read.xlsx(weliWB, sheet = "module_G3")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version")  %>%  #drop some variables
  select(!matches("labels|generated")) %>% 
  select(-matches("\\/\\d$")) %>% 
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

###Sheet on credit access (G3b)
CreditAccess <- as_tibble(read.xlsx(weliWB, sheet = "module_G3B")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  select(!matches("labels|generated")) %>% 
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on time allocation (G4)
Worktime <- as_tibble(read.xlsx(weliWB, sheet = "module_G4")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on group membership (G5)
GroupMem <- as_tibble(read.xlsx(weliWB, sheet = "module_G5")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on physical mobility (G6)
PhysicMobility <- as_tibble(read.xlsx(weliWB, sheet = "module_G6")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  select(!matches("(\\/\\d{1,2}$)|(_ot)")) %>% 
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on intra-household relationships (G7)
RespectHhMem <- as_tibble(read.xlsx(weliWB, sheet = "module_G7")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), -"_version") %>%   #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on autonomy in decision making (G8a)
Autonomy <- as_tibble(read.xlsx(weliWB, sheet = "module_G8")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), 
         -contains("labels"), -"_version") %>%   #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  

##Sheet on self-efficacy (G8b), domestic violence (G9)
SelfEff_DomViol <- as_tibble(read.xlsx(weliWB, sheet = "module_G8B_C_9")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), 
         -contains("labels"), -"_version") %>%   #drop some variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var
  
##Sheet on livestock vaccination (G10)
Vaccination <- as_tibble(read.xlsx(weliWB, sheet = "module_G10")) %>% 
  select(-"_id", -"_uuid", -"_submission_time", -"_tags", -contains("note"), 
         -contains("labels"), -"_version") %>%   #drop some variables
  select(-matches("\\/(\\d){1,2}$")) %>%         #drop multi-response variables
  setnames(names(.) %>% sub(pattern = ".*/", replacement = "", x = ., perl = TRUE))  #keep last bit of var


####Process data

###G1####
#Select ID variables for stata do file
Tbl1_ID <- MainTable %>% 
  select(`_index`, instanceName) %>% 
  left_join(HhInfo %>% select(`_parent_index`, g1_01), by = c("_index" = "_parent_index")) %>% 
  left_join(IntrvwInfo %>% select(`_parent_index`, g1_02, g1_03, g1_04),
            by = c("_index" = "_parent_index")) %>% 
  mutate(
    g1_02 = as.numeric(substr(g1_02, 8,9)),                      #keep only number of member ID
    g1_03 = as.numeric(g1_03),
    g1_04 = as.numeric(ifelse(g1_03 == 1 & is.na(g1_04), 1, g1_04) ) )       #husbands were not asked for hh type

###Respondent characteristics
HhRoster1 <- HhRoster %>%
  select(`_index`, `_parent_index`, `_parent_table_name`, 
         matches("^hhm_"), -matches("other$")) %>% 
  gather(var, val, -`_index`, -`_parent_index`, -`_parent_table_name`) %>% 
  mutate(
    var2 = sub("\\d{1,2}$", "", var, perl = TRUE),
    mem_index = as.numeric(sub("hhm_[a-z]*", "", var, perl = TRUE))) %>% 
  select(`_index`, `_parent_index`, `_parent_table_name`, var2, mem_index, val) %>% 
  spread(var2, value = val) %>% 
  rename(hhm_yob = hhm_age) %>% 
  mutate(hhm_age = 2020 - as.numeric(hhm_yob))

Respondent <- HhRoster1 %>% 
  filter(hhm_relationship == 1)

###G2a_crop####
#select only variables of interest
InputProdDec1 <- InputProdDec %>%
  select_at(vars(starts_with('_index'), starts_with('_parent'), starts_with('g2_01'), 
                 starts_with('g2_02'), starts_with('g2_03'), starts_with('g2_04'),
                 starts_with('g2_06'), starts_with('g2_07'),
                 -contains('calc'), -contains('note')))  %>% #drop vars with 'calc' and 'note'
  mutate_at(vars(starts_with('g2_0')), as.numeric)  %>% 
  setnames(names(.) %>% 
             gsub("^g2_0(\\d){1}", "g2_0\\1_", ., perl = TRUE) %>%  #insert "_"
             gsub("^g2_06", "g2_XX", ., perl = TRUE) %>%            #swap g2_06 and g2_07 to fit proWEIA
             gsub("^g2_07", "g2_06",  ., perl = TRUE) %>%
             gsub("^g2_XX", "g2_07",  ., perl = TRUE)        ) 
   

##replace dummy value in activity variables with index value
#function to multiply last part of variable name with variable value (1,NA) to create index value
varname1 <- function(x) (substitute(x) %>% as.character() %>% substr(.,8,9) %>% as.numeric() %>% 
                          "*" (as.numeric(x))  %>%  as.character() )
#apply function to relevant variables
InputProdDec2 <- InputProdDec1 %>%
  mutate_at(vars(contains("g2_02")), varname1 )

##compress activity variables into 3 variables per activity
#List of activities
VarList1 <- InputProdDec2 %>% 
  select(contains("g2_02")) %>% names() %>% substr(1,7) %>% unique()
#Pre-populate new table with non-activity variables
InputProdDec3 <- InputProdDec2 %>%
  select(-starts_with("g2_02") )
#number of non-activity variables
n <- length(names(InputProdDec3))

#unite activity variables and attach to new table
for (i in 1:length(VarList1)) {
  InputProdDec3[[i+n]] <- InputProdDec2 %>%
    unite(!!VarList1[[i]], starts_with(!!VarList1[[i]]), na.rm = TRUE, remove = TRUE) %>% 
    select(!!VarList1[[i]])  }
#function to remove "_0" from zero values; expect that they actually represent NA
funzero <- function(x) gsub("^(0_)+|(_0)+", "", x, perl = TRUE)
#reformat into one data-frame from list of data-frames
InputProdDec3 <- InputProdDec3 %>%
  do.call(data.frame,.) %>% 
  rename( `_index` = X_index,
          `_parent_index` = X_parent_index,
          `_parent_table_name` = X_parent_table_name ) %>% 
  mutate_at(vars(starts_with("g2_02")), funzero) #and apply removal function

#separate activity variables into 3 activity-person index variables
for ( i in 1:length(VarList1)) {
  InputProdDec3 <- InputProdDec3 %>%
    separate(VarList1[i], 
             c(paste0("g2_02_id1_",substr(VarList1[i],7,7)), 
               paste0("g2_02_id2_",substr(VarList1[i],7,7)),
               paste0("g2_02_id3_",substr(VarList1[i],7,7)) ), 
             sep = "_", remove = TRUE, fill = "right") %>% 
    mutate_at(vars(matches("^g2_02_id")), as.numeric)   }
Tbl2a_ActCrop <- InputProdDec3

####G2_b####
##Livestock activities         
##Variable renaming

##Livestock herd - to link with main table ID
LvstHerd1 <- LvstHerd %>%
  select(starts_with("_index"), starts_with("_parent"))

#Hh important species
LvstSpecImpHh1 <- LvstSpecImpHh %>% 
  mutate(importance_hh  = 1) %>%               #importance identifier
  rename(implvstockname = hhimplvstockname,    #standardise species name
         implvstockid   = hhimplvstock)        #standardise species id

#Woman important species
LvstSpecImpWe1 <- LvstSpecImpWe %>% 
  mutate(importance_we  = 1) %>%              #importance identifier
  rename(implvstockname = womanimplvstockname,  #standardise species name
         implvstockid   = womanimplvstock)      #standardise species id


#rename participation variables with activity descriptor - as used in stata code
LvstAct1 <-   LvstAct %>% 
  rename(
    g2_12_a = part_feeding,  
    g2_12_b = part_watering,  
    g2_12_c = part_grazing,
    g2_12_d = part_ahealth,
    g2_12_e = part_pdisease,
    g2_12_f = part_cmeasure,
    g2_12_g = part_milking,
    g2_12_h = part_smilk,
    g2_12_i = part_cleaning,
    g2_12_j = part_slghter,
    g2_12_k = part_prepfd,
    g2_12_l = part_breedg, 
    g2_12_m = part_AI, 
    g2_12_n = part_sire,
    g2_12_o = part_hmconsump, 
    g2_12_p = part_livmktg,
    g2_12_q = part_breedrear, 
    g2_12_r = part_workload,
    g2_12_s = part_lcollateral,
    g2_12_t = part_dungdraft, 
    g2_12_u = part_vRVF,  
    g2_12_v = part_vCCPP,
    g2_12_w = part_vPPR,
    g2_12_x = part_vNCD)

#Attach importance indicator to activity data
LvstAct1 <- LvstAct1 %>% 
  left_join(LvstSpecImpHh1 %>% select(implvstockid, `_parent_index`, importance_hh), 
            by = c("_parent_index", "import_livestock" = "implvstockid") ) %>% 
  left_join(LvstSpecImpWe1 %>% select(implvstockid, `_parent_index`, importance_we), 
            by = c("_parent_index", "import_livestock" = "implvstockid") ) %>% 
  mutate(
    importance_hh   = case_when(        #clean identifier variable for hh important species
      is.na(importance_hh)                           ~ 0,  
      TRUE                                           ~ importance_hh),
    importance_we   = case_when(        #clean identifier variable for woman important species
      is.na(importance_we)                           ~ 0,
      TRUE                                           ~ importance_we),
    importance_trgt = case_when(                           #create identifier variable for target species
      import_livestock == 5 | import_livestock == 7  ~ 1,  #should be checked for each project    
      TRUE                                           ~ 0  )  ) %>%   # what the targe species is
    #select only variables of interest to livestock module and format appropriately
  select_at(vars(starts_with('_index'), starts_with('_parent'), 
                 starts_with('import'), 
                 starts_with('g2_12_'), starts_with('g2_14'), starts_with('g2_15'), 
                 starts_with('g2_16'),
                 -contains('calc'), -contains('note'))) %>% #drop variables with 'calc' & 'note'
  mutate_at(vars(starts_with('g2_12'), starts_with('g2_15'), starts_with('g2_16')), as.numeric) %>% 
  mutate_at(vars(starts_with('g2_14')), na_if, 0) %>%
  mutate_at(vars(starts_with('g2_14')), as.character) %>% 
  setnames(names(.) %>% #add "_" to variable names g2_14, g2_15 and gn_16
       sub(".*g2_1(4|5|6)", replacement = "g2_1\\1_", ., perl = TRUE) ) 

##replace dummy value in activity variables (g2_14) with index value
#see G2a for varname1 function
LvstAct2 <- LvstAct1 %>%
  mutate_at(vars(contains("g2_14")), varname1)

##compress activity variables into 3 variables per activity
#List of activities
VarList2 <- LvstAct2 %>% 
  select(contains("g2_14")) %>% names() %>% substr(1,7) %>% unique()
#Pre-populate new table with non-activity variables
LvstAct3 <- LvstAct2 %>%
  select(-starts_with("g2_14") ) # %>% 
  #select(!starts_with("g2_14") )
#number of non-activity variables
n <- length(names(LvstAct3))
#unite activity variables and attach to new table
for (i in 1:length(VarList2)) {
  LvstAct3[[i+n]] <- LvstAct2 %>%
    unite(!!VarList2[[i]], starts_with(!!VarList2[[i]]), na.rm = TRUE, remove = TRUE) %>% 
    select(!!VarList2[[i]])  }
#reformat into one data-frame from list of data-frames
LvstAct3 <- LvstAct3 %>%
  do.call(data.frame,.) %>% 
  rename(
    `_index` = X_index,
    `_parent_index` = X_parent_index, 
    `_parent_table_name` = X_parent_table_name )

#separate activity variables into 3 activity-person index variables
for (i in 1:length(VarList2)) {
  LvstAct3 <- LvstAct3 %>%
    separate(VarList2[i], 
             c(paste0("g2_14_id1_",substr(VarList2[i],7,7)), 
               paste0("g2_14_id2_",substr(VarList2[i],7,7)),
               paste0("g2_14_id3_",substr(VarList2[i],7,7)) ), 
             remove = TRUE, sep = "_", fill = "right") %>% 
    mutate_at(vars(matches("^g2_14_id")), as.numeric)   }

##Filter empty records
#function to count NAs
funna <- function(x) (rowSums(is.na(x)))
#filter records where all g2_12 variables show NA
LvstAct3 <- LvstAct3 %>%
  mutate(
    g2_12x_NA = select(., starts_with("g2_12_")) %>% funna,
    g2_12x_col = select(., starts_with('g2_12_')) %>% ncol(.)   ) %>% 
  filter(g2_12x_NA != g2_12x_col) 

#What to do where we still have more than one livestock record?

##Select only species which is most important - for woman (_we) or for hh (_hh)
LvstAct4 <- LvstAct3 %>% 
  filter(importance_we == 1) %>% 
  group_by(`_parent_index`) %>%      #select only first record per herd (hh) 
    mutate(sn = row_number() )%>%    #- this needs to be checked!!
  ungroup() %>%
  filter(sn == 1) %>%
#rename index variables to avoid confusion with higher-level index variables (LvstHerd)
  rename(
      `_index_LvstAct` = `_index`,
      `_parent_index_LvstAct` = `_parent_index`,
      `_parent_table_name_LvstAct` = `_parent_table_name`) %>%
#attach to LvstHerd table to get correct link to main table
  right_join(LvstHerd1, by = c("_parent_index_LvstAct" = "_index") )

Tbl2b_ActLvst <- LvstAct4

####G3a####
###Assets - productive capital
#select only variables of interest 
OwnAssets <- OwnAssets %>%
  select_at(vars(starts_with('_index'), starts_with('_parent'), starts_with('g3_01'), 
                 starts_with('g3_05'), starts_with('g3_06'), starts_with('g3_07'),
#drop variables not used for processing and with non-unique last part of variable name
                 -contains('calc'), -contains('note'), -contains('_total'),
                 -contains('_jointly'), -contains('_solely'), -contains('labels'))) %>% 
  mutate_at(vars(starts_with('g3_01'), starts_with('g3_05'), starts_with('g3_06'), starts_with('g3_07')), 
            as.numeric)

Tbl3a_Assets  <- setnames(OwnAssets,  
                 names(OwnAssets) %>%
                     sub(pattern = ".*g3_06", replacement = "g3_06_", x = ., perl = TRUE) %>%
                     sub(pattern = ".*g3_07", replacement = "g3_07_", x = ., perl = TRUE)) 
   

####G3b####
###access to and decisions on credit
#select only variables of interest to index
CreditAccess1 <- CreditAccess %>%
  select_at(vars(starts_with('_index'), starts_with('_parent'),
                 starts_with('g3_08'), starts_with('g3_09'), starts_with('g3_10'), 
                 starts_with('g3_12'),  starts_with('g3_13'), starts_with('g3_14'), 
 #drop variables not used for processing and with non-unique last part of variable name
                 -contains('calc'), -contains('note'), -contains('_total'), 
                 -contains('_jointly'), -contains('_solely'), -contains('labels'))) %>% 
  mutate_at(vars(starts_with('g3_08'), starts_with('g3_09'), starts_with('g3_10'), 
                 starts_with('g3_12'), starts_with('g3_13'), starts_with('g3_14')), as.numeric) 
  
#change variable names because WELI has introduced new question g3_11
CreditAccess1 <- setnames(CreditAccess1, 
    names(CreditAccess1) %>% 
      sub(".*g3_12", "g3_11", ., perl = TRUE) %>% 
      sub(".*g3_13", "g3_12", ., perl = TRUE) %>%
      sub(".*g3_14", "g3_13", ., perl = TRUE)  ) %>%
  setnames(names(.) %>% #add "_" to variable names g3_10, g3_11 and g3_12
      sub("g3_1(0|1|2)", replacement = "g3_1\\1_", ., perl = TRUE) ) 
  

##replace dummy value with index value in all 3 credit decision variables groups 
#(borrow, use, repay)
#apply varname function to relevant variables
CreditAccess2 <- CreditAccess1 %>%
  mutate_at(vars(contains("g3_10")), varname1 ) %>% 
  mutate_at(vars(contains("g3_11")), varname1 ) %>% 
  mutate_at(vars(contains("g3_12")), varname1 )

##g3_10 - borrowing
#compress activity variables into 3 variables per activity
#List of activities
VarList3 <- CreditAccess2 %>% 
  select(contains("g3_10")) %>% names() %>% substr(1,7) %>% unique()

#Pre-populate new table only with index
CreditAccess3_10 <- CreditAccess2 %>%
  select('_index') 

#unite activity variables and attach to new table
for (i in 1:length(VarList3)) {
  CreditAccess3_10[[i+1]] <- CreditAccess2 %>%
    unite(!!VarList3[[i]], starts_with(!!VarList3[[i]]), na.rm = TRUE, remove = TRUE) %>% 
    select(!!VarList3[[i]])  }
#reformat into one data-frame from list of data-frames
CreditAccess3_10 <- CreditAccess3_10 %>%
  do.call(data.frame,.) %>% 
  rename(
    `_index` = X_index) %>% 
  mutate_at(vars(starts_with('g3_10')), funzero) #apply function to delete 0 values and '_'

#separate activity variables into 3 activity-person index variables
for ( i in 1:length(VarList3)) {
  CreditAccess3_10 <- CreditAccess3_10 %>%
    separate(VarList3[i], 
             c(paste0("g3_10_id1_",substr(VarList3[i],7,7)), 
               paste0("g3_10_id2_",substr(VarList3[i],7,7)),
               paste0("g3_10_id3_",substr(VarList3[i],7,7)) ), 
             remove = TRUE, sep = "_", fill = "right")  %>% 
    mutate_at(vars(matches("^g3_10_id")), as.numeric)  }

##g3_11 - use
#compress activity variables into 3 variables per activity
#List of activities
VarList4 <- CreditAccess2 %>% 
  select(contains("g3_11")) %>% names() %>% substr(1,7) %>% unique()

#Pre-populate new table only with index
CreditAccess3_11 <- CreditAccess2 %>%
  select('_index') 

#unite activity variables and attach to new table
for (i in 1:length(VarList4)) {
  CreditAccess3_11[[i+1]] <- CreditAccess2 %>%
    unite(!!VarList4[[i]], starts_with(!!VarList4[[i]]), na.rm = TRUE, remove = TRUE) %>% 
    select(!!VarList4[[i]])  }
#reformat into one data-frame from list of data-frames
CreditAccess3_11 <- CreditAccess3_11 %>%
  do.call(data.frame,.) %>% 
  rename(
    `_index` = X_index) %>% 
  mutate_at(vars(starts_with('g3_11')), funzero) #apply function to delete 0 values and '_'

#separate activity variables into 3 activity-person index variables
for ( i in 1:length(VarList4)) {
  CreditAccess3_11 <- CreditAccess3_11 %>%
    separate(VarList4[i], 
             c(paste0("g3_11_id1_",substr(VarList4[i],7,7)), 
               paste0("g3_11_id2_",substr(VarList4[i],7,7)),
               paste0("g3_11_id3_",substr(VarList4[i],7,7)) ), 
             remove = TRUE, sep = "_", fill = "right")  %>% 
    mutate_at(vars(matches("^g3_11_id")), as.numeric)  }

##g3_12 - repayment
#compress activity variables into 3 variables per activity
#List of activities
VarList5 <- CreditAccess2 %>% 
  select(contains("g3_12")) %>% names() %>% substr(1,7) %>% unique()

#Pre-populate new table only with index
CreditAccess3_12 <- CreditAccess2 %>%
  select('_index') 

#unite activity variables and attach to new table
for (i in 1:length(VarList5)) {
  CreditAccess3_12[[i+1]] <- CreditAccess2 %>%
    unite(!!VarList5[[i]], starts_with(!!VarList5[[i]]), na.rm = TRUE, remove = TRUE) %>% 
    select(!!VarList5[[i]])  }
#reformat into one data-frame from list of data-frames
CreditAccess3_12 <- CreditAccess3_12 %>%
  do.call(data.frame,.) %>% 
  rename(
    `_index` = X_index) %>% 
  mutate_at(vars(starts_with('g3_12')), funzero) #apply function to delete 0 values and '_'

#separate activity variables into 3 activity-person index variables
for ( i in 1:length(VarList5)) {
  CreditAccess3_12 <- CreditAccess3_12 %>%
    separate(VarList5[i], 
             c(paste0("g3_12_id1_",substr(VarList5[i],7,7)), 
               paste0("g3_12_id2_",substr(VarList5[i],7,7)),
               paste0("g3_12_id3_",substr(VarList5[i],7,7)) ), 
             remove = TRUE, sep = "_", fill = "right")  %>% 
    mutate_at(vars(matches("^g3_12_id")), as.numeric)  }

##Put tables together again
Tbl3b_Credit <- CreditAccess2 %>% 
  select(-contains('g3_10'), -contains('g3_11'), -contains('g3_12')) %>% 
  left_join(CreditAccess3_10, by = "_index") %>% 
  left_join(CreditAccess3_11, by = "_index") %>% 
  left_join(CreditAccess3_12, by = "_index")


####G4####
####time allocation (G4)
#select relevant variables
Worktime2 <- Worktime %>% 
  select(`_index`, starts_with('_parent'), 
         matches("^u\\d{1,2}$"),            #activity codes
         matches("^ut\\d{1,2}$"),           #end-time
         matches("^ucc\\d{1,2}$") )         #child-care indicator

#end-time         
Worktime3a <- Worktime2 %>% 
  select(`_index`, starts_with("ut")) %>% 
  gather(var, val, -`_index`)  %>%
  mutate(
    val    = as.numeric(val),
    period = as.numeric(substr(var,3,4)) ) %>%  #calculate period indicator
  rename(time1 = val)

#child-care
Worktime3b <- Worktime2 %>% 
  select(`_index`, starts_with("ucc")) %>% 
  gather(var, val, -`_index`)  %>%
  mutate(
    val    = as.numeric(val),
    period = as.numeric(substr(var,4,5)) ) %>%  #calculate period indicator
  rename(chldcr = val)

#create table with  time period shifted forward by one unit
Worktime4 <- Worktime3a %>%         
  mutate(period2 = period + 1) %>% 
  rename(time2 = time1)

Worktime5 <- Worktime3a %>%  
#attach start time variable
  left_join(Worktime4 %>% select(-var, -period), by = c("_index", "period" = "period2")) %>% 
#attach child care variable   
  left_join(Worktime3b %>% select(-var), by = c("_index", "period")) %>% 
         #calculate activity duration in min
  mutate(dur_min = 15*ifelse(!is.na(time2), time1 - time2, time1), 
         var = gsub("^ut","up", var),                          #change name value in var (-> "period")
         cc_dur = dur_min * chldcr) %>%                   #calculate child care duration
  select(`_index`, period, var, dur_min, cc_dur) %>% 
  arrange(`_index`, period)

#create child care time variable
#including sleeping & resting (A) and child care (R)
Worktime6 <- Worktime5 %>% 
  group_by(`_index`) %>% 
  summarise(
   time_childcare = sum(cc_dur, na.rm = TRUE) ) %>% 
  ungroup()

##activity names
#create activity code table to fit questionnaire letters to builder code values
actval <- as.numeric(c(1:24))
actname <- paste0("minutes_", LETTERS[1:24])
acttext <- c("Sleeping and resting", "Eating and drinking", "Personal care", "School (also homework) ", 
             "Work as employed", "Own business work ", "Staple grain farming", 
             "Horticultural (gardens) or high value crop farming", 
             "Large livestock raising (e.g. cattle, buffaloes)", 
             "Small livestock raising (e.g. sheep, goats, pigs)", 
             "Poultry and other small animals raising
             (e.g. chickens, ducks, turkeys)", "Fishpond culture", 
             "Commuting (to/from work or school)", "Shopping/getting service (incl health services)", 
             "Weaving, sewing, textile care", "Cooking", "Domestic work (incl fetching wood and water)", 
             "Care for children/adults/elderly", "Travelling (not for work or school)", 
             "Watching TV/listening to radio/reading", "Exercising", "Social activities and hobbies", 
             "Religious activities", "Other, specify.")

actcode <- tibble(actval, actname, acttext) %>% 
#recode to fit number codes from questionnaire to proWEIA letter codes
  mutate(
    actname = case_when(
      actval == 19                ~ "minutes_T",
      actval == 20                ~ "minutes_V",
      TRUE                        ~ actname ))
  
#table with activity names for each period
Worktime7 <- Worktime2 %>% 
  select(`_index`, matches("^u\\d{1,2}$")) %>%       #activity code variable
  gather(var, val, -`_index`)  %>%
  mutate(period = as.numeric(substr(var,2,3)) ) %>%  #calculate period indicator
  #attach activity names (letters) - keep all letters
  right_join(actcode %>% mutate(actval = as.character(actval)), by = c("val" = "actval"))

#attach activity labels and child care time; reformat table
Tbl4_Time <- Worktime5 %>% 
  #attach activity names, ensuring all activities are maintained in the table
  right_join(Worktime7 %>% select(`_index`, period, actname), by = c("_index", "period")) %>% 
  select(`_index`, actname, dur_min) %>% 
  group_by(`_index`, actname) %>% 
  summarise(                              #sum various periods of the same activity
    dur_min = sum(dur_min, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(`_index`, actname, dur_min) %>% 
  filter(!is.na(actname)) %>%   #delete rows without activity name
  spread(key = actname, value = dur_min) %>%
  mutate(minutes_S = NaN) %>%    # time_s (caring for adults) not collected separately by WELI,
                                #but required for processing
  filter(!is.na(`_index`)) %>%  #delete rows without index (introduced through enforcing activities)
  left_join(Worktime6, by = "_index") %>%   #add child care time variable
  #add parent index to link with other tables
  left_join(Worktime2 %>% select('_index', starts_with('_parent')), by = "_index") %>% 
  mutate(            #sum activities to total work-time
    worktime_min = rowSums(
      cbind(minutes_E, minutes_F, minutes_G, minutes_H,
            minutes_I, minutes_J, minutes_K, minutes_L, minutes_M, minutes_N,
            minutes_O, minutes_P, minutes_Q, minutes_R, minutes_S), na.rm = TRUE))
 
####G5####
###Group membership
#select only variables of interest for processing
Tbl5_Group <- GroupMem %>%
  select_at(vars(starts_with('_index'), starts_with('_parent'), 
                 starts_with('g5_01'), starts_with('g5_02'),  #g5_02 is g5_03 (Weai) "active in group"
                 starts_with('g5_05'), -ends_with('_oth'))) %>% 
  mutate_at(vars(starts_with('g5_0')), as.numeric) %>% 
  setnames(names(.) %>%  gsub(".*g5", "g5", ., perl = TRUE) ) %>%    #clean variable names
  setnames(names(.) %>%  gsub("g5_02", "g5_03", ., perl = TRUE) ) %>%    #change name to weai
  setnames(names(.) %>%                                #add _ before last letter
          gsub("(\\d{1})(\\w{1})$","\\1_\\2", ., perl = TRUE )  )
   
####G6####
###Physical Mobility
#select only variables of interest to livestock module
Tbl6_PhysicMobility <- PhysicMobility %>%
  select_at(vars(starts_with('_index'), starts_with('_parent'), 
                 matches("g6_01[a-e,k]$") )) %>% 
  mutate_at(vars(starts_with('g6_01')), as.numeric) %>% 
  rename(
    g6_01 = g6_01a, #city
    g6_02 = g6_01b, #market
    g6_03 = g6_01c, #family
    g6_04 = g6_01d, #friend
    g6_05 = g6_01e, #health
    g6_06 = g6_01k) #public 

####G7####
###Sheet on intra-household relationships
#select only variables of interest to livestock module
Tbl7_IntraHhRel <- RespectHhMem %>%
  select('_index', starts_with('_parent'), 
         starts_with('g7_0'), -'intro7a', -'intro7b') %>%  #keep only relevant variables
  mutate_at(vars(starts_with('g7_0')), as.numeric) %>% 
  setnames(names(.) %>%                                #add _ before last letter
           gsub("(\\d{1})(\\w{1})$","\\1_\\2", ., perl = TRUE )  ) 

####G8a####
#autonomy in decision making
Tbl8a_Autonomy <- Autonomy %>% 
  select('_index', starts_with('_parent'), 
         starts_with('g8_0'), -matches("\\d_([A-z]){2,4}$")) %>%  #keep only relevant variables
  mutate_at(vars(starts_with('g8_0')), as.numeric) 

####G8b####
##Self efficacy
Tbl8b_SelfEff <- SelfEff_DomViol %>% 
  select('_index', starts_with('_parent'), 
         starts_with('g8_04')) %>%  #keep only relevant variables
  mutate_at(vars(starts_with('g8_04')), as.numeric) 

####G9####
###Domestic violence
Tbl9DomViol <- SelfEff_DomViol %>%
  select(starts_with('_index'), starts_with('_parent'), starts_with('g9_0') ) %>%  
  mutate_at(vars(starts_with('g9_0')), as.numeric) %>% 
  setnames(names(.) %>% 
             gsub("^g9_0(\\d){1}", "g9_0\\1_", ., perl = TRUE) ) #insert "_"

####G10####
###Livestock vaccination - not required for WELI calculation but of interest for LVIF
Tbl10Vacc <- Vaccination %>%
  select(starts_with('_index'), starts_with('_parent'), starts_with('g10_') )



####Merge####
##Join all the relevant files
WELI_data <- Tbl1_ID %>% 
  left_join(Respondent %>% select(`_parent_index`, hhm_gender, hhm_age),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl2a_ActCrop %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl2b_ActLvst %>% select(-`_index_LvstAct`, -`_parent_table_name_LvstAct`,
                                     -`_parent_table_name`, -g2_12x_NA, -g2_12x_col, -sn),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl3a_Assets %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl3b_Credit %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl4_Time %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl5_Group %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl6_PhysicMobility %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl7_IntraHhRel %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl8a_Autonomy %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl8b_SelfEff %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl9DomViol %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  left_join(Tbl10Vacc %>% select(-`_index`, -`_parent_table_name`),
            by = c("_index" = "_parent_index") ) %>% 
  #ensure all main ID variables are available
  filter(!is.na(g1_01) & !is.na(g1_02) & !is.na(g1_03) & !is.na(g1_04)) %>% 
  #ensure a livestock activity records are available (a most important livestock has been selected)
  filter(!is.na(import_livestock)) %>% 
  #ensure some work-time is recorded
  filter(worktime_min > 0)


###The SheVax test data has unique household IDs.
#However, only if data has more than 1 hh member from the same household
#is it possible to run gender parity index
#Import hh list with edited hh id (same hh id for same hh, but differen mem id)
hhid_corr <- read_csv("WELI_data_hhcorr.csv") %>% 
  mutate(
    hhid_new1 = case_when(
      !is.na(hhid_new)  ~ hhid_new,
      TRUE              ~ g1_01 ),
    mid_new1 = case_when(
      !is.na(mid_new)   ~  mid_new,
      TRUE              ~ g1_02 ) )
#Add to WELI data and replace existing HhID
WELI_data <- WELI_data %>% 
  left_join(hhid_corr %>% select(`_index`, hhid_new1, mid_new1), by = c("_index")) %>% 
  rename(g1_01_old = g1_01,
         g1_02_old = g1_02,
         g1_01     = hhid_new1,
         g1_02     = mid_new1) %>% 
  select(`_index`, instanceName, g1_01, g1_01_old, g1_02, g1_02_old, 5:531)
 
#Check whether unique hhid x mid
 check <- WELI_data %>% 
   count(g1_01, g1_02) %>% 
   arrange(desc(n), g1_01)

##Export
#csv file for easy viewing
write_excel_csv (WELI_data, "WELI_data_200717.csv", na = "")
#stata file for further processing
write_dta(WELI_data, "WELI_data_200717.dta", version = 13)

##The End##


