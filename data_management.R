# Data management 
# Use code book in annexe to see variable definition. 

# Package installation.
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(here)

# ------- IMPORT DATA ----

folder <- "/Users/alexandreunger/Documents/PROJECTS/ICM/ICM_tradi/RProject_ICM_tradi/data"
df_all <- as.data.frame(read_excel(here(folder,"rawdata_donotmodify/ICM_data.xlsx")))

# ------- OPERATION ON DATA -------
# Assign variable name using code book in excell 
code_book <- as.data.frame(read_excel(here(folder,"ICM_code_book_tabl.xlsx")))

code_book <- code_book %>% 
  select(Variable_dataset, status, name_given) # our code_book with new variable name

code_book$Variable_dataset == colnames(df_all)

# # remove new variable
# subset_code_book <- code_book[code_book$status!=1,]
df_all <- df_all[,code_book$Variable_dataset] 

# assign new name
colnames(df_all) <- code_book[1:ncol(df_all),"name_given"]

# Remove unused variables
df_all <- df_all[, -which(colnames(df_all) %in% 
                            names(df_all)[colSums(!is.na(df_all)) == 0])]

# ------- Readjusting levels of variables -------

df_all <- df_all %>% mutate(
  demo_center = case_when(
    demo_center == 1 ~ 0,
    TRUE ~ 1
  ),
  
  demo_center = factor(
    demo_center,
    levels = c(0, 1),
    labels = c("ICPS", "Lariboisiere")
  ),
  
  CV_risk_Smoking = case_when(
    # take only current smoker in the risk 
    CV_risk_Smoking == 0 | CV_risk_Smoking == 2 ~ 0,
    CV_risk_Smoking == 1 ~ 1,
    TRUE ~ NA_real_
  ),
  clini_cardiac_rythm = case_when(
    clini_cardiac_rythm == 0 | clini_cardiac_rythm == 2  ~ 0,
    clini_cardiac_rythm == 1 ~ 1,
    TRUE ~ NA_real_
  ),
  
  history_coronary_procedure = case_when(
    history_interv_CABG == 0 & history_interv_PCI == 0  ~ 0,
    history_interv_CABG == 1 | history_interv_PCI == 1  ~ 1,
    TRUE ~ NA_real_
  ),
  
  history_sCAD_alone = case_when(
    history_med_MI == 0 & history_coronary_procedure == 0  ~ 1,
    TRUE ~ 0
  )
  )

# NEW CATEGORY OF LGE (keep with this !)

df_all<- df_all %>%
  mutate( 
    # Indicate the presence of both ischemic and midwall CMR LGE
    CMR_LGE_presence_ischemic_and_midwall = case_when(
      CMR_LGE_ischemic_presence == 1 & CMR_LGE_midwall_presence == 1 ~ 1,
      TRUE ~ 0
    ),
    
    CMR_LGE_presence_ischemic_and_midwall = factor(
      CMR_LGE_presence_ischemic_and_midwall,
      levels = c(0, 1),
      labels = c("No", "Yes")
      ),

    # Indicate the type of LGE (ischemic, midwall or both)
    CMR_LGE_type = case_when(
      CMR_LGE_presence_ischemic_or_midwall == 0 ~ 0,
      CMR_LGE_midwall_presence == 1 & CMR_LGE_ischemic_presence == 0 ~ 1,
      CMR_LGE_midwall_presence == 0 & CMR_LGE_ischemic_presence == 1 ~ 2,
      CMR_LGE_midwall_presence == 1 & CMR_LGE_ischemic_presence == 1 ~ 3,
      TRUE ~ NA_real_
    ),
    
    CMR_LGE_type = factor(
      CMR_LGE_type,
      levels = c(0, 1, 2, 3),
      labels = c("Absence_of_LGE", "Exclusive_midwall_LGE_presence", 
                 "Exclusive_ischemic_LGE_presence", 
                 "ischemic_and_midwall_LGE_presence")
    ),
    
    # Categorize the extent of ischemic CMR LGE based on count
    CMR_LGE_ischemic_extent_categ = case_when(
      CMR_LGE_ischemic_extent_count == 0 ~ 0,
      CMR_LGE_ischemic_extent_count %in% c(1, 2) ~ 1,
      CMR_LGE_ischemic_extent_count %in% c(3, 4, 5) ~ 2,
      CMR_LGE_ischemic_extent_count >= 6 ~ 3,
      TRUE ~ NA_real_
    ),
    # Assign name in order of levels
    CMR_LGE_ischemic_extent_categ = factor(
      CMR_LGE_ischemic_extent_categ,
      levels = c(0, 1, 2, 3),
      labels = c("A_No_ischemic_LGE", "B_1_2_segments", 
                 "C_3_5_segments", "D_more6_segments")
    ),
    
    # Create an optimal categorization of ischemic CMR LGE extent based on count
    CMR_LGE_ischemic_extent_categ_optimal = case_when(
      CMR_LGE_ischemic_extent_count == 0 ~ 0,
      CMR_LGE_ischemic_extent_count %in% c(1, 2, 3) ~ 1,
      CMR_LGE_ischemic_extent_count %in% c(4, 5) ~ 2,
      CMR_LGE_ischemic_extent_count >= 6 ~ 3,
      TRUE ~ NA_real_
    ),
    
    CMR_LGE_ischemic_extent_categ_optimal = factor(
      CMR_LGE_ischemic_extent_categ_optimal,
      levels = c(0, 1, 2, 3),
      labels = c("No_ischemic_LGE ", "Low_extent_(opt)", 
                 "Moderate_extent_(opt)", "High_extent_(opt)")
    ),
    
    
    # Categorize the extent of midwall CMR LGE based on count
    CMR_LGE_midwall_extent_categ = case_when(
      CMR_LGE_midwall_extent_count == 0 ~ 0,
      CMR_LGE_midwall_extent_count == 1 ~ 1,
      CMR_LGE_midwall_extent_count >= 2 ~ 2,
      TRUE ~ NA_real_
    ),
    
    CMR_LGE_midwall_extent_categ = factor(
      CMR_LGE_midwall_extent_categ,
      levels = c(0, 1, 2),
      labels = c("None", "Low_=1", "High_>1")
    ),

    CMR_LGE_ischemic_location_6 = case_when(
      CMR_LGE_ischemic_anterior == 0 & CMR_LGE_ischemic_septal == 0 &
        CMR_LGE_ischemic_inferior == 0 & CMR_LGE_ischemic_lateral == 0 &
        CMR_LGE_ischemic_Apical == 0 ~ 0,
      CMR_LGE_ischemic_anterior == 0 & CMR_LGE_ischemic_septal == 0 &
        CMR_LGE_ischemic_inferior == 0 & CMR_LGE_ischemic_lateral == 0 &
        CMR_LGE_ischemic_Apical == 1 ~ 1,
      CMR_LGE_ischemic_anterior == 0 & CMR_LGE_ischemic_septal == 0 &
        CMR_LGE_ischemic_inferior == 1 & CMR_LGE_ischemic_lateral == 0 &
        CMR_LGE_ischemic_Apical == 0 ~ 2,
      CMR_LGE_ischemic_anterior == 0 & CMR_LGE_ischemic_septal == 0 &
        CMR_LGE_ischemic_inferior == 0 & CMR_LGE_ischemic_lateral == 1 &
        CMR_LGE_ischemic_Apical == 0 ~ 3,
      CMR_LGE_ischemic_anterior == 1 & CMR_LGE_ischemic_septal == 0 &
        CMR_LGE_ischemic_inferior == 0 & CMR_LGE_ischemic_lateral == 0 &
        CMR_LGE_ischemic_Apical == 0 ~ 4,
      CMR_LGE_ischemic_anterior == 0 & CMR_LGE_ischemic_septal == 1 &
        CMR_LGE_ischemic_inferior == 0 & CMR_LGE_ischemic_lateral == 0 &
        CMR_LGE_ischemic_Apical == 0 ~ 5,
      TRUE ~ 6
    ),
    
    CMR_LGE_ischemic_location_6 = factor(
      CMR_LGE_ischemic_location_6,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c("No_ischemic_LGE", "Apical", "Inferior", "Lateral", "Anterior", 
                 "Septal", "Several_localization")
    ),
    
    
    CMR_LGE_ischemic_location_4 = case_when(
      CMR_LGE_ischemic_presence == 0 ~ 0,
      
      CMR_LGE_ischemic_presence == 1 & 
        CMR_LGE_ischemic_anterior == 0 &
        CMR_LGE_ischemic_septal == 0 ~ 1,
      
      CMR_LGE_ischemic_anterior == 1 &
        CMR_LGE_ischemic_septal == 0 ~ 2,
      
      CMR_LGE_ischemic_septal == 1 ~ 3
    ),
    
    CMR_LGE_ischemic_location_4 = factor(
      CMR_LGE_ischemic_location_4,
      levels = c(0, 1, 2, 3),
      labels = c("A_No_ischemic_LGE", "B_Neither_anterior_nor_septal", "C_Anterior_without_septal", "D_Septal")
    ),
  
    
    #### MIDWALL
    CMR_LGE_midwall_location_6 = case_when(
      CMR_LGE_midwall_anterior == 0 & CMR_LGE_midwall_septal == 0 &
        CMR_LGE_midwall_inferior == 0 & CMR_LGE_midwall_lateral == 0 &
        CMR_LGE_midwall_apical == 0 ~ 0,
      CMR_LGE_midwall_anterior == 0 & CMR_LGE_midwall_septal == 0 &
        CMR_LGE_midwall_inferior == 0 & CMR_LGE_midwall_lateral == 0 &
        CMR_LGE_midwall_apical == 1 ~ 1,
      CMR_LGE_midwall_anterior == 0 & CMR_LGE_midwall_septal == 0 &
        CMR_LGE_midwall_inferior == 1 & CMR_LGE_midwall_lateral == 0 &
        CMR_LGE_midwall_apical == 0 ~ 2,
      CMR_LGE_midwall_anterior == 0 & CMR_LGE_midwall_septal == 0 &
        CMR_LGE_midwall_inferior == 0 & CMR_LGE_midwall_lateral == 1 &
        CMR_LGE_midwall_apical == 0 ~ 3,
      CMR_LGE_midwall_anterior == 1 & CMR_LGE_midwall_septal == 0 &
        CMR_LGE_midwall_inferior == 0 & CMR_LGE_midwall_lateral == 0 &
        CMR_LGE_midwall_apical == 0 ~ 4,
      CMR_LGE_midwall_anterior == 0 & CMR_LGE_midwall_septal == 1 &
        CMR_LGE_midwall_inferior == 0 & CMR_LGE_midwall_lateral == 0 &
        CMR_LGE_midwall_apical == 0 ~ 5,
      TRUE ~ 6
    ),
    
    CMR_LGE_midwall_location_6 = factor(
      CMR_LGE_midwall_location_6,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c("No_midwall_LGE", "Apical", "Inferior", "Lateral", "Anterior", 
                 "Septal", "Several_localization")
    ),
    
    CMR_LGE_midwall_location_4 = case_when(
      CMR_LGE_midwall_presence == 0 ~ 0,
      CMR_LGE_midwall_presence == 1 & 
        CMR_LGE_midwall_lateral == 0 &
        CMR_LGE_midwall_septal == 0 ~ 1,
      CMR_LGE_midwall_lateral == 1 &
        CMR_LGE_midwall_septal == 0 ~ 2,
      CMR_LGE_midwall_septal == 1 ~ 3,
      TRUE ~ 4
    ),
    
    CMR_LGE_midwall_location_4 = factor(
      CMR_LGE_midwall_location_4,
      levels = c(0, 1, 2, 3, 4),
      labels = c("No_midwall_LGE", "Midwall_LGE_not_at_risk", 
                 "Lateral_midwall_LGE", "Septal_Midwall_LGE", "Error")
    ),
    
    
    CMR_LGE_midwall_location_3 = case_when(
      CMR_LGE_midwall_presence == 0 ~ 0,
      CMR_LGE_midwall_presence == 1 & 
        CMR_LGE_midwall_lateral == 0 &
        CMR_LGE_midwall_septal == 0 ~ 1,
      CMR_LGE_midwall_presence == 1 & 
        (CMR_LGE_midwall_lateral == 1 |
        CMR_LGE_midwall_septal == 1) ~ 2,
      TRUE ~ 3
    ),
    
    CMR_LGE_midwall_location_3 = factor(
      CMR_LGE_midwall_location_3,
      levels = c(0, 1, 2, 3),
      labels = c("No_midwall_LGE", "Midwall_LGE_not_at_risk", 
                 "At_risk_midwall_LGE_(septal_and/or_lateral)", "Error")
    ),
    
    CMR_LGE_ischemic_presence = factor(
      CMR_LGE_ischemic_presence,
      levels = c(0, 1),
      labels = c("No_ischemic_LGE", "Presence_of_ischemic_LGE")
    ),
    
    CMR_LGE_ischemic_transmurality = factor(
      CMR_LGE_ischemic_transmurality,
      levels = c(0, 1, 2, 3),
      labels = c("A_No_ischemic_LGE", "B_Subendocardial<50%", "C_Subendocardial≥50%", "D_Transmural")
    ),

    CMR_LGE_midwall_presence = factor(
      CMR_LGE_midwall_presence,
      levels = c(0, 1),
      labels = c("A_No_midwall_LGE", "B_Presence_of_midwall_LGE")
    ),
    
    CMR_LGE_ischemic_multiple = factor(
      CMR_LGE_ischemic_multiple,
      levels = c(0, 1, 2),
      labels = c("No_ischemic_LGE", "Focal_LGE", "Multiple")
    ),
    #### SCORE - MODIFIED TO CORRECT 
    score = CMR_LGE_ischemic_extent_count * 1 +
      ifelse(CMR_LGE_midwall_presence == "B_Presence_of_midwall_LGE", 2, 0) +
      ifelse(CMR_LGE_ischemic_transmurality == "C_Subendocardial≥50%", 2, 0) +
      ifelse(CMR_LGE_ischemic_transmurality == "D_Transmural≥50%", 3, 0) +
      ifelse(CMR_LGE_ischemic_location_4 == "C_Anterior_without_septal", 5, 0) +
      ifelse(CMR_LGE_ischemic_location_4 == "D_Septal", 6, 0),
    
    score_categ = case_when(
      score < 8 ~ "1_low_risk",
      score > 10 ~ "3_high_risk",
      TRUE ~ "2_medium_risk"
    )
    
    )

# ---- REMOVING VALUES AND CONVERTING TO FACTORS

columns_to_convert <- c("demo_gender", 
                        "clini_cardiac_rythm", 
                        "CV_risk_obesity", 
                        "CV_risk_dyslipidemia", 
                        "CV_risk_diabete", 
                        "CV_risk_HTA",
                        "CV_risk_Smoking", 
                        "CV_risk_history_fam_CAD", 
                        "history_coronary_procedure",
                        "history_interv_CABG",
                        "history_med_MI", 
                        "history_interv_PCI", 
                        "med_CKD",
                        "history_stroke", 
                        "med_pacemaker",
                        "med_periph_atheroma", 
                        "history_hospit_HF",
                        "history_AFib", 
                        "clini_NYHA",
                        "CMR_RV_dysfunction", 
                        "CMR_LGE_presence_ischemic_or_midwall",
                        "outcome_revascularisation_90days",
                        "CMR_LGE_ischemic_Apical",
                        "CMR_LGE_ischemic_inferior",
                        "CMR_LGE_ischemic_lateral",
                        "CMR_LGE_ischemic_anterior",
                        "CMR_LGE_ischemic_septal",
                        "CMR_LGE_midwall_anterior",
                        "CMR_LGE_midwall_septal",
                        "CMR_LGE_midwall_inferior",
                        "CMR_LGE_midwall_lateral",
                        "CMR_LGE_midwall_apical"
                        ) 


df_all <- df_all %>% 
  mutate(
    across(all_of(columns_to_convert), ~ factor(.,levels = c(0, 1), labels = c("No", "Yes"))),
    outcome_MACE = as.numeric(outcome_MACE),
    outcome_FU_time_MACE = as.numeric(outcome_FU_time_MACE),
    outcome_MACE_CV_death = as.numeric(outcome_MACE_CV_death),
    outcome_MACE_NonFatal_MI = as.numeric(outcome_MACE_NonFatal_MI)
    ) 


### dealing with the MACE variables
df_all <- df_all %>%
  mutate(
    time.censored = case_when(
      (outcome_FU_time_MACE <= outcome_FU_time_death) & !is.na(outcome_FU_time_MACE) ~ outcome_FU_time_MACE,
      TRUE ~ outcome_FU_time_death
    )
  )

D <- df_all %>% mutate(dead_before_MACE = ifelse(outcome_FU_time_MACE <= outcome_FU_time_death, 1, 0)) %>% select(outcome_death, outcome_FU_time_death, outcome_MACE, outcome_FU_time_MACE, dead_before_MACE, time.censored)

table(D$dead_before_MACE) # 20 only will be censored because of dead before MACE. (20 non cardiac - all-cause mortality)
D%>%filter(dead_before_MACE ==0)



#### OUTPUT VARIABLE :df_all, df_LGE, df_MACE_all

#### df_all
save(df_all, file = here("data","df_all.RData"))
#### df_LGE
df_LGE <- df_all %>% filter(CMR_LGE_ischemic_presence == "Presence_of_ischemic_LGE")
save(df_LGE, file = here("data","df_LGE.RData"))
#### df_MACE_all
df_MACE <- df_all %>% filter(!is.na(outcome_MACE))
save(df_MACE, file = here("data","df_MACE.RData"))



# df_derivation<- df_LGE %>% filter(
#   demo_center == "ICPS") %>% 
#   droplevels() 
# 
# df_validation <- df_LGE %>% 
#   filter(
#     demo_center == "Lariboisiere") %>% 
#   droplevels()
# 

