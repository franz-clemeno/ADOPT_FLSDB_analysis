############ Supplementary code for manuscript


# Load required packages
library("tidyverse")
library("readxl")
library("visdat")
library("mice")
library("ggmice")
library("ggpubr")
library("plyr")

################################################

############ Load relevant data for pre- and post-AI FLS performance

### Load 2023 Data - active sites

# These are the variables that are common in all three sites
common_names <- c("Age","1.03 Gender","1.04 Post Code",'1.05 CareRes','1.06 DateFLSContact',
                  '1.07 DateFLSAssess','1.08 AdmitHosp','1.09 Date Index Frac Diagnosed','1.10 TypeOfFracture',
                  '1.11 SiteFirstFrac','1.12 SiteSecFrac','1.13 SiteThirdFrac','2.01 Height','2.02 Weight',
                  '2.03 PrevFracHist','2.04 FamHipFracHist','2.05 Smoker','2.06 BoneSpareTherapy','3.01 DXA',
                  '3.02 DXANotOrdered','3.03 DateofDXA','3.04 TScore','3.05 RiskOfFracture','4.01 BoneTherapy',
                  '4.02 CalciumVitDRec','5.01 RiskAssesByFLS','5.02 TwoFalls','5.03 FearOfFalling','5.04 MedsIncRiskFall',
                  '5.05 PreFracMob','5.06 Vision','5.07 Toileting','5.08 AbnormalCardAssess','5.09 CognitiveImpairment',
                  '5.10 Referrals','6.01 FollowUp12_16','6.02 Dateof16WeekAss','6.05 BoneTherapy1216','6.06 StartStrenBalExe',
                  '7.01 FollowUp48_56','7.02 Dateof52WeekAss','7.05 BoneTherapy4856',
                  '7.06 NumReFracSinceIndex','site')


# Load patient-level 2023 data from hospital sites
# These will be used to measure pre-AI FLS performance
# Some variable names were different by site - variable names were standardised
brad_2023 <- read_excel("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/bradford flsdb 2023.xlsx") %>% 
  mutate(site = "Bradford") %>% 
  dplyr::rename(`Age` = `Age Frac diagnosed`,
                `1.10 TypeOfFracture` = `1.10 TypeOfFractue`) %>% 
  dplyr::select(any_of(common_names))

cardiff_2023 <- read_excel("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/Cardiff FLS DB 2023.xlsx") %>% 
  mutate(site = "Cardiff") %>% 
  dplyr::rename(`Age` = `Age at fracture`,
                `1.10 TypeOfFracture` = `1.10 TypeOfFractue`) %>% 
  dplyr::select(any_of(common_names))

soton_2023 <- read_excel("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/soton flsdb 2023.xlsx") %>% 
  mutate(site = "Southampton") %>% 
  dplyr::select(-c(FLS16WeekAss,StartCalVitDSupp,FLS52WeekAss:FLS52WeekAss,
                   ConfirmAdhereVitD,ReFracture:TypeOfFractueFUThird)) 
names(soton_2023) <- common_names
#Column 45 of the original is 7.05 -> it is all empty

#Using only Age at Fracture for calculations



# Variables that need to be converted into factors
factor_vars <- c("1.03 Gender","1.04 Post Code",'1.05 CareRes','1.08 AdmitHosp','1.09 Date Index Frac Diagnosed','1.10 TypeOfFracture',
                 '1.11 SiteFirstFrac','1.12 SiteSecFrac','1.13 SiteThirdFrac',
                 '2.03 PrevFracHist','2.04 FamHipFracHist','2.05 Smoker','2.06 BoneSpareTherapy','3.01 DXA',
                 '3.02 DXANotOrdered','3.05 RiskOfFracture','4.01 BoneTherapy',
                 '4.02 CalciumVitDRec','5.01 RiskAssesByFLS','5.02 TwoFalls','5.03 FearOfFalling','5.04 MedsIncRiskFall',
                 '5.05 PreFracMob','5.06 Vision','5.07 Toileting','5.08 AbnormalCardAssess','5.09 CognitiveImpairment',
                 '5.10 Referrals','6.01 FollowUp12_16','6.05 BoneTherapy1216','6.06 StartStrenBalExe',
                 '7.01 FollowUp48_56','7.05 BoneTherapy4856','7.06 NumReFracSinceIndex','site')

active_joined_2023 <- rbind(brad_2023,cardiff_2023,soton_2023) %>%
  mutate(Assessment_gap = as.numeric(difftime(`1.07 DateFLSAssess`,`1.09 Date Index Frac Diagnosed`,units = "days")),
         less_90_assess = ifelse(Assessment_gap <= 90,1,0),
         DXA_gap = as.numeric(difftime(`3.03 DateofDXA`,`1.09 Date Index Frac Diagnosed`,units = "days")),
         less_90_DXA = ifelse(DXA_gap <= 90,1,0),
         Follow_gap = as.numeric(difftime(`6.02 Dateof16WeekAss`,`1.09 Date Index Frac Diagnosed`,units = "weeks")),
         less_16_follow = ifelse(Follow_gap <= 16,1,0)) #3985 patients


# 3 variables have >80% missing data
# 2 variables have around 50% missing data

#Handling missing data
# 4.01 -> change to "Don't know"
# 5.01 -> change to "No"
# 6.01 -> change to "No"
# 6.05 -> change to "Don't know"
# 6.06 -> change to "No"
active_joined_2023_imputed <- active_joined_2023 %>% 
  replace_na(list(`4.01 BoneTherapy` = "Don't know",
                  `5.01 RiskAssesByFLS` = "No",
                  `6.01 FollowUp12_16` = "No",
                  `6.05 BoneTherapy1216` = "Don't know",
                  `6.06 StartStrenBalExe` = "No",
                  `7.05 BoneTherapy4856` = "Don't know")) %>% 
  mutate(across(any_of(factor_vars),  factor))

write.csv(active_joined_2023_imputed,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/active_sites_joined_2023.csv")
# Imputed data is exported and used to quantify pre-AI FLS performance

################################################

#Load 2024 data - active sites

common_names_2024 <- c("Age","1.03 Gender","1.04 Post Code",'1.05 CareRes','1.06 DateFLSContact',
                       '1.07 DateFLSAssess','1.08 AdmitHosp','1.09 Date Index Frac Diagnosed','1.10 TypeOfFracture',
                       '1.11 SiteFirstFrac','1.12 SiteSecFrac','1.13 SiteThirdFrac','2.01 Height','2.02 Weight',
                       '2.03 PrevFracHist','2.04 FamHipFracHist','2.05 Smoker','2.06 BoneSpareTherapy','3.01 DXA',
                       '3.02 DXANotOrdered','3.03 DateofDXA','3.04 TScore','3.05 RiskOfFracture','4.01 BoneTherapy',
                       '4.02 CalciumVitDRec','5.01 RiskAssesByFLS','5.02 TwoFalls','5.03 FearOfFalling','5.04 MedsIncRiskFall',
                       '5.05 PreFracMob','5.06 Vision','5.07 Toileting','5.08 AbnormalCardAssess','5.09 CognitiveImpairment',
                       '5.10 Referrals','6.01 FollowUp12_16','6.02 Dateof16WeekAss','6.05 BoneTherapy1216','6.06 StartStrenBalExe',
                       '7.01 FollowUp48_56','7.02 Dateof52WeekAss','7.05 BoneTherapy4856',
                       '7.06 NumReFracSinceIndex','site')

# Load patient-level 2023 data from hospital sites
# These will be used to measure pre-AI FLS performance
# Some variable names were different by site - variable names were standardised
brad_2024 <- read_excel("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/bradford flsdb 2024.xlsx") %>% 
  mutate(site = "Bradford") %>% 
  dplyr::rename(`Age` = `Age Frac Diagnosed`,
                `1.10 TypeOfFracture` = `1.10 TypeOfFractue`) %>% 
  dplyr::select(any_of(common_names_2024))

cardiff_2024 <- read_excel("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/Cardiff FLSDB 2024.xlsx") %>% 
  dplyr::select(-c(`...2`,`...3`,`...4`)) %>% 
  mutate(site = "Cardiff") %>% 
  dplyr::rename(`Age` = `Age at fracture`,
                `1.10 TypeOfFracture` = `1.10 TypeOfFractue`) %>% 
  dplyr::select(any_of(common_names_2024))

soton_2024 <- read_excel("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/soton flsdb 2024.xlsx") %>% 
  mutate(site = "Southampton") %>% 
  dplyr::rename(`1.10 TypeOfFracture` = `1.10 TypeOfFractue`) %>%
  dplyr::select(any_of(common_names_2024)) %>% 
  filter(!is.na(Age))

active_joined_2024 <- rbind(brad_2024,cardiff_2024,soton_2024) %>%
  mutate(`7.02 Dateof52WeekAss` = as.POSIXct(`7.02 Dateof52WeekAss`),
         Assessment_gap = as.numeric(difftime(`1.07 DateFLSAssess`,`1.09 Date Index Frac Diagnosed`,units = "days")),
         less_90_assess = ifelse(Assessment_gap <= 90,1,0),
         DXA_gap = as.numeric(difftime(`3.03 DateofDXA`,`1.09 Date Index Frac Diagnosed`,units = "days")),
         less_90_DXA = ifelse(DXA_gap <= 90,1,0),
         Follow_gap = as.numeric(difftime(`6.02 Dateof16WeekAss`,`1.09 Date Index Frac Diagnosed`,units = "weeks")),
         less_16_follow = ifelse(Follow_gap <= 16,1,0)) 


# Similar imputation strategy used for 2024 data
active_joined_2024_imputed <- active_joined_2024 %>% 
  replace_na(list(`4.01 BoneTherapy` = "Don't know",
                  `5.01 RiskAssesByFLS` = "No",
                  `6.01 FollowUp12_16` = "No",
                  `6.05 BoneTherapy1216` = "Don't know",
                  `6.06 StartStrenBalExe` = "No",
                  `7.01 FollowUp48_56` = "Don't know")) %>% 
  mutate(across(any_of(factor_vars),  factor),
         `7.05 BoneTherapy4856` = factor(`7.05 BoneTherapy4856`))

write.csv(active_joined_2024_imputed,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/active_sites_joined_2024.csv")
# Imputed data is exported and used to quantify pre-AI FLS performance


# Estimated caseloads obtained for 2023 period from FLSDB:
# Bradford - 1500
# Cardiff - 2680
# Soton - 4160
# Use these values for the 2024 period as well - last observation carried forward
est_caseload_active <- c(1500,2680,4160)

# These values will be used for the KPI calculations

################################################

# Identify control sites
# Select sites that: 
# 1. are within 10% of KPI 3 (vertebral fracture identification) in 2023 
# 2. have 2024 data available

#Bradford - Imperial
#Soton - North Durham, Shrewsbury, Pennine, West Suffolk
#Cardiff - Cambridge, Chesterfield, Broomfield, Western H&SC

# KPI data for control sites 
control_2023 <- read_xlsx("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/Control KPIs.xlsx",1)
control_2024 <- read_xlsx("~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/Control KPIs.xlsx",2)

control_caseloads <- c(849,2797,1515,887,1005,2188,1628,1260,402)


################################################

############ Generate inputs for health economic models


### 2024 data from active sites

# Mean age of patients by fracture type and by sex

inputs_2024_df <- active_joined_2024_imputed %>% 
  mutate(
    # Non-spine and non-hip fractures are labelled as "Other fractures"
    `1.11 SiteFirstFrac` = revalue(`1.11 SiteFirstFrac`, 
                                   c("Spine"="Spine",
                                     "Hip"="Hip",
                                     "Humerus"="Other",
                                     "Other"="Other",
                                     "Pelvis"="Other",
                                     "Wrist"="Other")),
    # Treatments are re-categorised
    `4.01 BoneTherapy` = revalue(`4.01 BoneTherapy`,
                                 c("Alendronate" = "Oral BP",
                                   "Alfacalcidol" = "Other",
                                   "Denosumab" = "Denosumab",
                                   "Don't know" = "Don't know",
                                   "Ibandronate" = "Oral BP",
                                   "Inappropriate" = "Inappropriate",
                                   "Informed decline" = "Informed decline",
                                   "Referred for further clinical opinion" = "Referred",
                                   "Referred to GP to decide prescription" = "Referred",
                                   "Risedronate" = "Oral BP",
                                   "Romosozumab" = "Romosozumab",
                                   "Teriparatide" = "Teriparatide",
                                   "Zoledronate" = "IV BP")),
    # Treatments are re-categorised
    `6.05 BoneTherapy1216` = factor(revalue(`6.05 BoneTherapy1216`,
                                            c("Alendronate" = "Oral BP",
                                              "Denosumab" = "Denosumab",
                                              "Don't know" = "Don't know",
                                              "Ibandronate" = "Oral BP",
                                              "Informed decline" = "Informed decline",
                                              "No longer appropriate" = "No longer appropriate",
                                              "Not started" = "Not started",
                                              "Refer for clinical opinion" = "Refer for clinical opinion",
                                              "Risedronate" = "Oral BP",
                                              "Zoledronate" = "IV BP"))),
    # Treatments are re-categorised
    `7.05 BoneTherapy4856` = factor(revalue(`7.05 BoneTherapy4856`,
                                            c("Alendronate" = "Oral BP",
                                              "Denosumab" = "Denosumab",
                                              "Not started" = "Not started")))) %>% 
  dplyr::select(Age,`1.03 Gender`,`1.06 DateFLSContact`,`1.07 DateFLSAssess`,
                `1.09 Date Index Frac Diagnosed`,`1.11 SiteFirstFrac`,
                `3.01 DXA`, `3.03 DateofDXA`,
                `4.01 BoneTherapy`,`6.01 FollowUp12_16`,`6.02 Dateof16WeekAss`,
                `6.05 BoneTherapy1216`,`7.01 FollowUp48_56`,`7.02 Dateof52WeekAss`,
                `7.05 BoneTherapy4856`,DXA_gap,site) %>% 
  dplyr::mutate(`1.09 Date Index Frac Diagnosed` = as.Date(`1.09 Date Index Frac Diagnosed`, format = "%Y-%m-%d"),
                `3.03 DateofDXA` = as.Date(`3.03 DateofDXA`, format = "%Y-%m-%d"))
write.csv(inputs_2024_df,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/inputs_df.csv")


# Mean age by sex and fracture site
inputs_2024_df %>% 
  dplyr::group_by(`1.03 Gender`,`1.11 SiteFirstFrac`) %>% 
  summary(.)
dplyr::summarise(age = mean(Age,na.rm = T))

# Proportion of treatment recommendation by fracture site and by sex - 4.01
inputs_2024_df %>% 
  dplyr::group_by(`1.03 Gender`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`) %>% 
  dplyr::summarise(N = n()) %>% 
  View(.)

inputs_2024_df %>% 
  dplyr::count(`1.03 Gender`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`) %>% 
  dplyr::mutate(sum_treat = sum(n),.by = c(`1.03 Gender`,`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat) %>% 
  View()
write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/treatment_recom_sex_fracture.csv")

# Group all drugs
inputs_2024_df %>% 
  dplyr::mutate(
    # Using broader categories for recommendations
    Treat_Recom = revalue(`4.01 BoneTherapy`,
                          c("Alendronate" = "Named drug",
                            "Alfacalcidol" = "Named drug",
                            "Denosumab" = "Named drug",
                            "Don't know" = "Don't know",
                            "Ibandronate" = "Named drug",
                            "Inappropriate" = "Inappropriate",
                            "Informed decline" = "Informed decline",
                            "Referred for further clinical opinion" = "Referred",
                            "Referred to GP to decide prescription" = "Referred",
                            "Risedronate" = "Named drug",
                            "Romosozumab" = "Named drug",
                            "Teriparatide" = "Named drug",
                            "Zoledronate" = "Named drug",
                            "IV BP" = "Named drug",
                            "Oral BP" = "Named drug",
                            "Other" = "Named drug"))) %>% 
  dplyr::count(`1.03 Gender`,`1.11 SiteFirstFrac`,Treat_Recom) %>% 
  dplyr::mutate(sum_treat = sum(n),.by = c(`1.03 Gender`,`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat) %>% 
  write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/grouped_treatment_recom_sex_fracture.csv")

# Treatment commenced and adherence by fracture site and sex - 6.05
# Looking at the dates
# Separately, look at the adherence
inputs_2024_df %>% 
  filter(!(`6.05 BoneTherapy1216` %in% c("Don't know","No longer appropriate",
                                         "Informed decline","Not started",
                                         "Refer for clinical opinion"))) %>% 
  mutate(
    # Calculate gap between initial FLS assessment and 16-week follow-up
    time_gap = as.numeric(difftime(`6.02 Dateof16WeekAss`,`1.07 DateFLSAssess`,units = "days"))) %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`6.02 Dateof16WeekAss`,time_gap) %>% 
  group_by(`1.03 Gender`,`1.11 SiteFirstFrac`) %>% 
  dplyr::summarise(mean_gap = mean(time_gap, na.rm = T)) %>% 
  View(.)

# Patient monitoring at 16 (6.01) and 52 weeks (7.01) 
# by the treatment recommended or initiated for all FLS patients

# Proportion of patients that have started treatment by 16 weeks
inputs_2024_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`6.02 Dateof16WeekAss`) %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","No longer appropriate",
                                     "Informed decline","Not started",
                                     "Refer for clinical opinion"))) %>%
  dplyr::count(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,`6.05 BoneTherapy1216`) %>% 
  dplyr::mutate(sum_treat_16 = sum(n), .by = c(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`),
                perc = 100*n/sum_treat_16) %>% 
  View()

# Calculate weighted average across treatment arms by fracture site
inputs_2024_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`6.02 Dateof16WeekAss`) %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","No longer appropriate",
                                     "Informed decline","Not started",
                                     "Refer for clinical opinion"))) %>%
  dplyr::count(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,`6.05 BoneTherapy1216`) %>% 
  dplyr::mutate(sum_treat_16 = sum(n), .by = c(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`),
                perc = 100*n/sum_treat_16) %>% 
  filter(`4.01 BoneTherapy` == `6.05 BoneTherapy1216`) %>% 
  dplyr::mutate(weighted_sum = perc*sum_treat_16) %>% 
  View()

# 52 weeks monitoring is highly missing as the 2024 data is not yet complete
inputs_2024_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`7.01 FollowUp48_56`,`7.02 Dateof52WeekAss`,
                `7.05 BoneTherapy4856`) %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","No longer appropriate",
                                     "Informed decline","Not started",
                                     "Refer for clinical opinion"))) %>% 
  dplyr::count(`6.05 BoneTherapy1216`,`7.05 BoneTherapy4856`) %>%
  dplyr::mutate(sum_treat_52 = sum(n), .by = c(`6.05 BoneTherapy1216`),
                perc = 100*n/sum_treat_52) %>% 
  View()


# Monitoring patients at 16 weeks (6.01) and 52 weeks (7.01) 
# by the treatment recommended or initiated for all FLS patients

inputs_2024_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.01 FollowUp12_16`,`6.05 BoneTherapy1216`) %>% 
  dplyr::group_by(`4.01 BoneTherapy`,`6.01 FollowUp12_16`) %>% 
  dplyr::summarise(N = n()) %>% 
  write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/treatment_monitoring_16.csv")

inputs_2024_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `7.01 FollowUp48_56`) %>% 
  dplyr::group_by(`4.01 BoneTherapy`,`7.01 FollowUp48_56`) %>% 
  dplyr::summarise(N = n()) %>% 
  write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/treatment_monitoring_52.csv")


# DXA Scan rate and time gap

inputs_2024_df %>% 
  dplyr::count(`1.03 Gender`,`1.11 SiteFirstFrac`,`3.01 DXA`) %>% 
  dplyr::mutate(sum_treat = sum(n),.by = c(`1.03 Gender`,`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat) %>% View()
write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/DXA_3_01_2024.csv")

inputs_2024_df %>% filter((`3.03 DateofDXA` > `1.09 Date Index Frac Diagnosed`) & (difftime(`3.03 DateofDXA`,`1.09 Date Index Frac Diagnosed`,units = "days") <= 90)) %>% 
  dplyr::group_by(`1.03 Gender`,`1.11 SiteFirstFrac`) %>% 
  dplyr::summarise(n = n()) %>% 
  View()




################################################################
# Generate model inputs for 2023 data - same method as above chunk



# Mean age of patients by fracture type and by sex

inputs_2023_df <- active_joined_2023_imputed %>% 
  mutate(`1.11 SiteFirstFrac` = revalue(`1.11 SiteFirstFrac`,
                                        c("Spine"="Spine",
                                          "Hip"="Hip",
                                          "Non hip/non spine"="Other")),
         `4.01 BoneTherapy` = revalue(`4.01 BoneTherapy`,
                                      c("Alendronate" = "Oral BP",
                                        "Alendronate;Denosumab" = "Oral BP",
                                        "Alendronate;Ibandronate" = "Oral BP",
                                        "Alendronate;Risedronate" = "Oral BP",
                                        "Denosumab" = "Denosumab",
                                        "Don't know" = "Don't know",
                                        "Ibandronate" = "Oral BP",
                                        "Inappropriate" = "Inappropriate",
                                        "Informed decline" = "Informed decline",
                                        "Referred for further clinical opinion" = "Referred",
                                        "Referred to GP to decide prescription" = "Referred",
                                        "Risedronate" = "Oral BP",
                                        "Romosozumab" = "Romosozumab",
                                        "Teriparatide" = "Teriparatide",
                                        "Teriparatide;Denosumab" = "Teriparatide;Denosumab",
                                        "Zoledronate" = "IV BP")),
         `6.05 BoneTherapy1216` = factor(revalue(`6.05 BoneTherapy1216`,
                                                 c("Alendronate" = "Oral BP",
                                                   "Denosumab" = "Denosumab",
                                                   "Don't know" = "Don't know",
                                                   "Donít know" = "Don't know",
                                                   "Ibandronate" = "Oral BP",
                                                   "Informed decline" = "Informed decline",
                                                   "No longer appropriate" = "Inappropriate",
                                                   "Not started" = "Not started",
                                                   "Risedronate" = "Oral BP",
                                                   "Romosozumab" = "Romosozumab",
                                                   "Teriparatide" = "Teriparatide",
                                                   "Zoledronate" = "IV BP"))),
         `7.05 BoneTherapy4856` = factor(revalue(`7.05 BoneTherapy4856`,
                                                 c("Alendronate" = "Oral BP",
                                                   "Denosumab" = "Denosumab",
                                                   "Don't know" = "Don't know",
                                                   "Donít know" = "Don't know",
                                                   "Ibandronate" = "Oral BP",
                                                   "Informed decline" = "Informed decline",
                                                   "No longer appropriate" = "Inappropriate",
                                                   "Not started" = "Not started",
                                                   "Refer for clinical opinion" = "Referred",
                                                   "Risedronate" = "Oral BP",
                                                   "Romosozumab" = "Romosozumab",
                                                   "Zoledronate" = "IV BP"
                                                 )))) %>% 
  dplyr::select(Age,`1.03 Gender`,`1.06 DateFLSContact`,`1.07 DateFLSAssess`,
                `1.09 Date Index Frac Diagnosed`,`1.11 SiteFirstFrac`,
                `3.01 DXA`,`3.03 DateofDXA`,
                `4.01 BoneTherapy`,`6.01 FollowUp12_16`,`6.02 Dateof16WeekAss`,
                `6.05 BoneTherapy1216`,`7.01 FollowUp48_56`,`7.02 Dateof52WeekAss`,
                `7.05 BoneTherapy4856`,DXA_gap,site) %>% 
  dplyr::mutate(`1.09 Date Index Frac Diagnosed` = as.Date(`1.09 Date Index Frac Diagnosed`, format = "%Y-%m-%d"),
                `3.03 DateofDXA` = as.Date(`3.03 DateofDXA`, format = "%Y-%m-%d"))
write.csv(inputs_2023_df,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/inputs_df_2023.csv")


# Mean age by sex and fracture site
inputs_2023_df %>% 
  dplyr::group_by(`1.03 Gender`,`1.11 SiteFirstFrac`) %>% 
  dplyr::summarise(age = mean(Age,na.rm = T))
summary(.)

inputs_2023_df %>% 
  dplyr::group_by(`1.11 SiteFirstFrac`) %>% 
  dplyr::summarise(age = mean(Age,na.rm = T))

# Proportion of treatment recommendation by fracture site and by sex - 4.01
inputs_2023_df %>% 
  dplyr::group_by(`1.03 Gender`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`) %>% 
  dplyr::summarise(N = n()) %>% 
  View(.)

inputs_2023_df %>% 
  dplyr::count(`1.03 Gender`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`) %>% 
  dplyr::mutate(sum_treat = sum(n),.by = c(`1.03 Gender`,`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat) %>% View()
write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/treatment_recom_sex_fracture_2023.csv")

# Group all drugs
inputs_2023_df %>% 
  dplyr::mutate(Treat_Recom = revalue(`4.01 BoneTherapy`,
                                      c("Oral BP" = "Named drug",
                                        "Denosumab" = "Named drug",
                                        "Don't know" = "Don't know",
                                        "Inappropriate" = "Inappropriate",
                                        "Informed decline" = "Informed decline",
                                        "Referred" = "Referred",
                                        "Romosozumab" = "Named drug",
                                        "Systemic Oestrogens" = "Named drug",
                                        "Teriparatide" = "Named drug",
                                        "Teriparatide;Denosumab" = "Named drug",
                                        "IV BP" = "Named drug"))) %>% 
  dplyr::count(`1.03 Gender`,`1.11 SiteFirstFrac`,Treat_Recom) %>% 
  dplyr::mutate(sum_treat = sum(n),.by = c(`1.03 Gender`,`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat) %>% 
  write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/grouped_treatment_recom_sex_fracture_2023.csv")

# Treatment commenced and adherence by fracture site and sex - 6.05
# Looking at the dates
# Separately, look at the adherence
inputs_2023_df %>% 
  filter(!(`6.05 BoneTherapy1216` %in% c("Don't know","Inappropriate",
                                         "Informed decline","Not started",
                                         "Refer for clinical opinion"))) %>% 
  mutate(time_gap = as.numeric(difftime(`6.02 Dateof16WeekAss`,`1.07 DateFLSAssess`,units = "days"))) %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`6.02 Dateof16WeekAss`,time_gap) %>% 
  group_by(`1.03 Gender`,`1.11 SiteFirstFrac`) %>% 
  dplyr::summarise(mean_gap= mean(time_gap, na.rm = T)) %>% 
  View(.)

# Patient monitoring at 16 (6.01) and 52 weeks (7.01) 
# by the treatment recommended or initiated for all FLS patients

# Proportion of patients that have started treatment by 16 weeks
inputs_2023_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`6.02 Dateof16WeekAss`) %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","Inappropriate",
                                     "Informed decline","Not started",
                                     "Refer for clinical opinion"))) %>% 
  dplyr::count(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,`6.05 BoneTherapy1216`) %>%
  dplyr::mutate(sum_treat_16 = sum(n), .by = c(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`),
                perc = 100*n/sum_treat_16) %>% 
  dplyr::filter(as.character(`4.01 BoneTherapy`) == as.character(`6.05 BoneTherapy1216`)) %>% 
  View()


# 52 weeks monitoring is highly missing as the 2023 data is not yet complete
inputs_2023_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`7.01 FollowUp48_56`,`7.02 Dateof52WeekAss`,
                `7.05 BoneTherapy4856`) %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","Inappropriate",
                                     "Informed decline","Not started",
                                     "Refer for clinical opinion"))) %>% 
  dplyr::count(`4.01 BoneTherapy`,`7.05 BoneTherapy4856`) %>%
  dplyr::mutate(sum_treat_52 = sum(n), .by = c(`4.01 BoneTherapy`),
                perc = 100*n/sum_treat_52) %>% 
  dplyr::filter(as.character(`7.05 BoneTherapy4856`) == as.character(`4.01 BoneTherapy`)) %>%
  View()


# Monitoring patients at 16 weeks (6.01) and 52 weeks (7.01) 
# by the treatment recommended or initiated for all FLS patients

inputs_2023_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.01 FollowUp12_16`,`6.05 BoneTherapy1216`) %>% 
  dplyr::group_by(`4.01 BoneTherapy`,`6.01 FollowUp12_16`) %>% 
  dplyr::summarise(N = n()) %>% 
  group_by(`4.01 BoneTherapy`) %>% 
  dplyr::mutate(Total = sum(N),
                Percentage = 100*N/Total) %>% View()
write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/treatment_monitoring_16_2023.csv")

inputs_2023_df %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `7.01 FollowUp48_56`) %>% 
  dplyr::group_by(`4.01 BoneTherapy`,`7.01 FollowUp48_56`) %>% 
  dplyr::summarise(N = n()) %>% 
  group_by(`4.01 BoneTherapy`) %>% 
  dplyr::mutate(Total = sum(N),
                Percentage = 100*N/Total) %>% 
  write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/treatment_monitoring_52_2023.csv")


# DXA Scan rate and time gap

inputs_2023_df %>% 
  dplyr::count(`1.03 Gender`,`1.11 SiteFirstFrac`,`3.01 DXA`) %>% 
  dplyr::mutate(sum_treat = sum(n),.by = c(`1.03 Gender`,`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat) %>%  View()
write.csv(.,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/DXA_3_01_2023.csv")

inputs_2023_df %>% filter((`3.03 DateofDXA` > `1.09 Date Index Frac Diagnosed`) & (difftime(`3.03 DateofDXA`,`1.09 Date Index Frac Diagnosed`,units = "days") <= 90)) %>% 
  dplyr::group_by(`1.03 Gender`,`1.11 SiteFirstFrac`) %>% 
  dplyr::summarise(n = n()) %>% 
  View()



###########################

# For 2023 data, generate separate exports for 6.05 and 7.05 (16- and 52-week treatment) that exclude one site (Southampton)
# This is because this site did not record any data for these two variables in 2023

inputs_2023_df_v2 <- inputs_2023_df %>% 
  filter(site != "Southampton")
write.csv(inputs_2023_df_v2,"~/OneDrive - Nexus365/ADOPT FLSDB/Datasets/inputs_df_2023_no_soton.csv")

inputs_2023_df_v2 %>% 
  filter((`4.01 BoneTherapy` %in% c("Oral BP","IV BP","Denosumab","Romosozumab","Teriparatide"))) %>% 
  dplyr::count(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`) %>% View()
dplyr::mutate(sum_treat = sum(n),.by = c(`1.11 SiteFirstFrac`),
              prop = n/sum_treat) %>% View()

inputs_2023_df_v2 %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`6.02 Dateof16WeekAss`) %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","Inappropriate",
                                     "Informed decline","Not started","Inappropriate",
                                     "Refer for clinical opinion","Referred"))) %>% 
  dplyr::count(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,`6.05 BoneTherapy1216`) %>%
  dplyr::mutate(sum_treat_16 = sum(n), .by = c(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`),
                perc = 100*n/sum_treat_16) %>% 
  filter(!(`6.05 BoneTherapy1216` %in% c("Don't know","Inappropriate",
                                         "Informed decline","Not started","Inappropriate",
                                         "Refer for clinical opinion","Referred"))) %>% 
  group_by(`1.11 SiteFirstFrac`) %>% 
  mutate(num_frac = sum(sum_treat_16),
         perc_frac = sum_treat_16/num_frac) %>% 
  ungroup() %>% 
  dplyr::mutate(weight_sum = perc_frac*perc) %>% View()
dplyr::filter(as.character(`4.01 BoneTherapy`) == as.character(`6.05 BoneTherapy1216`)) %>% 
  View()


inputs_2023_df_v2 %>% 
  dplyr::select(`1.03 Gender`,`1.07 DateFLSAssess`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,
                `6.05 BoneTherapy1216`,`7.01 FollowUp48_56`,`7.02 Dateof52WeekAss`,
                `7.05 BoneTherapy4856`) %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","Inappropriate",
                                     "Informed decline","Not started",
                                     "Refer for clinical opinion"))) %>% 
  dplyr::count(`4.01 BoneTherapy`,`7.05 BoneTherapy4856`) %>%
  dplyr::mutate(sum_treat_52 = sum(n), .by = c(`4.01 BoneTherapy`),
                perc = 100*n/sum_treat_52) %>% 
  dplyr::filter(as.character(`7.05 BoneTherapy4856`) == as.character(`4.01 BoneTherapy`)) %>%
  View()



inputs_2023_df_v2 %>%
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","No longer appropriate","Inappropriate",
                                     "Informed decline","Not started"))) %>% 
  dplyr::count(`1.03 Gender`,`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,`6.01 FollowUp12_16`,`6.05 BoneTherapy1216`,`7.01 FollowUp48_56`) %>%
  dplyr::mutate(sum_treat_52 = sum(n), .by = c(`1.03 Gender`,`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat_52) %>%
  dplyr::filter(`6.01 FollowUp12_16` == "Yes" | `7.01 FollowUp48_56` == "Yes") %>% View()



inputs_2023_df_v2 %>% 
  filter(!(`4.01 BoneTherapy` %in% c("Don't know","No longer appropriate","Inappropriate",
                                     "Informed decline","Not started",
                                     "Refer for clinical opinion"))) %>% 
  dplyr::count(`1.11 SiteFirstFrac`,`4.01 BoneTherapy`,`6.01 FollowUp12_16`,`6.05 BoneTherapy1216`,`7.01 FollowUp48_56`,`7.05 BoneTherapy4856`) %>%
  dplyr::mutate(sum_treat_52 = sum(n), .by = c(`1.11 SiteFirstFrac`),
                perc = 100*n/sum_treat_52) %>%
  filter(`6.01 FollowUp12_16` == "Yes" & `7.01 FollowUp48_56` == "Yes") %>% View



