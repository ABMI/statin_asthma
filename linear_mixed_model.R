library(DatabaseConnector)
library(ggplot2)
library(dplyr)
library(interactions)
library(ggpubr)
library(lme4)
library(tibble)
library(sjPlot)
library(nlme)

pd = position_dodge(width = 0.2)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
                                                                server = '128.1.99.58',
                                                                user = '',
                                                                password = '',
                                                                pathToDriver = '')
conn <- DatabaseConnector::connect(connectionDetails)

## Target cohort, Comparator cohort from MSSQL
statin <- dbGetQuery(conn,"SELECT * FROM [CDMPv534_ABMI].[results_v280].[cohort] where COHORT_DEFINITION_ID = '2002'")
statin <- statin %>% mutate(daysToCohortEnd = as.numeric(cohort_end_date - cohort_start_date)) %>% select(SUBJECT_ID, cohort_start_date, cohort_end_date, daysToCohortEnd)
colnames(statin) <- c('CDM_patient_ID', 'cohortStartDate', 'cohortEndDate', 'daysToCohortEnd')

no_statin <- dbGetQuery(conn,"SELECT * FROM [CDMPv534_ABMI].[results_v280].[cohort] where COHORT_DEFINITION_ID = '2003'")
no_statin <- no_statin %>% mutate(daysToCohortEnd = as.numeric(cohort_end_date - cohort_start_date)) %>% select(SUBJECT_ID, cohort_start_date, cohort_end_date, daysToCohortEnd)
colnames(no_statin) <- c('CDM_patient_ID', 'cohortStartDate', 'cohortEndDate', 'daysToCohortEnd')

person <- dbGetQuery(conn,"SELECT person_id, birth_datetime, gender_source_value FROM [CDMPv534_ABMI].[dbo].[person]")

## final
# statin - 3056248, 2917094
# no statin - 2261001, 1707336
afterPSMstatin <- read.csv('C:/Users/ABMI/Downloads/afterPSMstatin.csv')
afterPSMnostatin <- read.csv('C:/Users/ABMI/Downloads/afterPSMnostatin.csv')

statin_final <- merge(statin %>% select(CDM_patient_ID, cohortStartDate, cohortEndDate), afterPSMstatin %>% select(CDM_patient_ID, cohortStartDate))
no_statin_final <- merge(no_statin %>% select(CDM_patient_ID, cohortStartDate, cohortEndDate), afterPSMnostatin %>% select(CDM_patient_ID, cohortStartDate))

#################################### Lab 수치 비교하기 ########################################

# CRP(mg/L) : 3020460 (-1, 6) / IgE(IU/mL) : 42529219 (-30, 600) / ECP(μg/L) : 3005791 (0, 45) / Triglyceride : 3022192 (0, 500) / LDL(mg/dL) : 3028437 (0, 250) / HDL : 3007070 (0, 110) / AST(IU/L) : 3013721 / ALT(IU/L) : 3006923 / Creatinine kinase : 3007220 / HbA1c(%) : 3004410 / glucose : 3040820 / FeNO : 40480257

# 모든 측정치 활용 (+ 나이, 성별, 반복측정치 보정)
extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv534_ABMI].[dbo].[measurement] m 
                     where m.measurement_concept_id = 3040820 and m.person_id in (", paste(statin_final$CDM_patient_ID, collapse = ', '), ')')
statin_group <- dbGetQuery(conn, extractcode)
statin_group <- merge(statin_group, person, by = 'person_id')
colnames(statin_group) <- c('CDM_patient_ID', 'measurement_date', 'value_as_number', 'birth', 'gender')
statin_group <- merge(statin_final, statin_group, by = 'CDM_patient_ID')
statin_group <- statin_group %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(statin_treatment_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(statin = 'yes')

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv534_ABMI].[dbo].[measurement] m 
                     where m.measurement_concept_id = 3040820 and m.person_id in (", paste(no_statin_final$CDM_patient_ID, collapse = ', '), ')')
no_statin_group <- dbGetQuery(conn, extractcode)
no_statin_group <- merge(no_statin_group, person, by = 'person_id')
colnames(no_statin_group) <- c('CDM_patient_ID', 'measurement_date', 'value_as_number', 'birth', 'gender')
no_statin_group <- merge(no_statin_final, no_statin_group, by = 'CDM_patient_ID')
no_statin_group <- no_statin_group %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(statin_treatment_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(statin = 'no')

statin_group <- statin_group %>% filter(statin_treatment_years<10)
no_statin_group <- no_statin_group %>% filter(statin_treatment_years<10)
total <- rbind(statin_group, no_statin_group)

lmm_model <- lmer(
  value_as_number ~ statin_treatment_years*statin + age + gender
  + (1 | CDM_patient_ID),
  data = total
)

sjPlot::plot_model(lmm_model, type = "pred", terms = c("statin_treatment_years", 'statin'), axis.title = c('Years of follow-up', 'Blood glucose (mg/dl)'), title = '', alpha = 0) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey50")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .7, position = pd, linetype = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_fill_manual(values=c("white", "black")) +
  scale_color_manual(values=c("black", "black")) +
  scale_y_continuous(limits = c(0, 5)) + 
  aes(linetype=group, color=group) +
  geom_point(size = 4, stroke = .5, shape = 21, position = pd) + 
  scale_x_discrete(limits=c(0, 2, 4, 6, 8, 10)) # 0, 1, 2, 3, 4, 5  ## 0, 2, 4, 6, 8, 10

sjPlot::tab_model(lmm_model)

length(unique(total$CDM_patient_ID))
nrow(total)

# Total Eosinophils, Total Neutrophils 경향성을 보기 (+ 외래 측정치만 활용) (+ 나이, 성별, 반복측정치 보정)
### Eosinophil count (cells/mcL) : 3006504 (0, 1000) / Neutrophil count (cells/mcL) : 3018010 (0, 10000)

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv534_ABMI].[dbo].[measurement] m 
                     where m.measurement_concept_id = 3010813 and m.person_id in (", paste(statin_final$CDM_patient_ID, collapse = ', '), ')')
statin_leukocyte <- dbGetQuery(conn, extractcode)
statin_leukocyte <- merge(statin_leukocyte, person, by = 'person_id')
colnames(statin_leukocyte) <- c('CDM_patient_ID', 'measurement_date', 'leukocyte', 'birth', 'gender')
statin_leukocyte <- merge(statin_final, statin_leukocyte, by = 'CDM_patient_ID')
statin_leukocyte <- statin_leukocyte %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(statin_treatment_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(statin = 'yes')

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv534_ABMI].[dbo].[measurement] m 
                     where m.measurement_concept_id = 3006504 and m.person_id in (", paste(statin_final$CDM_patient_ID, collapse = ', '), ')')
statin_eosinophil_percent <- dbGetQuery(conn, extractcode)
statin_eosinophil_percent <- merge(statin_eosinophil_percent, person, by = 'person_id')
colnames(statin_eosinophil_percent) <- c('CDM_patient_ID', 'measurement_date', 'eosinophil_percent', 'birth', 'gender')
statin_eosinophil_percent <- merge(statin_final, statin_eosinophil_percent, by = 'CDM_patient_ID')
statin_eosinophil_percent <- statin_eosinophil_percent %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(statin_treatment_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(statin = 'yes')

statin_eosinophil <- merge(statin_leukocyte, statin_eosinophil_percent %>% select(CDM_patient_ID, measurement_date, eosinophil_percent), by=c('CDM_patient_ID', 'measurement_date'))
statin_eosinophil <- statin_eosinophil %>% mutate(total_eosinophil_count = leukocyte * eosinophil_percent * 10)

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv534_ABMI].[dbo].[measurement] m 
                     where m.measurement_concept_id = 3010813 and m.person_id in (", paste(no_statin_final$CDM_patient_ID, collapse = ', '), ')')
no_statin_leukocyte <- dbGetQuery(conn, extractcode)
no_statin_leukocyte <- merge(no_statin_leukocyte, person, by = 'person_id')
colnames(no_statin_leukocyte) <- c('CDM_patient_ID', 'measurement_date', 'leukocyte', 'birth', 'gender')
no_statin_leukocyte <- merge(no_statin_final, no_statin_leukocyte, by = 'CDM_patient_ID')
no_statin_leukocyte <- no_statin_leukocyte %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(statin_treatment_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(statin = 'no')

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv534_ABMI].[dbo].[measurement] m 
                     where m.measurement_concept_id = 3006504 and m.person_id in (", paste(no_statin_final$CDM_patient_ID, collapse = ', '), ')')
no_statin_eosinophil_percent <- dbGetQuery(conn, extractcode)
no_statin_eosinophil_percent <- merge(no_statin_eosinophil_percent, person, by = 'person_id')
colnames(no_statin_eosinophil_percent) <- c('CDM_patient_ID', 'measurement_date', 'eosinophil_percent', 'birth', 'gender')
no_statin_eosinophil_percent <- merge(no_statin_final, no_statin_eosinophil_percent, by = 'CDM_patient_ID')
no_statin_eosinophil_percent <- no_statin_eosinophil_percent %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(statin_treatment_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(statin = 'no')

no_statin_eosinophil <- merge(no_statin_leukocyte, no_statin_eosinophil_percent %>% select(CDM_patient_ID, measurement_date, eosinophil_percent), by=c('CDM_patient_ID', 'measurement_date'))
no_statin_eosinophil <- no_statin_eosinophil %>% mutate(total_eosinophil_count = leukocyte * eosinophil_percent * 10)

statin_eosinophil <- statin_eosinophil %>% filter(statin_treatment_years<10)
no_statin_eosinophil <- no_statin_eosinophil %>% filter(statin_treatment_years<10)
statin_eosinophil <- statin_eosinophil %>% filter(total_eosinophil_count != 0)
no_statin_eosinophil <- no_statin_eosinophil %>% filter(total_eosinophil_count != 0)
total <- rbind(statin_eosinophil, no_statin_eosinophil)

lmm_model <- lmer(
  total_eosinophil_count ~ statin_treatment_years*statin + age + gender
  + (1 | CDM_patient_ID),
  data = total
)

pd = position_dodge(width = 0.4)
sjPlot::plot_model(lmm_model, type = "pred", terms = c("statin_treatment_years", 'statin'), axis.title = c('Years of follow-up', 'Eosinophil count (cells/mcL)'), title = '', alpha = 0) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey50")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .7, position = pd, linetype = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_fill_manual(values=c("white", "black")) +
  scale_color_manual(values=c("black", "black")) +
  scale_y_continuous(limits = c(0, 330)) +
  scale_x_continuous(limits = c(-0.1, 1.1)) +
  aes(linetype=group, color=group) +
  geom_point(size = 4, stroke = .5, shape = 21, position = pd) + 
  scale_x_discrete(limits=c(0, 2, 4, 6, 8, 10)) # 0, 1, 2, 3, 4, 5  ## 0, 2, 4, 6, 8, 10

sjPlot::tab_model(lmm_model)

length(unique(total$CDM_patient_ID))
nrow(total)