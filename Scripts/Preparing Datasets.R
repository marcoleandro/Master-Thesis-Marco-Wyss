# Data Analysis Master Thesis Echo Chambers
# ******************************************

# CH data set
# ***********

setwd("~/OneDrive - Universitaet Bern/Masterarbeit/Daten")

fulldataCH <- read.csv("full_parliamentary_transcript_corpus.csv")

library(tidyverse)

# quick exploration of full data set: CH data
# -------------------------------------------

summary_X <- table(fulldataCH$X)
summary_ID <- table(fulldataCH$ID)
summary_Language <- table(fulldataCH$Language)
summary_IdSubject <- table(fulldataCH$IdSubject)
summary_VoteId<- table(fulldataCH$VoteId)
summary_PersonNumber <- table(fulldataCH$PersonNumber)
summary_Type <- table(fulldataCH$Type)
summary_Text <- table(fulldataCH$Text)
summary_MeetingCouncilAbbreviation <- table(fulldataCH$MeetingCouncilAbbreviation)
summary_MeetingDate <- table(fulldataCH$MeetingDate)
summary_MeetingVerbalixOid<- table(fulldataCH$MeetingVerbalixOid)
summary_IdSession<- table(fulldataCH$IdSession)
summary_SpeakerFirstName <- table(fulldataCH$SpeakerFirstName)
summary_SpeakerLastName <- table(fulldataCH$SpeakerLastName)
summary_SpeakerFullName <- table(fulldataCH$SpeakerFullName)
summary_SpeakerFunction <- table(fulldataCH$SpeakerFunction)
summary_CouncilId <- table(fulldataCH$CouncilId)
summary_CouncilName <- table(fulldataCH$CouncilName)
summary_CantonId<- table(fulldataCH$CantonId)
summary_CantonName <- table(fulldataCH$CantonName)
summary_CantonAbbreviation <- table(fulldataCH$CantonAbbreviation)
summary_ParlGroupName <- table(fulldataCH$ParlGroupName)
summary_ParlGroupAbbreviation <- table(fulldataCH$ParlGroupAbbreviation)
summary_SortOrder <- table(fulldataCH$SortOrder)
summary_Start <- table(fulldataCH$Start)
summary_End <- table(fulldataCH$End)
summary_Function <- table(fulldataCH$Function)
summary_DisplaySpeaker <- table(fulldataCH$DisplaySpeaker)
summary_LanguageOfText <- table(fulldataCH$LanguageOfText)
summary_Modified <- table(fulldataCH$Modified)
summary_StartTimeWithTimezone <- table(fulldataCH$StartTimeWithTimezone)
summary_EndTimeWithTimezone <- table(fulldataCH$EndTimeWithTimezone)
summary_VoteBusinessNumber <- table(fulldataCH$VoteBusinessNumber)
summary_VoteBusinessShortNumber <- table(fulldataCH$VoteBusinessShortNumber)
summary_VoteBusinessTitle <- table(fulldataCH$VoteBusinessTitle)

# data set cleansing part 1
# -------------------------

fulldataCH_tidy_prep <- fulldataCH[grepl("DE", fulldataCH$Language),]

fulldataCH_tidy_prep_DE <- filter(fulldataCH_tidy_prep, LanguageOfText == 'DE')

fulldataCH_tidy_prep_FR <- filter(fulldataCH_tidy_prep, LanguageOfText == 'FR')

fulldataCH_tidy_prep_IT <- filter(fulldataCH_tidy_prep, LanguageOfText == 'IT')

# translating CH french/italian statements into german
# ****************************************************

# with google translate
# ---------------------

install.packages("googleLanguageR")
library(googleLanguageR)
gl_auth("/Users/marcowyss/Library/CloudStorage/OneDrive-UniversitaetBern/Masterarbeit/Daten/Working with R/googleLanguageR/notional-buffer-370010-65be0f931989.json")

# italian to german

fulldataCH_tidy_prep_IT_translated <- 
  gl_translate(
    fulldataCH_tidy_prep_IT$Text,
    target = "de",
    format = c("text"),
    source = "it")

saveRDS(fulldataCH_tidy_prep_IT_translated, file = "fulldataCH_tidy_prep_IT_translated.rds")

IT_translated <- readRDS("~/OneDrive - Universitaet Bern/Masterarbeit/Daten/CH Daten/fulldataCH_tidy_prep_IT_translated.rds")

writexl::write_xlsx(fulldataCH_tidy_prep_IT_translated,"~/OneDrive - Universitaet Bern/Masterarbeit/Daten/fulldataCH_tidy_prep_IT_translated.xlsx")

# french to german

fulldataCH_tidy_prep_FR_translated <- 
  gl_translate(
    fulldataCH_tidy_prep_FR$Text,
    target = "de",
    format = c("text"),
    source = "fr")

saveRDS(fulldataCH_tidy_prep_FR_translated, file = "fulldataCH_tidy_prep_FR_translated.rds")

FR_translated <- readRDS("~/OneDrive - Universitaet Bern/Masterarbeit/Daten/CH Daten/fulldataCH_tidy_prep_FR_translated.rds")

write.csv(fulldataCH_tidy_prep_FR_translated,"~/OneDrive - Universitaet Bern/Masterarbeit/Daten/fulldataCH_tidy_prep_FR_translated.csv")

# with deepl (alternative option that wasn`t used in the end)
# -----------------------------------------------------------

library(devtools)
install.packages("deeplr")
install.packages("tokenizers")
library(tokenizers)
library(deeplr)

CO2_debates_full_tidy_IT_translated <- translate2(CO2_debates_full_tidy_IT$Text, 
                                                  source_lang = "IT",
                                                  target_lang = "DE",
                                                  auth_key = "cfda1e98-2d59-d894-0ee8-be7ab8ccc0dc:fx")

# data wrangling 
# **************

# merging translations into a fully german based data set
# -------------------------------------------------------

library(dplyr)
library(readr)

joined_CH_IT <- left_join(fulldataCH_tidy_prep, IT_translated, 
                          by = c("Text" = "text"))

saveRDS(joined_CH_IT, file = "joined_CH_IT.rds")

joined_CH_translated_full <- left_join(joined_CH_IT, FR_translated, 
                          by = c("Text" = "text"))

saveRDS(joined_CH_translated_full, file = "joined_CH_translated_full.rds")

# remove duplicates
# -----------------

dup_test <- joined_CH_translated_full$X[duplicated(joined_CH_translated_full$X)]
CH_translated_prep <- joined_CH_translated_full[!duplicated(joined_CH_translated_full$X), ]

saveRDS(CH_translated_prep, file = "CH_translated_prep.rds")

# replace original text with translated text
# ------------------------------------------

# french

FR_replaced <- filter(CH_translated_prep, LanguageOfText == 'FR')
FR_replaced$Text <- FR_replaced$translatedText.y

# italian

IT_replaced <- filter(CH_translated_prep, LanguageOfText == 'IT')
IT_replaced$Text <- IT_replaced$translatedText.x

# creating data frame with only one german text variable
# ------------------------------------------------------

DE_replaced <- filter(CH_translated_prep, LanguageOfText == 'DE')

CH_prep <- rbind(DE_replaced, FR_replaced, IT_replaced)

saveRDS(CH_prep, file = "CH_prep.rds")

# add business information to data set

library(swissparl)

business <- swissparl::get_data(
  table = "SubjectBusiness",
  Language = "DE"
)

saveRDS(business, file = "business.rds")

business_clean <- subset(business, select = c(IdSubject, Title, BusinessNumber))

CH_preparation_business <- merge(`CH_prep`, business_clean, by = "IdSubject")
dup_test <- CH_preparation_business[duplicated(CH_preparation_business$Text), ]
CH_preparation_business_clean <- CH_preparation_business %>% distinct(Text, .keep_all = TRUE)

saveRDS(CH_preparation_business_clean, file = "CH_preparation_business_clean.rds")

# selecting necessary variables
# -----------------------------

CH_translated_business_merged_tidy <- CH_preparation_business_clean %>% select(X, ID, IdSession, Text, MeetingCouncilAbbreviation, MeetingDate, SpeakerFullName, SpeakerFunction, CantonName, ParlGroupName, Start, LanguageOfText, Title, BusinessNumber)
saveRDS(CH_translated_business_merged_tidy, file = "CH_translated_business_merged_tidy.rds")

# filter for relevant data (keyword "Energie" in Curia Vista) 
# *********************************

CH_energy <- merge(CH_translated_business_merged_tidy, CuriaExport_2023_1_1_complete, by.x = "Title", by.y = "Geschäftstitel")
dup_test <- CH_energy[duplicated(CH_energy$Text), ]
CH_energy_clean <- CH_energy %>% distinct(Text, .keep_all = TRUE)

saveRDS(CH_energy_clean, file = "CH_energy_clean.rds")

# prepare CH data for SML prediction

CH_energy_SML_predict <- CH_energy_clean %>% select(ID, Text)
saveRDS(CH_energy_SML_predict, file = "CH_energy_SML_predict.rds")

# select random sample for reliability checks (10% of total sample)

set.seed(789)
CH_energy_SML_relcheck <- CH_energy_SML_predict[sample(nrow(CH_energy_SML_predict), 200), ]
saveRDS(CH_energy_SML_relcheck, file = "CH_energy_SML_relcheck")
writexl::write_xlsx(CH_energy_SML_relcheck,"CH_energy_SML_relcheck.xlsx")

saveRDS(CH_energy_SML_reliabilitycheck, file = "CH_energy_SML_reliabilitycheck.rds")

set.seed(987)
CH_energy_SML_relcheck_2 <- CH_energy_SML_predict[sample(nrow(CH_energy_SML_predict), 500), ]
saveRDS(CH_energy_SML_relcheck_2, file = "CH_energy_SML_relcheck_2")
writexl::write_xlsx(CH_energy_SML_relcheck_2,"CH_energy_SML_relcheck_2.xlsx")

# checking for doubles

xyz <- merge(CH_energy_SML_relcheck, CH_energy_SML_relcheck_2, by = "ID")

set.seed(978)
CH_energy_SML_relcheck_3 <- CH_energy_SML_predict[sample(nrow(CH_energy_SML_predict), 2), ]
saveRDS(CH_energy_SML_relcheck_3, file = "CH_energy_SML_relcheck_3")
writexl::write_xlsx(CH_energy_SML_relcheck_3,"CH_energy_SML_relcheck_3.xlsx")

# creating extensive data set for future research

CH_energydebates_dataset_MAMW <- merge(CH_preparation_business_clean, CuriaExport_2023_1_1_complete, by.x = "Title", by.y = "Geschäftstitel")
dup_test <- CH_energydebates_dataset_MAMW[duplicated(CH_energydebates_dataset_MAMW$Text), ]
CH_energydebates_dataset_MAMW <- CH_energydebates_dataset_MAMW %>% distinct(Text, .keep_all = TRUE)

saveRDS(CH_energydebates_dataset_MAMW, file = "CH_energydebates_dataset_MAMW.rds")

CH_parldebates_dataset_MAMW <- CH_preparation_business_clean

saveRDS(CH_parldebates_dataset_MAMW, file = "CH_parldebates_dataset_MAMW.rds")

# DE_EEG data set
# ***************

EEG_dataset <- read.csv("EEG_Daten_clean.csv", header = T)

saveRDS(EEG_dataset, file = "EEG_dataset.rds")


# outtakes

# filter for "CO2-Gesetz" (keyword) 
# *********************************

# part 1
# ------

CH_CO2_full <- filter(CH_translated_merged_tidy, str_detect(Text, "CO2-Gesetz")) # -> 1181 obs.

filter_CO2_DE <- filter(fulldataCH_tidy_prep_DE, str_detect(Text, "CO2-Gesetz")) # -> 933 obs. 

filter_CO2_FR <- filter(fulldataCH_tidy_prep_FR, str_detect(str_to_lower(Text), "Loi sur le CO2")) # -> no results

filter_CO2_IT <- filter(fulldataCH_tidy_prep_IT, str_detect(str_to_lower(Text), "Legge sul CO2")) # -> no results

# isolate IdSession
# -----------------

ID_CO2_CH <- CH_CO2_full %>% select(IdSession)

table(ID_CO2_CH)

# part 2
# ------

CH_CO2_debates_filtered <- CH_translated_merged_tidy %>% filter(IdSession == 4601 | IdSession == 4602 | IdSession == 4603 | IdSession == 4604 | IdSession == 4605 | IdSession == 4606 | IdSession == 4608 | IdSession == 4609 | IdSession == 4611 | IdSession == 4612 | IdSession == 4614 | IdSession == 4615 | IdSession == 4616 | IdSession == 4617 | IdSession == 4619 | IdSession == 4620 | IdSession == 4701 | IdSession == 4702 | IdSession == 4704 | IdSession == 4705 | IdSession == 4706 | IdSession == 4709 | IdSession == 4710 | IdSession == 4711 | IdSession == 4713 | IdSession == 4714 | IdSession == 4715 | IdSession == 4716 | IdSession == 4717 | IdSession == 4718 | IdSession == 4801 | IdSession == 4802 | IdSession == 4804 | IdSession == 4805 | IdSession == 4806 | IdSession == 4807 | IdSession == 4809 | IdSession == 4811 | IdSession == 4812 | IdSession == 4813 | IdSession == 4814 | IdSession == 4815 | IdSession == 4816 | IdSession == 4817 | IdSession == 4818 | IdSession == 4819 | IdSession == 4820 | IdSession == 4901 | IdSession == 4902 | IdSession ==4903 | IdSession == 4904 | IdSession == 4905 | IdSession == 4906 | IdSession == 4907 | IdSession == 4909 | IdSession == 4914 | IdSession == 4915 | IdSession == 4916 | IdSession == 4917 | IdSession == 4919 | IdSession == 4920 | IdSession == 5001 | IdSession == 5002 | IdSession == 5004 | IdSession == 5005 | IdSession == 5006 | IdSession == 5007 | IdSession == 5009 | IdSession == 5010 | IdSession == 5011 | IdSession == 5013 | IdSession == 5014 | IdSession == 5015 | IdSession == 5016 | IdSession == 5017  | IdSession == 5018 | IdSession == 5019 | IdSession == 5101 | IdSession == 5102 | IdSession == 5103 | IdSession == 5104  | IdSession ==5105 | IdSession == 5106 | IdSession == 5107 | IdSession == 5108 | IdSession == 5109 | IdSession == 5110 | IdSession == 5111 | IdSession == 5112 | IdSession == 5113)
saveRDS(CH_CO2_debates_filtered, file = "CH_CO2_debates_filtered.rds")


# DE_EEG data set
# ***************

setwd("~/OneDrive - Universitaet Bern/Masterarbeit/Daten/DE_EEG")

fulldataDE_EEG <- read.csv("DE_EEG_Goals.csv",sep =';')

# filter by variable "goal"
# ------------------------

dataDE_EEG_goal <- fulldataDE_EEG %>% 
  filter(Goal != "")

table(dataDE_EEG_goal$Goal)

# export as new xlsx
# ------------------

writexl::write_xlsx(dataDE_EEG_goal,"~/OneDrive - Universitaet Bern/Masterarbeit/Daten/DE_EEG/dataDE_EEG_goal.xlsx")



