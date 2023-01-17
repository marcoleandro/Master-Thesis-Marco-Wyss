# descriptive data analysis

# load files / create data
# setwd("/Users/marcowyss/master-thesis-mw")
setwd("/Users/simon/Documents/repo/Master-Thesis-Marco-Wyss")

EEG_data <- read.csv("Data/EEG_Daten_clean.csv", header = T)

# look at the values of the variables
EEG_overview <- sapply(EEG_data, table)

# prepare data for visualization

library(dplyr)

EEG_data <- EEG_data %>%
  mutate(Technologies = case_when(Renewables == 1 ~ 'Renewables',
                                Solar == 1 ~ 'Solar',
                                Wind == 1 ~ 'Wind',
                                Bio == 1 ~ 'Bio',
                                Nuclear == 1 ~ 'Nuclear'))

EEG_data <- EEG_data %>%
  mutate(Goals = case_when(Environmental.Protection == 1 ~ 'Environmental Protection',
                           Security.of.Electricity.Supply == 1 ~ 'Security of Electricity Supply',
                           Avoidance.of.Dependencies == 1 ~ 'Avoidance of Dependencies',
                           Costs.of.Electricity == 1 ~ 'Costs of Electricity',
                           Cheaper.RE.Technology == 1 ~ 'Cheaper RE Technology',
                           Competitiveness.of.Industry == 1 ~ 'Competitiveness of Industry',
                           Value.Chain == 1 ~ 'Value Chain',
                           Innovation == 1 ~ 'Innovation',
                           Employment == 1 ~ 'Employment',
                           Export == 1 ~ 'Export',
                           Energy.Democracy == 1 ~ 'Energy Democracy'))

EEG_data <- EEG_data %>%
  mutate(Objectives = case_when(Climate.Change.Mitigation == 1 ~ 'Climate Change Mitigation',
                           Pollution.Reduction == 1 ~ 'Pollution Reduction',
                           Sector.Coupling == 1 ~ 'Sector Coupling',
                           Grid.Extension == 1 ~ 'Grid Extension',
                           Energy.Storage == 1 ~ 'Energy Storage',
                           Increase.of.Actor.Variety == 1 ~ 'Increase of Actor Variety',
                           Costs.for.Consumers == 1 ~ 'Costs for Consumers',
                           Costs.for.Industry == 1 ~ 'Costs for Industry',
                           Employment.in.RE.Sector == 1 ~ 'Employment in RE Sector',
                           Employment.in.Solar.Industry == 1 ~ 'Employment in Solar Industry'))

EEG_data <- EEG_data %>%
  mutate(Parliament = case_when(Solar == 1 | Solar == 0 ~ 'Bundestag'))


# visual overview EEG data variables

install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)

# coul <- brewer.pal(5, "Set2") 
# barplot(height=data$value, names=data$name, col=coul )

ggplot(data=subset(EEG_data, !is.na(Technologies)),aes(x=factor(Technologies)))+
  geom_bar()

table(EEG_data$Technologies)

ggplot(data=subset(EEG_data, !is.na(Goals)),aes(x=factor(Goals)))+
  geom_bar()

table(EEG_data$Goals)

ggplot(data=subset(EEG_data, !is.na(Objectives)),aes(x=factor(Objectives)))+
  geom_bar()

table(EEG_data$Objectives)

# prepare CH data for visualization

library(dplyr)

CH_predictions <- CH_predictions %>%
  mutate(Technologies = case_when(renewables == 1 ~ 'Renewables',
                                  water == 1 ~ 'Water',
                                  solar == 1 ~ 'Solar',
                                  wind == 1 ~ 'Wind',
                                  bio == 1 ~ 'Bio',
                                  geo == 1 ~ 'Geo',
                                  nuclear == 1 ~ 'Nuclear',
                                  efficiency == 1 ~ 'Efficiency'))

CH_predictions <- CH_predictions %>%
  mutate(Goals = case_when(`environmental protection` == 1 ~ 'Environmental Protection',
                           `security of electricity supply` == 1 ~ 'Security of Electricity Supply',
                           `avoidance of dependencies` == 1 ~ 'Avoidance of Dependencies',
                           `costs of electricity` == 1 ~ 'Costs of Electricity',
                           `competitiveness of industry` == 1 ~ 'Competitiveness of Industry',
                           `value chain` == 1 ~ 'Value Chain',
                           innovation == 1 ~ 'Innovation',
                           employment == 1 ~ 'Employment',
                           `energy democracy` == 1 ~ 'Energy Democracy'))

CH_predictions <- CH_predictions %>%
  mutate(Objectives = case_when(`climate change mitigation` == 1 ~ 'Climate Change Mitigation',
                                `pollution reduction` == 1 ~ 'Pollution Reduction',
                                `mitigation of nuclear risk` == 1 ~ 'Mitigation of Nuclear Risks',
                                `substitution of limited resources` == 1 ~ 'Substitution of Limited Resources',
                                `grid extension` == 1 ~ 'Grid Extension'))

CH_predictions <- CH_predictions %>%
  mutate(Parliament = case_when(MeetingCouncilAbbreviation == 'N' ~ 'Nationalrat',
                                MeetingCouncilAbbreviation == 'S' ~ 'Ständerat'))


# visual overview EEG data variables

install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)

# coul <- brewer.pal(5, "Set2") 
# barplot(height=data$value, names=data$name, col=coul )

ggplot(data=subset(CH_predictions, !is.na(Technologies)),aes(x=factor(Technologies)))+
  geom_bar()

table(CH_predictions$Technologies)

ggplot(data=subset(CH_predictions, !is.na(Goals)),aes(x=factor(Goals)))+
  geom_bar()

table(CH_predictions$Goals)

ggplot(data=subset(CH_predictions, !is.na(Objectives)),aes(x=factor(Objectives)))+
  geom_bar()

table(CH_predictions$Objectives)





