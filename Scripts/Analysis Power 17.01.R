
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(tidyr)
library(swissparl)
library(texreg)
library(nlme)

# setwd("/Users/simon/Documents/repo/Master-Thesis-Marco-Wyss")
setwd("/Users/marcowyss/Master-Thesis-Marco-Wyss")

# read prediction data
ch_dat_debate <- readRDS("Data/CH_predictions_CHtrain.rds")
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  x <- lapply(x, as.data.frame)
  x <- do.call(rbind, x)
  x
}

# read power data
power <- read_excel_allsheets("Data/Machtpositionen CH Parlament 2000-2022.xlsx")

# get committee data
member_committee <- swissparl::get_data("MemberCommitteeHistory", Language = "DE")

# person_info <- swissparl::get_data("Person", Language = "DE")

# extract year in the data containing the debate transcripts
ch_dat_debate$year <- as.numeric(format(as.Date(ch_dat_debate$EndTimeWithTimezone, format="%Y-%m-%d"),"%Y"))

# join debate data and manually collected committee data
ch_dat_debate <- left_join(ch_dat_debate, power, by = c("SpeakerFullName" = "Politician", "year" = "Year"))

# aggregate number of times an MP is a committee
member_committee_panel <- member_committee %>%
  mutate(FullName = paste(LastName, FirstName)) %>%
  mutate(yearJoining = as.numeric(format(as.Date(DateJoining, format="%Y-%m-%d"),"%Y"))) %>% 
  arrange(FullName) %>%
  filter(CommitteeFunctionName == "Mitglied") %>% 
  select(FullName, CommitteeFunctionName, CommitteeName, yearJoining) %>% 
  complete(nesting(FullName, CommitteeFunctionName), yearJoining = full_seq(yearJoining, period = 1)) # create balanced panel 
member_committee_panel <- member_committee_panel %>%
  group_by(FullName, CommitteeFunctionName, yearJoining) %>%
  filter(CommitteeFunctionName == "Mitglied") %>% # only members
  count(CommitteeFunctionName) %>%  # how many?
  arrange(FullName) %>%
  rename(number_of_commission_memberships = n)
committee_function

# merge to debate data
ch_dat_debate <- left_join(ch_dat_debate, member_committee_panel , by = c("SpeakerFullName" = "FullName", "year" = "yearJoining"))
ch_dat_debate

# those with NA are not member are not member of any commission
ch_dat_debate <- ch_dat_debate %>% 
  mutate(number_of_commission_memberships = ifelse(is.na(number_of_commission_memberships), 0, number_of_commission_memberships))

# look at distribution
table(ch_dat_debate$number_of_commission_memberships)

# arrange by date and count seconds between the statements
ch_dat_debate <- ch_dat_debate %>%
  arrange(Title, EndTimeWithTimezone) %>% # Title (of Business), end time of the statement 
  mutate(amt_yes = ifelse(!is.na(Amt), 1, 0),
         president_yes = ifelse(Amt == "Präsident", 1, 0)) %>% 
  filter(EndTimeWithTimezone >= "2000") # filter larger than 1999

# how many statements per Business?
ch_dat_debate %>%  
  group_by(Title) %>%
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  select(Title, n) %>% 
  as.data.frame() %>% 
  head(n = 30) %>% 
  as_tibble()

ch_dat_debate %>% 
  group_by(Title) %>% 
  mutate(n_th_statement = 1:n()) %>% 
  mutate(cum = cumsum(`costs of electricity` == 1),
         power_first = ifelse(amt_yes == 1 & `costs of electricity` == 1, 1, 0)) %>%
  group_by(Title, cum) %>%
  mutate(count = n()) %>% 
  group_by(power_first) %>%
  group_by(cum) %>% 
  # ungroup() %>% select(SpeakerFullName,count, cum, `costs of electricity`, cum, amt_yes, power_first, number_of_commission_memberships_first) %>% as.data.frame() %>%
  mutate(power_first = first(power_first),
         ParlGroupName_first = first(ParlGroupName),
         CantonName_first = first(CantonName),
         CouncilName_first = first(CouncilName),
         number_of_commission_memberships_first = first(number_of_commission_memberships)) %>% 
  group_by(Title, cum) %>%
  select(SpeakerFullName,count, cum, `costs of electricity`, cum, amt_yes, power_first, number_of_commission_memberships_first) %>%
  slice(1) 


transform_dat <- function(var){
  ch_dat_debate %>% 
    group_by(Title) %>% 
    mutate(n_th_statement = 1:n()) %>% 
    mutate(cum = cumsum({{var}} == 1),
           power_first = ifelse(amt_yes == 1 & {{var}} == 1, 1, 0),
           ständerat = ifelse(CouncilName == "Ständerat", 1 , 0)) %>%
    group_by(Title, cum) %>%
    mutate(count = n()) %>% 
    group_by(power_first) %>%
    group_by(cum) %>% 
    # ungroup() %>% select(SpeakerFullName,count, `costs of electricity`, cum, amt_yes, power_first, number_of_commission_memberships_first) %>% as.data.frame() %>%
    mutate(power_first = first(power_first),
           ParlGroupName_first = first(ParlGroupName),
           CantonName_first = first(CantonName),
           CouncilName_first = first(CouncilName),
           ständerat_first = first(ständerat),
           number_of_commission_memberships_first = first(number_of_commission_memberships)) %>% 
    group_by(Title, cum) %>%
    # select(SpeakerFullName,count, cum, amt_yes, power_first, number_of_commission_memberships_first) %>%
    dplyr::slice(1) 
  
}

dat_test <- transform_dat(`renewables`)
mod1.1 <- lm(count ~ amt_yes, dat_test)
mod2.1 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.1 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.1, mod2.1, mod3.1), 
        file="model_renewables_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.1, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`solar`)
mod1.2 <- lm(count ~ amt_yes, dat_test)
mod2.2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.2, mod2.2, mod3.2), 
        file="model_solar_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.2, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`water`)
mod1.3 <- lm(count ~ amt_yes, dat_test)
mod2.3 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.3 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.3, mod2.3, mod3.3), 
        file="model_water_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.3, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`wind`)
mod1.4 <- lm(count ~ amt_yes, dat_test)
mod2.4 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.4 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.4, mod2.4, mod3.4), 
        file="model_wind_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.4, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`bio`)
mod1.5 <- lm(count ~ amt_yes, dat_test)
mod2.5 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.5 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.5, mod2.5, mod3.5), 
        file="model_bio_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.5, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`geo`)
mod1.6 <- lm(count ~ amt_yes, dat_test)
mod2.6 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.6 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.6, mod2.6, mod3.6), 
        file="model_geo_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.6, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`nuclear`)
mod1.7 <- lm(count ~ amt_yes, dat_test)
mod2.7 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.7 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.7, mod2.7, mod3.7), 
        file="model_nuclear_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.7, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`efficiency`)
mod1.8 <- lm(count ~ amt_yes, dat_test)
mod2.8 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.8 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.8, mod2.8, mod3.8), 
        file="model_efficiency_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.8, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`environmental protection`)
mod1.9 <- lm(count ~ amt_yes, dat_test)
mod2.9 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.9 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first), dat_test)
htmlreg(list(mod1.9, mod2.9, mod3.9), 
        file="model_environmental protection_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.9, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`security of electricity supply`)
mod1.10 <- lm(count ~ amt_yes, dat_test)
mod2.10 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.10 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.10, mod2.10, mod3.10), 
        file="model_security of electricity supply_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.10, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`avoidance of dependencies`)
mod1.11 <- lm(count ~ amt_yes, dat_test)
mod2.11 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.11 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.11, mod2.11, mod3.11), 
        file="model_avoidance of dependencies_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.11, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`costs of electricity`)
mod1.12 <- lm(count ~ amt_yes, dat_test)
mod2.12 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.12 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.12, mod2.12, mod3.12), 
        file="model_costs of electricity_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.12, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`competitiveness of industry`)
mod1.13 <- lm(count ~ amt_yes, dat_test)
mod2.13 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.13 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.13, mod2.13, mod3.13), 
        file="model_competitiveness of industry_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.13, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`value chain`)
mod1.14 <- lm(count ~ amt_yes, dat_test)
mod2.14 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.14 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.14, mod2.14, mod3.14), 
        file="model_value chain_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.14, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`innovation`)
mod1.15 <- lm(count ~ amt_yes, dat_test)
mod2.15 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.15 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.15, mod2.15, mod3.15), 
        file="model_innovation_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.15, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`employment`)
mod1.16 <- lm(count ~ amt_yes, dat_test)
mod2.16 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.16 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.16, mod2.16, mod3.16), 
        file="model_employment_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.16, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`energy democracy`)
mod1.17 <- lm(count ~ amt_yes, dat_test)
mod2.17 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.17 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.17, mod2.17, mod3.17), 
        file="model_energy democracy_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.17, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`climate change mitigation`)
mod1.18 <- lm(count ~ amt_yes, dat_test)
mod2.18 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.18 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.18, mod2.18, mod3.18), 
        file="model_climate change mitigation_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.18, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`pollution reduction`)
mod1.19 <- lm(count ~ amt_yes, dat_test)
mod2.19 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.19 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.19, mod2.19, mod3.19), 
        file="model_pollution reduction_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.19, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`mitigation of nuclear risk`)
mod1.20<- lm(count ~ amt_yes, dat_test)
mod2.20 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.20 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.20, mod2.20, mod3.20), 
        file="model_mitigation of nuclear risk_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.20, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`substitution of limited resources`)
mod1.21 <- lm(count ~ amt_yes, dat_test)
mod2.21 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.21 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.21, mod2.21, mod3.21), 
        file="model_substitution of limited resources_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.21, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`grid extension`)
mod1.22 <- lm(count ~ amt_yes, dat_test)
mod2.22 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.22 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
htmlreg(list(mod1.22, mod2.22, mod3.22), 
        file="model_grid extension_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
sjPlot::plot_model(mod3.22, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")


extract_res <- function(mod3, var){
  res <- summary(mod3)
  res <- as.data.frame(res$coefficients)
  res$name_coef <- rownames(res)
  res %>% 
    filter(name_coef %in% "number_of_commission_memberships_first") %>% 
    mutate(dep_var = var)
}

effect_size_plot <- bind_rows(extract_res(mod3.1, "Renewable Energy (in general)"), 
                              extract_res(mod3.2, "Solar Energy"),
                              extract_res(mod3.3, "Water Energy"),
                              extract_res(mod3.4, "Wind Energy"),
                              extract_res(mod3.5, "Biomass Energy"),
                              extract_res(mod3.6, "Geothermal Energy"),
                              extract_res(mod3.7, "Nuclear Energy"),
                              extract_res(mod3.8, "Energy Efficiency"),
                              extract_res(mod3.9, "Environmental Protection"),
                              extract_res(mod3.10, "Security of Electricity Supply"),
                              extract_res(mod3.11, "Avoidance of Dependencies"),
                              extract_res(mod3.12, "Costs of Electricity"),
                              extract_res(mod3.13, "Competitiveness of Swiss Industry"),
                              extract_res(mod3.14, "Swiss Value Chain"),
                              extract_res(mod3.15, "Innovation"),
                              extract_res(mod3.16, "Employment"),
                              extract_res(mod3.17, "Energy Democracy"),
                              extract_res(mod3.18, "Climate Change Mitigation"),
                              extract_res(mod3.19, "Pollution Reduction"),
                              extract_res(mod3.20, "Mitigation of Nuclear Risk"),
                              extract_res(mod3.21, "Substitution of Limited Resources"),
                              extract_res(mod3.22, "Grid Extension"),
)  %>%  
  mutate(sig_level = "",
         sig_level = case_when(`Pr(>|t|)` > 0.05 ~ "",
                               `Pr(>|t|)` < 0.001 ~ "***",
                               `Pr(>|t|)` < 0.01 ~ "**",
                               `Pr(>|t|)` < 0.05 ~ "*",)
  ) %>% 
  ggplot() +
  geom_pointrange(aes(y = Estimate, 
                      x = dep_var, 
                      ymin = (Estimate - (1.96*`Std. Error`)), 
                      ymax = (Estimate + (1.96*`Std. Error`))))  +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.7, color = "gray50", shape=22) +
  geom_text(aes(y = Estimate, x = dep_var, label = sig_level), 
            position = position_dodge(width = .9), vjust = -.1, 
  ) +
  labs(title = "Effect for Membership in Number of\nCommissions on the Time until Concpets proliferate in the Discourse", x = "Dependent Variable") + 
  theme_light() +
  coord_flip()
effect_size_plot

# save the plot with a specific height and width in the Plots folder
ggsave(effect_size_plot, filename = "Plots/effect_size_plot.pdf", width = 10, height = 5)


# model overview for appendix

htmlreg(list(mod3.1, mod3.2, mod3.3, mod3.4, mod3.5, mod3.6, mod3.7, mod3.8, mod3.9, mod3.10, mod3.12, mod3.13, mod3.15, mod3.16, mod3.17, mod3.18, mod3.19, mod3.20, mod3.21), 
        file="model_overview.doc", 
        single.row = TRUE, 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE) 
unlink("texreg.doc")



