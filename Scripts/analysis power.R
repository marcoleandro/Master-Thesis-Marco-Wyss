
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(tidyr)
library(swissparl)
library(texreg)

setwd("/Users/simon/Documents/repo/Master-Thesis-Marco-Wyss")

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
    slice(1) 

}

dat_test <- transform_dat(`renewables`)
mod1 <- lm(count ~ amt_yes, dat_test)
mod2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.1 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
texreg(list(mod1, mod2, mod3))
sjPlot::plot_model(mod3, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`solar`)
mod1 <- lm(count ~ amt_yes, dat_test)
mod2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
texreg(list(mod1, mod2, mod3))
sjPlot::plot_model(mod3, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`costs of electricity`)
mod1 <- lm(count ~ amt_yes, dat_test)
mod2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.3 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
texreg(list(mod1, mod2, mod3))
sjPlot::plot_model(mod3, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`substitution of limited resources`)
mod1 <- lm(count ~ amt_yes, dat_test)
mod2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.4 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
texreg(list(mod1, mod2, mod3))

dat_test <- transform_dat(`competitiveness of industry`)
mod1 <- lm(count ~ amt_yes, dat_test)
mod2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.5 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
texreg(list(mod1, mod2, mod3))
sjPlot::plot_model(mod3, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`environmental protection`)
mod1 <- lm(count ~ amt_yes, dat_test)
mod2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.6 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first), dat_test)
texreg(list(mod1, mod2, mod3), file = "Tables/table.doc")
sjPlot::plot_model(mod3, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")

dat_test <- transform_dat(`mitigation of nuclear risk`)
mod1 <- lm(count ~ amt_yes, dat_test)
mod2 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first), dat_test)
mod3.7 <- lm(count ~ amt_yes + number_of_commission_memberships_first + I(Geschäftstyp) + I(ständerat_first) + I(ParlGroupName_first) , dat_test)
texreg(list(mod1, mod2, mod3))
sjPlot::plot_model(mod3, terms = "number_of_commission_memberships_first", type = "est") +
  theme(legend.position = "bottom")


extract_res <- function(mod3, var){
  res <- summary(mod3)
  res <- as.data.frame(res$coefficients)
  res$name_coef <- rownames(res)
  res %>% 
    filter(name_coef %in% "number_of_commission_memberships_first") %>% 
    mutate(dep_var = var)
}

effect_size_plot <- bind_rows(extract_res(mod3, "environmental protection"), 
          extract_res(mod3.1, "mitigation of nuclear risk"),
          extract_res(mod3.2, "bla2"),
          extract_res(mod3.3, "bla3"),
          extract_res(mod3.4, "bla4"),
          extract_res(mod3.5, "bla5"),
          extract_res(mod3.6, "bla6"),
          extract_res(mod3.7, "bla7")
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
  labs(title = "Effect for membership in number of\ncommissions on the time until concpets proliferate in the discourse", x = "Dependent Variable") + 
  theme_light() +
  coord_flip()
effect_size_plot

# save the plot with a specific height and width in the Plots folder
ggsave(effect_size_plot, filename = "Plots/effect_size_plot.pdf", width = 10, height = 5)




  

