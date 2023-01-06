# Machine Learning Protocol Master Thesis MW

# ****************

# initialization process
# ----------------------

# load packages

# libs <- c("tidyverse","tm", "plyr", "class", "tidymodels", "dplyr", "ggplot2", "ggsci", "ggpubr", "baguette", "discrim", "finetune", "parsnip", "ranger", "ranger", "readxl", "reshape2", "rules", "spacyr", "stringr", "themis", "text2vec", "textrecipes", "tidytext", "tidyposterior", "tmap", "tune", "workflowsets", "kernlab", "writexl", "countrycode" )
# lapply(libs, require, character.only = TRUE)

rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(baguette)
library(discrim)
library(finetune)
library(parsnip)
library(ranger)
library(readxl)
library(reshape2)
library(rules)
library(spacyr)
library(stringr)
library(themis)
library(tidymodels)
library(text2vec)
library(textrecipes)
library(tidytext)
library(tidyposterior)
library(tmap)
library(tune)
library(workflowsets)
library(kernlab)
library(writexl)
library(countrycode)
library(stopwords)
library(tidyverse)

# load files / create data
# setwd("/Users/marcowyss/master-thesis-mw/Data")
setwd("/Users/simon/Documents/repo/Master-Thesis-Marco-Wyss")

abs <- read.csv("EEG_Daten_clean.csv", header = T)

# rename variables - lower cap and underscore instead of space 
colnames(abs) <- gsub(".", "_", str_to_lower(colnames(abs)), fixed = T)

# look at the values of the variables
sapply(abs, table)

# variables with very uneven distribution (> 15 observations) between 1 and 0 will not be useful
variables <- c("renewables", "solar" 
               # "water", 
               ,"wind" 
               # "geo", 
               ,"bio"
               # ,"efficiency"
               , "nuclear"
               , "environmental_protection", "security_of_electricity_supply"     
               , "avoidance_of_dependencies", "costs_of_electricity", "cheaper_re_technology"
               , "competitiveness_of_industry", "value_chain", "innovation"                         
               , "employment", "export", "energy_democracy", "climate_change_mitigation"          
               , "pollution_reduction"
               # , "mitigation_of_nuclear_risk", "substitution_of_limited_resources"
               , "sector_coupling"                    
               , "grid_extension", "energy_storage", "increase_of_actor_variety", "costs_for_consumers",                
               # , "costs_for_households"
               # , "costs_for_industry", 
               "employment_in_re_sector"
               # , "employment_not_related_to_re_sector", 
               , "employment_in_solar_industry"
) 

abs <- abs %>% 
  dplyr::mutate_at(all_of(variables), as.factor)



# create a custom theme for the layout

theme_SM <- function () { 
  theme_classic() +
    theme(
      text = element_text(size=12, colour = "black"),
      axis.text=element_text(size=12, 
                             # angle = 90
      ),
      axis.ticks.length.x = unit(.2, "cm"),
      axis.ticks.x = element_blank(),
      axis.line = element_line(size = 0.2),
      axis.text.x = element_text(
        # angle = 90, 
        vjust=0.5, 
        # hjust=1
      ),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      # panel.spacing = unit(3, "lines"),
      strip.background = element_rect(color = "white"),
      strip.text.x = element_text(size = 15),
      strip.placement = "inside"
      # legend.position = "none"
    )
}

# split this single dataset into two: a training set and a testing set
set.seed(234)
data_split <- initial_split(abs) # can be stratified, check this out if it improves the fit and if so for which vars

# Create data frames for the two sets:
abs_train <- training(data_split)
abs_test  <- testing(data_split)
dim(abs_train)
dim(abs_test)

# create folds to train data
set.seed(234)
abs_folds <- vfold_cv(abs_train)
abs_folds

# (1) recipes 
my_rec <- function(outcome) {
  abs_train %>% 
    dplyr::select(text, {{outcome}}) %>% 
    recipe() %>% 
    update_role(text, new_role = "predictor") %>%
    update_role({{outcome}}, new_role = "outcome") %>%
    step_tokenize(text, token = "word_stems") %>%
    # step_tokenize(Abstract, Article.Title, Author.Keywords, Keywords.Plus, engine = "spacyr") %>%
    # step_lemma(Abstract, Article.Title, Author.Keywords, Keywords.Plus) %>%
    step_stopwords(text, language = "de", keep = FALSE, stopword_source = "snowball") %>%
    step_tokenfilter(text, max_tokens = 1e3) %>%
    step_tfidf(text)  %>%
    step_smote({{outcome}}) 
}

rec <- lapply(variables, my_rec)


##################################
## (6) predict all variables on entire data (before that one should actually tune the model, but now I do not have time for that):bs
##################################
sm_spec <- function(model_type){
  
  if (model_type == "xgboost"){
    
    spec <- boost_tree(trees = 1000) %>%
      set_engine("xgboost") %>%
      set_mode("classification")
    
  } else if (model_type == "forest"){
    
    spec <- rand_forest(trees = 1000) %>%
      set_mode("classification") %>%
      set_engine("ranger", # num.threads = cores
      )
    
  } else if (model_type == "lasso"){
    
    spec <- logistic_reg(penalty = 0.1, mixture = 1) %>%
      set_mode("classification") %>%
      set_engine("glmnet")
    
  } else if (model_type == "svm"){
    
    spec <- svm_poly() %>%  
      set_mode("classification") %>% 
      set_engine("kernlab")
  }
  
  spec
}

# model tuning

# sm_spec_tuned <- function(model_type){
#   
#   if (model_type == "xgboost"){
#     
#     spec <- boost_tree(
#       # tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
#       # min_n = tune(), sample_size = tune(), trees = tune(),
#       # mtry = tune()
#       trees = 1000,
#       min_n = tune(),
#       tree_depth = tune(),
#       learn_rate = tune(),
#       loss_reduction = tune()
#     ) %>%
#       set_engine("xgboost") %>%
#       set_mode("classification")
#     
#   } else if (model_type == "forest"){
#     
#     spec <- rand_forest(
#       trees = 1000,
#       mtry = tune(),
#       min_n = tune()
#     ) %>%
#       set_mode("classification") %>%
#       set_engine("ranger", # num.threads = cores
#       )
#   }
#   
#   spec
# }

# create model workflow

sm_wf <- function(rec_abs3, spec){
  ## create workflows
  wf <- workflow() %>%
    add_recipe(rec_abs3) %>%
    add_model(spec)
  
  wf
}

# train the model

sm_train <- function(w_flow, spec){
  
  doParallel::registerDoParallel()
  
  set.seed(456)
  result <- tune_grid(
    w_flow,
    resamples = abs_folds,
    grid = 25,
    metrics = metric_set(roc_auc, precision,
                         accuracy,
                         f_meas, sens,
                         yardstick::spec),
    control = control_grid(save_pred = TRUE)
  )
  
  result
}

metrics <- metric_set(roc_auc, precision,
                      accuracy,
                      f_meas, sens,
                      yardstick::spec)

# model fitting

sm_fit <- function(res1, wf1){
  
  # select the best performing model
  best <- res1 %>% select_best(., metric = "f_meas")
  
  # integrate this model into the wf
  wf_1 <- wf1 %>% finalize_workflow(best)
  
  # fit the final model to the entire data 
  set.seed(2345)
  final_model <- last_fit(wf_1, data_split, metrics = metrics)
  
  final_model
}

# use trained workflow to predict
# -------------------------------

sm_predict <- function(final_model, abstract){
  
  # convert list to df
  fitted_wf <- pluck(final_model$.workflow, 1)
  
  predict(fitted_wf, new_data = abstract)
}

# res_boost <- list()
# res_svm <- list()
res_forest <- list()
# res_lasso <- list()
# model_boost <- list()
# model_svm <- list()
model_forest <- list()
# model_lasso <- list()
# For installation follow steps in https://cran.r-project.org/web/packages/spacyr/readme/README.html 
# spacyr::spacy_initialize(model = "en_core_web_sm", entity = F)
# for (i in 1:length(rec)){
#   res_boost[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("xgboost")), sm_spec("xgboost"))
#   model_boost[[i]] <- sm_fit(res_boost[[i]], sm_wf(rec[[i]], sm_spec("xgboost")))
#   print(paste("XGBoost model", i, "complete"))
# }
# for (i in 1:length(rec)){
#   res_svm[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("svm")), sm_spec("svm"))
#   model_svm[[i]] <- sm_fit(res_svm[[i]], sm_wf(rec[[i]], sm_spec("svm")))
#   print(paste("Support Vector Machine model", i, "complete"))
# }
for (i in 1:length(rec)){
  res_forest[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("forest")), sm_spec("forest"))
  model_forest[[i]] <- sm_fit(res_forest[[i]], sm_wf(rec[[i]], sm_spec("forest")))
  print(paste("Forest model", i, "complete"))
}
# for (i in 1:length(rec)){
#   res_lasso[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("lasso")), sm_spec("lasso"))
#   model_lasso[[i]] <- sm_fit(res_lasso[[i]], sm_wf(rec[[i]], sm_spec("lasso")))
#   print(paste("Lasso model", i, "complete"))
# }

###### 
## plot the result
###### 

var_names <- c("Renewables"
               # "Solar", "Water", "Wind", "Geo", "Bio", "Efficiency", "Nuclear", # technologies
               # "Environmental Protection", "Security of Electricity Supply", "Avoidance of Dependencies", 
               # "Costs of Electricity", "Cheaper RE Technology", "Competitiveness of Industry", "Value Chain", 
               # "Innovation", "Employment", "Export", "Energy Democracy", # goals
               # "Climate Change Mitigation", "Pollution Reduction", "Mitigation of Nuclear Risk", 
               # "Substitution of Limited Resources", "Sector Coupling", "Grid Extension", 
               # "Energy Storage", "Increase of Actor Variety", "Costs for Consumers", "Costs for Households", 
               # "Costs for Industry", "Employment in RE Sector", "Employment not related to RE Sector", "Employment in Solar Industry"
) # objectives
technologies <- c("Renewables", "Solar", "Water", "Wind", "Geo", "Bio", "Efficiency", "Nuclear")
goals <- c("Environmental Protection", "Security of Electricity Supply", "Avoidance of Dependencies", 
           "Costs of Electricity", "Cheaper RE Technology", "Competitiveness of Industry", "Value Chain", 
           "Innovation", "Employment", "Export", "Energy Democracy")
objectives <- c("Climate Change Mitigation", "Pollution Reduction", "Mitigation of Nuclear Risk", 
                "Substitution of Limited Resources", "Sector Coupling", "Grid Extension", 
                "Energy Storage", "Increase of Actor Variety", "Costs for Consumers", "Costs for Households", 
                "Costs for Industry", "Employment in RE Sector", "Employment not related to RE Sector", "Employment in Solar Industry")

evaluate_model_performance <- function(res_forest, model_forest){
  # create df with performance on each cross-fold validation in the training data
  fold_perf_df <- do.call(rbind, res_forest)
  fold_perf_df <- do.call(rbind, fold_perf_df$.metrics)
  # fold_perf_df$dv <- factor(rep(var_names, each = nrow(res_forest[[1]]$.metrics[[1]])*10), levels = rev(var_names))
  fold_perf_df$dv <- factor(rep(variables, each = nrow(res_forest[[1]]$.metrics[[1]])*10), levels = rev(variables))
  
  # create df without of sample performance 
  out_of_sample_df <- do.call(rbind, model_forest)
  out_of_sample_df <- do.call(rbind, out_of_sample_df$.metrics)
  # out_of_sample_df$dv <- factor(rep(var_names, each = length(unique(out_of_sample_df$.metric))), levels = rev(var_names))
  out_of_sample_df$dv <- factor(rep(variables, each = length(unique(out_of_sample_df$.metric))), levels = rev(variables))
  
  best_in_sample <- lapply(res_forest, show_best, "f_meas", n = 1)
  best_in_sample <- do.call(rbind, best_in_sample)
  # best_in_sample$dv <- var_names
  best_in_sample$dv <- variables
  best_in_sample <- best_in_sample[, c(".metric", "mean", "dv")]
  colnames(best_in_sample)[2] <- "best_in_sample_performance"
  
  # calculate CI for cross-fold training data
  fold_perf_df <- fold_perf_df %>% 
    group_by(dv, .metric) %>% 
    dplyr::summarise(mean = mean(.estimate, na.rm = TRUE), ci_lo = quantile(.estimate, 0.05, na.rm = TRUE), ci_hi = quantile(.estimate, 0.95, na.rm = TRUE))
  
  # join them together
  performance <- left_join(fold_perf_df, out_of_sample_df, by = c("dv", ".metric"))
  performance <- left_join(performance, best_in_sample, by = c("dv", ".metric"))
  # performance$feature <- factor(c(rep("Technologies", each = length(unique(performance$.metric))*length(technologies)), 
  #                                 rep("Goals", each = length(unique(performance$.metric))*length(goals)),
  #                                 rep("Objectives", each = length(unique(performance$.metric))*length(objectives))))
  performance
}

plot_performance <- function(performance){
  ggplot(performance) +
    geom_pointrange(aes(y = mean, x = dv, ymin = ci_lo, ymax = ci_hi, col = "10-Fold Cross-Validation"), fatten = .1) +
    geom_point(position = position_dodge(width = 2), aes(y = .estimate, x = dv, col = "Out-of-Sample Validation"), fatten = 2, shape = 2) +
    # geom_point(position = position_dodge(width = 2), aes(y = best_in_sample_performance, x = dv, col = "best performing model within the sample"), fatten = 2, shape = 3) +
    scale_x_discrete("Classified Article Features") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    coord_flip() +
    facet_grid(
      # feature
      ~.metric, scales = "free_y", labeller = labeller(.metric = c("accuracy" = "Accuracy", "f_meas" = "F1 score", "precision" = "Precision",
                                                                   "roc_auc" = "ROC", "sens" = "Recall", "spec" = "Specificity"))) +
    ylab("Performance Score") +
    theme_light() +
    scale_fill_npg() +
    theme(text=element_text(size=12, color="black"),
          strip.text.x = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          strip.text = element_text(color = "black"),
          strip.background = element_rect(fill=NA), # no background box colour in grey
          axis.text.x = element_text(size=12, vjust=0.5, color = 'black'),  # x-axis labels
          axis.text.y = element_text(size=12, vjust=0.5, color = 'black'),  # y-axis labels
          axis.title.x = element_text(size=15, vjust=0.1),                  # x-title justification  
          axis.title.y = element_text(size=15, vjust=1.5),                  # y-title justification
          panel.grid = element_blank(),
          legend.position = "bottom",       
          legend.text = element_text(size = 14.5),
          legend.title = element_text(size = 14.5)
    ) +
    geom_hline(yintercept = .9, lty = 2, alpha = 0.7, color = "gray50") +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .8, ymax = .9), alpha = .01, fill = "coral") +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .9, ymax = 1), alpha = .01, fill = "green") +
    # guides(color=guide_legend(title = "Legend", override.aes=list(shape=c(1,2,3), linetype = c(1,0,0))))
    guides(color=guide_legend(title = "Legend", override.aes=list(shape=c(1,2), linetype = c(1,0))))
}

# boost_perf <- evaluate_model_performance(res_boost, model_boost)
# svm_perf <- evaluate_model_performance(res_svm, model_svm)
forest_perf <- evaluate_model_performance(res_forest, model_forest)
# lasso_perf <- evaluate_model_performance(res_lasso, model_lasso)

# p_boost <- plot_performance(boost_perf[boost_perf$.metric %in% c("sens", "f_meas", "precision"),])
# p_svm <- plot_performance(svm_perf[svm_perf$.metric %in% c("sens", "f_meas", "precision"),])
p_forest <- plot_performance(forest_perf[forest_perf$.metric %in% c("sens", "f_meas", "precision"),])
# p_lasso <- plot_performance(lasso_perf[lasso_perf$.metric %in% c("sens", "f_meas", "precision"),])

# ggsave(p_boost, filename = "fold_and_oos_xgb.pdf", height = 6, width = 13)
# ggsave(p_svm, filename = "fold_and_oos_svm.pdf", height = 6, width = 13)
ggsave(p_forest, filename = "fold_and_oos_forest_coral.pdf", height = 6, width = 13)
# ggsave(p_lasso, filename = "fold_and_oos_lasso.pdf", height = 6, width = 13)

# Now we can visually inspect model performance. We additionally test model performance statistically.
# Observations:
# (1) We would like to avoid missing relevant articles. Therefore Sensitivity is an important measure because it measures
# - the true positive rate: True positives divided by the sum of true positives and false negatives. 
# - a score close to one indicates that the model is likely to detect those with a given feature but it does not measure false positives
# (2) Precision is the rate of TP divided by the sum of TP and FP.
# (3) F1 Score is the harmonic mean of precision and sensitivity. The higher the F1 Score, the better are precision and Sensitivity

# predict CH data
ch_dat <- readRDS("CH_energy_SML_predict.Rds") %>% as_tibble()
ch_dat_debate <- readRDS("CH_energydebates_dataset_MAMW.rds") %>% as_tibble()
glimpse(ch_dat_debate)

# make sure the name of the text variable is the same as in the training data.
ch_dat <- ch_dat %>% 
  rename(text = Text)

# test if it works for the first model, i.e. the first DV
sm_predict(model_forest[[i]], ch_dat)

# function to predict all variables
predict_all_variables <- function(model_forest, ch_dat){
  # initialise vector
  predictions <- c()
  # loop through variables and make predictions
  for (i in 1:length(variables)){
    # bind predictions
    predictions <- rbind(predictions, t(sm_predict(model_forest[[i]], ch_dat))) 
    print(paste0("Predictions for variable '", variables[i], "' finsihed"))
  }
  # transpose and rename
  predictions <- t(predictions)
  colnames(predictions) <- variables
  # transform to numeric
  predictions <- as_tibble(predictions) %>% mutate_if(is.character, as.numeric)
  # output
  predictions
}

# apply the function
predictions_boost_tuned <- predict_all_variables(model_forest, ch_dat)

CH_predictions <- cbind(ch_dat, predictions_boost_tuned)

p_dist_perc_ch <- predictions_boost_tuned %>% 
  pivot_longer(., cols = everything()) %>% 
  mutate(name = factor(name, levels = rev(variables))) %>% 
  ggplot(., aes(name, (value)/sum(value))) +
  geom_col() +
  scale_y_continuous(labels = percent, limits = c(0, 0.15)) +
  labs(title = "Switzerland", y = "Percentage", x = "Concepts") +
  coord_flip() +
  theme_light()
p_dist_perc_ch

p_dist_perc_de_train <- abs %>% 
  dplyr::select(variables) %>% 
  mutate_all(as.character) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(., cols = everything()) %>% 
  mutate(name = factor(name, levels = rev(variables))) %>% 
  ggplot(., aes(name, (value)/sum(value))) +
  geom_col() +
  scale_y_continuous(labels = percent, limits = c(0, 0.15)) +
  labs(title = "Germany", y = "Percentage", x = "Concepts") +
  coord_flip() +
  theme_light()  
p_dist_perc_de_train

p_dist_perc_arr <- ggarrange(p_dist_perc_ch, p_dist_perc_de_train)
ggsave(p_dist_perc_arr, file = "Plots/distribution_percentage_arranged.pdf")

# reliability checks

CH_relcheck <- readRDS("CH_energy_SML_reliabilitycheck.rds") %>% as_tibble()

CH_relcheck <- CH_relcheck %>% 
  rename(text = Text)

predictions_relcheck <- predict_all_variables(model_forest, CH_relcheck)

ch_relcheck_predicted <- cbind(CH_relcheck, predictions_relcheck)

sum(ch_relcheck_predicted$Renewables == 1 & ch_relcheck_predicted$renewables == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$Solar == 1 & ch_relcheck_predicted$solar == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$Wind == 1 & ch_relcheck_predicted$wind == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$Bio == 1 & ch_relcheck_predicted$bio == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$Nuclear == 1 & ch_relcheck_predicted$nuclear == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Environmental Protection` == 1 & ch_relcheck_predicted$environmental_protection == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Security of Electricity Supply` == 1 & ch_relcheck_predicted$security_of_electricity_supply == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Avoidance of Dependencies` == 1 & ch_relcheck_predicted$avoidance_of_dependencies == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Costs of Electricity` == 1 & ch_relcheck_predicted$costs_of_electricity == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Cheaper RE Technology` == 1 & ch_relcheck_predicted$cheaper_re_technology == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Competitiveness of Industry` == 1 & ch_relcheck_predicted$competitiveness_of_industry == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Value Chain` == 1 & ch_relcheck_predicted$value_chain == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$Innovation == 1 & ch_relcheck_predicted$innovation == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$Employment == 1 & ch_relcheck_predicted$employment == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$Export == 1 & ch_relcheck_predicted$export == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Energy Democracy` == 1 & ch_relcheck_predicted$energy_democracy == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Climate Change Mitigation` == 1 & ch_relcheck_predicted$climate_change_mitigation == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Pollution Reduction` == 1 & ch_relcheck_predicted$pollution_reduction == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Sector Coupling` == 1 & ch_relcheck_predicted$sector_coupling == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Grid Extension` == 1 & ch_relcheck_predicted$grid_extension == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Energy Storage` == 1 & ch_relcheck_predicted$energy_storage == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Increase of Actor Variety` == 1 & ch_relcheck_predicted$increase_of_actor_variety == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Costs for Consumers` == 1 & ch_relcheck_predicted$costs_for_consumers == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Employment in RE Sector` == 1 & ch_relcheck_predicted$employment_in_re_sector == 1)/nrow(ch_relcheck_predicted)
sum(ch_relcheck_predicted$`Employment in Solar Industry` == 1 & ch_relcheck_predicted$employment_in_solar_industry == 1)/nrow(ch_relcheck_predicted)

test <- ch_relcheck_predicted %>% select(Renewables, renewables, `Value Chain`, value_chain)

  
  
