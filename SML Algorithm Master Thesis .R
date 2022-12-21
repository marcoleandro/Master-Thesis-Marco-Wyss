# SML Algorithm Master Thesis 

# ****************

# initialization process
# ----------------------

# load packages

# libs <- c("tidyverse","tm", "plyr", "class", "tidymodels", "dplyr", "ggplot2", "ggsci", "ggpubr", "baguette", "discrim", "finetune", "parsnip", "ranger", "ranger", "readxl", "reshape2", "rules", "spacyr", "stringr", "themis", "text2vec", "textrecipes", "tidytext", "tidyposterior", "tmap", "tune", "workflowsets", "kernlab", "writexl", "countrycode" )
# lapply(libs, require, character.only = TRUE)

rm(list = ls())

# install.packages("pacman")
# pacman::p_load(dplyr)
# pacman::p_load(ggplot2)
# pacman::p_load(ggsci)
# pacman::p_load(ggpubr)
# pacman::p_load(baguette)
# pacman::p_load(discrim)
# pacman::p_load(stringr)
# pacman::p_load(parsnip)
# pacman::p_load(readxl)
# pacman::p_load(reshape2)
# install.packages("rules", Ncpus = 6)
# install.packages("spacyr", Ncpus = 6)
install.packages("finetune", Ncpus = 6)
Yes
# install.packages("ranger", Ncpus = 6)
Yes
install.packages("themis", Ncpus = 6)
Yes
install.packages("tidymodels", Ncpus = 6)
Yes
install.packages("text2vec", Ncpus = 6)
Yes
install.packages("textrecipes", Ncpus = 6)
Yes
# install.packages("tidytext", Ncpus = 6)
Yes
install.packages("tidyposterior", Ncpus = 6)
Yes
# install.packages("tmap", Ncpus = 6)
Yes
install.packages("tune", Ncpus = 6)
Yes
install.packages("workflowsets", Ncpus = 6)
Yes
install.packages("kernlab", Ncpus = 6)
Yes
# pacman::p_load(writexl)
# pacman::p_load(countrycode)

library(dplyr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(baguette)
library(discrim)
library(tidymodels)
library(finetune)
library(parsnip)
library(ranger)
library(readxl)
library(reshape2)
library(rules)
library(spacyr)
library(stringr)
library(themis)
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


# load files / create data

abs <- read_excel("EEG_Daten_clean.xlsx", 
                       col_types = c("numeric", "date", "numeric", 
                                     "text", "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

# abs <- abs %>%
#  mutate(id = 1:nrow(.)) %>% 
#  filter(
#    !is.na(Abstract) &
#      !is.na(junk) &
#      !is.na(feasibility) & !is.na(policy_evaluation) & !is.na(climate) & !is.na(environment) & !is.na(mitigation) & !is.na(adaptation) 
#    & !is.na(energy) & !is.na(transport) 
#    & !is.na(buildings) & !is.na(industry) 
#    # & !is.na(waste) 
#    & !is.na(afolu) & !is.na(economic_cost) & !is.na(distributional_dynamics) 
#    & !is.na(institutional_capacity) & !is.na(multilevel)
#  ) %>% 
#  mutate_at(variables, factor)

variables <- c(
  "Renewables" 
  # "Solar", "Water", "Wind", "Geo", "Bio", "Efficiency", "Nuclear", # technologies
  # "Environmental Protection", "Security of Electricity Supply", "Avoidance of Dependencies", 
  # "Costs of Electricity", "Cheaper RE Technology", "Competitiveness of Industry", "Value Chain", 
  # "Innovation", "Employment", "Export", "Energy Democracy", # goals
  # "Climate Change Mitigation", "Pollution Reduction", "Mitigation of Nuclear Risk", 
  # "Substitution of Limited Resources", "Sector Coupling", "Grid Extension", 
  # "Energy Storage", "Increase of Actor Variety", "Costs for Consumers", "Costs for Households", 
  # "Costs for Industry", "Employment in RE Sector", "Employment not related to RE Sector", "Employment in Solar Industry"
) # objectives

# set options

options(stringsAsFactors = FALSE)

# create a custom theme for the layout

theme_MT <- function () { 
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
    select(statement.ID, Text, {{outcome}}) %>% 
    recipe() %>% 
    update_role(Text = "predictor") %>%
    update_role({{outcome}}, new_role = "outcome") %>%
    step_tokenize(Text, token = "word_stems") %>%
    # step_tokenize(Abstract, Article.Title, Author.Keywords, Keywords.Plus, engine = "spacyr") %>%
    # step_lemma(Abstract, Article.Title, Author.Keywords, Keywords.Plus) %>%
    step_stopwords(Text, language = "en", keep = FALSE, stopword_source = "snowball") %>%
    step_tokenfilter(Text, max_tokens = 1e3) %>%
    step_tfidf(Text)  %>%
    step_smote({{outcome}}) 
}

rec <- lapply(variables, my_rec)


# function to capitalise first letter in a string
simple_cap <- function(x) {
  
  s <- strsplit(x, " ")[[1]]
  
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
  
}

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

sm_predict <- function(final_model){
  
  # convert list to df
  fitted_wf <- pluck(final_model$.workflow, 1)
  
  predict(fitted_wf, new_data = abstract)
}

res_boost <- list()
res_svm <- list()
res_forest <- list()
res_lasso <- list()
model_boost <- list()
model_svm <- list()
model_forest <- list()
model_lasso <- list()
# For installation follow steps in https://cran.r-project.org/web/packages/spacyr/readme/README.html 
# spacyr::spacy_initialize(model = "en_core_web_sm", entity = F)
# for (i in 1:length(rec)){
#   res_boost[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("xgboost")), sm_spec("xgboost"))
#   model_boost[[i]] <- sm_fit(res_boost[[i]], sm_wf(rec[[i]], sm_spec("xgboost")))
#   print(paste("XGBoost model iteration", i, "complete"))
# }
# for (i in 1:length(rec)){
#   res_svm[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("svm")), sm_spec("svm"))
#   model_svm[[i]] <- sm_fit(res_svm[[i]], sm_wf(rec[[i]], sm_spec("svm")))
#   print(paste("Support Vector Machine model iteration", i, "complete"))
# }
for (i in 1:length(rec)){
  res_forest[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("forest")), sm_spec("forest"))
  model_forest[[i]] <- sm_fit(res_forest[[i]], sm_wf(rec[[i]], sm_spec("forest")))
  print(paste("Forest model iteration", i, "complete"))
}
# for (i in 1:length(rec)){
#   res_lasso[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec("lasso")), sm_spec("lasso"))
#   model_lasso[[i]] <- sm_fit(res_lasso[[i]], sm_wf(rec[[i]], sm_spec("lasso")))
#   print(paste("Lasso model iteration", i, "complete"))
# }

###### 
## plot the result
###### 

var_names <- c("Renewables", "Solar", "Water", "Wind", "Geo", "Bio", "Efficiency", "Nuclear", # technologies
               "Environmental Protection", "Security of Electricity Supply", "Avoidance of Dependencies", 
               "Costs of Electricity", "Cheaper RE Technology", "Competitiveness of Industry", "Value Chain", 
               "Innovation", "Employment", "Export", "Energy Democracy", # goals
               "Climate Change Mitigation", "Pollution Reduction", "Mitigation of Nuclear Risk", 
               "Substitution of Limited Resources", "Sector Coupling", "Grid Extension", 
               "Energy Storage", "Increase of Actor Variety", "Costs for Consumers", "Costs for Households", 
               "Costs for Industry", "Employment in RE Sector", "Employment not related to RE Sector", "Employment in Solar Industry") # objectives
technologies <- c("Renewables", "Solar", "Water", "Wind", "Geo", "Bio", "Efficiency", "Nuclear")
goals <- c("Environmental Protection", "Security of Electricity Supply", "Avoidance of Dependencies", 
           "Costs of Electricity", "Cheaper RE Technology", "Competitiveness of Industry", "Value Chain", 
           "Innovation", "Employment", "Export", "Energy Democracy")
objectives <- c("Climate Change Mitigation", "Pollution Reduction", "Mitigation of Nuclear Risk", 
                "Substitution of Limited Resources", "Sector Coupling", "Grid Extension", 
                "Energy Storage", "Increase of Actor Variety", "Costs for Consumers", "Costs for Households", 
                "Costs for Industry", "Employment in RE Sector", "Employment not related to RE Sector", "Employment in Solar Industry")

evaluate_model_performance <- function(res_boost, model_boost){
  # create df with performance on each cross-fold validation in the training data
  fold_perf_df <- do.call(rbind, res_boost)
  fold_perf_df <- do.call(rbind, fold_perf_df$.metrics)
  fold_perf_df$dv <- factor(rep(var_names, each = nrow(res_boost[[1]]$.metrics[[1]])*10), levels = rev(var_names))
  
  # create df without of sample performance 
  out_of_sample_df <- do.call(rbind, model_boost)
  out_of_sample_df <- do.call(rbind, out_of_sample_df$.metrics)
  out_of_sample_df$dv <- factor(rep(var_names, each = length(unique(out_of_sample_df$.metric))), levels = rev(var_names))
  
  best_in_sample <- lapply(res_boost, show_best, "f_meas", n = 1)
  best_in_sample <- do.call(rbind, best_in_sample)
  best_in_sample$dv <- var_names
  best_in_sample <- best_in_sample[, c(".metric", "mean", "dv")]
  colnames(best_in_sample)[2] <- "best_in_sample_performance"
  
  # calculate CI for cross-fold training data
  fold_perf_df <- fold_perf_df %>% 
    group_by(dv, .metric) %>% 
    dplyr::summarise(mean = mean(.estimate, na.rm = TRUE), ci_lo = quantile(.estimate, 0.05), ci_hi = quantile(.estimate, 0.95))
  
  # join them together
  performance <- left_join(fold_perf_df, out_of_sample_df, by = c("dv", ".metric"))
  performance <- left_join(performance, best_in_sample, by = c("dv", ".metric"))
  performance$feature <- factor(c(rep("Technologies", each = length(unique(performance$.metric))*length(barriers)), 
                                  rep("Goals", each = length(unique(performance$.metric))*length(sectors)),
                                  rep("Objectives", each = length(unique(performance$.metric))*length(topics))))
  performance
}

plot_performance <- function(performance){
  ggplot(performance) +
    geom_pointrange(aes(y = mean, x = dv, ymin = ci_lo, ymax = ci_hi, col = "10-fold cross-validation"), fatten = .1) +
    geom_point(position = position_dodge(width = 2), aes(y = .estimate, x = dv, col = "out-of-sample validation"), fatten = 2, shape = 2) +
    # geom_point(position = position_dodge(width = 2), aes(y = best_in_sample_performance, x = dv, col = "best performing model within the sample"), fatten = 2, shape = 3) +
    scale_x_discrete("Classified Article Features") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    coord_flip() +
    facet_grid(feature~.metric, scales = "free_y", labeller = labeller(.metric = c("accuracy" = "Accuracy", "f_meas" = "F1 score", "precision" = "Precision",
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
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .8, ymax = .9), alpha = .01, fill = "lightgreen") +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .9, ymax = 1), alpha = .01, fill = "green") +
    # guides(color=guide_legend(title = "Legend", override.aes=list(shape=c(1,2,3), linetype = c(1,0,0))))
    guides(color=guide_legend(title = "Legend", override.aes=list(shape=c(1,2), linetype = c(1,0))))
}

boost_perf <- evaluate_model_performance(res_boost, model_boost)
svm_perf <- evaluate_model_performance(res_svm, model_svm)
forest_perf <- evaluate_model_performance(res_forest, model_forest)
lasso_perf <- evaluate_model_performance(res_lasso, model_lasso)

p_boost <- plot_performance(boost_perf[boost_perf$.metric %in% c("sens", "f_meas", "precision"),])
p_svm <- plot_performance(svm_perf[svm_perf$.metric %in% c("sens", "f_meas", "precision"),])
p_forest <- plot_performance(forest_perf[forest_perf$.metric %in% c("sens", "f_meas", "precision"),])
p_lasso <- plot_performance(lasso_perf[lasso_perf$.metric %in% c("sens", "f_meas", "precision"),])

ggsave(p_boost, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/fold_and_oos_xgb.pdf", height = 6, width = 13)
ggsave(p_svm, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/fold_and_oos_svm.pdf", height = 6, width = 13)
ggsave(p_forest, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/fold_and_oos_forest.pdf", height = 6, width = 13)
ggsave(p_lasso, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/fold_and_oos_lasso.pdf", height = 6, width = 13)

# Now we can visually inspect model performance. We additionally test model performance statistically.
# Observations:
# (1) We would like to avoid missing relevant articles. Therefore Sensitivity is an important measure because it measures
# - the true positive rate: True positives divided by the sum of true positives and false negatives. 
# - a score close to one indicates that the model is likely to detect those with a given feature but it does not measure false positives
# (2) Precision is the rate of TP divided by the sum of TP and FP.
# (3) F1 Score is the harmonic mean of precision and sensitivity. The higher the F1 Score, the better are precision and Sensitivity

###### 
## Model selection
###### 

# Create a tibble of the model results 
model_res <- tibble(model = append(append(append(res_boost, res_forest), res_lasso), res_svm),
                    model_name = c(rep("XGBoost", length(var_names)), rep("Forest", length(var_names)), rep("Lasso", length(var_names)), rep("SVM", length(var_names))),
                    dv = rep(var_names, 4))

# Create a helper function for collecting the metrics 
map_collect_metrics <- function(model){
  model %>% 
    select(id, .metrics) %>% 
    unnest()
}

# Apply helper function and extract the metrics
model_res <- model_res %>%
  mutate(res = map(model, map_collect_metrics)) %>%
  select(model_name, dv, res) %>%
  unnest(res, dv)

p_comare_mods <- model_res %>%
  mutate(dv_model_name = paste(dv, model_name)) %>%
  ggplot(aes(x = model_name, y = .estimate)) +
  geom_boxplot() +
  theme_SM() +
  theme(axis.line=element_line()) +
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  coord_flip() +
  # ylim(0, 1) +
  # stat_compare_means(comparisons = list(c("Lasso", "Forest"), c("XGBoost", "Forest"), c("Lasso", "XGBoost")), method = "t.test", paired = F, label = "p.format") +
  facet_wrap(~.metric, scales = "free_y", 
             labeller = labeller(.metric = c("accuracy" = "Accuracy", "f_meas" = "F1 score", "precision" = "Precision",
                                             "roc_auc" = "ROC", "sens" = "Recall", "spec" = "Specificity"))) +
  coord_cartesian(clip = "on", ylim = c(0, 1.3), expand = c(2, 10, 10, 10)) +
  theme(legend.position = c(.5,1.5),
        legend.direction = "horizontal",
        panel.spacing = unit(2, "lines"))
p_comare_mods
ggsave(p_comare_mods, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/comparemods.pdf", height = 7, width = 12)

# make predictions on unseen data for untuned models to investigate the distribution and plausibility of the predictions
abstract <- readxl::read_xlsx("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/all_articles_new_search_string_clean.xlsx", sheet = 1)
sum(duplicated(abstract$Article.Title)) # 11 duplicates
abstract <- abstract[!duplicated(abstract$Article.Title),]
sum(is.na(abstract$Abstract)) # 2047 NAs
abstract <- abstract[!is.na(abstract$Abstract),]

predict_all_variables <- function(model_boost_tuned){
  # initialise vector
  predictions <- c()
  # loop through variables and make predictions
  for (i in 1:length(var_names)){
    # bind predictions
    predictions <- rbind(predictions, t(sm_predict(model_boost_tuned[[i]]))) 
  }
  # transpose and rename
  predictions <- t(predictions)
  colnames(predictions) <- paste0(variables, "_pred")
  # transform to numeric
  predictions <- as_tibble(predictions) %>% mutate_if(is.character, as.numeric)
  # output
  predictions
}

# summarise the share of predictions on the entire data set and the share of coded article features in the training data. 
# We'd expect these to be similar
plot_predicted_and_coded_share <- function(predictions){
  # bind predictions to the main data and merge the coded data by Author names
  abstract1 <- left_join(bind_cols(predictions, abstract), abs %>% dplyr::select("Article.Title", variables), by = "Article.Title")
  
  plot <- abstract1 %>% 
    mutate_at(variables, as.character) %>% 
    mutate_at(variables, as.numeric) %>% 
    summarise_at(c(paste0(variables, "_pred"), variables), mean, na.rm = TRUE) %>%
    as.data.frame() %>% 
    pivot_longer(.,c(paste0(variables, "_pred"), variables), values_to = "value") %>% 
    mutate(`Data Set` = as.character(ifelse(grepl("_pred", name), "Predicted", "Coded")),
           name = gsub("_pred", "", name),
           name = str_to_title(gsub("_", " ", name)),
           name = ifelse(name == "Afolu", "AFOLU", name),
           name = ifelse(name == "Multilevel", "Multi-Level Governance", name),
           name = factor(name, levels = var_names)
    ) %>% 
    arrange(name) %>% 
    mutate(group = factor(c(rep("Topics", 12), rep("Sectors", 10), rep("Barriers", 8)), levels = c("Topics", "Sectors", "Barriers"))) %>% 
    arrange(name, group) %>%
    ggplot(aes(name, value, fill = `Data Set`, width = 0.5)) +
    geom_col(position = position_dodge(width = .6), alpha = .8) +
    xlab("Article Feature") +  ylab("Share of Articles") +
    facet_grid(~ group, scales = "free_x", space = "free_x") + 
    theme_SM() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
    scale_fill_npg()
  
  plot
}

# predict with each model
predictions_boost <- predict_all_variables(model_boost)
predictions_forest <- predict_all_variables(model_forest)
predictions_svm <- predict_all_variables(model_svm)
predictions_lasso <- predict_all_variables(model_lasso)

# plot the share of predicted and coded variables fo each model
share_boost <- plot_predicted_and_coded_share(predictions_boost)
share_forest <- plot_predicted_and_coded_share(predictions_forest)
share_svm <- plot_predicted_and_coded_share(predictions_svm)
share_lasso <- plot_predicted_and_coded_share(predictions_lasso)

# save the plots
ggsave(share_boost, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/scripts/Share_XGBoost.pdf", height = 5, width = 10)
ggsave(share_forest, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/Share_Forest.pdf", height = 5, width = 10)
ggsave(share_svm, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/Share_SVM.pdf", height = 5, width = 10)
ggsave(share_lasso, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/Share_Lasso.pdf", height = 5, width = 10)

###### 
## tune XGBoost model
###### 

res_boost_tuned <- list()
model_boost_tuned <- list()

for (i in 1:length(rec)){
  res_boost_tuned[[i]] <- sm_train(sm_wf(rec[[i]], sm_spec_tuned("xgboost")), sm_spec_tuned("xgboost"))
  model_boost_tuned[[i]] <- sm_fit(res_boost_tuned[[i]], sm_wf(rec[[i]], sm_spec_tuned("xgboost")))
  print(paste("XGBoost tuned model iteration", i, "complete"))
}

### 18.01.22
## fitted on recall
# save.image("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/tunedXGBoost.RData")
# load("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/tunedXGBoost.RData")
## fitted on F1
# save.image("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/tunedXGBoost_F1.RData")
# load("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/tunedXGBoost_F1.RData")
# res_boost_tuned <- res_boost_tuned[-11]
# model_boost_tuned <- model_boost_tuned[-11]
## fitted entire model: without waste on F1, load 'variables' object without waste for it work
# save.image("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/tunedXGBoost_and_untuned.RData")
# load("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/tunedXGBoost_and_untuned.RData")

boost_perf_tuned <- evaluate_model_performance(res_boost_tuned, model_boost_tuned)
p_boost_tuned <- plot_performance(boost_perf_tuned[boost_perf_tuned$.metric %in% c("sens", "f_meas", "precision"),])
ggsave(p_boost_tuned, filename = "/Users/marcowyss/github/Master-Thesis-Marco-Wyss/model_renewables/fold_and_oos_xgb_tuned.pdf", height = 6, width = 13)

res_boost_tuned[[1]] %>%
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  select(mean, min_n:loss_reduction) %>%
  pivot_longer(min_n:loss_reduction,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "F1 Score") +
  theme_SM()

###### 
## make predictions with tuned model
###### 

# predictions_boost_tuned <- predict_all_variables(model_boost_tuned)
# share_boost_tuned <- plot_predicted_and_coded_share(predictions_boost_tuned)
# 
# # join predictions to the entire data
# abstract1 <- left_join(bind_cols(predictions_boost_tuned, abstract), abs %>% dplyr::select("Article.Title", variables), by = "Article.Title")
# abstract1 <- left_join(abstract1, impact_factor %>% mutate(SJR = as.numeric(gsub(",", ".", SJR))) %>% distinct(Title_wo_stopwords, .keep_all = TRUE), by = c("Source.Title_wo_stopwords" = "Title_wo_stopwords"))
# 
# abstract1 <- abstract1 %>% 
#   mutate_at(variables, as.character, as.numeric) %>% # transform to numeric
#   mutate_at(paste0(variables, "_pred"), as.numeric) %>% # transform to numeric
#   mutate(
#     feasibility_pred = ifelse(feasibility_pred != feasibility & !is.na(feasibility), feasibility, feasibility_pred),
#     policy_evaluation_pred = ifelse(policy_evaluation_pred !=  policy_evaluation & !is.na(policy_evaluation), policy_evaluation, policy_evaluation_pred),
#     climate_pred = ifelse(climate_pred != climate & !is.na(climate), climate, climate_pred),
#     environment_pred = ifelse(environment_pred != environment & !is.na(environment), environment, environment_pred),
#     mitigation_pred = ifelse(mitigation_pred != mitigation & !is.na(mitigation), mitigation, mitigation_pred),
#     adaptation_pred = ifelse(adaptation_pred != adaptation & !is.na(adaptation), adaptation, adaptation_pred),
#     energy_pred = ifelse(energy_pred != energy & !is.na(energy), energy, energy_pred),
#     transport_pred = ifelse(transport_pred != transport & !is.na(transport), transport, transport_pred),
#     buildings_pred = ifelse(buildings_pred != buildings & !is.na(buildings), buildings, buildings_pred),
#     industry_pred = ifelse(industry_pred != industry & !is.na(industry), industry, industry_pred),
#     afolu_pred = ifelse(afolu_pred != afolu & !is.na(afolu), afolu, afolu_pred),
#     economic_cost_pred = ifelse(economic_cost_pred != economic_cost & !is.na(economic_cost), economic_cost, economic_cost_pred),
#     distributional_dynamics_pred = ifelse(distributional_dynamics_pred != distributional_dynamics & !is.na(distributional_dynamics), distributional_dynamics, distributional_dynamics_pred),
#     institutional_capacity_pred = ifelse(institutional_capacity_pred != institutional_capacity & !is.na(institutional_capacity), institutional_capacity, institutional_capacity_pred),
#     multilevel_pred = ifelse(multilevel_pred != multilevel & !is.na(multilevel), multilevel, multilevel_pred)
#   ) %>% 
#   mutate_at(paste0(variables, "_pred"), as.numeric) # transform back to numeric

# function to select articles by barrier
select_articles <- function(variable, format){
  abstract1 %>% 
    dplyr::arrange(-feasibility_pred, -mitigation_pred, -{{variable}}, -SJR, -Times.Cited..All.Databases) %>% 
    dplyr::mutate(qual = ifelse(feasibility_pred == 1 & {{variable}} == 1 & mitigation_pred == 1 & grepl(format, Document.Type), 1, 0),
                  id_sample = 1:nrow(.)) %>% 
    dplyr::filter(qual == 1)
}
econ_qual <- select_articles(economic_cost_pred, "Article")
dist_qual <- select_articles(distributional_dynamics_pred, "Article")
inst_qual <- select_articles(institutional_capacity_pred, "Article")
mult_qual <- select_articles(multilevel_pred, "Article")

# write_xlsx(econ_qual, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_econ_200_2.xlsx")
# write_xlsx(dist_qual, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_dist_200_2.xlsx")
# write_xlsx(inst_qual, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_inst_200_2.xlsx")
# write_xlsx(mult_qual, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_mult_200_2.xlsx")

abstract1 %>% 
  arrange(-feasibility_pred, -mitigation_pred, -SJR, -Times.Cited..All.Databases) %>% 
  mutate(qual = ifelse(feasibility_pred == 1 & mitigation_pred == 1 & grepl("Article", Document.Type) &
                         (economic_cost_pred == 1 | distributional_dynamics_pred == 1 | institutional_capacity_pred == 1 | multilevel_pred == 1), 1, 0)) %>% 
  filter(qual == 1) 

set.seed(1)
reliability_econ <- econ_qual[econ_qual$id_sample %in% sample(econ_qual$id_sample[1:100], size = 20),]
reliability_dist <- dist_qual[dist_qual$id_sample %in% sample(dist_qual$id_sample[1:100], size = 20),]
reliability_inst <- inst_qual[inst_qual$id_sample %in% sample(inst_qual$id_sample[1:100], size = 20),]
reliability_mult <- mult_qual[mult_qual$id_sample %in% sample(mult_qual$id_sample[1:100], size = 20),]

# write_xlsx(reliability_econ, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_econ_reliability.xlsx")
# write_xlsx(reliability_dist, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_dist_reliability.xlsx")
# write_xlsx(reliability_inst, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_inst_reliability.xlsx")
# write_xlsx(reliability_mult, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/Abstracts/in_depth_qualitative_XGBoost_mult_reliability.xlsx")

# ########################### start plotting ###############
emissions_lamb <- read_xlsx("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/emissions/lamb et al. supplementary/erlabee4esupp1_clean.xlsx")

continents_lamb <- read_xlsx("/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/emissions/country_list_clean.xlsx") 

data("World")
world <- left_join(World, continents_lamb, by = "iso_a3") 

# plot which countries these are
countries_per_region <- world %>% 
  tm_shape() +
  tm_polygons(col = "ar6_continent", legend.is.portrait = F, title = "Continent" ) + 
  tm_layout(frame = F, 
            legend.position = c("center", "top"), 
            legend.outside.position = "bottom",
            legend.outside = T, 
            legend.stack = "horizontal", 
            legend.text.size = .55, 
            legend.title.size = 1,
            outer.margins=c(-.18,-.1,-.01,-.1)) 
tmap_save(countries_per_region, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/countries_per_region.pdf", height = 5, width = 10)

## how many times is a country mentioned per abstract?
# subset to 2018
abstr_2018 <- abstract1 %>% 
  mutate_at(c("energy", "transport", "buildings", "industry", "afolu"), as.numeric) %>% 
  filter(Publication.Year %in% c(2018:2010), mitigation_pred == 1) %>% 
  filter(SJR > summary(SJR)[3])
country_in_abstract <- sapply(World$name, grepl, abstr_2018$Abstract)
colnames(country_in_abstract) <- World$name

research_mat <- as.data.frame(cbind(colSums(country_in_abstract * abstr_2018$energy_pred),
                                    colSums(country_in_abstract * abstr_2018$transport_pred), 
                                    colSums(country_in_abstract * abstr_2018$buildings_pred), 
                                    colSums(country_in_abstract * abstr_2018$industry_pred), 
                                    colSums(country_in_abstract * abstr_2018$afolu_pred)))
colnames(research_mat) <- c("energy", "transport", "buildings", "industry", "afolu")
research_mat$country <- rownames(research_mat)

research_mat <- research_mat %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(iso_a3 = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  dplyr::left_join(., world, by = "iso_a3") %>% 
  dplyr::group_by(ar6_continent) %>% 
  dplyr::summarise(energy = sum(energy, na.rm = T),
                   transport = sum(transport, na.rm = T),
                   buildings = sum(buildings, na.rm = T),
                   industry = sum(industry, na.rm = T),
                   afolu = sum(afolu, na.rm = T))

research_mat <- rbind(research_mat, c("World", colSums(research_mat[,2:6]))) %>% 
  dplyr::mutate_at(c("energy", "transport", "buildings", "industry", "afolu"), as.numeric)

re <- research_mat %>% 
  dplyr::group_by(ar6_continent) %>% 
  dplyr::mutate(total = sum(energy, transport, buildings, industry, afolu)) %>% 
  dplyr::mutate_at(vars(energy, transport, buildings, industry, afolu), funs(./total)) %>% 
  dplyr::filter(!is.na(ar6_continent)) %>%
  dplyr::select(-total)   

emission_mat <- emissions_lamb %>%
  dplyr::mutate(`2011` = `2010` + (`2018`- `2010`)/8,
                `2012` = `2010` + (`2018`- `2010`)/8*2,
                `2013` = `2010` + (`2018`- `2010`)/8*3,
                `2014` = `2010` + (`2018`- `2010`)/8*4,
                `2015` = `2010` + (`2018`- `2010`)/8*5,
                `2016` = `2010` + (`2018`- `2010`)/8*6,
                `2017` = `2010` + (`2018`- `2010`)/8*7) %>% 
  dplyr::left_join(., world, by =  c("region_ar6_10" = "ar6_continent")) %>% 
  dplyr::group_by(region_ar6_10, subsector_title) %>%
  dplyr::summarise(emissions = sum(`2010`, `2011`, `2012`, `2013`,  `2014`, `2015`, `2016`, `2017`, `2018`, na.rm = T)) %>% 
  dplyr::mutate(sector = subsector_title,
                sector = ifelse(subsector_title == "Energy systems", "energy", sector),
                sector = ifelse(subsector_title == "Transport", "transport", sector),
                sector = ifelse(subsector_title == "Buildings", "buildings", sector),
                sector = ifelse(subsector_title == "Industry", "industry", sector),
                sector = ifelse(subsector_title == "AFOLU", "afolu", sector)) %>% 
  dplyr::select(-subsector_title) %>% 
  dplyr::pivot_wider(names_from = "sector", values_from = "emissions")

emission_mat <- rbind(as.matrix(emission_mat), c("World", colSums(emission_mat[,2:6]))) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate_at(c("energy", "transport", "buildings", "industry", "afolu"), as.numeric)

em <- emission_mat %>% 
  dplyr::group_by(region_ar6_10) %>% 
  dplyr::mutate(total = sum(energy, transport, buildings, industry, afolu)) %>% 
  dplyr::mutate_at(vars(energy, transport, buildings, industry, afolu), funs(./total)) %>% 
  dplyr::select(-total)

gap <- em[,2:6] - re[,2:6]
gap$region_ar6_10 <- em$region_ar6_10

p_emissions_by_continent <- left_join(em, re, by =c("region_ar6_10" = "ar6_continent")) %>% 
  pivot_longer(cols = c("energy.x", "transport.x", "buildings.x", "industry.x", "afolu.x", "energy.y", "transport.y", "buildings.y", "industry.y", "afolu.y")) %>% 
  mutate(Share = ifelse(str_detect(name, ".x"), "Emissions", "Research"),
         name = gsub(".x", "", name, fixed = TRUE),
         name = gsub(".y", "", name, fixed = TRUE),
         name = str_to_title(name),
         name = ifelse(name == "Afolu", "AFOLU", name)
  ) %>% 
  mutate(name = factor(name, levels = c("Energy", "Transport", "Buildings", "Industry", "AFOLU"))) %>%
  ggplot(aes(x = region_ar6_10, y = value)) +
  geom_point(size=2, aes(colour = Share)) + 
  geom_line(arrow = arrow(length=unit(0.15,"cm"), ends="first", type = "closed")) +
  facet_wrap("name", nrow = 1) + ylab("Emission and Research Share") + xlab("Regions") +
  scale_fill_npg() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        legend.position = "bottom",
        strip.text.x = element_text(colour = 'black', size = 10)
  ) 
p_emissions_by_continent
ggsave(p_emissions_by_continent, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/emissions_by_continent_above_med.pdf", height = 7, width = 12)

# function to extract c
summarise_sectors <- function(economic_cost_pred, feasibility_pred){
  abstract1 %>% 
    ungroup() %>% 
    filter({{economic_cost_pred}} == 1 & {{feasibility_pred}} == 1 & mitigation_pred == 1) %>% 
    dplyr::summarise(energy_pred = sum(energy_pred, na.rm = T),
                     transport_pred = sum(transport_pred, na.rm = T),
                     buildings_pred = sum(buildings_pred, na.rm = T),
                     industry_pred = sum(industry_pred, na.rm = T),
                     afolu_pred = sum(afolu_pred, na.rm = T)
    )
}

mech_by_sec <- rbind(summarise_sectors(economic_cost_pred, feasibility_pred),
                     summarise_sectors(distributional_dynamics_pred, feasibility_pred),
                     summarise_sectors(institutional_capacity_pred, feasibility_pred),
                     summarise_sectors(multilevel_pred, feasibility_pred),
                     summarise_sectors(economic_cost_pred, policy_evaluation_pred),
                     summarise_sectors(distributional_dynamics_pred, policy_evaluation_pred),
                     summarise_sectors(institutional_capacity_pred, policy_evaluation_pred),
                     summarise_sectors(multilevel_pred, policy_evaluation_pred)
)
mech_by_sec$var <- rep(c("Economic Cost", "Distributional Dynamics", "Institutional Capacity", "Multi-Level Governance"), 2)
mech_by_sec$var_group <- c(rep("Policy Feasibility", 4), rep("Policy Evaluation", 4))

# extract percentages 
mech_by_sec<- mech_by_sec %>%
  reshape2::melt(., id.vars = c("var", "var_group"), measure.vars = c("energy_pred", "transport_pred", "buildings_pred", "industry_pred", "afolu_pred")) %>% 
  mutate(var = factor(var, levels = rev(c("Economic Cost", "Distributional Dynamics", "Institutional Capacity", "Multi-Level Governance"))),
         var_group = factor(var_group, levels = c("Policy Feasibility", "Policy Evaluation"))) %>% 
  group_by(var, var_group) %>% 
  dplyr::mutate(sum_var = sum(value)) %>% 
  ungroup() %>% 
  dplyr::mutate(pct = round(value/sum_var*100,0)) %>% 
  arrange(var) %>% 
  mutate(var = factor(var, levels = rev(c("Distributional Dynamics", "Economic Cost", "Institutional Capacity", "Multi-Level Governance"))))

p7 <- mech_by_sec %>% 
  filter(value != 0) %>%
  mutate(variable = forcats::fct_rev(variable)) %>% 
  ggplot(., aes(x = var, y = value, fill = variable)) + 
  geom_bar(position = "fill", stat = "identity", width = .9) +
  theme_SM() +
  theme(legend.position = "bottom") +
  scale_fill_npg(name = "Barriers", labels = c("Energy", "Transport", "Buildings", "Industry", "AFOLU"), limits = rev) +
  geom_text(aes(label = paste0(ifelse(pct>3,pct,""), ifelse(pct>3,"%","")), x = var, y = pct/100),  color = "black", position = position_stack(vjust = .5)) + 
  ylab("Share of Barriers") + xlab("") +
  theme(legend.position = "bottom") +
  coord_flip() +
  facet_wrap("var_group", nrow = 1)
p7
ggsave(p7, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/Fig_7_2.pdf", height = 5, width = 12)

p8 <- mech_by_sec %>% 
  mutate(pct = ifelse(var %in% c("Waste", "Industry", "Buildings", "Transport") & variable %in% c("institutional_capacity_pred", "multilevel_pred"), "", pct)) %>% 
  ggplot(., aes(x = var, y = value, fill = variable)) + 
  geom_bar(position = "stack", stat = "identity") +
  # geom_point(aes(y = total_number, color = "black"), size = 6, shape = 124) +
  theme_SM() +
  theme(legend.position = "bottom") +
  scale_fill_npg(name = "Sectors",
                 labels = c("Energy", "Transport", "Buildings", "Industry", "AFOLU"))+
  geom_text(aes(label = ifelse(pct != "", paste0(pct, "%"), ""), x = var, y = value),  color = "black", cex = 2, position = position_stack(vjust = .5)) + 
  # geom_text(aes(label = total_number, x = var, y = total_number),  color = "black", cex = 2, hjust = -0.5) + 
  ylab("Number") + xlab("Article Features") +
  coord_flip() +
  facet_grid("var_group", scales = "free", space = "free") +
  scale_colour_manual("", values = "black", label = "Total Number of Articles")
p8
ggsave(p8, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/Fig_8_2.pdf", height = 5, width = 12)

p9 <- abstract1 %>% 
  group_by(Publication.Year) %>% 
  summarise_at(c("energy_pred", "transport_pred", "buildings_pred", "industry_pred", "afolu_pred"), sum) %>% 
  pivot_longer(., c("energy_pred", "transport_pred", "buildings_pred", "industry_pred", "afolu_pred"), names_to = "sectors") %>% 
  mutate(sectors = factor(sectors, levels = c("energy_pred", "transport_pred", "buildings_pred", "industry_pred", "waste_pred", "afolu_pred"))) %>% 
  filter(Publication.Year < 2021) %>% 
  ggplot(aes(x = Publication.Year, y = value, fill = sectors)) +
  geom_col() +
  theme_SM() +
  ylab("Number of Sectors") +   xlab("Publication Year") +
  theme(legend.position = "bottom") +
  scale_fill_npg(name = "Sectors", labels = c("Energy", "Transport", "Buildings", "Industry", "Waste", "AFOLU"))
p9
ggsave(p9, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/Publications_and_Sector_over_Time.pdf", height = 5, width = 10)

p10 <- abstract1 %>% 
  group_by(Publication.Year) %>% 
  summarise_at(c("economic_cost_pred", "distributional_dynamics_pred", "institutional_capacity_pred", "multilevel_pred"), sum) %>% 
  pivot_longer(., c("economic_cost_pred", "distributional_dynamics_pred", "institutional_capacity_pred", "multilevel_pred"), names_to = "barriers") %>% 
  mutate(barriers = factor(barriers, levels = c("economic_cost_pred", "distributional_dynamics_pred", "institutional_capacity_pred", "multilevel_pred"))) %>% 
  filter(Publication.Year < 2021) %>% 
  ggplot(aes(x = Publication.Year, y = value, fill = barriers)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_SM() +
  ylab("Number of Barriers") +   xlab("Publication Year") +
  theme(legend.position = "bottom") + 
  scale_fill_npg(name = "Barriers", labels =c("Economic Cost", "Distributional Dynamics", "Institutional Capacity", "Multi-Level Governance")) 
p10
ggsave(p10, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/Publications_and_Barriers_over_Time.pdf", height = 5, width = 10)

p12 <- abstract1 %>% 
  group_by(Journal.Abbreviation) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  dplyr::slice(1:100) %>% 
  mutate(Journal.Abbreviation = factor(Journal.Abbreviation, levels = Journal.Abbreviation)) %>% 
  ggplot(aes(x = Journal.Abbreviation, y = count)) +
  geom_col() +
  coord_flip()

# function to measure which sectors are mentioned together
summarise_variable <- function(sector, feasibility_pred){
  abstract1 %>% 
    filter({{sector}} == 1, {{feasibility_pred}} == 1) %>% 
    summarise(energy_pred = sum(energy_pred),
              transport_pred = sum(transport_pred),
              buildings_pred = sum(buildings_pred),
              industry_pred = sum(industry_pred),
              afolu_pred = sum(afolu_pred)) 
}


cooccurrence <- rbind(summarise_variable(energy_pred, feasibility_pred), summarise_variable(energy_pred, policy_evaluation_pred),
                      summarise_variable(transport_pred, feasibility_pred), summarise_variable(transport_pred, policy_evaluation_pred),
                      summarise_variable(buildings_pred, feasibility_pred), summarise_variable(buildings_pred, policy_evaluation_pred),
                      summarise_variable(industry_pred, feasibility_pred), summarise_variable(industry_pred, policy_evaluation_pred),
                      summarise_variable(afolu_pred, feasibility_pred), summarise_variable(afolu_pred, policy_evaluation_pred)) %>% 
  as.data.frame()

cooccurrence$sector <- rep(c("Energy", "Transport", "Buildings", "Industry", "AFOLU"), each = 2)
cooccurrence$topic <- rep(c("Policy Feasibility", "Policy Evaluation"), 5)
colnames(cooccurrence) <- c("Energy", "Transport", "Buildings", "Industry", "AFOLU", "sector", "topic")

p_co <- melt(cooccurrence, id.vars =  c("topic", "sector"), measure.vars =  c("Energy", "Transport", "Buildings", "Industry", "AFOLU")) %>%
  mutate(sector = factor(sector, levels = c("Energy", "Transport", "Buildings", "Industry", "AFOLU")),
         variable = factor(variable, levels = rev(c("Energy", "Transport", "Buildings", "Industry", "AFOLU")))) %>% 
  ggplot(., aes(sector, variable)) + 
  geom_tile(aes(width=.85, height=.95, fill = value), show.legend = F) +
  scale_fill_gradient(high = "#132B43", low = "#56B1F7") +
  geom_text(aes(label=value)) + ylab("") + xlab("") +
  facet_grid("topic", scales = "free_x") +
  theme_SM() +
  theme(axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        panel.spacing = unit(2, "lines")) +
  coord_cartesian(expand=FALSE)
p_co
ggsave(p_co, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/Co_occurrence.pdf", height = 10, width = 10)


library(rnaturalearth)
library(sf)
library(pacman)
country_in_abstract <- sapply(world$name, grepl, abstract1$Abstract)
country_in_abstract <- cbind(colSums(country_in_abstract), as.character(world$name))
colnames(country_in_abstract) <- c("count", "name")

world <- left_join(world,  as.tibble(country_in_abstract), by = "name") %>% 
  mutate(count = as.numeric(count))

continents <- world %>%
  group_by(ar6_continent) %>% 
  summarise(geometry = st_union(geometry)) 

tmap_mode("plot")
p_map <- tm_shape(world) +
  tm_polygons(col = "count", 
              breaks = c(1,5,25,125,625,3125), 
              palette = "Blues", 
              title = "Number of Studies per Country", 
              legend.is.portrait = FALSE) +
  tm_layout(frame = F, 
            legend.position = c("center", "top"), 
            legend.outside.position = "bottom",
            legend.outside = T, 
            legend.stack = "horizontal", 
            legend.text.size = .55, legend.title.size = 1,
            legend.width = .1,
            outer.margins=c(-.18,-.1,-.01,-.1)) 
p_map

tmap_save(p_map, filename = "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/map.pdf", height = 5, width = 10)


library(rnaturalearth)
library(sf)
library(pacman)
data("World")
country_in_abstract <- sapply(World$name, grepl, abstract1$Abstract)
country_in_abstract_dist <- cbind(colSums(country_in_abstract[abstract1$distributional_dynamics_pred == 1,])/colSums(country_in_abstract), as.character(world$name))
country_in_abstract_econ <- cbind(colSums(country_in_abstract[abstract1$economic_cost_pred == 1,])/colSums(country_in_abstract), as.character(world$name))
country_in_abstract_inst <- cbind(colSums(country_in_abstract[abstract1$institutional_capacity_pred == 1,])/colSums(country_in_abstract), as.character(world$name))
country_in_abstract_mult <- cbind(colSums(country_in_abstract[abstract1$multilevel_pred == 1,])/colSums(country_in_abstract), as.character(world$name))

colnames(country_in_abstract_dist) <- c("count_distributional", "name")
colnames(country_in_abstract_econ) <- c("count_economic", "name")
colnames(country_in_abstract_inst) <- c("count_institutional", "name")
colnames(country_in_abstract_mult) <- c("count_multi", "name")

world_barriers <- World %>% 
  left_join(., as.tibble(country_in_abstract_dist), by = "name") %>% 
  left_join(., as.tibble(country_in_abstract_econ), by = "name") %>% 
  left_join(., as.tibble(country_in_abstract_inst), by = "name") %>% 
  left_join(., as.tibble(country_in_abstract_mult), by = "name") %>% 
  mutate_at(vars(contains("count")), as.numeric) %>% 
  mutate_at(vars("count_distributional", "count_economic", "count_institutional", "count_multi"), .funs = function(x){ifelse(is.nan(x), "0", x)})


tmap_mode("plot")
p_map_dist <- tm_shape(world_barriers) +
  tm_polygons(col = "count_distributional", 
              breaks = seq(0,1,0.2),
              palette = "Blues", 
              title = "Distributional dynamic barriers by country", 
              legend.is.portrait = FALSE) +
  tm_layout(frame = F, 
            legend.position = c("center", "top"), 
            legend.outside.position = "bottom",
            legend.outside = T, 
            legend.stack = "horizontal", 
            legend.text.size = .55, legend.title.size = 1,
            legend.width = .1,
            outer.margins=c(-.18,-.1,-.01,-.1)) 

p_map_econ <- tm_shape(world_barriers) +
  tm_polygons(col = "count_economic", 
              breaks = seq(0,1,0.2),
              palette = "Blues", 
              title = "Distributional dynamic barriers by country", 
              legend.is.portrait = FALSE) +
  tm_layout(frame = F, 
            legend.position = c("center", "top"), 
            legend.outside.position = "bottom",
            legend.outside = T, 
            legend.stack = "horizontal", 
            legend.text.size = .55, legend.title.size = 1,
            legend.width = .1,
            outer.margins=c(-.18,-.1,-.01,-.1)) 


p_map_inst <- tm_shape(world_barriers) +
  tm_polygons(col = "count_institutional", 
              breaks = seq(0,1,0.2),
              palette = "Blues", 
              title = "Distributional dynamic barriers by country", 
              legend.is.portrait = FALSE) +
  tm_layout(frame = F, 
            legend.position = c("center", "top"), 
            legend.outside.position = "bottom",
            legend.outside = T, 
            legend.stack = "horizontal", 
            legend.text.size = .55, legend.title.size = 1,
            legend.width = .1,
            outer.margins=c(-.18,-.1,-.01,-.1)) 

p_map_mult <- tm_shape(world_barriers) +
  tm_polygons(col = "count_multi", 
              breaks = seq(0,1,0.2),
              style = 'fixed',
              palette = "Blues", 
              title = "Share of research on specific barrier by country", 
              legend.is.portrait = FALSE) +
  tm_layout(frame = F, 
            legend.position = c("center", "top"), 
            legend.outside.position = "bottom",
            legend.outside = T, 
            legend.stack = "horizontal", 
            legend.text.size = .55, legend.title.size = 1,
            legend.width = .1,
            outer.margins=c(-.18,-.1,-.01,-.1)) 

tmap_save(p_map_dist, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/map_distributional.pdf")
tmap_save(p_map_econ, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/map_economic_cost.pdf")
tmap_save(p_map_inst, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/map_capacity.pdf")
tmap_save(p_map_mult, "/Volumes/Transcend/Uni/doktorat/Proposal_Seminar/Paper 1/scripts/map_multi.pdf")

tmap_arrange(p_map_dist, p_map_econ, p_map_inst, p_map_mult, nrow=2)
















