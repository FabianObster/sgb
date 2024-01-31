#current best: mstop = 3000, nu = 0.07, kfold = 3
library(tidyverse)
library(extrafont)
library(openxlsx)
library(magrittr)
library(tidyverse)
library(mboost)
library(mltools)
library(pROC)
library(plotROC)
library(pROC)
library(stringi)
library(questionr)
library(randomForest)
library(gbm)
library(neuralnet)
library(e1071)
library(gridExtra)
library(tictoc)
library(ggradar)
library(SGL)

# helper functions start
myApply <- function(X, FUN, ...) {
  myFun <- function(...) {
    library("mboost") # load mboost on nodes
    FUN(...)
  }
  ## further set up steps as required
  parLapply(cl = cl, X, myFun, ...)
}

clean_names <- function(data_to_clean) {
  data_to_clean %>%
    mutate(
      variable = str_replace_all(variable, "S1.3a", "Climate change acceptance"),
      variable = str_replace_all(variable, "S8.4a", "Dependent on farm"),
      variable = str_replace_all(variable, "reg", "Natural assets"),
      variable = str_replace_all(variable, "S1.9a", "Trust in newspapers"),
      variable = str_replace_all(variable, "S1.9b", "Trust in farming journals"),
      variable = str_replace_all(variable, "S1.9c", "Trust in TV"),
      variable = str_replace_all(variable, "S1.9d", "Trust in radio"),
      variable = str_replace_all(variable, "S1.9e", "Trust in internet"),
      variable = str_replace_all(variable, "S1.9f", "Trust in extension workers"),
      variable = str_replace_all(variable, "S1.9g", "Trust in government workers"),
      variable = str_replace_all(variable, "S1.9h", "Trust in neighbours"),
      variable = str_replace_all(variable, "S1.9i", "Trust in industry"),
      variable = str_replace_all(variable, "S1.9j", "Trust in farm associations"),
      variable = str_replace_all(variable, "S1.8a", "Use of newspapers"),
      variable = str_replace_all(variable, "S1.8b", "Use of farming journals"),
      variable = str_replace_all(variable, "S1.8c", "Use of TV"),
      variable = str_replace_all(variable, "S1.8d", "Use of radio"),
      variable = str_replace_all(variable, "S1.8e", "Use of internet"),
      variable = str_replace_all(variable, "S1.8f", "Use of extension workers"),
      variable = str_replace_all(variable, "S1.8g", "Use of government workers"),
      variable = str_replace_all(variable, "S1.8h", "Use of neighbours"),
      variable = str_replace_all(variable, "S1.8i", "Use of industry"),
      variable = str_replace_all(variable, "S1.8j", "Use of farm associations"),
      variable = str_replace_all(variable, "S8.1c", "Gender"),
      variable = str_replace_all(variable, "S8.1d", "Education"),
      variable = str_replace_all(variable, "S5.17a", "Trust in government institutions"),
      variable = str_replace_all(variable, "S5.17c", "Trust in religion"),
      variable = str_replace_all(variable, "S5.17d", "Trust in fate"),
      variable = str_replace_all(variable, "S8.6a", "Farm size"),
      variable = str_replace_all(variable, "S8.2balternative", "Prior ownership (family)"),
      variable = str_replace_all(variable, "S8.14", "Farm debt load"),
      variable = str_replace_all(variable, "S8.1b", "Age > 50"),
      variable = str_replace_all(variable, "S3.3", "Income invested"),
      variable = str_replace_all(variable, "S8.4b", "Family farm engagement"),
      variable = str_replace_all(variable, "A3", "Agronomic measures"),
      variable = str_replace_all(variable, "S8.6e", "Other products"),
      variable = str_replace_all(variable, "F3", "Economic measures"),
      variable = str_replace_all(variable, "T3", "Technological measures"),
      variable = str_replace_all(variable, "S1.3d", "Climate extremes"),
      variable = str_replace_all(variable, "S1.3b", "Human cause climate change"),
      variable = str_replace_all(variable, "S5.17b", "Trust in other farmers"),
      variable = str_replace_all(variable, "S8.6c", "More than one variety grown"),
      variable = str_replace_all(variable, "S2.1a1", "Increasing temperature"),
      variable = str_replace_all(variable, "S8.11_well", "Use of well irrigation"),
      variable = str_replace_all(variable, "S8.11_river", "Use of river irrigation"),
      variable = str_replace_all(variable, "S2.1d1", "Increasing extreme weather"),
      variable = str_replace_all(variable, "S8.2a2", "Years of farm possession"),
      variable = str_replace_all(variable, "S1.8d", "Trust in internet"),
      variable = str_replace_all(variable, "S2.1b3", "Decreasing rainfall"),
      variable = str_replace_all(variable, "S2.1c1", "Increasing drought"),
      variable = str_replace_all(variable, "S2.2c_pr", "Summer temperature proximity"),
      variable = str_replace_all(variable, "S2.3c_pr", "Winter temperature proximity"),
      variable = str_replace_all(variable, "S2.4c_pr", "Decreasing rainfall proximity"),
      variable = str_replace_all(variable, "S2.5c_pr", "Drought proximity"),
      variable = str_replace_all(variable, "S2.6c_pr", "Extreme weather proximity"),
      variable = str_replace_all(variable, "S2.2c_no", "No summer temperature vulnerability"),
      variable = str_replace_all(variable, "S2.3c_no", "No winter temperature vulnerability"),
      variable = str_replace_all(variable, "S2.4c_no", "No decreasing rainfall vulnerability"),
      variable = str_replace_all(variable, "S2.5c_no", "No drought vulnerability"),
      variable = str_replace_all(variable, "S2.6c_no", "No Extreme weather vulnerability"),
      variable = str_replace_all(variable, "S2.2c", "Summer temperature vulnerability"),
      variable = str_replace_all(variable, "S2.3c", "Winter temperature vulnerability"),
      variable = str_replace_all(variable, "S2.4c", "Decreasing rainfall vulnerability"),
      variable = str_replace_all(variable, "S2.5c", "Drought vulnerability"),
      variable = str_replace_all(variable, "S2.6c", "Extreme weather vulnerability"),
      variable = str_replace_all(variable, "S8.3b", "Years of farm managing"),
      variable = str_replace_all(variable, "S8.6b", "Orchard size"),
      variable = str_replace_all(variable, "S8.13_low", "Low financial wellbeing"),
      variable = str_replace_all(variable, "S8.13", "High financial wellbeing"),
      variable = str_replace_all(variable, "S1.1b", "Work independent"),
      variable = str_replace_all(variable, "S1.1c", "Keep tradition alive"),
      variable = str_replace_all(variable, "S1.1d", "Provide good living environment"),
      variable = str_replace_all(variable, "S1.1e", "Be in profitable business"),
      variable = str_replace_all(variable, "S1.2a", "Climate change is harmful"),
      variable = str_replace_all(variable, "S1.2c", "High optimism"),
      variable = str_replace_all(variable, "S1.3f", "High certainty"),
      variable = str_replace_all(variable, "S2.7", "Crop damage near farms"),
      variable = str_replace_all(variable, "S2.8", "Crop damage farms in Country"),
      variable = str_replace_all(variable, "S3.1a", "Climate change occurs"),
      variable = str_replace_all(variable, "S3.1b", "Climate threatens farm"),
      variable = str_replace_all(variable, "S3.1c", "Climate risks > benefits"),
      variable = str_replace_all(variable, "S3.4", "Adaptive measures efficacy"),
      variable = str_replace_all(variable, "S3.5", "Adapive measures near farms"),
    ) %>%
    return()
}
prepare_sgb <- function(alpha = 0.5, local_df = 1, index_df, blearner = "bols",
                        outcome_name = "y") {
  formula_frame <- index_df
  formula_frame$degf <- local_df
  formula_frame <- formula_frame %>%
    group_by(index) %>%
    ungroup() %>%
    mutate(
      term = paste0(blearner, "(", sgl_name, ", df = ", alpha, ")")
    )
  formula <- paste0(formula_frame$term, collapse = "+")
  formula_group <- formula_frame %>%
    group_by(index) %>%
    summarize(sgl_name = paste0(sgl_name, collapse = " , "), degf = (1 - alpha)) %>%
    mutate(term = paste0(blearner, "(", sgl_name, ", df = ", degf, ")"))
  formula_group <- paste0(formula_group$term, collapse = " + ")
  final_formula <- paste0(formula, " + ", formula_group)
  return(final_formula)
}

prepare_sgb_lambda <- function(alpha = 0.5, local_df = 1,lambda = 1, index_df,
                               blearner = "bols", outcome_name = "y") {
  formula_frame <- index_df
  formula_frame <- formula_frame %>%
    group_by(index) %>%
    ungroup() %>%
    mutate(
      term = paste0(blearner, "(", sgl_name, ", lambda = ", 1-alpha, ")")
    )
  formula <- paste0(formula_frame$term, collapse = "+")
  formula_group <- formula_frame %>%
    mutate(regularization = 1) %>%
    group_by(index) %>%
    summarize(sgl_name = paste0(sgl_name, collapse = " , "), degf = lambda*sqrt(sum(regularization))*(alpha)) %>%
    mutate(term = paste0(blearner, "(", sgl_name, ", lambda = ", degf, ")"))
  formula_group <- paste0(formula_group$term, collapse = " + ")
  final_formula <- paste0(formula, " + ", formula_group)
  return(final_formula)
}
cv_sgl_error <- function(vul_df, alpha = 0.95, outcome = 'A5',
                         index_df, k_folds = 3, nlam = 10, verbose = F){
  X <- vul_df %>%
    select(-outcome) %>%
    mutate_all(as.numeric) %>% 
    as.matrix()
  y <- vul_df[[outcome]] == 'TRUE'
  cv_sgl <- cvSGL(data = list(x = X, y = y),
                  index = index_df$index,
                  alpha = alpha, nfold = k_folds, nlam = nlam, verbose = F,type = 'logit'
  ) 
  cv_sgl
} 
cv_sgl <- possibly(cv_sgl_error, otherwise = 0)
fit_sgl_error <- function(lambda_opt, alpha, index_df, vul_df, outcome){
  X <- vul_df %>%
    select(-outcome) %>%
    mutate_all(as.numeric) %>% 
    as.matrix()
  y <- vul_df[[outcome]] == 'TRUE'
  SGL(data = list(x = X, y = y),
      index = index_df$index,
      lambdas = c(lambda_opt, lambda_opt),
      alpha = alpha,
  ) %>% return() 
}
fit_sgl <- possibly(fit_sgl_error, otherwise = 'error sgl')
# Helper functions end
# Data manipulation and cleaning start

df_raw <- read.csv("environmental_data/df_raw.csv") %>%
  mutate(
    "A5" = S5.5a | S5.5c | S5.5d | S5.5e | S5.5m,
    "F5" = S5.5i | S5.5j | S5.5k | S5.5l,
    "T5" = S5.5b | S5.5f | S5.5g | S5.5h,
    "O5" = case_when(is.na(S3.2n) ~ F, T ~ T),
    "A3" = S3.2a | S3.2c | S3.2d | S3.2e | S3.2m,
    "F3" = S3.2i | S3.2j | S3.2k | S3.2l,
    "T3" = S3.2b | S3.2f | S3.2g | S3.2h,
    "O3" = S3.2m,
    S4.7 = as.character(S4.7),
    S7.1alternative = as.character(S7.1alternative),
    S7.1 = as.character(S7.1)
  ) %>%
  mutate(
    "A5" = case_when(S5.5n == "1_A" ~ T, S5.5n == "1_P" ~ T, T ~ A5),
    "T5" = case_when(
      S5.5n == "2_WAT" ~ T, S5.5n == "2_TPS" ~ T, S5.5n == "2_F" ~ T,
      S5.5n == "2_OTH" ~ T, S5.5n == "2_WIND" ~ T, T ~ T5
    ),
    "F5" = case_when(S5.5n == "3_FFR" ~ T, S5.5n == "4_MAN" ~ T, S5.5n == "4_AGR" ~ T, T ~ F5)
  ) %>%
  mutate(
    "A3" = case_when(S3.2n == "1_A" ~ T, S3.2n == "1_P" ~ T, T ~ A3),
    "T3" = case_when(
      S3.2n == "2_WAT" ~ T, S3.2n == "2_IRR" ~ T, S3.2n == "2_TPS" ~ T, S3.2n == "2_F" ~ T,
      S3.2n == "2_OTH" ~ T, S3.2n == "2_WIND" ~ T, T ~ T3
    ),
    "F3" = case_when(S3.2n == "3_FFR" ~ T, S3.2n == "4_MAN" ~ T, S3.2n == "4_AGR" ~ T, T ~ F3)
  ) %>%
  select(-contains("S3.2")) %>%
  mutate(
    S4.2d = as.numeric(S4.2d),
    S4.2e = as.numeric(S4.2e),
    S4.7 = case_when(
      str_detect(S4.7, "1_") ~ "1",
      str_detect(S4.7, "2_") ~ "2",
      str_detect(S4.7, "3_") ~ "3",
      str_detect(S4.7, "4_") ~ "4",
      str_detect(S4.7, "48") ~ "NA",
      T ~ S4.7
    ),
    S4.3a = case_when(
      str_detect(S4.3a, "1_") ~ "1",
      str_detect(S4.3a, "2_") ~ "2",
      str_detect(S4.3a, "3_") ~ "3",
      str_detect(S4.3a, "4_") ~ "4",
      T ~ NA_character_
    ),
    S4.3b = case_when(
      str_detect(S4.3b, "1_") ~ "1",
      str_detect(S4.3b, "2_") ~ "2",
      str_detect(S4.3b, "3_") ~ "3",
      str_detect(S4.3b, "4_") ~ "4",
      T ~ NA_character_
    ),
    S1.2a = case_when(
      str_detect(S1.2a, "1_") ~ "1",
      str_detect(S1.2a, "2_") ~ "2",
      str_detect(S1.2a, "3_") ~ "3",
      str_detect(S1.2a, "4_") ~ "4",
      T ~ S4.7
    ),
    S1.2b1 = case_when(
      str_detect(S1.2b1, "1_") ~ "1",
      str_detect(S1.2b1, "2_") ~ "2",
      str_detect(S1.2b1, "3_") ~ "3",
      str_detect(S1.2b1, "4_") ~ "4",
      T ~ S4.7
    ),
    S7.1alternative = case_when(
      str_detect(S7.1alternative, "1_") ~ "1",
      str_detect(S7.1alternative, "2_") ~ "2",
      str_detect(S7.1alternative, "3_") ~ "3",
      str_detect(S7.1alternative, "4_") ~ "4",
      str_detect(S7.1alternative, "5_") ~ "5",
      str_detect(S7.1alternative, "6_") ~ "6",
      T ~ S7.1alternative
    ),
    S7.1 = case_when(
      str_detect(S7.1, "1_") ~ "1",
      str_detect(S7.1, "2_") ~ "2",
      str_detect(S7.1, "3_") ~ "3",
      str_detect(S7.1, "4_") ~ "4",
      str_detect(S7.1, "5_") ~ "5",
      str_detect(S7.1, "6_") ~ "6",
      T ~ S7.1
    ),
    S8.2a2 = relevel(factor(case_when(
      S8.2a2 >= 1 ~ 'TRUE', T ~ 'FALSE'
    )), ref = "FALSE")
  ) %>%
  mutate(
    S5.9 = case_when(S5.9 >= 4 ~ T, T ~ F),
    S5.7 = case_when(S5.7 >= 4 ~ T, T ~ F),
    S5.8 = case_when(S5.8 >= 4 ~ T, T ~ F),
    S5.6p = !is.na(S5.6p)
  ) %>%
  mutate(
    reg = case_when(
      RegionsDetail == 16 ~ "SouthernChile",
      RegionsDetail %in% c(6, 7, 13) ~ "CentralChile",
      RegionsDetail == 1 ~ "CentralTunisia",
      RegionsDetail == 2 ~ "NorthernTunisia"
    ),
    Regions = case_when(
      reg %in% c("NorthernTunisia", "SouthernChile") ~ "cooler",
      reg %in% c("CentralTunisia", "CentralChile") ~ "hotter"
    )
  ) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(reg = factor(reg, levels = c("CentralChile", "SouthernChile", "NorthernTunisia", "CentralTunisia"))) %>%
  mutate(
    S2.1c1 = case_when(str_detect(S2.1calt1, "unpredictable") | str_detect(S2.1calt1, "increased") ~ TRUE, T ~ FALSE),
    S2.1d1 = case_when(str_detect(S2.1dalt1, "unpredictable") | str_detect(S2.1dalt1, "increased") ~ TRUE, T ~ FALSE)
  )
set.seed(101)


# Time proximity
df_raw <- df_raw %>%
  mutate_at(
    c("S2.2b09", "S2.2b10", "S2.2b11", "S2.2b12", "S2.2b13", "S2.2b14", "S2.2b15", "S2.2b16", "S2.2b17", "S2.2b18", "S2.2b19"),
    function(x) case_when(is.na(x) ~ 0L, T ~ x)
  ) %>%
  mutate(
    S2.2c_frequency = S2.2b09 + S2.2b10 + S2.2b11 + S2.2b12 + S2.2b13 + S2.2b14 + S2.2b15 + S2.2b16 + S2.2b17 + S2.2b18 + S2.2b19
  ) %>%
  mutate_at(c("S2.2c", "S2.3c", "S2.4c", "S2.5c", "S2.6c"), function(x) {
    case_when(is.na(x) ~ 0L, T ~ x)
  })
for (varia in c("S2.2b09", "S2.2b10", "S2.2b11", "S2.2b12", "S2.2b13", "S2.2b14", "S2.2b15", "S2.2b16", "S2.2b17", "S2.2b18", "S2.2b19")) {
  df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
}
df_raw <- df_raw %>% mutate(
  S2.2c_proximity = pmax(S2.2b09, S2.2b10, S2.2b11, S2.2b12, S2.2b13, S2.2b14, S2.2b15, S2.2b16, S2.2b17, S2.2b18, S2.2b19),
  S2.2c_frequency = as.numeric(S2.2c_frequency)
)
df_raw <- df_raw %>%
  mutate_at(
    c("S2.5b09", "S2.5b10", "S2.5b11", "S2.5b12", "S2.5b13", "S2.5b14", "S2.5b15", "S2.5b16", "S2.5b17", "S2.5b18", "S2.5b19"),
    function(x) case_when(is.na(x) ~ 0L, T ~ x)
  ) %>%
  mutate(S2.5c_frequency = S2.5b09 + S2.5b10 + S2.5b11 + S2.5b12 + S2.5b13 + S2.5b14 + S2.5b15 + S2.5b16 + S2.5b17 + S2.5b18 + S2.5b19)

for (varia in c("S2.5b09", "S2.5b10", "S2.5b11", "S2.5b12", "S2.5b13", "S2.5b14", "S2.5b15", "S2.5b16", "S2.5b17", "S2.5b18", "S2.5b19")) {
  df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
}
df_raw <- df_raw %>% mutate(
  S2.5c_proximity = pmax(S2.5b09, S2.5b10, S2.5b11, S2.5b12, S2.5b13, S2.5b14, S2.5b15, S2.5b16, S2.5b17, S2.5b18, S2.5b19),
  S2.5c_frequency = as.numeric(S2.5c_frequency)
)

df_raw <- df_raw %>%
  mutate_at(
    c("S2.6b09", "S2.6b10", "S2.6b11", "S2.6b12", "S2.6b13", "S2.6b14", "S2.6b15", "S2.6b16", "S2.6b17", "S2.6b18", "S2.6b19"),
    function(x) case_when(is.na(x) ~ 0L, T ~ x)
  ) %>%
  mutate(S2.6c_frequency = S2.6b09 + S2.6b10 + S2.6b11 + S2.6b12 + S2.6b13 + S2.6b14 + S2.6b15 + S2.6b16 + S2.6b17 + S2.6b18 + S2.6b19)

for (varia in c("S2.6b09", "S2.6b10", "S2.6b11", "S2.6b12", "S2.6b13", "S2.6b14", "S2.6b15", "S2.6b16", "S2.6b17", "S2.6b18", "S2.6b19")) {
  df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
}
df_raw <- df_raw %>% mutate(
  S2.6c_proximity = pmax(S2.6b09, S2.6b10, S2.6b11, S2.6b12, S2.6b13, S2.6b14, S2.6b15, S2.6b16, S2.6b17, S2.6b18, S2.6b19),
  S2.6c_frequency = as.numeric(S2.6c_frequency)
)

df_raw <- df_raw %>%
  mutate_at(
    c("S2.4b09", "S2.4b10", "S2.4b11", "S2.4b12", "S2.4b13", "S2.4b14", "S2.4b15", "S2.4b16", "S2.4b17", "S2.4b18", "S2.4b19"),
    function(x) case_when(is.na(x) ~ 0L, T ~ x)
  ) %>%
  mutate(S2.4c_frequency = S2.4b09 + S2.4b10 + S2.4b11 + S2.4b12 + S2.4b13 + S2.4b14 + S2.4b15 + S2.4b16 + S2.4b17 + S2.4b18 + S2.4b19)

for (varia in c("S2.4b09", "S2.4b10", "S2.4b11", "S2.4b12", "S2.4b13", "S2.4b14", "S2.4b15", "S2.4b16", "S2.4b17", "S2.4b18", "S2.4b19")) {
  df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
}
df_raw <- df_raw %>% dplyr::mutate(
  S2.4c_proximity = pmax(S2.4b09, S2.4b10, S2.4b11, S2.4b12, S2.4b13, S2.4b14, S2.4b15, S2.4b16, S2.4b17, S2.4b18, S2.4b19),
  S2.4c_frequency = as.numeric(S2.4c_frequency)
)

df_raw <- df_raw %>%
  mutate_at(
    c("S2.3b09", "S2.3b10", "S2.3b11", "S2.3b12", "S2.3b13", "S2.3b14", "S2.3b15", "S2.3b16", "S2.3b17", "S2.3b18", "S2.3b19"),
    function(x) case_when(is.na(x) ~ 0L, T ~ x)
  ) %>%
  mutate(S2.3c_frequency = S2.3b09 + S2.3b10 + S2.3b11 + S2.3b12 + S2.3b13 + S2.3b14 + S2.3b15 + S2.3b16 + S2.3b17 + S2.3b18 + S2.3b19)

for (varia in c("S2.3b09", "S2.3b10", "S2.3b11", "S2.3b12", "S2.3b13", "S2.3b14", "S2.3b15", "S2.3b16", "S2.3b17", "S2.3b18", "S2.3b19")) {
  df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
}
df_raw <- df_raw %>% dplyr::mutate(
  S2.3c_proximity = pmax(S2.3b09, S2.3b10, S2.3b11, S2.3b12, S2.3b13, S2.3b14, S2.3b15, S2.3b16, S2.3b17, S2.3b18, S2.3b19),
  S2.3c_frequency = as.numeric(S2.3c_frequency)
)


vars <- c("Farmer.", "Regions", "RegionsDetail", "Farmer.ID")
df <- df_raw %>%
  select(-vars) %>%
  select(-contains("S6"), -"S8.1a", "S8.10b", -"S4.6not") %>%
  select(-contains("S8.12"), "S8.6e", -"S8.2b", -"S2.1aalt1", -"S4.6")

# change wrong coding
df$S4.1a[df$S4.1a == "6"] <- 3
df$S4.1b[df$S4.1b == "6"] <- 3
df$S4.1c[df$S4.1c == "6"] <- 3
df$S4.2a[df$S4.2a == "6"] <- 3
df$S4.2b[df$S4.2b == "6"] <- 3
df$S4.2d[df$S4.2d == "6"] <- 1
df$S4.2e[df$S4.2e == "6"] <- 1
df$S3.1b[df$S3.1b == 0] <- 1
# df$S2.5c[df$S2.5c == 0] <- 1
df$S2.8[df$S2.8 == 0] <- 1
df$S7.1[df$S7.1 == "N/A"] <- NA
df$S1.2a[df$S1.2a == "N/A"] <- NA
df[df == "N/A"] <- NA
df$S8.6c <- !is.na(df$S8.6c2)
df$S5.5n[is.na(df$S5.5n)] <- "NA"
df <- df %>% mutate_if(is.factor, as.character)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
for (vars in colnames(df)[!(colnames(df) %in% c("F5", "T5", "A5", "S4.1a"))]) {
  if (length(unique(df[[vars]])) < 4) {
    df[[vars]] <- as.character(df[[vars]])
  }
  if (sum(is.na(df[[vars]])) > 80) {
    df[[vars]] <- NULL
  }
  if (is.numeric(df[[vars]])) {
    df[[vars]][is.na(df[[vars]])] <- Mode(df[[vars]])
  }
  if (sum(is.na(df[[vars]])) < 10 & is.character(df[[vars]])) {
    df[[vars]][is.na(df[[vars]])] <- Mode(df[[vars]])
  }
  if (sum(is.na(df[[vars]])) > 10 & is.character(df[[vars]])) {
    df[[vars]][is.na(df[[vars]])] <- "NA"
  }
}

df <- df %>%
  mutate(
    S5.12_low = case_when(
      S5.12 %in% c("1") ~ "TRUE",
      T ~ "FALSE"
    ),
    S5.12_high = case_when(
      S5.12 %in% c("4", "5", "6") ~ "TRUE",
      T ~ "FALSE"
    ),
    S3.3_low = case_when(
      S3.3 %in% c("1") ~ "TRUE",
      S3.3 %in% c("2","3", "4", "5", "6") ~ "FALSE"
    ),
    S3.3 = case_when(
      S3.3 %in% c("1", "2", "3") ~ "FALSE",
      S3.3 %in% c("4", "5", "6") ~ "TRUE"
    ),
  )
model_df <- df %>%
  mutate_at(
    c("A5", "T5", "F5"),
    function(x) {
      relevel(as.factor(x), ref = "FALSE")
    }
  ) %>%
  mutate(
    S1.3a = S1.3a == "2", S1.3b = S1.3b == "1", S1.3d = S1.3d == "1", 
    S1.3f = S1.3f == "2",
    S2.7 = relevel(as.factor(S2.7 == "1"), ref = "FALSE"),
    S2.8 = relevel(as.factor(S2.8 == "1"), ref = "FALSE"),
    S3.5 = S3.5 ==1,
    S3.5 = relevel(as.factor(S3.5), ref = "FALSE"),
    S5.4 = relevel(as.factor(S5.4 == "YES"), ref = "FALSE"),
    S5.15 = relevel(as.factor(S5.15a == 1 | S5.15b == 1 | S5.15c == 5), ref = "FALSE"),
    S5.10 = relevel(as.factor(case_when(S5.10 == "I don’t know" ~ "not_know", T ~ S5.10)), ref = "NO"),
    S5.9 = relevel(as.factor(S5.9 == "TRUE"), ref = "FALSE"),
    S5.14 = relevel(as.factor(S5.14aa == 1 | S5.14ab == 1 | S5.14ac == 1 | S5.14ad == 1), ref = "FALSE"),
    S5.13d = relevel(as.factor(S5.13da >= 4 | S5.13db >= 4 | S5.13dc >= 4 | S5.13dd >= 4), ref = "FALSE"),
    S5.13 = relevel(as.factor(S5.13a >= 4 | S5.13b <= 2 | S5.13c >= 4), ref = "FALSE"),
    S5.13a = relevel(as.factor(S5.13a >= 4), ref = "FALSE"),
    S5.13b = relevel(as.factor(S5.13b >= 4), ref = "FALSE"),
    S5.13c = relevel(as.factor(S5.13c >= 4), ref = "FALSE"),
    S1.2a = relevel(as.factor(S1.2a %in% c("1", "2")), ref = "FALSE"),
    S2.2c_proximity = relevel(as.factor(S2.2c_proximity > 17), ref = "FALSE"),
    S2.2c_frequency = relevel(as.factor(S2.2c_frequency >= 3), ref = "FALSE"),
    S2.3c_proximity = relevel(as.factor(S2.3c_proximity > 17), ref = "FALSE"),
    S2.3c_frequency = relevel(as.factor(S2.3c_frequency >= 3), ref = "FALSE"),
    S2.5c_proximity = relevel(as.factor(S2.5c_proximity > 17), ref = "FALSE"),
    S2.5c_frequency = relevel(as.factor(S2.5c_frequency >= 3), ref = "FALSE"),
    S2.6c_proximity = relevel(as.factor(S2.6c_proximity > 17), ref = "FALSE"),
    S2.6c_frequency = relevel(as.factor(S2.6c_frequency >= 3), ref = "FALSE"),
    S2.4c_proximity = relevel(as.factor(S2.4c_proximity > 17), ref = "FALSE"),
    S8.1b = S8.1b > 50,
    S8.6e = (S8.6e != "NO")
  )

model_df <- lasso_df <- model_df %>%
  mutate(
    S3.1a = (S3.1a >= 4), S3.1b = S3.1b >= 4, S3.1c = S3.1c >= 4,
    S4.2a = S4.2a >= 4, S4.2b = S4.2b >= 4,
    S4.4 = relevel(as.factor(S4.4 >= 4), ref = "TRUE"), S8.4a = S8.4a > 2,
    S2.2a = S2.2a == "YES", S2.3a = S2.3a == "YES", S2.4a = S2.4a == "YES",
    S2.5a = S2.5a == "YES", S2.6a = S2.6a == "YES",
    S2.2 = relevel(factor(S2.2c >= 4 | S2.3c >= 4 | S2.4c >= 4 | S2.5c >= 4 | S2.6c >= 4), ref = "FALSE")
  )


model_df <- lasso_df <- model_df %>%
  mutate_at(
    c(
      "S5.11", "S4.9", "S5.17a", "S5.17b", "S5.17c", "S5.17d", "S3.4", "S1.2c", "S5.14b",
      "S5.1a", "S5.1b"
    ),
    function(x) {
      relevel(as.factor(x %in% c("4", "5")), ref = "FALSE")
    }
  ) %>%
  mutate_at(
    c("S1.1e", "S1.1c", "S1.1a", "S1.1d"),
    function(x) {
      relevel(as.factor(as.character(x > 4)), ref = "FALSE")
    }
  ) %>%
  mutate(S8.1b = relevel(as.factor(S8.1b), ref = "FALSE")) %>%
  mutate(S4.4 = relevel(as.factor(S4.4), ref = "FALSE")) %>%
  mutate_at(
    paste0("S7.2", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")),
    function(x) {
      relevel(as.factor(as.character(x >= 6)), ref = "FALSE")
    }
  )

get_varimp <- function(model) {
  data.frame(varimp(model)) %>%
    mutate(reduction = reduction / sum(reduction)) %>%
    as_tibble() %>%
    filter(reduction != 0) %>%
    arrange(-reduction)
}

model_df <- model_df %>%
  mutate(
    AF3 = (A3 == "TRUE" & F3 == "TRUE"),
    AT3 = (A3 == "TRUE" & T3 == "TRUE"),
    TF3 = (T3 == "TRUE" & F3 == "TRUE"),
    ATF3 = (A3 == "TRUE" | F3 == "TRUE" | T3 == "TRUE"),
    AF5 = (A5 == "TRUE" & F5 == "TRUE"),
    AT5 = (A5 == "TRUE" & T5 == "TRUE"),
    TF5 = (T5 == "TRUE" & F5 == "TRUE"),
    ATF5 = (A5 == "TRUE" & F5 == "TRUE" & T5 == "TRUE"),
    S8.10a = (S8.10a == 1),
    S8.1d = case_when(S8.1d >= 4 ~ "TRUE", T ~ "FALSE"),
    S8.3b = S8.3b > 10,
    S8.4b = S8.4b > 2,
    S8.6a = S8.6a > 7,
    S8.6b = case_when(S8.6b <= 2 ~ "FALSE", T ~ "TRUE"),
    S8.7a = S8.7a,
    S8.7b = S8.7b,
    S8.7c = S8.7c,
    S8.14 = S8.14 == 4,
    S8.11 = case_when(S8.11 == "W" ~ "W", S8.11 %in% c("R", "RW") ~ "R", T ~ "rest"),
    S8.11_well = case_when(S8.11 == "W"~ 'TRUE', T ~ 'FALSE'),
    S8.11_river = case_when(S8.11 == "R"~ 'TRUE', T ~ 'FALSE'),
    S4.5a = S4.5a >= 3,
    S4.5b = S4.5b > 2,
    S8.13_low = S8.13 <= 2,
    S8.13 = S8.13 >= 4,
    S8.2balternative = S8.2balternative == "FAMALL",
    S4.2e = case_when(is.na(S4.2e)~3,T~S4.2e),
    S4.2e_low = S4.2e <= 1,
    S4.2e_high = S4.2e >= 3,
    S4.2d_low = S4.2d <= 1,
    S4.2d_high = S4.2d >= 3,
    S4.3a = case_when(S4.3a == "3" ~ "4", T~ S4.3a),
    S4.3a = relevel(factor(S4.3a), ref = "4")
  ) %>%
  mutate(
    S2.2c_no = case_when(S2.2c == 0 ~ 4, T ~ 3),
    S2.3c_no = case_when(S2.3c == 0 ~ 4, T ~ 3),
    S2.4c_no = case_when(S2.4c == 0 ~ 4, T ~ 3),
    S2.5c_no = case_when(S2.5c == 0 ~ 4, T ~ 3),
    S2.6c_no = case_when(S2.6c == 0 ~ 4, T ~ 3),
    S5.10 = case_when(S5.10 == "YES" ~ "TRUE", T ~ "FALSE"),
    S8.7a = S8.7a > 0,
    S8.7b = S8.7b > 0,
    S8.7c = S8.7c > 0,
    S8.10b = S8.10b == "D",
    S8.6d = S8.6d >= 80,
    S8.1c = relevel(as.factor(S8.1c), ref = 'F'),
    S4.3a4 = S4.3a == '4',
    S4.3a1 = S4.3a == '1',
    reg = case_when(reg %in% c('NorthernTunisia', 'CentralChile')~'1',T~'0'),
    S8.1c = case_when(S8.1c %in% c('M')~'1',T~'0')
  ) %>%
  mutate_at(
    c(
      "S2.2c", "S2.3c", "S2.4c", "S2.5c", "S2.6c",
      "S2.2c_no", "S2.3c_no", "S2.4c_no", "S2.5c_no", "S2.6c_no",
      "S1.1b", "S1.1f", "S1.1g", "S1.4", "S1.5", "S1.6", "S1.7",
      "S4.8","S4.10", "S5.15a", "S5.15b", "S5.15c", "S5.15d", "S5.16a",
      "S4.1a", "S4.1b", "S4.1c", "S4.2c","S5.13da","S5.13db","S5.13dc","S5.13dd"
    ),
    function(x) {
      (x >= 4)
    }
  ) %>%
  mutate_at(c("S1.3c",  "S1.3e"), function(x){case_when(x == 2~ T, is.na(x)~F, T~F)}) %>%
  mutate_at(
    c(
      "S2.2c", "S2.3c", "S2.4c", "S2.5c", "S2.6c",
      "S2.2c_no", "S2.3c_no", "S2.4c_no", "S2.5c_no", "S2.6c_no",
      "S1.1b", "S1.1a", "S1.1f", "S1.1g", "S1.4", "S1.5", "S1.6", "S1.7",
      "S4.1a", "S4.1b", "S4.1c", "S4.2a", "S4.2b","S4.2c",
      "S1.3a", "S1.3b", "S1.3c", "S4.5b", "S4.8", 
      "S8.11_well", "S8.11_river", "S3.3", "S3.3_low", "S8.1d", "S8.2a2",
      "S1.3c", "S1.3e", "S4.2d_high", "S4.2d_low", "S4.2e_high", "S4.2e_low",
      "S4.5a", "S4.8", "S4.9", "S4.10", "S5.7", "S5.8", "S5.11", "S5.16a",
      "S5.13da","S5.13db","S5.13dc","S5.13dd", "S8.7a", "S8.7b", "S8.7c",
      "S8.10b", "S8.6d", "S5.12_high", "S5.12_low", "A3", "T3", "F3", "ATF3", "S5.10",
      "S5.15d", "S4.3a1","S4.3a4","S1.3a","S3.1c", 'S5.15b','S3.1a','S8.6c', 'S8.6b',
      'S2.1a1', 'S2.1b3', 'S2.1c1', 'S2.1d1'),
    function(x) {
      relevel(as.factor(as.character(x)),
        ref = "FALSE"
      )
    }
  ) 


model_df <- model_df %>%
  mutate_at(
    colnames(df)[str_detect(colnames(df), "S1.8|S1.9")],
    function(x) {
      (x >= 4)
    }
  ) %>%
  mutate_at(colnames(df)[str_detect(colnames(df), "S1.8|S1.9")], 
            function(x) {
              relevel(as.factor(as.character(x)),
                      ref = "FALSE"
              )
            })

# Data manipulation and cleaning end
# Definitions of groups
group_df <- data.frame(group = "Biophysical asset group", index = 1, col_names = c(
  "A3", "T3", "F3", "S8.11_well", "S8.6a","S8.6d", "S8.6e",
  "S8.6c", "S8.6b", "S8.7a", "S8.7b","S8.7c")) %>%
  rbind(data.frame(group = "Natural asset group", index = 2, col_names = c("reg"))) %>%
  rbind(data.frame(group = "Economic asset group", index = 3, col_names = c(
    "S3.3","S3.3_low", "S8.4b", "S8.13", "S8.13_low",
    "S8.4a", "S8.14"
  ))) %>%
  rbind(data.frame(group = "Human asset group", index = 4, col_names = c(
    "S8.1b", "S8.1c", "S8.1d", "S8.2a2",
    "S8.2balternative", "S8.3b", "S1.3a", "S1.3b", "S1.3d"
  ))) %>%
  rbind(data.frame(group = "Social asset group", index = 5, col_names = c(
    paste0("S1.8", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")),
    paste0("S1.9", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")),
    "S5.17a", "S5.17b", "S5.17c", "S5.17d"
  ))) %>%
  rbind(data.frame(group = "Climate experience group", index = 6, col_names = c("S2.1a1", "S2.1b3", "S2.1c1", "S2.1d1"))) %>%
  rbind(data.frame(group = "Vulnerability", index = 7, col_names = c(
    "S2.2c", "S2.3c", "S2.4c", "S2.5c", "S2.6c",
    "S2.2c_no", "S2.3c_no", "S2.4c_no", "S2.5c_no", "S2.6c_no"
  ))) %>%
  rbind(data.frame(group = "Goals group", index = 8, col_names = c(
    "S1.1a", "S1.1b", "S1.1c", "S1.1d", "S1.1e", "S1.1f", "S1.1g"
  ))) %>%
  rbind(data.frame(group = "Harm knowlege beliefs", index = 9, col_names = c(
    "S1.2a", "S1.2c", "S1.3f", "S3.1b", "S3.1c", "S1.3e", "S1.4", "S1.5", "S1.6", "S1.7"
  ))) %>%
  rbind(data.frame(group = "Temporal proximity group", index = 11, col_names = c(
   "S2.2c_proximity", "S2.3c_proximity", "S2.4c_proximity", "S2.5c_proximity",
   "S2.6c_proximity"
  ))) %>%
  rbind(data.frame(group = "Spatial group", index = 12, col_names = c(
   "S2.7", "S2.8", "S3.1a"
  ))) %>%
  rbind(data.frame(group = "Efficacy group", index = 13, col_names = c("S3.4"))) %>%
  rbind(data.frame(group = "Norms group", index = 14, col_names = c(
    "S3.5", "S5.10"
  ))) %>%
  rbind(data.frame(group = "Risk assessment", index = 15, col_names = c(
    "S4.1a", "S4.1b", "S4.1c", "S4.2a", "S4.2b", "S4.2c",
    "S4.2d_high", "S4.2d_low", "S4.2e_high", "S4.2e_low", "S4.3a1","S4.3a4"
  ))) %>%
  rbind(data.frame(group = "Investment", index = 15.5, col_names = c(
    "S4.4","S4.5a", "S4.5b"
  ))) %>%
  rbind(data.frame(group = "Concerns", index = 16, col_names = c(
   "S4.8", "S4.9", "S4.10"
  ))) %>%
  rbind(data.frame(group = "measures", index = 17, col_names = c(
    "F5", "T5", "A5", "S5.7", "S5.8", "S5.9", "S5.11", "S5.12_low", "S5.12_high",
    "S5.4"
  ))) %>%
  rbind(data.frame(group = "norm efficacy", index = 18, col_names = c(
    "S5.13a", "S5.13b", "S5.13c", "S5.13da","S5.13db","S5.13dc","S5.13dd"
  ))) %>%
  rbind(data.frame(group = "outcome assessment", index = 19, col_names = c(
    "S5.14b"
  ))) %>%
  rbind(data.frame(group = "Normative believes", index = 20, col_names = c(
    "S5.15a", "S5.15b", "S5.15c", "S5.15d"
  ))) %>%
  rbind(data.frame(group = "Uncertainty", index = 21, col_names = c(
   "S5.16a"
  ))) %>%
  rbind(data.frame(group = "Emotions", index = 21, col_names = c(
    "S7.2a", "S7.2b","S7.2c","S7.2d", "S7.2e","S7.2f","S7.2g","S7.2h","S7.2i","S7.2j","S7.2k"
  ))) %>%
  mutate(col_names = as.character(col_names)) 

model_df %>% 
  select(Country,group_df$col_names) %>%
  gather('variable', 'category', -Country) %>%
  group_by(variable, category, Country) %>%
  tally() %>%
  filter(category != 'FALSE') %>%
  spread('Country', 'n') %>% 
write.xlsx('environmental_data/variables_categories.xlsx')

# Analysis
# predictive modeling
big_pred_df <- data.frame()
tic()
model_df <- model_df %>% 
  mutate(S5.12_high_all = S5.12_high,
         S5.12_low_all = S5.12_low,
         A5_all = A5,
         T5_all = T5,
         F5_all = F5,
         S5.9_all = S5.9) %>%
  select(all_of(c(group_df$col_names, 'ATF3', 'F3', 'T3','A5','T5','F5','S5.9', 
                  'S5.4', 'S5.12_high', 'S5.12_low','S3.3', 'S3.3_low','A3')))
alpha <- 0.4
lambda <- 0.5
lambda_sgb <- 0.5
# Function to run the models
run_models <- function(region, alpha, outcome){
  if(region %in% unique(model_df$Country)){
    vul_dfff <- model_df %>% filter(Country %in% region)
    } else {vul_dfff <- model_df}
    print(paste0(region,outcome))
    group_df <- group_df %>%
      mutate(sgl_name = col_names)
    if(outcome %in% c('A5','T5','F5', 'S5.12_high', 'S5.12_low', 'S5.9')){
      vul_dff <- vul_dfff %>% filter(S5.4 == 'TRUE')
      index_df <- group_df %>% 
        filter(col_names != 'S5.4')  %>%
        rbind(data.frame(group = "past decision", index = 203, col_names = c(
          "ATF3"), sgl_name = c("ATF3")))
    } else if(outcome %in% c('S5.4')){
      vul_dff <- vul_dfff
      index_df <- group_df %>% 
        filter(col_names != 'S5.4')  %>%
        filter(group != 'measures') %>%
        rbind(data.frame(group = "past decision", index = 203, col_names = c(
          "ATF3"), sgl_name = c("ATF3")))
    } else if(outcome %in% c('A3', 'T3', 'F3', 'S3.3', 'S3.3_low', 'S3.4')){
      index_df <- group_df %>%
        filter(!(group %in% c('measures')))
      vul_dff <- vul_dfff %>% filter(ATF3 == 'TRUE')
    } else {index_df <- group_df}
    if(region == 'Tunisia'){
      index_df <- index_df %>%
        filter(col_names != 'S8.11_river')
    }
    if(outcome == 'S5.9'){
      index_df <- index_df %>%
        filter(col_names != 'S5.7')
    }
    index_df <- index_df %>%
      filter(str_sub(col_names, 1,4) != str_sub(outcome, 1,4))
    vul_df <- vul_dff %>% select(all_of(outcome), all_of(index_df$col_names))
    # statistical model
    # formulas sgb
    predictors <- index_df$col_names[index_df$col_names != outcome]
    sgb_df_formula <-  prepare_sgb(alpha = alpha, index_df = index_df, outcome_name = outcome)
    sgb_df_formula <- paste0(outcome, '~', sgb_df_formula) %>% as.formula()
    sgb_lambda_formula <- prepare_sgb_lambda(alpha = alpha, lambda = 100, index_df = index_df, outcome_name = outcome)
    sgb_lambda_formula <- paste0(outcome, '~', sgb_lambda_formula) %>% as.formula()
    # prediction 
    train_index <- sample(1:dim(vul_df)[1], dim(vul_df)[1]*0.7)
    
    ## fit sgb df
    sgb_df_start <- Sys.time()
    sgb_df_model_train <- mboost(formula = as.formula(sgb_df_formula),
                                 family = Binomial(link ="logit"), data = vul_df[train_index,],
                                 control = boost_control(mstop = 3000, nu = 0.05))
    cv_folds_train <- cv(model.weights(sgb_df_model_train), type = "kfold", B = 3)
    cv_sgb_df_train <- cvrisk(sgb_df_model_train, folds = cv_folds_train)
    sgb_df_end <- Sys.time()
    sgb_df_model_train <- sgb_df_model_train[mstop(cv_sgb_df_train)]
    
    print('sgb')
    ## fit sgb lambda
    sgb_lambda_start <- Sys.time()
    sgb_lambda_model_train <- mboost(formula = as.formula(sgb_lambda_formula),
                                 family = Binomial(link ="logit"), data = vul_df[train_index,],
                                 control = boost_control(mstop = 3000, nu = 0.05))
    cv_sgb_lambda_train <- cvrisk(sgb_lambda_model_train, folds = cv_folds_train)
    sgb_lambda_end <- Sys.time()
    sgb_lambda_model_train <- sgb_lambda_model_train[mstop(cv_sgb_lambda_train)]
    # fit sgl
    sgl_start <- Sys.time()
    sgl_tuned <- cv_sgl(vul_df[train_index,], alpha = alpha, index_df = index_df, k_folds = 3,
                              nlam=6, verbose = F, outcome = outcome)
    if(is.numeric(sgl_tuned)){
      lambda_opt <- 0.1
    } else{
    lambda_opt <- sgl_tuned$lambdas[sgl_tuned$lldiff == min(sgl_tuned$lldiff)]}
    sgl_model <- fit_sgl(lambda_opt = lambda_opt, index_df = index_df, vul_df = vul_df[train_index,],
                         alpha = alpha, outcome = outcome)
    if(is.character(sgl_model)){
     print('sgl fail...')
    } 
    sgl_end <- Sys.time()
    # prediction:
    pred_df_x <- vul_df[-train_index,] %>%
      select(-outcome) %>% mutate_all(as.numeric) %>% as.matrix()
    pred_df <- vul_df[-train_index,] %>%
      transmute(pred_sgb_df = predict(sgb_df_model_train,vul_df[-train_index,], type = 'response'),
                pred_sgb_lambda = predict(sgb_lambda_model_train,vul_df[-train_index,], type = 'response'),
                pred_sgl = predictSGL(sgl_model,pred_df_x,1))
    # sparsity 
    sel_sgb_df_vars <- sgb_df_model_train$coef() %>% names() %>% str_replace_all('bols.','') %>%
      str_replace_all('..df.*', '') %>% str_split(',') %>% unlist() %>% str_replace_all(' ','') %>% 
      unique() 
    sel_sgb_df <- sel_sgb_df_vars %>% length()
    if(sel_sgb_df != 0){
    sel_sgb_df_df <- index_df %>%
      left_join(data.frame(is_there = 1, name = sel_sgb_df_vars), by = c('col_names' = 'name')) %>%
      mutate(is_there = case_when(is.na(is_there)~0,T~is_there))  %>%
      group_by(index) %>%
      summarize(mean_sel = mean(is_there))
    } else{sel_sgb_df_df <- index_df %>% mutate(is_there = 0) %>% group_by(index) %>%
      summarize(mean_sel = mean(is_there))}
    sel_within_sgb_df <- mean(sel_sgb_df_df$mean_sel[sel_sgb_df_df$mean_sel != 0])
    sel_between_sgb_df_all <- sum(sel_sgb_df_df$mean_sel == 1)
    sel_between_sgb_df_any <- sum(sel_sgb_df_df$mean_sel != 0)
    
    sel_sgb_lambda_vars <- sgb_lambda_model_train$coef() %>% names() %>% str_replace_all('bols.','') %>%
      str_replace_all('..lambda.*', '') %>% str_split(',') %>% unlist() %>% str_replace_all(' ','') %>% 
      unique() 
    sel_sgb_lambda <- sel_sgb_lambda_vars %>% length()
    if(sel_sgb_lambda != 0){
    sel_sgb_lambda_df <- index_df %>%
      left_join(data.frame(is_there = 1, name = sel_sgb_lambda_vars), by = c('col_names' = 'name')) %>%
      mutate(is_there = case_when(is.na(is_there)~0,T~is_there))  %>%
      group_by(index) %>%
      summarize(mean_sel = mean(is_there))
    } else{sel_sgb_lambda_df <- index_df %>% mutate(is_there = 0) %>% group_by(index) %>%
      summarize(mean_sel = mean(is_there))}
    sel_within_sgb_lambda <- mean(sel_sgb_lambda_df$mean_sel[sel_sgb_lambda_df$mean_sel != 0])
    sel_between_sgb_lambda_all <- sum(sel_sgb_lambda_df$mean_sel == 1)
    sel_between_sgb_lmabda_any <- sum(sel_sgb_lambda_df$mean_sel != 0)
    
    sel_sgl <- sum(sgl_model$beta[,1] != 0)
    sgl_sel_df <- data.frame(sel_index =sgl_model$beta[,1],index = index_df$index) %>%
      group_by(index) %>%
      summarize(max_sel = max(abs(sel_index)),
                mean_sel = mean(sel_index != 0))
    sgl_between_any <- sum(sgl_sel_df$mean_sel != 0)
    sgl_between_all <- sum(sgl_sel_df$mean_sel == 1)
    sgl_within <- mean(sgl_sel_df$mean_sel[sgl_sel_df$mean_sel != 0])
    auc_df <- data.frame(
      model = c('sgb lambda', 'sgb df', 'sgl'), lambda = c(10,NA,NA), lambda_sgl = c(NA,NA,lambda_opt), 
      auc = c(auc(response = vul_df[-train_index,][[outcome]],
                  predictor = pred_df$pred_sgb_lambda),
              auc(response = vul_df[-train_index,][[outcome]],
                  predictor = pred_df$pred_sgb_df),
              auc(response = vul_df[-train_index,][[outcome]],
                  predictor = pred_df$pred_sgl)),
      time = c(sgb_lambda_end- sgb_lambda_start, sgb_df_end-sgb_df_start,
               sgl_end-sgl_start),
      mstop = c(mstop(sgb_lambda_model_train), mstop(sgb_df_model_train), NA),
      sel_within = c(sel_within_sgb_lambda, sel_within_sgb_df,sgl_within),
      sel_between_all = c(sel_between_sgb_lambda_all,sel_between_sgb_df_all,sgl_between_all),
      sel_between_any = c(sel_between_sgb_lmabda_any, sel_between_sgb_df_any, sgl_between_any),
      sel = c(sel_sgb_lambda, sel_sgb_df, sel_sgl)
      )
    return(auc_df)
}

#estimate models
library(furrr)
future:::ClusterRegistry("stop")
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(11)
options(future.globals.onReference = "error")
params <- expand_grid(alpha = 0,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9',
                                   'S5.4', 'S5.12_high', 'S5.12_low','S3.3', 'S3.3_low','A3'),
                      region = c('Chile','Tunisia'))
tryCatch(
  expr = {
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
# Run 10 times, since sgl crashes too often to run in one chunk...
saveRDS(results_df,  paste0('environmental_data/results/auc_data0.RDS'))
}, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(2)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.1,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)

saveRDS(results_df,  paste0('environmental_data/results/auc_data01.RDS'))
toc()
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(3)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.2,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data02.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(4)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.3,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data03.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()

plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(5)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.4,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data04.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()

plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(666)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.5,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data05.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(7)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.6,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data06.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(8)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.7,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data07.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(9)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.8,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data08.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()
plan(multisession, workers = availableCores()/2)
tictoc::tic()
set.seed(10)
tryCatch(
  expr = {
params <- expand_grid(alpha = 0.9,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data09.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()

tictoc::tic()
set.seed(11)
tryCatch(
  expr = {
params <- expand_grid(alpha = 1,
                      outcome = c( 'F3', 'T3','A5','T5','F5','S5.9', 'S5.4', 'S5.12_high', 'S5.12_low',
                                   'S3.3', 'S3.3_low','A3'), region = c('Chile','Tunisia'))
results_df <- params %>%
  mutate(
    res = future_pmap(., .f = run_models, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
saveRDS(results_df,  paste0('environmental_data/results/auc_data1.RDS'))
  }, error = function(e){print('fail')})
future:::ClusterRegistry("stop")
toc()

# Figure 3 from mnuscript
res_df <- paste0('environmental_data/results/',list.files('environmental_data/results')) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  mutate(sel_within = case_when(is.nan(sel_within)~ 0,T~sel_within))

res_df %>%
  group_by(alpha, region,model) %>%
  summarize(mean(mstop), mean(auc), mean(sel), mean(time)) 

plot_auc <- res_df %>%
  group_by(model, alpha, region) %>%
  summarize(auc = mean(auc)) %>%
  ggplot(aes(x = alpha, y = auc, color = model, group = model)) + geom_point() + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) + ylim(c(0.65,0.8)) +
  theme_bw() + theme(legend.position = 'top', legend.title = element_blank()) +
  facet_wrap(.~region)
plot_time <- res_df %>%
  group_by(model, alpha, region) %>%
  summarize(time = mean(time)) %>%
  ggplot(aes(x = alpha, y = time, color = model, group = model)) + geom_point() + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) +
  theme_bw() + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Model estimation time in seconds') + facet_wrap(.~region)
plot_sel <- res_df %>%
  group_by(model, alpha, region) %>%
  summarize(sel = mean(sel)) %>%
  ggplot(aes(x = alpha, y = sel, color = model, group = model)) + geom_point() + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) +
  theme_bw() + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Number of selected variables') + facet_wrap(.~region)
plot_sel_within <- res_df %>%
  group_by(model, alpha, region) %>%
  summarize(sel_within = mean(sel_within)) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = sel_within, color = model, group = model)) + geom_point() + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) +
  theme_bw() + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Percentage selected variables within groups') + facet_wrap(.~region)
plot_sel_between_any <-  res_df %>%
  group_by(model, alpha, region) %>%
  summarize(sel_between_any = mean(sel_between_any)) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = sel_between_any, color = model, group = model)) + geom_point() + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) +
  theme_bw() + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Number of partially selected groups') + facet_wrap(.~region)

plot_sel_between_all <-  res_df %>%
  group_by(model, alpha, region) %>%
  summarize(sel_between_all = mean(sel_between_all)) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = sel_between_all, color = model, group = model)) + geom_point() + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) +
  theme_bw() + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Number of fully selected groups') + facet_wrap(.~region)

final_figure <- grid.arrange(plot_auc, plot_sel,plot_time, plot_sel_within, plot_sel_between_any, plot_sel_between_all, ncol = 3)
ggsave(filename = 'environmental_data/env2_figure.png',plot = final_figure, dpi = 900, width= 12, height = 9)
