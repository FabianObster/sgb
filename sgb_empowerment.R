library(tidyverse)
library(mboost)
library(SGL)
library(readxl)
# df_raw <- read_xlsx('../data_empowerment_coded.xlsx') %>%
#   slice(-1)
# 
# df <- df_raw %>%
#   filter(!is.na(SO01)) %>%
#   select(contains('VA'), contains('KE'), contains('FO'),
#          contains('IP'), contains('IR')) %>%
#   mutate_all(as.numeric)
# df <- df %>%
#   mutate(Empowerment = rowMeans(df %>% select(contains('VA'), contains('KE'),
#                                               contains('FO')), na.rm=T),
#          EVA = rowMeans(df %>% select(contains('VA')), na.rm=T),
#          EKE = rowMeans(df %>% select(contains('KE')), na.rm=T),
#          EFO = rowMeans(df %>% select(contains('FO')), na.rm=T)
#   ) %>%
#   select(-starts_with('VA'), -starts_with('KE'), -starts_with('FO')) %>%
#   filter(!is.na(EFO), !is.na(EVA), !is.na(EKE)) %>%
#   mutate_all(function(x){case_when(is.na(x)~mean(x,na.rm=T), T~x)})
# saveRDS(df, 'empowerment_data.R')

# Analysis start
df <- readRDS('empowerment_data.R')
model_df <- df %>% mutate(outcome = Empowerment)
index <- c(rep(1:10, each = 4),rep(11, each = 20))
# helper functions start
cv_sgl <- function(model_df, outcome = 'Empowerment', alpha = 0.95, 
                   index = index, k_folds = 5, nlam = 10, verbose = F){
  x <- model_df %>% 
    dplyr::select(-starts_with('E')) %>%
    as.matrix()
  y <- model_df[[outcome]] 
  cv_sgl <- cvSGL(data = list(x = x, y = y),
                  index = index,
                  alpha = alpha, nfold = k_folds, nlam = nlam, verbose = F
  ) 
  cv_sgl
} 

fit_sgl <- function(lambda_opt, model_df, alpha, index, outcome = 'Empowerment'){
  x <- model_df %>%
    dplyr::select(-starts_with('E')) %>%
    as.matrix()
  y <- model_df[[outcome]]
  SGL(data = list(x = x, y = y),
      index = index,
      lambdas = c(lambda_opt, lambda_opt),
      alpha = alpha,
  ) %>% return() 
}
fit_sgb_lambda <- function(alpha, model_df, index, outcome = 'Empowerment', lambda,
                       stop_par, nu){
    model_df <- model_df %>%
      select(-contains('E'), all_of(outcome))
    index_df <- data.frame(index,name = colnames(model_df %>% select(-all_of(outcome))))
    term_df <- index_df %>% 
      mutate(regularization = 1) %>%
      group_by(index) %>%
      mutate(regularization = sqrt(sum(regularization))) %>%
      reframe(term = paste0(name, collapse = ','), regularization = regularization) %>%
      mutate(term = paste0('bols(',term, ',lambda=',regularization*lambda*(alpha), ',intercept=F)'))
    group_term <- paste(term_df$term, collapse = '+')
    indi_term <- paste0('bols(',index_df$name, ',lambda=',lambda*(1-alpha), ',intercept=F)')
    indi_term <- paste(indi_term, collapse = '+')
    sgb_formula <- as.formula(paste0(outcome,'~',indi_term,'+',group_term))
    mboost(sgb_formula, data = model_df, control = boost_control(mstop = stop_par, nu = nu))
}
fit_sgb_df <- function(alpha, model_df, index, outcome = 'Empowerment', stop_par, nu){
  model_df <- model_df %>%
    select(-contains('E'), all_of(outcome))
  index_df <- data.frame(index,name = colnames(model_df %>% select(-all_of(outcome))))
  term_df <- index_df %>% 
    group_by(index) %>%
    reframe(term = paste0(name, collapse = ',')) %>%
    mutate(term = paste0('bols(',term, ',df=',(1-alpha), ',intercept=F)'))
  group_term <- paste(term_df$term, collapse = '+')
  indi_term <- paste0('bols(',index_df$name, ',df=',alpha, ',intercept=F)')
  indi_term <- paste(indi_term, collapse = '+')
  sgb_formula <- as.formula(paste0(outcome,'~',indi_term,'+',group_term))
  mboost(sgb_formula, data = model_df, control = boost_control(mstop = stop_par, nu = nu))
}
cv_mboost <- function(mboost_model, k_folds =5){
  cv_folds <- cv(model.weights(mboost_model), type = "kfold",B = k_folds)
  cv_mboost <- cvrisk(mboost_model, folds = cv_folds)
  return(mboost_model[mstop(cv_mboost)])
}
# helper functions end
# train test split
df <- readRDS('empowerment_data.R')
set.seed(111)
train_index <- sample(1:dim(df)[1], dim(df)[1]*0.5)
model_df <- df[train_index,] %>% 
  mutate_all(function(x){as.numeric(scale(x))})
test_df <- df[-train_index,] %>% 
  mutate_all(function(x){as.numeric(scale(x))})
test_df_x <- test_df %>% 
  select(-starts_with('E')) %>%
  as.matrix()
model_df_x <- model_df %>% 
  select(-starts_with('E')) %>%
  as.matrix()
alpha <- 0.5
# function to fit/tune the models
run_sim <- function(alpha, outcome, stop_par = 2000, nu = 0.01) {
  
  sgl_tuned <- cv_sgl(model_df, alpha = alpha, index = index, k_folds = 5, nlam = 10, verbose = F)
lambda_opt <- sgl_tuned$lambdas[sgl_tuned$lldiff == min(sgl_tuned$lldiff)]
  lambdas <- sgl_tuned$lambdas
  results <- data.frame()
for(i in 1:10){
  lambda <- lambdas[i]
  sgb_df_model <- fit_sgb_df(alpha = alpha+0.01*(i-1), model_df =model_df, index, outcome = outcome,
                             stop_par = stop_par, nu = nu)
  cv_sgb_df_model <- cv_mboost(sgb_df_model)
  sgb_df <- sgb_df_model[mstop(cv_sgb_df_model)]
  sel_sgb_df_vars <- sgb_df_model$coef() %>% names() %>% str_replace_all('bols.','') %>%
    str_replace_all(', int.*', '') %>% str_split(',') %>% unlist() %>% str_replace_all(' ','') %>% 
    unique() 
  sel_sgb_df <- sel_sgb_df_vars %>% length()
  sel_sgb_df_all <- sgb_df_model$coef() %>% names() %>% str_replace_all('bols.','') 
  sel_sgb_df_df <- data.frame(index,name= colnames(df[,1:60])) %>%
    left_join(data.frame(is_there = 1, name = sel_sgb_df_vars), by = 'name') %>%
    mutate(is_there = case_when(is.na(is_there)~0,T~is_there))  %>%
    group_by(index) %>%
    summarize(mean_sel = mean(is_there))
  sel_within_sgb_df <- mean(sel_sgb_df_df$mean_sel)
  sel_between_sgb_df_all <- sum(sel_sgb_df_df$mean_sel == 1)
  sel_between_sgb_df_any <- sum(sel_sgb_df_df$mean_sel == 0)
  results <- results %>% rbind( data.frame(model = 'sgb df', Alpha = alpha, Outcome = outcome, lambda = NA,
                        mse = sum((test_df[[outcome]]-predict(sgb_df, test_df))^2),
                        mse_train = sum((model_df[[outcome]]-predict(sgb_df, model_df))^2),
                        sel = sel_sgb_df, sel_within =  sel_within_sgb_df,
                        sel_between_all = sel_between_sgb_df_all,
                        sel_between_any = sel_between_sgb_df_any, Mstop = mstop(sgb_df),
                        lambda_opt = NA, lambda_sgb = NA, alpha_sgb_df = alpha+0.01*(i-1)))
  
sgl_model <- fit_sgl(lambda_opt = lambda, index = index, model_df = model_df,
                     alpha = alpha, outcome = outcome)
sgb_lambda_model <- fit_sgb_lambda(alpha = alpha, model_df, index, outcome = outcome, lambda = i*50,
                                   stop_par = stop_par, nu = nu)
cv_sgb_lambda_model <- cv_mboost(sgb_lambda_model)
sgb_lambda <- sgb_lambda_model[mstop(cv_sgb_lambda_model)]

sel_sgb_lambda_vars <- sgb_lambda_model$coef() %>% names() %>% str_replace_all('bols.','') %>%
  str_replace_all(', int.*', '') %>% str_split(',') %>% unlist() %>% str_replace_all(' ','') %>% 
  unique() 
sel_sgb_lambda <- sel_sgb_lambda_vars %>% length()
sel_sgb_lambda_all <- sgb_lambda_model$coef() %>% names() %>% str_replace_all('bols.','') 
sel_between_sgb_lambda <- sum(nchar(sel_sgb_df_all) > 33)
sel_sgb_lambda_df <- data.frame(index,name= colnames(df[,1:60])) %>%
  left_join(data.frame(is_there = 1, name = sel_sgb_lambda_vars), by = 'name') %>%
  mutate(is_there = case_when(is.na(is_there)~0,T~is_there)) %>%
  group_by(index) %>%
  summarize(mean_sel = mean(is_there))
sel_within_sgb_lambda <- mean(sel_sgb_lambda_df$mean_sel)
sel_between_sgb_lambda_all <- sum(sel_sgb_lambda_df$mean_sel == 1)
sel_between_sgb_lambda_any <- sum(sel_sgb_lambda_df$mean_sel != 0)
sel_sgl <- sum(sgl_model$beta[,1] != 0)
sgl_sel_df <- data.frame(sel_index =sgl_model$beta[1],index) %>%
  group_by(index) %>%
  summarize(max_sel = max(abs(sel_index)),
            mean_sel = mean(sel_index != 0))
sgl_between_any <- sum(sgl_sel_df$mean_sel != 0)
sgl_between_all <- sum(sgl_sel_df$mean_sel == 0)
sgl_within <- mean(sgl_sel_df$mean_sel)
results <- results %>%
  rbind(
    data.frame(model = c('sgb lambda', 'sgl'), Alpha = alpha, Outcome = outcome, lambda = lambda,
               mse = c(sum((test_df[[outcome]]-predict(sgb_lambda, test_df))^2),
                       sum((test_df[[outcome]]-predictSGL(sgl_model,test_df_x,1))^2)),
               mse_train = c(sum((model_df[[outcome]]-predict(sgb_lambda, model_df))^2),
                       sum((model_df[[outcome]]-predictSGL(sgl_model,model_df_x,1))^2)),
               sel = c(sel_sgb_lambda, sel_sgl), sel_within =  c(sel_within_sgb_lambda, sgl_within),
               sel_between_all = c(sel_between_sgb_lambda_all, sgl_between_all),
               sel_between_any = c(sel_between_sgb_lambda_any, sgl_between_any),
               Mstop = mstop(sgb_lambda),
               lambda_opt = lambda_opt, lambda_sgb = i*50, alpha_sgb_df = NA)
  ) 
}
  return(results)
}

# estimate models
library(furrr)
plan(multisession, workers = 15)
tictoc::tic()
params <- tidyr::expand_grid(alpha = seq(0,1, 0.1),
                              outcome = 'Empowerment') 
set.seed(5)
results_df <- params %>%
  mutate( 
    res = future_pmap(., .f = run_sim, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
plan(sequential)
# Figure from publication
plot_1 <- results_df %>%
  group_by(model, Alpha) %>%
  filter(mse_train == min(mse_train))%>%
  mutate(mse = mse/dim(test_df)[1]) %>%
  ggplot(aes(x = Alpha, color = model, y = mse, group= interaction(model))) + 
  ylab('MSE') + theme_bw(base_size = 14)+ ylim(c(0.5,0.8)) + xlab('alpha') +
  geom_point() + geom_line() + scale_color_brewer(palette='Dark2') +
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  theme(legend.position='top',legend.title = element_blank()) 

plot_2 <- results_df %>%
  group_by(model, alpha) %>%
  filter(mse_train == min(mse_train))%>%
  #filter(!(model == 'sgl' & lambda != lambda_opt) | is.na(lambda)) %>%
  ggplot(aes(x = Alpha, color = model, y = sel, group= interaction(model))) + 
  ylab('Number of selected variables') + theme_bw(base_size = 14)+ ylim(c(0,60)) +
  xlab('alpha') + geom_point() + geom_line()  + scale_color_brewer(palette='Dark2')+
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  theme(legend.position='top',legend.title = element_blank()) 
library(gridExtra)
grid <- grid.arrange(plot_1,plot_2, layout_matrix = matrix(c(1,2),ncol = 2))
ggsave('sgl_sgb_empowerment.png',grid,width = 14, height = 7, dpi = 900)
# Further analysis and figures not shown in manuscript
results_df %>%
  group_by(model, alpha) %>%
  filter(mse_train == min(mse_train))%>%
  #filter(!(model == 'sgl' & lambda != lambda_opt) | is.na(lambda)) %>%
  ggplot(aes(x = Alpha, color = model, y = sel_within, group= interaction(model))) + 
  ylab('MSE') + theme_bw(base_size = 14)+ xlab('alpha') +
  geom_point() + geom_line()

results_df %>%
  group_by(model, alpha) %>%
  filter(mse_train == min(mse_train))%>%
  ggplot(aes(x = alpha, color = model, y = sel_between_all, group= interaction(model))) + 
  ylab('MSE') + theme_bw(base_size = 14)+ xlab('alpha') +
  geom_point() + geom_line()

results_df %>%
  group_by(model, alpha) %>%
  filter(mse_train == min(mse_train))%>%
  ggplot(aes(x = alpha, shape = model, linetype = model, y = sel_between_any, group= interaction(model))) + 
  ylab('MSE') + theme_bw(base_size = 14)+ xlab('alpha') +
  geom_point() + geom_line()
