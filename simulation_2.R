library(tidyverse)
library(mboost)
library(SGL)
library(gridExtra)
library(writexl)
library(sparsegl)
index <- c(rep(1:10, each = 4),rep(11, each = 20))
# helper functions start
cv_sgl_error <- function(x,y, alpha = 0.95, 
                   index = index, k_folds = 3, nlam = 100, verbose = F){
  cv_sgl <- cvSGL(data = list(x = x, y = y),
                  index = index$index,
                  alpha = alpha, nfold = k_folds, nlam = nlam, verbose = F
  ) 
  cv_sgl
} 
cv_sparsegl_error <- function(alpha = 0.95,x,y,
                         index = index, k_folds = 3, nlam = 100, verbose = F){
  cv_sparsegl <- sparsegl::cv.sparsegl(x = x,y = y,group = index$index,asparse = alpha, nfolds = k_folds,
                        nlambda = nlam) 
  cv_sparsegl
} 
cv_sgl <- possibly(cv_sgl_error, otherwise = 0)
cv_sparsegl <- possibly(cv_sparsegl_error, otherwise = 0)
fit_sgl_error <- function(lambda_opt, x, y, alpha, index, model_df){
  SGL(data = list(x = x, y = y),
      index = index$index,
      lambdas = c(lambda_opt, lambda_opt),
      alpha = alpha,
  ) %>% return() 
}
fit_sparsegl_error <- function(lambda_opt, x, y, alpha, index, model_df){
  return(sparsegl(x=x,y=y,group = index$index,lambda = lambda_opt, asparse = alpha))
}
fit_sgl <- possibly(fit_sgl_error, otherwise = 'error sgl')
fit_sparsegl <- possibly(fit_sparsegl_error, otherwise = 'error sparsegl')
fit_sgb_lambda <- function(alpha, model_df, index, lambda,
                           stop_par, nu){
  term_df <- index %>% 
    mutate(regularization = 1) %>%
    group_by(index) %>%
    transmute(term = paste0(colname, collapse = ','), regularization=regularization) %>%
    slice(1) %>% ungroup() %>%
    mutate(regularization = sqrt(sum(regularization))) %>%
    mutate(term = paste0('bols(',term, ',lambda=',regularization*lambda*(1-alpha), ',intercept=F)'))
  group_term <- paste(term_df$term, collapse = '+')
  indi_term <- paste0('bols(',index$colname, ',lambda=',lambda*alpha, ',intercept=F)')
  indi_term <- paste(indi_term, collapse = '+')
  sgb_formula <- as.formula(paste0('y','~',indi_term,'+',group_term))
  mboost(sgb_formula, data = model_df, control = boost_control(mstop = stop_par, nu = nu))
}
fit_sgb_df <- function(alpha, model_df, index, stop_par, nu){
  index_df <- data.frame(index,colname = colnames(model_df %>% select(-y)))
  term_df <- index_df %>% 
    group_by(index) %>%
    reframe(term = paste0(colname, collapse = ',')) %>%
    mutate(term = paste0('bols(',term, ',df=',(1-alpha), ',intercept=F)'))
  group_term <- paste(term_df$term, collapse = '+')
  indi_term <- paste0('bols(',index_df$colname, ',df=',alpha, ',intercept=F)')
  indi_term <- paste(indi_term, collapse = '+')
  sgb_formula <- as.formula(paste0('y','~',indi_term,'+',group_term))
  mboost(sgb_formula, data = model_df, control = boost_control(mstop = stop_par, nu = nu))
}
cv_mboost <- function(mboost_model, k_folds =3){
  cv_folds <- cv(model.weights(mboost_model), type = "kfold",B = k_folds)
  cv_mboost <- cvrisk(mboost_model, folds = cv_folds)
  return(mboost_model[mstop(cv_mboost)])
}
get_beta_index <- function(n_group_full = 5, n_group_empty = 5, n_group_half = 5, 
                           n_vars_full = 15, n_vars_empty = 15, n_vars_half=15,
                           group_variation = 'no'){
  beta_full <- rep(rep(1,n_group_full), each = n_vars_full)
  index_full <- rep(1:n_group_full, each = n_vars_full)
  beta_half <- rep(c(rep(1,round(n_vars_half/3,0)), rep(0,max(0,n_vars_half-round(n_vars_half/3,0)))), n_group_half)
  index_half <- rep((n_group_full+1):(n_group_full+n_group_half), each = n_vars_half)
  beta_empty <- rep(rep(0,n_group_empty), each = n_vars_empty)
  index_empty <- rep((n_group_full+n_group_half+1):(n_group_full+n_group_half+n_group_empty), each = n_vars_empty)
  ret <- data.frame(beta = c(beta_full,beta_half,beta_empty), 
             index = as.numeric(c(index_full, index_half,index_empty)))
  ret <- ret %>%
    mutate(colname = paste0('V',1:dim(ret)[1]))
}
get_data <- function(index, n, dependence = 0){
  X <- as.data.frame(mvtnorm::rmvnorm(
    n = n,
    mean = rep(0,dim(index)[1]),
    sigma = matrix(diag(rep(1-dependence,dim(index)[1])), ncol = dim(index)[1])+
      matrix(dependence, ncol = dim(index)[1],nrow=dim(index)[1])
  )) %>%
    mutate_all(function(x){as.numeric(scale(x))}) %>%
    as.matrix()
  y <- X%*%index[,1]
  noise <- rnorm(n=n)
  if(var(y) == 0){
    k <- 1
  } else{
  k <- as.numeric(sqrt(var(y)/(4*var(noise))))}
  y_new <- y+k*rnorm(n=n)
  return(list(X = X,y = y_new))
}
# helper functions end
# Function to run simulation with various parameters
alpha <- 0.2
run_sim <- function(alpha = 0.5, stop_par = 2500, nu = 0.01, n_group_empty = 5,
                    n_group_full = 5, n_group_half = 5, n_vars_empty = 15,
                    n_vars_full = 15, n_vars_half = 15, n = 50, dependence = 0, 
                    nlam = 100, k_folds = 3, iteration, scenario) {
  index <- get_beta_index(n_group_empty = n_group_empty, n_group_full = n_group_full,
                          n_group_half = n_group_half, n_vars_full = n_vars_full,
                          n_vars_empty = n_vars_empty, n_vars_half = n_vars_half)
  dat <- get_data(index, n=n, dependence=dependence)
  model_df <- data.frame(dat[['X']]) %>%
    mutate(y = as.numeric(dat[['y']]))
  # sgl_start <- Sys.time()
  # sgl_tuned <- cv_sgl(x = dat$X, y=dat$y, alpha = alpha, index = index, k_folds = k_folds, nlam = nlam, verbose = F)
  # if(is.numeric(sgl_tuned)){
  #   lambda_opt <- 0.1
  #   lambdas <- rep(0.1,nlam)
  #   is_error_cv_sgl <- T
  # } else{
  # lambda_opt <- sgl_tuned$lambdas[sgl_tuned$lldiff == min(sgl_tuned$lldiff)]
  # lambdas <- sgl_tuned$lambdas
  # is_error_cv_sgl <- F
  # }
  # sgl_end <- Sys.time()
  
  sparsegl_start <- Sys.time()
  sparsegl_tuned <- cv_sparsegl_error(x = dat$X, y = dat$y, alpha = alpha, index = index, k_folds = k_folds, nlam = nlam, verbose = F)
  if(is.numeric(sparsegl_tuned)){
    lambda_opt <- 0.1
    lambdas <- rep(0.1,nlam)
    is_error_cv_sparsegl <- T
  } else{
    lambda_opt_sparsegl <- sparsegl_tuned$lambda.min
    lambdas_sparsegl <- sparsegl_tuned$lambda
    is_error_cv_sparsegl <- F
  }
  sparsegl_end <- Sys.time()
  # results <- data.frame()
  # for(i in 1:100){
  #   lambda <- lambdas[i]
    # sgb_df_start <- Sys.time()
    # sgb_df_model <- fit_sgb_df(alpha = alpha+0.01*(i-1), model_df =model_df, index,
    #                            stop_par = stop_par, nu = nu)
    # sgb_df_end <- Sys.time()
    # cv_sgb_df_model <- cv_mboost(sgb_df_model)
    # sgb_df <- sgb_df_model[mstop(cv_sgb_df_model)]
    # sgb_df_indis <- sgb_df_model$coef()[lengths(sgb_df_model$coef()) == 1] %>% unname() %>% unlist()
    # sgb_df_indis <- data.frame('beta_indis'=sgb_df_indis) %>% rownames_to_column()
    # sgb_df_groups <- sgb_df_model$coef()[lengths(sgb_df_model$coef()) > 1] %>% unname() %>% unlist()
    # sgb_df_groups <- data.frame('beta_groups'=sgb_df_groups) %>% rownames_to_column()
    # if(is.null(sgb_df_groups$beta_groups)){sgb_df_groups$beta_groups <- numeric()}
    # if(is.null(sgb_df_indis$beta_indis)){sgb_df_indis$beta_indis <- numeric()}
    # sel_data_sgb_df <- index %>% left_join(sgb_df_indis, by = c('colname'='rowname')) %>%
    #   left_join(sgb_df_groups, by = c('colname'='rowname')) %>%
    #   mutate_if(is.numeric, function(x){case_when(is.na(x)~0,T~x)}) %>%
    #   mutate(beta_sgb = beta_indis+beta_groups)
    # results <- results %>% 
    #   rbind( data.frame(model = 'sgb df', Alpha = alpha,lambda = NA,
    #                     mse = sum((sel_data_sgb_df$beta_sgb-sel_data_sgb_df$beta)^2),
    #                     correct_classified = mean(sel_data_sgb_df$beta == (sel_data_sgb_df$beta_sgb != 0)),
    #                     correct_zeros = sum(sel_data_sgb_df$beta == 0 & sel_data_sgb_df$beta_sgb == 0)/
    #                       sum(sel_data_sgb_df$beta == 0),
    #                     correct_effects = sum(sel_data_sgb_df$beta != 0 & sel_data_sgb_df$beta_sgb != 0)/
    #                       sum(sel_data_sgb_df$beta != 0), Mstop = mstop(sgb_df),
    #                     lambda_opt = NA, lambda_sgb = NA, alpha_sgb_df = alpha+0.01*(i-1),
    #                     is_error_sgl = NA, is_error_cv_sgl = NA, time = sgb_df_end-sgb_df_start))
    # sgl_model <- fit_sgl_error(lambda_opt = lambda_opt_sparsegl, index = index, x = dat$X, y = dat$y,
    #                      alpha = alpha)
    sparsegl_model <- fit_sparsegl_error(x = dat$X, y = dat$y, lambda_opt = lambda_opt_sparsegl, index = index,
                         alpha = alpha)
    # sgb_lambda_start <- Sys.time()
    # sgb_lambda_model <- fit_sgb_lambda(alpha = alpha, model_df, index, lambda = i*50,
    #                                    stop_par = stop_par, nu = nu)
    # sgb_lambda_end <- Sys.time()
    # cv_sgb_lambda_model <- cv_mboost(sgb_lambda_model)
    # sgb_lambda <- sgb_lambda_model[mstop(cv_sgb_lambda_model)]
    # #
    # sgb_lambda_indis <- sgb_lambda_model$coef()[lengths(sgb_lambda_model$coef()) == 1] %>% unname() %>% unlist()
    # sgb_lambda_indis <- data.frame('beta_indis'=sgb_lambda_indis) %>% rownames_to_column()
    # sgb_lambda_groups <- sgb_lambda_model$coef()[lengths(sgb_lambda_model$coef()) > 1] %>% unname() %>% unlist()
    # sgb_lambda_groups <- data.frame('beta_groups'=sgb_lambda_groups) %>% rownames_to_column()
    # if(is.null(sgb_lambda_groups$beta_groups)){sgb_lambda_groups$beta_groups <- numeric()}
    # if(is.null(sgb_lambda_indis$beta_indis)){sgb_lambda_indis$beta_indis <- numeric()}
    # sel_data_sgb_lambda <- index %>% left_join(sgb_lambda_indis, by = c('colname'='rowname')) %>%
    #   left_join(sgb_lambda_groups, by = c('colname'='rowname')) %>%
    #   mutate_if(is.numeric, function(x){case_when(is.na(x)~0,T~x)}) %>%
    #   mutate(beta_sgb = beta_indis+beta_groups)
    # results <- results %>% 
    #   rbind( data.frame(model = 'sgb lambda', Alpha = alpha,lambda = lambda,
    #                     mse = sum((sel_data_sgb_lambda$beta_sgb-sel_data_sgb_lambda$beta)^2),
    #                     correct_classified = mean(sel_data_sgb_lambda$beta == (sel_data_sgb_lambda$beta_sgb != 0)),
    #                     correct_zeros = sum(sel_data_sgb_lambda$beta == 0 & sel_data_sgb_lambda$beta_sgb == 0)/
    #                       sum(sel_data_sgb_lambda$beta == 0),
    #                     correct_effects = sum(sel_data_sgb_lambda$beta != 0 & sel_data_sgb_lambda$beta_sgb != 0)/
    #                       sum(sel_data_sgb_lambda$beta != 0), Mstop = mstop(sgb_lambda),
    #                     lambda_opt = lambda_opt, lambda_sgb = i*50, alpha_sgb_df = NA,
    #                     is_error_sgl = NA, is_error_cv_sgl = NA, time = sgb_lambda_end-sgb_lambda_start))
    ###
    # if(is.character(sgl_model)){
    #   sgl_sel <- data.frame(beta = 0,colname=index$colname)
    #   is_error_sgl <- TRUE
    # } else{
    #   sgl_sel <- index %>%
    #     left_join(data.frame(beta_sgl = sgl_model$beta[,1],colname=index$colname), by = 'colname')
    #   is_error_sgl <- FALSE}
    # results <-data.frame(model = 'sgl', Alpha = alpha,lambda = lambda_opt,
    #                    mse = sum((sgl_sel$beta_sgl-sgl_sel$beta)^2),
    #                    correct_classified = mean(sgl_sel$beta == (sgl_sel$beta_sgl != 0)),
    #                    correct_zeros = sum(sgl_sel$beta == 0 & sgl_sel$beta_sgl == 0)/
    #                      sum(sgl_sel$beta == 0),
    #                    correct_effects = sum(sgl_sel$beta != 0 & sgl_sel$beta_sgl != 0)/
    #                      sum(sgl_sel$beta != 0), Mstop = NA,
    #                    lambda_opt = lambda_opt, lambda_sgb = NA, alpha_sgb_df = NA,
    #                    is_error_sgl = is_error_sgl, is_error_cv_sgl = is_error_cv_sgl,
    #                    time = sgl_end-sgl_start)
    if(is.character(sparsegl_model)){
      sparsegl_sel <- data.frame(beta = 0,colname=index$colname)
      is_error_sparsegl <- TRUE
    } else{
      sparsegl_sel <- index %>%
        left_join(data.frame(beta_sparsegl = sparsegl_model$beta[,1],colname=index$colname), by = 'colname')
      is_error_sparsegl <- FALSE}
    results_sparsegl <- data.frame(model = 'sparsegl', Alpha = alpha,lambda = lambda_opt_sparsegl,
                         mse = sum((sparsegl_sel$beta_sparsegl-sparsegl_sel$beta)^2),
                         correct_classified = mean(sparsegl_sel$beta == (sparsegl_sel$beta_sparsegl != 0)),
                         correct_zeros = sum(sparsegl_sel$beta == 0 & sparsegl_sel$beta_sparsegl == 0)/
                           sum(sparsegl_sel$beta == 0),
                         correct_effects = sum(sparsegl_sel$beta != 0 & sparsegl_sel$beta_sparsegl != 0)/
                           sum(sparsegl_sel$beta != 0), Mstop = NA,
                         lambda_opt = lambda_opt_sparsegl, lambda_sgb = NA, alpha_sgb_df = NA,
                         is_error_sparsegl = is_error_sparsegl, is_error_cv_sparsegl = is_error_cv_sparsegl,
                         time = sparsegl_end-sparsegl_start)
  return(results_sparsegl)
}

library(furrr)
plan(multisession)
iterations <- 15
# Define the 12 data scenarios
params <- tidyr::expand_grid(alpha = seq(0,1, 0.1), stop_par = 2500,########### 
                             nu = 0.03, n_group_empty = 5,
                             n_group_full = 5, n_group_half = 5, n_vars_empty = 15,
                             n_vars_full = 15, n_vars_half = 15, n = 50,########
                             dependence = 0, 
                             nlam = 10,###########################################
                             k_folds = 3,iteration = 1:iterations, scenario = 1) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_vars_full = 5,
                               n_vars_half = 5,iteration = 1:iterations, scenario = 2)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_vars_empty = 5,
                               n_vars_full = 5,iteration = 1:iterations, scenario =3)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_vars_empty = 5,
                               n_vars_half = 5,iteration = 1:iterations, scenario = 4)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_group_empty = 5,
                               n_group_full = 2, n_group_half = 2,iteration = 1:iterations,
                               scenario = 5)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_group_full = 5,
                               n_group_empty = 2, n_group_half = 2, iteration = 1:iterations,
                               scenario = 6)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_group_half = 5,
                               n_group_full = 2, n_group_empty = 2, iteration = 1:iterations,
                               scenario = 7) ) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_group_half = 0,
            n_vars_half = 0, n_group_full = 0, n_vars_full = 0,iteration = 1:iterations,
            scenario = 8)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n_group_half = 0,
            n_vars_half = 0, n_group_empty = 0, n_vars_empty = 0,iteration = 1:iterations,
            scenario = 9)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), n=500,iteration = 1:iterations,
                               scenario = 10)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), dependence=0.5,
                               iteration = 1:iterations, scenario = 11)) %>%
  bind_rows(tidyr::expand_grid(alpha = seq(0,1, 0.1), dependence=0.95,
                               iteration = 1:iterations, scenario = 12))
params$stop_par[is.na(params$stop_par)] <- 2500
params$nu[is.na(params$nu)] <- 0.05
params$n_group_empty[is.na(params$n_group_empty)] <- 5
params$n_group_full[is.na(params$n_group_full)] <- 5
params$n_group_half[is.na(params$n_group_half)] <- 5
params$n_vars_empty[is.na(params$n_vars_empty)] <- 15
params$n_vars_full[is.na(params$n_vars_full)] <- 15
params$n_vars_half[is.na(params$n_vars_half)] <- 15
params$n[is.na(params$n)] <- 50
params$dependence[is.na(params$dependence)] <- 0
params$nlam[is.na(params$nlam)] <- 10
params$k_folds[is.na(params$k_folds)] <- 3

# run simulation in parallel
set.seed(5)
tictoc::tic()
results_df <- params %>%
  mutate( 
    res = future_pmap(., .f = run_sim, .progress = T, .options=furrr_options(seed = TRUE))
  ) %>%
  unnest(cols = res)
tictoc::toc()
saveRDS(file = 'simulation_results/simulation_sparsegl.RDS', results_df)
plan(sequential)

# Analysis of results
results_df <- readRDS('simulation_results/simulation_sparsegl.RDS') %>%
  mutate(time = time/5) %>%
  bind_rows(readRDS('simulation_results/simulation_02.RDS') %>%
              filter(model != 'sgl'))



res_df <- results_df %>%
  mutate(mse = mse/n,
         rmse = sqrt(mse),
         alpha = case_when(model == 'sgb lambda' ~ 1-alpha, T~alpha))%>%
  mutate(alpha_sgb_df = case_when(is.na(alpha_sgb_df)~ 0,T~alpha_sgb_df )) %>%
  mutate(alpha = case_when((model == 'sgb df' & alpha_sgb_df != 0 & alpha == 0)~ 0.1, T~alpha)) %>%
  mutate(alpha = case_when((model == 'sgb df' & alpha_sgb_df != 1 & alpha == 1)~ 0.9, T~alpha)) %>%
  group_by(alpha,n_group_empty,n_group_full, n_group_half, n_vars_empty,
           n_vars_full,n_vars_half,n,dependence, model, scenario, iteration) %>%
  filter((model != 'sgl' & rmse == min(rmse))) 

# Figure 4 for manuscript
plot_rmse <- res_df %>%
  group_by(scenario) %>%
  mutate(rmse = as.numeric(scale((rmse)))) %>%
  ungroup() %>%
  group_by(alpha,n_group_empty,n_group_full, n_group_half, n_vars_empty,
           n_vars_full,n_vars_half,n,dependence, model, scenario) %>%
  summarize(mean_rmse = mean(rmse)) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = mean_rmse, color = model, group = model, linetype = model)) +
  geom_point(aes(shape=model)) + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) +
  facet_wrap(~scenario) + theme_bw(base_size = 12) + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Average standardized RMSE')

plot_time <- res_df %>%
  group_by(alpha,n_group_empty,n_group_full, n_group_half, n_vars_empty,
           n_vars_full,n_vars_half,n,dependence, model, scenario) %>%
  summarize(time = mean((as.numeric(time)))) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = time, color = model, group = model, linetype = model)) + 
  geom_point(aes(shape=model)) + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) +# ylim(c(0,6)) +
  facet_wrap(~scenario) + theme_bw(base_size = 12) + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('time') 
# 
plot_effects <- res_df %>%
  group_by(alpha,n_group_empty,n_group_full, n_group_half, n_vars_empty,
           n_vars_full,n_vars_half,n,dependence, model, scenario) %>%
  summarize(correct_effects_mean = mean(correct_effects)) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = correct_effects_mean, color = model, group = model, linetype = model)) + 
  geom_point(aes(shape=model)) + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) + ylim(c(0,1)) +
  facet_wrap(~scenario) + theme_bw(base_size = 12) + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Average proportion correct effects') 

plot_zeros <- res_df %>%
  group_by(alpha,n_group_empty,n_group_full, n_group_half, n_vars_empty,
           n_vars_full,n_vars_half,n,dependence, model, scenario) %>%
  summarize(correct_zeros_mean = mean(correct_zeros)) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = correct_zeros_mean, color = model, group = model, linetype = model)) + 
  geom_point(aes(shape=model)) + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) + ylim(c(0,1)) +
  facet_wrap(~scenario) + theme_bw(base_size = 12) + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Average proportion correct zeros')

plot_classified <- res_df %>%
  group_by(alpha,n_group_empty,n_group_full, n_group_half, n_vars_empty,
           n_vars_full,n_vars_half,n,dependence, model, scenario) %>%
  summarize(correct_classified_mean = mean(correct_classified)) %>%
  ungroup() %>%
  ggplot(aes(x = alpha, y = correct_classified_mean, color = model, group = model, linetype = model)) +
  geom_point(aes(shape=model)) + geom_line() +
  scale_color_brewer(palette = 'Dark2') + scale_x_continuous(breaks = seq(0,1,0.2)) + ylim(c(0,1)) +
  facet_wrap(~scenario) + theme_bw(base_size = 12) + theme(legend.position = 'top', legend.title = element_blank()) +
  ylab('Average proportion correct classified')

plot_sgb <- res_df %>%
  filter(model == 'sgb df') %>%
  group_by(scenario, alpha_sgb_df) %>%
  summarize('correct effects' = mean(correct_effects),
         'correct zeros' = mean(correct_zeros),
         'correct classified' = mean(correct_classified)) %>%
  gather(value = 'Proportion', key = 'correct',-scenario,-alpha_sgb_df) %>%
  ggplot(aes(x = alpha_sgb_df, y = Proportion, color = correct)) + geom_point(size = 0.2) +
  geom_smooth(span = 0.1,se = F, linewidth = 0.4, alpha = 2) + xlab('alpha sgb df') + ylim(c(0,1)) +
  scale_color_brewer(palette = 'Accent') + scale_x_continuous(breaks = seq(0,1,0.2)) +
  facet_wrap(~scenario) + theme_bw() +  theme(legend.position = 'top', legend.title = element_blank()) 

big_plot <- gridExtra::grid.arrange(plot_rmse,plot_classified, plot_zeros, plot_effects)
ggsave(plot = big_plot, filename = 'simulation_figures_tables/sim_results_sparsegl.png', dpi = 900, width = 14, height = 10)
#ggsave(plot = plot_sgb, filename = 'simulation_figures_tables/sgb_alpha.png', dpi = 900, width = 7, height = 5)

res_df %>% 
  group_by(scenario, model, alpha) %>%
  summarize(mean_mse = mean(mse), mean_correct_zeros = mean(correct_zeros), 
            mean_correct_classified = mean(correct_classified), 
            mean_correct_effects = mean(correct_effects),
            sd_mse = sd(mse), sd_correct_zeros = sd(correct_zeros), 
            sd_correct_classified = sd(correct_classified), 
            sd_correct_effects = sd(correct_effects),
            min_mse = min(mse), max_correct_classified = max(correct_classified)) %>%
  writexl::write_xlsx('simulation_figures_tables/sim_results_sgl.xlsx')
