set.seed(11)
library(tidyverse)
library(mboost)
library(mvtnorm)
library(pracma)
library(SGL)

# Code Figure 1: SGB Path
get_data <- function(beta_1, beta_2, cor = F) {
  if (cor) {
    df <- as.data.frame(mvtnorm::rmvnorm(
      n = 500,
      mean = c(0, 0),
      sigma = matrix(c(1, 0.7, 0.7,1), ncol = 2)
    ))
  } else{
    df <- as.data.frame(matrix(0, ncol = 2, nrow = 500))
  }
  df <- df %>%
    mutate_all(function(x) {
      rnorm(n = 500, 0, 1)
    })
  df <- df %>%
    mutate(y = beta_1 * V1 + beta_2 * V2 + rnorm(n = 500, 0, 0.5))
  
get_plotdata <- function(sgb_model, alpha) {
  plotdata <- data.frame()
  for (i in 100:1) {
    plotdata <- plotdata %>%
      bind_rows(
        sgb_model[i]$coef() %>%
          as.data.frame() %>%
          mutate(iteration = i) %>%
          rownames_to_column()
      )
  }
  colnames(plotdata) <-
    str_replace_all(colnames(plotdata), 'bols.', '')
  colnames(plotdata) <-
    str_replace_all(colnames(plotdata), '..intercept.*', '')
  if (!('V1' %in% colnames(plotdata))) {
    plotdata <- plotdata %>% mutate(V1 = 0)
  }
  if (!('V2' %in% colnames(plotdata))) {
    plotdata <- plotdata %>% mutate(V2 = 0)
  }
  plotdata <- plotdata %>%
    mutate_if(is.numeric, function(x) {
      case_when(is.na(x) ~ 0, T ~ x)
    }) 
  if(('V1..V2' %in% colnames(plotdata))){
    plotdata <- plotdata %>%
    mutate(
      V1 = case_when(rowname == 'V2' ~ 0, T ~ V1 + V1..V2),
      V2 = case_when(rowname == 'V1' ~ 0, T ~ V2 + V1..V2),
      beta = V1 + V2
    ) %>%
    select(beta, iteration, rowname) %>%
    spread(key = rowname, value = beta) 
  }
  plotdata <- plotdata %>%
    mutate_if(is.numeric, function(x) {
      case_when(is.na(x) ~ 0, T ~ x)
    }) %>%
    bind_rows(data.frame(
      iteration = 0,
      V1 = 0,
      V2 = 0
    )) %>%
    mutate(alpha = alpha)
  return(plotdata)
}


sgb0.35_model <- mboost(y ~bols(V1, V2,intercept = F, df = 0.65)+bols(V1, intercept=F, df = 0.35)+
                         bols(V2, intercept=F, df = 0.35),
                    data = df,
                    control = boost_control(mstop = 100, nu = 0.3))

sgb0.4_model <- mboost(y ~bols(V1, V2,intercept = F, df = 0.6)+bols(V1, intercept=F, df = 0.4)+
                         bols(V2, intercept=F, df = 0.4),
                       data = df,
                       control = boost_control(mstop = 100, nu = 0.3))
sgb0.5_model <- mboost(y ~bols(V1, V2,intercept = F, df = 0.5)+bols(V1, intercept=F, df = 0.5)+
                         bols(V2, intercept=F, df = 0.5),
                       data = df,
                       control = boost_control(mstop = 100, nu = 0.3))
sgb0_model <- mboost(y ~bols(V1, V2,intercept = F, df = 1),
                       data = df,
                       control = boost_control(mstop = 100, nu = 0.3))
sgb1_model <- mboost(y ~bols(V1, intercept=F, df = 1)+bols(V2, intercept=F, df = 1),
                     data = df,
                     control = boost_control(mstop = 100, nu = 0.3))
lm_model <- lm(data = df, formula = y ~ V1 + V2)
summary(lm_model)

plotdata <- get_plotdata(sgb0.35_model, alpha = '0.35') %>% 
  bind_rows(get_plotdata(sgb0.4_model, alpha = '0.4')) %>%
  bind_rows(get_plotdata(sgb0.5_model, alpha = '0.5')) %>%
  bind_rows(get_plotdata(sgb0_model, alpha = '0')) %>%
  bind_rows(get_plotdata(sgb1_model, alpha = '1')) %>%
  mutate(beta = paste0('beta = (', beta_1,',', beta_2, '',')'),
         beta_1 = lm_model$coefficients[2], beta_2 = lm_model$coefficients[3],
         cor = cor)
}

plotdata <- get_data(1,6) %>%
  rbind(get_data(1,10)) %>%
  rbind(get_data(1,2)) %>%
  rbind(get_data(1,6, cor = T)) %>%
  rbind(get_data(1,10, cor = T)) %>%
  rbind(get_data(1,2, cor = T)) %>%
  mutate(beta = factor(beta, levels = c("beta = (1,2)", "beta = (1,6)","beta = (1,10)")),
         cor = case_when(cor~ 'cor: 0.7', T ~'cor: 0')) 
plotdata %>% 
  arrange(beta,iteration) %>%
  ggplot(aes_string(x = 'V1', y = 'V2', color = 'alpha'))  + geom_line(aes(linewidth = alpha)) + 
  geom_point(aes(x=beta_1,y=beta_2), colour='red') + xlab('Estimate V1') + ylab('Estimate V2') +
  facet_grid(cor~beta) + theme_bw(base_size = 14) +
  scale_color_manual(values = c('#BB8FCE', '#2980B9', '#2ECC71','#CD6155','#909497')) +
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0), labels = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_discrete_manual("linewidth", values = seq(1.4, 0.2, length.out = 5))
  ggsave('beta_paths.png', width = 7, height = 5, dpi = 600)


# Code Figure 2: Sparsity curve orthonormal/independent/dependent
set.seed(1111)
plotdata <- data.frame(regularization= NULL,selection_group = NULL, alpha = NULL,
                         iteration = NULL)
for(i in 1:100){
    dat <- pracma::randortho(100)[,1:2,drop=F] %>%
      as.data.frame() %>%
      mutate(y = rnorm(n=100))
    dat_2 <- pracma::randortho(100)[,1:3,drop=F] %>%
      as.data.frame() %>%
      mutate(y = rnorm(n=100))
   for(alpha in seq(0.02,0.98, by = 0.02)){
sgb_df_model <- mboost(y ~bols(V1, V2,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                          bols(V2, intercept=F, df = alpha),
                        data = dat,
                        control = boost_control(mstop = 1, nu = 1))
sgb_penalty_1_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                         bols(V2, intercept=F, lambda = 1-alpha),
                       data = dat,
                       control = boost_control(mstop = 1, nu = 1))
sgb_penalty_10_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                  bols(V2, intercept=F, lambda = (1-alpha)*10),
                                data = dat,
                                control = boost_control(mstop = 1, nu = 1))
sgb_penalty_100_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                bols(V2, intercept=F, lambda = (1-alpha)*100),
                                data = dat,
                                control = boost_control(mstop = 1, nu = 1))
sgb_df_model_2 <- mboost(y ~bols(V1, V2, V3,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                         bols(V2, intercept=F, df = alpha)+ bols(V3, intercept=F, df = alpha),
                       data = dat_2,
                       control = boost_control(mstop = 1, nu = 1))
sgb_penalty_1_model_2 <- mboost(y ~bols(V1, V2,V3intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                                bols(V2, intercept=F, lambda = 1-alpha) + bols(V3, intercept=F, lambda = 1-alpha),
                              data = dat_2,
                              control = boost_control(mstop = 1, nu = 1))
sgb_penalty_10_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                 bols(V2, intercept=F, lambda = (1-alpha)*10)+ bols(V3, intercept=F, lambda = (1-alpha)*10),
                               data = dat_2,
                               control = boost_control(mstop = 1, nu = 1))
sgb_penalty_100_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                  bols(V2, intercept=F, lambda = (1-alpha)*100)+bols(V3, intercept=F, lambda = (1-alpha)*100),
                                data = dat_2,
                                control = boost_control(mstop = 1, nu = 1))
plotdata <- plotdata %>%
  bind_rows(
  bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model$xselect() == 1,alpha = alpha,
                       iteration = i, group_size = 'group size: 2')) %>%
  bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model$xselect() == 1,alpha = alpha,
                       iteration = i, group_size = 'group size: 2')) %>%
    bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 2')) %>%
  bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model$xselect() == 1,alpha = alpha,
                       iteration = i, group_size = 'group size: 2')) %>%
    bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')) %>%
    bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')) %>%
    bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')) %>%
    bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')))
 }
    print(i)
}
plotdata_orthogonal <- plotdata %>%
  mutate(dependence = 'orthogonal')
plotdata <- data.frame(regularization= NULL,selection_group = NULL, alpha = NULL,
                       iteration = NULL)
for(i in 1:100){
  dat <- data.frame(V1 = rnorm(n=100), V2 = rnorm(n=100)) %>%
    mutate(y = rnorm(n=100))
  dat_2 <- data.frame(V1 = rnorm(n=100), V2 = rnorm(n=100),  V3 = rnorm(n=100)) %>%
    mutate(y = rnorm(n=100))
  plotdata <- plotdata %>%
  bind_rows(
  bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model$xselect() == 1,alpha = alpha,
                       iteration = i, group_size = 'group size: 2')) %>%
  bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model$xselect() == 1,alpha = alpha,
                       iteration = i, group_size = 'group size: 2')) %>%
    bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 2')) %>%
  bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model$xselect() == 1,alpha = alpha,
                       iteration = i, group_size = 'group size: 2')) %>%
    bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')) %>%
    bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')) %>%
    bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')) %>%
    bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model_2$xselect() == 1,alpha = alpha,
                         iteration = i, group_size = 'group size: 3')))
  for(alpha in seq(0.02,0.98, by = 0.02)){
    sgb_df_model <- mboost(y ~bols(V1, V2,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                             bols(V2, intercept=F, df = alpha),
                           data = dat,
                           control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_1_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                                    bols(V2, intercept=F, lambda = 1-alpha),
                                  data = dat,
                                  control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_10_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                     bols(V2, intercept=F, lambda = (1-alpha)*10),
                                   data = dat,
                                   control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_100_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                      bols(V2, intercept=F, lambda = (1-alpha)*100),
                                    data = dat,
                                    control = boost_control(mstop = 1, nu = 1))
    sgb_df_model_2 <- mboost(y ~bols(V1, V2, V3,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                               bols(V2, intercept=F, df = alpha)+ bols(V3, intercept=F, df = alpha),
                             data = dat_2,
                             control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_1_model_2 <- mboost(y ~bols(V1, V2,V3intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                                      bols(V2, intercept=F, lambda = 1-alpha) + bols(V3, intercept=F, lambda = 1-alpha),
                                    data = dat_2,
                                    control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_10_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                       bols(V2, intercept=F, lambda = (1-alpha)*10)+ bols(V3, intercept=F, lambda = (1-alpha)*10),
                                     data = dat_2,
                                     control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_100_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                        bols(V2, intercept=F, lambda = (1-alpha)*100)+bols(V3, intercept=F, lambda = (1-alpha)*100),
                                      data = dat_2,
                                      control = boost_control(mstop = 1, nu = 1))
    plotdata <- plotdata %>%
      bind_rows(
        bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model$xselect() == 1,alpha = alpha,
                             iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')))
  }
  print(i)
}
plotdata_independent <- plotdata %>%
  mutate(dependence = 'independent')
plotdata <- data.frame(regularization= NULL,selection_group = NULL, alpha = NULL,
                       iteration = NULL)
# dependent
for(i in 1:100){
  dat <- as.data.frame(mvtnorm::rmvnorm(n = 100,mean = c(0, 0),
    sigma = matrix(c(1, 0.7, 0.7,1), ncol = 2)
  )) %>%
    mutate(y = rnorm(n=100))
  dat_2 <- as.data.frame(mvtnorm::rmvnorm(n = 100,mean = c(0, 0,0),
                                        sigma = matrix(c(1, 0.7,0.7, 0.7,1,0.7, 0.7,0.7,1), ncol = 3)
  )) %>%
    mutate(y = rnorm(n=100))
  for(alpha in seq(0.02,0.98, by = 0.02)){
    sgb_df_model <- mboost(y ~bols(V1, V2,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                             bols(V2, intercept=F, df = alpha),
                           data = dat,
                           control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_1_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                                    bols(V2, intercept=F, lambda = 1-alpha),
                                  data = dat,
                                  control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_10_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                     bols(V2, intercept=F, lambda = (1-alpha)*10),
                                   data = dat,
                                   control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_100_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                      bols(V2, intercept=F, lambda = (1-alpha)*100),
                                    data = dat,
                                    control = boost_control(mstop = 1, nu = 1))
    sgb_df_model_2 <- mboost(y ~bols(V1, V2, V3,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                               bols(V2, intercept=F, df = alpha)+ bols(V3, intercept=F, df = alpha),
                             data = dat_2,
                             control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_1_model_2 <- mboost(y ~bols(V1, V2,V3intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                                      bols(V2, intercept=F, lambda = 1-alpha) + bols(V3, intercept=F, lambda = 1-alpha),
                                    data = dat_2,
                                    control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_10_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                       bols(V2, intercept=F, lambda = (1-alpha)*10)+ bols(V3, intercept=F, lambda = (1-alpha)*10),
                                     data = dat_2,
                                     control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_100_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                        bols(V2, intercept=F, lambda = (1-alpha)*100)+bols(V3, intercept=F, lambda = (1-alpha)*100),
                                      data = dat_2,
                                      control = boost_control(mstop = 1, nu = 1))
    plotdata <- plotdata %>%
      bind_rows(
        bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model$xselect() == 1,alpha = alpha,
                             iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')))
  }
  print(i)
}
plotdata_correlated <- plotdata %>%
  mutate(dependence = 'correlated: 0.7')
plotdata <- data.frame(regularization= NULL,selection_group = NULL, alpha = NULL,
                       iteration = NULL)
# strongly dependent
for(i in 1:100){
  dat <- as.data.frame(mvtnorm::rmvnorm(n = 100,mean = c(0, 0),
                                        sigma = matrix(c(1, 0.95, 0.95,1), ncol = 2)
  )) %>%
    mutate(y = rnorm(n=100))
  dat_2 <- as.data.frame(mvtnorm::rmvnorm(n = 100,mean = c(0, 0,0),
                                        sigma = matrix(c(1, 0.95,0.95,0.95,1,0.95,0.95,0.95,1), ncol = 3)
  )) %>%
    mutate(y = rnorm(n=100))
  for(alpha in seq(0.02,0.98, by = 0.02)){
    sgb_df_model <- mboost(y ~bols(V1, V2,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                             bols(V2, intercept=F, df = alpha),
                           data = dat,
                           control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_1_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                                    bols(V2, intercept=F, lambda = 1-alpha),
                                  data = dat,
                                  control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_10_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                     bols(V2, intercept=F, lambda = (1-alpha)*10),
                                   data = dat,
                                   control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_100_model <- mboost(y ~bols(V1, V2,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                      bols(V2, intercept=F, lambda = (1-alpha)*100),
                                    data = dat,
                                    control = boost_control(mstop = 1, nu = 1))
    sgb_df_model_2 <- mboost(y ~bols(V1, V2, V3,intercept = F, df = 1-alpha)+bols(V1, intercept=F, df = alpha)+
                               bols(V2, intercept=F, df = alpha)+ bols(V3, intercept=F, df = alpha),
                             data = dat_2,
                             control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_1_model_2 <- mboost(y ~bols(V1, V2,V3intercept = F, lambda = sqrt(2)*alpha)+bols(V1, intercept=F, lambda = 1-alpha)+
                                      bols(V2, intercept=F, lambda = 1-alpha) + bols(V3, intercept=F, lambda = 1-alpha),
                                    data = dat_2,
                                    control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_10_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*10)+bols(V1, intercept=F, lambda = (1-alpha)*10)+
                                       bols(V2, intercept=F, lambda = (1-alpha)*10)+ bols(V3, intercept=F, lambda = (1-alpha)*10),
                                     data = dat_2,
                                     control = boost_control(mstop = 1, nu = 1))
    sgb_penalty_100_model_2 <- mboost(y ~bols(V1, V2,V3,intercept = F, lambda = sqrt(2)*(alpha)*100)+bols(V1, intercept=F, lambda = (1-alpha)*100)+
                                        bols(V2, intercept=F, lambda = (1-alpha)*100)+bols(V3, intercept=F, lambda = (1-alpha)*100),
                                      data = dat_2,
                                      control = boost_control(mstop = 1, nu = 1))
    plotdata <- plotdata %>%
      bind_rows(
        bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model$xselect() == 1,alpha = alpha,
                             iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 2')) %>%
          bind_rows(data.frame(regularization= 'df',selection_group = sgb_df_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 1',selection_group = sgb_penalty_1_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 10',selection_group = sgb_penalty_10_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')) %>%
          bind_rows(data.frame(regularization= 'lambda: 100',selection_group = sgb_penalty_100_model_2$xselect() == 1,alpha = alpha,
                               iteration = i, group_size = 'group size: 3')))
  }
  print(i)
}
plotdata_strongly_correlated <- plotdata %>%
  mutate(dependence = 'correlated: 0.95')
plotdata <- rbind(plotdata_correlated, plotdata_independent, plotdata_strongly_correlated,
                  plotdata_orthogonal)
plotdata %>% 
  group_by(alpha, regularization,dependence, group_size) %>%
  summarize(prob = mean(selection_group)) %>%
  mutate(dependence = factor(dependence, levels = c('orthogonal', 'independent','correlated: 0.7', 'correlated: 0.95')),
         regularization = factor(regularization, levels = c('df', 'lambda: 1', 'lambda: 10', 'lambda: 100'))
         ) %>%
  ggplot(aes(x = alpha, y = prob, color = regularization, group = regularization, linetype = regularization)) + geom_point(size=0.2) +
  ylab('Relative frequency group selection') + scale_color_manual(values = c('#1b9e77', '#d95f02', '#e7298a', '#3440eb')) +
  geom_line(linewidth=0.6) + theme_bw(base_size = 16) +
  theme(legend.position = 'top', legend.title = element_blank(),axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9)) + 
  facet_grid(group_size~dependence) 
ggsave('within_group_selection.png', width = 12, height = 5, dpi = 600)
