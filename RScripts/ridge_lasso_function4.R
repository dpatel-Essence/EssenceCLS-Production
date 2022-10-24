ridge_lasso_function4 =function(power,powers2 = 1,parameter_grid = 100,alpha_grid = c(seq(0,1,0.25)), cost = "cost_spent_on_exposed_group" , response = "absolute_lift", data = Final_CLS_2022_Study_List_Non_Search_model_file_chrome,poly_ind = 1) {
  alpha = alpha_grid
  parameters <- parameter_grid
  grid <- expand.grid(alpha = alpha, lambda = parameters)
  x<- data[[cost]] %>%
    as.data.frame() %>% 
    mutate_at(1,~(.^power)) %>% 
    rename(cost_p = 1) %>%
    mutate(cost_p2 = poly_ind*(cost_p^(1/power))^powers2) %>% 
    data.matrix()
  y <- data[[response]]
  #  data2 = cbind(y,cost_p,cost_p2) %>% as.data.frame()
  mod <- train(y = y,
               x = x,
               method = 'glmnet', 
               metric = "RMSE",
               tuneGrid = grid,
               trControl =  trainControl(method="cv", number = 5),
               intercept = FALSE
  ) 
  # return(as.data.frame.matrix(coef(mod$finalModel, mod$finalModel$lambdaOpt)))
  # return(cbind(t(as.data.frame.matrix(coef(mod$finalModel, mod$finalModel$lambdaOpt))) %>%
  #                as.data.frame(),getTrainPerf(mod) %>% select(-TrainMAE, -method)) %>% select(-1) %>% remove_rownames())
  return(
    cbind(t(as.data.frame.matrix(coef(mod$finalModel, mod$finalModel$lambdaOpt))) %>%
            as.data.frame(),getTrainPerf(mod),
          mod$finalModel$lambdaOpt %>% as.data.frame() %>% rename (lambda = 1),
          mod$bestTune$alpha %>% as.data.frame() %>% rename (alpha = 1),
          power %>% as.data.frame %>% rename(power = 1),
          powers2 %>% as.data.frame %>% rename(power2 = 1)
    ) %>%
      select(-TrainMAE, -method) %>%
      select(-1) %>%
      remove_rownames()
  )
}
