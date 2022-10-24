ridge_lasso_function2 =function(power,parameter_grid = 100,alpha_grid = c(seq(0,1,0.25)), cost = "cost_spent_on_exposed_group" , response = "absolute_lift", data = Search_data) {
  alpha = alpha_grid
  parameters <- parameter_grid
  grid <- expand.grid(alpha = alpha, lambda = parameters)
  x<- data[[cost]] %>%
    as.data.frame() %>% 
    mutate_at(1,~(.^power)) %>% 
    rename(cost_p = 1) %>%
    mutate(cost_p2 = cost_p^0.5) %>% 
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
          mod$bestTune$alpha %>% as.data.frame() %>% rename (alpha = 1)
    ) %>%
      select(-TrainMAE, -method) %>%
      select(-1) %>%
      remove_rownames()
  )
}
