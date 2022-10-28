rlm_function2 =  function(power, cost = "cost_spent_on_exposed_group" , response = "absolute_lift", data =  Final_CLS_2022_Study_List_Non_Search_model_file_chrome[2][[1]]) {
  grid <- expand.grid(intercept = FALSE, 
                      psi = c("psi.huber", "psi.hampel", "psi.bisquare")
  )
  cost_p <- data[[cost]] ^ power
  y <- data[[response]]
  data2 = cbind(y,cost_p) %>% as.data.frame()
  mod <- train(
    y ~ cost_p,
    data = data2,
    method = "rlm",
    metric = "RMSE",
    #  preProc= c("center","scale"),
    trControl =  trainControl(method="cv", number = 5),
    tuneGrid = grid,
    na.action = na.omit,
    maxit = 100
  )
  return(
    cbind(cbind(t(coef(mod$finalModel)) %>% as.data.frame()
                ,getTrainPerf(mod)
                ,power %>% as.data.frame %>% rename(power = 1)
    ) %>%
      select(-TrainMAE,-method)
    )
  )
}
                 

               
