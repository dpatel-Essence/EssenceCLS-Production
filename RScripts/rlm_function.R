rlm_function =  function(power, cost = "cost_spent_on_exposed_group" , response = "absolute_lift", data = Search_data) {
  grid <- data.frame(intercept = FALSE, 
                     psi = c("psi.huber")
  ) #, "psi.hampel", "psi.bisquare"))
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
  return(data.frame(cbind((mod$finalModel$coefficients),(mod$results$RMSE))))
}
