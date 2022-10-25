model_wrapper_function = function(df = Final_CLS_2022_Study_List_Non_Search_model_file_chrome, model = ridge_lasso_function4, ridge = "Y",rlm = "Y",poly_ind = 1){
  df2 <- lapply(1:length(df), function(i) {
    lapply(powers2, function(j) {
      ((lapply(powers, model,
                   powers2 = j,
                   parameter_grid = parameters, alpha_grid = alpha_parameters, cost = "cost_spent_on_exposed_group",
                   response = "absolute_lift",poly_ind = poly_ind, data = df[[i]]
      )
      ))
    })
  })
}



