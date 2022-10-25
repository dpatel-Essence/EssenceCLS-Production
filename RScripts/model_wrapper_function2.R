model_wrapper_function2 = function(df = Final_CLS_2022_Study_List_Non_Search_model_file_chrome) {
  lapply(1:length(df), function(i) {
    lapply(powers, rlm_function2, cost = "cost_spent_on_exposed_group",
           response = "absolute_lift", data =  df[i][[1]]
    )
  }
  )
}
  
  