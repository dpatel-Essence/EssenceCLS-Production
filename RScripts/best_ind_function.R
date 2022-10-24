best_ind_function = function(i,df = fits_non_search_chrome,df2 =Final_CLS_2022_Study_List_Non_Search_model_file_chrome)
{
  df[[i]] %>%
    bind_rows() %>%
    slice_min(TrainRMSE) %>%
    mutate(
      model = paste0(unique(df2[[1]]["pa"]),"|",names(df2)[i])
    ) %>% 
    filter(row_number()==1)
} 
  
  
