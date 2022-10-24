export_rplots_function = function(starting_name = "Non_Search_",folder_name = folder_name,df2 = Final_CLS_2022_Study_List_Non_Search_model_file_Chrome,i) {
  p2 <- ggplotly(graph_list[[i]])
  file_name <- paste0(starting_name, paste0(unique(df2[[i]]["pa"]), "_", names(df2[i])),".html") %>%
    str_replace_all( "/", "") %>% 
    str_replace_all(" ",'_')
  paste0(folder_name, file_name)
  saveWidget(p2, paste0(folder_name, file_name), selfcontained = F, title = "test", libdir = NULL)
  p <- graph_list[[i]]
  file_name2 <- paste0(starting_name, paste0(unique(df2[[i]]["pa"]), "_", names(df2[i])),".png") %>%
    str_replace_all( "/", "") %>% 
    str_replace_all(" ",'_')
  ggsave(plot = p,filename = paste0(folder_name, file_name2))
}
