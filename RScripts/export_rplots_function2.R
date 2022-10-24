export_rplots_function2 = function (i,starting_name = "Non_Search_",folder_name = folder_name,df_list = df_names[[j]],graphing_list = graph_names[j][[1]]) {
  p2 <- ggplotly(graphing_list[[i]])
  file_name <- paste0(starting_name, paste0(unique(df_list[[1]]["pa"]), "_", names(df_list[i])),".html") %>%
    str_replace_all( "/", "") %>% 
    str_replace_all(" ",'_')
  saveWidget(p2, paste0(folder_name, file_name), selfcontained = F, title = "test", libdir = NULL)
  p <- graphing_list[[i]]
  file_name2 <- paste0(starting_name, paste0(unique(df_list[[1]]["pa"]), "_", names(df_list[i])),".png") %>%
    str_replace_all( "/", "") %>% 
    str_replace_all(" ",'_')
  ggsave(plot = p,filename = paste0(folder_name, file_name2))
}