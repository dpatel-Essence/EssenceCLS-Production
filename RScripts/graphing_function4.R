graphing_function4 = function(df1= best_fit_non_search_chrome,df2 = Final_CLS_2022_Study_List_Non_Search_model_file_Chrome,i)
{
  p <- df1[[i]]["power"]
  
  p2 <- df1[[i]]["power2"]
  
  b1 <- df1[[i]]["cost_p"]
  
  b2 <- df1[[i]]["cost_p2"]
  
  max_x <- 1.1 * max(df2[[i]]["cost_spent_on_exposed_group"])
  
  x1 <- seq(0, max_x, by = max_x / 100) %>%
    as.data.frame()
  colnames(x1) <- "spend"
  
  y_predict_best_fit <-
    (mapply("*", data.frame(mapply(`^`, x1, p)), b1) +
       mapply("*", data.frame(mapply(`^`, x1, p2)), b2)) %>%
    as.data.frame()
  colnames(y_predict_best_fit) <- "response"
  
  y_pred <-
    (mapply("*", mapply(`^`, df2[[i]]["cost_spent_on_exposed_group"], p), b1) +
       mapply("*", data.frame(mapply(`^`, df2[[i]]["cost_spent_on_exposed_group"], p2)), b2)) %>%
    as.data.frame()
  colnames(y_pred) <- "response_actual"
  
  y_actual <- df2[[i]]["absolute_lift"]
  
  R_Squared = cor(y_pred,y_actual)^2 %>% 
    first() %>% 
    round(digits = 2)
  
  predicted_response <-
    bind_cols(x1, y_predict_best_fit)
  
  p <-
    ggplot() +
    geom_line(data = predicted_response, aes(
      x = spend,
      y = response
    )) +
    geom_point(
      data = df2[[i]],
      aes(
        x = cost_spent_on_exposed_group, y = absolute_lift, color = 'red',
        text = paste(pa, channel, tactic, region,average_depth, study_id,"anomaly_score:",round(anomaly_score * 100,digits = 1))
      )
    ) +
    ggtitle(paste0(unique(df2[[i]]["pa"]), "|", names(df2[i]))) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title.x = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12, color = "black")
    ) +
    # scale_colour_discrete(name = "Tactic") +
    scale_x_continuous("Dollars Spent USD", labels = scales::dollar) +
    scale_y_continuous("Absolute Lift", labels = scales::comma) + 
    annotate("text", x = round(max_x,0)/2, y = 1, label = paste0("R_Squared= ",R_Squared))
  return(p)
}
