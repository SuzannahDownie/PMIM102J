create_menu_dataframe <- function(){
  option_no <- c(1, 2, 3, 4, 5)
  option <- c("AVERAGE MEDICATION SPEND.\n", 
"AVERAGE MEDICATION SPEND COMPARED TO POSTCODE AREA", "RATE OF DIABETES",
"ALL-WALES STATISITICAL ANALYSIS OF THE RATE OF DIABETES AND THE RATE OF INSULIN PRESCRIPTION",
"ALL-WALES STATISITICAL ANALYSIS OF THE RATE OF DIABETES AND THE RATE OF METFORMIN PRESCRIPTION")
  
  menu_df <- data.frame(option_no, option)
  
  colnames(menu_df)[which(names(menu_df) == "option_no")] <- "Option Number"
  colnames(menu_df)[which(names(menu_df) == "option")] <- " "
  
  menu_df_hux <- hux(menu_df) %>%
    set_all_padding(4) %>%
    set_outer_padding(0) %>%
    set_number_format(0) %>%
    set_bold(row = 1, col = everywhere) %>%
    set_bottom_border(row = 1, col = everywhere)
  
  final_hux <- print_screen(menu_df_hux, colnames = FALSE)
  return(final_hux)
}


visualise_opt_2 <- function(df){
  plot_spend <- ggdotchart(data = df, x = "street.x", y = "per_patient_spend",
                     color = "street.x", sorting = "ascending",                        
                     add = "segments", dot.size = 5, 
                     title = "Average Spend per Patient", xlab = "Practice",
                     ylab = "Spend", legend_title = "Practice") + 
                     theme_classic()
  plot_spend_final <- ggpar(plot_spend, tickslab = FALSE)
  print(plot_spend_final)
}


visualise_opt_3 <- function(diabetes_df) {
  diabetes_plot <- ggplot(diabetes_df) + 
    geom_bar(aes(x = cat_name, y = diabetes_rate, fill = cat_name), 
             stat = "identity", width = 0.5) + 
    ggtitle("Rate of Diabetes at Practice v All Wales Average") + 
    xlab(" ") + ylab("Rate of Diabetes as %") + labs(fill = " ") + 
    scale_fill_discrete(labels = c("Practice", "All Wales Average")) +
    scale_x_discrete(breaks=c("diabetes_rate_practice","diabetes_rate_wales"),
                     labels=c("Practice", "Wales Average")) +
    theme_pubr()
  print(diabetes_plot)
}

suppress_plotly_error <- function(p) {
  suppressMessages(plotly_build(p))
}

visualise_opt_4 <- function(diabetes_insulin_rate){
  fig <- suppress_plotly_error(
  plot_ly(data = diabetes_insulin_rate, 
                 x = diabetes_insulin_rate$total_insulin, 
                 y = diabetes_insulin_rate$total_with_diabetes,
                 marker = list(size = 8,
                               color = 'steelblue1',
                               line = list(color = 'cornflowerblue',
                                           width = 1)),
                 text = paste("Total Insulin: ", 
                              diabetes_insulin_rate$total_insulin, 
                              '<br>Patients with Diabetes: ',
                              diabetes_insulin_rate$total_with_diabetes,
                              '<br>Practice Name: ', diabetes_insulin_rate$street,
                              '<br>Practice ID: ', diabetes_insulin_rate$orgcode)) %>%
    layout(title = 'Rate of Diabetes Prevalence and Insulin Prescription for 2015')
  )
  print(fig)
}


