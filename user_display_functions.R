### FUNCTION TO CREATE A USER MENU ASKING THEM WHICH OF THE PART 1 TASKS THEY 
### WOULD LIKE TO SEE
create_menu_dataframe <- function(){
  option_no <- c(1, 2, 3, 4, 5)
  option <- c("AVERAGE MEDICATION SPEND.\n", 
"AVERAGE MEDICATION SPEND COMPARED TO POSTCODE AREA", "RATE OF DIABETES",
"ALL-WALES STATISTICAL ANALYSIS OF THE RATE OF DIABETES AND INSULIN PRESCRIPTION",
"ALL-WALES STATISTICAL ANALYSIS OF THE RATE OF DIABETES AND METFORMIN PRESCRIPTION")
  
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

### CREATE THE PLOT FOR OPTION 2 - AVERAGE MEDICATION SPEND
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


### CREATE THE PLOT FOR OPTION 3 - RATE OF DIABETES
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

### FUNCTION TO SUPRESS THE INTERACTIVE PLOTLY UNNECCESARY WARNING MESSAGE THAT
### DISRUPTS USER EXPERIENCE
suppress_plotly_error <- function(p) {
  suppressMessages(plotly_build(p))
}


### CREATE THE PLOT FOR OPTION 4 - RATE OF DIABETES V RATE OF INSULIN PRESCRIPTION
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
    layout(title = '\nRate of Diabetes Prevalence and Insulin Prescription for 2015')
  )
  print(fig)
}


### CREATE THE PLOT FOR OPTION 5 - RRATE OF DIABETES V RATE OF METFORMIN PRESCRIPTION
visualise_opt_5 <- function(diabetes_metformin_rate){
  fig <- suppress_plotly_error(
    plot_ly(data = diabetes_metformin_rate, 
            x = diabetes_metformin_rate$total_metformin, 
            y = diabetes_metformin_rate$total_with_diabetes,
            marker = list(size = 8,
                          color = 'steelblue1',
                          line = list(color = 'cornflowerblue',
                                      width = 1)),
            text = paste("Total Insulin: ", 
                         diabetes_metformin_rate$total_metformin, 
                         '<br>Patients with Diabetes: ',
                         diabetes_metformin_rate$total_with_diabetes,
                         '<br>Practice Name: ', diabetes_metformin_rate$street,
                         '<br>Practice ID: ', diabetes_metformin_rate$orgcode)) %>%
      layout(title = '\nRate of Diabetes Prevalence and Metformin Prescription for 2015')
  )
  print(fig)
}