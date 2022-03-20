create_menu_dataframe <- function(){
  option_no <- c(1, 2, 3, 4, 5)
  option <- c("AVERAGE MEDICATION SPEND PER MONTH AT THIS PRACTICE.\n", 
              "AVERAGE SPEND PER MONTH ON MEDICATION AT THIS PRACTICE COMPARED TO POSTCODE AREA",
              "RATE OF DIABETES AT THIS PRACTICE COMPARED TO ALL-WALES",
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
                     ylab = "Spend", legend_title = "Practice", 
                     ggtheme = theme_classic())
  plot_spend_final <- ggpar(plot_spend, tickslab = FALSE)
  print(plot_spend_final)
}


visualise_opt_3 <- function(diabetes_df) {
  diabetes_df <- melt(diabetes_df, measure.vars = c("diabetes_rate_practice", 
                                                    "diabetes_rate_wales"))
  colnames(diabetes_df) <- c("cat_name", "value")
  plot_diabetes_data <- ggplot(diabetes_df) +
    geom_bar(aes(x = cat_name, y = value, fill = cat_name), stat = "identity",
             width = 0.5) + ggtitle("Rate of Diabetes at Practice v All Wales Average")
              + xlab(" ") + ylab("Rate of Diabetes as %") + labs(fill = " ") +
              ggtheme = theme_pubr()
    print(plot_diabetes_data)
}

