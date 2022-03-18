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

