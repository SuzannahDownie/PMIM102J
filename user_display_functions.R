### FUNCTION TO CREATE A USER MENU ASKING THEM WHICH OPTION THEY 
### WOULD LIKE TO SEE
create_menu_dataframe <- function(){
    cat("\nPlease select one of the following options by typing the number of\n")
    cat("the option you require followed by enter:\n \n")
    option_no <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    option <- c("PRACTICE MEDICATION SPEND\n", 
    "PRACTICE MEDICATION SPEND COMPARED TO POSTCODE AREA", 
    "PRACTICE RATE OF DIABETES",
    "PRACTICE TOP 500 MEDICINES BY COST AND BNF CHAPTER DESCRIPTION",
    "ALL-WALES ANALYSIS OF THE RATE OF DIABETES AND INSULIN PRESCRIPTION",
    "ALL-WALES ANALYSIS OF THE RATE OF DIABETES AND METFORMIN PRESCRIPTION", 
    "STATISTICAL COMPARISON: RATE OF DIABETES AND METFORMIN/INSULIN PRESCRIPTION",
    "ALL-WALES MONTHLY SPENDING 2014 AND 2015",
    "ALL-WALES ANALYSIS OF RATE OF SMOKING AND CANCER DIAGNOSIS BY COUNTY",
    "ALL-WALES ANALYSIS OF RATE OF SMOKING AND COPD DIAGNOSIS BY COUNTY",
    'STATISTICAL COMPARISON: RATE OF SMOKING AND CANCER/COPD DIAGNOSIS')
  
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
}


### CREATE THE PLOT FOR AVERAGE MEDICATION SPEND
vis_practice_postcode_med_spend <- function(df){
    plot_spend <- ggdotchart(data = df, x = "street.x", y = "per_patient_spend",
                           color = "street.x", sorting = "ascending",                        
                           add = "segments", dot.size = 5, 
                           title = "Average Spend per Patient", 
                           xlab = "Practice", ylab = "Spend", 
                           legend_title = "Practice") +
                           theme_classic()
    plot_spend <- plot_spend + theme(axis.text.x = element_blank())
    print(plot_spend)
}

  
### CREATE THE PLOT FOR RATE OF DIABETES
vis_practice_diabetes_rate <- function(diabetes_df) {
    diabetes_plot <- ggplot(diabetes_df) + 
    geom_bar(aes(x = cat_name, y = diabetes_rate, fill = cat_name), 
            stat = "identity", width = 0.2) + 
            ggtitle("Rate of Diabetes at Practice v All Wales Average") + 
            xlab(" ") + ylab("Rate of Diabetes as %") + labs(fill = " ") + 
            ylim(0, 8) + scale_fill_discrete(labels = c("Practice", 
                                                        "All Wales Average")) +
            scale_x_discrete(breaks=c("diabetes_rate_practice",
                                      "diabetes_rate_wales"), 
                             labels=c("Practice", "Wales Average")) +
            geom_text(aes(x = cat_name, y = diabetes_rate, 
                          label = paste0(diabetes_rate, "%")), 
            position = position_dodge(width=0.9), vjust = -0.20) +
            theme_pubr()
    print(diabetes_plot)
}


### FUNCTION TO SUPRESS THE INTERACTIVE PLOTLY UNNECCESARY WARNING MESSAGE THAT
### DISRUPTS USER EXPERIENCE
suppress_plotly_error <- function(p) {
    suppressMessages(plotly_build(p))
}


### CREATE THE PLOT FOR RATE OF DIABETES V RATE OF INSULIN PRESCRIPTION
vis_wales_diabetes_rate_ins <- function(diabetes_insulin_rate){
    fig <- suppress_plotly_error(
    plot_ly(data = diabetes_insulin_rate, x = ~total_insulin, 
            y = ~total_with_diabetes, marker = list(size = 8,
                                                    color = 'steelblue1',
                            line = list(color = 'cornflowerblue', width = 1)),
            text = paste("Total Insulin: ", diabetes_insulin_rate$total_insulin, 
                         '<br>Patients with Diabetes: ',
                         diabetes_insulin_rate$total_with_diabetes,
                         '<br>Practice Name: ', diabetes_insulin_rate$street,
                         '<br>Practice ID: ', diabetes_insulin_rate$orgcode)) %>%
    layout(title = '\n    Rate of Diabetes  and Insulin Prescription for 2014') %>%
    layout(xaxis = list(title = "Total Amount of Insulin Prescribed"),
             yaxis = list(title = "Total Number of Patients with Diabetes"))
  )
  print(fig)
}


### CREATE THE PLOT FOR RATE OF DIABETES V RATE OF METFORMIN PRESCRIPTION
vis_wales_diabetes_rate_met <- function(diabetes_metformin_rate){
    fig <- suppress_plotly_error(plot_ly(data = diabetes_metformin_rate, 
                                         x = ~total_metformin, 
                                         y = ~total_with_diabetes,
            marker = list(size = 8, color = 'steelblue1',
                    line = list(color = 'cornflowerblue', width = 1)),
                    text = paste("Total Insulin: ",
                                 diabetes_metformin_rate$total_metformin,
                                '<br>Patients with Diabetes: ',
                                diabetes_metformin_rate$total_with_diabetes,
                     '<br>Practice Name: ', diabetes_metformin_rate$street,
                     '<br>Practice ID: ', diabetes_metformin_rate$orgcode)) %>%
  layout(title = '\n    Rate of Diabetes  and Metformin Prescription for 2014') %>%
  layout(xaxis = list(title = "Total Amount of Metformin Prescribed"),
         yaxis = list(title = "Total Number of Patients with Diabetes"))
  )
  print(fig)
}



### CREATE PLOT TO VISUALISE ALL WALES SPENDING FOR 2014 AND 2015
vis_wales_monthly_spend <- function(spend) {
    options(scipen=999)
    fig <- ggplot(spend, aes(x = month, y = total_spend, colour = period, 
                                     group = period)) + 
        geom_point() + geom_line() +
        ggtitle("Total Monthly Spend Across Wales 2014 and 2015") +
        xlab("Month") + ylab("Total Spend") + ylim(40000000, 55000000) +
        theme_pubr()
    
    print(fig)
}


### CREATE PLOT TO VISUALISE TOP MED SPEND AND BNF CHAPTER
vis_practice_top_meds <- function(result) {
    fig <- ggplot(data = result, aes(x = total_cost, y = chapterdesc, 
                                     color = chapterdesc, 
        text = paste("Name: ", bnfname, "<br>Cost £", total_cost, "<br>Items: ", 
                     total_items))) +
        geom_point() + theme(axis.ticks.y = element_blank(),
                         axis.text.y = element_blank(),
                         axis.title.y = element_blank()) +
        ggtitle("Practice Level: Top 500 Medicines by Cost and BNF Chapter Description") +
        xlab("Total Spend £") 
  
    print(ggplotly(fig))
}

### PLOT TO VISUALISE RATE OF SMOKING AND RATE OF CANCER
vis_wales_smoking_cancer_diagnosis <- function(df) {
  cancer_smoke_plot <- ggplot(df, aes(x = total_cancer, y = total_smokers,
                                            color = county.x, 
                                            text = paste ("Practice", street))) + 
    geom_point() +
    ggtitle("All Wales Number of Smokers v Number of Cancer Patients by County") + 
    xlab("Total Cancer Patients") + ylab("Total Smokers")+
    theme(legend.position = "none")
  
    print(ggplotly(cancer_smoke_plot))
}


### PLOT TO VISUALISE RATE OF SMOKING AND RATE OF COPD
vis_wales_smoking_COPD_diagnosis <- function(df) {
  COPD_smoke_plot<- ggplot(df, aes(x = total_copd, y = total_smokers,
                                   color = county.x, 
                                   text = paste ("Practice", street))) + 
    geom_point() +
    ggtitle("All Wales Number of Smokers v Number of COPD Patients by County") + 
    xlab("Total COPD Patients") + ylab("Total Smokers")+
    theme(legend.position = "none")
  
  
  print(ggplotly(COPD_smoke_plot))
}


