# id <- 'W93009'
# 
# 
# diabetes_rate_practice <- get_diabetes_rate_practice(db, id)
# diabetes_rate_wales <- get_diabetes_rate_wales(db, id)
# diabetes_df <- data.frame(diabetes_rate_practice, diabetes_rate_wales)
# diabetes_df <- melt(diabetes_df, measure.vars = c("diabetes_rate_practice", 
#                                                   "diabetes_rate_wales"))
# colnames(diabetes_df) <- c("cat_name", "diabetes_rate")
# 
# #plot_diabetes_rate <- visualise_opt_3(diabetes_df)
# 
# 
# 
# 
# 
# 
# 
# diabetes_plot <- ggplot(diabetes_df) + 
#   geom_bar(aes(x = cat_name, y = diabetes_rate, fill = cat_name), stat = "identity",
#            width = 0.5) + ggtitle("Rate of Diabetes at Practice v All Wales Average") + 
#   xlab(" ") + ylab("Rate of Diabetes as %") + labs(fill = " ") + 
#   scale_fill_discrete(labels = c("Practice", "All Wales Average")) +
#   scale_x_discrete(breaks=c("diabetes_rate_practice","diabetes_rate_wales"),
#                    labels=c("Practice", "Wales Average")) +
#                     theme_pubr()
#   print(plot_diabetes_data)
# 
# print(diabetes_plot)



