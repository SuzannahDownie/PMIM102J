library(stringr)

query <- qq("SELECT g.period AS period, 
                  ROUND(SUM(g.nic::decimal), 2) AS total_spend
                  FROM gp_data_up_to_2015 g
                  WHERE g.period >= 201401 AND g.period <= 201512
                  GROUP BY g.period")
spend <- get_data(db, query)
spend$month <- as.numeric(str_sub(spend$period,-2,-1))
spend$period <- strtrim(spend$period, 4)
spend$month <- month.abb[spend$month]
spend$month <- factor(spend$month, levels = month.abb)





plot_option_x <- ggplot(spend, aes(x = month, y = total_spend, colour = period, 
                               group = period)) + 
  geom_point() + geom_line() +
  ggtitle("Total Monthly Spend Across Wales 2014 and 2015") +
  xlab("Month") + ylab("Total Spend") + ylim(40000000, 55000000) +
  theme_pubr()

print(plot_option_x)


                     