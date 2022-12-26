data_re <- read.csv('data/employment_data2.csv', stringsAsFactors = FALSE)
dim(data_re)
View(data_re)
plot(data_re$`employment_rate_ft_perm` ~ data_re$basic_monthly_median)

cor.test(data_re$`employment_rate_ft_perm`, data_re$basic_monthly_median)
cor.test(data_re$`employment_rate_ft_perm`, data_re$basic_monthly_median, method = "spearman")
cor.test(data_re$`employment_rate_ft_perm`, data_re$basic_monthly_median, method = "kendall")

hoiquy <- lm(data_re$`basic_monthly_median` ~ data_re$employment_rate_ft_perm)
fitted(hoiquy)
resid(hoiquy)
#vẽ biểu đồ kiểm định giả định 
datten <- par(mfrow = c(2,2))
plot(hoiquy)
