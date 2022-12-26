# Read dataset
data_re <- read.csv('data/employment_data2.csv', stringsAsFactors = FALSE)
dim(data_re)
View

########### quan sát bằng biểu đồ xem các biến của chúng####### có tương quan hay không ?
# biểu đồ thể hiện mối uan hệ giữa tỉ lệ việc làm toàn thời gian và lương cơ bản hàng tháng
plot(data_re$`employment_rate_ft_perm` ~ data_re$basic_monthly_median)
# thấy được giá trị thì khi làm việc toàn thòi gian có mối tương quan hay không

########## kiểm định hệ số tương quan #########
# có 3 hệ số tương quan: Pearson r, Spearman p, Kendal T
# hệ số tương quan sx chạy từ [-1,1]
# nếu hstq gần gái trị 1 haowjc -1 thì 2 biến có mối liên hệ
# nếu hstq có r<0, thì khi x tăng thì y giảm và ngược lại
# nếu hstq có r>0, thì khi x tăng thì y tăng và ngược lại
cor.test(data_re$`employment_rate_ft_perm`, data_re$basic_monthly_median)
# thấy p rất nhỏ, có khẳng định biến này có hệ số tương quan đáng tin cậy, r = 0.3731471 
cor.test(data_re$`employment_rate_ft_perm`, data_re$basic_monthly_median, method = "spearman")
# hstq thứ bậc bắt buộc phải biến đổi thành các thứ bậc trong data
cor.test(data_re$`employment_rate_ft_perm`, data_re$basic_monthly_median, method = "kendall")
# # hstq thứ bậc bắt buộc phải biến đổi thành các thứ bậc trong data

######### tìm hương trình hồi uy tuyến tính###########
hoiquy <- lm(data_re$`basic_monthly_median` ~ data_re$employment_rate_ft_perm)
# y = basic.., x = 
hoiquy
#intercept = hệ số chặn = anpha
# 11.53 = beta = độ dốc
# khi đó phương trình là : basic_monthly_median= 2208.02 + 11.53*employment_rate_ft_perm 
# dựa vào phương trình này biết được khi việc làm toàn thời gian tính ra lương cơ bản hàng tháng ntn


######### giả định của phân tích hồi quy #######
fitted(hoiquy)
# tính toán phần dư
resid(hoiquy)
#########3#vẽ biểu đồ kiểm định giả định ###########
datten <- par(mfrow = c(2,2))
plot(hoiquy)
