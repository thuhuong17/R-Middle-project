## Load necessary libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(reshape)
library(purrr)

## Set work directory
getwd()
setwd('D:/Datasets')
setwd("D:/Datasets/R-Shiny-App-Graduate-Employment-Singapore-master")


####        Employment dataset        ####

## Read employment_data
data_e <- read.csv('data/employment_data.csv', stringsAsFactors = FALSE)
## display column and row
nrow(data_e)
ncol(data_e)
dim(data_e)
# column name
colnames(data_e)
#Trích xuất các biến liên quan
str(data_e)
summary(data_e)
############ COLUMNAMES #############
# 1 year - năm
# 2 university -  đại học
# 3 school - ngành học
# 4 degree - bằng cấp
# 5 employment_rate_overall - tỷ lệ việc làm chung %
# 6 employment_rate_ft_perm - Tỷ lệ việc làm cố định toàn thời gian %
# 7 basic_monthly_mean - Lương cơ bản hàng tháng - Trung bình (S$) 
# 8 basic_monthly_median - Lương cơ bản hàng tháng - Trung bình (S$) 
# 9 gross_monthly_mean - Tổng lương hàng tháng - Trung bình (S$)
# 10 gross_monthly_median - Tổng lương hàng tháng - Trung bình
# 11 gross_mthly_25_percentile - Tổng lương hàng tháng - phần trăm thứ 25
# 12 gross_mthly_75_percentile - Tổng lương hàng tháng - phần trăm thứ 75

# display 10 head row
head(data_e)
# view table
View(data_e)
# tóm tắt dữ liệu
summary(data_e)
#truy cap du lieu
data_e$year

## Kiểm tra và loại bỏ NA. Trong bộ dữ liệu có 'na',
## thay đổi bằng NA và sau đó xóa 
data_e[data_e == "na"] <- NAnó
table(is.na(data_e))
sapply(data_e, function(x)
  sum(is.na(x)))
# Singapore University of Technology and Design have empty School column
# sử dụng thông tin được cung cấp trong cột độ để điền vào các NA
# Điền tên trường còn thiếu với tên bằng cấp
View(data_e)
SUTD <- data_e %>%
  filter(is.na(school)) %>%
  mutate(school = sub("\\).*", "", sub(".*\\(", "", .$degree))) %>%
  mutate(degree = gsub("\\s*\\([^\\)]+\\)", "", .$degree))
dim(data_e)
# Xóa các hàng ban đầu và cập nhật chúng bằng các hàng đã được làm sạch
data_e[data_e$university == "Singapore University of Technology and Design", ] <-
  NA
data_e <- rbind(data_e, SUTD)
data_e <- drop_na(data_e)

# vì tất cả các trường trong SMU đều được dán nhãn (chương trình 4 năm)
# xóa chúng bằng regex
SMU <- data_e %>%
  filter(university == "Singapore Management University") %>%
  mutate(school = gsub("\\s*\\([^\\)]+\\)", "", .$school))
# Remove the original rows, and update them with the cleaned ones
data_e[data_e$university == "Singapore Management University", ] <-
  NA
data_e <- rbind(data_e, SMU)
data_e <- drop_na(data_e)

# Convert college name College of Business (Nanyang Business School)
# to Nanyang Business School only
NBS <- data_e %>%
  filter(school == "College of Business (Nanyang Business School)") %>%
  mutate(school = sub("\\).*", "", sub(".*\\(", "", .$school)))
# Remove the original rows, and update them with the cleaned ones
data_e[data_e$school == "College of Business (Nanyang Business School)", ] <-
  NA
data_e <- rbind(data_e, NBS)
data_e <- drop_na(data_e)

# drop unused levels- # giảm cấp độ không sử dụng
data_e <- droplevels(data_e)

# Check school names
uni <- unique(data_e$university)# unique func use delete or remove duplicate values
sch_list <- list()

for (u in 1:length(uni)) {
  sch <- unique(data_e[data_e$university == uni[u],]$school)
  sch_list[[u]] <- sch
  
}


# manually correct the school names:- # tự sửa tên trường:

# 1. NTU: Sports Science and Management & Sport Science and Management
data_e[data_e$school == "Sports Science and Management", "school"] <-
  "Sport Science and Management"
View(data_e)

# 2. NUS:
data_e[data_e$school == "Faculty Of Dentistry", "school"] <-
  "Faculty of Dentistry"
data_e[data_e$school == "Faculty Of Engineering", "school"] <-
  "Faculty of Engineering"
data_e[data_e$school == "Multidisciplinary Programme", "school"] <-
  "Multidisciplinary Programmes"
data_e[data_e$school == "Multidisciplinary Program", "school"] <-
  "Multidisciplinary Programmes"
data_e[data_e$school == "Multi-Disciplinary Programme", "school"] <-
  "Multidisciplinary Programmes"
data_e[data_e$school == "YST Conservatory Of Music", "school"] <-
  "Yong Siew Toh Conservatory of Music"
data_e[data_e$school == "Yong Loo Lin School (Medicine)", "school"] <-
  "YLL School of Medicine"
data_e[data_e$school == "School of Design and Environment", "school"] <-
  "School of Design & Environment"

# 3. SIT

data_e[data_e$school == "Singapore Institute of Technology (SIT)", "school"] <-
  "Singapore Institute of Technology"
data_e[data_e$school == "Singapore Institute of Technology -Trinity College Dublin", "school"] <-
  "Trinity College Dublin"
data_e[data_e$school == "Singapore Institute of Technology -Trinity College Dublin / Trinity College Dublin", "school"] <-
  "Trinity College Dublin"
data_e[data_e$school == "SIT-Trinity College Dublin / Trinity College Dublin", "school"] <-
  "Trinity College Dublin"
data_e[data_e$school == "SIT-University of Glasgow", "school"] <-
  "University of Glasgow"
data_e[data_e$school == "Trinity College Dublin / Singapore Institute of Technology-Trinity College Dublin", "school"] <-
  "Trinity College Dublin"


# 4. SMU

data_e[data_e$university == "Singapore Management University",]$degree <-
  sub("[[:blank:]]\\([[:digit:]].*\\)", "", data_e[data_e$university == "Singapore Management University",]$degree)
data_e[data_e$university == "Singapore Management University",]$degree <-
  as.character(sub("\\)", "", sub("\\(", "", data_e[data_e$university == "Singapore Management University",]$degree)))

# update the levels again
data_e <- droplevels(data_e)

# check the na count again
sapply(data_e, function(x)
  sum(is.na(x)))

## Transform factor variables into numeric
data_e <- data_e %>% modify_at(c(5:12), as.numeric)
## Check data
str(data_e)
## Save as .rds extension for Shiny
saveRDS(data_e, file = "data/employment_data.rds")
data_e <- readRDS("data/employment_data.rds")
str(data_e)

# write the output
write.csv(data_e, "data/employment_data1.csv", row.names = FALSE)


####        Graduates dataset         ####
## Read graduate data
data_g <- read.csv('data/graduates_by_institutions.csv')
head(data_g)
str(data_g)
View(data_g)
## display column and row
nrow(data_g)
ncol(data_g)
dim(data_g)
# column name
colnames(data_g)
############# COLUMNAMES #############
#1 year năm
#2 sex giới tính
#3 nus Universities refers to National University of Singapore 
#4 ntu Nanyang Technological University
#5 smu Singapore Management University
#6 sit Singapore Institute of Technology
#7 sutd Singapore University of Technology Design
#8 suss Singapore University of Social Sciences
#9 nie National Institute of Education
#10 singapore_polytechnic
#11 ngee_ann_polytechnic
#12 temasek_polytechnic
#13 nanyang_polytechnic
#14 republic_polytechnic
#15 lasalle_diploma
#16 lasalle_degree
#17 nafa_diploma
#18 nafa_degree
#19 ite

## We can see that data contains each university in columns
## We can easily melt it to have university name in one column - Chúng ta có thể dễ dàng trộn nó để có tên trường đại học trong một cột
data_g <- melt(data_g, id = c("year", "sex"))
str(data_g)
## we have "-" special character in the values
## Let's replace that with "NA" and then remove from data
data_g[data_g == "-"] <- NA
## Check NAs
table(is.na(data_g))
sapply(data_g, function(x)
  sum(is.na(x)))
## Remove NA
data_g <- drop_na(data_g)
sapply(data_g, function(x)
  sum(is.na(x)))
dim(data_g)
## Transform value column from character into numeric variable - ## Chuyển đổi cột giá trị từ ký tự thành biến số
data_g <- data_g %>% modify_at(4, as.numeric)
str(data_g)
## change the variable with "university" & value with "graduates"
colnames(data_g)[3:4] <- c("university", "graduates")
colnames(data_g)
## check a list of universities and number of graduates in 2018 ## kiểm tra danh sách các trường đại học và số lượng sinh viên tốt nghiệp năm 2018
check_university <- data_g %>%
  filter(year == "2018") %>%
  group_by(university) %>%
  tally(graduates)
head(check_university)
## correctly name of the universities to make it easily readable tên chính xác của các trường đại học để dễ đọc
## Get the titles of universities - Nhận danh hiệu của các trường đại học
levels(data_g$university)
## Set new titles of universities - Đặt tên mới cho các trường đại học
levels(data_g$university) <-
  c(
    "National University of Singapore",
    "Nanyang Technological University",
    "Singapore Management University",
    "Singapore Institute of Technology",
    "Singapore University of Technology and Design",
    "Singapore University of Social Sciences",
    "National Institute of Education",
    "Singapore Politechnic",
    "Ngee Ann Polytechnic",
    "Temasek Polytechnic",
    "Nanyang Polytechnic",
    "Republic Polytechnic",
    "Lasalle Diploma",
    "Lasalle Degree",
    "Nafa Diploma",
    "Nafa Degree",
    "Institute of Technical Education"
  )

levels(data_g$university)
## Save as .rds extension for Shiny
saveRDS(data_g, file = "data/graduates_by_institutions.rds")
