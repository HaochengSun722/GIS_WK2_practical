library(tidyverse)
library(readr)
library(here)

###########################
#####2.4 introduction####
A <- 1+5
A
getwd() ###帮助找到当前工作路径
### Alt + - 等于 <-  ####

#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")
####c means combine###

df <- data.frame(Data1, Data2)
###data.frame 就想带你关于把数据制成表格，横向合并###
###data.frame[row,column]####

head_df <- df %>% 
  head()
tail_df <- df %>% 
  tail()
####head(x) 表格开始的x行 tail(x) 表格结束的x行####
####Alt+= 就是 %>%通道函数###

df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)
#### dplyr::rename 重命名--新名称 = 旧名称；"::"以为着dplyr的rename功能####

df %>% 
  select(column1)
df$column1
df[["column1"]]
###以上三个方式都是一样的效果，选中相应的列的所有数据####

#####2.5 Reading data in R#####
LondonDataOSK<- read.csv("C:/Users/10126/OneDrive/CASA/module/GIS/WK2-practical/london_data.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")
LondonDataOSK<- read.csv("C:/Users/10126/OneDrive/CASA/module/GIS/WK2-practical/ward-profiles-excel-version.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")

LondonDataOSK<- read.csv(here::here("ward-profiles-excel-version.csv"), 
                         header = TRUE, sep = ",",  
                         encoding = "latin1")




