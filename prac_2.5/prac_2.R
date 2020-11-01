#######2.5 Reading data into R#####
library(tidyverse)
library(here)
library(readr)
library(janitor)##janitor（学校的）看门人
library(poltly)
library(maptools)###可以用于读取shipfile文件
library(RColorBrewer)##地区分布图的颜色
library(classInt)###作用于空间分析
library(sp)
library(rgeos)##GDAL and GEOS
library(tmap)###Thematic Map专题地图，可阅读、编写、操作空间数据并可视化
library(tmaptools)
library(sf)##空间数据的存储与操作
library(rgdal)##GDAL and GEOS
library(geojsonio)
library(rJava)
library(Rcpp)
library(OpenStreetMap)

######由于地域性原因，中国地区默认编码encoding是GBK936.第二行是更改默认配置的编码####
Sys.getlocale()
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
##############################
LondonData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
###locale means 发生地点，readr默认的是UTF-8，但我们所需要的是latin1######

class(LondonData)###“class”用于检查数据类型####
test <- LondonData %>%  dplyr::summarise_all(., class)

####可用于观察表格内各列元素的属性类型####
Datatypelist <- LondonData %>% 
  dplyr::summarise_all(class) %>%
  tidyr::pivot_longer(everything(), ####把数据由上方转移到左侧，并依次向下排列###
               names_to="All_variables", ###原表格第一行，所有的变量
               values_to="Variable_class")#####原表格变量的属性，变量的内容
Datatypelist

######来尝试一下，pivot_wider####
test_wider <- LondonData %>% 
  dplyr::summarise_all(class) %>%
  tidyr::pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
test_wider
########结论，暂时未发现区别，go on####

LondonData <- edit(LondonData)
######edit()功能，可以在R中像excel一样编辑数据

LondonData%>% colnames()
######colnames()功能，可以只显示表格的表头属性栏

LondonBoroughs<-LondonData[626:658,]###逗号后是空的，表明选中所有数字
LondonBoroughs<-LondonData%>%
  slice(626:658)
####slice()功能，切割表格中的部分；前者只能进行选中，序号未改变###

Femalelifeexp<- LondonData %>% 
  dplyr::filter(`Female life expectancy -2009-13`>90)
####filter('表格中的head，代表着某一个data'，条件)---filter过滤功能，以表格中某一特征对整个表格进行过滤

LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))
#####字符串检索str_detect()，“^"意为以E09开头的，这是一个多功能同时作用的例子###

LondonBoroughs<-LondonBoroughs %>%
  dplyr::distinct()
#####distinct()功能，合并表中相同的observation；
#####不要过的多的合并步骤，一步一步想清楚在做

LondonBoroughs_contains<-LondonBoroughs %>% 
  select(dplyr::contains("expectancy"), 
         dplyr::contains("obese - 2011/12 to 2013/14"),
         dplyr::contains("Ward name")) 
#######contains()功能，通过搜索关键词，对表格的变量进行筛选（与filter的不同，filter不会改变原有变量，contains后的表格只剩所选变量）

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)
####顾名思义，rename()功能就是重命名，new name='old name'###

LondonBoroughs <- LondonBoroughs %>%
  clean_names()
#####吊炸天功能clean names(),清理variables中的空格、符号等

######dplyr::mutate()功能，将两个变量通过运算法则生成一个变量，variable or column，不是融合，所以原有变量不会消失
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  dplyr::mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  dplyr::mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  select(new_code,
         borough,
         averagelifeexpectancy, 
         normalisedlifeepectancy)%>%
  arrange(desc(normalisedlifeepectancy))
##arrange()功能，排布功能，默认arrange(variables)为升序，降序为arrange(desc(variables))

#####dplyr::case_when()功能，需要借助mutate功能，mutate(新的变量名称 = case_when(条件 ~<then的意思> “输出的string”,
                                          #######TURE<otherwise的意思> ~ "相反输出的string“))
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = dplyr::case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2

######group_by()功能，group_by(分组依据variable)
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  group_by(UKcompare) %>% 
summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))
#####输出结果：`summarise()` ungrouping output (override with `.groups` argument)
####group_by+summarise，group_by划分observation，summarise划分variable，创造一个新的表格

####across()功能，其属于变体函数，需附加其他函数使用
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3)) %>% ##谓词函数，mutate(across(where(is.xxxx特性)，xxxx操作))
  ###不会创建新的变量及column,会对原有表格进行修改，并将小数位控制在三位
  mutate(across(UKdiff, round, 0)) %>% ###将ukdiff控制在个位数
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                               str_c("equal or above UK average by",
                                     UKdiff, 
                                     "years", 
                                     sep=" "), 
                             TRUE ~ str_c("below UK average by",
                                          UKdiff,
                                          "years",
                                          sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())
###review时，理解函数意义练习

plot(Life_expectancy$averagelifeexpectancy,Life_expectancy$normalisedlifeepectancy)
####plot()功能，plot(x,y)明确到两列数据

plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, ###可交互性，鼠标放在点上，可显示的信息
        type = "scatter", 
        mode = "markers")


#####################

EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")
LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))
qtm(LondonMap)###qtm功能，from tmap，快速浏览地图

LondonData <- clean_names(LondonData)
BoroughDataMap <- EW %>%
  clean_names() %>% 
  filter(str_detect(lad15cd, "^E09"))%>%
#####这里“.”的意思是我们最近加载的数据，在这里就是BoroughDataMap（可以想象成这个编码的主语）
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE) %>% 
  distinct(.,lad15cd, 
           .keep_all = TRUE)
#######在distinct的上面，我们对“编码”column进行了操作，“.keep_all = TURE”意为着保留所有其他变量，否则“.keep_all = FALSE”（默认就是这样）将只保留编码即修改匹配的column

BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))
#*merge那步，同样可以被left_join功能代替，left_join是指保留前者的全部以及与后者想匹配的部分
#*类似功能还包括：right_join
#*inner_join 只保留两者相同的部分
#*full_join 两者全部都保留

######tmap_mode默认为plot，可调换成view,就可以在地图上显示结果
tmap_mode("plot")
tmap_mode("view")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

###st_bbox功能，在结果地图周边创造一个边界框，框住结果
tmaplondon <- BoroughDataMap %>%
  sf::st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)###调出OSM地图

#######################
tmap_mode("plot")

tm_shape(tmaplondon)+####上一步加了边框，置入OSM的数据
  tm_rgb()+####RGB
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",###样式，如何对数据进行色彩分割，可选择的还有"pretty"
              palette="YlOrBr",###调色板，要使用的调色方案，还有"blue"
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + ###透明度
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

######可通过tmaptools::palette_explorer()看到更多配色选项
tmaptools::palette_explorer()


#####tidydata
flytipping <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv")
####加载后，会发现所有的列都是col_character(),这意味着他们都是字符无法进行定量分析

flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                        col_types = cols(
                          code = col_character(),
                          area = col_character(),
                          year = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()
                        ))
#####但可以通过以上手段对其强行定义

flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )
#####对原表格4至11列重新定义为，tipping_type,并将其原有value名称变为count。

flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )