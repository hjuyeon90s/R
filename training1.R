install.packages( "foreign" )

library( foreign )
library( dplyr )
library( ggplot2 )
library( readxl )

df <- read.spss( file = "D:/Workspace/R/Koweps_h10_2015_beta1.sav",
                 to.data.frame = TRUE )
df_work <- df

str( df_work )
dim( df_work )
View( df_work )

df_work <- rename( df_work, 
                   area = h10_reg7,
                   income = h10_din,
                   sex = h1001_4,
                   people = h1001_1,
                   education = h1001_6,
                   married = h1001_11,
                   medical = h1002_7 )

View( df_work )

df_work_data <- select( df_work, c( "area",
                                    "income",
                                    "sex",
                                    "people",
                                    "education",
                                    "married",
                                    "medical" ) )
str( df_work_data )
dim( df_work_data )
View( df_work_data )

# 1. 7개 권역별 가처분 소득/ 성별에 따른 가처분 소득 비교
df_work_sex <- select( df_work_data, "area", "income", "sex" )
str( df_work_sex )
dim( df_work_sex )
head( df_work_sex, 10 )
tail( df_work_sex )
View( df_work_sex )

# 권역과 성별에 대한 결측치/이상치 처리
df_work_sex$area <- ifelse( df_work_sex$area < 1 & df_work_sex$area > 7,
                             NA, df_work_data$area )
df_work_sex$sex <- ifelse( df_work_sex$sex < 1 & df_work_sex$sex > 2,
                             NA, df_work_sex$sex )
table( is.na( df_work_sex$area ) )
table( is.na( df_work_sex$sex ) )

# 권역과 성별 데이터값 변경
df_work_sex$area <- ifelse( df_work_sex$area == 1, "서울",
                            ifelse( df_work_sex$area == 2, "수도권",
                                    ifelse( df_work_sex$area == 3, "부산/경남/울산",
                                            ifelse( df_work_sex$area == 4, "대구/경북",
                                                    ifelse( df_work_sex$area == 5, "대전충남",
                                                            ifelse( df_work_sex$area == 6, "강원충북",
                                                                    "광주/전남/전북/제주" ) ) ) ) ) )

df_work_sex$sex <- ifelse( df_work_sex$sex == 1,
                            "male",
                            "female" )
View( df_work_sex )

# 가처분 소득 권역별/성별 평균 변수 추가
df_work_sex <- df_work_sex %>% 
  filter( !is.na( area ) & !is.na( sex ) ) %>% 
  group_by( area, sex ) %>%
  summarise( income_avg = mean( income ) )
head( df_work_sex )

# 권역별 남성 가처분 소득 평규 
df_work_sex_male <- df_work_sex %>%
  filter( sex == "male" )
df_work_sex_male <- df_work_sex_male %>%
  arrange( desc( income_avg ) )
head( df_work_sex_male, 10 )

# 권역별 여성 가처분 소득 평규 
df_work_sex_female <- df_work_sex %>%
  filter( sex == "female" )
df_work_sex_female <- df_work_sex_female %>%
  arrange( desc( income_avg ) )
head( df_work_sex_female, 10 )

# 권역별/성별 가처분 평균 소득 그래프
ggplot( data = df_work_sex_male, aes( x = area, y = income_avg )  ) + 
  geom_col()

ggplot( data = df_work_sex_female, aes( x = area, y = income_avg )  ) + 
  geom_col()
