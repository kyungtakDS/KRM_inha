#'---
#'title: "Exposure - 총주택,총인구,평균공시지가"
#'author: "Kyungtak Kim"
#'date: '2020 3 26 '
#'output:
#'  html_document:
#'    keep_md: TRUE
#'   
#'    
#'  
#'---


#+ library, warning=FALSE, message=FALSE
library(tidyverse)
library(sf)
library(tmap)
Sys.setenv(Language="En")
library(caret)
library(knitr)



#' # 원본 데이터 읽기 / 특성 분석
DB <- read.csv('input/exposure_db.csv')
head(DB)


#' ## 총주택수 자료 특성(_ex_str)  
#' 
#' 연도별 확률밀도함수
#' 침수구역내의 주택수에 대한 분포를 보면 0-500채 사이가 가장 높다
#'  

DB_s<- DB %>% 
  select(NameK, contains("str"))
DB_s_p <- DB_s %>%                           # pivoting
  pivot_longer(c("X16_ex_str", "X17_ex_str"),
               names_to = "year",
               values_to = "house")
DB_s_p %>% 
  ggplot()+
  geom_density(aes(x=house, y=..density.., color=year))
DB_s %>% 
  ggplot(aes(X16_ex_str))+
  geom_histogram(bins=200)



#' outlier를 찾기 boxplot을 년도 별로 그려본다.  
#' 최대값은 서울의 값이며, 큰 값들의 영향을 조금 줄이는 효과를 보기 위해 
#' z-score 보다는 min-max scaling(보통 normalizaiton이라고 하고,  
#' 경우에 따라 standardization이라고도 함)를 사용
#' 
DB_s_p %>%
  ggplot(aes(year, house))+
  geom_boxplot()
  

#' 침수구역내 총건축물수 
#' 
#+ fig.width=12, fig.height=25
DB_s_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(house))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=house))+
  geom_boxplot()+
  coord_flip()

#' 총건축물수가 적은 지역에 대한 분포 비교
#+ fig.width=12, fig.height=12
DB_s_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(house))%>%   
  filter(mean < 300) %>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=house))+
  geom_boxplot()+
  coord_flip()


#' 총건축물수가 많은 지역에 대한 분포 비교  
#' 인천광역시의 경우 침수구역내 총주택수가 적다.
#' 제주특별자치도의 경우 침수구역내 총주택수가 많은 편에 속한다.(소하천때문??)
#' 인천광역시의 경우 총주택수 (16년 2065 , 17년 2373채, 차이 308채 )
DB_s_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(house))%>%   
  filter(mean > 10000) %>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=house))+
  geom_boxplot()+
  coord_flip()


#' 년도별 침수구역내 총주택수의 변화  
#' 
#' **총주택수가 2016년에 비해 2017년에 줄어든 것은 이지역의 재개발**  
#' **로 이해 단독주택이 아파트로 바뀌어서 여러 객체가 하나의 객체로**  
#' **인식된 것이 아닌지? check해볼 필요가 있다**  
#'
#' 
DB_s %>% 
  mutate(dif=(X17_ex_str - X16_ex_str)) %>% 
  filter(NameK == "인천광역시")
DB_s_dif <- DB_s%>%
  mutate(dif=(X17_ex_str - X16_ex_str)) %>% 
  arrange(-dif)
knitr::kable(DB_s_dif[1:10, ])  # 침수구역내 총주택수가 늘어난 시군
knitr::kable(DB_s_dif[152:161, ])  # 침수구역내 총주택수가 줄어든 시군
DB_s_dif[1:10,]
DB_s_dif[152:161,]



#' lattice test
regVar <- c("X16_ex_str", "X17_ex_str")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = DB[, regVar], 
            y = DB$SGG, 
            plot = "scatter", 
            layout = c(2, 1))




#' ## 총인구수 자료 특성(_ex_pop)  
#' 
#' 연도별 확률밀도함수
#' 침수구역내의 인구수에 대한 분포
#'  
DB_p <- DB %>% 
  select(NameK, contains("pop"))
DB_p_p <- DB_p %>%                           # pivoting
  pivot_longer(c("X16_ex_pop", "X17_ex_pop"),
               names_to = "year",
               values_to = "people")
DB_p_p %>% 
  ggplot()+
  geom_density(aes(x=people, y=..density.., color=year))
DB_p %>% 
  ggplot(aes(X17_ex_pop))+
  geom_histogram(bins=200)



#' 침수구역내 총인구수 
#' 
#+ fig.width=12, fig.height=25
DB_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(people))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=people))+
  geom_boxplot()+
  coord_flip()


#' 총인구수가 적은 지역에 대한 분포 비교
#+ fig.width=12, fig.height=12
DB_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(people))%>%   
  filter(mean < 300) %>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=people))+
  geom_boxplot()+
  coord_flip()


#' 총인구수가 많은 지역에 대한 분포 비교  
#' 
DB_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(people))%>%   
  filter(mean > 100000) %>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=people))+
  geom_boxplot()+
  coord_flip()


#' 침수구역내 총인구수의 변화  
#' 년도별 침수구역내 총인구수의 변화
DB_p %>% 
  mutate(dif=(X17_ex_pop - X16_ex_pop)) %>% 
  filter(NameK == "서울특별시")
DB_p_dif <- DB_p%>%
  mutate(dif=(X17_ex_pop - X16_ex_pop)) %>% 
  arrange(-dif)
knitr::kable(DB_p_dif[1:10, ])  # 침수구역내 총인구가 늘어난 시군
knitr::kable(DB_p_dif[152:161, ])  # 침수구역내 총인구가 줄어든 시군
DB_p_dif[1:10,]
DB_p_dif[152:161,]




#' lattice test
regVar <- c("X16_ex_pop", "X17_ex_pop")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = DB[, regVar], 
            y = DB$SGG, 
            plot = "scatter", 
            layout = c(2, 1))



#' ## 평균공시지가 자료 특성(_ex_eco)  
#' 
#' 연도별 확률밀도함수
#' 침수구역내의 평균공시지가에 대한 분포
#'  
DB_e <- DB %>% 
  select(NameK, contains("eco"))
DB_e_p <- DB_e %>%                           # pivoting
  pivot_longer(c("X16_ex_eco", "X17_ex_eco"),
               names_to = "year",
               values_to = "price")
DB_e_p %>% 
  ggplot()+
  geom_density(aes(x=price, y=..density.., color=year))
DB_e %>% 
  ggplot(aes(X16_ex_eco))+
  geom_histogram(bins=200)



#' 침수구역내 평균공시지가 
#' 
#+ fig.width=12, fig.height=25
DB_e_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(price))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=price))+
  geom_boxplot()+
  coord_flip()


#' 평균공시지가 작은 지역에 대한 분포 비교
#+ fig.width=12, fig.height=12
DB_e_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(price))%>%   
  filter(mean < 10000) %>%    # 만원
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=price))+
  geom_boxplot()+
  coord_flip()


#' 평균공시지가가 큰 지역에 대한 분포 비교  
#' 
DB_e_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(price))%>%   
  filter(mean > 500000) %>%   # 50만원
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=price))+
  geom_boxplot()+
  coord_flip()


#' 침수구역내 평균공시지가의 변화  
#' 년도별 침수구역내 평균공시지가의 변화
#'   
#' **check할것-서울, 광명이 타 지역에 비해 너무 크다.?**  
#' **check할것-인천광역시의 공시지가가 떨어졌는지???**  
#' 
#' 
DB_e %>% 
  mutate(dif=(X17_ex_eco - X16_ex_eco)) %>% 
  filter(NameK == "서울특별시")
DB_e_dif <- DB_p%>%
  mutate(dif=(X17_ex_pop - X16_ex_pop)) %>% 
  arrange(-dif)
knitr::kable(DB_e_dif[1:10, ])  # 침수구역내 평균공시지가가 늘어난 시군
knitr::kable(DB_e_dif[152:161, ])  # 침수구역내 평균공시지가가 줄어든 시군
DB_e_dif[1:10,]
DB_e_dif[152:161,]


#' lattice test
regVar <- c("X16_ex_eco", "X17_ex_eco")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = DB[, regVar], 
            y = DB$SGG, 
            plot = "scatter", 
            layout = c(2, 1))


#' # Exposure 정규화(Normalization Function)함수 - log 정규화
standard_log <- function(x){
  return((log(x,base=10)-min(log(x,base=10)))/(max(log(x,base=10))-min(log(x,base=10))))
}


#' # 161개 시군별 변화 Mapping 
#' 
# 연도별 데이터 프레임에 정규화 적용
exposure <- as.data.frame(lapply(DB[,4:9],standard_log))
exposure <- cbind(DB[,1:3], exposure)
colnames(exposure)[4:9] <- c("X16_ex_str_log", "X16_ex_pop_log", "X16_ex_eco_log",
                             "X17_ex_str_log", "X17_ex_pop_log", "X17_ex_eco_log")


# 16년~17년 Exposure 지수 산정
ex_index_16 <- as.data.frame((rowSums(exposure[,4:6]))/3)
colnames(ex_index_16) <- c("X16_ex_index")
ex_index_17 <- as.data.frame((rowSums(exposure[,7:9]))/3)
colnames(ex_index_17) <- c("X17_ex_index")
exposure <- cbind(exposure, c(ex_index_16,ex_index_17))


# Exposure 지수 표준화 함수 설정
standard <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


# 연도별 Exposure 지수 표준화 산정
result <- as.data.frame(lapply(exposure[,10:11],standard))
colnames(result) <- c("X16_exposure", "X17_exposure")
result <- cbind(DB[,1:3], result)



# 시군 shp 파일 불러오기
library(sf)
analysis <- st_read("input/analysis.shp")


# 폴리곤 에러 체크(기본 파일을 에러 수정한 파일로 변경하였음)
#st_is_valid(analysis)
#library(lwgeom)
#analysis <- st_make_valid(analysis)
st_is_valid(analysis)


# shp파일에 연도별 Exposure 지수(표준화 적용) 추가
library(dplyr)
analysis <- right_join(analysis, result[,3:5])


# 폴리곤 단순화
library(tmap)
analysis_simp <- st_simplify(analysis, dTolerance = 50)



#+ fig.width=12, fig.height=6
# 결과 확인
tmap_mode("plot")
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_exposure", "X17_exposure")
tm_shape(analysis_simp)+
  tm_polygons(facets,
              breaks=breaks,
              palette = c("green", "greenyellow", "yellow", "orange", "red"),
              legend.reverse = TRUE)+
  tm_facets(ncol = 2)+
  tm_layout(legend.position = c("right", "bottom"))+
  tm_compass(type = "rose", position = c("right", "top"), size = 2.5)+
  tm_scale_bar(breaks = c(0, 25, 50, 100, 150, 200), position = c("left", "bottom"))



######################
library(leaflet)
library(rgdal)
library(htmltools)
#+ fig.width=8, fig.height=6
a <- st_transform(analysis_simp, 4326)
pal <- colorBin(
  palette=c("green", "greenyellow", "yellow", "orange", "red"),
  domain=NULL,
  bins = c(0, .2, .4, .6, 0.8, 1),
  pretty = FALSE)

leaflet(a) %>% 
  setView(lng = 128, lat = 35.9, zoom = 7) %>% 
  # base groups
  addPolygons(color = ~pal(X16_exposure),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X16_exposure),
              highlightOptions = highlightOptions(
                color = "white", weight = 2,
                bringToFront = TRUE),
              group="Exposure 2016") %>% 
  addPolygons(color = ~pal(X17_exposure),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X17_exposure),
              highlightOptions = highlightOptions(
                color = "white", weight = 2,
                bringToFront = TRUE),
              group="Exposure 2017") %>% 
  #overlay groups
  addProviderTiles(providers$Esri.WorldStreetMap,
                   group="Esri") %>%  
  addLegend("bottomright", pal = pal, values = ~X17_exposure,
            title = "Exposure Index",
            labFormat = labelFormat(digits=10),
            opacity = 1) %>% 
  #Layer controls
  addLayersControl(
    baseGroups = c("Exposure 2016", "Exposure 2017"),
    overlayGroups = c("Esri"),
    options=layersControlOptions(collapsed=FALSE)
  )


#############################





# 결과값 저장
write.csv(result, 'output/exposure_result.csv', row.names = F)


# 열 명칭별 의미

# Name : 161개 시군별 영문명
# NameK : 161개 시군별 한글명
# SGG : 시군구 코드
# X16_ex_str : 16년도 총 건축물수(개)
# X17_ex_str : 17년도 총 건축물수(개)
# X16_ex_pop : 16년도 총 인구수(명)
# X17_ex_pop : 17년도 총 인구수(명)
# X16_ex_eco : 16년도 평균공시지가(원/m2)
# X17_ex_eco : 17년도 평균공시지가(원/m2)
# X16_ex_str_log : 16년도 총 건축물수(log 표준화 적용)
# X17_ex_str_log : 17년도 총 건축물수(log 표준화 적용)
# X16_ex_pop_log : 16년도 총 인구수(log 표준화 적용)
# X17_ex_pop_log : 17년도 총 인구수(log 표준화 적용)
# X16_ex_eco_log : 16년도 평균공시지가(log 표준화 적용)
# X17_ex_eco_log : 17년도 평균공시지가(log 표준화 적용)
# X16_ex_index : 16년도 Exposure 지수
# X17_ex_index : 17년도 Exposure 지수
# X16_exposure : 16년도 Exposure 지수(표준화 적용)
# X17_exposure : 17년도 Exposure 지수(표준화 적용)




