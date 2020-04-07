#'---
#'title: "Capacity-소방/경창관서수밀도,  방재시설개수,  방재예산액"
#'author: "Kyungtak Kim"
#'date: '2020 3 26 '
#'output: github_document
#'
#'  
#'---

# 패키지 설치
#install.packages("sf")
#install.packages("tmap")
#install.packages("dplyr")

#+ library, warning=FALSE, message=FALSE
library(tidyverse)
library(sf)
library(tmap)
Sys.setenv(Language="En")
library(caret)
library(knitr)
library(leaflet)
library(rgdal)
library(htmltools)


#' # 원본 데이터 읽기  
#' 
DB <- as.data.frame(read.csv('input/capacity_db.csv'))
head(DB, 3)


#' ## 면적에 대한 분석  (area_km2)  
#' 무슨 면적인가? 각 자치단체의 면적...
#'  
DB_a <- DB %>% 
  select(NameK, SGG, area_km2)
head(DB_a, 3)

#'  
#+ fig.width=12, fig.height=25
DB_a %>% 
  ggplot(aes(x=fct_reorder(NameK, area_km2),
             y=area_km2))+
  geom_point(aes(color=factor(SGG)))+
  coord_flip()+
  theme(legend.position = "none")

#+ fig.width=6, fig.height=6 
DB_a %>% 
  ggplot(aes(area_km2))+
  geom_density()
DB_a %>% 
  ggplot(aes(area_km2))+
  geom_histogram(aes(color=factor(SGG)))+
  theme(legend.position = "none")


#' ## 소방관서수에 대한 분석  
#' 
DB_f <- DB %>% 
  select(NameK, SGG, contains("fire"))
head(DB_f, 3)
DB_f_p <- DB_f %>%                           # pivoting
  pivot_longer(c("X16_fire", "X17_fire", "X18_fire"),
               names_to = "year",
               values_to = "fire")
DB_f_p %>% 
  ggplot()+
  geom_density(aes(x=fire, y=..density.., color=year))

#' SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다.  
#' 
DB_f_p %>% 
  group_by(year) %>% 
  ggplot(aes(fire, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#'  
#+ fig.width=12, fig.height=25
DB_f_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(fire))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=fire))+
  geom_boxplot()+
  coord_flip()


#' ## 경찰관서수에 대한 분석  
#'  
DB_p <- DB %>% 
  select(NameK, SGG, contains("police"))
head(DB_p, 3)
DB_p_p <- DB_p %>%                           # pivoting
  pivot_longer(c("X16_police", "X17_police", "X18_police"),
               names_to = "year",
               values_to = "police")
DB_p_p %>% 
  ggplot()+
  geom_density(aes(x=police, y=..density.., color=year))


#' SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다.  
#' 
DB_p_p %>% 
  group_by(year) %>% 
  ggplot(aes(police, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#'  
#+ fig.width=12, fig.height=25
DB_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(police))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=police))+
  geom_boxplot()+
  coord_flip()

#' ## 연도별 (소방관서수+경찰서수) 밀도 산정  
#'  
result_s1 <- as.data.frame(matrix(nrow = 0, ncol = 1))
result_s2 <- as.data.frame(matrix(nrow = 0, ncol = 1))
result_s3 <- as.data.frame(matrix(nrow = 0, ncol = 1))
for(i in 1:161){
  work1 <- as.data.frame((DB$X16_fire[i]+DB$X16_police[i])/DB$area_km2[i])
  result_s1 <- rbind(result_s1, work1)
  work2 <- as.data.frame((DB$X17_fire[i]+DB$X17_police[i])/DB$area_km2[i])
  result_s2 <- rbind(result_s2, work2)
  work3 <- as.data.frame((DB$X18_fire[i]+DB$X18_police[i])/DB$area_km2[i])
  result_s3 <- rbind(result_s3, work3)
}
result_soc <-cbind(result_s1, result_s2, result_s3)
colnames(result_soc) <- c("X16_cap_soc","X17_cap_soc","X18_cap_soc")
head(result_soc,3)


#' ## (소방관서수+경찰관서수)밀도에 대한 분석  
#' **[주]소방/경찰관서수의 경우 밀도함수로 변화하여도 자료의 분포를 보면**
#' ** sqrt 또는 log  함수를 취하는 것이 바람직함**
#'   
result_soc1 <- cbind(DB[,2:3], result_soc) 
head(result_soc1, 3)
result_soc1_p <- result_soc1 %>%                           # pivoting
  pivot_longer(c("X16_cap_soc", "X17_cap_soc", "X18_cap_soc"),
               names_to = "year",
               values_to = "fp_density")
result_soc1_p %>% 
  ggplot()+
  geom_density(aes(x=fp_density, y=..density.., color=year))

#' SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다.  
#' 
result_soc1_p %>% 
  group_by(year) %>% 
  ggplot(aes(fp_density, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#'  
#+ fig.width=12, fig.height=25
result_soc1_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(fp_density))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=fp_density))+
  geom_boxplot()+
  coord_flip()


#' ## 방재시설개수 (개)에 대한 분석  
#'  
DB_y <- DB %>% 
  select(NameK, SGG, contains("phy"))
head(DB_y, 3)
DB_y_p <- DB_y %>%                           # pivoting
  pivot_longer(c("X16_cap_phy", "X17_cap_phy", "X18_cap_phy"),
               names_to = "year",
               values_to = "cap_phy")
DB_y_p %>% 
  ggplot()+
  geom_density(aes(x=cap_phy, y=..density.., color=year))

#' SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다. 
#' **소방/경찰관서수 자료에 비해서는 양호하나**
#' **sqr 변환을 시도하는 것이 적절할것 같음** 
#' 
DB_y_p %>% 
  group_by(year) %>% 
  ggplot(aes(cap_phy, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#'  
#+ fig.width=12, fig.height=25
DB_y_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(cap_phy))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=cap_phy))+
  geom_boxplot()+
  coord_flip()


#' ## 재난방재민방위 세출예산(천원) 자료에 대한 분석  
#' 
DB_x <- DB %>% 
  select(NameK, SGG, contains("ex_budget"))
head(DB_x, 3)
DB_x_p <- DB_x %>%                           # pivoting
  pivot_longer(c("X16_ex_budget", "X17_ex_budget", "X18_ex_budget"),
               names_to = "year",
               values_to = "ex_budget")
DB_x_p %>% 
  ggplot()+
  geom_density(aes(x=ex_budget, y=..density.., color=year))

#' SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다. 
#' **sqr 변환을 시도하는 것이 적절할것 같음** 
#' 
DB_x_p %>% 
  group_by(year) %>% 
  ggplot(aes(ex_budget, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#'  
#+ fig.width=12, fig.height=25
DB_x_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(ex_budget))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=ex_budget))+
  geom_boxplot()+
  coord_flip()



#' ## 재난방재민방위 본청예산(천원) 자료에 대한 분석  
#' 
DB_m <- DB %>% 
  select(NameK, SGG, contains("main_budget"))
head(DB_m, 3)
DB_m_p <- DB_m %>%                           # pivoting
  pivot_longer(c("X16_main_budget", "X17_main_budget", "X18_main_budget"),
               names_to = "year",
               values_to = "main_budget")

#' ### 본청예산의 경우 년도별 분포의 변화가 심하다  
#' **이 변화가 영향을 미치는 것이 타 요소보다 심할것으로 추정됨**
#' 
DB_m_p %>% 
  ggplot()+
  geom_density(aes(x=main_budget, y=..density.., color=year))

#' SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다. 
#' **광역시의 경우 예산이 없다.** 
#' 
DB_m_p %>% 
  group_by(year) %>% 
  ggplot(aes(main_budget, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#' ** 지방의 경우 예산분포의 차이가 심하다**
#' **경기도의 경우 가장 예산 변동이 심하고**
#' **도별예산이므로 도에서는 같은 값을 가진다.**
#' #'    
#+ fig.width=12, fig.height=25
DB_m_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(main_budget))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=main_budget))+
  geom_boxplot()+
  coord_flip()


#' ## ex_budget과 main_budget자료의 통합  
#' ### 본청예산을 시도별예산에 비례하여 배분   
#' 
# 연도별 방재예산액 산정
DB_eco <- as.data.frame(substr(DB$SGG,1,2))
head(DB_eco,3)
colnames(DB_eco) <- c("code")
DB_eco <- cbind(DB_eco,DB[,c(8,9,13,14,18,19)])
name<-unique(DB_eco$code)
head(DB_eco,3)

# 비례식
cap_eco <- function(x,y){
  return(x+(x/sum(x))*y)
}

result_e1 <- as.data.frame(matrix(nrow = 0, ncol = 1))
result_e2 <- as.data.frame(matrix(nrow = 0, ncol = 1))
result_e3 <- as.data.frame(matrix(nrow = 0, ncol = 1))  
for(i in 1:17){
  work <- as.data.frame(DB_eco[DB_eco$code==name[i],])
  work1 <- as.data.frame(cap_eco(work[,2],work[,3]))
  result_e1 <- rbind(result_e1,work1)
  work2 <- as.data.frame(cap_eco(work[,4],work[,5]))
  result_e2 <- rbind(result_e2,work2)
  work3 <- as.data.frame(cap_eco(work[,6],work[,7]))
  result_e3 <- rbind(result_e3,work3)
}
result_eco <-cbind(result_e1, result_e2, result_e3)
colnames(result_eco) <- c("X16_cap_eco","X17_cap_eco","X18_cap_eco")
head(result_eco, 3)


#' ## ex_budget과 main_budget자료의 통합 결과자료에 대한 분석  
#'  **본청예산을 시도별예산에 비례하여 배분**  
#'  
DB_c <- cbind(DB[,2:3], result_eco)
head(DB_c,3)
DB_c_p <- DB_c %>%                           # pivoting
  pivot_longer(c("X16_cap_eco", "X17_cap_eco", "X18_cap_eco"),
               names_to = "year",
               values_to = "cap_eco")
DB_c_p %>% 
  ggplot()+
  geom_density(aes(x=cap_eco, y=..density.., color=year))

#' SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다. 
#' 
DB_c_p %>% 
  group_by(year) %>% 
  ggplot(aes(cap_eco, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#'  
#+ fig.width=12, fig.height=25
DB_c_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(cap_eco))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=cap_eco))+
  geom_boxplot()+
  coord_flip()

#' 
#+ fig.width=6, fig.height=6
DB_c_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(cap_eco))%>%   
  filter(mean < 10000000) %>%   #평균 방재예산 10억미만
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=cap_eco))+
  geom_boxplot()+
  coord_flip()

#' 
DB_c_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(cap_eco))%>%   
  filter(mean > 50000000) %>%   #평균 방재예산 500억 이상
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=cap_eco))+
  geom_boxplot()+
  coord_flip()


#' # Capacity 지표별 정규화 - root 정규화  
#' 
# 연도별 Capacity 지표 데이터 생성 (방재개소수, 소방/경찰관서밀도, 방재예산)
DB_final <-cbind(DB[,c(5,10,15)],result_soc,result_eco)
head(DB_final,3)

# Capacity 지표별 root 표준화 함수 설정
standard_root <- function(x){
  return((sqrt(x)-min(sqrt(x)))/(max(sqrt(x))-min(sqrt(x))))
}

# 연도별 데이터 프레임에 root 표준화 적용
capacity <- as.data.frame(lapply(DB_final[,1:9],standard_root))
capacity <- cbind(DB[,1:3], capacity)

colnames(capacity)[4:12] <- c("X16_cap_phy_root", "X17_cap_phy_root", "X18_cap_phy_root",
                              "X16_cap_sco_root", "X17_cap_sco_root", "X18_cap_sco_root",
                              "X16_cap_eco_root", "X17_cap_eco_root", "X18_cap_eco_root")

# 16년~18년 Capacity 지수 산정
cap_index_16 <- as.data.frame((rowSums(capacity[,c(4,7,10)]))/3)
colnames(cap_index_16) <- c("X16_cap_index")
cap_index_17 <- as.data.frame((rowSums(capacity[,c(5,8,11)]))/3)
colnames(cap_index_17) <- c("X17_cap_index")
cap_index_18 <- as.data.frame((rowSums(capacity[,c(6,9,12)]))/3)
colnames(cap_index_18) <- c("X18_cap_index")
capacity <- cbind(capacity, c(cap_index_16,cap_index_17,cap_index_18))
head(capacity, 3)
summary(capacity$X16_cap_index)
summary(capacity$X17_cap_index)
summary(capacity$X18_cap_index)







#' # 최종 min-max 미포함 ---------------------------------------------------------  


result_final <-capacity[,13:15]  
head(result_final,3)

standard_reverse <- function(x){
  return(1-x)
}

# 연도별 Capacity 지수 (반전) 산정
result_final <- as.data.frame(lapply(result_final[,1:3],standard_reverse))
colnames(result_final) <- c("X16_capacity", "X17_capacity", "X18_capacity")
result_final <- cbind(DB[,1:3], result_final)
head(result_final,3)
summary(result_final)



#' # Mapping  
#' 
# 시군 shp 파일 불러오기
analysis <- st_read("input/analysis.shp")

# 폴리곤 에러 체크(기존 shp 파일을 에러 수정한 파일로 변경하였음)
#st_is_valid(analysis)
#library(lwgeom)
#analysis <- st_make_valid(analysis)
st_is_valid(analysis)

# shp파일에 연도별 Capacity 지수(표준화 적용) 추가
analysis <- right_join(analysis, result_final[,3:6])

# 폴리곤 단순화
analysis_simp <- st_simplify(analysis, dTolerance = 50)

#+ fig.width=12, fig.height=6
# 결과 확인
tmap_mode("plot")
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_capacity", "X17_capacity", "X18_capacity")
tm_shape(analysis_simp)+
  tm_polygons(facets,
              breaks=breaks,
              palette = c("green", "greenyellow", "yellow", "orange", "red"),
              legend.reverse = TRUE)+
  tm_facets(ncol = 3)+
  tm_layout(legend.position = c("right", "bottom"))+
  tm_compass(type = "rose",
             position = c("right", "top"),
             size = 1.5)+
  tm_scale_bar(breaks = c(0, 25, 50, 100, 150, 200),
               position = c("left", "bottom"))


###################
#' leaflet test
#' 
#+ fig.width=8, fig.height=6
a <- st_transform(analysis_simp, 4326)
pal <- colorBin(palette=c("green", "greenyellow", "yellow", "orange", "red"),
                domain=NULL,
                bins = c(0, .2, .4, .6, 0.8, 1),
                pretty = FALSE)

leaflet(a) %>% 
  setView(lng = 128, lat = 35.9, zoom = 7) %>% 
  # base groups
  addPolygons(color = ~pal(X16_capacity),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X16_capacity),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="capacity 2016") %>% 
  addPolygons(color = ~pal(X17_capacity),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X17_capacity),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="capacity 2017") %>%
  addPolygons(color = ~pal(X18_capacity),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X18_capacity),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="capacity 2018") %>%
  # overlay groups
  addProviderTiles(providers$Esri.WorldStreetMap,
                   group="Esri") %>%  #CartoDB.Positron
  addProviderTiles(providers$CartoDB.Positron,
                   group="CartoDB") %>%  
  addLegend("bottomright",
            pal = pal,
            values = ~X16_capacity,
            title = "Capacity Index",
            labFormat = labelFormat(digits=10),
            opacity = 1) %>% 
  hideGroup("CartoDB") %>% 
  #Layer controls
  addLayersControl(baseGroups = c("capacity 2016", "capacity 2017", "capacity 2018"),
                   overlayGroups = c("Esri", "CartoDB"),
                   options=layersControlOptions(collapsed=FALSE))

#'
# 결과값 저장
write.csv(result_final,'output/capacity_result1.csv', row.names = F)










#' # 최종 min-max 포함 ---------------------------------------------------------  



#' ## 각각의 요소별로 합하고/나누었으므로 다시 0~1사이로 rescaling  
#' 
# Capacity 지수 표준화 함수 및 반전 함수 설정
standard <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

standard_reverse <- function(x){
  return(1-x)
}

#' ## (1-x)로 반전하기 전의 자료 특성 분석  
#' **Capacity 지수의 특성 분석**
#'  
# 연도별 Capacity 지수 표준화(반전) 산정
result_final <- as.data.frame(lapply(capacity[,13:15],standard))  
head(result_final,3)
summary(result_final)
  
#' 연도별 확률밀도함수  
#'    
result_p <- cbind(DB[,2:3], result_final)  
head(result_p)
result_p_p <- result_p %>%                           # pivoting
  pivot_longer(c("X16_cap_index", "X17_cap_index", "X18_cap_index"),
               names_to = "year",
               values_to = "cap_index")
result_p_p %>% 
  ggplot()+
  geom_density(aes(x=cap_index, y=..density.., color=year))
result_p %>% 
  ggplot(aes(X17_cap_index))+
  geom_histogram(bins=100)

#' 
#+ fig.width=12, fig.height=25
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(cap_index))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=cap_index))+
  geom_boxplot()+
  coord_flip()

#' 
#+  fig.width=6, fig.height=6
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(cap_index))%>%   
  filter(mean < 0.15) %>%              #15% 이하
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=cap_index))+
  geom_boxplot()+
  coord_flip()

#' 
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(cap_index))%>%   
  filter(mean > 0.40) %>%               # 40% 이상
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=cap_index))+
  geom_boxplot()+
  coord_flip()

result_p_p %>% 
  group_by(year) %>% 
  ggplot(aes(cap_index, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

#' ## (1-x)로 반전한 후의 자료 특성 분석  
#' **(1-x) 이후 Capacity 지수의 특성 분석**
#'  
# 연도별 Capacity 지수 표준화(반전) 산정
result_final <- as.data.frame(lapply(result_final[,1:3],standard_reverse))
colnames(result_final) <- c("X16_capacity", "X17_capacity", "X18_capacity")
result_final <- cbind(DB[,1:3], result_final)
head(result_final,3)

#' 연도별 확률밀도함수  
#'    
result_p <- result_final %>%
  select(-Name)  
head(result_p)
result_p_p <- result_p %>%                           # pivoting
  pivot_longer(c("X16_capacity", "X17_capacity", "X18_capacity"),
               names_to = "year",
               values_to = "capacity")
result_p_p %>% 
  ggplot()+
  geom_density(aes(x=capacity, y=..density.., color=year))
result_p %>% 
  ggplot(aes(X17_capacity))+
  geom_histogram(bins=100)

#' 
#+ fig.width=12, fig.height=25
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(capacity))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=capacity))+
  geom_boxplot()+
  coord_flip()

#' 
#+  fig.width=6, fig.height=6
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(capacity))%>%   
  filter(mean < 0.50) %>%      # 50% 보다 작음지역
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=capacity))+
  geom_boxplot()+
  coord_flip()

#' 
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(capacity))%>%   
  filter(mean > 0.90) %>%      # 90% 보다 큰 지역
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=capacity))+
  geom_boxplot()+
  coord_flip()

result_p_p %>% 
  group_by(year) %>% 
  ggplot(aes(capacity, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")


#' # Mapping  
#' 
# 시군 shp 파일 불러오기
analysis <- st_read("input/analysis.shp")

# 폴리곤 에러 체크(기존 shp 파일을 에러 수정한 파일로 변경하였음)
#st_is_valid(analysis)
#library(lwgeom)
#analysis <- st_make_valid(analysis)
st_is_valid(analysis)

# shp파일에 연도별 Capacity 지수(표준화 적용) 추가
analysis <- right_join(analysis, result_final[,3:6])

# 폴리곤 단순화
analysis_simp <- st_simplify(analysis, dTolerance = 50)

#+ fig.width=12, fig.height=6
# 결과 확인
tmap_mode("plot")
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_capacity", "X17_capacity", "X18_capacity")
tm_shape(analysis_simp)+
  tm_polygons(facets,
              breaks=breaks,
              palette = c("green", "greenyellow", "yellow", "orange", "red"),
              legend.reverse = TRUE)+
  tm_facets(ncol = 3)+
  tm_layout(legend.position = c("right", "bottom"))+
  tm_compass(type = "rose",
             position = c("right", "top"),
             size = 1.5)+
  tm_scale_bar(breaks = c(0, 25, 50, 100, 150, 200),
               position = c("left", "bottom"))


###################
#' leaflet test
#' 
#+ fig.width=8, fig.height=6
a <- st_transform(analysis_simp, 4326)
pal <- colorBin(palette=c("green", "greenyellow", "yellow", "orange", "red"),
                domain=NULL,
                bins = c(0, .2, .4, .6, 0.8, 1),
                pretty = FALSE)

leaflet(a) %>% 
  setView(lng = 128, lat = 35.9, zoom = 7) %>% 
  # base groups
  addPolygons(color = ~pal(X16_capacity),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X16_capacity),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="capacity 2016") %>% 
  addPolygons(color = ~pal(X17_capacity),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X17_capacity),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="capacity 2017") %>%
  addPolygons(color = ~pal(X18_capacity),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X18_capacity),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="capacity 2018") %>%
  # overlay groups
  addProviderTiles(providers$Esri.WorldStreetMap,
                   group="Esri") %>%  #CartoDB.Positron
  addProviderTiles(providers$CartoDB.Positron,
                   group="CartoDB") %>%  
  addLegend("bottomright",
            pal = pal,
            values = ~X16_capacity,
            title = "Capacity Index",
            labFormat = labelFormat(digits=10),
            opacity = 1) %>% 
  hideGroup("CartoDB") %>% 
  #Layer controls
  addLayersControl(baseGroups = c("capacity 2016", "capacity 2017", "capacity 2018"),
                   overlayGroups = c("Esri", "CartoDB"),
                   options=layersControlOptions(collapsed=FALSE))

##############
#'# 결과값 저장  
#'
# 결과값 저장
write.csv(result_final,'output/capacity_result.csv', row.names = F)

# 열 명칭별 의미

# Name : 161개 시군별 영문명
# NameK : 161개 시군별 한글명
# SGG : 시군구 코드
# area_km2 : 시군면적(km2)
# X16_cap_phy : 16년도 방재시설개수(개)
# X17_cap_phy : 17년도 방재시설개수(개)
# X18_cap_phy : 18년도 방재시설개수(개)
# X16_fire : 16년도 소방관서수
# X17_fire : 17년도 소방관서수
# X18_fire : 18년도 소방관서수
# X16_ex_budget : 16년도 재난방재민방위 세출예산(천원)
# X17_ex_budget : 17년도 재난방재민방위 세출예산(천원)
# X18_ex_budget : 18년도 재난방재민방위 세출예산(천원)
# X16_main_budget : 16년도 재난방재민방위 본청예산(천원)
# X17_main_budget : 17년도 재난방재민방위 본청예산(천원)
# X18_main_budget : 18년도 재난방재민방위 본청예산(천원)
# X16_cap_soc : 16년도 소방-경찰관서수 밀도(개/km2)
# X17_cap_soc : 17년도 소방-경찰관서수 밀도(개/km2)
# X18_cap_soc : 18년도 소방-경찰관서수 밀도(개/km2)
# X16_cap_eco : 16년도 방재예산액(원)
# X17_cap_eco : 17년도 방재예산액(원)
# X18_cap_eco : 18년도 방재예산액(원)
# X16_cap_phy_root : 16년도 방재시설개수(root 표준화 적용)
# X17_cap_phy_root : 17년도 방재시설개수(root 표준화 적용)
# X18_cap_phy_root : 18년도 방재시설개수(root 표준화 적용)
# X16_cap_soc_root : 16년도 소방-경찰관서수 밀도(root 표준화 적용)
# X17_cap_soc_root : 17년도 소방-경찰관서수 밀도(root 표준화 적용)
# X18_cap_soc_root : 18년도 소방-경찰관서수 밀도(root 표준화 적용)
# X16_cap_eco_root : 16년도 방재예산액(root 표준화 적용)
# X17_cap_eco_root : 17년도 방재예산액(root 표준화 적용)
# X18_cap_eco_root : 18년도 방재예산액(root 표준화 적용)
# X16_cap_index : 16년도 Capacity 지수
# X17_cap_index : 17년도 Capacity 지수
# X18_cap_index : 18년도 Capacity 지수
# X16_capacity : 16년도 Capacity 지수(표준화 적용)
# X17_capacity : 17년도 Capacity 지수(표준화 적용)
# X18_capacity : 18년도 Capacity 지수(표준화 적용)

