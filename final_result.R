#'---
#'title: "Qualitative Risk Analysis _ Inha"
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




#' # 최종 min-max 미포함 ----------------------------------------------------------  


#' # RISK = f(Hazard, Exposure, Vulnerability, Capacity)  
#' 
# 원본 데이터 읽기
hazard <- read.csv('output/hazard_result.csv')
exposure <- read.csv('output/exposure_result1.csv')
vulnerability <- read.csv('output/vulnerability_result1.csv')
capacity <- read.csv('output/capacity_result1.csv')


#데이터 결합
DB <- cbind(hazard, c(exposure[,4:5],vulnerability[,4:5],capacity[,4:6]))
head(DB, 3)
summary(DB)

#' # RISK 계산
#' 
# 16년~17년 홍수피해위험지수 산정
result_index_16 <- as.data.frame((rowSums(DB[,c("X16_hazard","X16_exposure","X16_vulnerability","X16_capacity")]))/4)
colnames(result_index_16) <- c("X16_result_index")
result_index_17 <- as.data.frame((rowSums(DB[,c("X17_hazard","X17_exposure","X17_vulnerability","X17_capacity")]))/4)
colnames(result_index_17) <- c("X17_result_index")
result_index <- cbind(DB[,1:3], c(result_index_16,result_index_17))
summary(result_index)


# 연도별 데이터 프레임에 표준화 적용 안함.
result <- result_index[,4:5]
colnames(result) <- c("X16_result", "X17_result")
result <- cbind(DB[,1:3], result)
head(result, 3)
summary(result)


#' 연도별 확률밀도함수  
#' 
result_p <- result %>% 
  select(-Name)
head(result_p, 3)
result_p_p <- result_p %>%                           # pivoting
  pivot_longer(c("X16_result", "X17_result"),
               names_to = "year",
               values_to = "result")
result_p_p %>% 
  ggplot()+
  geom_density(aes(x=result, y=..density.., color=year))
result_p %>% 
  ggplot(aes(X17_result))+
  geom_histogram(bins=100)

#' 
#+ fig.width=12, fig.height=25
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result))+
  geom_boxplot()+
  coord_flip()

#' 
#+  fig.width=6, fig.height=6
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result))%>%   
  filter(mean < 0.50) %>%            #35% 이하
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result))+
  geom_boxplot()+
  coord_flip()

#' 
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result))%>%   
  filter(mean > 0.65) %>%            #75% 이상
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result))+
  geom_boxplot()+
  coord_flip()

#' 
result_p %>% 
  mutate(dif=(X17_result - X16_result)) %>% 
  filter(NameK == "서울특별시")
result_p_dif <- result_p%>%
  mutate(dif=(X17_result - X16_result)) %>% 
  arrange(-dif)
knitr::kable(result_p_dif[1:10, ])  # 침수구역내 총인구가 늘어난 시군
knitr::kable(result_p_dif[152:161, ])  # 침수구역내 총인구가 줄어든 시군

result_p_p %>% 
  group_by(year) %>% 
  ggplot(aes(result, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")


#' # RISK ~ f(Hazard, Exposure, VUlnerability, Capacity) 분석  
#' 
#' ## RISK ~ Hazard  
#' 
result_p <- result %>% 
  pivot_longer(c("X16_result","X17_result"), names_to = "year", values_to = "risk")
hazard_p <- hazard %>% 
  pivot_longer(c("X16_hazard", "X17_hazard"), names_to = "year", values_to = "hazard")
hazard_path <- cbind(result_p, hazard_p[,6])

hazard_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, hazard,col=NameK))+
  geom_path(arrow=arrow(angle=10,
                        ends="last",
                        type="closed",
                        length = unit(0.15, "inches")),
            show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Hazard")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))


#' ## RISK ~ Exposure  
#' 
exposure_p <- exposure %>% 
  pivot_longer(c("X16_exposure", "X17_exposure"), names_to = "year", values_to = "exposure")
exposure_path <- cbind(result_p, exposure_p[,5])

exposure_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, exposure,col=NameK))+
  geom_path(arrow=arrow(angle=10,
                        ends="last",
                        type="closed",
                        length = unit(0.15, "inches")),
            show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Exposure")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))


#' ## RISK ~ Vulnerability  
#' 
vulnerability_p <- vulnerability %>% 
  pivot_longer(c("X16_vulnerability", "X17_vulnerability"), names_to = "year", values_to = "vulnerability")
vulnerability_path <- cbind(result_p, vulnerability_p[,5])

vulnerability_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, vulnerability,col=NameK))+
  geom_path(arrow=arrow(angle=10,
                        ends="last",
                        type="closed",
                        length = unit(0.15, "inches")),
            show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Vulnerability")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))


#' ## RISK ~ Capacity  
#' 
capacity_p <- capacity %>% 
  pivot_longer(c("X16_capacity", "X17_capacity"), names_to = "year", values_to = "capacity")
capacity_path <- cbind(result_p, capacity_p[,6])

capacity_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, capacity, col=NameK))+
  geom_path(arrow=arrow(angle=10,
                        ends="last",
                        type="closed",
                        length = unit(0.15, "inches")),
            show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Capacity")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))

#' # Mapping      
#' 시군 shp 파일 불러오기
analysis <- st_read("input/analysis.shp")

# 폴리곤 에러 체크(기본 파일을 에러 수정한 파일로 변경하였음)
#st_is_valid(analysis)
#library(lwgeom)
#analysis <- st_make_valid(analysis)
st_is_valid(analysis)

# shp파일에 연도별 홍수피해위험지수(표준화 적용) 추가
analysis <- right_join(analysis, result[,3:5])

# 폴리곤 단순화
analysis_simp <- st_simplify(analysis, dTolerance = 50)

# 결과 확인
tmap_mode("plot")
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_result", "X17_result")
tm_shape(analysis_simp)+
  tm_polygons(facets,
              breaks=breaks,
              palette = c("green", "greenyellow", "yellow", "orange", "red"),
              legend.reverse = TRUE)+
  tm_facets(ncol = 2)+
  tm_layout(legend.position = c("right", "bottom"))+
  tm_compass(type = "rose",
             position = c("right", "top"),
             size = 2.0)+
  tm_scale_bar(breaks = c(0, 25, 50, 100, 150, 200),
               position = c("left", "bottom"))


######################
#library(leaflet)
#library(rgdal)
#library(htmltools)
#+ fig.width=8, fig.height=6
a <- st_transform(analysis_simp, 4326)
pal <- colorBin(
  palette=c("green", "greenyellow", "yellow", "orange", "red"),
  domain=NULL,
  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  pretty = FALSE)

leaflet(a) %>% 
  setView(lng = 128, lat = 35.9, zoom = 7) %>% 
  # base groups
  addPolygons(color = ~pal(X16_result),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X16_result),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="result 2016") %>% 
  addPolygons(color = ~pal(X17_result),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X17_result),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="result 2017") %>% 
  #overlay groups
  addProviderTiles(providers$Esri.WorldStreetMap,
                   group="Esri") %>%  
  addProviderTiles(providers$CartoDB.Positron,
                   group="CartoDB") %>%  
  addLegend("bottomright",
            pal = pal,
            values = ~X17_result,
            title = "RISK Index",
            labFormat = labelFormat(digits=10),
            opacity = 1) %>% 
  hideGroup("CartoDB") %>% 
  #Layer controls
  addLayersControl(baseGroups = c("result 2016", "result 2017"),
                   overlayGroups = c("Esri", "CartoDB"),
                   options=layersControlOptions(collapsed=FALSE))

#' # 결과값 저장  
#' 
write.csv(result, 'output/final_result1.csv')











#' # 최종 min-max 포함 ---------------------------------------------------------
#' # RISK = f(Hazard, Exposure, Vulnerability, Capacity)  
#' 
# 원본 데이터 읽기
hazard <- read.csv('output/hazard_result.csv')
exposure <- read.csv('output/exposure_result.csv')
vulnerability <- read.csv('output/vulnerability_result.csv')
capacity <- read.csv('output/capacity_result.csv')


#데이터 결합
DB <- cbind(hazard, c(exposure[,4:5],vulnerability[,4:5],capacity[,4:6]))
head(DB, 3)
summary(DB)

#' # RISK 계산
#' 
# 16년~17년 홍수피해위험지수 산정
result_index_16 <- as.data.frame((rowSums(DB[,c("X16_hazard","X16_exposure","X16_vulnerability","X16_capacity")]))/4)
colnames(result_index_16) <- c("X16_result_index")
result_index_17 <- as.data.frame((rowSums(DB[,c("X17_hazard","X17_exposure","X17_vulnerability","X17_capacity")]))/4)
colnames(result_index_17) <- c("X17_result_index")
result_index <- cbind(DB[,1:3], c(result_index_16,result_index_17))
summary(result_index)

# 홍수피해위험지수 표준화 함수 설정
standard <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# 연도별 데이터 프레임에 표준화 적용
result <- as.data.frame(lapply(result_index[,4:5],standard))
colnames(result) <- c("X16_result", "X17_result")
result <- cbind(DB[,1:3], result)
head(result, 3)


#' 연도별 확률밀도함수  
#' 
result_p <- result %>% 
  select(-Name)
head(result_p, 3)
result_p_p <- result_p %>%                           # pivoting
  pivot_longer(c("X16_result", "X17_result"),
               names_to = "year",
               values_to = "result")
result_p_p %>% 
  ggplot()+
  geom_density(aes(x=result, y=..density.., color=year))
result_p %>% 
  ggplot(aes(X17_result))+
  geom_histogram(bins=100)

#' 
#+ fig.width=12, fig.height=25
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result))+
  geom_boxplot()+
  coord_flip()

#' 
#+  fig.width=6, fig.height=6
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result))%>%   
  filter(mean < 0.35) %>%            #35% 이하
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result))+
  geom_boxplot()+
  coord_flip()

#' 
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result))%>%   
  filter(mean > 0.75) %>%            #75% 이상
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result))+
  geom_boxplot()+
  coord_flip()

#' 
result_p %>% 
  mutate(dif=(X17_result - X16_result)) %>% 
  filter(NameK == "서울특별시")
result_p_dif <- result_p%>%
  mutate(dif=(X17_result - X16_result)) %>% 
  arrange(-dif)
knitr::kable(result_p_dif[1:10, ])  # 침수구역내 총인구가 늘어난 시군
knitr::kable(result_p_dif[152:161, ])  # 침수구역내 총인구가 줄어든 시군

result_p_p %>% 
  group_by(year) %>% 
  ggplot(aes(result, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")


#' # RISK ~ f(Hazard, Exposure, VUlnerability, Capacity) 분석  
#' 
#' ## RISK ~ Hazard  
#' 
result_p <- result %>% 
  pivot_longer(c("X16_result","X17_result"), names_to = "year", values_to = "risk")
hazard_p <- hazard %>% 
  pivot_longer(c("X16_hazard", "X17_hazard"), names_to = "year", values_to = "hazard")
hazard_path <- cbind(result_p, hazard_p[,6])

hazard_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, hazard,col=NameK))+
  geom_path(arrow=arrow(angle=10,
                      ends="last",
                      type="closed",
                      length = unit(0.15, "inches")),
          show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Hazard")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))


#' ## RISK ~ Exposure  
#' 
exposure_p <- exposure %>% 
  pivot_longer(c("X16_exposure", "X17_exposure"), names_to = "year", values_to = "exposure")
exposure_path <- cbind(result_p, exposure_p[,5])

exposure_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, exposure,col=NameK))+
  geom_path(arrow=arrow(angle=10,
                        ends="last",
                        type="closed",
                        length = unit(0.15, "inches")),
            show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Exposure")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))


#' ## RISK ~ Vulnerability  
#' 
vulnerability_p <- vulnerability %>% 
  pivot_longer(c("X16_vulnerability", "X17_vulnerability"), names_to = "year", values_to = "vulnerability")
vulnerability_path <- cbind(result_p, vulnerability_p[,5])

vulnerability_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, vulnerability,col=NameK))+
  geom_path(arrow=arrow(angle=10,
                        ends="last",
                        type="closed",
                        length = unit(0.15, "inches")),
            show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Vulnerability")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))


#' ## RISK ~ Capacity  
#' 
capacity_p <- capacity %>% 
  pivot_longer(c("X16_capacity", "X17_capacity"), names_to = "year", values_to = "capacity")
capacity_path <- cbind(result_p, capacity_p[,6])

capacity_path %>% 
  group_by(NameK) %>% 
  filter(str_detect(NameK, "^강원") ) %>% 
  ggplot(aes(risk, capacity, col=NameK))+
  geom_path(arrow=arrow(angle=10,
                        ends="last",
                        type="closed",
                        length = unit(0.15, "inches")),
            show.legend = F)+
  geom_point(size=2, alpha=0.4, show.legend = F)+
  geom_vline(xintercept = 0.5, alpha=0.3)+
  geom_hline(yintercept = 0.5, alpha=0.3)+
  labs(x="RISK", y="Capacity")+
  directlabels::geom_dl(aes(label=NameK),
                        method = list("first.points",rot=45), 
                        position = "identity",
                        alpha=0.3)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))



#' # Mapping      
#' 시군 shp 파일 불러오기
analysis <- st_read("input/analysis.shp")

# 폴리곤 에러 체크(기본 파일을 에러 수정한 파일로 변경하였음)
#st_is_valid(analysis)
#library(lwgeom)
#analysis <- st_make_valid(analysis)
st_is_valid(analysis)

# shp파일에 연도별 홍수피해위험지수(표준화 적용) 추가
analysis <- right_join(analysis, result[,3:5])

# 폴리곤 단순화
analysis_simp <- st_simplify(analysis, dTolerance = 50)

# 결과 확인
tmap_mode("plot")
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_result", "X17_result")
tm_shape(analysis_simp)+
  tm_polygons(facets,
              breaks=breaks,
              palette = c("green", "greenyellow", "yellow", "orange", "red"),
              legend.reverse = TRUE)+
  tm_facets(ncol = 2)+
  tm_layout(legend.position = c("right", "bottom"))+
  tm_compass(type = "rose",
             position = c("right", "top"),
             size = 2.0)+
  tm_scale_bar(breaks = c(0, 25, 50, 100, 150, 200),
               position = c("left", "bottom"))


######################
#library(leaflet)
#library(rgdal)
#library(htmltools)
#+ fig.width=8, fig.height=6
a <- st_transform(analysis_simp, 4326)
pal <- colorBin(
  palette=c("green", "greenyellow", "yellow", "orange", "red"),
  domain=NULL,
  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  pretty = FALSE)

leaflet(a) %>% 
  setView(lng = 128, lat = 35.9, zoom = 7) %>% 
  # base groups
  addPolygons(color = ~pal(X16_result),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X16_result),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="result 2016") %>% 
  addPolygons(color = ~pal(X17_result),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              label = ~htmlEscape(NameK),
              popup = ~htmlEscape(X17_result),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group="result 2017") %>% 
  #overlay groups
  addProviderTiles(providers$Esri.WorldStreetMap,
                   group="Esri") %>%  
  addProviderTiles(providers$CartoDB.Positron,
                   group="CartoDB") %>%  
  addLegend("bottomright",
            pal = pal,
            values = ~X17_result,
            title = "RISK Index",
            labFormat = labelFormat(digits=10),
            opacity = 1) %>% 
  hideGroup("CartoDB") %>% 
  #Layer controls
  addLayersControl(baseGroups = c("result 2016", "result 2017"),
                   overlayGroups = c("Esri", "CartoDB"),
                   options=layersControlOptions(collapsed=FALSE))

#############################
#' # 결과값 저장  
#' 
write.csv(result, 'output/final_result.csv')


# 열 명칭별 의미

# Name : 161개 시군별 영문명
# NameK : 161개 시군별 한글명
# SGG : 시군구 코드
# X16_hazard : 16년도 hazard 지수(표준화 적용)
# X17_hazard : 17년도 hazard 지수(표준화 적용)
# X18_hazard : 18년도 hazard 지수(표준화 적용)
# X16_exposure : 16년도 Exposure 지수(표준화 적용)
# X17_exposure : 17년도 Exposure 지수(표준화 적용)
# X16_vulnerability : 16년도 Vulnerability 지수(표준화 적용)
# X17_vulnerability : 17년도 Vulnerability 지수(표준화 적용)
# X16_capacity : 16년도 Capacity 지수(표준화 적용)
# X17_capacity : 17년도 Capacity 지수(표준화 적용)
# X18_capacity : 18년도 Capacity 지수(표준화 적용)
# X16_result_index : 16년도 홍수피해위험지수
# X17_result_index : 17년도 홍수피해위험지수
# X16_result : 16년도 홍수피해위험지수(표준화 적용)
# X17_result : 17년도 홍수피해위험지수(표준화 적용)

