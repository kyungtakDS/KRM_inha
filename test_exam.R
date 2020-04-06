install.packages("caret")
library(caret)
preproc1 <- preProcess(dat[,c(1:4,6)], method=c("center", "scale"))
norm1 <- predict(preproc1, dat[,c(1:4,6)])
summary(norm1)


dat_scaled <- as.data.frame(scale(dat[,c(1:4,6)]))
summary(dat_scaled$Income)



summary(dat$Income)
logincome = log(dat$Income)
summary(logincome)



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




library(caret)  
pre <- preProcess(DB[,4:6], method=c("range"))  #Min-max scaling
pred <- predict(pre, DB[,4:6])
colnames(pred) <- c("X16_hazard", "X17_hazard", "X18_hazard")
result <- cbind(DB[,1:3], pred)



DB_e_dif[1:10,]
DB_e_dif[152:161,]






#' ## 표준화이전 RISK Index 특성 분석    
#' 연도별 확률밀도함수  
#' 
#'       
result_p <- result_index %>% 
  select(-Name)
head(result_p, 3)
result_p_p <- result_p %>%                           # pivoting
  pivot_longer(c("X16_result_index", "X17_result_index"),
               names_to = "year",
               values_to = "result_index")
result_p_p %>% 
  ggplot()+
  geom_density(aes(x=result_index, y=..density.., color=year))
result_p %>% 
  ggplot(aes(X17_result_index))+
  geom_histogram(bins=100)

#' 
#+ fig.width=12, fig.height=25
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result_index))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result_index))+
  geom_boxplot()+
  coord_flip()

#' 
#+  fig.width=6, fig.height=6
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result_index))%>%   
  filter(mean < 0.5) %>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result_index))+
  geom_boxplot()+
  coord_flip()

#' 
result_p_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(result_index))%>%   
  filter(mean > 0.65) %>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=result_index))+
  geom_boxplot()+
  coord_flip()

#' 
result_p %>% 
  mutate(dif=(X17_result_index - X16_result_index)) %>% 
  filter(NameK == "서울특별시")
result_p_dif <- result_p%>%
  mutate(dif=(X17_result_index - X16_result_index)) %>% 
  arrange(-dif)
knitr::kable(result_p_dif[1:10, ])  # 침수구역내 총인구가 늘어난 시군
knitr::kable(result_p_dif[152:161, ])  # 침수구역내 총인구가 줄어든 시군

result_p_p %>% 
  group_by(year) %>% 
  ggplot(aes(result_index, SGG))+
  geom_point(aes(color=factor(SGG)))+
  facet_grid(. ~year)+
  theme(legend.position = "none")

# 연도별 데이터 프레임에 표준화 적용 (column 명칭 변경)
result <- as.data.frame(result_index[,4:5])
colnames(result) <- c("X16_result", "X17_result")
result <- cbind(DB[,1:3], result)
head(result,3)

#' # Mapping -1 (표준화이전)  
#' 
# 시군 shp 파일 불러오기
library(sf)
analysis <- st_read("input/analysis.shp")


# 폴리곤 에러 체크(기본 파일을 에러 수정한 파일로 변경하였음)
#st_is_valid(analysis)
#library(lwgeom)
#analysis <- st_make_valid(analysis)
st_is_valid(analysis)


# shp파일에 연도별 홍수피해위험지수(표준화 적용) 추가
library(dplyr)
analysis <- right_join(analysis, result[,3:5])


# 폴리곤 단순화
library(tmap)
analysis_simp <- st_simplify(analysis, dTolerance = 50)


# 결과 확인
tmap_mode("plot")
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_result", "X17_result")
tm_shape(analysis_simp)+
  tm_polygons(facets, breaks=breaks, palette = c("green", "greenyellow", "yellow", "orange", "red"), legend.reverse = TRUE)+
  tm_facets(ncol = 2)+
  tm_layout(legend.position = c("right", "bottom"))+
  tm_compass(type = "rose", position = c("right", "top"), size = 2.0)+
  tm_scale_bar(breaks = c(0, 25, 50, 100, 150, 200), position = c("left", "bottom"))







