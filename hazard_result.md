---
title: "Hazard-확률강우량"
author: "Kyungtak Kim"
date: '2020 3 26 '
output:
 html_document:
   keep_md: TRUE
  
   
 
---


```r
library(tidyverse)
library(sf)
library(tmap)
Sys.setenv(Language="En")
library(caret)
```

# 원본 데이터 읽기 / 특성 분석


```r
DB <- read.csv('input/hazard_db.csv')
head(DB)
```

```
##                   Name         NameK   SGG X16_ha_pro X17_ha_pro X18_ha_pro
## 1 Gangwon Gangneung-si 강원도 강릉시 42150   536.5288   526.7586   528.8285
## 2  Gangwon Goseong-gun 강원도 고성군 42820   384.9000   388.6000   408.0000
## 3   Gangwon Donghae-si 강원도 동해시 42170   412.4000   406.2000   401.2000
## 4  Gangwon Samcheok-si 강원도 삼척시 42230   360.6449   356.5321   352.4753
## 5    Gangwon Sokcho-si 강원도 속초시 42210   384.9000   388.6000   408.0000
## 6   Gangwon Yanggu-gun 강원도 양구군 42800   381.9000   381.4000   381.9000
```

원 확률강우량 자료(최근 30년간의 자료 이용)에 대한 연도별 확률밀도함수를
보면.....
 


```r
DB_h<- DB %>% 
  select(NameK, contains("pro"))
DB_h_p <- DB_h %>%                           # pivoting
  pivot_longer(c("X16_ha_pro", "X17_ha_pro", "X18_ha_pro"),
               names_to = "year",
               values_to = "p_rain")
DB_h_p %>% 
  ggplot()+
  geom_density(aes(x=p_rain, y=..density.., color=year))
```

![](hazard_result_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

lattice test
SGG는 시군 고유번호로 지역별 대략적인 분포를 알 수 있다.



```r
regVar <- c("X16_ha_pro", "X17_ha_pro", "X18_ha_pro")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = DB[, regVar], 
            y = DB$SGG, 
            plot = "scatter", 
            layout = c(3, 1))
```

![](hazard_result_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

각 시군별 16-18년사이의 확률강우량의 변화를 보면
충청남도 지역의 일부 지역이 변화가 가장 심하다.



```r
DB_h_p %>% 
  group_by(NameK) %>% 
  mutate(mean=mean(p_rain))%>% 
  ggplot(aes(x=fct_reorder(NameK, mean),
             y=p_rain))+
  geom_boxplot()+
  coord_flip()
```

![](hazard_result_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

# 확률강우량 정규화(Normalization Function)함수


```r
#standard <- function(x){
#  return((x-min(x))/(max(x)-min(x)))
#}
```

# 161개 시군별 변화 Mapping 



```r
# 연도별 데이터 프레임에 정규화 적용
#result <- as.data.frame(lapply(DB[,4:6],standard))
#colnames(result) <- c("X16_hazard", "X17_hazard", "X18_hazard")
#result <- cbind(DB[,1:3], result)


library(caret)  
pre <- preProcess(DB[,4:6], method=c("range"))  #Min-max scaling
pred <- predict(pre, DB[,4:6])
colnames(pred) <- c("X16_hazard", "X17_hazard", "X18_hazard")
result <- cbind(DB[,1:3], pred)



# 시군 shp 파일 불러오기
analysis <- st_read("input/analysis.shp")
```

```
## Reading layer `analysis' from data source `C:\00_R\0_Git\KRM_inha\input\analysis.shp' using driver `ESRI Shapefile'
## Simple feature collection with 161 features and 3 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: 746109.3 ymin: 1458771 xmax: 1387956 ymax: 2068444
## epsg (SRID):    NA
## proj4string:    +proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs
```

```r
# 폴리곤 에러 체크(기본 파일을 에러 수정한 파일로 변경하였음)
#st_is_valid(analysis)
#library(lwgeom)
#analysis <- st_make_valid(analysis)
st_is_valid(analysis)
```

```
##   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
##  [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
##  [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
##  [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
##  [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
##  [76] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
##  [91] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [106] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [121] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [136] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [151] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
# shp파일에 연도별 hazard 지수(표준화 적용) 추가
analysis <- right_join(analysis, result[,3:6])
```

```
## Joining, by = "SGG"
```

```r
# 폴리곤 단순화
analysis_simp <- st_simplify(analysis, dTolerance = 50)
```

```r
# 결과 확인
tmap_mode("plot")
```

```
## tmap mode set to plotting
```

```r
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_hazard", "X17_hazard", "X18_hazard")
tm_shape(analysis_simp)+
  tm_polygons(facets,
              breaks=breaks,
              palette = c("green", "greenyellow", "yellow", "orange", "red"),
              legend.reverse = TRUE)+
  tm_layout(legend.position = c("right", "bottom"))+
  tm_compass(type = "rose", position = c("right", "top"), size = 1.5)+
  tm_scale_bar(breaks = c(0, 25, 50, 100, 150, 200), position = c("left", "bottom"))+
  tm_facets(nrow=2)
```

![](hazard_result_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# 결과값 저장


```r
write.csv(result, 'output/hazard_result.csv', row.names = F)


# 열 명칭별 의미

# Name : 161개 시군별 영문명
# NameK : 161개 시군별 한글명
# SGG : 시군구 코드
# X16_ha_pro : 16년도 확률강우량(mm)
# X17_ha_pro : 17년도 확률강우량(mm)
# X18_ha_pro : 18년도 확률강우량(mm)
# X16_hazard : 16년도 hazard 지수(표준화 적용)
# X17_hazard : 17년도 hazard 지수(표준화 적용)
# X18_hazard : 18년도 hazard 지수(표준화 적용)
```


---
title: "hazard_result.R"
author: "Kyungtak Kim"
date: "2020-03-31"
---
