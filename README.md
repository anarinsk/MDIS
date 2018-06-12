﻿# MDIS 

## 목적 
2017년 1분기와 2018년 1분기 "가계동향조사"의 소득 항목 비교를 살펴보기 위한 R 코드 및 문서화, Shiny app 등을 제공한다. 

## 유의사항 

  * 디렉토리 의존성에서 벗어나려면 `RStudio`에서 제공하는 project 기능을 이용하라. `MDIS.proj`을 먼저 로드하고 나머지 코드를 실행하면 된다. 
  * 그렇지 않을 경우, 코드를 참고해 `setwd()` 적절하게 활용하라. 하지만 권장하지는 않는다. 
  
## Directory desc. 

### code 

분석을 위한 코드 

  1. `code_munge.R`: 원자료 로드 및 기본적인 데이터 정제 
  2. `code_analysis.R`: 분석을 위한 코드 
  3. `code_analysis_testing.R`: 분석을 위한 코드 (테스트 버전) 

### raw_data 

통계청 MDIS에서 제공된 원자료(txt)

### data 

분석에 활용된 가공된 데이터(rds 포맷)

### documentation 

문서화를 위한 디렉토리 

  * 문서화에 필요한 별도의 data 디렉토리가 안에 있다. 

### Shinyapps 

Shiny 구현을 위한 앱 폴더 

  * 세 개의 버전이 있으며 `Shiny_MDS_3`가 최신 버전이며, [웹](http://209.97.160.244:3838/MDIS_Shiny_3/)을 통해서도 구현되어 있다 (이 서비스는 언제 끊어질지 모른다). 
