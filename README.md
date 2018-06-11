# MDIS 

## 목적 
2017년 1분기와 2018년 1분기 "가계동향조사"의 소득 항목 비료를 위한 R 코드 제공 

## 유의사항 

  * 디렉토리 의존성에서 벗어나려면 `RStudio`에서 제공하는 project 기능을 이용하라. `MDIS.proj`을 로드하라. 
  
## Directory desc. 

### code 

분석을 위한 코드 

  1. `code_munge.R`: 원자료 로드 및 기본적인 데이터 정제 
  2. `code_analysis.R`: 분석을 위한 코드 

### raw_data 

통계청 MDIS에서 제공된 원자료(txt)

### data 

분석에 활용된 가공된 rds 

### documentation 

문서화

### Shinyapps 

Shiny 구현을 위한 앱 풀더 