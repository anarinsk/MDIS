### Initialize code ----
source("./code_header.R")
library(readxl)

### Read source files ----
read_delim("./raw_data/extr_anarinsk_20180605_32528_2017.txt", ",", col_names = F) -> vdf1 
read_delim("./raw_data/extr_anarinsk_20180605_39914_2018.txt", ",", col_names = F) -> vdf2

### Munge data ----
"./raw_data/colnames.xlsx" %.>% 
  read_excel(., sheet = 1, col_names = T) %.>% 
  mutate(., 
    clean_17 = str_replace_all(C_2017, "C...|=|\\'|;", "") %.>% str_squish(.), 
    clean_18 = str_replace_all(C_2018, "C...|=|\\'|;", "") %.>% str_squish(.)
    ) -> vdf 

names(vdf1) <- vdf$common[!is.na(vdf$clean_17)]
names(vdf2) <- vdf$common[!is.na(vdf$clean_18)]

vdf$common[!is.na(vdf$clean_17)]
vdf$common[!is.na(vdf$clean_18)]

vdf2 %.>% 
  mutate(., 
    V107 = NA, 
    V108 = NA, 
    V109 = NA, 
    V110 = NA, 
    V124 = NA, 
    V125 = NA
    ) -> vdf2 

rbind(vdf1, vdf2) -> tdf

saveRDS(tdf, "./data/tbl.rds")
write_csv(tdf, "./data/Khousehold_17_18.csv")

### End of code ----




