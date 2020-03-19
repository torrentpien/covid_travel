library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(xlsx)
library(ggplot2)

country_code <- read_xlsx("data/countries.xlsx")

tw_outbound <- read.csv("data/OutboundVisitor-Total-sheet.csv", stringsAsFactors = FALSE)

tw_outbound <- tw_outbound %>%
  left_join(country_code[, c(1, 4)], by = c("年別" = "中文名稱"))

tw_outbound <- tw_outbound %>%
  left_join(country_code[, c(1, 3)], by = c("Year" = "country"))

tw_outbound$iso3c.x[is.na(tw_outbound$iso3c.x) == TRUE] <- tw_outbound$iso3c.y[is.na(tw_outbound$iso3c.x) == TRUE]

tw_outbound <- tw_outbound%>%
  filter(總計 != "0")
  
colnames(tw_outbound)[7] <- "iso3c"

tw_outbound$iso3c.y <- NULL

tw_outbound$iso3c[tw_outbound$年別 == "中國大陸"] <- "CHN"

tw_outbound$iso3c[tw_outbound$年別 == "馬紹爾"] <- "MHL"

tw_outbound$iso3c[tw_outbound$年別 == "密克羅尼西亞"] <- "FSM"

colnames(tw_outbound) <- c("cont", "country", "country_e", "201901", "201902", "sum", "iso3c")

cn_arrival <- read_xlsx("data/0156012120142018201912.xlsx", skip = 5)

colnames(cn_arrival)[1] <- "country"


cn_arrival <- cn_arrival %>%
  dplyr::select(one_of(c("country", "2017", "2018")))


cn_arrival$rank <- sapply(cn_arrival$country, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

cn_arrival$iso3c <- country_code$iso3c[as.numeric(cn_arrival$rank)]

cn_arrival <- cn_arrival[-1,]

cn_arrival$cap <- case_when(grepl("[A-Z]{2,}", cn_arrival$country) == TRUE ~ 1,
                            TRUE ~ 0)

cn_arrival <- cn_arrival %>%
  filter(cap == 0)

cn_outbound <- read_xlsx("data/0156250119952018202001.xlsx", skip = 5)

colnames(cn_outbound)[1] <- "country"

cn_outbound <- cn_outbound %>%
  dplyr::select(one_of(c("country", "SERIES", "2017", "2018")))

cn_outbound <- cn_outbound[-1,]

cn_outbound_2018 <- cn_outbound[, -3] %>%
  spread(SERIES, `2018`)

cn_outbound_2018$sum <- apply(cn_outbound_2018[,2:9], 1, function(x) max(x, na.rm = TRUE))

cn_outbound_2018$sum[cn_outbound_2018$sum == "-Inf"] <- NA

cn_outbound_2018 <- rename(cn_outbound_2018, sum_2018 = sum)

cn_outbound_2017 <- cn_outbound[, -4] %>%
  spread(SERIES, `2017`)

cn_outbound_2017$sum <- apply(cn_outbound_2017[,2:9], 1, function(x) max(x, na.rm = TRUE))

cn_outbound_2017$sum[cn_outbound_2017$sum == "-Inf"] <- NA

cn_outbound_2017 <- rename(cn_outbound_2017, sum_2017 = sum)

cn_outbound <- cn_outbound_2017[, c(1, 11)] %>%
  left_join(cn_outbound_2018[, c(1, 11)], by = "country")

cn_outbound <- cn_outbound %>%
  filter(is.na(sum_2017) == FALSE | is.na(sum_2018) == FALSE)

cn_outbound$rank <- sapply(cn_outbound$country, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

cn_outbound$iso3c <- country_code$iso3c[as.numeric(cn_outbound$rank)]


wb_arrival <- read.csv("data/API_ST.INT.ARVL_DS2_en_csv_v2_868314.csv", skip = 4)
wb_arrival <- wb_arrival[, c(1, 2, 62, 63)]

wb_gdp <- read.csv("data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_866857.csv", skip = 4)
wb_gdp <- wb_gdp[, c(1, 2, 62, 63)]

wb_dense <- read.csv("data/API_EN.POP.DNST_DS2_en_csv_v2_868885.csv", skip = 4)
wb_dense <- wb_dense[, c(1, 2, 62, 63)]

merged <- wb_arrival %>%
  full_join(cn_outbound, by = c("Country.Code" = "iso3c"))

merged <- merged %>%
  select(one_of("Country.Code", "country"), everything())

