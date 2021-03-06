library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(xlsx)
library(ggplot2)
library(ggrepel)
library(Hmisc)
library(stringdist)

options(warn = -1)

#國別碼

country_code <- read_xlsx("data/countries.xlsx")

#####--台灣出境國統計觀光局-----

tw_outbound <- read.csv("data/OutboundVisitor-20180102.csv", stringsAsFactors = FALSE, sep = "\t")

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
tw_outbound$iso3c[tw_outbound$年別 == "波士尼亞"] <- "BIH"
tw_outbound$iso3c[tw_outbound$年別 == "留尼旺"] <- "REU"
tw_outbound$iso3c[tw_outbound$年別 == "馬其頓"] <- "MKD"
tw_outbound$iso3c[tw_outbound$年別 == "千里達及托貝哥"] <- "TTO"
tw_outbound$iso3c[tw_outbound$年別 == "聖克里斯多福"] <- "KNA"
tw_outbound$iso3c[tw_outbound$年別 == "剛果共和國"] <- "COG"

colnames(tw_outbound) <- c("cont", "country", "country_e", "201801", "201802", "sum", "iso3c")

#tw_outbound$hot_travel <- case_when(tw_outbound$cont == "歐洲" ~ 1,
#                                    tw_outbound$country == "以色列" ~ 1,
#                                    tw_outbound$country == "約旦" ~ 1,
#                                    tw_outbound$country == "黎巴嫩" ~ 1,
#                                    tw_outbound$country == "阿拉伯聯合大公國" ~ 1,
#                                    tw_outbound$country == "土耳其" ~ 1,
#                                    tw_outbound$country == "埃及" ~ 1,
#                                    tw_outbound$country == "摩洛哥" ~ 1,
#                                    tw_outbound$country == "阿爾及利亞" ~ 1,
#                                    tw_outbound$country == "突尼西亞	" ~ 1,
#                                    TRUE ~ 0)

#eu_med <- tw_outbound[, c(1, 2, 7, 8)]

#write_xlsx(eu_med, "eu_med.xlsx")

#####----台灣出境國統計 unwto----

tw_outbound_unwto <- read_xlsx("data/0158250119952018202001.xlsx", skip = 5)

colnames(tw_outbound_unwto)[1] <- "country"

tw_outbound_unwto <- tw_outbound_unwto %>%
  dplyr::select(one_of(c("country", "SERIES", "2017", "2018")))

tw_outbound_unwto <- tw_outbound_unwto[-1,]

tw_outbound_unwto_2018 <- tw_outbound_unwto[, -3] %>%
  spread(SERIES, `2018`)

tw_outbound_unwto_2018$sum <- apply(tw_outbound_unwto_2018[,2:9], 1, function(x) max(x, na.rm = TRUE))

tw_outbound_unwto_2018$sum[tw_outbound_unwto_2018$sum == "-Inf"] <- NA

tw_outbound_unwto_2018 <- rename(tw_outbound_unwto_2018, sum_unwto_2018 = sum)

tw_outbound_unwto_2017 <- tw_outbound_unwto[, -4] %>%
  spread(SERIES, `2017`)

tw_outbound_unwto_2017$sum <- apply(tw_outbound_unwto_2017[,2:9], 1, function(x) max(x, na.rm = TRUE))

tw_outbound_unwto_2017$sum[tw_outbound_unwto_2017$sum == "-Inf"] <- NA

tw_outbound_unwto_2017 <- rename(tw_outbound_unwto_2017, sum_unwto_2017 = sum)

tw_outbound_unwto <- tw_outbound_unwto_2017[, c(1, 11)] %>%
  left_join(tw_outbound_unwto_2018[, c(1, 11)], by = "country")

tw_outbound_unwto <- tw_outbound_unwto %>%
  filter(is.na(sum_unwto_2017) == FALSE | is.na(sum_unwto_2018) == FALSE)

tw_outbound_unwto$rank <- sapply(tw_outbound_unwto$country, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

tw_outbound_unwto$iso3c <- country_code$iso3c[as.numeric(tw_outbound_unwto$rank)]

tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Egypt"] <- "EGY"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Tanzania, United Republic of"] <- "TZA"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Eswatini"] <- "SWZ"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Cook Islands"] <- "COK"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Kyrgyzstan"] <- "KGZ"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "North Macedonia"] <- "MKD"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Niue"] <- "NIU"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Lao People's Democratic Republic"] <- "LAO"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Moldova, Republic of"] <- "MDA"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Venezuela, Bolivarian Republic of"] <- "VEN"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Slovakia"] <- "SVK"
tw_outbound_unwto$iso3c[tw_outbound_unwto$country == "Congo"] <- "COG" 

#####----台灣入境旅客國家統計----

tw_inbound <- read.csv("data/出國旅客按目的地統計-2018全年.csv", stringsAsFactors = FALSE, sep = "\t")

colnames(tw_inbound)[3] <- "tw_in_2018"

tw_inbound <- tw_inbound %>%
  left_join(tw_outbound[, c(2, 7)], by = c("國名" = "country"))

tw_inbound_iso3c <- tw_inbound %>%
  filter(is.na(iso3c) == TRUE)

tw_inbound_iso3c$rank <- sapply(tw_inbound_iso3c$英文國名, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

tw_inbound_iso3c$iso3c <- country_code$iso3c[as.numeric(tw_inbound_iso3c$rank)]

tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Unknow"] <- NA
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Saharaui"] <- "ESH"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Yugoslavia"] <- "MKD"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Democratic People's Republic of Korea"] <- "PRK"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Togolese Republic"] <- "TGO"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Yugoslavia"] <- "MKD"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Saint Vincent"] <- "VCT"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Tahiti"] <- NA
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Yemen"] <- "YEM"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Syria"] <- "SYR"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "South Yemen"] <- NA
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Holy See"] <- "VAT"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Timor"] <- "TLS"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Gambia"] <- "GMB"
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "German Democratic"] <- NA
tw_inbound_iso3c$iso3c[tw_inbound_iso3c$英文國名 == "Yugoslavia"] <- NA


tw_inbound$iso3c[is.na(tw_inbound$iso3c) == TRUE] <- tw_inbound_iso3c$iso3c

colnames(tw_inbound)[1:2] <- c("country", "country_e")

#####----各國赴中國遊客統計----

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

cn_arrival$iso3c[cn_arrival$country == "Anguilla"] <- "AIA"
cn_arrival$iso3c[cn_arrival$country == "Christmas Island, Australia"] <- "CXR"
cn_arrival$iso3c[cn_arrival$country == "Egypt"] <- "EGY"
cn_arrival$iso3c[cn_arrival$country == "Tanzania, United Republic of"] <- "TZA"
cn_arrival$iso3c[cn_arrival$country == "State of Palestine"] <- "PSE"
cn_arrival$iso3c[cn_arrival$country == "Eswatini"] <- "SWZ"
cn_arrival$iso3c[cn_arrival$country == "Cook Islands"] <- "COK"
cn_arrival$iso3c[cn_arrival$country == "Kyrgyzstan"] <- "KGZ"
cn_arrival$iso3c[cn_arrival$country == "Other countries of the World"] <- NA
cn_arrival$iso3c[cn_arrival$country == "North Macedonia"] <- "MKD"
cn_arrival$iso3c[cn_arrival$country == "Netherlands Antilles"] <- "ANT"
cn_arrival$iso3c[cn_arrival$country == "Niue"] <- "NIU"
cn_arrival$iso3c[cn_arrival$country == "Lao People's Democratic Republic"] <- "LAO"
cn_arrival$iso3c[cn_arrival$country == "Moldova, Republic of"] <- "MDA"
cn_arrival$iso3c[cn_arrival$country == "Yemen"] <- "YEM"
cn_arrival$iso3c[cn_arrival$country == "Venezuela, Bolivarian Republic of"] <- "VEN"
cn_arrival$iso3c[cn_arrival$country == "Slovakia"] <- "SVK"
cn_arrival$iso3c[cn_arrival$country == "Congo"] <- "COG"
cn_arrival$iso3c[cn_arrival$country == "Gambia"] <- "GMB"

cn_arrival <- rename(cn_arrival, "cn_arr_2017" = "2017")
cn_arrival <- rename(cn_arrival, "cn_arr_2018" = "2018")


#####----中國遊客赴各國統計----

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

cn_outbound$iso3c[cn_outbound$country == "Egypt"] <- "EGY"
cn_outbound$iso3c[cn_outbound$country == "Tanzania, United Republic of"] <- "TZA"
cn_outbound$iso3c[cn_outbound$country == "Eswatini"] <- "SWZ"
cn_outbound$iso3c[cn_outbound$country == "Cook Islands"] <- "COK"
cn_outbound$iso3c[cn_outbound$country == "Kyrgyzstan"] <- "KGZ"
cn_outbound$iso3c[cn_outbound$country == "North Macedonia"] <- "MKD"
cn_outbound$iso3c[cn_outbound$country == "Niue"] <- "NIU"
cn_outbound$iso3c[cn_outbound$country == "Lao People's Democratic Republic"] <- "LAO"
cn_outbound$iso3c[cn_outbound$country == "Moldova, Republic of"] <- "MDA"
cn_outbound$iso3c[cn_outbound$country == "Venezuela, Bolivarian Republic of"] <- "VEN"
cn_outbound$iso3c[cn_outbound$country == "Slovakia"] <- "SVK"
cn_outbound$iso3c[cn_outbound$country == "Congo"] <- "COG"


#----航空旅客資料----

air_psg <- read.csv("data/Air travel_201802-04.csv", stringsAsFactors = FALSE)

colnames(air_psg) <- c("country", "volume", "risk")

air_psg$rank <- sapply(air_psg$country, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

air_psg$iso3c <- country_code$iso3c[as.numeric(air_psg$rank)]

air_psg$iso3c[air_psg$country == "Russia"] <- "RUS"
air_psg$iso3c[air_psg$country == "Reunion"] <- "REU"
air_psg$iso3c[air_psg$country == "Congo (Brazzaville)"] <- "COG"
air_psg$iso3c[air_psg$country == "Bonaire, Saint Eustatius and Saba"] <- "BES"
air_psg$iso3c[air_psg$country == "Turks-Caicos"] <- "TCA"
air_psg$iso3c[air_psg$country == "Egypt"] <- "EGY"
air_psg$iso3c[air_psg$country == "Cook Islands"] <- "COK"
air_psg$iso3c[air_psg$country == "Laos"] <- "LAO"
air_psg$iso3c[air_psg$country == "Guadeloupe"] <- "GLP"
air_psg$iso3c[air_psg$country == "Micronesia"] <- "FSM"
air_psg$iso3c[air_psg$country == "Iran"] <- "IRN"
air_psg$iso3c[air_psg$country == "Kyrgyzstan"] <- "KGZ"
air_psg$iso3c[air_psg$country == "Korea (South)"] <- "KOR"
air_psg$iso3c[air_psg$country == "Korea (North)"] <- "PRK"
air_psg$iso3c[air_psg$country == "Svalbard"] <- "SJM"
air_psg$iso3c[air_psg$country == "Congo (Kinshasa)"] <- "COD"
air_psg$iso3c[air_psg$country == "Martinique"] <- "MTQ"
air_psg$iso3c[air_psg$country == "Jersey"] <- "JEY"
air_psg$iso3c[air_psg$country == "Syria"] <- "SYR"
air_psg$iso3c[air_psg$country == "Slovakia"] <- "SVK"
air_psg$iso3c[air_psg$country == "Swaziland"] <- "SWZ"
air_psg$iso3c[air_psg$country == "Taiwan"] <- "TWN"
air_psg$iso3c[air_psg$country == "Virgin Islands (GB)"] <- "VGB"
air_psg$iso3c[air_psg$country == "Gambia"] <- "GMB"

#----epirisk----

epi_risk <- read.csv("data/epirisk-CHN_January.csv", stringsAsFactors = FALSE)

epi_risk$rank <- sapply(epi_risk$label, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

epi_risk$iso3c <- country_code$iso3c[as.numeric(epi_risk$rank)]

iso3c_check <- wb_arrival %>%
  full_join(epi_risk[, c(4, 8)], by = c("Country.Code" = "iso3c"))

iso3c_check <- iso3c_check %>%
  select(one_of("Country.Code", "label"), everything())

epi_risk$iso3c[epi_risk$label == "Iran"] <- "IRN"
epi_risk$iso3c[epi_risk$label == "Kyrgyzstan"] <- "KGZ"
epi_risk$iso3c[epi_risk$label == "Korea, Dem. Rep."] <- "PRK"
epi_risk$iso3c[epi_risk$label == "Taiwan"] <- "TWN"

epi_risk <- rename(epi_risk, epi_risk = risk)

#----世界銀行各國旅客統計----

wb_arrival <- read.csv("data/API_ST.INT.ARVL_DS2_en_csv_v2_868314.csv", skip = 4)
wb_arrival <- wb_arrival[, c(1, 2, 62, 63)]

wb_arrival$X2018[is.na(wb_arrival$X2018) == TRUE] <- wb_arrival$X2017[is.na(wb_arrival$X2018) == TRUE]

wb_arrival <- rename(wb_arrival, "tour_arr_2017" = "X2017")
wb_arrival <- rename(wb_arrival, "tour_arr_2018" = "X2018")


#----世界銀行GDP----

wb_gdp <- read.csv("data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_866857.csv", skip = 4)
wb_gdp <- wb_gdp[, c(1, 2, 62, 63)]

wb_gdp$X2018[is.na(wb_gdp$X2018) == TRUE] <- wb_gdp$X2017[is.na(wb_gdp$X2018) == TRUE]

wb_gdp <- rename(wb_gdp, "gdp_2017" = "X2017")
wb_gdp <- rename(wb_gdp, "gdp_2018" = "X2018")

#----世界銀行人口密度----

wb_dense <- read.csv("data/API_EN.POP.DNST_DS2_en_csv_v2_868885.csv", skip = 4)
wb_dense <- wb_dense[, c(1, 2, 62, 63)]

wb_dense$X2018[is.na(wb_dense$X2018) == TRUE] <- wb_dense$X2017[is.na(wb_dense$X2018) == TRUE]

wb_dense <- rename(wb_dense, "dense_2017" = "X2017")
wb_dense <- rename(wb_dense, "dense_2018" = "X2018")

#----John Hopkins COVID19 data----

COVID_case <- read.csv("data/time_series_covid19_confirmed_global_20200324.csv", stringsAsFactors = FALSE)

JP_country <- data.frame(country = unique(COVID_case$Country.Region))

JP_country$rank <- sapply(JP_country$country, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

JP_country$iso3c <- country_code$iso3c[as.numeric(JP_country$rank)]

JP_country$iso3c[JP_country$country == "Russia"] <- "RUS"
JP_country$iso3c[JP_country$country == "Brunei"] <- "BRN"
JP_country$iso3c[JP_country$country == "Congo (Brazzaville)"] <- "COG"
JP_country$iso3c[JP_country$country == "Czechia"] <- "CZE"
JP_country$iso3c[JP_country$country == "Egypt"] <- "EGY"
JP_country$iso3c[JP_country$country == "Diamond Princess"] <- NA
JP_country$iso3c[JP_country$country == "Cruise Ship"] <- NA
JP_country$iso3c[JP_country$country == "US"] <- "USA"
JP_country$iso3c[JP_country$country == "Eswatini"] <- "SWZ"
JP_country$iso3c[JP_country$country == "Iran"] <- "IRN"
JP_country$iso3c[JP_country$country == "Laos"] <- "LAO"
JP_country$iso3c[JP_country$country == "Kyrgyzstan"] <- "KGZ"
JP_country$iso3c[JP_country$country == "Congo (Kinshasa)"] <- "COD"
JP_country$iso3c[JP_country$country == "Martinique"] <- "MTQ"
JP_country$iso3c[JP_country$country == "New Caledonia"] <- "NCL"
JP_country$iso3c[JP_country$country == "Syria"] <- "SYR"
JP_country$iso3c[JP_country$country == "Slovakia"] <- "SVK"
JP_country$iso3c[JP_country$country == "Holy See"] <- "VAT"
JP_country$iso3c[JP_country$country == "Taiwan*"] <- "TWN"
JP_country$iso3c[JP_country$country == "Gambia"] <- "GMB"

COVID_case <- COVID_case %>%
  left_join(JP_country, by = c("Country.Region" = "country"))


COVID_sum <- COVID_case %>%
  group_by(iso3c) %>%
  summarise(`20200122` = sum(X1.22.20),
            `20200123` = sum(X1.23.20),
            `20200124` = sum(X1.24.20),
            `20200126` = sum(X1.26.20),
            `20200202` = sum(X2.2.20),
            `20200205` = sum(X2.5.20),
            `20200206` = sum(X2.6.20),
            `20200207` = sum(X2.7.20),
            `20200210` = sum(X2.10.20),
            `20200217` = sum(X2.17.20),
            `20200219` = sum(X2.19.20),
            `20200221` = sum(X2.21.20),
            `20200304` = sum(X3.4.20),
            `20200307` = sum(X3.7.20),
            `20200318` = sum(X3.18.20),
            `20200324` = sum(X3.24.20))

#----端傳媒紀錄確診案例----

covid_initium <- read_xlsx("data/武漢肺炎_20200331.xlsx", sheet = "new sheet")
#covid_initium <- rename(covid_initium, confirmed_20200329 = `...184`)

covid_initium <- covid_initium %>%
  dplyr::select(1:5, contains("confirmed"))

covid_initium[41, 6:97] <- colSums(covid_initium[1:31, 6:97])

covid_initium <- covid_initium %>%
  filter(type != "china") %>%
  filter(is.na(iso3c) == FALSE)

covid_initium <- covid_initium %>%
  dplyr::select(1:5, contains(c("20200111", "20200122", "20200123", "20200124", "20200126",
                              "20200202", "20200205", "20200206", "20200207", "20200210",
                              "20200211", "20200217", "20200219", "20200221", "20200223",
                              "20200225", "20200227",
                              "20200304", "20200307", "20200310", "20200311", "20200312",
                              "20200314", "20200316", "20200318", "20200320", "20200324", 
                              "20200325", "20200331")))

#write_xlsx(covid_initium, "data/initium.xlsx")


#----台灣境外移入來源----

imported <- read_xlsx("data/境外移入案例來源_20200331.xlsx", sheet = "全台武肺個案+國碼")

colnames(imported)[3] <- "domestic"

imported <- imported %>%
  filter(domestic == "境外")

imported <- imported %>%
  dplyr::select(-contains("all"))

imported[is.na(imported$gene) == FALSE, 4] <- imported$gene[is.na(imported$gene) == FALSE]
imported[is.na(imported$gene) == FALSE, 5:10] <- NA

imported <- imported %>%
  gather(key = "in", value = country, 4:10)

imported <- imported %>%
  filter(is.na(country) == FALSE)

imported <- imported %>%
  group_by(公佈日期, country) %>%
  summarise(count = n())

imported <- imported %>%
  spread(公佈日期, count)

imported_accum <- imported %>%
  mutate(im_20200111 = 0,
         im_20200122 = rowSums(imported[, 2], na.rm = TRUE),
         im_20200123 = rowSums(imported[, 2], na.rm = TRUE),
         im_20200124 = rowSums(imported[, 2:3], na.rm = TRUE),
         im_20200126 = rowSums(imported[, 2:4], na.rm = TRUE),
         im_20200202 = rowSums(imported[, 2:7], na.rm = TRUE),
         im_20200205 = rowSums(imported[, 2:8], na.rm = TRUE),
         im_20200206 = rowSums(imported[, 2:9], na.rm = TRUE),
         im_20200207 = rowSums(imported[, 2:9], na.rm = TRUE),
         im_20200210 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200211 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200217 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200219 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200221 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200223 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200225 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200227 = rowSums(imported[, 2:11], na.rm = TRUE),
         im_20200304 = rowSums(imported[, 2:13], na.rm = TRUE),
         im_20200307 = rowSums(imported[, 2:14], na.rm = TRUE),
         im_20200310 = rowSums(imported[, 2:15], na.rm = TRUE),
         im_20200311 = rowSums(imported[, 2:16], na.rm = TRUE),
         im_20200312 = rowSums(imported[, 2:17], na.rm = TRUE),
         im_20200314 = rowSums(imported[, 2:18], na.rm = TRUE),
         im_20200316 = rowSums(imported[, 2:20], na.rm = TRUE),
         im_20200318 = rowSums(imported[, 2:22], na.rm = TRUE),
         im_20200320 = rowSums(imported[, 2:24], na.rm = TRUE),
         im_20200324 = rowSums(imported[, 2:28], na.rm = TRUE),
         im_20200325 = rowSums(imported[, 2:29], na.rm = TRUE),
         im_20200331 = rowSums(imported[, 2:35], na.rm = TRUE))

imported_accum <- imported_accum %>%
  dplyr::select(1, contains("im"))


#----trade data----

trade_data <- read.table('data/year_origin_destination_hs07_4.tsv',
                  sep = '\t', header = TRUE, quote="")

country_names = read.table('data/country_names.tsv',
                           sep = '\t', header = TRUE)

product_names = read.table('data/products_hs_07.tsv',
                           sep = '\t', header = TRUE)

trade_data <- subset(trade_data, origin == 'chn' & year == 2017)


trade_data[,'export_val'] <- as.numeric(as.character(trade_data[,'export_val']))
trade_data[,'import_val'] <- as.numeric(as.character(trade_data[,'import_val']))
trade_data[is.na(trade_data)] <- 0

china_trade <- trade_data %>%
  group_by(dest) %>%
  summarise(export = sum(export_val),
            import = sum(import_val))


china_trade$dest <- toupper(china_trade$dest)

#----歐洲地中海國家----

eu_med_list <- read_xlsx("data/境外移入案例來源_20200330.xlsx", sheet = "歐洲、地中海國家標記") %>%
  filter(mark == 1)


#----merge data----

merged <- wb_arrival %>%
  full_join(cn_outbound[, c(5, 2, 3)], by = c("Country.Code" = "iso3c")) %>%
  full_join(cn_arrival[, c(5, 2, 3)], by = c("Country.Code" = "iso3c")) %>%
  full_join(tw_outbound[, c(7, 4, 5, 6)], by = c("Country.Code" = "iso3c")) %>%
  full_join(tw_outbound_unwto[, c(5, 2, 3)], by = c("Country.Code" = "iso3c")) %>%
  full_join(tw_inbound[, c(4, 3)], by = c("Country.Code" = "iso3c")) %>%
  full_join(wb_gdp[, c(2, 3, 4)], by = "Country.Code") %>%
  full_join(wb_dense[, c(2, 3, 4)], by = "Country.Code") %>%
  full_join(china_trade, by = c("Country.Code" = "dest")) %>%
  full_join(covid_initium[, c(1, 4:34)], by = c("Country.Code" = "iso3c")) %>%
  full_join(imported_accum, by = c("Country.Code" = "country")) %>%
  full_join(eu_med_list[, c(3:4)], by = c("Country.Code" = "iso3c")) %>%
  full_join(air_psg[, c(2, 3, 5)], by = c("Country.Code" = "iso3c")) %>%
  full_join(epi_risk[, c(8, 6)], by = c("Country.Code" = "iso3c"))

merged <- merged %>%
  dplyr::select(one_of("Country.Code", "name", "name_zh"), everything())

#colnames(merged)[80]

merged[, 23:80][is.na(merged[, 23:80])] <- 0

merged$name_zh[merged$name == "France"] <- "法國"

#write.csv(merged, "data/merged_20200331.csv", row.names = FALSE)

#merged <- read.csv("data/merged_20200328.csv", stringsAsFactors = FALSE)

#cn_aireffect <- merged %>%
#  dplyr::select(1:3, 
#                one_of("hot_travel", "sum_2018", "dense_2018", "cn_arr_2018", "volume", "risk", "epi_risk"), 
#                contains("confirmed"))

merged <- merged %>%
  mutate(tour_popu_2018 = sum_2018 + cn_arr_2018,
         cn_tour_dense = tour_popu_2018 * dense_2018,
         cn_psg_dense = volume * dense_2018)

#加美國

merged$mark[merged$Country.Code == "USA"] <- "1"

eu_med <- merged %>%
  filter(mark == 1)

crucial_date <- colnames(eu_med)[23:51]

#中國航空旅客密集度與確診人數

cn_aireffect <- eu_med %>%
  dplyr::select(one_of("Country.Code", "name_zh", "name", "cn_psg_dense"))

cn_aireffect$ln_cnpsg_dense <- log(cn_aireffect$cn_psg_dense + 1)

for (i in 1:29) {
  
  df_plot <- data.frame(x = log(eu_med$cn_psg_dense + 1), y = log(eu_med[, crucial_date[i]] + 1), name = eu_med$name_zh, iso3c = eu_med$Country.Code)
  
  date <- paste0(substr(crucial_date[i], 16, 16), "月", substr(crucial_date[i], 17, 18), "日")
  
  fit <- lm(y ~ x, df_plot)
  
  cd_plot <- ggplot(df_plot, aes(x = x, y = y, label = name)) + 
    geom_text_repel(family = "Heiti TC Medium") + 
    theme_minimal() +
    geom_point() +
    ggtitle("歐洲、地中海週邊旅遊區\n中國航空旅客密集度與確診人數關係") + 
    ylab(paste0("Log(", date, "各國確診人數)")) +
    xlab('Log(中國航空旅客密集度)') +
    theme(text = element_text(family = "Heiti TC Medium")) +
    labs(title = paste("歐洲、地中海週邊旅遊區\n中國航空旅客密集度與確診人數關係\n", 
      "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
    stat_smooth(method = "lm")
  
  ggsave(paste0("plot_psg/", crucial_date[i], ".png"), plot = cd_plot, 
         width = 8, height = 3.2, dpi = 150)
  
  colnames(df_plot)[1:2] <- c("ln_cnpsg_dense", paste0("ln_", crucial_date[i]))
  
  cn_aireffect <- cn_aireffect %>%
    left_join(df_plot[, c(4, 2)], by = c("Country.Code" = "iso3c"))
  
}

write_xlsx(cn_aireffect, "result/cn_aireffect.xlsx")

#台灣外國旅遊暴露度與境外確診相關


tw_tour_expose <- eu_med %>%
  dplyr::select(one_of("Country.Code", "name_zh", "name", "sum", "cn_psg_dense"))

tw_tour_expose$ln_twexpose <- log(tw_tour_expose$sum * (tw_tour_expose$cn_psg_dense + 1))


crucial_date <- colnames(eu_med)[23:51]
tw_confirmed_date <- colnames(eu_med)[52:80]

for (i in 1:29) {
  
  df_plot <- data.frame(x = log(eu_med$sum * (eu_med$cn_psg_dense + 1)), y = log(eu_med[, tw_confirmed_date[i]] + 1), name = eu_med$name_zh, iso3c = eu_med$Country.Code)
  
  date <- paste0(substr(crucial_date[i], 16, 16), "月", substr(crucial_date[i], 17, 18), "日")
  
  fit <- lm(y ~ x, df_plot)
  
  cd_plot <- ggplot(df_plot, aes(x = x, y = y, label = name)) + 
    geom_text_repel(family = "Heiti TC Medium") + 
    theme_minimal() +
    geom_point() +
    ggtitle("歐洲、地中海週邊旅遊區\n台灣旅客暴露度與確診人數關係") + 
    ylab(paste0("Log(", date, "台灣境外確診人數)")) +
    xlab('Log(台灣旅客暴露度)') +
    theme(text = element_text(family = "Heiti TC Medium")) +
    labs(title = paste("歐洲、地中海週邊旅遊區\n台灣旅客暴露度與確診人數關係\n", 
                       "Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
    stat_smooth(method = "lm")
  
  ggsave(paste0("plot_twexpose/", crucial_date[i], ".png"), plot = cd_plot, 
         width = 6.4, height = 4.8, dpi = 150)
  
  colnames(df_plot)[1:2] <- c("ln_twexpose", paste0("ln_", tw_confirmed_date[i]))
  
  tw_tour_expose <- tw_tour_expose %>%
    left_join(df_plot[, c(4, 2)], by = c("Country.Code" = "iso3c"))
  
  
}

write_xlsx(tw_tour_expose, "result/tw_tour_expose.xlsx")

#各國風險度與台灣境外確診相關

tw_tour_risk <- eu_med %>%
  dplyr::select(one_of("Country.Code", "name_zh", "name", "sum", "risk"))

tw_tour_risk$ln_tw_risk <- log(tw_tour_risk$sum * (tw_tour_risk$risk + 1))

crucial_date <- colnames(eu_med)[23:51]
tw_confirmed_date <- colnames(eu_med)[52:80]

for (i in 1:29) {
  
  df_plot <- data.frame(x = log(eu_med$sum * (eu_med$risk + 1)), y = log(eu_med[, tw_confirmed_date[i]] + 1), name = eu_med$name_zh, iso3c = eu_med$Country.Code)
  
  date <- paste0(substr(crucial_date[i], 16, 16), "月", substr(crucial_date[i], 17, 18), "日")
  
  fit <- lm(y ~ x, df_plot)
  
  cd_plot <- ggplot(df_plot, aes(x = x, y = y, label = name)) + 
    geom_text_repel(family = "Heiti TC Medium") + 
    theme_minimal() +
    geom_point() +
    ggtitle("歐洲、地中海週邊旅遊區\n台灣旅客風險度與確診人數關係") + 
    ylab(paste0("Log(", date, "台灣境外確診人數)")) +
    xlab('Log(台灣旅客風險度)') +
    theme(text = element_text(family = "Heiti TC Medium")) +
    labs(title = paste("歐洲、地中海週邊旅遊區\n台灣旅客風險度與確診人數關係\n", 
                       "Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
    stat_smooth(method = "lm")
  
  ggsave(paste0("plot_risk/", crucial_date[i], ".png"), plot = cd_plot, 
         width = 6.4, height = 4.8, dpi = 150)
  
  colnames(df_plot)[1:2] <- c("ln_twrisk", paste0("ln_", tw_confirmed_date[i]))
  
  tw_tour_risk <- tw_tour_risk %>%
    left_join(df_plot[, c(4, 2)], by = c("Country.Code" = "iso3c"))
  
  
}

write_xlsx(tw_tour_risk, "result/tw_tour_risk.xlsx")


#各國epi風險度與台灣境外確診相關

tw_tour_epirisk <- eu_med %>%
  dplyr::select(one_of("Country.Code", "name_zh", "name", "sum", "epi_risk"))

tw_tour_epirisk$ln_tw_epirisk <- log(tw_tour_epirisk$sum * (tw_tour_epirisk$epi_risk + 1))

crucial_date <- colnames(eu_med)[23:51]
tw_confirmed_date <- colnames(eu_med)[52:80]

for (i in 1:29) {
  
  df_plot <- data.frame(x = log(eu_med$sum * (eu_med$epi_risk + 1)), y = log(eu_med[, tw_confirmed_date[i]] + 1), name = eu_med$name_zh, iso3c = eu_med$Country.Code)
  
  date <- paste0(substr(crucial_date[i], 16, 16), "月", substr(crucial_date[i], 17, 18), "日")
  
  fit <- lm(y ~ x, df_plot)
  
  cd_plot <- ggplot(df_plot, aes(x = x, y = y, label = name)) + 
    geom_text_repel(family = "Heiti TC Medium") + 
    theme_minimal() +
    geom_point() +
    ggtitle("歐洲、地中海週邊旅遊區\n台灣旅客epi風險度與確診人數關係") + 
    ylab(paste0("Log(", date, "台灣境外確診人數)")) +
    xlab('Log(台灣旅客epi風險度)') +
    theme(text = element_text(family = "Heiti TC Medium")) +
    labs(title = paste("歐洲、地中海週邊旅遊區\n台灣旅客epi風險度與確診人數關係\n", 
                       "Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
    stat_smooth(method = "lm")
  
  ggsave(paste0("plot_epirisk/", crucial_date[i], ".png"), plot = cd_plot, 
         width = 6.4, height = 4.8, dpi = 150)
  
  colnames(df_plot)[1:2] <- c("ln_tw_epirisk", paste0("ln_", tw_confirmed_date[i]))
  
  tw_tour_epirisk <- tw_tour_epirisk %>%
    left_join(df_plot[, c(4, 2)], by = c("Country.Code" = "iso3c"))
  
}

write_xlsx(tw_tour_epirisk, "result/tw_tour_epirisk.xlsx")

#各國中國旅客與確診數map plot

cn_air_map <- eu_med %>%
  dplyr::select(one_of("Country.Code", "name_zh", "name", "cn_psg_dense", "volume"), contains("confirmed"))

write_xlsx(cn_air_map, "result/cn_air_map.xlsx")

#各國中國旅客密集度與確診數bar plot

cn_air_df <- cn_aireffect %>%
  dplyr::select(one_of("Country.Code", "name_zh", "name", "ln_cnpsg_dense", "ln_confirmed_20200325")) %>%
  arrange(desc(ln_cnpsg_dense))

cn_air_df$group <- factor(rep(1:7, each = 10, length.out = nrow(cn_air_df)))

cn_air_df$order <- factor(cn_air_df$name_zh, levels = cn_air_df$name_zh)

cn_air_df <- cn_air_df %>%
  filter(is.na(ln_cnpsg_dense) == FALSE,
         ln_cnpsg_dense != 0)

#cn_air_df <- cn_air_df %>% 
#  gather(cate, data, 4:5)

ggplot(cn_air_df) +
  geom_bar(aes(x = order, y = ln_cnpsg_dense), position = "dodge", stat="identity", width=.5, colour = "blue") +
  geom_point(aes(x = order, y = ln_confirmed_20200325)) +
  geom_line(aes(x = order, y = ln_confirmed_20200325)) +
  #geom_text(aes(label=data, group=cate), position=position_dodge(width=0.5), vjust=-0.5) +
  facet_wrap(~group, scales="free_y", ncol = 1)+
  theme_bw(base_size = 10) +
  ylab('log(中國旅客密集度)/log(各國確診人數)') +
  xlab("國家")



#plot end




merged <- merged %>%
  mutate(tour_popu_2017 = sum_2017 + cn_arr_2017,
         tour_popu_2018 = sum_2018 + cn_arr_2018,
         cn_tour_dense = tour_popu_2018 * dense_2018,
         cn_psg_dense = volume * dense_2018,
         tw_exposed = sum * cn_tour_dense * X20200318)

travel_leak <- merged %>%
  filter(hot_travel == 1)




#write.csv(merged, "data/all_merged.csv")
#write.csv(travel_leak, "data/travel_eu_me.csv")


#plot

crucial_date <- colnames(travel_leak)[19:34]

for (i in 1:16) {
  
  df_plot <- data.frame(x = log(travel_leak$cn_psg_dense), y = log(travel_leak[, crucial_date[i]] + 1), name = travel_leak$Country.Name)
  
  date <- paste0(substr(crucial_date[i], 7, 7), "月", substr(crucial_date[i], 8, 9), "日")
  
  cd_plot <- ggplot(df_plot, aes(x = x, y = y, label = name)) + 
    geom_text_repel() + 
    theme_minimal() +
    geom_point() +
    ggtitle("歐洲、地中海週邊旅遊區\n中國航空旅客密集度與確診人數關係") + 
    ylab(paste0("Log(", date, "各國確診人數)")) +
    xlab('Log(中國航空旅客密集度)') +
    theme(text = element_text(family = "Heiti TC Medium"))
  
  ggsave(paste0("plot_psg/", crucial_date[i], ".png"), plot = cd_plot, width = 640, height = 480)
  
}

ggplot(travel_leak, aes(x = log(cn_psg_dense), y = log(X20200324), label = Country.Name)) + 
  geom_text_repel() + 
  theme_minimal()+
  geom_point()+
  ggtitle("歐洲、地中海週邊旅遊區\n中國航空旅客密集度與確診人數關係") + 
  ylab('Log(3月24日各國確診人數)')+
  xlab('Log(中國航空旅客密集度)') +
  theme(text = element_text(family = "Heiti TC Medium")) +
  stat_smooth(method = "lm")


ggplot(travel_leak, aes(x = log(cn_tour_dense), y = log(X20200318), label = Country.Name)) + 
  geom_text_repel() + 
  theme_minimal()+
  geom_point()+
  ggtitle("歐洲、地中海週邊旅遊區\n中國航空旅客密集度與確診人數關係") + 
  ylab('Log(3月18日各國確診人數)')+
  xlab('Log(中國航空旅客密集度)') +
  theme(text = element_text(family = "Heiti TC Medium")) 
        

ggplot(travel_leak, aes(x = log(risk), y = log(import_sum), label = Country.Name)) + 
  geom_text_repel() + 
  theme_minimal()+
  geom_point()+
  ggtitle("歐洲、地中海週邊旅遊\n台灣旅客暴露度與確診人數關係") + 
  ylab('Log(台灣境外確診人數)') +
  xlab('Log(台灣旅客暴露度)') +
  theme(text = element_text(family = "Heiti TC Medium")) +
  stat_smooth(method = "lm")

ggplot(travel_leak, aes(x = log(X20200318), y = log(imported_sum), label = Country.Name)) + 
  geom_text_repel() + 
  theme_minimal()+
  geom_point()+
  ggtitle("歐洲、地中海週邊旅遊\n台灣境外旅遊人數與3月18日確診人數關係") + 
  ylab('Log(3月18日台灣境外確診人數)') +
  xlab('Log(台灣境外旅遊人數)') +
  theme(text = element_text(family = "Heiti TC Medium"))


#端傳媒iso3c編碼處理

covid_initium <- read_xlsx("data/武漢肺炎.xlsx", sheet = "new sheet")

covid_initium <- covid_initium[, 3:5]

covid_initium_country <- data.frame(country = covid_initium$name[covid_initium$type != "china"])

covid_initium_country <- covid_initium_country %>%
  filter(is.na(country) == FALSE)

covid_initium_country$rank <- sapply(covid_initium_country$country, function(x) {
  which.min(stringdist::stringdist(x, country_code$country, method = 'lcs', useBytes = TRUE))
})

covid_initium_country$iso3c <- country_code$iso3c[as.numeric(covid_initium_country$rank)]

covid_initium_country$iso3c[covid_initium_country$country == "Ashmore and Cartier Is."] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Russia"] <- "RUS"
covid_initium_country$iso3c[covid_initium_country$country == "Akrotiri"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Anguilla"] <- "AIA"
covid_initium_country$iso3c[covid_initium_country$country == "Antarctica"] <- "ATA"
covid_initium_country$iso3c[covid_initium_country$country == "Brunei"] <- "BRN"
covid_initium_country$iso3c[covid_initium_country$country == "Bajo Nuevo Bank"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Baikonur"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "W. Sahara"] <- "ESH"
covid_initium_country$iso3c[covid_initium_country$country == "Siachen Glacier"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Heard I. and McDonald Is."] <- "HMD"
covid_initium_country$iso3c[covid_initium_country$country == "Coral Sea Is."] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Dhekelia"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Czechia"] <- "CZE"
covid_initium_country$iso3c[covid_initium_country$country == "Clipperton I."] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Cook Is."] <- "COK"
covid_initium_country$iso3c[covid_initium_country$country == "Scarborough Reef"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Egypt"] <- "EGY"
covid_initium_country$iso3c[covid_initium_country$country == "N. Cyprus"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Cyprus U.N. Buffer Zone"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Guernsey"] <- "GGY"
covid_initium_country$iso3c[covid_initium_country$country == "U.S. Minor Outlying Is."] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Dominican Rep."] <- "DOM"
covid_initium_country$iso3c[covid_initium_country$country == "North Korea"] <- "PRK"
covid_initium_country$iso3c[covid_initium_country$country == "St-Martin"] <- "MAF"
covid_initium_country$iso3c[covid_initium_country$country == "eSwatini"] <- "SWZ"
covid_initium_country$iso3c[covid_initium_country$country == "U.S. Virgin Is."] <- "VIR"
covid_initium_country$iso3c[covid_initium_country$country == "Saint Helena"] <- "SHN"
covid_initium_country$iso3c[covid_initium_country$country == "Spratly Is."] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "St. Vin. and Gren."] <- "VCT"
covid_initium_country$iso3c[covid_initium_country$country == "Falkland Is."] <- "FLK"
covid_initium_country$iso3c[covid_initium_country$country == "Norfolk Island"] <- "NFK"
covid_initium_country$iso3c[covid_initium_country$country == "Laos"] <- "LAO"
covid_initium_country$iso3c[covid_initium_country$country == "Eq. Guinea"] <- "GNQ"
covid_initium_country$iso3c[covid_initium_country$country == "Micronesia"] <- "FSM"
covid_initium_country$iso3c[covid_initium_country$country == "Indian Ocean Ter."] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Br. Indian Ocean Ter."] <- "IOT"
covid_initium_country$iso3c[covid_initium_country$country == "Iran"] <- "IRN"
covid_initium_country$iso3c[covid_initium_country$country == "St-Barthélemy"] <- "BLM"
covid_initium_country$iso3c[covid_initium_country$country == "Kyrgyzstan"] <- "KGZ"
covid_initium_country$iso3c[covid_initium_country$country == "Fr. S. Antarctic Lands"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Serranilla Bank"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Vatican"] <- "VAT"
covid_initium_country$iso3c[covid_initium_country$country == "Montserrat"] <- "MSR"
covid_initium_country$iso3c[covid_initium_country$country == "Macao"] <- "MAC"
covid_initium_country$iso3c[covid_initium_country$country == "Palestine"] <- "PSE"
covid_initium_country$iso3c[covid_initium_country$country == "Wallis and Futuna Is."] <- "WLF"
covid_initium_country$iso3c[covid_initium_counry$country == "N. Mariana Is."] <- "MNP"
covid_initium_country$iso3c[covid_initium_country$country == "Niue"] <- "NIU"
covid_initium_country$iso3c[covid_initium_country$country == "Yemen"] <- "YEM"
covid_initium_country$iso3c[covid_initium_country$country == "Pitcairn Is."] <- "PCN"
covid_initium_country$iso3c[covid_initium_country$country == "Jersey"] <- "JEY"
covid_initium_country$iso3c[covid_initium_country$country == "Åland"] <- "ALA"
covid_initium_country$iso3c[covid_initium_country$country == "S. Geo. and the Is."] <- "SGS"
covid_initium_country$iso3c[covid_initium_country$country == "S. Sudan"] <- "SSD"
covid_initium_country$iso3c[covid_initium_country$country == "St. Pierre and Miquelon"] <- "SPM"
covid_initium_country$iso3c[covid_initium_country$country == "Sint Maarten"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Somaliland"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Syria"] <- "SYR"
covid_initium_country$iso3c[covid_initium_country$country == "Slovakia"] <- "SVK"
covid_initium_country$iso3c[covid_initium_country$country == "Congo"] <- "COG"
covid_initium_country$iso3c[covid_initium_country$country == "Hong Kong"] <- "HKG"
covid_initium_country$iso3c[covid_initium_country$country == "Taiwan"] <- "TWN"
covid_initium_country$iso3c[covid_initium_country$country == "USNB Guantanamo Bay"] <- NA
covid_initium_country$iso3c[covid_initium_country$country == "Dem. Rep. Congo"] <- "COD"
covid_initium_country$iso3c[covid_initium_country$country == "South Korea"] <- "KOR"
covid_initium_country$iso3c[covid_initium_country$country == "Gambia"] <- "GMB"

covid_initium <- covid_initium %>%
  left_join(covid_initium_country, by = c("name" = "country"))



###end


hist(log(travel_leak$cn_tour_dense))


plot(log(travel_leak$cn_tour_dense), log(travel_leak$X20200318))

cor.test(log(travel_leak$cn_psg_dense), log(travel_leak$X20200318 + 1))


cor(log(travel_leak$cn_psg_dense), log(travel_leak$X20200318 + 1))


rsq <- r ^ 2

plot(log(travel_leak$cn_tour_dense), travel_leak$imported_sum)

import_reg <- lm(log(X20200318 + 1) ~ log(cn_tour_dense), travel_leak)


head(merged$cn_tour_dense)

merged$dense_2018[1:20]

plot(log(merged$`20200307` + 1), log(merged$export + 1))

cor.test(merged$export, merged$`20200307`)

plot(merged$export, merged$`20200307`)

iso3c_check <- wb_arrival %>%
  full_join(covid_initium_country[, c(1, 3)], by = c("Country.Code" = "iso3c"))

iso3c_check <- iso3c_check %>%
  select(one_of("Country.Code", "country"), everything())















