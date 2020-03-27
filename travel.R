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

#台灣出境國統計

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

tw_outbound$hot_travel <- case_when(tw_outbound$cont == "歐洲" ~ 1,
                                    tw_outbound$country == "以色列" ~ 1,
                                    tw_outbound$country == "約旦" ~ 1,
                                    tw_outbound$country == "黎巴嫩" ~ 1,
                                    tw_outbound$country == "阿拉伯聯合大公國" ~ 1,
                                    tw_outbound$country == "土耳其" ~ 1,
                                    tw_outbound$country == "埃及" ~ 1,
                                    tw_outbound$country == "摩洛哥" ~ 1,
                                    tw_outbound$country == "阿爾及利亞" ~ 1,
                                    tw_outbound$country == "突尼西亞	" ~ 1,
                                    TRUE ~ 0)

eu_med <- tw_outbound[, c(1, 2, 7, 8)]

write_xlsx(eu_med, "eu_med.xlsx")

#各國赴中國遊客統計

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


#中國遊客赴各國統計

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


#航空旅客資料

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


#世界銀行各國旅客統計

wb_arrival <- read.csv("data/API_ST.INT.ARVL_DS2_en_csv_v2_868314.csv", skip = 4)
wb_arrival <- wb_arrival[, c(1, 2, 62, 63)]

wb_arrival$X2018[is.na(wb_arrival$X2018) == TRUE] <- wb_arrival$X2017[is.na(wb_arrival$X2018) == TRUE]

wb_arrival <- rename(wb_arrival, "tour_arr_2017" = "X2017")
wb_arrival <- rename(wb_arrival, "tour_arr_2018" = "X2018")


#世界銀行GDP

wb_gdp <- read.csv("data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_866857.csv", skip = 4)
wb_gdp <- wb_gdp[, c(1, 2, 62, 63)]

wb_gdp$X2018[is.na(wb_gdp$X2018) == TRUE] <- wb_gdp$X2017[is.na(wb_gdp$X2018) == TRUE]

wb_gdp <- rename(wb_gdp, "gdp_2017" = "X2017")
wb_gdp <- rename(wb_gdp, "gdp_2018" = "X2018")

#世界銀行人口密度

wb_dense <- read.csv("data/API_EN.POP.DNST_DS2_en_csv_v2_868885.csv", skip = 4)
wb_dense <- wb_dense[, c(1, 2, 62, 63)]

wb_dense$X2018[is.na(wb_dense$X2018) == TRUE] <- wb_dense$X2017[is.na(wb_dense$X2018) == TRUE]

wb_dense <- rename(wb_dense, "dense_2017" = "X2017")
wb_dense <- rename(wb_dense, "dense_2018" = "X2018")

#John Hopkins COVID19 data

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

#台灣境外移入來源

imported <- read_xlsx("data/境外移入案例來源_20200325.xlsx", sheet = "全台武肺個案+國碼")

colnames(imported)[3] <- "domestic"

imported <- imported %>%
  filter(domestic == "境外")

imported <- imported %>%
  dplyr::select(-contains("all"))

imported[is.na(imported$`基因定序確認感染來源`) == FALSE, 4] <- imported$`基因定序確認感染來源`[is.na(imported$`基因定序確認感染來源`) == FALSE]
imported[is.na(imported$`基因定序確認感染來源`) == FALSE, 5:10] <- NA

imported <- imported %>%
  gather(key = "in", value = country, 4:10)

imported <- imported %>%
  filter(is.na(country) == FALSE)

imported <- imported %>%
  group_by(公佈日期, country) %>%
  summarise(count = n())

imported <- imported %>%
  spread(公佈日期, count)

imported$import_sum <- rowSums(imported[,2:30], na.rm = TRUE)

#trade data

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

#merge data

merged <- wb_arrival %>%
  full_join(cn_outbound[, c(5, 2, 3)], by = c("Country.Code" = "iso3c")) %>%
  full_join(cn_arrival[, c(5, 2, 3)], by = c("Country.Code" = "iso3c")) %>%
  full_join(tw_outbound[, c(7, 4, 5, 6, 8)], by = c("Country.Code" = "iso3c")) %>%
  full_join(wb_gdp[, c(2, 3, 4)], by = "Country.Code") %>%
  full_join(wb_dense[, c(2, 3, 4)], by = "Country.Code") %>%
  full_join(china_trade, by = c("Country.Code" = "dest")) %>%
  full_join(COVID_sum, by = c("Country.Code" = "iso3c")) %>%
  full_join(imported[,c(1, 31)], by = c("Country.Code" = "country")) %>%
  full_join(air_psg[, c(2, 3, 5)], by = c("Country.Code" = "iso3c"))


merged[, 19:37][is.na(merged[, 19:37])] <- 0

#write.csv(merged, "data/merged_20200325.csv", row.names = FALSE)

merged <- read.csv("data/merged_20200325.csv", stringsAsFactors = FALSE)

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

merged <- merged %>%
  select(one_of("Country.Code", "country"), everything())
















