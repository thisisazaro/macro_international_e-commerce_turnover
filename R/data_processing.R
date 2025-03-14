
install.packages(c("tidyverse", "WDI", "IMFData", "quantmod", "corrr"))

library(httr)
library(jsonlite)
library(tidyverse)
library(WDI)
library(quantmod)
library(corrr)
library(dplyr)
library(tidyr)
library(lubridate)

# Получение данных по ВВП, производству и доходам населения
# Выбираем страны
countries <- c("RUS", "CHN", "USA", "GBR", "EU", "IND", "TUR", "JPN")

# Загружаем данные
economic_data <- WDI(
        country = countries,
        indicator = c("NY.GDP.PCAP.CD", "NV.IND.MANF.CD", "NE.CON.PRVT.CD", "SL.UEM.TOTL.ZS", "SP.POP.TOTL"),
        start = 2010, end = 2024,
        extra = TRUE
)

# Посмотрим на данные
head(economic_data)

unique(economic_data$country)

# Оставляем только нужные колонки и переименовываем их для удобства
economic_data_clean <- economic_data %>%
        select(year, country, NY.GDP.PCAP.CD, NV.IND.MANF.CD, NE.CON.PRVT.CD, SL.UEM.TOTL.ZS, SP.POP.TOTL) %>%
        rename(
                GDP_per_capita = NY.GDP.PCAP.CD,
                Manufacturing_output = NV.IND.MANF.CD,
                Household_consumption = NE.CON.PRVT.CD,
                Unemployment_rate = SL.UEM.TOTL.ZS,
                Population = SP.POP.TOTL
        )

# Проверяем итоговую таблицу
head(economic_data_clean)

library(ggplot2)

ggplot(economic_data_clean, aes(x = year, y = GDP_per_capita, color = country)) +
        geom_line(linewidth = 1) +
        theme_minimal() +
        labs(title = "GDP per Capita Over Time",
             x = "Year", y = "GDP per Capita (USD)") +
        theme(legend.position = "bottom")


# Дублируем макроэкономические данные для всех 4 кварталов каждого года
economic_data_quarterly <- economic_data_clean %>%
        expand_grid(quarter = c("Q1", "Q2", "Q3", "Q4"))

# Проверяем итоговую таблицу
head(economic_data_quarterly)





# Подготовка данных по платежным системам
# Определяем код базы данных МВФ (IFS - International Financial Statistics)
databaseID <- "IFS"

# Определяем параметры запроса: 
base_url <- "https://dataservices.imf.org/REST/SDMX_JSON.svc/"
query <- "CompactData/IFS/Q.RUS+CHN+USA+GBR+EU+IND+TUR+JPN.FI_R_PA_SA?startPeriod=2010&endPeriod=2023"

# Выполняем запрос
response <- GET(paste0(base_url, query), timeout(60))

# Преобразуем в JSON
data_json <- content(response, as = "text", encoding = "UTF-8")
data_list <- fromJSON(data_json, flatten = TRUE)

# Извлекаем данные
imf_data <- data_list$CompactData$DataSet$Series

# Преобразуем в датафрейм
df <- data.frame(
        Country = sapply(imf_data, function(x) x$`@REF_AREA`),
        Time = sapply(imf_data, function(x) x$Obs$`@TIME_PERIOD`),
        Value = sapply(imf_data, function(x) x$Obs$`@OBS_VALUE`)
)

# Выводим данные
head(df)




# Загрузка данных по оборотам торговых площадок
# Загружаем данные для Alibaba (BABA) и Amazon (AMZN)
getSymbols(c("AMZN", "EBAY", "BABA", "JD", "ASC.L", "ZAL.DE", "HEPS"), 
           src = "yahoo", from = "2010-01-01", to = "2024-12-31")

head(AMZN)  # Amazon
head(BABA)  # Alibaba
head(EBAY)  # Ebay
head(HEPS)  # Hepsiburada
head(JD)  # JD
head(ASC.L)  # ASC.L
head(ZAL.DE)  # ZAL.DE

# Теперь давайте преобразуем данные в квартальные показатели (усредним цену 
# акций по кварталам, чтобы сопоставить с макроэкономическими данными).
# Объединим с экономическими данными по странам.



# Функция для усреднения данных по кварталам
convert_to_quarterly <- function(stock_data, ticker) {
        stock_data %>%
                data.frame(date = index(stock_data), coredata(stock_data)) %>%
                mutate(
                        year = year(date),
                        quarter = paste0("Q", quarter(date))
                ) %>%
                group_by(year, quarter) %>%
                summarize(Average_Price = mean(get(paste0(ticker, ".Adjusted")), na.rm = TRUE)) %>%
                mutate(platform = ticker) %>%
                ungroup()
}

# Преобразуем данные для всех платформ
amzn_qtr <- convert_to_quarterly(AMZN, "AMZN")
ebay_qtr <- convert_to_quarterly(EBAY, "EBAY")
baba_qtr <- convert_to_quarterly(BABA, "BABA")
jd_qtr <- convert_to_quarterly(JD, "JD")
asc_qtr <- convert_to_quarterly(ASC.L, "ASC.L")
zal_qtr <- convert_to_quarterly(ZAL.DE, "ZAL.DE")
heps_qtr <- convert_to_quarterly(HEPS, "HEPS")


platform_country_map <- tibble(
        platform = c("AMZN", "EBAY", "BABA", "JD", "ASC.L", "ZAL.DE", "HEPS"),
        country = c("United States", "United States", "China", "China", "United Kingdom", "European Union", "Turkiye")
)

str(platform_country_map)

# Объединяем данные по всем платформам
ecommerce_data <- bind_rows(amzn_qtr, ebay_qtr, baba_qtr, jd_qtr, asc_qtr, zal_qtr, heps_qtr)
str(ecommerce_data)

# Объединяем платформы с их странами
ecommerce_data <- ecommerce_data %>%
        left_join(platform_country_map, by = "platform")
str(ecommerce_data)


final_data <- ecommerce_data %>%
        left_join(economic_data_quarterly, by = c("year", "quarter", "country"))

# Проверяем итоговый результат
head(final_data)
summary(final_data)


library(tidyr)

# Удаляем строки, где есть NA в важных колонках
final_data_clean <- final_data %>%
        drop_na(GDP_per_capita, Manufacturing_output, Household_consumption, Unemployment_rate, Population)

# Проверяем, ушли ли NA
summary(final_data_clean)



























# Устанавливаем и загружаем нужные пакеты
install.packages(c("tidyverse", "WDI", "IMFData", "httr", "jsonlite", "rvest"))
library(tidyverse)
library(WDI)
library(IMFData)
library(httr)
library(jsonlite)
library(rvest)

# Определяем список стран
countries <- c("RUS", "CHN", "USA", "EUU", "GBR", "IND", "TUR", "JPN")

# 1. ВВП (GDP)
gdp_data <- WDI(country = countries, indicator = "NY.GDP.MKTP.CD", start = 2010, end = 2024)
colnames(gdp_data) <- c("Country", "Year", "GDP")

# 2. Производство (Industrial Production Index - IPI)
ipi_data <- WDI(country = countries, indicator = "IP.JPN.PROD", start = 2010, end = 2024)
colnames(ipi_data) <- c("Country", "Year", "Industrial_Production")

# 3. Электронная торговля (E-commerce trade volume)
ecommerce_trade_url <- "https://unctad.org/topic/ecommerce-and-digital-economy"
ecommerce_trade_page <- read_html(ecommerce_trade_url)
ecommerce_trade_table <- ecommerce_trade_page %>%
        html_table(fill = TRUE) %>%
        .[[1]] # Обрабатываем первую таблицу

# 4. Потребительские доходы и расходы (Household Income and Expenditure)
income_expense_data <- WDI(country = countries, indicator = c("NE.CON.PRVT.CD", "NY.GDP.PCAP.CD"), start = 2010, end = 2024)
colnames(income_expense_data) <- c("Country", "Year", "Private_Consumption", "GDP_per_Capita")

# 5. Номинальная зарплата (Nominal Wages)
wages_data <- WDI(country = countries, indicator = "SL.UEM.TOTL.ZS", start = 2010, end = 2024)
colnames(wages_data) <- c("Country", "Year", "Unemployment_Rate")

# 6. Численность населения (Population)
population_data <- WDI(country = countries, indicator = "SP.POP.TOTL", start = 2010, end = 2024)
colnames(population_data) <- c("Country", "Year", "Population")

# 7. Количество банков в странах (Number of Banks)
banks_data <- WDI(country = countries, indicator = "FB.BNK.CAPA.ZS", start = 2010, end = 2024)
colnames(banks_data) <- c("Country", "Year", "Banking_Capacity")

# 8. Обороты международных торговых платформ (B2B и B2C)
# Пример получения данных по оборотам Amazon и Alibaba (API Statista или парсинг данных)
amazon_url <- "https://www.statista.com/statistics/273963/quarterly-revenue-of-amazoncom/"
amazon_page <- read_html(amazon_url)
amazon_data <- amazon_page %>%
        html_table(fill = TRUE) %>%
        .[[1]] # Обрабатываем первую таблицу

alibaba_url <- "https://www.statista.com/statistics/272838/quarterly-revenue-of-alibaba/"
alibaba_page <- read_html(alibaba_url)
alibaba_data <- alibaba_page %>%
        html_table(fill = TRUE) %>%
        .[[1]] # Обрабатываем первую таблицу

# Объединяем все данные в один датафрейм
final_dataset <- list(
        GDP = gdp_data,
        Ecommerce_Trade = ecommerce_trade_table,
        Income_Expense = income_expense_data,
        Wages = wages_data,
        Population = population_data,
        Banking = banks_data,
        Amazon_Revenue = amazon_data,
        Alibaba_Revenue = alibaba_data
)


# Сохраняем данные в CSV
write.csv(final_dataset$GDP, "GDP_data.csv", row.names = FALSE)
write.csv(final_dataset$Industrial_Production, "IPI_data.csv", row.names = FALSE)
write.csv(final_dataset$Ecommerce_Trade, "Ecommerce_Trade.csv", row.names = FALSE)
write.csv(final_dataset$Income_Expense, "Income_Expense.csv", row.names = FALSE)
write.csv(final_dataset$Wages, "Wages.csv", row.names = FALSE)
write.csv(final_dataset$Population, "Population.csv", row.names = FALSE)
write.csv(final_dataset$Banking, "Banking.csv", row.names = FALSE)
write.csv(final_dataset$Amazon_Revenue, "Amazon_Revenue.csv", row.names = FALSE)
write.csv(final_dataset$Alibaba_Revenue, "Alibaba_Revenue.csv", row.names = FALSE)

# Выводим итоговые данные
str(final_dataset)








