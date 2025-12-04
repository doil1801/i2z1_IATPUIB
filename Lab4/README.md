# Практическая работа 004
dolgov18012005@yandex.ru

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Программное обеспечение Manjaro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1

## Задание

Используя программный пакет dplyr, освоить анализ DNS логов с помощью
языка программирования R.

## Ход работы

1.  Подготовка данных  
    1.1. Импортируйте данные DNS –
    https://storage.yandexcloud.net/dataset.ctfsec/dns.zip  
    1.2. Добавьте пропущенные данные о структуре данных (назначении
    столбцов)  
    1.3. Преобразуйте данные в столбцах в нужный формат  
    1.4. Просмотрите общую структуру данных с помощью функции
    glimpse()  

2.  Анализ  
    2.1. Сколько участников информационного обмена в сети Доброй
    Организации?  
    2.2. Какое соотношение участников обмена внутри сети и участников
    обращений к внешним ресурсам?  
    2.3. Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.  
    2.4. Найдите топ-10 доменов, к которым обращаются пользователи сети
    и соответственное количество обращений  
    2.5. Опеределите базовые статистические характеристики (функция
    summary() ) интервала времени между последовательными обращениями к
    топ-10 доменам.  
    2.6. Часто вредоносное программное обеспечение использует DNS канал
    в качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?  

3.  Обогащение данных  
    3.1. Определите местоположение (страну, город) и
    организацию-провайдера для топ-10 доменов. Для этого можно
    использовать сторонние сервисы, например http://ip-api.com
    (API-эндпоинт – http://ip-api.com/json).

### Шаг 1

Для начала подключим необходимые библиотеки

``` r
library(dplyr, readr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(lubridate)
```


    Attaching package: 'lubridate'

    The following objects are masked from 'package:base':

        date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats 1.0.1     ✔ stringr 1.5.2
    ✔ ggplot2 4.0.1     ✔ tibble  3.3.0
    ✔ purrr   1.1.0     ✔ tidyr   1.3.1
    ✔ readr   2.1.6     

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

#### Импорт данных

``` r
dns_data <- read.csv(file = 'dns.log', header = FALSE, sep = '\t', na.strings = c("-"))
```

#### Добавление пропущенных данных о струткуре столбцов

``` r
colnames(dns_data) <- c(
  "ts", "uid", "id.orig_h", "id.orig_p", "id.resp_h", "id.resp_p",
  "proto", "trans_id", "query", "qclass", "qclass_name", "qtype",
  "qtype_name", "rcode", "rcode_name", "AA", "TC", "RD", "RA", "Z",
  "answers", "TTLs", "rejected"
)
```

#### Преобразование данных в нужный формат

``` r
dns_data <- dns_data %>%
  mutate(
    ts = as_datetime(ts),
    across(c(id.orig_p, id.resp_p, trans_id), as.integer)

  )
```

#### Общая структура

``` r
glimpse(dns_data)
```

    Rows: 427,935
    Columns: 23
    $ ts          <dttm> 2012-03-16 12:30:05, 2012-03-16 12:30:15, 2012-03-16 12:3…
    $ uid         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bs…
    $ id.orig_h   <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "19…
    $ id.orig_p   <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ id.resp_h   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "1…
    $ id.resp_p   <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ proto       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "u…
    $ trans_id    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 62…
    $ query       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ qclass_name <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C…
    $ qtype       <int> 33, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 12, 12…
    $ qtype_name  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB…
    $ rcode       <int> 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    $ rcode_name  <chr> "NOERROR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    $ AA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ TC          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ RD          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
    $ RA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ Z           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0…
    $ answers     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ TTLs        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ rejected    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…

### Шаг 2

#### Кол-во участников информационного обмена в сети Доброй орагнизации

``` r
length(unique(c(dns_data$id.orig_h, dns_data$id.resp_h)))
```

    [1] 1359

#### Соотношение участников обмена внутри сети и участников обращений к внешним ресурсам

``` r
internal_patterns <- c("10\\.", "192\\.168\\.", "172\\.(1[6-9]|2[0-9]|3[0-1])\\.")

amonut_external_users <- dns_data %>% 
  filter(., !grepl(paste(internal_patterns, collapse = "|"), id.resp_h)) %>%
  pull(id.orig_h) %>%
  unique() %>%
  length()



amount_internal_users = length(unique(c(dns_data$id.orig_h, dns_data$id.resp_h))) - amonut_external_users 
amount_internal_users / amonut_external_users
```

    [1] 21.27869

#### Топ-10 участников с наибольшей сетевой активностью

``` r
dns_data %>% group_by(., id.orig_h) %>% count(sort=TRUE) %>% head(10)
```

    # A tibble: 10 × 2
    # Groups:   id.orig_h [10]
       id.orig_h           n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

#### Топ-10 доменов, к которым обращаются пользователи

``` r
top_domains <- dns_data %>% count(query, sort = TRUE) %>% head(10)

top_domains
```

                                                                         query
    1                                                teredo.ipv6.microsoft.com
    2                                                         tools.google.com
    3                                                            www.apple.com
    4                                                           time.apple.com
    5                                          safebrowsing.clients.google.com
    6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00
    7                                                                     WPAD
    8                                              44.206.168.192.in-addr.arpa
    9                                                                 HPE8AA67
    10                                                                  ISATAP
           n
    1  39273
    2  14057
    3  13390
    4  13109
    5  11658
    6  10401
    7   9134
    8   7248
    9   6929
    10  6569

#### Статистика по топ доменам

``` r
top10_list <- top_domains$query

dns_top10 <- dns_data %>%
  filter(., query %in% top10_list) %>%
  arrange(., query, ts)

intervals_by_domain <- dns_top10 %>%
  group_by(query) %>%
  mutate(
    time_diff_sec = c(NA, diff(ts))
  ) %>%
  ungroup()

summary_stats <- intervals_by_domain %>%
  group_by(query) %>%
  summarise(
    summary_text = list(summary(time_diff_sec, na.rm = TRUE)),
    .groups = "drop"
  )

for (i in seq_len(nrow(summary_stats))) {
  cat("\nДомен:", summary_stats$query[i], "\n")
  print(summary_stats$summary_text[[i]])
}
```


    Домен: *\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
        0.00     0.15     0.50    11.24     1.50 52723.50        1 

    Домен: 44.206.168.192.in-addr.arpa 
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
        0.00     2.09     4.00    16.01    20.09 49679.81        1 

    Домен: HPE8AA67 
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
        0.00     0.75     0.75    16.61    25.49 50044.43        1 

    Домен: ISATAP 
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
        0.00     0.75     0.76    17.46     1.05 51997.79        1 

    Домен: WPAD 
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
        0.00     0.75     0.75    12.61     1.11 50049.11        1 

    Домен: safebrowsing.clients.google.com 
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
        0.00     0.00     1.00    10.00     2.01 49952.32        1 

    Домен: teredo.ipv6.microsoft.com 
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
        0.000     0.000     0.000     2.941     0.510 50387.760         1 

    Домен: time.apple.com 
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
        0.000     0.370     1.760     8.665     4.723 50924.280         1 

    Домен: tools.google.com 
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
        0.000     0.000     0.000     8.187     1.000 50364.830         1 

    Домен: www.apple.com 
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
        0.000     0.000     1.000     8.607     3.010 50963.630         1 

### Шаг 3

#### Подозрительные ip адреса

``` r
top_domains <- dns_data %>%
  count(query, sort = TRUE) %>%
  filter(n > 5) %>%
  pull(query)

get_timing_stats <- function(domain_data) {
  times <- sort(domain_data$ts)
  diffs <- as.numeric(diff(times))
  tibble(
    query = domain_data$query[1],
    n_intervals = length(diffs),
    mean_gap = mean(diffs),
    sd_gap = sd(diffs),
    cv = sd_gap / mean_gap
  )
}

timing_summary <- dns_data %>%
  filter(query %in% top_domains) %>%
  split(.$query) %>%
  map_dfr(~ get_timing_stats(.x)) %>%
  filter(cv < 0.3) %>%
  arrange(sd_gap)

timing_summary
```

    # A tibble: 39 × 5
       query                                    n_intervals mean_gap  sd_gap      cv
       <chr>                                          <int>    <dbl>   <dbl>   <dbl>
     1 _ldap._tcp.54540b4e-6ae9-4836-97b1-0f52…           9   15.1   5.58e-4 3.69e-5
     2 _ldap._tcp.dc._msdcs.ccbc.ccbcmd.edu               9   15.1   5.58e-4 3.69e-5
     3 EIVXGJQRTS                                         5    0.748 4.47e-3 5.98e-3
     4 UWPKDLUJMR                                         5    0.748 4.47e-3 5.98e-3
     5 VOAGETTQEU                                         5    0.748 4.47e-3 5.98e-3
     6 input.mozilla.com                                 31    5.00  4.75e-3 9.50e-4
     7 opengts.tx.hec.net                                15    5.00  4.88e-3 9.75e-4
     8 FZZDSQLOIN                                         5    0.744 5.48e-3 7.36e-3
     9 LQMCUWCBUA                                         5    0.744 5.48e-3 7.36e-3
    10 XRTQYXYWZG                                         5    0.744 5.48e-3 7.36e-3
    # ℹ 29 more rows

#### Определение местоположения и провайдера для топ доменов

``` r
library(httr)
library(jsonlite)
```


    Attaching package: 'jsonlite'

    The following object is masked from 'package:purrr':

        flatten

``` r
top_domains <- dns_data %>% count(query, sort = TRUE) %>% head(10)
top10_list <- top_domains$query

get_geo_info <- function(domain) {
  url <- paste0("http://ip-api.com/json/", URLencode(domain))
  resp <- GET(url)
  
  if (http_type(resp) != "application/json") {
    return(tibble(domain = domain, country = NA, city = NA, isp = NA, success = FALSE))
  }
  
  data <- content(resp, "parsed", encoding = "UTF-8")
  
  if (is.null(data) || !isTRUE(data$status == "success")) {
    return(tibble(domain = domain, country = NA, city = NA, isp = NA, success = FALSE))
  }
  
  tibble(
    domain = domain,
    country = data$country %||% NA,
    city = data$city %||% NA,
    isp = data$isp %||% NA,
    success = TRUE
  )
}

geo_results <- tibble(domain = top10_list) %>%
  pmap_dfr(~ get_geo_info(.x))

print(geo_results %>% select(domain, country, city, isp))
```

    # A tibble: 10 × 4
       domain                                                    country city  isp  
       <chr>                                                     <chr>   <chr> <chr>
     1 "teredo.ipv6.microsoft.com"                               <NA>    <NA>  <NA> 
     2 "tools.google.com"                                        United… Moun… Goog…
     3 "www.apple.com"                                           United… Lond… Akam…
     4 "time.apple.com"                                          France  Clic… Appl…
     5 "safebrowsing.clients.google.com"                         United… Moun… Goog…
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x0… <NA>    <NA>  <NA> 
     7 "WPAD"                                                    <NA>    <NA>  <NA> 
     8 "44.206.168.192.in-addr.arpa"                             <NA>    <NA>  <NA> 
     9 "HPE8AA67"                                                <NA>    <NA>  <NA> 
    10 "ISATAP"                                                  <NA>    <NA>  <NA> 

### Итог

Отчёт написан и оформлен
