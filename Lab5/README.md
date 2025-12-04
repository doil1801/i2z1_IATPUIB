# Практическая работа 005
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

Используя программный пакет dplyr языка программирования R провести
анализ журналов и ответить на вопросы.

## Ход работы

1.  Подготовка данных  
    1.1. Импортируйте данные.  
    1.2. Привести датасеты в вид “аккуратных данных”, преобразовать типы
    столбцов в соответствии с типом данных  
    1.3. Просмотрите общую структуру данных с помощью функции
    glimpse()  

2.  Анализ  
    2.1. Определить небезопасные точки доступа (без шифрования – OPN)  
    2.2. Определить производителя для каждого обнаруженного устройства  
    2.3. Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах  
    2.4. Отсортировать точки доступа по интервалу времени, в течение
    которого они находились на связи, по убыванию.  
    2.5. Обнаружить топ-10 самых быстрых точек доступа.  
    2.6. Отсортировать точки доступа по частоте отправки запросов
    (beacons) в единиц времени по их убыванию.  

3.  Данные клиентов  
    3.1. Определить производителя для каждого обнаруженного устройства
    3.2. Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес
    3.3. Кластеризовать запросы от устройств к точкам доступа по их
    именам. Определить время появления устройства в зоне радиовидимости
    и время выхода его из нее. 3.4. Оценить стабильность уровня сигнала
    внури кластера во времени. Выявить наиболее стабильный кластер.

### Шаг 1

#### Импорт данных

``` r
library(readr)
library(dplyr)
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

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
lines = readLines("P2_wifi_data.csv")

split_str <- which(lines=="")[1]
wifi_data1 = read_csv("P2_wifi_data.csv", skip=0, n_max = split_str-2)
```

    Rows: 167 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (6): BSSID, Privacy, Cipher, Authentication, LAN IP, ESSID
    dbl  (6): channel, Speed, Power, # beacons, # IV, ID-length
    lgl  (1): Key
    dttm (2): First time seen, Last time seen

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
wifi_data2 =  read_csv("P2_wifi_data.csv", skip=split_str)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 12081 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (3): Station MAC, BSSID, Probed ESSIDs
    dbl  (2): Power, # packets
    dttm (2): First time seen, Last time seen

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

#### Приведем к аккуартному виду таблицу с анонсами беспроводных точек доступа

``` r
wifi_clean <- wifi_data1 %>%
  rename(.,
        First_time_seen=2,
        Last_time_seen=3,
        Beacons=10,
        IV=11,
        Lan_IP=12,
        ID_length=13
    
  ) %>%
  mutate(
    channel = as.integer(channel),
    Speed = as.integer(Speed),
    First_time_seen = ymd_hms(First_time_seen, tz = "UTC"),
    Last_time_seen = ymd_hms(Last_time_seen, tz = "UTC")
  )

glimpse(wifi_clean)
```

    Rows: 167
    Columns: 15
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ First_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last_time_seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 …
    $ channel         <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, …
    $ Speed           <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180,…
    $ Privacy         <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2",…
    $ Cipher          <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "C…
    $ Authentication  <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "…
    $ Power           <dbl> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42,…
    $ Beacons         <dbl> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 13…
    $ IV              <dbl> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, …
    $ Lan_IP          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.…
    $ ID_length       <dbl> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 1…
    $ ESSID           <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "M…
    $ Key             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

#### Приведем к аккуартному виду таблицу с анонсами беспроводных точек доступа

``` r
user_clean <- wifi_data2 %>%
  mutate(
    Power = as.integer(Power),
  ) %>%
  rename(
    Station_MAC=1,
    First_time_seen=2,
    Last_time_seen=3,
    Packets=5,
    Probed_ESSIDs=7
    
  )

glimpse(user_clean)
```

    Rows: 12,081
    Columns: 7
    $ Station_MAC     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E…
    $ First_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last_time_seen  <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 …
    $ Power           <int> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45,…
    $ Packets         <dbl> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7…
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D6:…
    $ Probed_ESSIDs   <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U…

### Шаг 2

#### Определим небезопасные точки доступа

``` r
wifi_clean %>% filter(., Privacy=="OPN")
```

    # A tibble: 42 × 15
       BSSID    First_time_seen     Last_time_seen      channel Speed Privacy Cipher
       <chr>    <dttm>              <dttm>                <int> <int> <chr>   <chr> 
     1 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
     2 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:12       6   130 OPN     <NA>  
     3 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:11       6   130 OPN     <NA>  
     4 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:10       6    -1 OPN     <NA>  
     5 00:25:0… 2023-07-28 09:13:06 2023-07-28 11:56:21      44    -1 OPN     <NA>  
     6 E8:28:C… 2023-07-28 09:13:09 2023-07-28 11:56:05      11   130 OPN     <NA>  
     7 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:27:06       6   130 OPN     <NA>  
     8 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:39:43       6   130 OPN     <NA>  
     9 E8:28:C… 2023-07-28 09:13:17 2023-07-28 11:52:32       1   130 OPN     <NA>  
    10 E8:28:C… 2023-07-28 09:13:50 2023-07-28 11:43:39      11   130 OPN     <NA>  
    # ℹ 32 more rows
    # ℹ 8 more variables: Authentication <chr>, Power <dbl>, Beacons <dbl>,
    #   IV <dbl>, Lan_IP <chr>, ID_length <dbl>, ESSID <chr>, Key <lgl>

#### Определим производителя для обнаруженных устройств

``` r
oui = read_delim("manuf.txt", col_names = c("OUI", "Short_name", "Full_name"), delim = "\t", quote = '"') %>% 
  mutate(
    OUI = substr(OUI, 1, 8)
  )
```

    Rows: 55614 Columns: 3
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (3): OUI, Short_name, Full_name

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
wifi_clean_vendor <- wifi_clean %>%
  mutate(
    OUI = substr(BSSID, 1, 8)
  )

wifi_clean_vendor <- wifi_clean_vendor %>%
  left_join(oui, by = "OUI")
wifi_clean_vendor
```

    # A tibble: 167 × 18
       BSSID    First_time_seen     Last_time_seen      channel Speed Privacy Cipher
       <chr>    <dttm>              <dttm>                <int> <int> <chr>   <chr> 
     1 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:50       1   195 WPA2    CCMP  
     2 6E:C7:E… 2023-07-28 09:13:03 2023-07-28 11:55:12       1   130 WPA2    CCMP  
     3 9A:75:A… 2023-07-28 09:13:03 2023-07-28 11:53:31       1   360 WPA2    CCMP  
     4 4A:EC:1… 2023-07-28 09:13:03 2023-07-28 11:04:01       7   360 WPA2    CCMP  
     5 D2:6D:5… 2023-07-28 09:13:03 2023-07-28 10:30:19       6   130 WPA2    CCMP  
     6 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
     7 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:44      11   195 WPA2    CCMP  
     8 0A:C5:E… 2023-07-28 09:13:03 2023-07-28 11:36:31      11   130 WPA2    CCMP  
     9 38:1A:5… 2023-07-28 09:13:03 2023-07-28 10:25:02      11   130 WPA2    CCMP  
    10 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 10:29:21       1   195 WPA2    CCMP  
    # ℹ 157 more rows
    # ℹ 11 more variables: Authentication <chr>, Power <dbl>, Beacons <dbl>,
    #   IV <dbl>, Lan_IP <chr>, ID_length <dbl>, ESSID <chr>, Key <lgl>, OUI <chr>,
    #   Short_name <chr>, Full_name <chr>

#### Устройства, использующие WPA3

``` r
wifi_clean %>%
  filter(str_detect(Privacy, regex("WPA3", ignore_case = TRUE))) %>%
  select(BSSID, ESSID, Privacy)
```

    # A tibble: 8 × 3
      BSSID             ESSID                Privacy  
      <chr>             <chr>                <chr>    
    1 26:20:53:0C:98:E8 <NA>                 WPA3 WPA2
    2 A2:FE:FF:B8:9B:C9 Christie’s           WPA3 WPA2
    3 96:FF:FC:91:EF:64 <NA>                 WPA3 WPA2
    4 CE:48:E7:86:4E:33 iPhone (Анастасия)   WPA3 WPA2
    5 8E:1F:94:96:DA:FD iPhone (Анастасия)   WPA3 WPA2
    6 BE:FD:EF:18:92:44 Димасик              WPA3 WPA2
    7 3A:DA:00:F9:0C:02 iPhone XS Max 🦊🐱🦊 WPA3 WPA2
    8 76:C5:A0:70:08:96 <NA>                 WPA3 WPA2

#### Отсортируем точки доступа по интервалу времени, в течение которого они находились на связи, по убыванию.

``` r
GAP_LIMIT_SEC <- 45 * 60
ap_duration <- wifi_clean %>%
  arrange(BSSID, First_time_seen) %>%
  group_by(BSSID, ESSID) %>%
  filter(!is.na(First_time_seen), !is.na(Last_time_seen)) %>%
  mutate(
    start = First_time_seen,
    prev_end = lag(Last_time_seen),
    gap = as.numeric(difftime(start, prev_end, units = "secs")),
    is_new_session = is.na(gap) | (gap > GAP_LIMIT_SEC),
    session_group = cumsum(is_new_session)
  ) %>%
  group_by(BSSID, ESSID, session_group) %>%
  summarise(
    session_start = min(First_time_seen, na.rm = TRUE),
    session_end   = max(Last_time_seen,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(BSSID, ESSID) %>%
  summarise(
    total_time_on_air_sec = sum(as.numeric(difftime(session_end, session_start, units = "secs"))),
    .groups = "drop"
  ) %>%
  arrange(desc(total_time_on_air_sec)) %>%
  select(BSSID, ESSID, total_time_on_air_sec)

ap_duration
```

    # A tibble: 167 × 3
       BSSID             ESSID         total_time_on_air_sec
       <chr>             <chr>                         <dbl>
     1 00:25:00:FF:94:73 <NA>                           9795
     2 E8:28:C1:DD:04:52 MIREA_HOTSPOT                  9776
     3 E8:28:C1:DC:B2:52 MIREA_HOTSPOT                  9755
     4 08:3A:2F:56:35:FE <NA>                           9746
     5 6E:C7:EC:16:DA:1A Cnet                           9729
     6 E8:28:C1:DC:B2:50 MIREA_GUESTS                   9726
     7 48:5B:39:F9:7A:48 <NA>                           9725
     8 E8:28:C1:DC:B2:51 <NA>                           9725
     9 E8:28:C1:DC:FF:F2 <NA>                           9724
    10 8E:55:4A:85:5B:01 Vladimir                       9723
    # ℹ 157 more rows

#### Топ-10 самых быстрых точек доступа

``` r
wifi_clean %>%
  mutate(Speed = as.numeric(Speed)) %>%
  filter(!is.na(Speed)) %>%
  select(BSSID, ESSID, Speed) %>%
  arrange(desc(Speed)) %>%
  head(10)
```

    # A tibble: 10 × 3
       BSSID             ESSID              Speed
       <chr>             <chr>              <dbl>
     1 26:20:53:0C:98:E8 <NA>                 866
     2 96:FF:FC:91:EF:64 <NA>                 866
     3 CE:48:E7:86:4E:33 iPhone (Анастасия)   866
     4 8E:1F:94:96:DA:FD iPhone (Анастасия)   866
     5 9A:75:A8:B9:04:1E KC                   360
     6 4A:EC:1E:DB:BF:95 POCO X5 Pro 5G       360
     7 56:C5:2B:9F:84:90 OnePlus 6T           360
     8 E8:28:C1:DC:B2:41 MIREA_GUESTS         360
     9 E8:28:C1:DC:B2:40 MIREA_HOTSPOT        360
    10 E8:28:C1:DC:B2:42 <NA>                 360

#### Отсортируем точки доступа по частоте отправки запросов (beacons) в единицу времени по их убыванию.

``` r
wifi_beacon <- wifi_clean %>%
  group_by(BSSID, ESSID) %>%
  summarise(
    total_beacons = sum(Beacons, na.rm = TRUE),
    first_seen    = min(First_time_seen, na.rm = TRUE),
    last_seen     = max(Last_time_seen,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    duration_sec = as.numeric(difftime(last_seen, first_seen, units = "secs")),
    beacon_rate  = ifelse(duration_sec > 0, total_beacons / duration_sec, NA)
  ) %>%
  filter(!is.na(beacon_rate)) %>%
  arrange(desc(beacon_rate)) %>%
  select(BSSID, ESSID, total_beacons, duration_sec, beacon_rate)

wifi_beacon
```

    # A tibble: 124 × 5
       BSSID             ESSID                total_beacons duration_sec beacon_rate
       <chr>             <chr>                        <dbl>        <dbl>       <dbl>
     1 F2:30:AB:E9:03:ED iPhone (Uliana)                  6            7       0.857
     2 B2:CF:C0:00:4A:60 Михаил's Galaxy M32              4            5       0.8  
     3 3A:DA:00:F9:0C:02 iPhone XS Max 🦊🐱🦊             5            9       0.556
     4 00:3E:1A:5D:14:45 MT_FREE                          1            2       0.5  
     5 02:BC:15:7E:D5:DC MT_FREE                          1            2       0.5  
     6 76:C5:A0:70:08:96 <NA>                             1            2       0.5  
     7 D2:25:91:F6:6C:D8 Саня                             5           13       0.385
     8 BE:F1:71:D6:10:D7 C322U21 0566                  1647         9461       0.174
     9 00:03:7A:1A:03:56 MT_FREE                          1            6       0.167
    10 38:1A:52:0D:84:D7 EBFCD57F-EE81fI_DL_…           704         4319       0.163
    # ℹ 114 more rows

### Шаг 3

#### Определить производителя для каждого обнаруженного устройства

``` r
user_oui <- user_clean %>%  mutate(
    OUI = substr(Station_MAC, 1, 8)
  )

user_with_vendor <- user_oui %>%
  left_join(oui, by = "OUI")

user_with_vendor
```

    # A tibble: 12,081 × 10
       Station_MAC       First_time_seen     Last_time_seen      Power Packets BSSID
       <chr>             <dttm>              <dttm>              <int>   <dbl> <chr>
     1 CA:66:3B:8F:56:DD 2023-07-28 09:13:03 2023-07-28 10:59:44   -33     858 BE:F…
     2 96:35:2D:3D:85:E6 2023-07-28 09:13:03 2023-07-28 09:13:03   -65       4 (not…
     3 5C:3A:45:9E:1A:7B 2023-07-28 09:13:03 2023-07-28 11:51:54   -39     432 BE:F…
     4 C0:E4:34:D8:E7:E5 2023-07-28 09:13:03 2023-07-28 11:53:16   -61     958 BE:F…
     5 5E:8E:A6:5E:34:81 2023-07-28 09:13:04 2023-07-28 09:13:04   -53       1 (not…
     6 10:51:07:CB:33:E7 2023-07-28 09:13:05 2023-07-28 11:56:06   -43     344 (not…
     7 68:54:5A:40:35:9E 2023-07-28 09:13:06 2023-07-28 11:50:50   -31     163 1E:9…
     8 74:4C:A1:70:CE:F7 2023-07-28 09:13:06 2023-07-28 09:20:01   -71       3 E8:2…
     9 8A:A3:5A:33:76:57 2023-07-28 09:13:06 2023-07-28 10:20:27   -74     115 00:2…
    10 CA:54:C4:8B:B5:3A 2023-07-28 09:13:06 2023-07-28 11:55:04   -65     437 00:2…
    # ℹ 12,071 more rows
    # ℹ 4 more variables: Probed_ESSIDs <chr>, OUI <chr>, Short_name <chr>,
    #   Full_name <chr>

#### Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
is_locally_administered <- function(mac) {
  second_nibble <- substr(mac, 2, 2)
  toupper(second_nibble) %in% c("2", "6", "A", "E")
}

non_randomized_devices <- user_with_vendor %>%
  mutate(
    is_global = !is_locally_administered(Station_MAC)
  ) %>%
  filter(is_global) %>%
  select(Station_MAC, OUI, Short_name, Full_name, Probed_ESSIDs) %>%
  distinct()

non_randomized_devices
```

    # A tibble: 220 × 5
       Station_MAC       OUI      Short_name     Full_name             Probed_ESSIDs
       <chr>             <chr>    <chr>          <chr>                 <chr>        
     1 5C:3A:45:9E:1A:7B 5C:3A:45 "ChongqingFug" Chongqing Fugui Elec… C322U21 0566 
     2 C0:E4:34:D8:E7:E5 C0:E4:34 "AzureWaveTec" AzureWave Technology… C322U13 3965 
     3 10:51:07:CB:33:E7 10:51:07 "Intel       " Intel Corporate       <NA>         
     4 68:54:5A:40:35:9E 68:54:5A "Intel       " Intel Corporate       C322U06 5179…
     5 74:4C:A1:70:CE:F7 74:4C:A1 "LiteonTechno" Liteon Technology Co… <NA>         
     6 BC:F1:71:D4:DB:04 BC:F1:71 "Intel       " Intel Corporate       <NA>         
     7 4C:44:5B:14:76:E3 4C:44:5B "Intel       " Intel Corporate       <NA>         
     8 A0:E7:0B:AE:D5:44 A0:E7:0B "Intel       " Intel Corporate       AndroidAP177B
     9 00:95:69:E7:7F:35 00:95:69 "LSDSciencean" LSD Science and Tech… nvripcsuite  
    10 00:95:69:E7:7C:ED 00:95:69 "LSDSciencean" LSD Science and Tech… nvripcsuite  
    # ℹ 210 more rows

#### Кластеризовать запросы от устройств к точкам доступа по их именам. Определить время появления устройства в зоне радиовидимости и время выхода его из нее

``` r
device_clusters <- user_clean %>%
  filter(!is.na(Probed_ESSIDs), Probed_ESSIDs != "", Probed_ESSIDs != "N/A") %>%
  mutate(
    Probed_ESSIDs = str_squish(Probed_ESSIDs),
    ESSID_list = str_split(Probed_ESSIDs, ",")
  ) %>%
  unnest(ESSID_list) %>%
  mutate(ESSID_list = str_squish(ESSID_list)) %>%
  filter(ESSID_list != "") %>%
  rename(ESSID = ESSID_list) %>%
  group_by(Station_MAC, ESSID) %>%
  summarise(
    first = min(First_time_seen, na.rm = TRUE),
    last  = max(Last_time_seen,  na.rm = TRUE),
    power = mean(Power),
    .groups = "drop"
  ) 

device_clusters
```

    # A tibble: 1,831 × 5
       Station_MAC       ESSID         first               last                power
       <chr>             <chr>         <dttm>              <dttm>              <dbl>
     1 00:90:4C:E6:54:54 "Redmi"       2023-07-28 09:16:59 2023-07-28 10:21:15   -65
     2 00:95:69:E7:7C:ED "nvripcsuite" 2023-07-28 09:13:11 2023-07-28 11:56:13   -55
     3 00:95:69:E7:7D:21 "nvripcsuite" 2023-07-28 09:13:15 2023-07-28 11:56:17   -33
     4 00:95:69:E7:7F:35 "nvripcsuite" 2023-07-28 09:13:11 2023-07-28 11:56:07   -69
     5 00:F4:8D:F7:C5:19 "Hornet24"    2023-07-28 10:45:04 2023-07-28 11:43:26   -73
     6 00:F4:8D:F7:C5:19 "Redmi 12"    2023-07-28 10:45:04 2023-07-28 11:43:26   -73
     7 02:00:00:00:00:00 "CPPK_FREE"   2023-07-28 09:54:40 2023-07-28 11:55:36   -67
     8 02:00:00:00:00:00 "MIREA"       2023-07-28 09:54:40 2023-07-28 11:55:36   -67
     9 02:00:00:00:00:00 "MIREA_HOTSP… 2023-07-28 09:54:40 2023-07-28 11:55:36   -67
    10 02:00:00:00:00:00 "\\xAC\\xBA\… 2023-07-28 09:54:40 2023-07-28 11:55:36   -67
    # ℹ 1,821 more rows

#### Оценим стабильность уровня сигнала внури кластера во времени. Выявим наиболее стабильный кластер.

``` r
stable_cluster <- device_clusters %>%
  group_by(ESSID) %>%
  summarise(
    .,
    mean_power = mean(power, na.rm = TRUE),
    sd_power = sd(power, na.rm = TRUE),
    stability_score = sd_power/abs(mean_power)
  ) %>% 
  filter(!is.na(stability_score)) %>%
  arrange(., stability_score)
head(stable_cluster, 1) 
```

    # A tibble: 1 × 4
      ESSID     mean_power sd_power stability_score
      <chr>          <dbl>    <dbl>           <dbl>
    1 CPPK_FREE        -67        0               0

### Итог

Отчёт написан и оформлен
