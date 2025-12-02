##################################################################
####                   Number of assets                       ####
##################################################################
library(lubridate)
library(readxl)
library(tidyverse)

us <- suppressWarnings(read_csv("./Data/daily_prices_nyse_stooq.csv", col_types = c("D", rep("d", 3515)))) |> rename(Data = DATA) |> 
  mutate(Data = lubridate::ymd(Data)) |> 
  filter(Data >= '2011-06-01') |> 
  filter(Data <= '2025-05-31') |> 
  rename_with(~ str_replace(.x, "PU\najust p/ prov\nEm moeda orig\n", "")) |> 
  filter(!if_all(where(is.numeric), is.na)) |> 
  mutate_if(is.numeric, ~ (./lag(.) - 1)) |> 
  filter(!if_all(where(is.numeric), is.na))
br <- read_excel("./Data/daily_prices_b3_all.xlsx", skip = 3, na = c("-", "NA"), col_types = c("date", rep("numeric", 1410))) |> 
  mutate(Data = lubridate::ymd(Data)) |> 
  filter(Data >= '2011-06-01') |> 
  filter(Data <= '2025-05-31') |> 
  rename_with(~ str_replace(.x, "PU\najust p/ prov\nEm moeda orig\n", "")) |> 
  filter(!if_all(where(is.numeric), is.na)) |> 
  mutate_if(is.numeric, ~ (./lag(.) - 1)) |> 
  filter(!if_all(where(is.numeric), is.na))



InS <- 252*3
trading_month <- 21
n_tot <- nrow(us)
OoS <- n_tot  - InS
n_assets_portfolio_us <- c()
j_month <- 0
i <- 1
datas <- c()
list_names <- list()
while (InS + i <= n_tot) { 
  if ((i - 1) %% 21 == 0) {
    j_month <- j_month + 1
    datas[j_month] <- as.character(us[InS - 1 + i, 1]$Data)
    window_stocks <- us[i: min(InS - 1 + i + trading_month, n_tot), -1]
    window_stocks <- window_stocks[, which(sapply(window_stocks[ -c(1 : InS), ], function(y) sum(length(which(is.na(y))))) == 0)] # Full Future trading Days
    window_stocks <- window_stocks[, which(sapply(window_stocks[1 : InS, ], function(y) sum(length(which(is.na(y)))) / length(y))  == 0)] # Full trading history
    window_stocks <- window_stocks[1:InS, ]
    n_assets <-  ncol(window_stocks)
    n_assets_portfolio_us[j_month] <- n_assets
  }
  i <- i + 1
}


n_assets_us <- data.frame(datas, n_assets_portfolio_us) |> 
  mutate(YearMonth = floor_date(ymd(datas), unit = "month")) |> 
  select(YearMonth, n_assets_portfolio_us)


n_assets_us


InS <- 252*3
trading_month <- 21
n_tot <- nrow(br)
OoS <- n_tot  - InS
n_assets_portfolio_br <- c()
j_month <- 0
i <- 1
datas <- c()
list_names <- list()
while (InS + i <= n_tot) { 
  if ((i - 1) %% 21 == 0) {
    j_month <- j_month + 1
    datas[j_month] <- as.character(br[InS - 1 + i, 1]$Data)
    window_stocks <- br[i: min(InS - 1 + i + trading_month, n_tot), -1]
    window_stocks <- window_stocks[, which(sapply(window_stocks[ -c(1 : InS), ], function(y) sum(length(which(is.na(y))))) == 0)] # Full Future trading Days
    window_stocks <- window_stocks[, which(sapply(window_stocks[1 : InS, ], function(y) sum(length(which(is.na(y)))) / length(y))  == 0)] # Full trading history
    window_stocks <- window_stocks[1:InS, ]
    n_assets <-  ncol(window_stocks)
    n_assets_portfolio_br[j_month] <- n_assets
  }
  i <- i + 1
}

n_assets_br <- data.frame(datas, n_assets_portfolio_br) |> 
  mutate(YearMonth = floor_date(ymd(datas), unit = "month")) |> 
  select(YearMonth, n_assets_portfolio_br)


len_us <- length(na.omit(n_assets_us$n_assets_portfolio_us))
len_br <- length(na.omit(n_assets_br$n_assets_portfolio_br))
max_len <- max(len_us, len_br)
US      <- c(na.omit(n_assets_us$n_assets_portfolio_us), rep(NA, max_len - len_us))
Brazil  <- c(na.omit(n_assets_br$n_assets_portfolio_br), rep(NA, max_len - len_br))
n_assets <- data.frame(Data = 1:max_len, US = US, Brazil = Brazil)
n_assets


ggplot(n_assets) + 
  geom_line(aes(x = Data, y = US), color = "blue", linetype = "dashed") +
  geom_line(aes(x = Data, y = Brazil), color = "red", linetype = "solid")


scale_factor <- max(n_assets$US, na.rm = TRUE) / max(n_assets$Brazil, na.rm = TRUE)


ggplot(n_assets, aes(x = Data)) +
  geom_line(aes(y = US, color = "US"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = Brazil * scale_factor, color = "Brazil"), linewidth = 1) +
  scale_y_continuous(name = "Number of assets US market", sec.axis = sec_axis(~ . / scale_factor, name = "Number of assets Brazilian market")) +
  scale_x_continuous(breaks = c(0, 50, 100), labels = c("2014", "2018", "2022")) +  
  scale_color_manual(values = c("US" = "blue", "Brazil" = "red"), name = "Market: ") +
  labs(x = "") + 
  theme_minimal() + 
  theme(legend.position = "bottom")


