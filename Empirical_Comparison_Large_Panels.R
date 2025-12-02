#################################
###   Empirical Comparison    ###
#################################
rm(list = ls())
library(dplyr)
library(stringr)
library(tidyr)
library(RiskPortfolios)
library(HierPortfolios)
library(nlshrink)
library(cvCovEst)
library(ShrinkCovMat)
library(readxl)
library(readr)
library(kableExtra)
library(lmf)
library(matrixcalc)
library(moments)
source("utils.R")
select <- dplyr::select

shrinkage_names <- list.files(path = 'covShrinkage', pattern = '*.R', full.names = T)
for (name in shrinkage_names) source(name)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#            Data              #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

market <- "US" # B3

if (market == "US") {
  #assets_active <- read_excel("./Data/daily_prices_nyse_active.xlsx", skip = 3, na = c("-", "NA"), col_types = c("date", rep("numeric", 1372)))
  #assets_delisted <- read_excel("./Data/daily_prices_nyse_delisted.xlsx", skip = 3, na = c("-", "NA"), col_types = c("date", rep("numeric", 1011)))[, -2]
  #assets <- left_join(assets_active, assets_delisted, by = "Data")
  assets <- suppressWarnings(read_csv("./Data/daily_prices_nyse_stooq.csv", col_types = c("D", rep("d", 3515)))) |> rename(Data = DATA)
} else {
  assets <- read_excel("./Data/daily_prices_b3_all.xlsx", skip = 3, na = c("-", "NA"), col_types = c("date", rep("numeric", 1410)))
}

stocks <- assets |> 
  mutate(Data = lubridate::ymd(Data)) |> 
  filter(Data >= '2011-06-01') |> 
  filter(Data <= '2025-05-31') |> 
  rename_with(~ str_replace(.x, "PU\najust p/ prov\nEm moeda orig\n", "")) |> 
  filter(!if_all(where(is.numeric), is.na)) |> 
  mutate_if(is.numeric, ~ (./lag(.) - 1)) |> 
  filter(!if_all(where(is.numeric), is.na))
    

dim(stocks)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#       General Settings       #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
InS <- 252 * 3
trading_month <- 21
n_tot <- nrow(stocks)
OoS <- n_tot  - InS# - trading_month
p <- ncol(stocks) - 1
nmethods <- 10
cov_shrinkages <- c(stats::cov, cov1Para, cov2Para, covCor, covDiag, cov_nlshrink, nlShrinkLWEst, qis, cvc_shrinkage)

for (s in 1:9) {
  cov_estim = cov_shrinkages[s][[1]]
  w_ew_full = w_mv_full = w_iv_full = w_rp_full = w_md_full = w_mde_full = w_hrp_full = w_hcaa_full = w_herc_full = w_dhrp_full = matrix(NA, nrow = OoS, ncol = p, dimnames = list(NULL, colnames(stocks)[-1]))
  Rport <- matrix(NA, nrow = OoS, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "chrp")))
  to <- matrix(NA, nrow = OoS - 1, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "chrp")))
  sspw <- matrix(NA, nrow = OoS, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "chrp")))
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #    Out-of-sample exercise    #  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  r_oos_full <- matrix(NA, ncol = p, nrow = 1, dimnames = list(NULL, colnames(stocks)[-1]))
  n_assets_portfolio <- c()
  j_month <- 0
  i <- 1
  aux <- NULL
  while (InS + i <= n_tot) { 
    print(paste("Shrinkage method", s, "iteration", i, "of", OoS))
    if ((i - 1) %% 21 == 0) {
      j_month <- j_month + 1
      pre_aux <- aux
      window_stocks <- stocks[i: min(InS - 1 + i + trading_month, n_tot), -1]
      window_stocks <- window_stocks[, which(sapply(window_stocks[ -c(1 : InS), ], function(y) sum(length(which(is.na(y))))) == 0)] # Full Future trading Days
      window_stocks <- window_stocks[, which(sapply(window_stocks[1 : InS, ], function(y) sum(length(which(is.na(y)))) / length(y))  == 0)] # Full trading history
      window_stocks <- window_stocks[1:InS, ]
      #window_stocks[is.na(window_stocks)] <- 0
      aux <- colnames(window_stocks)
      n_assets <-  ncol(window_stocks)
      n_assets_portfolio[i] <- n_assets
      r_oos_full[1, aux] <- stocks[InS + i, ] |> select(all_of(aux)) |> as.matrix() |> as.numeric()

      cova <- cov_estim(window_stocks)
      cova <- (cova + t(cova))/2
      set.seed(i)
      w_ew <- rep(1/n_assets, n_assets)
      w_hrp <- HRP_Portfolio(cova)$weights
      aux_hcaa <- HCAA_Portfolio(cova)
      w_hcaa <- aux_hcaa[[1]]$weights
      w_herc <- HERC_Portfolio(cova, clusters = aux_hcaa[[2]])[[1]]$weights
      w_dhrp <- DHRP_Portfolio(cova)$weights
      if(is.symmetric.matrix(cova) && is.positive.definite(cova)) {
        w_mv <- optimalPortfolio(Sigma = cova, control = list(type = 'minvol', constraint = 'lo'))
        w_mde <-optimalPortfolio(Sigma = cova, control = list(type = 'maxdec', constraint = 'lo'))
        w_iv <- optimalPortfolio(Sigma = cova, control = list(type = 'invvol', constraint = 'lo'))
        w_rp <- optimalPortfolio(Sigma = cova, control = list(type = 'erc', constraint = 'lo'))
        w_md <- optimalPortfolio(Sigma = cova, control = list(type = 'maxdiv', constraint = 'lo'))
      } else {
        aux_cova <- nearPD(cova)
        w_mv <- optimalPortfolio(Sigma = aux_cova, control = list(type = 'minvol', constraint = 'lo'))
        w_mde <-optimalPortfolio(Sigma = aux_cova, control = list(type = 'maxdec', constraint = 'lo'))
        w_iv <- optimalPortfolio(Sigma = aux_cova, control = list(type = 'invvol', constraint = 'lo'))
        w_rp <- optimalPortfolio(Sigma = aux_cova, control = list(type = 'erc', constraint = 'lo'))
        w_md <- optimalPortfolio(Sigma = aux_cova, control = list(type = 'maxdiv', constraint = 'lo'))
      }
      w <- rbind(w_ew, w_mv, w_iv, w_rp,  w_md, w_mde, w_hrp, w_hcaa, w_herc, w_dhrp)
      
      w_ew_full[j_month, aux] <- w_ew
      w_mv_full[j_month, aux] <- w_mv
      w_iv_full[j_month, aux] <- w_iv
      w_rp_full[j_month, aux] <- w_rp
      w_md_full[j_month, aux] <- w_md
      w_mde_full[j_month, aux] <- w_mde
      w_hrp_full[j_month, aux] <- w_hrp
      w_hcaa_full[j_month, aux] <- w_hcaa
      w_herc_full[j_month, aux] <- w_herc
      w_dhrp_full[j_month, aux] <- w_dhrp
      
      if (j_month > 1) {
        last_returns <- matrix(NA, nrow = 1, ncol = p, dimnames = list(NULL, colnames(stocks)[-1]))
        last_returns[, pre_aux] <- stocks[(InS + i - 21) : (InS + i - 1), ] |> select(all_of(pre_aux)) |> as.matrix() |> apply(2, sum) |> as.numeric()
        
        to[j_month - 1, ] <- c(calculate_to(w_ew_full[j_month - 1, ], w_ew_full[j_month, ], last_returns, p),
          calculate_to(w_mv_full[j_month - 1, ], w_mv_full[j_month, ], last_returns, p),
          calculate_to(w_iv_full[j_month - 1, ], w_iv_full[j_month, ], last_returns, p),
          calculate_to(w_rp_full[j_month - 1, ], w_rp_full[j_month, ], last_returns, p),
          calculate_to(w_md_full[j_month - 1, ], w_md_full[j_month, ], last_returns, p),
          calculate_to(w_mde_full[j_month - 1, ], w_mde_full[j_month, ], last_returns, p),
          calculate_to(w_hrp_full[j_month - 1, ], w_hrp_full[j_month, ], last_returns, p),
          calculate_to(w_hcaa_full[j_month - 1, ], w_hcaa_full[j_month, ], last_returns, p),
          calculate_to(w_herc_full[j_month - 1, ], w_herc_full[j_month, ], last_returns, p),
          calculate_to(w_dhrp_full[j_month - 1, ], w_dhrp_full[j_month, ], last_returns, p))
      }   
      sspw[j_month, ] <- apply(w^2, 1, sum)

    } else {
      returns_end_day <- stocks[InS + i - 1, ] |> select(all_of(aux)) |> as.matrix() |> as.numeric()
      for (h in 1:nmethods) {
        num <- w[h, ] *(1 + returns_end_day / 100) 
        den <- sum(num, na.rm = TRUE)
        w[h, ] <- as.numeric(num/den)
      }
      r_oos_full[1, aux] <- stocks[InS + i, ] |> select(all_of(aux)) |> as.matrix() |> as.numeric()
    }
    Rport[i,] <- w %*% r_oos_full[1, aux]
    i <- i + 1
  }
  
  write.table(Rport, paste0("Results/Rport_", InS, "_ls_", s - 1, "_", market, ".csv"), sep = ",")
  write.table(n_assets_portfolio, paste0("Results/n_assets_portfolio", InS, "_ls_", s - 1, "_", market, ".csv"), sep = ",")
  
  Caption <- "Out-of-sample performance measures of the minimum variance portfolio with short-selling constraints: AV, SD, SR, ASR, SO, TO and SSPW stand for the average, standard deviation, Sharpe ratio, Adjusted Sharpe ratio, Sortino ratio, average turnover and average sum of squared portfolio weights, respectively."
  oos_results <- rbind(apply(Rport[, 1:nmethods], 2, medidas),
                       apply(to[, 1:nmethods], 2, mean, na.rm = TRUE),
                       apply(sspw[, 1:nmethods], 2, mean, na.rm = TRUE))
  row.names(oos_results) <- c("AV", "SD", "SR", "ASR", "SO", "TO", "SSPW")
  colnames(oos_results) <- c("EW", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "chrp")
  t(oos_results) %>%
    knitr::kable(digits = 4, format = "latex", align = "lccccccc", caption = Caption,
                 table.envir = "table", label = "empirical_mvp") %>%
    save_kable(keep_tex = T, file = paste0("Results/risk-based_", InS, "_ls_", s - 1, "_", market, ".tex"))

}
  
  
