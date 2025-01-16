#################################
###   Empirical Comparison    ###
#################################
rm(list = ls())
library(dplyr)
library(stringr)
library(tidyr)
library(RiskPortfolios)
library(HierPortfolios)
library(readxl)
library(nlshrink)
library(cvCovEst)
library(ShrinkCovMat)
library(readr)
library(kableExtra)
library(lmf)
source("utils.R")


shrinkage_names <- list.files(path = 'covShrinkage', pattern = '*.R', full.names = T)
for (name in shrinkage_names) source(name)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#            Data              #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
nasdaq_active <- read_excel("./Data/nasdaq_mensal_ativos.xlsx", skip = 3, na = c("-", "NA"))
nasdaq_delisted <- read_excel("./Data/nasdaq_mensal_inativos.xlsx", skip = 3, na = c("-", "NA"))[, -1]
nasdaq <- cbind(nasdaq_active, nasdaq_delisted)


stocks <- nasdaq |> 
  mutate(Data = str_replace(Data, "Jan", "01"), 
         Data = str_replace(Data, "Fev", "02"),
         Data = str_replace(Data, "Mar", "03"), 
         Data = str_replace(Data, "Abr", "04"),
         Data = str_replace(Data, "Mai", "05"), 
         Data = str_replace(Data, "Jun", "06"),
         Data = str_replace(Data, "Jul", "07"), 
         Data = str_replace(Data, "Ago", "08"),
         Data = str_replace(Data, "Set", "09"), 
         Data = str_replace(Data, "Out", "10"),
         Data = str_replace(Data, "Nov", "11"), 
         Data = str_replace(Data, "Dez", "12")) |> 
  mutate(dates = lubridate::my(Data)) |> 
  select(-Data) |> 
  select(dates, everything()) |> 
  filter(dates >= '1997-12-01') |> 
  filter(dates <= '2023-12-01')
colnames(stocks) <- str_replace(colnames(stocks), "Retorno\ndo fechamento\nem 1 meses\nEm moeda orig\najust p/ prov\n", "")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#       General Settings       #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
InS <- 120
OoS <- nrow(stocks) - InS
p <- ncol(stocks) - 1
nmethods <- 10

cov_shrinkages <- c(stats::cov, cov1Para, cov2Para, covCor, covDiag, nlshrink_cov, nlShrinkLWEst, gis, lis, qis)

for (s in 6) {
  cov_estim = cov_shrinkages[s][[1]]
  w_ew_full = w_mv_full = w_iv_full = w_rp_full = w_md_full = w_mde_full = w_hrp_full = w_hcaa_full = w_herc_full = w_dhrp_full = matrix(0, nrow = OoS, ncol = p, dimnames = list(NULL, colnames(stocks)[-1]))
  Rport <- matrix(NA, nrow = OoS, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")))
  to <- matrix(0, nrow = OoS - 1, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")))
  sspw <- matrix(0, nrow = OoS, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")))
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #    Out-of-sample exercise    #  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  r_oos_full <- matrix(NA, ncol = p, nrow = 1, dimnames = list(NULL, colnames(stocks)[-1]))
  
  for (i in 1:OoS) { 
    print(i)
    window_stocks <- stocks[i:(InS - 1 + i + 1), -1]
    window_stocks <- window_stocks[, which(sapply(window_stocks, function(y) sum(length(which(is.na(y))))) == 0)]
    
    n_assets <- ncol(window_stocks)
    r_oos <- tail(window_stocks, 1)
    aux <- colnames(window_stocks)
    r_oos_full[1, aux] <- as.numeric(r_oos)
    
    cova <- cov_estim(window_stocks[1:InS, ])
    set.seed(i)
    w_ew <- rep(1/n_assets, n_assets)
    w_hrp <- HRP_Portfolio(cova)$weights
    w_hcaa <- HCAA_Portfolio(cova)$weights
    w_herc <- HERC_Portfolio(cova)$weights
    w_dhrp <- DHRP_Portfolio(cova)$weights
    if(det(cova) > 0) {
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
    Rport[i,] <- w %*% t(r_oos)
    sspw[i, ] <- apply(w^2, 1, sum)
    
    
    w_ew_full[i, aux] <- w_ew
    w_mv_full[i, aux] <- w_mv
    w_iv_full[i, aux] <- w_iv
    w_rp_full[i, aux] <- w_rp
    w_md_full[i, aux] <- w_md
    w_mde_full[i, aux] <- w_mde
    w_hrp_full[i, aux] <- w_hrp
    w_hcaa_full[i, aux] <- w_hcaa
    w_herc_full[i, aux] <- w_herc
    w_dhrp_full[i, aux] <- w_dhrp

    
    if (i > 2) {
      to[i - 1, ] <- c(calculate_to(w_ew_full[i - 1, ], w_ew_full[i, ], r_oos_full, p),
                       calculate_to(w_mv_full[i - 1, ], w_mv_full[i, ], r_oos_full, p),
                       calculate_to(w_iv_full[i - 1, ], w_iv_full[i, ], r_oos_full, p),
                       calculate_to(w_rp_full[i - 1, ], w_rp_full[i, ], r_oos_full, p),
                       calculate_to(w_md_full[i - 1, ], w_md_full[i, ], r_oos_full, p),
                       calculate_to(w_mde_full[i - 1, ], w_mde_full[i, ], r_oos_full, p),
                       calculate_to(w_hrp_full[i - 1, ], w_hrp_full[i, ], r_oos_full, p),
                       calculate_to(w_hcaa_full[i - 1, ], w_hcaa_full[i, ], r_oos_full, p),
                       calculate_to(w_herc_full[i - 1, ], w_herc_full[i, ], r_oos_full, p),
                       calculate_to(w_dhrp_full[i - 1, ], w_dhrp_full[i, ], r_oos_full, p))
    }   
  }
  
  write.table(Rport, paste0("Results/Rport_", InS, "_ls_", s - 1, "_nasdaq.csv"), sep = ",")
  
  Caption <- "Out-of-sample performance measures of the minimum variance portfolio with short-selling constraints: AV, SD, SR, ASR, SO, TO and SSPW stand for the average, standard deviation, Sharpe ratio, Adjusted Sharpe ratio, Sortino ratio, average turnover and average sum of squared portfolio weights, respectively."
  oos_results <- rbind(apply(Rport[, 1:nmethods], 2, medidas),
                       apply(to[, 1:nmethods], 2, mean),
                       apply(sspw[, 1:nmethods], 2, mean))
  row.names(oos_results) <- c("AV", "SD", "SR", "ASR", "SO", "TO", "SSPW")
  colnames(oos_results) <- c("EW", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")
  t(oos_results) %>%
    knitr::kable(digits = 4, format = "latex", align = "lccccccc", caption = Caption,
                 table.envir = "table", label = "empirical_mvp") %>%
    save_kable(keep_tex = T, file = paste0("Results/risk-based_", InS, "_ls_", s - 1, "_nasdaq.tex"))
}
  
  
