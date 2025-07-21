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
library(kableExtra)
library(lmf)
library(lubridate)
library(matrixcalc)
source("utils.R")


shrinkage_names <- list.files(path = 'covShrinkage', pattern = '*.R', full.names = T)
for (name in shrinkage_names) source(name)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#            Data              #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
composition <- read_excel("Data/composicao_ibrx.xlsx")[, -c(1, 2)] |> 
  pivot_longer(cols = `dez-97`:`jul-25`, names_to = "Date", values_to = "valores") |> 
  mutate(Date = str_replace(Date, "jan", "01"), 
    Date = str_replace(Date, "fev", "02"),
    Date = str_replace(Date, "mar", "03"), 
    Date = str_replace(Date, "abr", "04"),
    Date = str_replace(Date, "mai", "05"), 
    Date = str_replace(Date, "jun", "06"),
    Date = str_replace(Date, "jul", "07"), 
    Date = str_replace(Date, "ago", "08"),
    Date = str_replace(Date, "set", "09"), 
    Date = str_replace(Date, "out", "10"),
    Date = str_replace(Date, "nov", "11"), 
    Date = str_replace(Date, "dez", "12")) |> 
  mutate(dates = lubridate::my(Date)) |> 
  select(-Date) |> 
  select(dates, everything()) |> 
  pivot_wider(names_from = Code, values_from = valores)

stocks <- read_excel("Data/retornos_mensal_b3.xlsx", skip = 3, na = c("-", "NA")) |> 
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
  filter(dates <= '2025-06-01')
colnames(stocks) <- str_replace(colnames(stocks), "Retorno\ndo fechamento\nem 1 meses\nEm moeda orig\najust p/ prov\n", "")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#       General Settings       #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
InS <- 120.     # Dont touch
last_obs <- 252 # 60 or 120 or 252
OoS <- nrow(stocks) - InS
p <- ncol(stocks) - 1
nmethods <- 10



if (last_obs == 252) {
  assets_daily <- read_excel("./Data/retornos_diarios_b3.xlsx", skip = 3, na = c("-", "NA"), col_types = c("date", rep("numeric", 1410)))
  colnames(assets_daily) <- str_replace(colnames(assets_daily), "Retorno\ndo fechamento\nem 1 dias\nEm moeda orig\najust p/ prov\n", "")
  assets_daily$Data <- lubridate::ymd(assets_daily$Data)
}

cov_shrinkages <- c(stats::cov, cov1Para, cov2Para, covCor, covDiag, nlshrink_cov, nlShrinkLWEst, gis, lis, qis, cvc_shrinkage)

for (s in c(1:11)) {
  cov_estim = cov_shrinkages[s][[1]]
  w_ew_full = w_mv_full = w_iv_full = w_rp_full = w_md_full = w_mde_full = w_hrp_full = w_hcaa_full = w_herc_full = w_dhrp_full = matrix(0, nrow = OoS, ncol = p, dimnames = list(NULL, colnames(stocks)[-1]))
  Rport <- matrix(NA, nrow = OoS, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")))
  to <- matrix(0, nrow = OoS - 1, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")))
  sspw <- matrix(0, nrow = OoS, ncol = nmethods, dimnames = list(NULL, c("ew", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")))
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #    Out-of-sample exercise    #  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  n_assets_portfolio <- rep(0, OoS)
  for (i in 1:OoS) {
    print(paste("Shrinkage method", s, "iteration", i, "of", OoS))
    date_ins <- stocks[i:(InS - 1 + i), ]$dates
    aux_1 <- composition[i:(InS - 1 + i), ] |> filter(dates <= date_ins[InS]) |> tail(1) |> select(-dates) |> is.na() |> apply(2, sum) 
    aux_2 <- stocks[i:(InS - 1 + i), ] |> select(-dates) |> is.na() |> apply(2, sum) 
    aux <- intersect(names(which(aux_1 == 0)), names(which(aux_2 == 0)))
    retu_ins <- stocks[i:(InS - 1 + i), ] |> select(all_of(aux)) |> as.matrix()
    
    if (last_obs == 252) {
      daily_returns <- assets_daily |> select(Data, all_of(all_of(aux))) |> dplyr::filter(Data < stocks[InS + i, ]$dates) |> filter(!if_all(where(is.numeric), is.na)) 
      retu_ins <-  daily_returns |> tail(252) |> select(all_of(aux))
      retu_ins[is.na(retu_ins)] <- 0 # Illiquidity
    }
    
    r_oos <- stocks[InS + i, ] |> select(all_of(aux)) |> as.matrix()
    r_oos_full <- matrix(NA, ncol = p, nrow = 1, dimnames = list(NULL, colnames(stocks)[-1]))
    r_oos_full[1, aux] <- r_oos
    n_assets <-  ncol(retu_ins)
    n_assets_portfolio[i] <- n_assets
    
    retu_ins <- as.matrix(tail(retu_ins, last_obs))
    
    cova <- cov_estim(retu_ins)
    set.seed(i)
    w_ew <- rep(1/n_assets, n_assets)
    w_hrp <- HRP_Portfolio(cova)$weights
    w_hcaa <- HCAA_Portfolio(cova)$weights
    w_herc <- HERC_Portfolio(cova)$weights
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
  
  write.table(Rport, paste0("Results/Rport_",  InS, "_last_", last_obs, "_ls_", s - 1, "_ibrx.csv"), sep = ",")
  write.table(n_assets_portfolio, paste0("Results/n_assets_portfolio",  InS, "_last_", last_obs, "_ls_", s - 1, "_ibrx.csv"), sep = ",")
  
  Caption <- "Out-of-sample performance measures of the minimum variance portfolio with short-selling constraints: AV, SD, SR, ASR, SO, TO and SSPW stand for the average, standard deviation, Sharpe ratio, Adjusted Sharpe ratio, Sortino ratio, average turnover and average sum of squared portfolio weights, respectively."
  oos_results <- rbind(apply(Rport[, 1:nmethods], 2, medidas),
                       apply(to[, 1:nmethods], 2, mean),
                       apply(sspw[, 1:nmethods], 2, mean))
  row.names(oos_results) <- c("AV", "SD", "SR", "ASR", "SO", "TO", "SSPW")
  colnames(oos_results) <- c("EW", "mv", "iv", "rp", "md", "mde", "hrp", "hcaa", "herc", "dhrp")
  t(oos_results) %>%
    knitr::kable(digits = 4, format = "latex", align = "lccccccc", caption = Caption,
                 table.envir = "table", label = "empirical_mvp") %>%
    save_kable(keep_tex = T, file = paste0("Results/risk-based_", InS, "_last_", last_obs, "_ls_", s - 1, "_ibrx.tex"))
}


