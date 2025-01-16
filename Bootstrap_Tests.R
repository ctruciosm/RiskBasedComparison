library(dplyr)
load("Results/Var/Var.RData")
load("Results/SharpeR/Sharpe.RData")

num_m <- 199
block <- 12
###############################
### Brazilian Stock Market  ###
###############################
Rp_ls_0 <- read.csv("Results/BR/Rport_120_ls_0_ibrx.csv")[, c(2, 1, 3:10)]

p_values_sd <- c()
p_values_sr <- c()
for (i in 2:10) {
  set.seed(i+123)
  p_values_sd[i - 1] <- boot.time.inference.log.var(Rp_ls_0[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr[i - 1] <- boot.time.inference(Rp_ls_0[, c(1, i)], b = block, M = num_m)$p.Value
}
names(p_values_sd) <- names(Rp_ls_0[-1])
names(p_values_sr) <- names(Rp_ls_0[-1])
p_values_sd
p_values_sr




Rp_ls_1 <- read.csv("Results/BR/Rport_120_ls_1_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_2 <- read.csv("Results/BR/Rport_120_ls_2_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_3 <- read.csv("Results/BR/Rport_120_ls_3_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_4 <- read.csv("Results/BR/Rport_120_ls_4_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_5 <- read.csv("Results/BR/Rport_120_ls_5_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_6 <- read.csv("Results/BR/Rport_120_ls_6_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_7 <- read.csv("Results/BR/Rport_120_ls_7_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_8 <- read.csv("Results/BR/Rport_120_ls_8_ibrx.csv")[, c(2, 1, 3:10)]
Rp_ls_9 <- read.csv("Results/BR/Rport_120_ls_9_ibrx.csv")[, c(2, 1, 3:10)]


Rp_MV <-   cbind(Rp_ls_0[, 1],  Rp_ls_1[, 1],  Rp_ls_2[, 1],  Rp_ls_3[, 1],  Rp_ls_4[, 1],  Rp_ls_5[, 1],  Rp_ls_6[, 1],  Rp_ls_7[, 1],  Rp_ls_8[, 1],  Rp_ls_9[, 1])
Rp_IV <-   cbind(Rp_ls_0[, 3],  Rp_ls_1[, 3],  Rp_ls_2[, 3],  Rp_ls_3[, 3],  Rp_ls_4[, 3],  Rp_ls_5[, 3],  Rp_ls_6[, 3],  Rp_ls_7[, 3],  Rp_ls_8[, 3],  Rp_ls_9[, 3])
Rp_RP <-   cbind(Rp_ls_0[, 4],  Rp_ls_1[, 4],  Rp_ls_2[, 4],  Rp_ls_3[, 4],  Rp_ls_4[, 4],  Rp_ls_5[, 4],  Rp_ls_6[, 4],  Rp_ls_7[, 4],  Rp_ls_8[, 4],  Rp_ls_9[, 4])
Rp_MD <-   cbind(Rp_ls_0[, 5],  Rp_ls_1[, 5],  Rp_ls_2[, 5],  Rp_ls_3[, 5],  Rp_ls_4[, 5],  Rp_ls_5[, 5],  Rp_ls_6[, 5],  Rp_ls_7[, 5],  Rp_ls_8[, 5],  Rp_ls_9[, 5])
Rp_MDE <-  cbind(Rp_ls_0[, 6],  Rp_ls_1[, 6],  Rp_ls_2[, 6],  Rp_ls_3[, 6],  Rp_ls_4[, 6],  Rp_ls_5[, 6],  Rp_ls_6[, 6],  Rp_ls_7[, 6],  Rp_ls_8[, 6],  Rp_ls_9[, 6])
Rp_HRP <-  cbind(Rp_ls_0[, 7],  Rp_ls_1[, 7],  Rp_ls_2[, 7],  Rp_ls_3[, 7],  Rp_ls_4[, 7],  Rp_ls_5[, 7],  Rp_ls_6[, 7],  Rp_ls_7[, 7],  Rp_ls_8[, 7],  Rp_ls_9[, 7])
Rp_HCAA <- cbind(Rp_ls_0[, 8],  Rp_ls_1[, 8],  Rp_ls_2[, 8],  Rp_ls_3[, 8],  Rp_ls_4[, 8],  Rp_ls_5[, 8],  Rp_ls_6[, 8],  Rp_ls_7[, 8],  Rp_ls_8[, 8],  Rp_ls_9[, 8])
Rp_HERC <- cbind(Rp_ls_0[, 9],  Rp_ls_1[, 9],  Rp_ls_2[, 9],  Rp_ls_3[, 9],  Rp_ls_4[, 9],  Rp_ls_5[, 9],  Rp_ls_6[, 9],  Rp_ls_7[, 9],  Rp_ls_8[, 9],  Rp_ls_9[, 9])
Rp_DHRP <- cbind(Rp_ls_0[, 10], Rp_ls_1[, 10], Rp_ls_2[, 10], Rp_ls_3[, 10], Rp_ls_4[, 10], Rp_ls_5[, 10], Rp_ls_6[, 10], Rp_ls_7[, 10], Rp_ls_8[, 10], Rp_ls_9[, 10])



p_values_sd_full <- matrix(NA, ncol = 9, nrow = 9)
p_values_sr_full <- matrix(NA, ncol = 9, nrow = 9)
for (i in 2:10) {
  set.seed(i)
  if (i %in% c(2, 3, 6, 7, 8, 9, 10)) {
    p_values_sd_full[i - 1, 2] <- boot.time.inference.log.var(Rp_IV[, c(1, i)], b = block, M = num_m)$p.Value
    p_values_sr_full[i - 1, 2] <- boot.time.inference(Rp_IV[, c(1, i)], b = block, M = num_m)$p.Value
  }
  
  p_values_sd_full[i - 1, 1] <- boot.time.inference.log.var(Rp_MV[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 3] <- boot.time.inference.log.var(Rp_RP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 4] <- boot.time.inference.log.var(Rp_MD[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 5] <- boot.time.inference.log.var(Rp_MDE[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 6] <- boot.time.inference.log.var(Rp_HRP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 7] <- boot.time.inference.log.var(Rp_HCAA[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 8] <- boot.time.inference.log.var(Rp_HERC[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 9] <- boot.time.inference.log.var(Rp_DHRP[, c(1, i)], b = block, M = num_m)$p.Value
  
  p_values_sr_full[i - 1, 1] <- boot.time.inference(Rp_MV[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 3] <- boot.time.inference(Rp_RP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 4] <- boot.time.inference(Rp_MD[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 5] <- boot.time.inference(Rp_MDE[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 6] <- boot.time.inference(Rp_HRP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 7] <- boot.time.inference(Rp_HCAA[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 8] <- boot.time.inference(Rp_HERC[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 9] <- boot.time.inference(Rp_DHRP[, c(1, i)], b = block, M = num_m)$p.Value
}
colnames(p_values_sd_full) <- colnames(p_values_sr_full) <- c("MV", "IV", "RP", "MD", "MDE", "HRP", "HCAA", "HERC", "DHRP")
row.names(p_values_sd_full) <- row.names(p_values_sr_full) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9")

p_values_sd_full
p_values_sr_full



###############################
### US Stock Market  ###
###############################
Rp_ls_0 <- read.csv("Results/US/Rport_120_ls_0_nasdaq.csv")[, c(2, 1, 3:10)]
Rp_ls_0_SR <- read.csv("Results/US/Rport_120_ls_0_nasdaq.csv")[, c(3, 1, 2, 4:10)]
num_m <- 199
block <- 12

p_values_sd <- c()
p_values_sr <- c()
for (i in 2:10) {
  set.seed(i+123)
  print(i)
  p_values_sd[i - 1] <- boot.time.inference.log.var(Rp_ls_0[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr[i - 1] <- boot.time.inference(Rp_ls_0_SR[, c(1, i)], b = block, M = num_m)$p.Value
}
names(p_values_sd) <- names(Rp_ls_0[-1])
names(p_values_sr) <- names(Rp_ls_0_SR[-1])
p_values_sd
p_values_sr



Rp_ls_1 <- read.csv("Results/US/Rport_120_ls_1_nasdaq.csv")[, c(2, 1, 3:10)]
Rp_ls_2 <- read.csv("Results/US/Rport_120_ls_2_nasdaq.csv")[, c(2, 1, 3:10)]
Rp_ls_3 <- read.csv("Results/US/Rport_120_ls_3_nasdaq.csv")[, c(2, 1, 3:10)]
Rp_ls_4 <- read.csv("Results/US/Rport_120_ls_4_nasdaq.csv")[, c(2, 1, 3:10)]
Rp_ls_6 <- read.csv("Results/US/Rport_120_ls_6_nasdaq.csv")[, c(2, 1, 3:10)]
Rp_ls_9 <- read.csv("Results/US/Rport_120_ls_9_nasdaq.csv")[, c(2, 1, 3:10)]


Rp_MV <-   cbind(Rp_ls_0[, 1],  Rp_ls_1[, 1],  Rp_ls_2[, 1],  Rp_ls_3[, 1],  Rp_ls_4[, 1],  Rp_ls_6[, 1],  Rp_ls_9[, 1])
Rp_IV <-   cbind(Rp_ls_0[, 3],  Rp_ls_1[, 3],  Rp_ls_2[, 3],  Rp_ls_3[, 3],  Rp_ls_4[, 3],  Rp_ls_6[, 3],  Rp_ls_9[, 3])
Rp_RP <-   cbind(Rp_ls_0[, 4],  Rp_ls_1[, 4],  Rp_ls_2[, 4],  Rp_ls_3[, 4],  Rp_ls_4[, 4],  Rp_ls_6[, 4],  Rp_ls_9[, 4])
Rp_MD <-   cbind(Rp_ls_0[, 5],  Rp_ls_1[, 5],  Rp_ls_2[, 5],  Rp_ls_3[, 5],  Rp_ls_4[, 5],  Rp_ls_6[, 5],  Rp_ls_9[, 5])
Rp_MDE <-  cbind(Rp_ls_0[, 6],  Rp_ls_1[, 6],  Rp_ls_2[, 6],  Rp_ls_3[, 6],  Rp_ls_4[, 6],  Rp_ls_6[, 6],  Rp_ls_9[, 6])
Rp_HRP <-  cbind(Rp_ls_0[, 7],  Rp_ls_1[, 7],  Rp_ls_2[, 7],  Rp_ls_3[, 7],  Rp_ls_4[, 7],  Rp_ls_6[, 7],  Rp_ls_9[, 7])
Rp_HCAA <- cbind(Rp_ls_0[, 8],  Rp_ls_1[, 8],  Rp_ls_2[, 8],  Rp_ls_3[, 8],  Rp_ls_4[, 8],  Rp_ls_6[, 8],  Rp_ls_9[, 8])
Rp_HERC <- cbind(Rp_ls_0[, 9],  Rp_ls_1[, 9],  Rp_ls_2[, 9],  Rp_ls_3[, 9],  Rp_ls_4[, 9],  Rp_ls_6[, 9],  Rp_ls_9[, 9])
Rp_DHRP <- cbind(Rp_ls_0[, 10], Rp_ls_1[, 10], Rp_ls_2[, 10], Rp_ls_3[, 10], Rp_ls_4[, 10], Rp_ls_6[, 10], Rp_ls_9[, 10])



p_values_sd_full <- matrix(NA, ncol = 9, nrow = 6)
p_values_sr_full <- matrix(NA, ncol = 9, nrow = 6)
for (i in 2:7) {
  set.seed(i)
  if (i %in% c(2, 3, 6, 7, 8, 9, 10)) {
    p_values_sd_full[i - 1, 2] <- boot.time.inference.log.var(Rp_IV[, c(1, i)], b = block, M = num_m)$p.Value
    p_values_sr_full[i - 1, 2] <- boot.time.inference(Rp_IV[, c(1, i)], b = block, M = num_m)$p.Value
  }
  
  p_values_sd_full[i - 1, 1] <- boot.time.inference.log.var(Rp_MV[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 3] <- boot.time.inference.log.var(Rp_RP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 4] <- boot.time.inference.log.var(Rp_MD[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 5] <- boot.time.inference.log.var(Rp_MDE[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 6] <- boot.time.inference.log.var(Rp_HRP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 7] <- boot.time.inference.log.var(Rp_HCAA[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 8] <- boot.time.inference.log.var(Rp_HERC[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sd_full[i - 1, 9] <- boot.time.inference.log.var(Rp_DHRP[, c(1, i)], b = block, M = num_m)$p.Value
  
  p_values_sr_full[i - 1, 1] <- boot.time.inference(Rp_MV[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 3] <- boot.time.inference(Rp_RP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 4] <- boot.time.inference(Rp_MD[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 5] <- boot.time.inference(Rp_MDE[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 6] <- boot.time.inference(Rp_HRP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 7] <- boot.time.inference(Rp_HCAA[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 8] <- boot.time.inference(Rp_HERC[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 9] <- boot.time.inference(Rp_DHRP[, c(1, i)], b = block, M = num_m)$p.Value
}
colnames(p_values_sd_full) <- colnames(p_values_sr_full) <- c("MV", "IV", "RP", "MD", "MDE", "HRP", "HCAA", "HERC", "DHRP")
row.names(p_values_sd_full) <- row.names(p_values_sr_full) <- c("S1", "S2", "S3", "S4", "S6", "S9")

p_values_sd_full
p_values_sr_full


Rp_all <- cbind(Rp_ls_0[, 2], Rp_MV, Rp_IV, Rp_RP, Rp_MD, Rp_MDE, Rp_HRP, Rp_HCAA, Rp_HERC, Rp_DHRP)

colnames(Rp_all) <- c("EW", 
  "MV-S0", "MV-S1", "MV-S2", "MV-S3", "MV-S4", "MV-S6", "MV-S9",
  "IV-S0", "IV-S1", "IV-S2", "IV-S3", "IV-S4", "IV-S6", "IV-S9",
  "RP-S0", "RP-S1", "RP-S2", "RP-S3", "RP-S4", "RP-S6", "RP-S9",
  "MD-S0", "MD-S1", "MD-S2", "MD-S3", "MD-S4", "MD-S6", "MD-S9",
  "MDE-S0", "MDE-S1", "MDE-S2", "MDE-S3", "MDE-S4", "MDE-S6", "MDE-S9",
  "HRP-S0", "HRP-S1", "HRP-S2", "HRP-S3", "HRP-S4", "HRP-S6", "HRP-S9",
  "HCAA-S0", "HCAA-S1", "HCAA-S2", "HCAA-S3", "HCAA-S4", "HCAA-S6", "HCAA-S9",
  "HERC-S0", "HERC-S1", "HERC-S2", "HERC-S3", "HERC-S4", "HERC-S6", "HERC-S9",
  "DHRP-S0", "DHRP-S1", "DHRP-S2", "DHRP-S3", "DHRP-S4", "DHRP-S6", "DHRP-S9")

Rp_all <- Rp_all[, c(40, 1:39, 41:64)]

num_m <- 199
block <- 12

p_values_sr_all <- c()
for (i in 2:64) {
  set.seed(i+123)
  print(i)
  p_values_sr_all[i - 1] <- boot.time.inference(Rp_all[, c(1, i)], b = block, M = num_m)$p.Value
}
names(p_values_sr_all) <- names(Rp_all[-1])
p_values_sr_all

colnames(Rp_all)[which(p_values_sr_all <= 0.1)]
