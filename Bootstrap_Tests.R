library(dplyr)
load("Results/Var/Var.RData")
load("Results/SharpeR/Sharpe.RData")
source("Results/SharpeR/block.size.calibrate_.R")


################################################
###                Table 1  (Brazil)         ###
################################################

num_m <- 199
block <- 21
Rp_ls_0 <- read.csv("Results/Rport_756_ls_0_B3.csv")[, c(2, 1, 3:10)]

p_values_sd <- c()
p_values_sr <- c()
for (i in 2:ncol(Rp_ls_0)) {
  set.seed(i + 123)
  print(colnames(Rp_ls_0[, c(1, j)]))
  p_values_sd[i - 1] <- boot.time.inference.log.var(Rp_ls_0[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr[i - 1] <- boot.time.inference(Rp_ls_0[, c(1, i)], b = block, M = num_m)$p.Value
}
names(p_values_sd) <- names(Rp_ls_0[-1])
names(p_values_sr) <- names(Rp_ls_0[-1])
p_values_sd
p_values_sr


################################################
###                Table 3  (US)         ###
################################################

num_m <- 199
block <- 21
Rp_ls_0 <- read.csv("Results/Rport_756_ls_0_US.csv")[, c(2, 1, 3:10)]
p_values_sd <- c()
for (i in 2:ncol(Rp_ls_0)) {
  set.seed(i + 123)
  print(colnames(Rp_ls_0[, c(1, i)]))
  p_values_sd[i - 1] <- boot.time.inference.log.var(Rp_ls_0[, c(1, i)], b = block, M = num_m)$p.Value
}
names(p_values_sd) <- names(Rp_ls_0[-1])
p_values_sd


num_m <- 199
block <- 21
Rp_ls_0 <- read.csv("Results/Rport_756_ls_0_US.csv")[, c(6, 1:5, 7:10)]
p_values_sr <- c()
for (i in 2:ncol(Rp_ls_0)) {
  set.seed(i + 123)
  print(colnames(Rp_ls_0[, c(1, i)]))
  p_values_sr[i - 1] <- boot.time.inference(Rp_ls_0[, c(1, i)], b = block, M = num_m)$p.Value
}
names(p_values_sr) <- names(Rp_ls_0[-1])
p_values_sr


################################################
###             Table 2 (Brazil)             ###
################################################

Rp_ls_0 <- read.csv("Results/Rport_756_ls_0_B3.csv")
Rp_ls_1 <- read.csv("Results/Rport_756_ls_1_B3.csv")
Rp_ls_2 <- read.csv("Results/Rport_756_ls_2_B3.csv")
Rp_ls_3 <- read.csv("Results/Rport_756_ls_3_B3.csv")
Rp_ls_4 <- read.csv("Results/Rport_756_ls_4_B3.csv")
Rp_ls_5 <- read.csv("Results/Rport_756_ls_5_B3.csv")
Rp_ls_6 <- read.csv("Results/Rport_756_ls_6_B3.csv")
Rp_ls_7 <- read.csv("Results/Rport_756_ls_7_B3.csv")
Rp_ls_8 <- read.csv("Results/Rport_756_ls_8_B3.csv")


Rp_MV <-   cbind(Rp_ls_0[, 2],  Rp_ls_1[, 2],  Rp_ls_2[, 2],  Rp_ls_3[, 2],  Rp_ls_4[, 2],  Rp_ls_5[, 2],  Rp_ls_6[, 2],  Rp_ls_7[, 2], Rp_ls_8[, 2])
Rp_IV <-   cbind(Rp_ls_0[, 3],  Rp_ls_1[, 3],  Rp_ls_2[, 3],  Rp_ls_3[, 3],  Rp_ls_4[, 3],  Rp_ls_5[, 3],  Rp_ls_6[, 3],  Rp_ls_7[, 3], Rp_ls_8[, 3])
Rp_RP <-   cbind(Rp_ls_0[, 4],  Rp_ls_1[, 4],  Rp_ls_2[, 4],  Rp_ls_3[, 4],  Rp_ls_4[, 4],  Rp_ls_5[, 4],  Rp_ls_6[, 4],  Rp_ls_7[, 4], Rp_ls_8[, 4])
Rp_MD <-   cbind(Rp_ls_0[, 5],  Rp_ls_1[, 5],  Rp_ls_2[, 5],  Rp_ls_3[, 5],  Rp_ls_4[, 5],  Rp_ls_5[, 5],  Rp_ls_6[, 5],  Rp_ls_7[, 5], Rp_ls_8[, 5])
Rp_MDE <-  cbind(Rp_ls_0[, 6],  Rp_ls_1[, 6],  Rp_ls_2[, 6],  Rp_ls_3[, 6],  Rp_ls_4[, 6],  Rp_ls_5[, 6],  Rp_ls_6[, 6],  Rp_ls_7[, 6], Rp_ls_8[, 6])
Rp_HRP <-  cbind(Rp_ls_0[, 7],  Rp_ls_1[, 7],  Rp_ls_2[, 7],  Rp_ls_3[, 7],  Rp_ls_4[, 7],  Rp_ls_5[, 7],  Rp_ls_6[, 7],  Rp_ls_7[, 7], Rp_ls_8[, 7])
Rp_HCAA <- cbind(Rp_ls_0[, 8],  Rp_ls_1[, 8],  Rp_ls_2[, 8],  Rp_ls_3[, 8],  Rp_ls_4[, 8],  Rp_ls_5[, 8],  Rp_ls_6[, 8],  Rp_ls_7[, 8], Rp_ls_8[, 8])
Rp_HERC <- cbind(Rp_ls_0[, 9],  Rp_ls_1[, 9],  Rp_ls_2[, 9],  Rp_ls_3[, 9],  Rp_ls_4[, 9],  Rp_ls_5[, 9],  Rp_ls_6[, 9],  Rp_ls_7[, 9], Rp_ls_8[, 8])
Rp_CHRP <- cbind(Rp_ls_0[, 10],  Rp_ls_1[, 10],  Rp_ls_2[, 10],  Rp_ls_3[, 10],  Rp_ls_4[, 10],  Rp_ls_5[, 10],  Rp_ls_6[, 10],  Rp_ls_7[, 10], Rp_ls_8[, 10])


num_m <- 199
block <- 21
p_values_sd_full <- matrix(NA, ncol = 9, nrow = 8)
p_values_sr_full <- matrix(NA, ncol = 9, nrow = 8)
for (i in 2:9) {
  set.seed(i + 123)
  
  if (i %in% c(2, 3, 6, 7, 8, 9)) {
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
  p_values_sd_full[i - 1, 9] <- boot.time.inference.log.var(Rp_CHRP[, c(1, i)], b = block, M = num_m)$p.Value
  
  p_values_sr_full[i - 1, 1] <- boot.time.inference(Rp_MV[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 3] <- boot.time.inference(Rp_RP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 4] <- boot.time.inference(Rp_MD[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 5] <- boot.time.inference(Rp_MDE[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 6] <- boot.time.inference(Rp_HRP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 7] <- boot.time.inference(Rp_HCAA[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 8] <- boot.time.inference(Rp_HERC[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 9] <- boot.time.inference(Rp_CHRP[, c(1, i)], b = block, M = num_m)$p.Value
}
colnames(p_values_sd_full) <- colnames(p_values_sr_full) <- c("MV", "IV", "RP", "MD", "MDE", "HRP", "HCAA", "HERC", "CHRP")
row.names(p_values_sd_full) <- row.names(p_values_sr_full) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8")

p_values_sd_full
p_values_sr_full


################################################
###               Table 4 (US)               ###
################################################

Rp_ls_0 <- read.csv("Results/Rport_756_ls_0_US.csv")
Rp_ls_1 <- read.csv("Results/Rport_756_ls_1_US.csv")
Rp_ls_2 <- read.csv("Results/Rport_756_ls_2_US.csv")
Rp_ls_3 <- read.csv("Results/Rport_756_ls_3_US.csv")
Rp_ls_4 <- read.csv("Results/Rport_756_ls_4_US.csv")
Rp_ls_5 <- read.csv("Results/Rport_756_ls_5_US.csv")
Rp_ls_6 <- read.csv("Results/Rport_756_ls_6_US.csv")
Rp_ls_7 <- read.csv("Results/Rport_756_ls_7_US.csv")
Rp_ls_8 <- read.csv("Results/Rport_756_ls_8_US.csv")


Rp_MV <-   cbind(Rp_ls_0[, 2],  Rp_ls_1[, 2],  Rp_ls_2[, 2],  Rp_ls_3[, 2],  Rp_ls_4[, 2],  Rp_ls_5[, 2],  Rp_ls_6[, 2],  Rp_ls_7[, 2], Rp_ls_8[, 2])
Rp_IV <-   cbind(Rp_ls_0[, 3],  Rp_ls_1[, 3],  Rp_ls_2[, 3],  Rp_ls_3[, 3],  Rp_ls_4[, 3],  Rp_ls_5[, 3],  Rp_ls_6[, 3],  Rp_ls_7[, 3], Rp_ls_8[, 3])
Rp_RP <-   cbind(Rp_ls_0[, 4],  Rp_ls_1[, 4],  Rp_ls_2[, 4],  Rp_ls_3[, 4],  Rp_ls_4[, 4],  Rp_ls_5[, 4],  Rp_ls_6[, 4],  Rp_ls_7[, 4], Rp_ls_8[, 4])
Rp_MD <-   cbind(Rp_ls_0[, 5],  Rp_ls_1[, 5],  Rp_ls_2[, 5],  Rp_ls_3[, 5],  Rp_ls_4[, 5],  Rp_ls_5[, 5],  Rp_ls_6[, 5],  Rp_ls_7[, 5], Rp_ls_8[, 5])
Rp_MDE <-  cbind(Rp_ls_0[, 6],  Rp_ls_1[, 6],  Rp_ls_2[, 6],  Rp_ls_3[, 6],  Rp_ls_4[, 6],  Rp_ls_5[, 6],  Rp_ls_6[, 6],  Rp_ls_7[, 6], Rp_ls_8[, 6])
Rp_HRP <-  cbind(Rp_ls_0[, 7],  Rp_ls_1[, 7],  Rp_ls_2[, 7],  Rp_ls_3[, 7],  Rp_ls_4[, 7],  Rp_ls_5[, 7],  Rp_ls_6[, 7],  Rp_ls_7[, 7], Rp_ls_8[, 7])
Rp_HCAA <- cbind(Rp_ls_0[, 8],  Rp_ls_1[, 8],  Rp_ls_2[, 8],  Rp_ls_3[, 8],  Rp_ls_4[, 8],  Rp_ls_5[, 8],  Rp_ls_6[, 8],  Rp_ls_7[, 8], Rp_ls_8[, 8])
Rp_HERC <- cbind(Rp_ls_0[, 9],  Rp_ls_1[, 9],  Rp_ls_2[, 9],  Rp_ls_3[, 9],  Rp_ls_4[, 9],  Rp_ls_5[, 9],  Rp_ls_6[, 9],  Rp_ls_7[, 9], Rp_ls_8[, 8])
Rp_CHRP <- cbind(Rp_ls_0[, 10],  Rp_ls_1[, 10],  Rp_ls_2[, 10],  Rp_ls_3[, 10],  Rp_ls_4[, 10],  Rp_ls_5[, 10],  Rp_ls_6[, 10],  Rp_ls_7[, 10], Rp_ls_8[, 10])


num_m <- 199
block <- 21
p_values_sd_full <- matrix(NA, ncol = 9, nrow = 8)
p_values_sr_full <- matrix(NA, ncol = 9, nrow = 8)
for (i in 2:9) {
  set.seed(i + 123)
  
  if (i %in% c(2, 3, 6, 7, 8, 9)) {
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
  p_values_sd_full[i - 1, 9] <- boot.time.inference.log.var(Rp_CHRP[, c(1, i)], b = block, M = num_m)$p.Value
  
  p_values_sr_full[i - 1, 1] <- boot.time.inference(Rp_MV[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 3] <- boot.time.inference(Rp_RP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 4] <- boot.time.inference(Rp_MD[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 5] <- boot.time.inference(Rp_MDE[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 6] <- boot.time.inference(Rp_HRP[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 7] <- boot.time.inference(Rp_HCAA[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 8] <- boot.time.inference(Rp_HERC[, c(1, i)], b = block, M = num_m)$p.Value
  p_values_sr_full[i - 1, 9] <- boot.time.inference(Rp_CHRP[, c(1, i)], b = block, M = num_m)$p.Value
}
colnames(p_values_sd_full) <- colnames(p_values_sr_full) <- c("MV", "IV", "RP", "MD", "MDE", "HRP", "HCAA", "HERC", "CHRP")
row.names(p_values_sd_full) <- row.names(p_values_sr_full) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8")

p_values_sd_full
p_values_sr_full