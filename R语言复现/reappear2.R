# ==================================================
# 复现 Christensen et al. (2018)
# 稳定版（移除有问题的随机网络检验，专注核心指标）
# ==================================================

# 1. 加载包 ---------------------------------------------------------
library(NetworkToolbox)
library(igraph)
library(SemNeT)

# 2. 自定义余弦相似度函数 -------------------------------------------
cosine_sim <- function(mat) {
  mat <- as.matrix(mat)
  if (ncol(mat) < 2) stop("矩阵列数不足")
  norm <- sqrt(colSums(mat^2))
  norm[norm == 0] <- 1e-10
  sim <- crossprod(mat) / tcrossprod(norm)
  return(sim)
}

# 3. 读取数据 -------------------------------------------------------
fluency <- read.csv("cleaned_animal_fluency_FIXED.csv", row.names = 1)
latent <- read.csv("FINAL open.csv")

if (!"id" %in% colnames(latent)) latent$id <- latent[,1]

common_ids <- intersect(rownames(fluency), latent$id)
fluency <- fluency[common_ids, , drop = FALSE]
latent <- latent[latent$id %in% common_ids, , drop = FALSE]

latent <- latent[order(latent$no_int), ]
comb <- data.frame(latent = latent$no_int, id = latent$id,
                   fluency[match(latent$id, rownames(fluency)), , drop = FALSE])

# 4. 分组 -----------------------------------------------------------
n_total <- nrow(comb)
n_half <- n_total %/% 2
low <- comb[1:n_half, , drop = FALSE]
high <- comb[(n_half+1):n_total, , drop = FALSE]

deLow <- low[, -c(1,2), drop = FALSE]
deHigh <- high[, -c(1,2), drop = FALSE]

# 5. 行为分析 -------------------------------------------------------
cat("\n===== 行为分析 =====\n")
sumAll <- rowSums(comb[, -c(1,2), drop = FALSE])
cor_test <- cor.test(sumAll, comb$latent)
print(cor_test)

sumLow <- rowSums(deLow)
sumHigh <- rowSums(deHigh)
t_test <- t.test(sumHigh, sumLow, var.equal = TRUE)
print(t_test)

onlyH <- deHigh[, colSums(deHigh) >= 1, drop = FALSE]
onlyL <- deLow[, colSums(deLow) >= 1, drop = FALSE]
uniH <- colnames(onlyH)
uniL <- colnames(onlyL)
uniT <- unique(c(uniH, uniL))
oneH <- match(uniT, uniH)
oneL <- match(uniT, uniL)
chitest <- matrix(0, nrow = length(uniT), ncol = 2)
chitest[,1] <- ifelse(!is.na(oneH), 1, 0)
chitest[,2] <- ifelse(!is.na(oneL), 1, 0)
mcnemar <- mcnemar.test(chitest[,1], chitest[,2])
print(mcnemar)

# 6. 节点过滤 -------------------------------------------------------
low_count <- colSums(deLow)
high_count <- colSums(deHigh)
valid_animals <- names(which(low_count >= 2 & high_count >= 2))
if (length(valid_animals) < 2) stop("有效动物词不足，请检查数据")
cat("\n有效动物词数量:", length(valid_animals), "\n")

low_mat <- as.matrix(deLow[, valid_animals, drop = FALSE])
high_mat <- as.matrix(deHigh[, valid_animals, drop = FALSE])

# 7. 构建语义网络 ---------------------------------------------------
cos_low <- cosine_sim(low_mat) + 0.01
cos_high <- cosine_sim(high_mat) + 0.01
diag(cos_low) <- 1
diag(cos_high) <- 1

tmp_low <- TMFG(cos_low)
tmp_high <- TMFG(cos_high)
if (is.list(tmp_low) && "A" %in% names(tmp_low)) {
  net_low <- tmp_low$A
  net_high <- tmp_high$A
} else {
  net_low <- tmp_low
  net_high <- tmp_high
}
net_low <- ifelse(net_low > 0, 1, 0)
net_high <- ifelse(net_high > 0, 1, 0)

# 8. 计算网络指标 ---------------------------------------------------
cat("\n===== 网络指标 =====\n")
meas_low <- semnetmeas(net_low)
meas_high <- semnetmeas(net_high)

result <- data.frame(
  group = c("Low Openness", "High Openness"),
  ASPL = round(c(meas_low["ASPL"], meas_high["ASPL"]), 3),
  CC   = round(c(meas_low["CC"],   meas_high["CC"]),   3),
  Q    = round(c(meas_low["Q"],    meas_high["Q"]),     3)
)
print(result)

cat("\n论文 Table 2 参考值:\n")
cat("Low:  ASPL=3.19, CC=1.03, Q=0.590\n")
cat("High: ASPL=2.84, CC=1.05, Q=0.521\n")

# 9. 导出 Cytoscape 文件（可选）-------------------------------------
highCyto <- convert2cytoscape(net_high)
lowCyto <- convert2cytoscape(net_low)
write.csv(highCyto, "high_open_cyto.csv", row.names = FALSE)
write.csv(lowCyto, "low_open_cyto.csv", row.names = FALSE)

cat("\n分析完成！\n")