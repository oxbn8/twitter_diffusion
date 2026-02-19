# ==============================================================
# 用户聚类 + 动态关系网络（轻量版 - 仅 base R + igraph）
# 避免 tidyverse/factoextra 的 rlang 依赖冲突
# 直接生成图表
# ==============================================================

# --- 设置工作目录 ---
# setwd("c:/Users/lenovo/Downloads/ulib_data_1/twitter_diffusion_1")

# 依赖: igraph (若未安装: install.packages("igraph"))
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("请先安装 igraph: install.packages('igraph')")
}
library(igraph)

# ========== Step 1: 加载数据 ==========
cat("🔍 正在加载 data_model.Rdata...\n")
if (!file.exists("data_model.Rdata")) {
  stop("❌ data_model.Rdata 不存在！请确保在项目目录运行。")
}
load("data_model.Rdata")

required <- c("retweeter_id", "parent_id", "interaction_freq", "interest_similarity", 
              "structural_redundancy", "number_exposures", "time_elapsed")
missing <- setdiff(required, names(data))
if (length(missing) > 0) stop("❌ 缺少列: ", paste(missing, collapse = ", "))

cat("✅ 数据加载成功！总行数:", nrow(data), "\n")

# ========== Step 2: 聚合用户特征（base R）==========
ids <- unique(c(data$retweeter_id, data$parent_id))
agg <- aggregate(
  cbind(interaction_freq, interest_similarity, structural_redundancy, number_exposures) ~ retweeter_id,
  data = data, FUN = mean, na.rm = TRUE
)
colnames(agg) <- c("uid", "avg_interaction", "avg_interest_sim", "avg_structural_red", "total_exposures_raw")

# parent 聚合
agg_p <- aggregate(
  cbind(interaction_freq, interest_similarity, structural_redundancy, number_exposures) ~ parent_id,
  data = data, FUN = mean, na.rm = TRUE
)
colnames(agg_p) <- c("uid", "avg_interaction", "avg_interest_sim", "avg_structural_red", "total_exposures_raw")

# 合并 retweeter + parent，同用户取平均；total_exposures 取 sum
rb <- rbind(
  agg[, c("uid", "avg_interaction", "avg_interest_sim", "avg_structural_red")],
  setNames(agg_p[, c("uid", "avg_interaction", "avg_interest_sim", "avg_structural_red")], 
           c("uid", "avg_interaction", "avg_interest_sim", "avg_structural_red"))
)
user_feat <- aggregate(cbind(avg_interaction, avg_interest_sim, avg_structural_red) ~ uid, data = rb, FUN = mean, na.rm = TRUE)
s1 <- aggregate(number_exposures ~ retweeter_id, data, sum, na.rm = TRUE)
s2 <- aggregate(number_exposures ~ parent_id, data, sum, na.rm = TRUE)
m1 <- setNames(s1$number_exposures, s1$retweeter_id)
m2 <- setNames(s2$number_exposures, s2$parent_id)
user_feat$total_exposures <- pmax(m1[user_feat$uid], m2[user_feat$uid], na.rm = TRUE)
user_feat$total_exposures[is.na(user_feat$total_exposures)] <- 0

clust_vars <- c("avg_interaction", "avg_interest_sim", "avg_structural_red", "total_exposures")
valid <- complete.cases(user_feat[clust_vars]) & apply(user_feat[clust_vars], 1, function(x) all(is.finite(x)))
user_ok <- user_feat[valid, ]
scaled <- as.data.frame(scale(user_ok[clust_vars]))
rownames(scaled) <- user_ok$uid

# ========== Step 3: K-means 聚类 ==========
k <- 4
set.seed(123)
km <- kmeans(scaled, centers = k, nstart = 25)
user_ok$cluster <- factor(paste0("Cluster_", km$cluster))
user_cluster <- merge(user_feat[, "uid", drop = FALSE], user_ok[, c("uid", "cluster")], by = "uid", all.x = TRUE)

#  silhouettes（轮廓系数；用户数>3000 时跳过以节省时间）
if (requireNamespace("cluster", quietly = TRUE) && nrow(scaled) <= 3000) {
  sil <- cluster::silhouette(km$cluster, dist(scaled))
  png("clustering_silhouette.png", width = 600, height = 400, res = 120)
  plot(sil, main = "轮廓系数 (Silhouette)")
  dev.off()
  cat("✅ clustering_silhouette.png\n")
} else if (nrow(scaled) > 3000) {
  cat("⚠️ 用户数过多，跳过轮廓系数图（可抽样后重跑）\n")
}

# 聚类散点图 (PC1 vs PC2)
pca <- prcomp(scaled)
png("clustering_result.png", width = 900, height = 720, res = 120)
par(mar = c(1, 4, 3, 4), oma = c(8, 0, 0, 0), xpd = NA)
plot(pca$x[, 1], pca$x[, 2], col = km$cluster, pch = 16, cex = 0.6,
     xlab = "PC1", ylab = "PC2", main = paste0("K-means Clustering Results (k=", k, ")"))
legend("topright", legend = c(
  "Cluster 1: Central / balanced across all four features",
  "Cluster 2: High exposure & structural redundancy, lower interest similarity",
  "Cluster 3: Low interaction, exposure, and structural redundancy",
  "Cluster 4: High interest similarity & diverse exposure patterns"),
  col = 1:k, pch = 16, bty = "n", cex = 0.8)
# 底部详细解释：从四个 feature 到 axis interpretation
feat_lines <- c(
  "Four features used for clustering:",
  "  (1) Interaction frequency: how often the retweeter interacted with the parent before retweeting.",
  "  (2) Interest similarity: overlap of interests between retweeter and parent.",
  "  (3) Structural redundancy: degree of overlap in their social networks.",
  "  (4) Number of exposures: total times the user was exposed to the tweet.",
  "",
  "Axis interpretation: X-axis (PC1) = 1st principal component (linear combo of the 4 features, captures max variance);",
  "Y-axis (PC2) = 2nd principal component (orthogonal to PC1, captures 2nd most variance).")
for (i in seq_along(feat_lines)) {
  mtext(feat_lines[i], side = 1, outer = TRUE, line = 8 - i, cex = 0.65, adj = 0)
}
dev.off()
cat("✅ clustering_result.png\n")

# ========== Step 4: 动态边 ==========
edges <- data[, c("retweeter_id", "parent_id", "time_elapsed")]
colnames(edges) <- c("source", "target", "time")
edges <- edges[edges$source != edges$target, ]
uc <- user_cluster
colnames(uc) <- c("uid", "cluster")
edges <- merge(edges, uc, by.x = "source", by.y = "uid")
edges <- merge(edges, uc, by.x = "target", by.y = "uid", suffixes = c("_src", "_tgt"))
edges <- edges[!is.na(edges$cluster_src) & !is.na(edges$cluster_tgt), 
               c("cluster_src", "cluster_tgt", "time")]
colnames(edges) <- c("source_cluster", "target_cluster", "time")

tr <- range(edges$time, na.rm = TRUE)
n_bins <- min(20, max(5, length(unique(edges$time)) %/% 10))
br <- seq(tr[1], tr[2], length.out = n_bins + 1)
edges$time_bin <- cut(edges$time, breaks = br, include.lowest = TRUE)

dyn <- aggregate(rep(1, nrow(edges)) ~ source_cluster + target_cluster + time_bin, 
                 data = edges, FUN = sum)
colnames(dyn)[4] <- "n"
dyn <- dyn[dyn$n > 0, ]

write.csv(dyn, "dynamic_edges.csv", row.names = FALSE)
cat("✅ dynamic_edges.csv\n")

# ========== Step 5: 网络图 ==========
agg_e <- aggregate(n ~ source_cluster + target_cluster, data = dyn, FUN = sum)
g <- graph_from_data_frame(agg_e, directed = TRUE, vertices = NULL)

png("network_static.png", width = 800, height = 600, res = 120)
par(mar = c(0, 0, 2, 0))
plot(g, layout = layout_in_circle,
     vertex.size = 15, vertex.color = "steelblue", vertex.label.cex = 1.2,
     edge.width = 0.5 + 2 * (E(g)$n / max(E(g)$n)), edge.arrow.size = 0.5,
     main = "簇间关系网络（全时段聚合）")
dev.off()
cat("✅ network_static.png\n")

# 各时间片
tbins <- sort(unique(dyn$time_bin))
n_save <- min(5, length(tbins))
for (i in seq_len(n_save)) {
  sub <- dyn[dyn$time_bin == tbins[i], ]
  if (nrow(sub) == 0) next
  g2 <- graph_from_data_frame(sub[, c("source_cluster", "target_cluster", "n")], directed = TRUE, vertices = NULL)
  png(paste0("network_time_", i, ".png"), width = 700, height = 500, res = 120)
  par(mar = c(0, 0, 2, 0))
  plot(g2, layout = layout_in_circle,
       vertex.size = 12, vertex.color = "steelblue", vertex.label.cex = 1,
       edge.width = 0.5 + 1.5 * (E(g2)$n / max(E(g2)$n, 1)), edge.arrow.size = 0.5,
       main = paste0("时间片: ", as.character(tbins[i])))
  dev.off()
}
cat("✅ network_time_1.png ~ network_time_", n_save, ".png\n")

save(user_cluster, dyn, agg_e, file = "clustering_dynamic_results.RData")
cat("💾 clustering_dynamic_results.RData\n")
cat("\n🎉 完成！图表已生成。\n")
