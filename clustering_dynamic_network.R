# ==============================================================
# Twitter 用户聚类 + 动态关系网络可视化
# 基于特征聚类 → 簇间互动 → 随时间演化的关系图
# 适配数据: data_model.Rdata (retweeter_id, parent_id, interaction_freq, interest_similarity, structural_redundancy, number_exposures, time_elapsed)
# ==============================================================

# --- 设置工作目录（若 data_model.Rdata 不在当前目录，请修改）---
# setwd("你的数据所在路径")

# --- 安装依赖（首次运行取消注释）---
# install.packages(c("tidyverse", "cluster", "factoextra", "igraph", "ggraph", "magick", "cowplot"))
#
# 若因 rlang/ggplot2 版本冲突报错，请改用轻量版：
#   source("clustering_dynamic_network_lite.R")

err <- tryCatch({
  library(tidyverse)
  NULL
}, error = function(e) conditionMessage(e))
if (!is.null(err)) {
  stop("❌ tidyverse 加载失败: ", err, "\n\n请改用轻量版脚本: source(\"clustering_dynamic_network_lite.R\")")
}
library(factoextra)
library(igraph)
library(ggraph)
library(gganimate)

# ==============================================================
# Step 1: 加载数据 & 聚合用户特征
# ==============================================================
cat("🔍 正在加载 data_model.Rdata...\n")

if (!file.exists("data_model.Rdata")) {
  stop("❌ data_model.Rdata 不存在！请确保在项目目录运行，或先执行 clean.r 生成数据。")
}

load("data_model.Rdata")

# 检查必要列
required_cols <- c("retweeter_id", "parent_id", "interaction_freq", "interest_similarity", 
                   "structural_redundancy", "number_exposures", "time_elapsed")
missing <- setdiff(required_cols, names(data))
if (length(missing) > 0) stop("❌ 缺少列: ", paste(missing, collapse = ", "))

cat("✅ 数据加载成功！总行数:", nrow(data), "\n")

# 聚合每个 retweeter 的平均特征
user_features <- data %>%
  group_by(retweeter_id) %>%
  summarise(
    avg_interaction    = mean(interaction_freq, na.rm = TRUE),
    avg_interest_sim   = mean(interest_similarity, na.rm = TRUE),
    avg_structural_red = mean(structural_redundancy, na.rm = TRUE),
    total_exposures    = sum(number_exposures, na.rm = TRUE),
    n_events           = n(),
    .groups = "drop"
  )

# 对 parent 也聚合特征（边中的 target 需要聚类标签）
parent_features <- data %>%
  group_by(parent_id) %>%
  summarise(
    avg_interaction    = mean(interaction_freq, na.rm = TRUE),
    avg_interest_sim   = mean(interest_similarity, na.rm = TRUE),
    avg_structural_red = mean(structural_redundancy, na.rm = TRUE),
    total_exposures    = sum(number_exposures, na.rm = TRUE),
    n_events           = n(),
    .groups = "drop"
  ) %>%
  rename(retweeter_id = parent_id)

# 合并：用户若既为 retweeter 又为 parent，特征取平均
user_features <- bind_rows(
  user_features %>% mutate(role = "r"),
  parent_features %>% mutate(role = "p")
) %>%
  group_by(retweeter_id) %>%
  summarise(
    avg_interaction    = mean(avg_interaction, na.rm = TRUE),
    avg_interest_sim   = mean(avg_interest_sim, na.rm = TRUE),
    avg_structural_red = mean(avg_structural_red, na.rm = TRUE),
    total_exposures    = sum(total_exposures, na.rm = TRUE),
    n_events           = sum(n_events, na.rm = TRUE),
    .groups = "drop"
  )

cat("📊 用户数:", nrow(user_features), "\n")

# ==============================================================
# Step 2: 特征标准化 + 聚类
# ==============================================================
clust_vars <- c("avg_interaction", "avg_interest_sim", "avg_structural_red", "total_exposures")

# 只保留有有效值的行
user_for_clust <- user_features %>%
  filter(if_all(all_of(clust_vars), ~ !is.na(.) & is.finite(.)))

scaled_features <- as.data.frame(scale(user_for_clust[clust_vars]))
rownames(scaled_features) <- user_for_clust$retweeter_id

# 选择聚类数 k（肘部法 + 轮廓系数）
cat("📐 计算最优聚类数...\n")

# 肘部法（可选，较慢时注释掉）
# p_wss <- fviz_nbclust(scaled_features, kmeans, method = "wss", k.max = 10) +
#   labs(title = "肘部法选择 k")
# ggsave("clustering_elbow.png", p_wss, width = 6, height = 4, dpi = 150)

# 轮廓系数（推荐 k=3~5）
p_sil <- fviz_nbclust(scaled_features, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "轮廓系数选择聚类数 k")
ggsave("clustering_silhouette.png", p_sil, width = 6, height = 4, dpi = 150)
cat("✅ 轮廓系数图已保存: clustering_silhouette.png\n")

# 执行 K-means（默认 k=4，可根据上图调整）
k <- 4
set.seed(123)
kmeans_result <- kmeans(scaled_features, centers = k, nstart = 25)

user_clusters <- user_for_clust %>%
  mutate(cluster = as.factor(paste0("Cluster_", kmeans_result$cluster)))

# 合并回完整 user_features（未参与聚类的用户标为 NA）
user_clusters_full <- user_features %>%
  left_join(user_clusters %>% select(retweeter_id, cluster), by = "retweeter_id")

# 聚类可视化
p_clust <- fviz_cluster(kmeans_result, data = scaled_features, geom = "point", 
                        main = paste0("K-means 聚类结果 (k=", k, ")"))
ggsave("clustering_result.png", p_clust, width = 7, height = 5, dpi = 150)
cat("✅ 聚类图已保存: clustering_result.png\n")

# ==============================================================
# Step 3: 构建动态边（簇 → 簇，按时间切片）
# ==============================================================
edges_raw <- data %>%
  select(source = retweeter_id, target = parent_id, time = time_elapsed) %>%
  filter(source != target)  # 排除自环

edges_with_cluster <- edges_raw %>%
  left_join(user_clusters_full %>% select(retweeter_id, source_cluster = cluster),
            by = c("source" = "retweeter_id")) %>%
  left_join(user_clusters_full %>% select(retweeter_id, target_cluster = cluster),
            by = c("target" = "retweeter_id")) %>%
  select(source_cluster, target_cluster, time) %>%
  drop_na()

# 按时间分箱（可根据数据范围调整：小时/天）
time_range <- range(edges_with_cluster$time, na.rm = TRUE)
n_bins <- min(20, max(5, length(unique(edges_with_cluster$time)) %/% 10))
breaks <- seq(time_range[1], time_range[2], length.out = n_bins + 1)

edges_with_cluster <- edges_with_cluster %>%
  mutate(time_bin = cut(time, breaks = breaks, include.lowest = TRUE))

dynamic_edges <- edges_with_cluster %>%
  count(source_cluster, target_cluster, time_bin, .drop = FALSE) %>%
  filter(n > 0)

cat("📊 动态边数:", nrow(dynamic_edges), "| 时间片数:", n_distinct(dynamic_edges$time_bin), "\n")

# 导出 Gephi 可用格式
write_csv(dynamic_edges, "dynamic_edges.csv")
cat("✅ 动态边已导出: dynamic_edges.csv (可用于 Gephi)\n")

# ==============================================================
# Step 4: 可视化
# ==============================================================

# --- 4.1 静态：所有时间片聚合的簇间关系图 ---
agg_edges <- dynamic_edges %>%
  group_by(source_cluster, target_cluster) %>%
  summarise(n = sum(n), .groups = "drop")

g_agg <- graph_from_data_frame(agg_edges, directed = TRUE, vertices = NULL)

p_static <- ggraph(g_agg, layout = "circle") +
  geom_edge_link(aes(width = n), alpha = 0.6,
                 arrow = arrow(length = unit(2, "mm")), end_cap = circle(3, "mm")) +
  geom_node_point(size = 12, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_edge_width(range = c(0.5, 3)) +
  labs(title = "簇间关系网络（全时段聚合）") +
  theme_void()

ggsave("network_static.png", p_static, width = 8, height = 6, dpi = 150)
cat("✅ 静态网络图已保存: network_static.png\n")

# --- 4.2 动态：按时间片生成多帧图 ---
time_bins <- unique(dynamic_edges$time_bin) %>% sort()
plot_list <- list()

for (i in seq_along(time_bins)) {
  tb <- time_bins[i]
  sub_edges <- dynamic_edges %>% filter(time_bin == tb)
  
  if (nrow(sub_edges) == 0) next
  
  g <- graph_from_data_frame(sub_edges %>% select(source_cluster, target_cluster, n),
                             directed = TRUE, vertices = NULL)
  
  p <- ggraph(g, layout = "circle") +
    geom_edge_link(aes(width = n), alpha = 0.6,
                   arrow = arrow(length = unit(2, "mm")), end_cap = circle(3, "mm")) +
    geom_node_point(size = 10, color = "steelblue") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
    scale_edge_width(range = c(0.5, 2.5)) +
    labs(title = paste0("时间片: ", as.character(tb))) +
    theme_void()
  
  plot_list[[i]] <- p
}

# 保存前几张为单独 PNG（若时间片过多）
n_save <- min(5, length(plot_list))
for (i in 1:n_save) {
  ggsave(paste0("network_time_", i, ".png"), plot_list[[i]], width = 7, height = 5, dpi = 150)
}
cat("✅ 前", n_save, "个时间片网络图已保存: network_time_1.png ~ network_time_", n_save, ".png\n")

# --- 4.3 动态 GIF（需要 gifski）---
if (length(plot_list) <= 30 && length(plot_list) >= 1) {
  # 将 plot_list 转为 gganimate 可用的数据驱动形式
  # gganimate 需要一列 "frame" 的 data，这里用 cowplot 拼成动画
  tryCatch({
    library(cowplot)
    library(magick)
    
    imgs <- lapply(plot_list, function(p) {
      tf <- tempfile(fileext = ".png")
      ggsave(tf, p, width = 6, height = 5, dpi = 100)
      magick::image_read(tf)
    })
    
    anim <- magick::image_animate(do.call(c, imgs), fps = 2)
    magick::image_write(anim, "dynamic_relationship_map.gif")
    cat("✅ 动态 GIF 已保存: dynamic_relationship_map.gif\n")
  }, error = function(e) {
    cat("⚠️ GIF 生成跳过（需 magick 包）: ", conditionMessage(e), "\n")
  })
} else {
  cat("⚠️ 时间片过多(>30)或为0，跳过 GIF 生成。可手动用 network_time_*.png 制作。\n")
}

# ==============================================================
# Step 5: 保存中间结果
# ==============================================================
save(user_clusters_full, dynamic_edges, agg_edges, 
     file = "clustering_dynamic_results.RData")
cat("💾 中间结果已保存: clustering_dynamic_results.RData\n")

# --- 完成 ---
cat("\n🎉 聚类与动态网络可视化完成！\n")
cat("📁 输出文件:\n")
cat("   - clustering_silhouette.png  (选 k 参考)\n")
cat("   - clustering_result.png      (聚类结果)\n")
cat("   - network_static.png         (全时段网络)\n")
cat("   - network_time_1~5.png       (各时间片网络)\n")
cat("   - dynamic_edges.csv          (Gephi 动态边)\n")
cat("   - dynamic_relationship_map.gif (若成功生成)\n")
cat("   - clustering_dynamic_results.RData\n")
