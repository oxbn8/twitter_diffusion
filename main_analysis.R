# ==============================================================
# Twitter 转发行为分析：兴趣相似性、互动频率与扩散深度的影响
# 作者: [你的名字]
# 日期: 2026-02-07
# ==============================================================

# --- Step 1: 加载必要包 ---
# 如果未安装，取消注释下面两行进行安装
# install.packages(c("lme4", "dplyr", "ggplot2", "ggeffects", "broom.mixed"))

library(lme4)        # 多层逻辑回归
library(dplyr)       # 数据处理
library(ggplot2)     # 可视化
library(ggeffects)   # 边际效应
library(broom.mixed) # 模型结果整理

# --- Step 2: 加载主数据 ---
cat("🔍 正在加载 data_model.Rdata...\n")
load("data_model.Rdata")  # 假设对象名为 'data'

# 检查数据是否存在
if (!exists("data")) {
  stop("❌ 'data' 对象不存在！请确认 data_model.Rdata 已正确加载。")
}

cat("✅ 数据加载成功！总行数:", nrow(data), "\n")

# --- Step 3: 数据清理（关键！避免 NA 导致模型失败）---
cat("🧹 正在清理缺失值...\n")

# 选择建模所需变量
data_selected <- data %>%
  select(
    retweeted,
    interest_similarity,
    interaction_freq,
    depth,
    tweet_id
  )

# 显式移除任何含 NA 的行（安全方式）
data_clean <- data_selected %>%
  filter(
    !is.na(retweeted),
    !is.na(interest_similarity),
    !is.na(interaction_freq),
    !is.na(depth),
    !is.na(tweet_id)
  )

# 确保 tweet_id 是因子（glmer 要求）
data_clean$tweet_id <- as.factor(data_clean$tweet_id)

cat("✅ 清理完成！有效样本数:", nrow(data_clean), "\n")

# --- Step 4: 拟合多层逻辑回归模型 ---
cat("⚙️ 正在拟合多层逻辑回归模型...\n")

model1 <- glmer(
  retweeted ~ interest_similarity + interaction_freq + depth + (1 | tweet_id),
  data = data_clean,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")  # 更稳定
)

cat("✅ 模型拟合完成！\n")

# --- Step 5: 查看并保存模型摘要 ---
summary_result <- summary(model1)
print(summary_result)

# 保存到文本文件
sink("model_summary.txt")
print(summary_result)
sink()
cat("📄 模型摘要已保存为 model_summary.txt\n")

# --- Step 6: 计算 Odds Ratio（便于解释）---
or_table <- coef(summary_result)["fixed", ] %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(
    OR = exp(Estimate),
    OR_lower = exp(Estimate - 1.96 * `Std. Error`),
    OR_upper = exp(Estimate + 1.96 * `Std. Error`)
  ) %>%
  select(variable, OR, OR_lower, OR_upper, `Pr(>|z|)`)

print(or_table)
cat("📊 Odds Ratio 表已生成\n")

# --- Step 7: 可视化 1 — 兴趣相似度效应 ---
cat("🖼️ 正在生成兴趣相似度效果图...\n")

p1 <- ggpredict(model1, "interest_similarity") %>%
  plot() +
  labs(
    title = "兴趣相似度对转发概率的影响",
    x = "兴趣相似度",
    y = "预测转发概率"
  ) +
  theme_minimal(base_size = 12)

ggsave("interest_effect.png", p1, width = 8, height = 5, dpi = 300)
cat("✅ 图表已保存: interest_effect.png\n")

# --- Step 8: 可视化 2 — 扩散深度效应 ---
cat("🖼️ 正在生成扩散深度效果图...\n")

p2 <- ggpredict(model1, "depth") %>%
  plot() +
  labs(
    title = "扩散深度对转发概率的影响",
    x = "扩散深度（第几层转发）",
    y = "预测转发概率"
  ) +
  theme_minimal(base_size = 12)

ggsave("depth_effect.png", p2, width = 8, height = 5, dpi = 300)
cat("✅ 图表已保存: depth_effect.png\n")

# --- Step 9: 可视化 3 — 系数森林图（Odds Ratio）---
cat("🖼️ 正在生成系数森林图...\n")

forest_data <- or_table %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    variable = recode(
      variable,
      interest_similarity = "兴趣相似度",
      interaction_freq = "互动频率",
      depth = "扩散深度"
    )
  )

p3 <- ggplot(forest_data, aes(x = reorder(variable, OR), y = OR)) +
  geom_point(size = 3, color = "#2E86AB") +
  geom_errorbar(aes(ymin = OR_lower, ymax = OR_upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(
    title = "各因素对转发行为的影响（Odds Ratio, 95% CI）",
    x = "",
    y = "Odds Ratio"
  ) +
  theme_minimal(base_size = 12)

ggsave("forest_plot.png", p3, width = 6, height = 4, dpi = 300)
cat("✅ 图表已保存: forest_plot.png\n")

# --- Step 10: 保存所有对象（便于明天继续）---
save(model1, data_clean, or_table, file = "final_analysis.RData")
cat("💾 所有对象已保存为 final_analysis.RData\n")

# --- 完成提示 ---
cat("\n🎉 分析完成！\n")
cat("📁 输出文件:\n")
cat("   - model_summary.txt\n")
cat("   - interest_effect.png\n")
cat("   - depth_effect.png\n")
cat("   - forest_plot.png\n")
cat("   - final_analysis.RData\n")
cat("\n💡 明天继续：打开此脚本，或加载 final_analysis.RData 即可！\n")