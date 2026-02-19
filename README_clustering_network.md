# 用户聚类与动态关系网络可视化

基于 `data_model.Rdata` 进行用户特征聚类，并构建随时间演化的簇间关系网络图。

## 数据要求

需先有 `data_model.Rdata`（可由 `clean.r` 生成），包含以下列：

- `retweeter_id`, `parent_id`：转发者与被转发者
- `interaction_freq`, `interest_similarity`, `structural_redundancy`, `number_exposures`：特征
- `time_elapsed`：时间（用于动态切片）

## 运行方式

**推荐**：若主脚本因 tidyverse/rlang 版本冲突报错，请用**轻量版**（仅需 igraph，无 tidyverse）：

```r
setwd("c:/Users/lenovo/Downloads/ulib_data_1/twitter_diffusion_1")
source("clustering_dynamic_network_lite.R")  # 轻量版，直接生成图表
```

主脚本（需要 tidyverse 等）：

```r
source("clustering_dynamic_network.R")
```

终端：`Rscript clustering_dynamic_network_lite.R`

## 依赖包

```r
install.packages(c("tidyverse", "cluster", "factoextra", "igraph", "ggraph", "magick", "cowplot"))
```

GIF 生成需 `magick`，若安装失败可注释掉 GIF 相关代码，仍会生成静态图。

## 输出文件

| 文件 | 说明 |
|------|------|
| `clustering_silhouette.png` | 轮廓系数图，用于选择 k |
| `clustering_result.png` | K-means 聚类散点图 |
| `network_static.png` | 全时段簇间关系网络 |
| `network_time_1.png` ~ `network_time_5.png` | 各时间片网络图 |
| `dynamic_edges.csv` | 动态边列表（可导入 Gephi） |
| `dynamic_relationship_map.gif` | 动态 GIF（若成功生成） |
| `clustering_dynamic_results.RData` | 中间结果（用户簇、边） |

## 参数调整

- **聚类数 k**：默认 4，根据 `clustering_silhouette.png` 调整脚本中的 `k <- 4`
- **时间切片数**：由 `n_bins` 控制（约 5–20）
- **Gephi**：导入 `dynamic_edges.csv` 作为动态边，可用 Timeline 播放
