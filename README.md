Twitter Diffusion Analysis

Twitter 转发扩散分析：基于用户特征的聚类、多层逻辑回归与动态关系网络可视化。

 项目概览

本项目对 Twitter 转发行为进行多维度分析：

1. 用户聚类：基于interaction_frequency、interest_similarity、structural_rebundancy、number_of_exposure，用 K-means 将用户分成 4 簇
2. 转发行为建模：多层逻辑回归（glmer）分析兴趣相似性、互动频率、扩散深度对转发概率的影响
3. **动态关系网络**：构建簇间随时间演化的互动网络，并可视化

---

## 项目结构

```
twitter_diffusion_1/
├── README.md                    # 本文件
├── README_clustering_result.md  # 聚类图详解（图例、坐标轴、计算、意义）
├── README_clustering_network.md # 聚类与网络运行说明
├── GITHUB_GUIDE.md              # GitHub 上传与分享步骤
│
├── 数据流水线
│   ├── preprocess.r             # 预处理：重建扩散路径、计算 depth
│   ├── clean.r                  # 整理列名，生成 data_model.Rdata
│   └── data_model.Rdata         # 主数据（需自行准备，见下方）
│
├── 聚类与网络
│   ├── clustering_dynamic_network_lite.R  # 主脚本（推荐，仅需 igraph）
│   ├── clustering_dynamic_network.R       # 完整版（需 tidyverse 等）
│   ├── clustering_result.png              # K-means 聚类散点图
│   ├── clustering_silhouette.png          # 轮廓系数图（用户数≤3000 时）
│   ├── network_static.png                 # 簇间关系网络（全时段）
│   ├── network_time_1.png ~ network_time_5.png  # 各时间片网络图
│   ├── dynamic_edges.csv                  # 动态边（可导入 Gephi）
│   └── clustering_dynamic_results.RData   # 中间结果
│
├── 回归建模
│   ├── main_analysis.R          # glmer 多层逻辑回归
│   ├── Network.R                # 模型调用
│   ├── TableFig.r               # 交互效应、敏感度分析
│   ├── model_summary.txt        # 模型摘要
│   └── final_analysis.RData     # 模型与清洗后数据
│
└── 其他
    ├── Toexcel.R                # 导出 Excel
    └── *.xlsx                   # 导出结果
```

---

## 数据要求

### 主数据：`data_model.Rdata`

需包含以下列（可由 `clean.r` 从上游数据生成）：

| 列名 | 说明 |
|------|------|
| `tweet_id` | 推文 ID |
| `retweeter_id` | 转发者 ID |
| `parent_id` | 被转发者（原文作者）ID |
| `retweeted` | 是否转发（1/0） |
| `depth` | 扩散深度（第几层转发） |
| `time_elapsed` | 时间（用于动态切片） |
| `interaction_freq` | 互动频率 |
| `number_exposures` | 曝光次数 |
| `interest_similarity` | 兴趣相似度 |
| `structural_redundancy` | 结构冗余 |
| `reciprocity` | 互惠关系 |
| `retweeting_inertia` | 转发惯性 |

### 上游数据（用于 `preprocess.r`）

- `sampledRTs.Rdata`：转发事件
- `friends.rds`：关注关系（可自 [Dropbox](https://www.dropbox.com/s/fna98ccv5ooaalj/friends.rds?dl=0) 下载）
- `UserInf.Rdata`：用户信息

---

## 环境要求

- **R** >= 4.4（推荐 4.4.1 及以上）
- **R 包**：
  - 聚类与网络（轻量版）：`igraph`
  - 聚类与网络（完整版）：`tidyverse`, `cluster`, `factoextra`, `igraph`, `ggraph`, `magick`, `cowplot`
  - 回归建模：`lme4`, `dplyr`, `ggplot2`, `ggeffects`, `broom.mixed`

---

## 安装

```r
# 仅聚类与网络（推荐）
install.packages("igraph")

# 完整依赖（若用 clustering_dynamic_network.R）
install.packages(c("tidyverse", "cluster", "factoextra", "igraph", "ggraph", "magick", "cowplot"))

# 回归建模
install.packages(c("lme4", "dplyr", "ggplot2", "ggeffects", "broom.mixed"))
```

---

## 运行

### 1. 聚类与动态网络

**推荐**：轻量版（仅需 igraph，无 tidyverse 依赖冲突）

```r
setwd("你的项目路径")  # 如 c:/Users/lenovo/Downloads/ulib_data_1/twitter_diffusion_1
source("clustering_dynamic_network_lite.R")
```

或终端：

```powershell
Rscript clustering_dynamic_network_lite.R
```

**输出**：`clustering_result.png`、`network_static.png`、`network_time_1~5.png`、`dynamic_edges.csv` 等。

### 2. 多层逻辑回归

```r
source("main_analysis.R")
```

**输出**：`model_summary.txt`、`interest_effect.png`、`depth_effect.png`、`forest_plot.png`、`final_analysis.RData`。

### 3. 数据准备（若有原始数据）

1. 运行 `preprocess.r`（需修改 `setwd` 为你的数据路径）
2. 运行 `clean.r` 生成 `data_model.Rdata`

---

## 输出文件说明

| 文件 | 说明 |
|------|------|
| `clustering_result.png` | K-means 聚类散点图（PC1 vs PC2，四簇着色） |
| `clustering_silhouette.png` | 轮廓系数图，用于选择聚类数 k |
| `network_static.png` | 簇间关系网络（全时段聚合） |
| `network_time_1~5.png` | 各时间片的簇间网络图 |
| `dynamic_edges.csv` | 动态边列表（可导入 Gephi 做动态可视化） |
| `model_summary.txt` | glmer 模型摘要 |
| `interest_effect.png` | 兴趣相似度边际效应 |
| `depth_effect.png` | 扩散深度边际效应 |
| `forest_plot.png` | Odds Ratio 森林图 |

---

## 文档

| 文档 | 内容 |
|------|------|
| [README_clustering_result.md](README_clustering_result.md) | 聚类图图例、坐标轴、计算流程、图表意义 |
| [README_clustering_network.md](README_clustering_network.md) | 聚类与网络运行方式、参数调整 |
| [GITHUB_GUIDE.md](GITHUB_GUIDE.md) | 上传到 GitHub、添加 Collaborator、组员协作步骤 |

---

## 分享与协作

使用 GitHub 共享项目与协作：详见 [GITHUB_GUIDE.md](GITHUB_GUIDE.md)。

---

## License
??
