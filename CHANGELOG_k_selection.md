# 聚类数 k 选择：修改说明与代码详解

本文档列出本次为「先测试再选 k」所做的全部修改，并说明每段代码在做什么、为什么这样做。

---

## 一、涉及的文件清单

### 1. 被修改的脚本（2 个）

| 文件 | 说明 |
|------|------|
| `clustering_dynamic_network_lite.R` | 轻量版聚类+动态网络脚本（仅 base R + igraph） |
| `clustering_dynamic_network.R` | 完整版聚类+动态网络脚本（tidyverse + factoextra + igraph） |

### 2. 运行脚本后新生成/变更的输出文件

| 输出文件 | 来源脚本 | 说明 |
|----------|----------|------|
| `clustering_elbow.png` | 两版都会生成 | 肘部法图：不同 k 的 WSS，用于看拐点 |
| `clustering_silhouette_choice.png` | 两版都会生成 | 轮廓系数随 k 变化图，用于选最优 k |
| `clustering_silhouette.png` | 仅 lite 版（且用户数≤3000 时） | 选定 k 后，该次聚类的轮廓系数条形图 |
| `clustering_result.png` | 两版 | 使用**自动选定的 k** 的 PCA 聚类散点图 |

---

## 二、修改前的问题（为什么改）

- **之前**：两个脚本里都直接写死 `k <- 4`，没有用数据做任何「选 k」的检验。
- **问题**：不同数据集下，合适的聚类数可能不是 4；盲目用 4 可能导致过度聚类或欠聚类，结果难以解释。
- **目标**：先对 k = 2～10 做**肘部法 + 轮廓系数**检验，再**根据轮廓系数自动选 k**（也可看图后手动改代码里的 k）。

---

## 三、轻量版 `clustering_dynamic_network_lite.R` 修改详解

### 3.1 新增：Step 2.5 选择聚类数 k（约第 65～112 行）

#### （1）参数与随机种子

```r
k_max <- 10
k_min <- 2
set.seed(123)
```

- **做什么**：设定测试的 k 范围为 2～10，并固定随机种子，保证每次运行结果可复现。
- **为什么**：k=1 没有聚类意义；k 太大（如 >10）往往过拟合且难以解释，2～10 是常见搜索范围。

#### （2）肘部法（Elbow Method）

```r
wss <- sapply(k_min:k_max, function(kk) {
  km_temp <- kmeans(scaled, centers = kk, nstart = 25)
  km_temp$tot.withinss
})
# 然后画图保存为 clustering_elbow.png
```

- **做什么**：对每个 k 跑一次 K-means，记录**组内平方和**（Within-cluster Sum of Squares, WSS），并画出 WSS 随 k 变化的曲线。
- **为什么**：WSS 会随 k 增大而下降；曲线出现明显「拐点」（斜率变缓）时，对应的 k 常被当作「再增加 k 收益变小」的候选。肘部法给出一个直观参考，但不自动选 k。

#### （3）轮廓系数（Silhouette）选 k

```r
for (i in seq_len(k_max - k_min + 1)) {
  kk <- k_min + i - 1
  km_temp <- kmeans(scaled, centers = kk, nstart = 25)
  sil_kk <- cluster::silhouette(km_temp$cluster, dist(scaled))
  sil_means[i] <- mean(sil_kk[, 3])
}
# 画图保存为 clustering_silhouette_choice.png
```

- **做什么**：对每个 k 跑 K-means，用 `cluster::silhouette()` 算每个样本的轮廓系数，再对 k 取平均，得到「平均轮廓系数 vs k」的曲线并出图。
- **为什么**：轮廓系数衡量「样本与自己簇的紧密度」和「与最近其他簇的分离度」；**平均轮廓系数越大，聚类质量越好**。用轮廓系数选 k 比肘部法更常用、更易自动化。

#### （4）自动选定 k

```r
if (requireNamespace("cluster", quietly = TRUE) && all(!is.na(sil_means))) {
  k <- k_min + which.max(sil_means) - 1
  cat("✅ 根据轮廓系数自动选择 k =", k, "\n")
} else {
  k <- 4
  cat("⚠️ 未安装 cluster 或轮廓值为 NA，使用默认 k =", k, "（请根据图手动调整）\n")
}
```

- **做什么**：若 `cluster` 包可用且轮廓值无 NA，则 k 取**平均轮廓系数最大**对应的那个数；否则退回默认 k=4，并提示用户看图手动改。
- **为什么**：优先用数据驱动的 k；在依赖缺失或异常时保留一个合理默认值并提醒用户。

### 3.2 修改：Step 3 使用选定的 k（约第 108～111 行）

- **做什么**：删除了原来的 `k <- 4`，改为使用 Step 2.5 中得到的 `k`，再执行 `kmeans(scaled, centers = k, nstart = 25)`。
- **为什么**：保证后续聚类、网络、图都用「测试选出来的 k」，而不是写死的 4。

### 3.3 修改：聚类散点图图例（约第 131 行）

- **原来**：图例写死为 4 个簇的文字描述（Cluster 1～4 各一句）。
- **现在**：`legend(..., legend = paste0("Cluster ", 1:k), ...)`，根据实际 k 动态生成「Cluster 1」「Cluster 2」…。
- **为什么**：k 不再固定为 4，图例需随 k 变化，避免错位或误导。

---

## 四、完整版 `clustering_dynamic_network.R` 修改详解

### 4.1 新增依赖

```r
library(cluster)   # 轮廓系数选 k 时用到
```

- **做什么**：显式加载 `cluster`，供后面 `cluster::silhouette()` 使用。
- **为什么**：后面用 `sapply` 循环里直接调 `cluster::silhouette()`，保证包一定存在，避免运行时报错。

### 4.2 肘部法从注释改为正式运行（约第 106～111 行）

- **原来**：肘部法代码被注释掉，不画图。
- **现在**：用 `fviz_nbclust(..., method = "wss", k.max = k_max)` 画肘部法图，保存为 `clustering_elbow.png`。
- **为什么**：与 lite 版一致，先给出肘部法图，方便用户结合轮廓系数一起判断 k。

### 4.3 轮廓系数图命名与自动选 k（约第 113～125 行）

- **轮廓系数图**：由原来的 `clustering_silhouette.png` 改为保存为 `clustering_silhouette_choice.png`，表示「选 k 用」的图，与 lite 版命名一致。
- **自动选 k**：
  ```r
  sil_scores <- sapply(2:k_max, function(kk) {
    km_temp <- kmeans(scaled_features, centers = kk, nstart = 25)
    mean(cluster::silhouette(km_temp$cluster, dist(scaled_features))[, 3])
  })
  k <- 2L + which.max(sil_scores) - 1L
  ```
- **做什么**：对 k=2～k_max 算平均轮廓系数，k 取使平均轮廓系数最大的值。
- **为什么**：与 lite 版逻辑一致，用轮廓系数自动选 k，避免默认写死 4。

### 4.4 后续 K-means 与说明文字

- **做什么**：删除原来的 `k <- 4`，改为使用上面计算得到的 `k`；结尾的 `cat(...)` 中把「选 k 参考」的文件名改为 `clustering_elbow.png` 和 `clustering_silhouette_choice.png`。
- **为什么**：保证完整版也是「先测试、再定 k、再用该 k 做聚类和网络」，并在输出说明里指向正确的图文件。

---

## 五、整体流程小结（两版一致）

1. **加载数据** → 聚合用户特征 → 标准化 → 得到 `scaled`（lite）或 `scaled_features`（完整版）。
2. **肘部法**：对 k = 2～10 算 WSS，画图存为 `clustering_elbow.png`（只看拐点，不自动选 k）。
3. **轮廓系数**：对 k = 2～10 算平均轮廓系数，画图存为 `clustering_silhouette_choice.png`，并**自动选 k = argmax(平均轮廓系数)**。
4. **K-means**：用选定的 k 做一次 K-means，得到簇标签。
5. **后续**：用该簇标签做动态边、网络图、保存结果等；聚类散点图（及 lite 版的图例）都基于这个 k。

若你希望**固定 k=4 或其它值**，只需在「自动选 k」那一段之后、执行 K-means 之前，手动写一行 `k <- 4`（或你要的数字）即可覆盖自动结果。

---

## 六、如何根据图手动改 k

- 看 **`clustering_elbow.png`**：拐点（斜率明显变缓）附近的 k 可作为候选。
- 看 **`clustering_silhouette_choice.png`**：通常选曲线**最高点**对应的 k；若最高点 k 很大且你更希望少一点簇，可酌情选次高或结合业务选一个较小 k。
- 在脚本里找到「根据轮廓系数自动选择 k」的赋值处，在其后加一行，例如：`k <- 4`，即可固定使用 4 个簇。
