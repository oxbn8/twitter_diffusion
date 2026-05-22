# Shiny Cascade Dashboard

基于 `cascade_shape_outputs/` 的交互式分析网站，适合：

- 浏览全部 cascade 的整体分布
- 用缩略图矩阵快速扫 900+ 个传播图形态
- 点开单个 cascade 看传播图和层级结构
- 做内部分析：判断中V是否够用、哪些案例更像必须依赖大V、哪些账号像潜力新星

## 依赖

```r
install.packages(c("shiny", "DT", "plotly", "igraph"))
```

## 运行

在项目根目录执行：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "shiny::runApp('shiny_cascade_dashboard', host='127.0.0.1', port=8051, launch.browser=TRUE)"
```

或在 R 中：

```r
setwd("C:/Users/lenovo/twitter_diffusion")
shiny::runApp("shiny_cascade_dashboard", host = "127.0.0.1", port = 8051, launch.browser = TRUE)
```

## 使用前提

需要先跑：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "cascade_shape_pipeline.R"
```

这样会生成：

- `cascade_shape_outputs/cascade_metrics.csv`
- `cascade_shape_outputs/cascade_depth_profile.csv`
- `cascade_shape_outputs/embedding_cv_comparison.csv`
- `cascade_shape_outputs/feature_type_differences_control_size.csv`

详情页如果要重建单个 `propagation graph`，还需要能访问 `data_model.Rdata`。

## 页面说明

- `总览`
  - 看 `depth vs breadth` 散点图
  - 看当前筛选下 `broadcast / viral` 的结构均值
  - 用表格快速筛选 cascade

- `缩略图矩阵`
  - 每张卡片是一张迷你层级分布图
  - 可按 ROI/中V效率、大V需求、新星潜力等排序
  - 点击卡片跳到详情页

- `Cascade 详情`
  - 显示单个 cascade 的传播图
  - 显示 depth profile
  - 显示核心结构指标
  - 给出预算建议标签：`中V够了 / 先中V测试再决定 / 建议大V`
  - 支持导出当前 cascade 的 `PNG` 和 `CSV`

- `内部分析`
  - `中V 更可能够用的案例`
  - `更像必须依赖大V 的案例`
  - `潜力新星候选`
  - 控制 `size` 后的类型差异和 embedding 对比
  - 直接展示预算决策标签分布

## 部署到 shinyapps.io

这个目录已经整理成可部署版本，部署时会直接使用：

- `shiny_cascade_dashboard/app_data/cascade_metrics.csv`
- `shiny_cascade_dashboard/app_data/cascade_depth_profile.csv`
- `shiny_cascade_dashboard/app_data/cascade_edges.csv`
- `shiny_cascade_dashboard/app_data/embedding_cv_comparison.csv`
- `shiny_cascade_dashboard/app_data/feature_type_differences_control_size.csv`

也就是说，部署后的应用**不依赖你本机 OneDrive 路径**。

### 1. 安装部署依赖

```r
install.packages("rsconnect")
```

### 2. 第一次登录 shinyapps.io

先在 [shinyapps.io](https://www.shinyapps.io/) 注册并登录，然后在 Account 页面拿到：

- `name`
- `token`
- `secret`

在 R 里执行：

```r
rsconnect::setAccountInfo(
  name = "你的账号名",
  token = "你的 token",
  secret = "你的 secret"
)
```

### 3. 部署

在项目根目录执行：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "shiny_cascade_dashboard/deploy_shinyapps.R"
```

部署成功后，终端会返回一个公网 URL，别人就可以直接打开。
