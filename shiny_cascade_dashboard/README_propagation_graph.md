# Propagation Graph 代码说明

## 文件分工

| 文件 | 说明 |
|------|------|
| `app.R` | **原版**，逻辑全部写在单文件内；线上 shinyapps 仍用这个 |
| `app_modular.R` | **模块化版**，与 `app.R` 功能相同，传播图逻辑抽到 `R/propagation_graph.R` |
| `R/propagation_graph.R` | 传播图专用：读边、建图、绘图（可单独给建站/开发同学看） |
| `tools/export_cascade_edges.R` | 从 `data_model.Rdata` 导出 `cascade_edges.csv` |

## 运行模块化版

```r
setwd("C:/Users/lenovo/twitter_diffusion/shiny_cascade_dashboard")
shiny::runApp("app_modular.R", host = "127.0.0.1", port = 8051)
```

## `R/propagation_graph.R` 导出函数

- `load_propagation_edge_data()` — 读 `cascade_edges.csv` 或 `data_model.Rdata`
- `build_propagation_graph_for_tweet()` — 单条 cascade 的 igraph
- `plot_propagation_graph()` — base 图形绘制

## 给 Project Website 的链接建议

```
传播图模块:
.../tree/main/shiny_cascade_dashboard/R/propagation_graph.R

模块化 Shiny 入口:
.../tree/main/shiny_cascade_dashboard/app_modular.R

原版 Shiny（线上同款）:
.../tree/main/shiny_cascade_dashboard/app.R
```
