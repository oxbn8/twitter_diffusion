# Website 交接用 Shiny 包（模块化版）

给建站/开发同学的一小包，内含 **`app.R`**（模块化入口）和 **`R/propagation_graph.R`**。

- 仓库根目录的 **`../app.R`** 仍是线上部署用的原版，未改动。
- 本目录的 **`app.R`** 与 **`../app_modular.R`** 内容相同，只是文件名便于交接。

运行：

```r
setwd(".../shiny_cascade_dashboard/website_bundle")
shiny::runApp("app.R")
```

还需 `app_data/` 下的 CSV：请从上级目录复制 `app_data`，或在上级目录运行 `app_modular.R`。
