options(stringsAsFactors = FALSE, scipen = 999)

required_pkgs <- c("shiny", "DT", "plotly", "igraph")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
    "\nPlease install them first, e.g. install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
    "))"
  )
}

library(shiny)
library(DT)
library(plotly)
library(igraph)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

discover_data_dir <- function() {
  candidates <- unique(c(
    normalizePath(file.path(getwd(), "app_data"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "cascade_shape_outputs"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "cascade_shape_outputs"), winslash = "/", mustWork = FALSE)
  ))
  for (p in candidates) {
    if (file.exists(file.path(p, "cascade_metrics.csv"))) {
      return(p)
    }
  }
  stop("Cannot find a data directory containing `cascade_metrics.csv`.")
}

data_dir <- discover_data_dir()
project_root <- normalizePath(file.path(data_dir, ".."), winslash = "/", mustWork = FALSE)
output_dir <- data_dir
default_external_path <- NA_character_

pick_existing_path <- function(paths) {
  for (p in paths) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  NA_character_
}

coerce_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

scale01 <- function(x) {
  x <- coerce_numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.finite(x), format(round(x, digits), big.mark = ",", trim = TRUE), "NA")
}

fmt_int <- function(x) {
  ifelse(is.finite(x), format(round(x), big.mark = ",", trim = TRUE), "NA")
}

decision_label <- function(delta) {
  ifelse(
    delta >= 0.15, "midv_enough",
    ifelse(delta <= -0.15, "need_bigv", "test_first")
  )
}

decision_reason <- function(label) {
  ifelse(
    label == "midv_enough",
    "midv_reason",
    ifelse(
      label == "need_bigv",
      "bigv_reason",
      "test_reason"
    )
  )
}

i18n <- list(
  en = list(
    app_title = "Cascade Shape Dashboard",
    language = "Language",
    sidebar_title = "Global filters",
    type_filter = "Cascade type",
    type_all = "All",
    type_broadcast = "Broadcast",
    type_viral = "Viral",
    type_unclassified = "Unclassified",
    classified_only = "Show classified cascades only",
    size_range = "Size range",
    depth_range = "Depth range",
    search_tweet = "Search tweet_id",
    sort_by = "Thumbnail sort",
    sort_efficiency = "ROI / Mid-tier efficiency first",
    sort_bigv = "Big-V need first",
    sort_rising = "Rising-star potential first",
    cards_per_page = "Cards per page",
    matrix_page = "Thumbnail page",
    thumb_note = "Each card in the thumbnail matrix is a mini depth-profile view of one cascade. Click a card or a point in the overview scatter to jump into details.",
    project_root = "Project root",
    data_model_path = "Data model path",
    data_model_missing = "Unavailable; detail graph reconstruction will rely on bundled edge data only",
    tab_overview = "Overview",
    tab_matrix = "Thumbnail Matrix",
    tab_detail = "Cascade Detail",
    tab_internal = "Internal Analysis",
    metric_total = "Total cascades",
    metric_filtered = "Current selection",
    metric_types = "Broadcast / Viral",
    metric_cv = "Embedding + baseline AUC",
    scatter_title = "Shape map: depth vs breadth",
    scatter_x = "log(1 + depth)",
    scatter_y = "log(1 + max breadth)",
    hover_tweet = "tweet_id",
    hover_type = "type",
    hover_size = "size",
    hover_depth = "depth",
    hover_breadth = "max breadth",
    hover_root_followers = "root followers",
    compare_title = "Average structural metrics under current filters",
    compare_depth = "depth",
    compare_breadth = "max_breadth",
    compare_virality = "structural_virality",
    overview_cols = c("tweet_id", "type", "size", "depth", "max_breadth", "structural_virality", "root_followers", "efficiency_score", "bigv_need_score", "rising_star_score"),
    matrix_status = "Showing %s cascades; page %s / %s.",
    thumb_size = "size",
    thumb_depth = "depth",
    thumb_breadth = "breadth",
    thumb_root_followers = "root followers",
    thumb_efficiency = "efficiency",
    thumb_rising = "rising",
    detail_select = "Select tweet_id",
    detail_download_png = "Download current PNG",
    detail_download_csv = "Download current CSV",
    detail_metric_names = c("tweet_id", "cascade_type", "budget_recommendation", "size", "depth", "max_breadth", "structural_virality", "root_followers", "avg_interest_similarity", "avg_number_exposures", "mean_reciprocity"),
    detail_metric_col1 = "metric",
    detail_metric_col2 = "value",
    detail_graph_missing = "Edge data not found; cannot reconstruct this propagation graph.",
    detail_graph_title = "Propagation graph",
    detail_depth_title = "Depth profile",
    detail_depth_x = "Depth layer",
    detail_depth_y = "Breadth",
    detail_profile_missing = "No depth profile is available for this cascade.",
    internal_title_midv = "Cases where mid-tier creators are more likely enough",
    internal_title_bigv = "Cases that look more dependent on Big-V creators",
    internal_title_rising = "Rising-star candidates",
    internal_title_feature = "Type differences after controlling for size",
    internal_title_embedding = "Embedding extension performance",
    analysis_intro = "This page translates your budget question into three ranked lists:",
    analysis_bullets = c(
      "Mid-tier likely enough: lower or medium root followers, but stronger depth, structural virality, similarity, and reciprocity.",
      "More dependent on Big-V: higher root followers, larger first-wave breadth, shallower and more concentrated spread.",
      "Rising stars: follower base is still modest, but the cascade travels deeper and sustains downstream relay.",
      "Rule-based label: `Mid-tier is enough / Test with mid-tier first / Big-V recommended`, based on the gap between efficiency score and Big-V need score."
    ),
    analysis_sentence = "Under the current filters, the average efficiency score for viral cascades is about %s; the average Big-V need score for broadcast cascades is about %s.",
    analysis_distribution = "Current recommendation mix: %s",
    feature_gap_cols = c("feature", "n", "mean_broadcast", "mean_viral", "beta_viral_control_size", "p_value"),
    embedding_cols = c("model", "rows", "folds", "accuracy_mean", "auc_mean", "spec"),
    midv_reason = "Depth, reciprocity, and interest match are stronger, so a mid-tier strategy is more likely to deliver better budget efficiency.",
    bigv_reason = "The cascade looks more dependent on first-wave burst and root-node amplification, so a head creator is more likely needed.",
    test_reason = "The two mechanisms are close. Start with a smaller mid-tier test, then upgrade to Big-V only if early breadth stays weak.",
    rec_midv = "Mid-tier is enough",
    rec_bigv = "Big-V recommended",
    rec_test = "Test with mid-tier first",
    root_followers_plot = "root followers",
    no_classified = "No classified cascades under the current filters.",
    png_no_graph = "Edge data not found; graph export is unavailable.",
    png_no_profile = "No depth profile is available for this cascade."
  ),
  zh = list(
    app_title = "Cascade 传播仪表盘",
    language = "语言",
    sidebar_title = "全局筛选",
    type_filter = "Cascade 类型",
    type_all = "全部",
    type_broadcast = "广播型",
    type_viral = "病毒型",
    type_unclassified = "未分类",
    classified_only = "只看已分类 cascade",
    size_range = "size 范围",
    depth_range = "depth 范围",
    search_tweet = "搜索 tweet_id",
    sort_by = "缩略图排序",
    sort_efficiency = "ROI / 中V效率优先",
    sort_bigv = "大V需求优先",
    sort_rising = "新星潜力优先",
    cards_per_page = "矩阵每页显示",
    matrix_page = "缩略图页码",
    thumb_note = "缩略图矩阵中的每张卡片都是一个 cascade 的迷你层级分布图。点击卡片或总览散点中的点，会跳到详情页。",
    project_root = "项目根目录",
    data_model_path = "数据模型路径",
    data_model_missing = "不可用；详情页将只依赖打包边数据重建图",
    tab_overview = "总览",
    tab_matrix = "缩略图矩阵",
    tab_detail = "Cascade 详情",
    tab_internal = "内部分析",
    metric_total = "全部 cascade",
    metric_filtered = "当前筛选结果",
    metric_types = "广播型 / 病毒型",
    metric_cv = "Embedding + 基线 AUC",
    scatter_title = "形态图：depth vs breadth",
    scatter_x = "log(1 + depth)",
    scatter_y = "log(1 + max breadth)",
    hover_tweet = "tweet_id",
    hover_type = "类型",
    hover_size = "size",
    hover_depth = "depth",
    hover_breadth = "最大宽度",
    hover_root_followers = "根节点粉丝数",
    compare_title = "当前筛选下的平均结构指标",
    compare_depth = "depth",
    compare_breadth = "max_breadth",
    compare_virality = "structural_virality",
    overview_cols = c("tweet_id", "类型", "size", "depth", "max_breadth", "structural_virality", "root_followers", "efficiency_score", "bigv_need_score", "rising_star_score"),
    matrix_status = "共 %s 个 cascade；当前第 %s / %s 页。",
    thumb_size = "size",
    thumb_depth = "depth",
    thumb_breadth = "breadth",
    thumb_root_followers = "根节点粉丝数",
    thumb_efficiency = "效率分数",
    thumb_rising = "新星分数",
    detail_select = "选择 tweet_id",
    detail_download_png = "导出当前 PNG",
    detail_download_csv = "导出当前 CSV",
    detail_metric_names = c("tweet_id", "cascade_type", "预算建议", "size", "depth", "max_breadth", "structural_virality", "root_followers", "avg_interest_similarity", "avg_number_exposures", "mean_reciprocity"),
    detail_metric_col1 = "指标",
    detail_metric_col2 = "数值",
    detail_graph_missing = "未找到边数据，无法重建该传播图。",
    detail_graph_title = "传播图",
    detail_depth_title = "层级分布",
    detail_depth_x = "层级",
    detail_depth_y = "宽度",
    detail_profile_missing = "没有该 cascade 的 depth profile。",
    internal_title_midv = "中V 更可能够用的案例",
    internal_title_bigv = "更像必须依赖大V 的案例",
    internal_title_rising = "潜力新星候选",
    internal_title_feature = "控制 size 后的类型差异",
    internal_title_embedding = "Embedding 扩展效果",
    analysis_intro = "这页把你的预算问题翻成三个榜单：",
    analysis_bullets = c(
      "中V 更可能够用：低/中 root_followers，但 depth、structural virality、兴趣匹配和互惠都更高。",
      "更像必须依赖大V：root_followers 高、第一层宽度高、传播更浅更集中。",
      "潜力新星：当前粉丝不高，但传播深度和后续接力能力很强。",
      "规则标签：`中V够了 / 先中V测试再决定 / 建议大V`，基于效率分数与大V需求分数的差值。"
    ),
    analysis_sentence = "当前筛选下，viral 的平均效率分数约为 %s；broadcast 的大V需求分数约为 %s。",
    analysis_distribution = "当前标签分布：%s",
    feature_gap_cols = c("feature", "n", "mean_broadcast", "mean_viral", "beta_viral_control_size", "p_value"),
    embedding_cols = c("model", "rows", "folds", "accuracy_mean", "auc_mean", "spec"),
    midv_reason = "深传播、互惠和兴趣匹配更强，优先用中V争取更高单位预算效率。",
    bigv_reason = "第一层爆发和根节点带动需求更高，更像需要头部账号快速打宽首轮传播。",
    test_reason = "两种机制接近，建议先用中V做小预算测试，再根据早期 breadth/depth 决定是否升级到大V。",
    rec_midv = "中V够了",
    rec_bigv = "建议大V",
    rec_test = "先中V测试再决定",
    root_followers_plot = "根节点粉丝数",
    no_classified = "当前筛选下没有已分类 cascade。",
    png_no_graph = "未找到边数据，无法导出 graph 图。",
    png_no_profile = "没有该 cascade 的 depth profile。"
  )
)

tr <- function(lang, key) {
  lang <- if (is.null(lang) || !lang %in% names(i18n)) "en" else lang
  i18n[[lang]][[key]]
}

type_label <- function(x, lang) {
  if (lang == "zh") {
    ifelse(
      x == "broadcast", tr(lang, "type_broadcast"),
      ifelse(x == "viral", tr(lang, "type_viral"), ifelse(x == "unclassified", tr(lang, "type_unclassified"), x))
    )
  } else {
    x
  }
}

decision_label_display <- function(x, lang) {
  ifelse(
    x == "midv_enough", tr(lang, "rec_midv"),
    ifelse(x == "need_bigv", tr(lang, "rec_bigv"), tr(lang, "rec_test"))
  )
}

decision_reason_display <- function(x, lang) {
  ifelse(
    x == "midv_enough", tr(lang, "midv_reason"),
    ifelse(x == "need_bigv", tr(lang, "bigv_reason"), tr(lang, "test_reason"))
  )
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

cascade_metrics <- read_csv_safe(file.path(output_dir, "cascade_metrics.csv"))
depth_profile <- read_csv_safe(file.path(output_dir, "cascade_depth_profile.csv"))
embedding_cv <- read_csv_safe(file.path(output_dir, "embedding_cv_comparison.csv"))
feature_gap <- read_csv_safe(file.path(output_dir, "feature_type_differences_control_size.csv"))
cascade_edges_csv <- file.path(output_dir, "cascade_edges.csv")

if (!"tweet_id" %in% names(cascade_metrics)) stop("`cascade_metrics.csv` is missing tweet_id.")
if (!"tweet_id" %in% names(depth_profile)) stop("`cascade_depth_profile.csv` is missing tweet_id.")

cascade_metrics$tweet_id <- as.character(cascade_metrics$tweet_id)
depth_profile$tweet_id <- as.character(depth_profile$tweet_id)

for (nm in setdiff(names(cascade_metrics), c("tweet_id", "cascade_type"))) {
  cascade_metrics[[nm]] <- coerce_numeric(cascade_metrics[[nm]])
}
for (nm in setdiff(names(depth_profile), "tweet_id")) {
  depth_profile[[nm]] <- coerce_numeric(depth_profile[[nm]])
}

cascade_metrics$log_root_followers <- log1p(pmax(cascade_metrics$root_followers, 0))
cascade_metrics$log_size <- log1p(pmax(cascade_metrics$size, 0))
cascade_metrics$efficiency_score <- (
  scale01(cascade_metrics$depth_graph) +
  scale01(cascade_metrics$structural_virality) +
  scale01(cascade_metrics$avg_interest_similarity) +
  scale01(cascade_metrics$mean_reciprocity) -
  scale01(cascade_metrics$log_root_followers)
)
cascade_metrics$bigv_need_score <- (
  scale01(cascade_metrics$max_breadth) +
  scale01(cascade_metrics$root_share) +
  scale01(cascade_metrics$log_root_followers) -
  scale01(cascade_metrics$depth_graph)
)
cascade_metrics$rising_star_score <- (
  scale01(cascade_metrics$depth_graph) +
  scale01(cascade_metrics$structural_virality) +
  scale01(cascade_metrics$avg_interest_similarity) +
  scale01(cascade_metrics$avg_number_exposures) -
  scale01(cascade_metrics$log_root_followers)
)
cascade_metrics$decision_gap <- cascade_metrics$efficiency_score - cascade_metrics$bigv_need_score
cascade_metrics$budget_decision <- decision_label(cascade_metrics$decision_gap)
cascade_metrics$budget_reason <- decision_reason(cascade_metrics$budget_decision)

edge_data <- NULL
data_model_path <- pick_existing_path(c(
  Sys.getenv("DATA_MODEL_PATH", unset = ""),
  file.path(project_root, "data_model.Rdata"),
  default_external_path
))

if (file.exists(cascade_edges_csv)) {
  edge_df <- read_csv_safe(cascade_edges_csv)
  edge_df$tweet_id <- as.character(edge_df$tweet_id)
  edge_df$parent_id <- as.character(edge_df$parent_id)
  edge_df$retweeter_id <- as.character(edge_df$retweeter_id)
  edge_df$time_elapsed <- coerce_numeric(edge_df$time_elapsed)
  edge_data <- split(edge_df, edge_df$tweet_id)
} else if (!is.na(data_model_path)) {
  env <- new.env(parent = emptyenv())
  load(data_model_path, envir = env)
  if (exists("data", envir = env)) {
    raw_data <- get("data", envir = env)
    raw_data$tweet_id <- as.character(raw_data$tweet_id)
    raw_data$retweeter_id <- as.character(raw_data$retweeter_id)
    raw_data$parent_id <- as.character(raw_data$parent_id)
    if ("retweeted" %in% names(raw_data)) {
      rv <- sort(unique(stats::na.omit(coerce_numeric(raw_data$retweeted))))
      if (length(rv) > 0 && all(rv %in% c(0, 1))) {
        raw_data <- raw_data[coerce_numeric(raw_data$retweeted) == 1, , drop = FALSE]
      }
    }
    raw_data <- raw_data[
      !is.na(raw_data$tweet_id) &
        !is.na(raw_data$retweeter_id) &
        !is.na(raw_data$parent_id) &
        raw_data$retweeter_id != raw_data$parent_id,
      c("tweet_id", "parent_id", "retweeter_id", "time_elapsed"),
      drop = FALSE
    ]
    raw_data$time_elapsed <- coerce_numeric(raw_data$time_elapsed)
    edge_data <- split(raw_data, raw_data$tweet_id)
  }
}

build_graph_for_tweet <- function(tweet_id) {
  if (is.null(edge_data) || !tweet_id %in% names(edge_data)) return(NULL)
  df <- edge_data[[tweet_id]]
  df <- unique(df[, c("parent_id", "retweeter_id", "time_elapsed"), drop = FALSE])
  names(df) <- c("from", "to", "time_elapsed")
  df <- df[df$from != df$to & !is.na(df$from) & !is.na(df$to), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  g <- graph_from_data_frame(df[, c("from", "to")], directed = TRUE)
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  list(graph = g, edges = df)
}

make_thumb_svg <- function(tweet_id, width = 180, height = 70) {
  df <- depth_profile[depth_profile$tweet_id == tweet_id, c("depth", "breadth"), drop = FALSE]
  if (nrow(df) == 0) {
    return(
      sprintf(
        "<svg width='%d' height='%d' viewBox='0 0 %d %d'><rect x='0' y='0' width='%d' height='%d' fill='#f3f4f6'/><text x='%d' y='%d' text-anchor='middle' fill='#9ca3af' font-size='12'>No profile</text></svg>",
        width, height, width, height, width, height, width / 2, height / 2
      )
    )
  }
  max_b <- max(df$breadth, na.rm = TRUE)
  n <- nrow(df)
  bar_w <- max((width - 12) / max(n, 1), 4)
  fills <- ifelse(
    cascade_metrics$cascade_type[match(tweet_id, cascade_metrics$tweet_id)] == "viral",
    "#d62728",
    ifelse(cascade_metrics$cascade_type[match(tweet_id, cascade_metrics$tweet_id)] == "broadcast", "#1f77b4", "#6b7280")
  )
  bars <- character(n)
  for (i in seq_len(n)) {
    x <- 6 + (i - 1) * bar_w
    h <- if (max_b <= 0) 2 else (df$breadth[i] / max_b) * (height - 16)
    y <- height - h - 6
    bars[i] <- sprintf(
      "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='1' fill='%s' fill-opacity='0.85'></rect>",
      x, y, max(bar_w - 1.5, 2), h, fills
    )
  }
  sprintf(
    "<svg width='%d' height='%d' viewBox='0 0 %d %d'><rect x='0' y='0' width='%d' height='%d' rx='8' fill='#f9fafb' stroke='#e5e7eb'/>%s</svg>",
    width, height, width, height, width, height, paste(bars, collapse = "")
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f6f8fb; }
      .app-title { margin: 0; }
      .top-bar {
        display:flex; justify-content:space-between; align-items:center;
        gap: 16px; margin: 12px 0 10px 0;
      }
      .lang-wrap { min-width: 220px; margin-left: auto; }
      .side-note { font-size: 12px; color: #6b7280; line-height: 1.5; }
      .metric-card {
        background: white; border: 1px solid #e5e7eb; border-radius: 12px;
        padding: 14px 16px; margin-bottom: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.03);
      }
      .metric-label { font-size: 12px; color: #6b7280; text-transform: uppercase; }
      .metric-value { font-size: 26px; font-weight: 700; }
      .thumb-grid {
        display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
        gap: 12px;
      }
      .thumb-card {
        background: white; border: 1px solid #e5e7eb; border-radius: 12px;
        padding: 10px 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.03);
      }
      .thumb-header { display:flex; justify-content:space-between; gap:8px; align-items:center; margin-bottom: 8px; }
      .thumb-id { font-weight:600; font-size:12px; color:#111827; max-width:140px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
      .thumb-type { font-size:11px; padding:2px 8px; border-radius:999px; color:white; }
      .type-broadcast { background:#1f77b4; }
      .type-viral { background:#d62728; }
      .type-unclassified { background:#6b7280; }
      .thumb-meta { font-size:12px; color:#4b5563; line-height:1.45; margin-top:6px; }
      .thumb-link { text-decoration:none !important; color:inherit !important; }
      .thumb-link:hover .thumb-card { border-color:#2563eb; box-shadow:0 3px 12px rgba(37,99,235,0.10); }
      .detail-table th { width: 220px; }
      .section-card {
        background:white; border:1px solid #e5e7eb; border-radius: 12px;
        padding: 16px; margin-bottom: 14px;
      }
      .decision-chip {
        display:inline-block; padding:4px 10px; border-radius:999px; color:white;
        font-size:12px; font-weight:600; margin-bottom:8px;
      }
      .decision-midv { background:#059669; }
      .decision-test { background:#d97706; }
      .decision-bigv { background:#dc2626; }
    "))
  ),
  div(
    class = "top-bar",
    uiOutput("page_title"),
    div(
      class = "lang-wrap",
      selectInput("lang", "Language", choices = c("English" = "en", "中文" = "zh"), selected = "en", width = "100%")
    )
  ),
  uiOutput("app_body")
)

server <- function(input, output, session) {
  current_lang <- reactive({
    if (is.null(input$lang) || !input$lang %in% c("en", "zh")) "en" else input$lang
  })

  sort_metric_map <- reactive({
    lang <- current_lang()
    setNames(
      c("efficiency_score", "bigv_need_score", "rising_star_score", "size", "depth_graph", "max_breadth", "structural_virality", "root_followers"),
      c(tr(lang, "sort_efficiency"), tr(lang, "sort_bigv"), tr(lang, "sort_rising"), "size", "depth", "max_breadth", "structural_virality", "root_followers")
    )
  })

  selected_tweet <- reactiveVal(cascade_metrics$tweet_id[1])

  output$page_title <- renderUI({
    tags$h2(class = "app-title", tr(current_lang(), "app_title"))
  })

  output$app_body <- renderUI({
    lang <- current_lang()
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(tr(lang, "sidebar_title")),
        selectInput(
          "type_filter",
          tr(lang, "type_filter"),
          choices = stats::setNames(c("all", "broadcast", "viral", "unclassified"), c(tr(lang, "type_all"), tr(lang, "type_broadcast"), tr(lang, "type_viral"), tr(lang, "type_unclassified"))),
          selected = "all"
        ),
        checkboxInput("classified_only", tr(lang, "classified_only"), value = TRUE),
        sliderInput(
          "size_filter", tr(lang, "size_range"),
          min = floor(min(cascade_metrics$size, na.rm = TRUE)),
          max = ceiling(max(cascade_metrics$size, na.rm = TRUE)),
          value = c(floor(min(cascade_metrics$size, na.rm = TRUE)), ceiling(max(cascade_metrics$size, na.rm = TRUE)))
        ),
        sliderInput(
          "depth_filter", tr(lang, "depth_range"),
          min = floor(min(cascade_metrics$depth_graph, na.rm = TRUE)),
          max = ceiling(max(cascade_metrics$depth_graph, na.rm = TRUE)),
          value = c(floor(min(cascade_metrics$depth_graph, na.rm = TRUE)), ceiling(max(cascade_metrics$depth_graph, na.rm = TRUE)))
        ),
        textInput("tweet_search", tr(lang, "search_tweet"), value = ""),
        selectInput("sort_by", tr(lang, "sort_by"), choices = sort_metric_map(), selected = "efficiency_score"),
        numericInput("cards_per_page", tr(lang, "cards_per_page"), value = 48, min = 12, max = 120, step = 12),
        numericInput("matrix_page", tr(lang, "matrix_page"), value = 1, min = 1, step = 1),
        hr(),
        p(class = "side-note", tr(lang, "thumb_note")),
        p(class = "side-note", paste(tr(lang, "project_root"), ":", project_root)),
        p(class = "side-note", paste(tr(lang, "data_model_path"), ":", data_model_path %||% tr(lang, "data_model_missing")))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "main_tabs",
          tabPanel(
            tr(lang, "tab_overview"),
            br(),
            fluidRow(
              column(3, uiOutput("metric_total")),
              column(3, uiOutput("metric_filtered")),
              column(3, uiOutput("metric_types")),
              column(3, uiOutput("metric_cv"))
            ),
            fluidRow(
              column(8, div(class = "section-card", plotlyOutput("shape_scatter", height = "420px"))),
              column(4, div(class = "section-card", plotlyOutput("type_compare_plot", height = "420px")))
            ),
            fluidRow(
              column(12, div(class = "section-card", DTOutput("overview_table")))
            )
          ),
          tabPanel(
            tr(lang, "tab_matrix"),
            br(),
            div(class = "section-card", strong(textOutput("matrix_status", inline = TRUE)), uiOutput("thumb_grid"))
          ),
          tabPanel(
            tr(lang, "tab_detail"),
            br(),
            fluidRow(
              column(
                4,
                div(
                  class = "section-card",
                  selectizeInput("detail_tweet", tr(lang, "detail_select"), choices = cascade_metrics$tweet_id, selected = selected_tweet(), options = list(maxOptions = 2000)),
                  uiOutput("detail_decision"),
                  fluidRow(
                    column(6, downloadButton("download_detail_png", tr(lang, "detail_download_png"))),
                    column(6, downloadButton("download_detail_csv", tr(lang, "detail_download_csv")))
                  ),
                  br(),
                  tableOutput("detail_metrics")
                )
              ),
              column(
                8,
                div(class = "section-card", plotOutput("detail_graph_plot", height = "420px")),
                div(class = "section-card", plotOutput("detail_depth_plot", height = "260px"))
              )
            )
          ),
          tabPanel(
            tr(lang, "tab_internal"),
            br(),
            fluidRow(column(12, div(class = "section-card", uiOutput("analysis_notes")))),
            fluidRow(column(12, div(class = "section-card", h4(tr(lang, "internal_title_midv")), DTOutput("midv_table")))),
            fluidRow(column(12, div(class = "section-card", h4(tr(lang, "internal_title_bigv")), DTOutput("bigv_table")))),
            fluidRow(column(12, div(class = "section-card", h4(tr(lang, "internal_title_rising")), DTOutput("rising_table")))),
            fluidRow(
              column(6, div(class = "section-card", h4(tr(lang, "internal_title_feature")), DTOutput("feature_gap_table"))),
              column(6, div(class = "section-card", h4(tr(lang, "internal_title_embedding")), DTOutput("embedding_table")))
            )
          )
        )
      )
    )
  })

  filtered_metrics <- reactive({
    df <- cascade_metrics
    if (isTRUE(input$classified_only)) {
      df <- df[df$cascade_type %in% c("broadcast", "viral"), , drop = FALSE]
    }
    if (!is.null(input$type_filter) && input$type_filter != "all") {
      df <- df[df$cascade_type == input$type_filter, , drop = FALSE]
    }
    df <- df[
      df$size >= input$size_filter[1] & df$size <= input$size_filter[2] &
        df$depth_graph >= input$depth_filter[1] & df$depth_graph <= input$depth_filter[2],
      ,
      drop = FALSE
    ]
    if (!is.null(input$tweet_search) && length(input$tweet_search) == 1 && nzchar(input$tweet_search)) {
      key <- trimws(input$tweet_search)
      df <- df[grepl(key, df$tweet_id, fixed = TRUE), , drop = FALSE]
    }
    df
  })

  observe({
    df <- filtered_metrics()
    choices <- df$tweet_id
    current <- selected_tweet()
    updateSelectizeInput(session, "detail_tweet", choices = choices, selected = if (current %in% choices) current else choices[1], server = TRUE)
  })

  observeEvent(input$detail_tweet, {
    req(input$detail_tweet)
    selected_tweet(input$detail_tweet)
  }, ignoreInit = TRUE)

  observeEvent(input$selected_tweet_click, {
    selected_tweet(input$selected_tweet_click)
    updateSelectizeInput(session, "detail_tweet", selected = input$selected_tweet_click, server = TRUE)
    updateTabsetPanel(session, "main_tabs", selected = tr(current_lang(), "tab_detail"))
  })

  observeEvent(plotly::event_data("plotly_click", source = "shape_scatter"), {
    click <- plotly::event_data("plotly_click", source = "shape_scatter")
    tweet_id <- click$customdata %||% NA_character_
    if (is.character(tweet_id) && length(tweet_id) == 1 && !is.na(tweet_id)) {
      selected_tweet(tweet_id)
      updateSelectizeInput(session, "detail_tweet", selected = tweet_id, server = TRUE)
      updateTabsetPanel(session, "main_tabs", selected = tr(current_lang(), "tab_detail"))
    }
  })

  output$metric_total <- renderUI({
    lang <- current_lang()
    div(class = "metric-card",
        div(class = "metric-label", tr(lang, "metric_total")),
        div(class = "metric-value", fmt_int(nrow(cascade_metrics))))
  })

  output$metric_filtered <- renderUI({
    lang <- current_lang()
    df <- filtered_metrics()
    div(class = "metric-card",
        div(class = "metric-label", tr(lang, "metric_filtered")),
        div(class = "metric-value", fmt_int(nrow(df))))
  })

  output$metric_types <- renderUI({
    lang <- current_lang()
    df <- filtered_metrics()
    b <- sum(df$cascade_type == "broadcast", na.rm = TRUE)
    v <- sum(df$cascade_type == "viral", na.rm = TRUE)
    div(class = "metric-card",
        div(class = "metric-label", tr(lang, "metric_types")),
        div(class = "metric-value", paste0(fmt_int(b), " / ", fmt_int(v))))
  })

  output$metric_cv <- renderUI({
    lang <- current_lang()
    auc <- embedding_cv$auc_mean[embedding_cv$spec == "baseline_plus_embedding"][1] %||% NA_real_
    div(class = "metric-card",
        div(class = "metric-label", tr(lang, "metric_cv")),
        div(class = "metric-value", fmt_num(auc, 3)))
  })

  output$shape_scatter <- renderPlotly({
    lang <- current_lang()
    df <- filtered_metrics()
    req(nrow(df) > 0)
    cols <- c(broadcast = "#1f77b4", viral = "#d62728", unclassified = "#6b7280")
    p <- plot_ly(
      data = df,
      x = ~log1p(depth_graph),
      y = ~log1p(max_breadth),
      type = "scatter",
      mode = "markers",
      color = ~cascade_type,
      colors = cols,
      source = "shape_scatter",
      customdata = ~tweet_id,
      marker = list(size = ~pmin(8 + log1p(size) * 2.2, 22), opacity = 0.78),
      text = ~paste0(
        tr(lang, "hover_tweet"), ": ", tweet_id,
        "<br>", tr(lang, "hover_type"), ": ", type_label(cascade_type, lang),
        "<br>", tr(lang, "hover_size"), ": ", fmt_int(size),
        "<br>", tr(lang, "hover_depth"), ": ", fmt_num(depth_graph, 0),
        "<br>", tr(lang, "hover_breadth"), ": ", fmt_num(max_breadth, 0),
        "<br>", tr(lang, "hover_root_followers"), ": ", fmt_int(root_followers)
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = tr(lang, "scatter_title"),
        xaxis = list(title = tr(lang, "scatter_x")),
        yaxis = list(title = tr(lang, "scatter_y"))
      )
    event_register(p, "plotly_click")
  })

  output$type_compare_plot <- renderPlotly({
    lang <- current_lang()
    df <- filtered_metrics()
    req(nrow(df) > 0)
    show_df <- df[df$cascade_type %in% c("broadcast", "viral"), , drop = FALSE]
    req(nrow(show_df) > 0)
    agg <- stats::aggregate(
      cbind(depth_graph, max_breadth, structural_virality) ~ cascade_type,
      data = show_df,
      FUN = mean
    )
    long <- rbind(
      data.frame(cascade_type = agg$cascade_type, metric = tr(lang, "compare_depth"), value = agg$depth_graph),
      data.frame(cascade_type = agg$cascade_type, metric = tr(lang, "compare_breadth"), value = agg$max_breadth),
      data.frame(cascade_type = agg$cascade_type, metric = tr(lang, "compare_virality"), value = agg$structural_virality)
    )
    long$cascade_type <- type_label(long$cascade_type, lang)
    plot_ly(long, x = ~metric, y = ~value, color = ~cascade_type, type = "bar") %>%
      layout(barmode = "group", title = tr(lang, "compare_title"))
  })

  output$overview_table <- renderDT({
    lang <- current_lang()
    df <- filtered_metrics()
    req(nrow(df) > 0)
    show <- df[, c("tweet_id", "cascade_type", "size", "depth_graph", "max_breadth", "structural_virality", "root_followers", "efficiency_score", "bigv_need_score", "rising_star_score")]
    show$cascade_type <- type_label(show$cascade_type, lang)
    names(show) <- tr(lang, "overview_cols")
    datatable(show, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  paged_metrics <- reactive({
    df <- filtered_metrics()
    req(nrow(df) > 0)
    sort_col <- input$sort_by %||% "efficiency_score"
    ord <- order(df[[sort_col]], decreasing = TRUE, na.last = TRUE)
    df <- df[ord, , drop = FALSE]
    per_page <- max(1, input$cards_per_page)
    total_pages <- max(1, ceiling(nrow(df) / per_page))
    page <- min(max(1, input$matrix_page), total_pages)
    list(
      data = df[((page - 1) * per_page + 1):min(page * per_page, nrow(df)), , drop = FALSE],
      total = nrow(df),
      page = page,
      total_pages = total_pages
    )
  })

  output$matrix_status <- renderText({
    lang <- current_lang()
    pg <- paged_metrics()
    sprintf(tr(lang, "matrix_status"), fmt_int(pg$total), pg$page, pg$total_pages)
  })

  output$thumb_grid <- renderUI({
    lang <- current_lang()
    pg <- paged_metrics()
    df <- pg$data
    req(nrow(df) > 0)
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      type_class <- paste0("type-", row$cascade_type)
      tags$a(
        href = "#",
        class = "thumb-link",
        onclick = sprintf(
          "Shiny.setInputValue('selected_tweet_click','%s',{priority:'event'}); return false;",
          row$tweet_id
        ),
        div(
          class = "thumb-card",
          div(class = "thumb-header",
              span(class = "thumb-id", row$tweet_id),
              span(class = paste("thumb-type", type_class), type_label(row$cascade_type, lang))),
          HTML(make_thumb_svg(row$tweet_id)),
          div(
            class = "thumb-meta",
            HTML(paste0(
              tr(lang, "thumb_size"), ": <b>", fmt_int(row$size), "</b><br>",
              tr(lang, "thumb_depth"), ": <b>", fmt_int(row$depth_graph), "</b> | ", tr(lang, "thumb_breadth"), ": <b>", fmt_int(row$max_breadth), "</b><br>",
              tr(lang, "thumb_root_followers"), ": <b>", fmt_int(row$root_followers), "</b><br>",
              tr(lang, "thumb_efficiency"), ": <b>", fmt_num(row$efficiency_score, 2), "</b> | ", tr(lang, "thumb_rising"), ": <b>", fmt_num(row$rising_star_score, 2), "</b>"
            ))
          )
        )
      )
    })
    div(class = "thumb-grid", cards)
  })

  current_detail <- reactive({
    tweet_id <- selected_tweet()
    df <- cascade_metrics[cascade_metrics$tweet_id == tweet_id, , drop = FALSE]
    req(nrow(df) == 1)
    df[1, , drop = FALSE]
  })

  output$detail_decision <- renderUI({
    lang <- current_lang()
    row <- current_detail()
    chip_class <- if (row$budget_decision == "midv_enough") {
      "decision-midv"
    } else if (row$budget_decision == "need_bigv") {
      "decision-bigv"
    } else {
      "decision-test"
    }
    tagList(
      div(class = paste("decision-chip", chip_class), decision_label_display(row$budget_decision, lang)),
      p(style = "font-size:12px;color:#4b5563;margin:0 0 10px 0;", decision_reason_display(row$budget_reason, lang))
    )
  })

  output$detail_metrics <- renderTable({
    lang <- current_lang()
    row <- current_detail()
    data.frame(
      metric = tr(lang, "detail_metric_names"),
      value = c(
        row$tweet_id,
        type_label(row$cascade_type, lang),
        decision_label_display(row$budget_decision, lang),
        fmt_int(row$size),
        fmt_int(row$depth_graph),
        fmt_int(row$max_breadth),
        fmt_num(row$structural_virality, 2),
        fmt_int(row$root_followers),
        fmt_num(row$avg_interest_similarity, 3),
        fmt_num(row$avg_number_exposures, 3),
        fmt_num(row$mean_reciprocity, 3)
      ),
      stringsAsFactors = FALSE
    ) |> stats::setNames(c(tr(lang, "detail_metric_col1"), tr(lang, "detail_metric_col2")))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$detail_graph_plot <- renderPlot({
    lang <- current_lang()
    row <- current_detail()
    obj <- build_graph_for_tweet(row$tweet_id)
    validate(need(!is.null(obj), tr(lang, "detail_graph_missing")))
    g <- obj$graph
    roots <- V(g)$name[degree(g, mode = "in") == 0]
    if (length(roots) == 0) roots <- V(g)$name[1]
    lay <- layout_as_tree(g, root = roots, circular = FALSE)
    cols <- ifelse(V(g)$name %in% roots, "#f59e0b", "#2563eb")
    par(mar = c(0, 0, 3, 0))
    plot(
      g,
      layout = lay,
      vertex.size = pmax(5, 12 - log1p(vcount(g))),
      vertex.label = NA,
      vertex.color = cols,
      edge.arrow.size = 0.25,
      edge.color = "#94a3b8",
      main = paste0(tr(lang, "detail_graph_title"), ": ", row$tweet_id)
    )
  })

  output$detail_depth_plot <- renderPlot({
    lang <- current_lang()
    row <- current_detail()
    df <- depth_profile[depth_profile$tweet_id == row$tweet_id, , drop = FALSE]
    validate(need(nrow(df) > 0, tr(lang, "detail_profile_missing")))
    col <- if (row$cascade_type == "viral") "#d62728" else if (row$cascade_type == "broadcast") "#1f77b4" else "#6b7280"
    par(mar = c(4, 4, 2, 1))
    barplot(
      height = df$breadth,
      names.arg = df$depth,
      col = col,
      border = NA,
      xlab = tr(lang, "detail_depth_x"),
      ylab = tr(lang, "detail_depth_y"),
      main = tr(lang, "detail_depth_title")
    )
    grid(nx = NA, ny = NULL, lty = 3, col = "gray80")
  })

  output$analysis_notes <- renderUI({
    lang <- current_lang()
    df <- filtered_metrics()
    show_df <- df[df$cascade_type %in% c("broadcast", "viral"), , drop = FALSE]
    if (nrow(show_df) == 0) {
      return(tags$p(tr(lang, "no_classified")))
    }
    midv_enough <- mean(show_df$efficiency_score[show_df$cascade_type == "viral"], na.rm = TRUE)
    bigv_need <- mean(show_df$bigv_need_score[show_df$cascade_type == "broadcast"], na.rm = TRUE)
    decision_counts <- sort(table(df$budget_decision), decreasing = TRUE)
    decision_counts_text <- paste(
      decision_label_display(names(decision_counts), lang),
      fmt_int(as.numeric(decision_counts)),
      sep = "=",
      collapse = if (lang == "zh") "；" else "; "
    )
    tags$div(
      tags$p(tr(lang, "analysis_intro")),
      tags$ul(lapply(tr(lang, "analysis_bullets"), tags$li)),
      tags$p(
        sprintf(tr(lang, "analysis_sentence"), fmt_num(midv_enough, 2), fmt_num(bigv_need, 2))
      ),
      tags$p(sprintf(tr(lang, "analysis_distribution"), decision_counts_text))
    )
  })

  output$midv_table <- renderDT({
    lang <- current_lang()
    df <- filtered_metrics()
    df <- df[df$budget_decision == "midv_enough", , drop = FALSE]
    req(nrow(df) > 0)
    df <- df[order(df$efficiency_score, decreasing = TRUE), ]
    show <- df[, c("tweet_id", "cascade_type", "size", "depth_graph", "structural_virality", "root_followers", "avg_interest_similarity", "avg_number_exposures", "mean_reciprocity", "budget_decision", "efficiency_score")]
    show$cascade_type <- type_label(show$cascade_type, lang)
    show$budget_decision <- decision_label_display(show$budget_decision, lang)
    datatable(utils::head(show, 30), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$bigv_table <- renderDT({
    lang <- current_lang()
    df <- filtered_metrics()
    req(nrow(df) > 0)
    df <- df[df$budget_decision == "need_bigv", , drop = FALSE]
    req(nrow(df) > 0)
    df <- df[order(df$bigv_need_score, decreasing = TRUE), ]
    show <- df[, c("tweet_id", "cascade_type", "size", "max_breadth", "root_followers", "root_share", "depth_graph", "budget_decision", "bigv_need_score")]
    show$cascade_type <- type_label(show$cascade_type, lang)
    show$budget_decision <- decision_label_display(show$budget_decision, lang)
    datatable(utils::head(show, 30), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$rising_table <- renderDT({
    lang <- current_lang()
    df <- filtered_metrics()
    req(nrow(df) > 0)
    df <- df[order(df$rising_star_score, decreasing = TRUE), ]
    show <- df[, c("tweet_id", "cascade_type", "budget_decision", "root_followers", "size", "depth_graph", "structural_virality", "avg_interest_similarity", "avg_number_exposures", "rising_star_score")]
    show$cascade_type <- type_label(show$cascade_type, lang)
    show$budget_decision <- decision_label_display(show$budget_decision, lang)
    datatable(utils::head(show, 30), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$feature_gap_table <- renderDT({
    lang <- current_lang()
    show <- feature_gap
    names(show) <- tr(lang, "feature_gap_cols")
    datatable(show, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$embedding_table <- renderDT({
    lang <- current_lang()
    show <- embedding_cv
    names(show) <- tr(lang, "embedding_cols")
    datatable(show, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$download_detail_png <- downloadHandler(
    filename = function() {
      paste0("cascade_", selected_tweet(), ".png")
    },
    content = function(file) {
      lang <- current_lang()
      row <- current_detail()
      obj <- build_graph_for_tweet(row$tweet_id)
      png(file, width = 1400, height = 900, res = 140)
      old_par <- par(no.readonly = TRUE)
      on.exit({
        par(old_par)
        dev.off()
      }, add = TRUE)
      layout(matrix(c(1, 2), nrow = 2), heights = c(2.3, 1.1))

      if (!is.null(obj)) {
        g <- obj$graph
        roots <- V(g)$name[degree(g, mode = "in") == 0]
        if (length(roots) == 0) roots <- V(g)$name[1]
        lay <- layout_as_tree(g, root = roots, circular = FALSE)
        cols <- ifelse(V(g)$name %in% roots, "#f59e0b", "#2563eb")
        par(mar = c(0, 0, 3, 0))
        plot(
          g,
          layout = lay,
          vertex.size = pmax(5, 12 - log1p(vcount(g))),
          vertex.label = NA,
          vertex.color = cols,
          edge.arrow.size = 0.25,
          edge.color = "#94a3b8",
          main = paste0(tr(lang, "detail_graph_title"), ": ", row$tweet_id, " | ", decision_label_display(row$budget_decision, lang))
        )
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, tr(lang, "png_no_graph"))
      }

      df <- depth_profile[depth_profile$tweet_id == row$tweet_id, , drop = FALSE]
      par(mar = c(4, 4, 2, 1))
      if (nrow(df) > 0) {
        col <- if (row$cascade_type == "viral") "#d62728" else if (row$cascade_type == "broadcast") "#1f77b4" else "#6b7280"
        barplot(
          height = df$breadth,
          names.arg = df$depth,
          col = col,
          border = NA,
          xlab = tr(lang, "detail_depth_x"),
          ylab = tr(lang, "detail_depth_y"),
          main = paste0(tr(lang, "detail_depth_title"), " | ", tr(lang, "root_followers_plot"), "=", fmt_int(row$root_followers))
        )
        grid(nx = NA, ny = NULL, lty = 3, col = "gray80")
      } else {
        plot.new()
        text(0.5, 0.5, tr(lang, "png_no_profile"))
      }
    }
  )

  output$download_detail_csv <- downloadHandler(
    filename = function() {
      paste0("cascade_", selected_tweet(), "_detail.csv")
    },
    content = function(file) {
      lang <- current_lang()
      row <- current_detail()
      obj <- build_graph_for_tweet(row$tweet_id)

      metrics_export <- data.frame(
        record_type = "metric",
        key = c("tweet_id", "cascade_type", "budget_decision", "budget_reason", "size", "depth_graph", "max_breadth", "structural_virality", "root_followers", "avg_interest_similarity", "avg_number_exposures", "mean_reciprocity"),
        value = c(
          row$tweet_id,
          type_label(row$cascade_type, lang),
          decision_label_display(row$budget_decision, lang),
          decision_reason_display(row$budget_reason, lang),
          row$size,
          row$depth_graph,
          row$max_breadth,
          row$structural_virality,
          row$root_followers,
          row$avg_interest_similarity,
          row$avg_number_exposures,
          row$mean_reciprocity
        ),
        from = NA_character_,
        to = NA_character_,
        time_elapsed = NA_real_,
        depth = NA_real_,
        breadth = NA_real_,
        stringsAsFactors = FALSE
      )

      depth_export <- depth_profile[depth_profile$tweet_id == row$tweet_id, c("depth", "breadth"), drop = FALSE]
      if (nrow(depth_export) > 0) {
        depth_export <- data.frame(
          record_type = "depth_profile",
          key = NA_character_,
          value = NA_character_,
          from = NA_character_,
          to = NA_character_,
          time_elapsed = NA_real_,
          depth = depth_export$depth,
          breadth = depth_export$breadth,
          stringsAsFactors = FALSE
        )
      } else {
        depth_export <- metrics_export[0, ]
      }

      if (!is.null(obj) && nrow(obj$edges) > 0) {
        edge_export <- data.frame(
          record_type = "edge",
          key = NA_character_,
          value = NA_character_,
          from = obj$edges$from,
          to = obj$edges$to,
          time_elapsed = obj$edges$time_elapsed,
          depth = NA_real_,
          breadth = NA_real_,
          stringsAsFactors = FALSE
        )
      } else {
        edge_export <- metrics_export[0, ]
      }

      export_df <- rbind(metrics_export, depth_export, edge_export)
      utils::write.csv(export_df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
