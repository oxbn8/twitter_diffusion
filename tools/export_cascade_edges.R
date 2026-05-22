options(stringsAsFactors = FALSE)

raw_path <- "C:/Users/lenovo/OneDrive - The Chinese University of Hong Kong/Competition& Activity/ulib_data_analysis/twitter_diffusion/原始数据/data_model.Rdata"
out_path <- "C:/Users/lenovo/twitter_diffusion/cascade_shape_outputs/cascade_edges.csv"

if (!file.exists(raw_path)) stop("Missing data_model.Rdata: ", raw_path)
load(raw_path)
if (!exists("data")) stop("Object `data` not found in ", raw_path)

coerce_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

data$tweet_id <- as.character(data$tweet_id)
data$retweeter_id <- as.character(data$retweeter_id)
data$parent_id <- as.character(data$parent_id)

if ("retweeted" %in% names(data)) {
  retweeted_vals <- sort(unique(stats::na.omit(coerce_numeric(data$retweeted))))
  if (length(retweeted_vals) > 0 && all(retweeted_vals %in% c(0, 1))) {
    data <- data[coerce_numeric(data$retweeted) == 1, , drop = FALSE]
  }
}

edge_data <- data[
  !is.na(data$tweet_id) &
    !is.na(data$retweeter_id) &
    !is.na(data$parent_id) &
    data$retweeter_id != data$parent_id,
  c("tweet_id", "parent_id", "retweeter_id", "time_elapsed"),
  drop = FALSE
]
edge_data$time_elapsed <- coerce_numeric(edge_data$time_elapsed)

utils::write.csv(edge_data, out_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("Wrote:", out_path, "\nRows:", nrow(edge_data), "\n")
