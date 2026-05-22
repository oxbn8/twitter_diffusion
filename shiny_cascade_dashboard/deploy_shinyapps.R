app_dir <- normalizePath("C:/Users/lenovo/twitter_diffusion/shiny_cascade_dashboard", winslash = "/", mustWork = TRUE)

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Please install rsconnect first: install.packages('rsconnect')")
}

message("Deploying app from: ", app_dir)
message("If you have not logged in yet, run rsconnect::setAccountInfo(...) first.")

rsconnect::deployApp(
  appDir = app_dir,
  appName = "cascade-shape-dashboard",
  appTitle = "Cascade Shape Dashboard",
  forceUpdate = TRUE
)
