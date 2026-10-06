# ================================================================
#  ui.R — Frontend (application shell) for Data and Image Analyzer
# ================================================================
# This file only defines the outer page shell. Everything the user
# actually sees inside it (splash/login/dashboard/upload/etc.) is
# built dynamically in server.R via renderUI, because those screens
# depend on reactive state (who's logged in, what was uploaded, etc.)
# — that logic can't be split out of the server in plain Shiny without
# converting the app into Shiny modules.
# ================================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/style.css")),
  uiOutput("page_root")
)
