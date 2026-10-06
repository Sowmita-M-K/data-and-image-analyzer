# ================================================================
#  global.R — Setup & wiring for Data and Image Analyzer
# ================================================================
#
# HOW TO RUN THIS APP:
#   This folder uses R Shiny's standard multi-file layout:
#     global.R   (this file — runs once, loads everything)
#     ui.R       (frontend)
#     server.R   (backend / reactive logic)
#     database.R (SQLite storage layer)
#     backend.R  (data & image analysis engine)
#     www/style.css (styling)
#
#   You do NOT need an app.R and you do NOT call shinyApp() yourself —
#   Shiny finds this pattern automatically. To run:
#     1. Open this folder as your working directory in RStudio.
#     2. Open any of global.R / ui.R / server.R.
#     3. Click "Run App" (RStudio detects the multi-file layout), or run:
#          shiny::runApp("path/to/this/folder")
#   Click "Open in Browser" once it launches — avoid the built-in Viewer
#   pane, it can fail to render some elements correctly.
#
#   Run this once before your first launch (tesseract is optional — only
#   needed for OCR on images; everything else works without it):
#     install.packages(c("shiny","shinyjs","DBI","RSQLite","digest",
#                         "ggplot2","DT","gridExtra","png","jpeg"))
#     install.packages("tesseract")
# ================================================================

options(shiny.fullstacktrace = TRUE)   # show full R error traces in console

library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(digest)
library(ggplot2)
library(DT)
library(gridExtra)
library(png)
library(jpeg)

# tesseract (OCR) is optional — the app still runs without it, just skips
# "read a photographed table" and only does image color analysis (and only
# when the user explicitly asks for that).
has_tesseract <- requireNamespace("tesseract", quietly = TRUE)

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
log_msg <- function(...) message("[DataLens] ", ...)

# Shared UI choice list (used on the Role and Profile pages in server.R).
role_choices <- c("Student", "Employee", "Manager / Team Lead", "Teacher / Educator",
                  "Business Owner", "Freelancer", "Researcher", "Other")

# ---- Load the database layer, then initialize it ----
source("database.R")
init_db()
log_msg("Database ready at: ", db_path)

# ---- Load the data/image analysis engine ----
source("backend.R")
