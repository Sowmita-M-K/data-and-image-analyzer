# ================================================================
#  server.R — Backend (Shiny reactive logic) for Data and Image Analyzer
# ================================================================
# Wires up the app: authentication flow, page navigation, file uploads,
# calling into backend.R (analysis engine) and database.R (storage)
# functions, and rendering every dynamic screen shown in ui.R.
# ================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    page = "splash",
    user_id = NULL, username = NULL, category = NULL, role = NULL,
    nav = "dashboard",
    current_data = NULL, current_summary = NULL, current_numeric = NULL, current_cat_summaries = NULL,
    current_csv_res = NULL, current_is_ocr = FALSE, pending_image_reason = NULL,
    current_image_stats = NULL, current_file_type = NULL,
    current_original_name = NULL, current_stored_path = NULL,
    selected_history_id = NULL
  )
  
  # Small helper so every observer surfaces its error as a red toast
  # instead of silently crashing the session (this is what was making
  # the screen go blank with no clue why).
  safely <- function(label, expr) {
    tryCatch(
      expr,
      error = function(e) {
        log_msg("ERROR in ", label, ": ", conditionMessage(e))
        showNotification(paste0("[", label, "] ", conditionMessage(e)),
                         type = "error", duration = NULL)
      }
    )
  }
  
  # ---- Splash auto-transition ----
  session$onFlushed(function() {
    shinyjs::delay(1800, { rv$page <- "auth" })
  }, once = TRUE)
  
  # ================================================================
  # PAGE DEFINITIONS
  # ================================================================
  
  splash_page <- function() {
    div(id = "splash_div",
        div(class = "splash-logo", "DataLens"),
        div(class = "splash-sub", "See your data clearly")
    )
  }
  
  auth_page <- function() {
    div(class = "center-box",
        div(class = "splash-logo", style = "font-size:32px;", "DataLens"),
        br(),
        tabsetPanel(
          tabPanel("Login",
                   br(),
                   textInput("login_username", "Username"),
                   passwordInput("login_password", "Password"),
                   actionButton("login_btn", "Log In", class = "btn-primary", width = "100%"),
                   div(style = "color:#f87171; margin-top:10px;", textOutput("login_error"))
          ),
          tabPanel("Register",
                   br(),
                   textInput("reg_username", "Choose a username"),
                   passwordInput("reg_password", "Choose a password"),
                   actionButton("register_btn", "Create Account", class = "btn-primary", width = "100%"),
                   div(style = "color:#f87171; margin-top:10px;", textOutput("register_error"))
          )
        )
    )
  }
  
  role_page <- function() {
    div(class = "center-box",
        h3("Tell us about yourself"),
        p(style = "color:#8b8fa3;", "This helps tailor your dashboard."),
        radioButtons("category_input", "I'm using this for:",
                     choices = c("Personal", "Professional"), inline = TRUE),
        conditionalPanel(
          condition = "input.category_input == 'Professional'",
          selectInput("role_input", "What's your role?", choices = role_choices)
        ),
        actionButton("save_role_btn", "Continue", class = "btn-primary", width = "100%")
    )
  }
  
  app_page <- function() {
    fluidRow(
      column(2,
             div(class = "sidebar-nav",
                 div(class = "app-title", "DataLens"),
                 uiOutput("sidebar_menu")
             )
      ),
      column(10,
             div(class = "main-content", uiOutput("main_content"))
      )
    )
  }
  
  output$page_root <- renderUI({
    log_msg("Rendering page_root, rv$page = ", rv$page)
    tryCatch({
      switch(rv$page,
             "splash" = splash_page(),
             "auth"   = auth_page(),
             "role"   = role_page(),
             "app"    = app_page(),
             splash_page()
      )
    }, error = function(e) {
      log_msg("ERROR in page_root: ", conditionMessage(e))
      div(class = "center-box",
          h3("Something went wrong"),
          p(style = "color:#f87171;", conditionMessage(e)))
    })
  })
  
  # ---- REGISTER ----
  output$register_error <- renderText("")
  observeEvent(input$register_btn, {
    safely("register", {
      res <- register_user(input$reg_username, input$reg_password)
      if (!res$success) {
        output$register_error <- renderText(res$message)
      } else {
        output$register_error <- renderText("")
        rv$user_id <- res$user_id
        rv$username <- trimws(input$reg_username)
        rv$category <- NULL
        rv$role <- NULL
        log_msg("Registered user_id=", res$user_id, " -> going to role page")
        rv$page <- "role"
      }
    })
  })
  
  # ---- LOGIN ----
  output$login_error <- renderText("")
  observeEvent(input$login_btn, {
    safely("login", {
      res <- login_user(input$login_username, input$login_password)
      if (!res$success) {
        output$login_error <- renderText(res$message)
      } else {
        output$login_error <- renderText("")
        rv$user_id  <- res$user_id
        rv$username <- res$username
        rv$category <- res$category
        rv$role     <- res$role
        
        if (is.null(res$category) || is.na(res$category)) {
          log_msg("Logged in user_id=", res$user_id, " -> no category yet, going to role page")
          rv$page <- "role"
        } else {
          log_msg("Logged in user_id=", res$user_id, " -> going straight to app")
          rv$nav <- "dashboard"
          rv$page <- "app"
        }
      }
    })
  })
  
  # ---- ROLE SELECTION ----
  observeEvent(input$save_role_btn, {
    safely("save_role", {
      category <- input$category_input
      role <- if (category == "Professional") input$role_input else NA
      update_user_role(rv$user_id, category, role)
      rv$category <- category
      rv$role <- role
      log_msg("Saved role/category for user_id=", rv$user_id, " -> going to app")
      rv$nav <- "dashboard"
      rv$page <- "app"
    })
  })
  
  # ---- SIDEBAR MENU ----
  output$sidebar_menu <- renderUI({
    tryCatch({
      items <- list(
        c("dashboard", "Dashboard"),
        c("upload", "Upload"),
        c("analytics", "Analytics"),
        c("records", "Records"),
        c("history", "History"),
        c("profile", "Profile")
      )
      links <- lapply(items, function(it) {
        cls <- if (rv$nav == it[1]) "sidebar-link active" else "sidebar-link"
        actionLink(paste0("nav_", it[1]), it[2], class = cls)
      })
      tagList(links, tags$div(style = "margin-top:30px; border-top:1px solid #333850; padding-top:10px;"),
              actionLink("nav_logout", "Logout", class = "sidebar-link", style = "color:#f87171;"))
    }, error = function(e) {
      log_msg("ERROR in sidebar_menu: ", conditionMessage(e))
      div(style = "color:#f87171;", paste("Sidebar error:", conditionMessage(e)))
    })
  })
  
  observeEvent(input$nav_dashboard, { rv$nav <- "dashboard" })
  observeEvent(input$nav_upload,    { rv$nav <- "upload" })
  observeEvent(input$nav_analytics, { rv$nav <- "analytics" })
  observeEvent(input$nav_records,   { rv$nav <- "records" })
  observeEvent(input$nav_history,   { rv$nav <- "history" })
  observeEvent(input$nav_profile,   { rv$nav <- "profile" })
  
  observeEvent(input$nav_logout, {
    rv$user_id <- NULL; rv$username <- NULL; rv$category <- NULL; rv$role <- NULL
    rv$current_data <- NULL; rv$current_summary <- NULL; rv$current_numeric <- NULL
    rv$current_csv_res <- NULL
    rv$current_image_stats <- NULL; rv$current_file_type <- NULL
    rv$page <- "auth"
  })
  
  # ---- MAIN CONTENT ROUTER ----
  output$main_content <- renderUI({
    log_msg("Rendering main_content, rv$nav = ", rv$nav)
    tryCatch({
      switch(rv$nav,
             "dashboard" = dashboard_ui(),
             "upload"    = upload_ui(),
             "analytics" = analytics_ui(),
             "records"   = records_ui(),
             "history"   = history_ui(),
             "profile"   = profile_ui()
      )
    }, error = function(e) {
      log_msg("ERROR in main_content: ", conditionMessage(e))
      div(class = "card",
          h4("Something went wrong loading this page"),
          p(style = "color:#f87171;", conditionMessage(e)))
    })
  })
  
  # ---- DASHBOARD PAGE ----
  dashboard_ui <- function() {
    n_uploads <- nrow(get_user_uploads(req(rv$user_id)))
    tagList(
      h2(paste0("Welcome back, ", rv$username)),
      p(style = "color:#8b8fa3;", "Here's a quick look at your account."),
      fluidRow(
        column(4, div(class = "stat-box", div(class = "stat-number", n_uploads),
                      div(class = "stat-label", "Total Uploads"))),
        column(4, div(class = "stat-box", div(class = "stat-number", rv$category %||% "-"),
                      div(class = "stat-label", "Account Type"))),
        column(4, div(class = "stat-box", div(class = "stat-number", ifelse(is.null(rv$role) || is.na(rv$role), "-", rv$role)),
                      div(class = "stat-label", "Role")))
      ),
      br(),
      div(class = "card",
          h4("Get started"),
          p(style = "color:#8b8fa3;", "Head to Upload to analyze a CSV file or an image."),
          actionButton("go_upload_btn", "Upload something", class = "btn-primary")
      )
    )
  }
  observeEvent(input$go_upload_btn, { rv$nav <- "upload" })
  
  # ---- UPLOAD PAGE ----
  upload_ui <- function() {
    tagList(
      h2("Upload"),
      div(class = "card",
          radioButtons("upload_type", "What are you uploading?",
                       choices = c("CSV file" = "csv", "Image" = "image"), inline = TRUE),
          conditionalPanel(condition = "input.upload_type == 'csv'",
                           fileInput("csv_file", "Choose a CSV file", accept = ".csv")),
          conditionalPanel(condition = "input.upload_type == 'image'",
                           fileInput("image_file", "Choose an image", accept = c(".png", ".jpg", ".jpeg")))
      ),
      uiOutput("upload_result_ui")
    )
  }
  
  # -- Handle CSV upload --
  observeEvent(input$csv_file, {
    safely("csv_upload", {
      req(input$csv_file)
      df <- tryCatch(read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
                     error = function(e) NULL)
      if (is.null(df)) {
        showNotification("Could not read that CSV file.", type = "error")
        return()
      }
      res <- analyze_csv(df)
      if (!res$ok) {
        showNotification(res$message, type = "error")
        return()
      }
      stored_name <- paste0(rv$user_id, "_", as.integer(Sys.time()), "_", input$csv_file$name)
      stored_path <- file.path(uploads_dir, stored_name)
      file.copy(input$csv_file$datapath, stored_path, overwrite = TRUE)
      
      rv$current_data <- df
      rv$current_summary <- res$summary
      rv$current_numeric <- res$numeric_cols
      rv$current_cat_summaries <- res$cat_summaries
      rv$current_csv_res <- res
      rv$current_is_ocr <- FALSE
      rv$current_file_type <- "csv"
      rv$current_original_name <- input$csv_file$name
      rv$current_stored_path <- stored_path
      rv$current_image_stats <- NULL
    })
  })
  
  # -- Handle image upload --
  # First tries OCR: if the photo/scan looks like a printed table (mark
  # sheet, attendance register, etc.), it gets converted into real data and
  # run through the exact same analytics engine as a CSV upload. If no
  # table is found, falls back to color/brightness image analysis.
  observeEvent(input$image_file, {
    safely("image_upload", {
      req(input$image_file)
      ext <- tools::file_ext(input$image_file$name)
      
      stored_name <- paste0(rv$user_id, "_", as.integer(Sys.time()), "_", input$image_file$name)
      stored_path <- file.path(uploads_dir, stored_name)
      file.copy(input$image_file$datapath, stored_path, overwrite = TRUE)
      
      ocr_attempt <- try_ocr_table(input$image_file$datapath)
      
      if (isTRUE(ocr_attempt$ok)) {
        res <- analyze_csv(ocr_attempt$data)
        if (isTRUE(res$ok)) {
          showNotification("Found a table in this image — analyzing it as data.", type = "message")
          # Save the EXTRACTED TABLE as a real CSV (not the original image),
          # so History can reopen it later with read.csv() like any other
          # CSV upload.
          csv_stored_path <- paste0(tools::file_path_sans_ext(stored_path), "_extracted.csv")
          write.csv(ocr_attempt$data, csv_stored_path, row.names = FALSE)
          rv$current_data <- ocr_attempt$data
          rv$current_summary <- res$summary
          rv$current_numeric <- res$numeric_cols
          rv$current_cat_summaries <- res$cat_summaries
          rv$current_csv_res <- res
          rv$current_is_ocr <- TRUE
          rv$current_file_type <- "csv"
          rv$current_original_name <- input$image_file$name
          rv$current_stored_path <- csv_stored_path
          rv$current_image_stats <- NULL
          rv$pending_image_reason <- NULL
          return()
        }
      }
      
      # No usable table found (or tesseract isn't installed). Per spec, we do
      # NOT automatically fall back to pixel/color analysis — that's a
      # different kind of analysis and should only run if the user asks for
      # it explicitly, via the button shown on the Upload page.
      rv$current_file_type <- "image_pending"
      rv$current_original_name <- input$image_file$name
      rv$current_stored_path <- stored_path
      rv$current_data <- NULL
      rv$current_csv_res <- NULL
      rv$current_image_stats <- NULL
      rv$current_is_ocr <- FALSE
      rv$pending_image_reason <- if (!has_tesseract) {
        "OCR isn't installed (install.packages(\"tesseract\")), so this image couldn't be read as a table."
      } else {
        paste0("No readable table was found in this image (", ocr_attempt$reason %||% "no table detected", ").")
      }
    })
  })
  
  # User explicitly asked to analyze the image's colors/brightness instead
  # of (or after) trying OCR — this is opt-in, never automatic.
  observeEvent(input$analyze_image_colors_btn, {
    safely("image_color_analysis", {
      req(rv$current_stored_path, rv$current_file_type == "image_pending")
      ext <- tools::file_ext(rv$current_original_name)
      res <- analyze_image(rv$current_stored_path, ext)
      if (!res$ok) {
        showNotification(res$message, type = "error")
        return()
      }
      rv$current_image_stats <- res
      rv$current_file_type <- "image"
    })
  })
  
  # -- Show summary + chart-type picker after upload --
  output$upload_result_ui <- renderUI({
    tryCatch({
      if (is.null(rv$current_file_type)) return(NULL)
      
      if (rv$current_file_type == "image_pending") {
        return(tagList(
          div(class = "card",
              h4("Your image"),
              div(style = "text-align:center;", imageOutput("image_preview_pending", height = "260px"))
          ),
          div(class = "card",
              div(class = "insight-box",
                  p(style = "margin:0 0 10px 0;", rv$pending_image_reason),
                  p(style = "margin:0 0 12px 0; color:#5b6270;",
                    "This app analyzes the actual data in your file, not image pixels, by default — so no chart is shown automatically for a non-table image. If you'd still like color/brightness analysis for this picture, you can request it below."),
                  actionButton("analyze_image_colors_btn", "Analyze this image's colors instead", class = "btn-primary")
              )
          )
        ))
      }
      
      if (rv$current_file_type == "csv") {
        res <- rv$current_csv_res
        tagList(
          if (isTRUE(rv$current_is_ocr)) {
            div(class = "card",
                div(class = "insight-box",
                    p(style = "margin:0;", strong("Read from your image via OCR — "),
                      "this table was extracted from the photo you uploaded and is being analyzed like a spreadsheet.")))
          },
          div(class = "card",
              h4("Quick stats"),
              fluidRow(lapply(res$kpis, function(k) {
                column(3, div(class = "stat-box", div(class = "stat-number", k$value), div(class = "stat-label", k$label)))
              }))
          ),
          div(class = "card",
              h4("Your data (first 10 rows)"),
              DTOutput("raw_data_preview")
          ),
          if (res$duplicate_count > 0 || length(res$missing_by_col) > 0) {
            div(class = "card",
                h4("Data cleaning notes"),
                div(class = "insight-box",
                    tags$ul(
                      if (res$duplicate_count > 0) tags$li(paste0(res$duplicate_count, " duplicate row(s) found.")),
                      lapply(names(res$missing_by_col), function(cn) {
                        tags$li(paste0(res$missing_by_col[[cn]], " missing/blank value(s) in \"", cn, "\"."))
                      })
                    )
                ),
                downloadButton("download_clean_csv", "Download cleaned data (duplicates removed)", class = "btn-primary")
            )
          },
          div(class = "card",
              h4("Summary statistics"),
              DTOutput("summary_table")
          ),
          div(class = "card",
              h4("What this means"),
              div(class = "insight-box",
                  tags$ul(lapply(build_csv_insights(res), tags$li)))
          ),
          if (!is.na(res$group_col)) {
            div(class = "card",
                h4("Filter"),
                selectInput("group_filter", paste("Show only this", res$group_col),
                            choices = c("All", sort(unique(as.character(res$data[[res$group_col]])))))
            )
          },
          div(class = "card",
              h4("Choose how to visualize it"),
              radioButtons("chart_choice", NULL,
                           choices = build_chart_choices(res),
                           inline = TRUE),
              plotOutput("upload_plot", height = "420px")
          )
        )
      } else {
        s <- rv$current_image_stats
        tagList(
          div(class = "card",
              h4("Your image"),
              div(style = "text-align:center;", imageOutput("image_preview", height = "260px"))
          ),
          div(class = "card",
              h4("Image details"),
              fluidRow(
                column(3, div(class = "stat-box", div(class = "stat-number", s$width), div(class = "stat-label", "Width (px)"))),
                column(3, div(class = "stat-box", div(class = "stat-number", s$height), div(class = "stat-label", "Height (px)"))),
                column(3, div(class = "stat-box", div(class = "stat-number", paste0(s$brightness, "%")), div(class = "stat-label", "Brightness"))),
                column(3, div(class = "stat-box", div(class = "stat-number", toupper(tools::file_ext(rv$current_original_name))), div(class = "stat-label", "Format")))
              )
          ),
          if (!is.null(s$dominant_colors) && nrow(s$dominant_colors) > 0) {
            div(class = "card",
                h4("Main colors found in this image"),
                div(style = "display:flex; gap:16px; flex-wrap:wrap;",
                    lapply(seq_len(nrow(s$dominant_colors)), function(i) {
                      row <- s$dominant_colors[i, ]
                      div(style = "text-align:center;",
                          div(style = paste0("width:56px; height:56px; border-radius:10px; background:",
                                             row$hex, "; border:1px solid #e7eaf3; margin:0 auto 6px;")),
                          div(style = "font-size:12px; color:#5b6270;", paste0(row$name, " (", row$pct, "%)"))
                      )
                    })
                )
            )
          },
          div(class = "card",
              h4("What this means"),
              div(class = "insight-box",
                  tags$ul(lapply(build_image_insights(s), tags$li)))
          ),
          div(class = "card",
              h4("Choose how to visualize it"),
              radioButtons("chart_choice", NULL,
                           choices = c("Bar Chart" = "bar", "Pie Chart" = "pie",
                                       "Line Chart" = "line", "Full Dashboard" = "dashboard"),
                           inline = TRUE),
              plotOutput("upload_plot", height = "420px")
          )
        )
      }
    }, error = function(e) {
      log_msg("ERROR in upload_result_ui: ", conditionMessage(e))
      div(style = "color:#f87171;", paste("Upload display error:", conditionMessage(e)))
    })
  })
  
  output$image_preview_pending <- renderImage({
    req(rv$current_stored_path, rv$current_file_type == "image_pending")
    list(src = rv$current_stored_path, contentType = NULL,
         width = "auto", height = 250, style = "object-fit:contain; max-width:100%;")
  }, deleteFile = FALSE)
  
  output$download_clean_csv <- downloadHandler(
    filename = function() paste0("cleaned_", tools::file_path_sans_ext(rv$current_original_name %||% "data"), ".csv"),
    content = function(file) {
      d <- rv$current_data
      write.csv(d[!duplicated(d), , drop = FALSE], file, row.names = FALSE)
    }
  )
  
  output$summary_table <- renderDT({
    req(rv$current_summary)
    datatable(rv$current_summary, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })
  
  output$image_preview <- renderImage({
    req(rv$current_stored_path, rv$current_file_type == "image")
    list(src = rv$current_stored_path, contentType = NULL,
         width = "auto", height = 250, style = "object-fit:contain; max-width:100%;")
  }, deleteFile = FALSE)
  
  # Raw preview of the actual uploaded rows, respecting the group filter
  # if one is selected — so you can see the real (filtered) data.
  output$raw_data_preview <- renderDT({
    req(rv$current_data)
    df <- rv$current_data
    if (!is.null(rv$current_csv_res) && !is.na(rv$current_csv_res$group_col) &&
        !is.null(input$group_filter) && input$group_filter != "All") {
      df <- df[as.character(df[[rv$current_csv_res$group_col]]) == input$group_filter, , drop = FALSE]
    }
    datatable(utils::head(df, 10),
              options = list(dom = "t", scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # Re-analyzes the current data after applying the group filter (if any),
  # so charts/KPIs reflect only the selected Department/Section/Team.
  filtered_csv_res <- reactive({
    req(rv$current_csv_res)
    res <- rv$current_csv_res
    if (!is.na(res$group_col) && !is.null(input$group_filter) && input$group_filter != "All") {
      df <- res$data[as.character(res$data[[res$group_col]]) == input$group_filter, , drop = FALSE]
      filtered <- analyze_csv(df)
      if (isTRUE(filtered$ok)) return(filtered)
    }
    res
  })
  
  # -- Render the chosen chart + auto-save to history --
  output$upload_plot <- renderPlot({
    req(input$chart_choice, rv$current_file_type)
    
    if (rv$current_file_type == "csv") {
      req(rv$current_csv_res)
      res <- filtered_csv_res()
      p <- safe_plot(switch(input$chart_choice,
                            "bar" = plot_csv_bar(res),
                            "pie" = plot_csv_pie(res),
                            "line" = plot_csv_line(res$numeric_cols, res$perf_col),
                            "trend" = if (!is.null(res$date_trend_df)) {
                              plot_csv_date_trend(res$date_trend_df, res$roles$date_col, res$perf_col)
                            } else plot_csv_line(res$numeric_cols, res$perf_col),
                            "scatter" = if (!is.null(res$scatter_pair)) {
                              plot_csv_scatter(res$data, res$scatter_pair$x, res$scatter_pair$y)
                            } else plot_csv_bar(res),
                            "dashboard" = plot_csv_dashboard(res)
      ))
      details_str <- build_csv_details(res)
    } else {
      req(rv$current_image_stats)
      s <- rv$current_image_stats
      p <- safe_plot(switch(input$chart_choice,
                            "bar" = plot_image_bar(s),
                            "pie" = plot_image_pie(s),
                            "line" = plot_image_line(s),
                            "dashboard" = plot_image_dashboard(s)
      ))
      details_str <- build_image_details(s)
    }
    
    safely("save_upload_record", {
      save_upload_record(rv$user_id, rv$current_original_name, rv$current_stored_path,
                         rv$current_file_type, input$chart_choice, details_str)
    })
    
    if (inherits(p, "gtable") || inherits(p, "grob")) {
      grid::grid.draw(p)
    } else {
      print(p)
    }
  })
  
  # ---- ANALYTICS PAGE ----
  analytics_ui <- function() {
    if (is.null(rv$current_file_type)) {
      return(tagList(h2("Analytics"), p(style = "color:#8b8fa3;",
                                        "Upload a file first — your analysis will appear here.")))
    }
    tagList(
      h2("Analytics"),
      p(style = "color:#8b8fa3;", paste("Currently viewing:", rv$current_original_name)),
      div(class = "card",
          h4("What this means"),
          div(class = "insight-box",
              tags$ul(lapply(
                if (rv$current_file_type == "csv") {
                  build_csv_insights(rv$current_csv_res)
                } else {
                  build_image_insights(rv$current_image_stats)
                },
                tags$li)))
      ),
      div(class = "card", plotOutput("analytics_plot", height = "500px"))
    )
  }
  
  output$analytics_plot <- renderPlot({
    req(rv$current_file_type)
    if (rv$current_file_type == "csv") {
      req(rv$current_csv_res)
      grid::grid.draw(safe_plot(plot_csv_dashboard(rv$current_csv_res)))
    } else {
      req(rv$current_image_stats)
      grid::grid.draw(safe_plot(plot_image_dashboard(rv$current_image_stats)))
    }
  })
  
  # ---- RECORDS PAGE ----
  # Look up ONE person/row by name or ID instead of scrolling the whole
  # sheet — e.g. a professor pulling up one student's full record.
  records_ui <- function() {
    if (is.null(rv$current_data) || rv$current_file_type != "csv") {
      return(tagList(h2("Records"), p(style = "color:#8b8fa3;",
                                      "Upload a CSV/Excel file with individual records (e.g. students or employees) to search them here.")))
    }
    roles <- rv$current_csv_res$roles
    lookup_col <- if (!is.null(roles$name_col) && !is.na(roles$name_col)) roles$name_col
    else if (!is.null(roles$id_col) && !is.na(roles$id_col)) roles$id_col
    else names(rv$current_data)[1]
    tagList(
      h2("Records"),
      p(style = "color:#8b8fa3;", paste0("Search by \"", lookup_col, "\" to pull up one record's full details.")),
      div(class = "card",
          textInput("record_search", NULL, placeholder = paste("Search", lookup_col, "...")),
      ),
      uiOutput("record_results_ui")
    )
  }
  
  output$record_results_ui <- renderUI({
    req(rv$current_data, rv$current_file_type == "csv")
    roles <- rv$current_csv_res$roles
    lookup_col <- if (!is.null(roles$name_col) && !is.na(roles$name_col)) roles$name_col
    else if (!is.null(roles$id_col) && !is.na(roles$id_col)) roles$id_col
    else names(rv$current_data)[1]
    
    df <- rv$current_data
    query <- trimws(input$record_search %||% "")
    matches <- if (nchar(query) == 0) {
      df
    } else {
      df[grepl(query, as.character(df[[lookup_col]]), ignore.case = TRUE), , drop = FALSE]
    }
    
    if (nrow(matches) == 0) {
      return(div(class = "card", p(style = "color:#8b8fa3;", "No matching records.")))
    }
    if (nchar(query) == 0) {
      return(div(class = "card", p(style = "color:#8b8fa3;",
                                   paste0(nrow(df), " total record(s) — start typing above to find one.")))
      )
    }
    
    matches <- utils::head(matches, 20)
    tagList(lapply(seq_len(nrow(matches)), function(i) {
      row <- matches[i, , drop = FALSE]
      div(class = "card",
          h4(as.character(row[[lookup_col]])),
          tags$table(style = "width:100%; border-collapse:collapse;",
                     lapply(names(row), function(cn) {
                       tags$tr(
                         tags$td(style = "padding:4px 10px; color:#6b7280; width:40%;", cn),
                         tags$td(style = "padding:4px 10px; font-weight:600;", as.character(row[[cn]]))
                       )
                     })
          )
      )
    }))
  })
  
  # ---- HISTORY PAGE ----
  history_ui <- function() {
    tagList(
      h2("History"),
      p(style = "color:#8b8fa3;", "Only you can see this."),
      div(class = "card", DTOutput("history_table")),
      uiOutput("history_detail_ui")
    )
  }
  
  output$history_table <- renderDT({
    req(rv$user_id)
    df <- get_user_uploads(rv$user_id)
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No uploads yet."), options = list(dom = "t")))
    display <- df[, c("id", "original_filename", "file_type", "details", "chart_type", "upload_time")]
    names(display) <- c("ID", "File", "Type", "Details", "Chart", "Uploaded At")
    datatable(display, selection = "single", options = list(pageLength = 8), rownames = FALSE)
  })
  
  observeEvent(input$history_table_rows_selected, {
    df <- get_user_uploads(rv$user_id)
    idx <- input$history_table_rows_selected
    if (length(idx) == 0) { rv$selected_history_id <- NULL; return() }
    rv$selected_history_id <- df$id[idx]
  })
  
  output$history_detail_ui <- renderUI({
    req(rv$selected_history_id)
    df <- get_user_uploads(rv$user_id)
    rec <- df[df$id == rv$selected_history_id, ]
    req(nrow(rec) == 1)
    div(class = "card",
        h4("Reopened report"),
        p(style = "color:#8b8fa3;", paste0(rec$original_filename, " — ", rec$details %||% "")),
        plotOutput("history_plot", height = "420px"))
  })
  
  output$history_plot <- renderPlot({
    req(rv$selected_history_id)
    df <- get_user_uploads(rv$user_id)
    rec <- df[df$id == rv$selected_history_id, ]
    req(nrow(rec) == 1)
    
    if (rec$file_type == "csv") {
      d <- tryCatch(read.csv(rec$stored_path, stringsAsFactors = FALSE), error = function(e) NULL)
      req(d)
      res <- analyze_csv(d)
      req(res$ok)
      p <- safe_plot(switch(rec$chart_type,
                            "bar" = plot_csv_bar(res),
                            "pie" = plot_csv_pie(res),
                            "line" = plot_csv_line(res$numeric_cols, res$perf_col),
                            "trend" = if (!is.null(res$date_trend_df)) {
                              plot_csv_date_trend(res$date_trend_df, res$roles$date_col, res$perf_col)
                            } else plot_csv_line(res$numeric_cols, res$perf_col),
                            "scatter" = if (!is.null(res$scatter_pair)) {
                              plot_csv_scatter(res$data, res$scatter_pair$x, res$scatter_pair$y)
                            } else plot_csv_bar(res),
                            "dashboard" = plot_csv_dashboard(res)
      ))
    } else {
      ext <- tools::file_ext(rec$original_filename)
      res <- analyze_image(rec$stored_path, ext)
      req(res$ok)
      p <- safe_plot(switch(rec$chart_type,
                            "bar" = plot_image_bar(res),
                            "pie" = plot_image_pie(res),
                            "line" = plot_image_line(res),
                            "dashboard" = plot_image_dashboard(res)
      ))
    }
    
    if (inherits(p, "gtable") || inherits(p, "grob")) grid::grid.draw(p) else print(p)
  })
  
  # ---- PROFILE PAGE ----
  profile_ui <- function() {
    tagList(
      h2("Profile"),
      div(class = "card",
          p(strong("Username: "), rv$username),
          p(strong("Account type: "), rv$category %||% "-"),
          p(strong("Role: "), ifelse(is.null(rv$role) || is.na(rv$role), "-", rv$role)),
          hr(),
          h4("Update your role"),
          radioButtons("profile_category", "I'm using this for:",
                       choices = c("Personal", "Professional"), inline = TRUE,
                       selected = rv$category %||% "Personal"),
          conditionalPanel(condition = "input.profile_category == 'Professional'",
                           selectInput("profile_role", "Role", choices = role_choices,
                                       selected = if (!is.null(rv$role) && !is.na(rv$role)) rv$role else role_choices[1])),
          actionButton("update_profile_btn", "Save changes", class = "btn-primary")
      )
    )
  }
  
  observeEvent(input$update_profile_btn, {
    safely("update_profile", {
      category <- input$profile_category
      role <- if (category == "Professional") input$profile_role else NA
      update_user_role(rv$user_id, category, role)
      rv$category <- category
      rv$role <- role
      showNotification("Profile updated.", type = "message")
    })
  })
}
