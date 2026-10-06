# ================================================================
#  backend.R — Data & Image Analysis Engine for Data and Image Analyzer
#  (column-role detection, cleaning, statistics, OCR, chart building)
#  This file is pure logic — no Shiny reactive code lives here.
# ================================================================

# Build a short human-readable summary of the upload's key details, so the
# history table can show what a file contained without re-reading it.
build_csv_details <- function(res) {
  perf_note <- if (!is.null(res$perf_col) && !is.na(res$perf_col)) paste0(", tracking \"", res$perf_col, "\"") else ""
  paste0(res$rows, " record(s), ", res$cols, " field(s)", perf_note)
}

build_image_details <- function(stats) {
  if (!is.null(stats$ocr_table) && !is.null(stats$ocr_table$ok) && isTRUE(stats$ocr_table$ok)) {
    return(paste0("Table extracted via OCR: ", nrow(stats$ocr_table$data), " rows x ",
                  ncol(stats$ocr_table$data), " cols"))
  }
  top_color <- if (!is.null(stats$dominant_colors) && nrow(stats$dominant_colors) > 0) {
    paste0(", mostly ", stats$dominant_colors$name[1])
  } else ""
  paste0(stats$width, "x", stats$height, "px, ",
         stats$brightness, "% brightness", top_color)
}

# Plain-English "what does this actually mean" bullets, shown alongside
# the charts so numbers aren't left to speak for themselves. Leads with
# the record-level story (grade distribution, group comparison) before
# any raw column statistics.
build_csv_insights <- function(res) {
  bullets <- sprintf("This file has %d record(s) across %d field(s).", res$rows, res$cols)
  
  if (!is.null(res$perf_col) && !is.na(res$perf_col) && !is.null(res$grade_summary)) {
    gs <- res$grade_summary
    top_band <- gs$Band[which.max(gs$Count)]
    bullets <- c(bullets, sprintf(
      "Looking at \"%s\": most records (%d of %d) fall in the \"%s\" band — see the pie/bar chart for the full breakdown.",
      res$perf_col, max(gs$Count), sum(gs$Count), top_band))
  }
  
  if (!is.null(res$group_summary) && !is.na(res$group_col)) {
    grp <- res$group_summary
    bullets <- c(bullets, sprintf(
      "By \"%s\": \"%s\" has the highest average %s (%.2f), \"%s\" has the lowest (%.2f).",
      res$group_col, grp$Group[1], res$perf_col, grp$Average[1],
      grp$Group[nrow(grp)], grp$Average[nrow(grp)]))
  }
  
  if (!is.null(res$summary) && nrow(res$summary) > 0) {
    s <- res$summary
    top_col   <- s$Column[which.max(s$Mean)]
    low_col   <- s$Column[which.min(s$Mean)]
    most_missing <- s$Column[which.max(s$Missing)]
    bullets <- c(bullets,
                 sprintf("Across all numeric field(s), \"%s\" runs highest on average (%.2f) and \"%s\" runs lowest (%.2f).",
                         top_col, max(s$Mean), low_col, min(s$Mean))
    )
    if (res$missing_total > 0) {
      bullets <- c(bullets, sprintf("%d missing value(s) total — mostly in \"%s\".",
                                    res$missing_total, most_missing))
    } else {
      bullets <- c(bullets, "No missing values — the data is complete.")
    }
  }
  
  if (!is.null(res$roles$name_col) && !is.na(res$roles$name_col)) {
    bullets <- c(bullets, sprintf("Use the Records page to look up any individual by \"%s\".", res$roles$name_col))
  }
  
  if (!is.null(res$duplicate_count) && res$duplicate_count > 0) {
    bullets <- c(bullets, sprintf("%d duplicate row(s) were found — see \"Data cleaning notes\" below.",
                                  res$duplicate_count))
  }
  
  if (!is.null(res$date_trend_df) && !is.na(res$perf_col)) {
    dt <- res$date_trend_df
    direction <- if (dt$Average[nrow(dt)] > dt$Average[1]) "increased" else if (dt$Average[nrow(dt)] < dt$Average[1]) "decreased" else "stayed flat"
    bullets <- c(bullets, sprintf("Over time, average %s has %s (from %.2f to %.2f).",
                                  res$perf_col, direction, dt$Average[1], dt$Average[nrow(dt)]))
  }
  
  bullets
}

# A small named-color lookup so image insights can say "sky blue" instead
# of raw RGB numbers.
color_name_palette <- data.frame(
  name = c("black","white","gray","red","dark red","orange","yellow","green",
           "dark green","teal","sky blue","blue","navy","purple","pink",
           "brown","beige","cream"),
  r = c(0.05,0.97,0.5,0.85,0.45,1.0,0.98,0.25,0.05,0.05,0.45,0.15,0.03,0.5,0.93,0.5,0.88,0.97),
  g = c(0.05,0.97,0.5,0.12,0.02,0.55,0.88,0.6,0.35,0.5,0.7,0.35,0.03,0.15,0.6,0.32,0.82,0.94),
  b = c(0.05,0.97,0.5,0.12,0.05,0.02,0.15,0.25,0.1,0.5,0.9,0.9,0.4,0.6,0.68,0.15,0.65,0.85)
)

nearest_color_name <- function(r, g, b) {
  d <- (color_name_palette$r - r)^2 + (color_name_palette$g - g)^2 + (color_name_palette$b - b)^2
  color_name_palette$name[which.min(d)]
}

build_image_insights <- function(stats) {
  rgb <- stats$rgb_df
  dominant <- rgb$Channel[which.max(rgb$Mean)]
  spread <- max(rgb$Mean) - min(rgb$Mean)
  tone <- if (stats$brightness >= 70) "bright"
  else if (stats$brightness >= 40) "medium brightness"
  else "dark"
  balance <- if (spread < 5) "very balanced across Red, Green and Blue (close to grayscale/neutral tones)"
  else sprintf("leaning toward %s", dominant)
  
  bullets <- c(
    sprintf("Image size: %d x %d pixels (aspect ratio %.2f).", stats$width, stats$height, stats$aspect_ratio),
    sprintf("Overall brightness: %.1f%% — a %s image, %s.", stats$brightness, tone, balance)
  )
  
  if (!is.null(stats$dominant_colors) && nrow(stats$dominant_colors) > 0) {
    dc <- stats$dominant_colors
    top3 <- head(dc, 3)
    desc <- paste(sprintf("%s (%.0f%%)", top3$name, top3$pct), collapse = ", ")
    bullets <- c(bullets, sprintf("Main colors in this image: %s.", desc))
  }
  
  contrast_note <- if (!is.null(stats$contrast)) {
    if (stats$contrast >= 25) "high contrast (a strong mix of light and dark areas)"
    else if (stats$contrast >= 12) "moderate contrast"
    else "low contrast (fairly flat/uniform lighting)"
  } else NULL
  if (!is.null(contrast_note)) {
    bullets <- c(bullets, sprintf("Contrast: %s.", contrast_note))
  }
  bullets
}

# Guess the "meaning" of a column from its name, so charts/insights use the
# right vocabulary (Salary vs Marks vs Working Hours) instead of generic
# "numeric column" language everywhere.
semantic_type_for_colname <- function(colname) {
  cn <- tolower(colname)
  if (grepl("salary|wage|pay|income|revenue|sales|price|cost|amount", cn)) return("money")
  if (grepl("hour", cn)) return("hours")
  if (grepl("attendance|present", cn)) return("attendance")
  if (grepl("mark|score|grade|result", cn)) return("score")
  if (grepl("target|goal|kpi", cn)) return("target")
  if (grepl("rating|rate$|productivity|performance", cn)) return("rating")
  if (grepl("stock|quantity|qty|inventory|count", cn)) return("quantity")
  "generic"
}

semantic_label <- function(semtype) {
  switch(semtype,
         money = "amount", hours = "hours", attendance = "attendance %",
         score = "score", target = "target", rating = "rating",
         quantity = "quantity", "value")
}

# Try to find a column that represents a DATE (by name, or by successfully
# parsing as a date), so time-based trends (daily/weekly/monthly) are
# possible instead of only row-order trends.
detect_date_column <- function(df) {
  nms <- names(df)
  date_pattern <- "(?i)date|^dob$|day|month|year|timestamp"
  candidates <- nms[grepl(date_pattern, nms, perl = TRUE)]
  # also try any character column that mostly parses as a date, in case the
  # header doesn't say "date" explicitly
  other_char_cols <- setdiff(nms[sapply(df, function(x) is.character(x) || is.factor(x))], candidates)
  candidates <- c(candidates, other_char_cols)
  
  for (cn in candidates) {
    v <- as.character(df[[cn]])
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) < 3) next
    parsed <- suppressWarnings(as.Date(v, tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y")))
    if (mean(!is.na(parsed)) >= 0.8) return(cn)
  }
  NA
}

# ---- CSV analysis ------------------------------------------------

# Summarize any character/factor columns with a manageable number of
# distinct values (e.g. "Department", "Category", "Status") — these are
# often the most meaningful part of a spreadsheet and were previously
# ignored entirely.
summarize_categoricals <- function(df) {
  cat_cols <- df[sapply(df, function(x) is.character(x) || is.factor(x))]
  out <- list()
  if (ncol(cat_cols) == 0) return(out)
  for (colname in names(cat_cols)) {
    vals <- as.character(cat_cols[[colname]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) next
    n_unique <- length(unique(vals))
    # skip columns that look like free text or unique IDs
    if (n_unique < 2 || n_unique > 20 || n_unique >= length(vals)) next
    tab <- sort(table(vals), decreasing = TRUE)
    top_n <- utils::head(tab, 8)
    out[[colname]] <- data.frame(Category = names(top_n), Count = as.integer(top_n), row.names = NULL)
  }
  out
}

# Figure out what each column is FOR, so the same engine works for a
# student mark sheet, an employee performance sheet, or any other record
# list: which column identifies each row (ID/Reg No/Emp ID), which is the
# person's name, which are grouping fields (Department/Section/Team), which
# numeric columns are actual performance figures (marks, ratings, target %,
# salary, hours) rather than an ID that happens to be numeric, and which
# column (if any) is a date for time-based trends.
detect_column_roles <- function(df) {
  nms <- names(df)
  id_pattern   <- "(?i)^(id|.*id|regno|reg.?no|roll.?no|rollnumber|s\\.?no|serial.?no|emp.?no)$"
  name_pattern <- "(?i)name"
  
  id_col <- nms[grepl(id_pattern, nms, perl = TRUE)]
  id_col <- if (length(id_col) > 0) id_col[1] else NA
  
  # Fallback: a column (any type) whose values are all unique — likely an ID —
  # if nothing matched by name.
  if (is.na(id_col)) {
    for (cn in nms) {
      v <- df[[cn]]
      v_valid <- v[!is.na(v)]
      if (length(v_valid) > 1 && length(unique(v_valid)) == length(v_valid)) {
        id_col <- cn
        break
      }
    }
  }
  
  name_col <- nms[grepl(name_pattern, nms, perl = TRUE) & sapply(df, function(x) is.character(x) || is.factor(x))[nms]]
  name_col <- if (length(name_col) > 0) name_col[1] else NA
  
  date_col <- detect_date_column(df)
  
  numeric_cols_all <- names(df)[sapply(df, is.numeric)]
  performance_cols <- setdiff(numeric_cols_all, id_col)
  
  semantic_types <- stats::setNames(sapply(performance_cols, semantic_type_for_colname), performance_cols)
  
  list(id_col = id_col, name_col = name_col, date_col = date_col,
       performance_cols = performance_cols, semantic_types = semantic_types)
}

# Bucket a numeric column into readable bands. If values look like
# marks/percentages (0-100), use grade-style bands; otherwise fall back to
# quartile-based Low/Medium/High/Very High bands so this also works for
# employee ratings, target %, sales figures, etc.
bucket_scores <- function(x, colname) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NULL)
  if (min(x) >= 0 && max(x) <= 100) {
    breaks <- c(-Inf, 39, 59, 74, 89, Inf)
    labels <- c("Fail (<40)", "40-59", "60-74", "75-89", "90-100")
  } else {
    qs <- unique(stats::quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
    if (length(qs) < 3) return(NULL)
    breaks <- c(-Inf, qs[-c(1, length(qs))], Inf)
    labels <- utils::head(c("Low", "Medium", "High", "Very High"), length(breaks) - 1)
  }
  band <- cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
  tab <- table(band)
  data.frame(Band = factor(names(tab), levels = labels), Count = as.integer(tab), row.names = NULL)
}

# Average of a performance column, grouped by a category column (e.g.
# average Marks per Department, or average Target% per Team).
group_comparison <- function(df, group_col, perf_col) {
  if (is.na(group_col) || is.na(perf_col)) return(NULL)
  agg <- stats::aggregate(df[[perf_col]], by = list(Group = as.character(df[[group_col]])),
                          FUN = function(x) round(mean(x, na.rm = TRUE), 2))
  names(agg) <- c("Group", "Average")
  agg <- agg[order(-agg$Average), ]
  if (nrow(agg) < 2 || nrow(agg) > 20) return(NULL)
  agg
}

# Average of the performance column per date, so a date/DOB/timestamp-style
# column produces a real time trend instead of just row order. Falls back to
# NULL gracefully if too many or too few distinct dates.
date_trend <- function(df, date_col, perf_col) {
  if (is.na(date_col) || is.na(perf_col)) return(NULL)
  d <- suppressWarnings(as.Date(as.character(df[[date_col]]),
                                tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y")))
  ok <- !is.na(d) & !is.na(df[[perf_col]])
  if (sum(ok) < 3) return(NULL)
  agg <- stats::aggregate(df[[perf_col]][ok], by = list(Date = d[ok]),
                          FUN = function(x) round(mean(x, na.rm = TRUE), 2))
  names(agg) <- c("Date", "Average")
  agg <- agg[order(agg$Date), ]
  if (nrow(agg) < 2) return(NULL)
  agg
}

# Two performance columns together → a scatter relationship (e.g. Attendance
# vs Marks, Working Hours vs Productivity), only offered when it's genuinely
# meaningful (two distinct numeric performance columns exist).
find_scatter_pair <- function(roles) {
  cols <- roles$performance_cols
  if (length(cols) < 2) return(NULL)
  list(x = cols[1], y = cols[2])
}

# Duplicate rows + per-column missing counts, so cleaning issues are shown
# to the user up front instead of silently ignored.
data_quality_check <- function(df) {
  dup_count <- sum(duplicated(df))
  missing_by_col <- sapply(df, function(x) sum(is.na(x) | (is.character(x) & !nzchar(trimws(x)))))
  missing_by_col <- missing_by_col[missing_by_col > 0]
  list(duplicate_count = dup_count, missing_by_col = missing_by_col)
}

# Headline KPI numbers for the primary performance column — the "quick
# glance" cards at the top of the dashboard.
build_kpis <- function(df, roles, perf_col) {
  kpis <- list(list(label = "Total Records", value = nrow(df)))
  if (!is.na(perf_col)) {
    x <- df[[perf_col]][!is.na(df[[perf_col]])]
    lbl <- tools::toTitleCase(gsub("_", " ", perf_col))
    if (length(x) > 0) {
      kpis <- c(kpis, list(
        list(label = paste("Average", lbl), value = round(mean(x), 2)),
        list(label = paste("Highest", lbl), value = max(x)),
        list(label = paste("Lowest", lbl), value = min(x))
      ))
    }
  }
  kpis
}

analyze_csv <- function(df) {
  numeric_cols <- df[sapply(df, is.numeric)]
  cat_summaries <- summarize_categoricals(df)
  roles <- detect_column_roles(df)
  quality <- data_quality_check(df)
  
  if (ncol(numeric_cols) == 0) {
    return(list(ok = FALSE, message = "No numeric columns found in this file."))
  }
  summary_df <- data.frame(
    Column = names(numeric_cols),
    Mean   = sapply(numeric_cols, function(x) round(mean(x, na.rm = TRUE), 2)),
    Median = sapply(numeric_cols, function(x) round(median(x, na.rm = TRUE), 2)),
    Min    = sapply(numeric_cols, function(x) round(min(x, na.rm = TRUE), 2)),
    Max    = sapply(numeric_cols, function(x) round(max(x, na.rm = TRUE), 2)),
    Missing = sapply(numeric_cols, function(x) sum(is.na(x))),
    row.names = NULL
  )
  
  # Primary performance column: first numeric column that isn't the ID.
  perf_col <- if (length(roles$performance_cols) > 0) roles$performance_cols[1] else NA
  grade_summary <- if (!is.na(perf_col)) bucket_scores(df[[perf_col]], perf_col) else NULL
  
  # Primary group column: first categorical column with a sane number of
  # groups (already filtered by summarize_categoricals), excluding the name column.
  group_candidates <- setdiff(names(cat_summaries), roles$name_col)
  group_col <- if (length(group_candidates) > 0) group_candidates[1] else NA
  group_summary <- if (!is.na(group_col) && !is.na(perf_col)) group_comparison(df, group_col, perf_col) else NULL
  
  date_trend_df <- if (!is.na(roles$date_col) && !is.na(perf_col)) date_trend(df, roles$date_col, perf_col) else NULL
  scatter_pair <- find_scatter_pair(roles)
  kpis <- build_kpis(df, roles, perf_col)
  
  list(ok = TRUE, data = df, summary = summary_df, numeric_cols = numeric_cols,
       cat_summaries = cat_summaries, roles = roles,
       perf_col = perf_col, grade_summary = grade_summary,
       group_col = group_col, group_summary = group_summary,
       date_trend_df = date_trend_df, scatter_pair = scatter_pair, kpis = kpis,
       duplicate_count = quality$duplicate_count, missing_by_col = quality$missing_by_col,
       rows = nrow(df), cols = ncol(df), missing_total = sum(is.na(df)))
}

# Theme used across all ggplot charts — clean light theme.
light_plot_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid = element_line(color = "#e7eaf3"),
      text = element_text(color = "#1f2430"),
      plot.title = element_text(face = "bold", color = "#1f2430"),
      axis.text = element_text(color = "#5b6270"),
      legend.background = element_rect(fill = "#ffffff"),
      legend.text = element_text(color = "#1f2430"),
      legend.title = element_text(color = "#1f2430")
    )
}

chart_palette <- c("#2f6fed", "#7c5cf5", "#12b3c4", "#f0578a", "#f5a623", "#22b573")

plot_csv_grade_bar <- function(grade_summary, perf_col) {
  ggplot(grade_summary, aes(x = Band, y = Count, fill = Band)) +
    geom_col() +
    scale_fill_manual(values = rep(chart_palette, length.out = nrow(grade_summary))) +
    labs(title = paste("How many records fall in each", perf_col, "band"),
         subtitle = "e.g. how many students scored in each grade range",
         x = NULL, y = "Count") +
    light_plot_theme() + theme(legend.position = "none")
}

plot_csv_grade_pie <- function(grade_summary, perf_col) {
  ggplot(grade_summary, aes(x = "", y = Count, fill = Band)) +
    geom_col(width = 1) + coord_polar("y") +
    scale_fill_manual(values = rep(chart_palette, length.out = nrow(grade_summary))) +
    labs(title = paste("Share of records by", perf_col, "band"),
         subtitle = "Each slice = share of records in that band",
         x = NULL, y = NULL) +
    light_plot_theme() + theme(axis.text = element_blank(), panel.grid = element_blank())
}

plot_csv_group_bar <- function(group_summary, group_col, perf_col) {
  group_summary$Group <- factor(group_summary$Group, levels = group_summary$Group)
  ggplot(group_summary, aes(x = Group, y = Average, fill = Group)) +
    geom_col() +
    scale_fill_manual(values = rep(chart_palette, length.out = nrow(group_summary))) +
    labs(title = paste("Average", perf_col, "by", group_col),
         subtitle = "Which group is ahead / behind on average",
         x = NULL, y = paste("Average", perf_col)) +
    light_plot_theme() + theme(legend.position = "none",
                               axis.text.x = element_text(angle = 30, hjust = 1))
}

plot_csv_line <- function(numeric_cols, perf_col = NULL) {
  # Real row-by-row trend of the primary performance column (or first
  # numeric column if none was detected) — this is what the data actually
  # does across your records, e.g. are marks trending up or down.
  col_name <- if (!is.null(perf_col) && !is.na(perf_col) && perf_col %in% names(numeric_cols)) {
    perf_col
  } else {
    names(numeric_cols)[1]
  }
  df <- data.frame(row = seq_along(numeric_cols[[col_name]]), value = numeric_cols[[col_name]])
  ggplot(df, aes(x = row, y = value)) +
    geom_line(color = "#2f6fed", linewidth = 0.9) +
    geom_point(color = "#7c5cf5", size = 1.6, alpha = 0.7) +
    labs(title = paste(col_name, "across your records"), subtitle = "Actual values in row order — spot trends, spikes, or outliers",
         x = "Record #", y = col_name) +
    light_plot_theme()
}

plot_csv_hist <- function(numeric_cols) {
  first_col <- names(numeric_cols)[1]
  df <- data.frame(value = numeric_cols[[1]])
  ggplot(df, aes(x = value)) +
    geom_histogram(fill = "#2f6fed", bins = 15, color = "#ffffff") +
    labs(title = paste("Distribution of", first_col), subtitle = "How values in this column are spread out",
         x = first_col, y = "Count") +
    light_plot_theme()
}

plot_csv_category <- function(cat_df, colname) {
  cat_df$Category <- factor(cat_df$Category, levels = cat_df$Category[order(-cat_df$Count)])
  ggplot(cat_df, aes(x = Category, y = Count, fill = Category)) +
    geom_col() +
    scale_fill_manual(values = rep(chart_palette, length.out = nrow(cat_df))) +
    labs(title = paste("Most common values in", colname), subtitle = "Top categories by how often they appear",
         x = NULL, y = "Count") +
    light_plot_theme() + theme(legend.position = "none",
                               axis.text.x = element_text(angle = 30, hjust = 1))
}

# Bar chart: grade/performance bands if we found a performance column,
# otherwise fall back to category counts, otherwise raw column means.
plot_csv_bar <- function(res) {
  if (!is.null(res$grade_summary)) return(plot_csv_grade_bar(res$grade_summary, res$perf_col))
  if (!is.null(res$cat_summaries) && length(res$cat_summaries) > 0) {
    return(plot_csv_category(res$cat_summaries[[1]], names(res$cat_summaries)[1]))
  }
  ggplot(res$summary, aes(x = Column, y = Mean, fill = Column)) +
    geom_col() + scale_fill_manual(values = rep(chart_palette, length.out = nrow(res$summary))) +
    labs(title = "Average value per column", subtitle = "Which column runs highest / lowest on average",
         x = NULL, y = "Mean") +
    light_plot_theme() + theme(legend.position = "none")
}

# Pie chart: grade/performance band share if available, otherwise category
# share, otherwise raw column-mean share.
plot_csv_pie <- function(res) {
  if (!is.null(res$grade_summary)) return(plot_csv_grade_pie(res$grade_summary, res$perf_col))
  if (!is.null(res$cat_summaries) && length(res$cat_summaries) > 0) {
    cat_df <- res$cat_summaries[[1]]
    colname <- names(res$cat_summaries)[1]
    return(
      ggplot(cat_df, aes(x = "", y = Count, fill = Category)) +
        geom_col(width = 1) + coord_polar("y") +
        scale_fill_manual(values = rep(chart_palette, length.out = nrow(cat_df))) +
        labs(title = paste("Share of each value in", colname),
             subtitle = "How the actual entries in this column break down",
             x = NULL, y = NULL) +
        light_plot_theme() +
        theme(axis.text = element_blank(), panel.grid = element_blank())
    )
  }
  df <- res$summary
  df$AbsMean <- abs(df$Mean)
  ggplot(df, aes(x = "", y = AbsMean, fill = Column)) +
    geom_col(width = 1) + coord_polar("y") +
    scale_fill_manual(values = rep(chart_palette, length.out = nrow(df))) +
    labs(title = "Share of total (by mean value)", subtitle = "Each slice = one column's share of the combined average",
         x = NULL, y = NULL) +
    light_plot_theme() +
    theme(axis.text = element_blank(), panel.grid = element_blank())
}

plot_csv_dashboard <- function(res) {
  plots <- list(plot_csv_bar(res), plot_csv_pie(res), plot_csv_line(res$numeric_cols, res$perf_col))
  if (!is.null(res$date_trend_df)) {
    plots[[length(plots) + 1]] <- plot_csv_date_trend(res$date_trend_df, res$roles$date_col, res$perf_col)
  }
  if (!is.null(res$group_summary)) {
    plots[[length(plots) + 1]] <- plot_csv_group_bar(res$group_summary, res$group_col, res$perf_col)
  } else if (!is.null(res$cat_summaries) && length(res$cat_summaries) > 1) {
    plots[[length(plots) + 1]] <- plot_csv_category(res$cat_summaries[[2]], names(res$cat_summaries)[2])
  } else {
    plots[[length(plots) + 1]] <- plot_csv_hist(res$numeric_cols)
  }
  if (!is.null(res$scatter_pair)) {
    plots[[length(plots) + 1]] <- plot_csv_scatter(res$data, res$scatter_pair$x, res$scatter_pair$y)
  }
  gridExtra::arrangeGrob(grobs = plots, ncol = 2)
}

# All chart-choice options that are actually meaningful for this dataset —
# Trend Over Time and Scatter only appear when the data supports them.
build_chart_choices <- function(res) {
  choices <- c("Bar Chart" = "bar", "Pie Chart" = "pie", "Line Chart" = "line")
  if (!is.null(res$date_trend_df)) choices <- c(choices, "Trend Over Time" = "trend")
  if (!is.null(res$scatter_pair)) choices <- c(choices, "Scatter (Relationship)" = "scatter")
  c(choices, "Full Dashboard" = "dashboard")
}

# Average of the performance column over time, as an area chart — good for
# "study hours trend", "attendance over the month", "score improvement".
plot_csv_date_trend <- function(trend_df, date_col, perf_col) {
  ggplot(trend_df, aes(x = Date, y = Average)) +
    geom_area(fill = "#2f6fed", alpha = 0.18) +
    geom_line(color = "#2f6fed", linewidth = 1) +
    geom_point(color = "#7c5cf5", size = 1.8) +
    labs(title = paste(perf_col, "over time"),
         subtitle = paste("Average", perf_col, "by", date_col, "— is it going up or down?"),
         x = date_col, y = paste("Average", perf_col)) +
    light_plot_theme()
}

# Relationship between two performance columns (e.g. Attendance vs Marks,
# Working Hours vs Productivity) — only built when two exist.
plot_csv_scatter <- function(df, x_col, y_col) {
  d <- df[!is.na(df[[x_col]]) & !is.na(df[[y_col]]), , drop = FALSE]
  ggplot(d, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(color = "#2f6fed", size = 2.4, alpha = 0.75) +
    geom_smooth(method = "lm", se = FALSE, color = "#7c5cf5", linewidth = 0.8, linetype = "dashed") +
    labs(title = paste(x_col, "vs", y_col), subtitle = "Each dot is one record — is there a relationship?",
         x = x_col, y = y_col) +
    light_plot_theme()
}

# ---- Image analysis ------------------------------------------------

# Try to read a photographed/scanned TABLE (e.g. a printed mark sheet or
# attendance register) out of an image using OCR, and turn it into a real
# data frame we can run through the same analytics engine as a CSV. This
# only works for images that actually contain printed/typed text laid out
# as a table (comma-separated, tab-separated, pipe-separated, or aligned
# with multiple spaces) — a normal photo will simply fail this. Runs fully
# offline once the `tesseract` package + its English data are installed.
try_ocr_table <- function(path) {
  if (!has_tesseract) return(list(ok = FALSE, reason = "tesseract package not installed"))
  
  raw_text <- tryCatch(tesseract::ocr(path), error = function(e) NA)
  if (is.na(raw_text) || !nzchar(trimws(raw_text))) {
    return(list(ok = FALSE, reason = "No readable text found in image", raw_text = raw_text))
  }
  
  lines <- strsplit(raw_text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) < 2) return(list(ok = FALSE, reason = "Not enough text lines for a table", raw_text = raw_text))
  
  # Try each plausible separator and keep whichever splits the MOST lines
  # into a consistent, multi-column shape. This is what actually makes it
  # work for comma-separated exports as well as space-aligned printouts.
  candidate_seps <- list(
    comma  = ",",
    tab    = "\t+",
    pipe   = "\\|",
    spaces = "\\s{2,}"
  )
  
  best <- NULL
  for (sep in candidate_seps) {
    split_lines <- lapply(lines, function(l) trimws(strsplit(l, sep)[[1]]))
    split_lines <- lapply(split_lines, function(v) v[nzchar(v)])
    n_cols <- sapply(split_lines, length)
    if (all(n_cols <= 1)) next
    target_cols <- as.integer(names(sort(table(n_cols), decreasing = TRUE))[1])
    if (is.na(target_cols) || target_cols < 2) next
    good_rows <- split_lines[n_cols == target_cols]
    if (length(good_rows) < 3) next
    score <- length(good_rows) * target_cols
    if (is.null(best) || score > best$score) {
      best <- list(good_rows = good_rows, target_cols = target_cols, score = score)
    }
  }
  
  if (is.null(best)) {
    return(list(ok = FALSE, reason = "Couldn't detect consistent columns", raw_text = raw_text))
  }
  
  header_candidate <- best$good_rows[[1]]
  # If the "header" row itself looks like data (numbers, or a date pattern),
  # there's no real header in the image — use generic column names and treat
  # every row as data instead of losing the first record.
  looks_like_data <- any(!is.na(suppressWarnings(as.numeric(header_candidate)))) ||
    any(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$|^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}$", header_candidate))
  
  if (looks_like_data) {
    header <- paste0("Column", seq_len(best$target_cols))
    body <- best$good_rows
  } else {
    header <- header_candidate
    body <- best$good_rows[-1]
  }
  df <- as.data.frame(do.call(rbind, body), stringsAsFactors = FALSE)
  names(df) <- make.unique(header)
  # Convert columns that are entirely numeric-looking into actual numbers,
  # so they feed into the same numeric analysis as a real CSV upload.
  for (cn in names(df)) {
    as_num <- suppressWarnings(as.numeric(df[[cn]]))
    if (!any(is.na(as_num))) df[[cn]] <- as_num
  }
  list(ok = TRUE, data = df, raw_text = raw_text, had_header = !looks_like_data)
}

analyze_image <- function(path, ext) {
  img <- tryCatch({
    if (tolower(ext) == "png") png::readPNG(path)
    else jpeg::readJPEG(path)
  }, error = function(e) NULL)
  
  if (is.null(img)) return(list(ok = FALSE, message = "Could not read this image file."))
  
  d <- dim(img)
  if (is.null(d) || length(d) < 2) {
    return(list(ok = FALSE, message = "Unsupported image format."))
  }
  
  height <- d[1]; width <- d[2]
  
  # Downsample large images before doing per-pixel math, so this stays fast
  # and memory-safe (a 12MP photo would otherwise mean tens of millions of
  # floating point comparisons for dominant-color clustering).
  target_px <- 150000
  step <- max(1, floor(sqrt((height * width) / target_px)))
  row_idx <- seq(1, height, by = step)
  col_idx <- seq(1, width, by = step)
  
  if (length(d) == 2) {
    small <- img[row_idx, col_idx, drop = FALSE]
    r <- mean(img); g <- mean(img); b <- mean(img)
    pixels <- as.vector(small)
    pixel_matrix <- cbind(as.vector(small), as.vector(small), as.vector(small))
  } else {
    n_channels <- min(3, d[3])
    r <- mean(img[,,1])
    g <- if (n_channels >= 2) mean(img[,,2]) else r
    b <- if (n_channels >= 3) mean(img[,,3]) else r
    if (n_channels == 3) {
      small_r <- img[row_idx, col_idx, 1]
      small_g <- img[row_idx, col_idx, 2]
      small_b <- img[row_idx, col_idx, 3]
    } else {
      small_r <- small_g <- small_b <- img[row_idx, col_idx, 1]
    }
    pixels <- as.vector(small_r)
    pixel_matrix <- cbind(as.vector(small_r), as.vector(small_g), as.vector(small_b))
  }
  
  brightness_pct <- round(mean(c(r, g, b)) * 100, 1)
  rgb_df <- data.frame(Channel = c("Red", "Green", "Blue"),
                       Mean = round(c(r, g, b) * 100, 1))
  
  # Dominant colors: cluster the sampled pixels into up to 5 representative
  # colors so the user sees what the image actually contains, not just
  # channel averages.
  dominant_colors <- tryCatch({
    n_px <- nrow(pixel_matrix)
    k <- min(5, length(unique(round(pixel_matrix[,1] + pixel_matrix[,2]*10 + pixel_matrix[,3]*100, 3))))
    if (k < 2 || n_px < 10) {
      hex <- grDevices::rgb(mean(pixel_matrix[,1]), mean(pixel_matrix[,2]), mean(pixel_matrix[,3]))
      data.frame(hex = hex, pct = 100,
                 name = nearest_color_name(mean(pixel_matrix[,1]), mean(pixel_matrix[,2]), mean(pixel_matrix[,3])))
    } else {
      km <- stats::kmeans(pixel_matrix, centers = k, nstart = 3, iter.max = 20)
      centers <- km$centers
      pct <- round(100 * km$size / sum(km$size), 1)
      hexes <- apply(centers, 1, function(c) {
        cc <- pmin(pmax(c, 0), 1)
        grDevices::rgb(cc[1], cc[2], cc[3])
      })
      names_v <- apply(centers, 1, function(c) {
        cc <- pmin(pmax(c, 0), 1)
        nearest_color_name(cc[1], cc[2], cc[3])
      })
      out <- data.frame(hex = hexes, pct = pct, name = names_v, stringsAsFactors = FALSE)
      out[order(-out$pct), ]
    }
  }, error = function(e) NULL)
  
  # Contrast: how spread out the light/dark values are (std dev of luminance)
  luminance <- 0.299 * pixel_matrix[,1] + 0.587 * pixel_matrix[,2] + 0.114 * pixel_matrix[,3]
  contrast_pct <- round(stats::sd(luminance) * 100, 1)
  
  aspect_ratio <- round(width / height, 2)
  
  list(ok = TRUE, width = width, height = height, brightness = brightness_pct,
       rgb_df = rgb_df, pixels = pixels, dominant_colors = dominant_colors,
       contrast = contrast_pct, aspect_ratio = aspect_ratio)
}

plot_image_bar <- function(stats) {
  ggplot(stats$rgb_df, aes(x = Channel, y = Mean, fill = Channel)) +
    geom_col() +
    scale_fill_manual(values = c(Red = "#f0578a", Green = "#22b573", Blue = "#2f6fed")) +
    labs(title = "Average color intensity", subtitle = "Higher bar = that color channel is stronger in this image",
         x = NULL, y = "Intensity (%)") +
    light_plot_theme() + theme(legend.position = "none")
}

plot_image_pie <- function(stats) {
  ggplot(stats$rgb_df, aes(x = "", y = Mean, fill = Channel)) +
    geom_col(width = 1) + coord_polar("y") +
    scale_fill_manual(values = c(Red = "#f0578a", Green = "#22b573", Blue = "#2f6fed")) +
    labs(title = "Color makeup", subtitle = "Relative share of Red vs Green vs Blue in the image",
         x = NULL, y = NULL) +
    light_plot_theme() + theme(axis.text = element_blank(), panel.grid = element_blank())
}

plot_image_line <- function(stats) {
  ggplot(stats$rgb_df, aes(x = Channel, y = Mean, group = 1)) +
    geom_line(color = "#7c5cf5", linewidth = 1.2) +
    geom_point(color = "#2f6fed", size = 3) +
    labs(title = "Channel intensity", subtitle = "Same numbers as the bar chart, shown as a trend",
         x = NULL, y = "Intensity (%)") +
    light_plot_theme()
}

plot_image_hist <- function(stats) {
  df <- data.frame(value = stats$pixels)
  ggplot(df, aes(x = value)) +
    geom_histogram(fill = "#7c5cf5", bins = 20, color = "#ffffff") +
    labs(title = "Pixel intensity distribution", subtitle = "0 = pure black, 1 = pure white — where most pixels fall",
         x = "Intensity (0-1)", y = "Count") +
    light_plot_theme()
}

plot_image_dashboard <- function(stats) {
  p1 <- plot_image_bar(stats)
  p2 <- plot_image_pie(stats)
  p3 <- plot_image_line(stats)
  p4 <- plot_image_hist(stats)
  gridExtra::arrangeGrob(p1, p2, p3, p4, ncol = 2)
}

safe_plot <- function(expr) {
  tryCatch(expr, error = function(e) {
    ggplot() +
      labs(title = paste("Could not render chart:", conditionMessage(e))) +
      light_plot_theme()
  })
}

