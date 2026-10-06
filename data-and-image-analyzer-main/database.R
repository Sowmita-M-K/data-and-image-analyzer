# ================================================================
#  database.R — SQLite storage layer for Data and Image Analyzer
#  (user accounts, authentication, upload history)
# ================================================================

app_dir     <- getwd()
uploads_dir <- file.path(app_dir, "datalens_uploads")
db_path     <- file.path(app_dir, "datalens.sqlite")

if (!dir.exists(uploads_dir)) dir.create(uploads_dir)

get_db <- function() dbConnect(RSQLite::SQLite(), db_path)

init_db <- function() {
  con <- get_db()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      salt TEXT NOT NULL,
      password_hash TEXT NOT NULL,
      category TEXT,
      role TEXT
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS uploads (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      original_filename TEXT,
      stored_path TEXT,
      file_type TEXT,
      chart_type TEXT,
      details TEXT,
      upload_time TEXT
    )
  ")
  # If an older datalens.sqlite exists from before "details" was added,
  # add the column instead of forcing the user to delete the DB.
  existing_cols <- dbListFields(con, "uploads")
  if (!("details" %in% existing_cols)) {
    dbExecute(con, "ALTER TABLE uploads ADD COLUMN details TEXT")
  }
}
# NOTE: init_db() is called from global.R after this file is sourced —
# not here, so the calling order stays explicit and visible in one place.

# ================================================================
# 1. HELPER FUNCTIONS
# ================================================================

# ---- Auth helpers ---------------------------------------------

make_salt <- function() paste0(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")

hash_password <- function(password, salt) digest::digest(paste0(salt, password), algo = "sha256")

register_user <- function(username, password) {
  username <- trimws(username)
  if (nchar(username) < 3)  return(list(success = FALSE, message = "Username must be at least 3 characters."))
  if (nchar(password) < 4)  return(list(success = FALSE, message = "Password must be at least 4 characters."))
  
  con <- get_db()
  on.exit(dbDisconnect(con))
  
  existing <- dbGetQuery(con, "SELECT id FROM users WHERE username = ?", params = list(username))
  if (nrow(existing) > 0) return(list(success = FALSE, message = "That username is already taken."))
  
  salt <- make_salt()
  hash <- hash_password(password, salt)
  
  dbExecute(con,
            "INSERT INTO users (username, salt, password_hash, category, role) VALUES (?, ?, ?, NULL, NULL)",
            params = list(username, salt, hash))
  
  new_id <- dbGetQuery(con, "SELECT id FROM users WHERE username = ?", params = list(username))$id[1]
  list(success = TRUE, message = "Account created!", user_id = new_id)
}

login_user <- function(username, password) {
  con <- get_db()
  on.exit(dbDisconnect(con))
  
  row <- dbGetQuery(con, "SELECT * FROM users WHERE username = ?", params = list(trimws(username)))
  if (nrow(row) == 0) return(list(success = FALSE, message = "No account with that username."))
  
  attempt_hash <- hash_password(password, row$salt[1])
  if (!identical(attempt_hash, row$password_hash[1])) {
    return(list(success = FALSE, message = "Incorrect password."))
  }
  
  list(success = TRUE, user_id = row$id[1], username = row$username[1],
       category = row$category[1], role = row$role[1])
}

update_user_role <- function(user_id, category, role) {
  con <- get_db()
  on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE users SET category = ?, role = ? WHERE id = ?",
            params = list(category, role, user_id))
}

# ---- Upload / history helpers -----------------------------------

save_upload_record <- function(user_id, original_filename, stored_path, file_type, chart_type, details) {
  con <- get_db()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    INSERT INTO uploads (user_id, original_filename, stored_path, file_type, chart_type, details, upload_time)
    VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(user_id, original_filename, stored_path, file_type, chart_type, details,
                          as.character(Sys.time())))
}

# ---- Upload / history helpers (continued) -----------------------

get_user_uploads <- function(user_id) {
  con <- get_db()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT * FROM uploads WHERE user_id = ? ORDER BY upload_time DESC",
             params = list(user_id))
}
