hash_password <- function(pw) {
  tryCatch({
    pw_hash <- sodium::password_store(pw)
    message('User password successfuly hashed')
    return(pw_hash)
  }, error = function(e) {
    message('error: ', e$message)
  })
}

verify_password <- function(pw, pw_hash) {
  tryCatch({
    pw_v <- sodium::password_verify(pw_hash, pw)
    if(pw_v) {
      message('password successfuly verified')
      return(TRUE)
    } else {
      message('password not verified')
      return(FALSE)
    }
  }, error = function(e){
    message('Error: ',e$message)
    return(FALSE)
  })
}

is_admin <- function(role) {
  
  if(role == 'admin') {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_manager <- function(role) {
  if(role %in% c('admin', 'dev')) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

can_edit <- function(role) {
  if(role %in% c('dev')) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



# Generate a one-time setup token 
generate_token <- function(user_id) {
  token      <- paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
  expires_at <- format(Sys.time() + (24 * 60 * 60), "%Y-%m-%d %H:%M:%S")
  
  dbExecute(CON,
            "INSERT INTO tokens (token, user_id, expires_at, used) VALUES (?, ?, ?, FALSE)",
            params = list(token, user_id, expires_at)
  )
  
  return(token)
}

# Validate and consume a token 
consume_token <- function(token) {
  row <- dbGetQuery(CON,
                    "SELECT * FROM tokens 
     WHERE token = ? 
     AND used = FALSE 
     AND expires_at > CURRENT_TIMESTAMP",
                    params = list(token)
  )
  
  if (nrow(row) == 0) return(NULL)
  
  dbExecute(CON,
            "UPDATE tokens SET used = TRUE WHERE token = ?",
            params = list(token)
  )
  
  return(row)
}

# Create New User
create_user <- function(username, role) {
  tryCatch({
    dbExecute(CON,
              "INSERT INTO users (username, role) VALUES (?, ?)",
              params = list(username, role)
    )
    
    user_id <- dbGetQuery(CON,
                          "SELECT id FROM users WHERE username = ?",
                          params = list(username)
    )$id
    
    token <- generate_token(user_id)
    return(token)
    
  }, error = function(e) {
    message("create_user error: ", e$message)
    return(NULL)
  })
}