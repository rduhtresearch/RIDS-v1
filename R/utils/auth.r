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