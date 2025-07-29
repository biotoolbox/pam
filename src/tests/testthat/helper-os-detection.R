# Returns a string identifying the OS or distro
get_os_distro <- function() {
  sys_name <- Sys.info()[["sysname"]]

  if (sys_name == "Linux") {
    # If the file doesn't exist, we can't determine distro
    if (!file.exists("/etc/os-release")) {
      return("unknown")
    }

    # Try reading the file safely
    os_release <- tryCatch(
      readLines("/etc/os-release"),
      error = function(e) NULL
    )

    if (is.null(os_release)) {
      return("unknown")
    }

    # Extract ID_LIKE and ID
    distro_id_like <- grep("^ID_LIKE=", os_release, value = TRUE)
    distro_id_like <- if (length(distro_id_like) > 0) {
      sub("^ID_LIKE=", "", distro_id_like)
    } else {
      ""
    }

    distro_id <- grep("^ID=", os_release, value = TRUE)
    distro_id <- if (length(distro_id) > 0) {
      sub("^ID=", "", distro_id)
    } else {
      ""
    }

    distro_names <- paste(distro_id_like, distro_id, collapse = " ")
    if (nchar(trimws(distro_names)) == 0) {
      return("unknown")
    } else {
      return(distro_names)
    }
  }

  # For non-Linux, just return the system name (e.g. "Windows", "Darwin")
  return(tolower(sys_name))
}

is_supported_os <- function() {
  os <- get_os_distro()
  grepl("\\b(ubuntu|debian|windows)\\b", os, ignore.case = TRUE)
}

is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}

is_debian_or_ubuntu <- function() {
  os <- get_os_distro()
  grepl("\\b(debian|ubuntu)\\b", os, ignore.case = TRUE)
}
