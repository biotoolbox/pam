get_os_id <- function() {
  sys_name <- Sys.info()[["sysname"]]

  if (sys_name == "Linux") {
    if (!file.exists("/etc/os-release")) {
      return(NA)
    }
    os_release <- readLines("/etc/os-release")
    id_line <- grep("^ID=", os_release, value = TRUE)
    if (length(id_line) == 0) {
      return(NA)
    }
    return(id_line)
  }

  return(tolower(sys_name))
}

is_reference_platform <- function() {
  os <- get_os_id()
  if (is.na(os) || !grepl("\\b(debian|ubuntu)\\b", os, ignore.case = TRUE)) {
    return(FALSE)
  }

  blas <- tolower(extSoftVersion()[["BLAS"]])
  if (!grepl("^/usr/lib/.*/blas/libblas\\.so", blas)) {
    return(FALSE)
  }

  arch <- tolower(Sys.info()[["machine"]])
  if (!(arch %in% c("x86_64", "amd64", "i386", "i686"))) {
    return(FALSE)
  }

  return(TRUE)
}
