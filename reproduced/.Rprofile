activate_paths <- c(
  file.path(getwd(), "renv", "activate.R"),
  file.path(getwd(), "reproduced", "renv", "activate.R"),
  file.path(dirname(getwd()), "reproduced", "renv", "activate.R")
)
for (candidate in unique(activate_paths)) {
  if (is.character(candidate) && nzchar(candidate) && file.exists(candidate)) {
    source(candidate, local = FALSE)
    break
  }
}
if (exists("candidate", inherits = FALSE)) {
  rm(candidate)
}
rm(activate_paths)

local_library_paths <- c(
  file.path(getwd(), ".Rlib"),
  file.path(getwd(), "reproduced", ".Rlib"),
  file.path(dirname(getwd()), "reproduced", ".Rlib")
)
existing_local_libraries <- unique(local_library_paths[dir.exists(local_library_paths)])
if (length(existing_local_libraries) > 0) {
  .libPaths(c(normalizePath(existing_local_libraries, winslash = "/"), .libPaths()))
}
rm(local_library_paths, existing_local_libraries)
