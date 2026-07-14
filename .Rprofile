activate_paths <- c(
  file.path(getwd(), "renv", "activate.R"),
  file.path(getwd(), "reproduced", "renv", "activate.R"),
  file.path(getwd(), "reproduced", "reproduced", "renv", "activate.R"),
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
