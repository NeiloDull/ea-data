read <- function(opts) {
  project <- syberia_project() # Always guaranteed to point to this syberia.
  # TODO: Implement opts$by
  pieces <- setNames(nm = opts$pieces, lapply(opts$pieces, function(piece) {
    file_path <- file.path(opts$with, piece) 
    if (!project$exists(file_path)) stop("No such piece ", sQuote(piece))
    runner <- project$resource(file_path)
    runner$run(remember_flag = FALSE)
    runner$context$data
  }))

  if (isTRUE(opts$bind)) { plyr::rbind.fill(pieces) }
  else { pieces }
}

write <- function(...) { } 
