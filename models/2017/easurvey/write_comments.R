file_name <- "data/2017/ea_comments.txt"
unlink(file_name)
file_handle <- file(file_name, "w")
for (var in get_vars(df, "comment")) {
  cat(paste("##", var, " ##\n\n\n\n"), file = file_handle, append = TRUE)
  for (comment in df[[var]]) {
    cat(paste0(comment, "\n\n"), file = file_handle, append = TRUE)
  }
}
close(file_handle)
