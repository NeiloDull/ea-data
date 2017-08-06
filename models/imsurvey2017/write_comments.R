file_name <- "data/ea_comments.txt"
unlink(file_name)
file_handle <- file(file_name, "w")
for (var in get_vars(df, "commment")) {
  write(file_handle, paste("##", var, " ##\n\n\n\n"), append = TRUE)
  for (comment in df[[var]]) {
    write(file_handle, paste0(comment, "\n\n"), append = TRUE)
  }
}
close(file_handle)
