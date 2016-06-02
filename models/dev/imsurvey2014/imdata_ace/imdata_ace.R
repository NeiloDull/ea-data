list(
  import = list(file = file.path(root(), "data", "ace-results.csv")),
  data = list(
    "Add A to IDs" = list(column_transformation(function(ids) {
      lapply(ids, function(id) paste0('A', id))
    }), 'Response.ID') 
  )
)
