sink_to_temp <- function(expr) {
  sink(tempfile()); on.exit(suppressWarnings(sink()))
  eval.parent(expr)
}

print_to_string <- function(expr, condense = FALSE) {
  out <- capture.output(print(eval.parent(expr)))
  if (condense) out <- Filter(nzchar, out)
  paste(out, collapse = '\n')
}
