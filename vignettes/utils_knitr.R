
# Simple way to output code as-is to knitr
kat <- knitr::asis_output

# Function to wrap example code between ``` before katting
kat_code <- function(code, language="r") {
  result <- c(
      paste0("```",language, "\n"),
      code,
      "\n```")
  kat(result)
}
