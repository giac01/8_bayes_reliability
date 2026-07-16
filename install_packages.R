# Checks a list of R packages and installs any that are missing.

required_packages <- c(
  "abind", "admisc", "ape", "arm", "arrow", "askpass", "assertthat",
  "backports", "base64enc", "bayesplot", "bayestestR", "BH", "bigD", "biglm",
  "BiocManager", "bit", "bit64", "bitops", "blob", "brew", "bridgesampling",
  "brio", "brms", "Brobdingnag", "broom", "bslib", "cachem", "callr", "car",
  "carData", "cellranger", "checkmate", "cli", "clipr", "clubSandwich",
  "clue", "cmdstanr", "coda", "colorspace", "commonmark", "CompQuadForm",
  "conflicted", "corpcor", "correlation", "cowplot", "cpp11", "crayon",
  "credentials", "crosstalk", "curl", "dagitty", "data.table", "datawizard",
  "DBI", "dbplyr", "dbscan", "Deriv", "desc", "devtools", "DHARMa",
  "DiagrammeR", "diffobj", "digest", "distr", "distrEx", "distributional",
  "doBy", "docopt", "doParallel", "downlit", "dplyr", "dtplyr", "duckdb",
  "effectsize", "ellipsis", "evaluate", "extrafont", "extrafontdb", "fansi",
  "farver", "fastDummies", "fastmap", "fdrtool", "fontawesome",
  "fontBitstreamVera", "fontLiberation", "fontquiver", "forcats", "foreach",
  "forecast", "Formula", "fracdiff", "fs", "fst", "fstcore", "furrr",
  "future", "future.apply", "gap", "gap.datasets", "gargle", "gbtoolbox",
  "gdata", "gdtools", "generics", "gert", "ggdist", "ggh4x", "ggmice",
  "ggplot2", "ggrepel", "ggridges", "ggtext", "gh", "gitcreds", "glasso",
  "globals", "glmnet", "glue", "googledrive", "googlesheets4", "GPArotation",
  "gridExtra", "gridtext", "gsubfn", "gt", "gtable", "gtools", "haven",
  "HDInterval", "highr", "Hmisc", "hms", "hrbrthemes",
  "htmlTable", "htmltools", "htmlwidgets", "httpuv", "httr", "httr2",
  "ICC", "ids", "igraph", "ini", "inline", "insight", "isoband",
  "iterators", "jomo", "jpeg", "jquerylib", "jsonlite", "juicyjuice",
  "kableExtra", "knitr", "kutils", "labeling", "Lahman", "later", "lavaan",
  "lavaanPlot", "lazyeval", "lifecycle", "lisrelToR", "listenv", "litedown",
  "littler", "lme4", "lmtest", "loo", "lubridate", "magick", "magrittr",
  "marginaleffects", "markdown", "MatrixModels", "matrixStats", "MBESS", "memoise",
  "mi", "mice", "microbenchmark", "mime", "miniUI", "minqa", "mitml",
  "mnormt", "modelbased", "modelr", "MplusAutomation", "MuMIn", "mvtnorm",
  "nleqslv", "nloptr", "nonnest2", "numDeriv", "nycflights13", "OpenMx",
  "openssl", "openxlsx", "ordinal", "otel", "pak", "pan", "pander",
  "parallelly", "parameters", "paran", "patchwork", "pbapply", "pbivnorm",
  "pbkrtest", "performance", "pillar", "pkgbuild", "pkgconfig", "pkgdown",
  "pkgload", "PlotTools", "plogr", "plotly", "plyr", "png", "polycor",
  "posterior", "pracma", "praise", "prettyunits", "pROC", "processx",
  "profvis", "progress", "progressr", "promises", "proto", "ps", "psych",
  "psychTools", "purrr", "pwr", "qgam", "qgraph", "quadprog", "quantreg",
  "QuickJSR", "R.methodsS3", "R.oo", "R2HTML", "R6", "ragg", "RANN",
  "rappdirs", "rbibutils", "rcmdcheck", "RColorBrewer", "Rcpp",
  "RcppArmadillo", "RcppEigen", "RcppHungarian", "RcppParallel", "RCurl",
  "Rdpack", "reactable", "reactR", "readr", "readxl", "reformulas",
  "remotes", "rematch", "rematch2", "reprex", "reshape2", "rlang",
  "RMariaDB", "rmarkdown", "rockchalk", "roxygen2", "rpf", "RPostgres",
  "rprojroot", "RSQLite", "rstan", "rstantools", "rstudioapi", "rtf",
  "Rttf2pt1", "RUnit", "rversions", "rvest", "S7", "sandwich", "sass",
  "scales", "see", "selectr", "sem", "semPlot", "sessioninfo", "sfsmisc",
  "shape", "shiny", "sourcetools", "SparseM", "speedglm", "StanHeaders",
  "startupmsg", "statip", "stringi", "stringr", "svglite", "sys",
  "systemfonts", "Ternary", "tensorA", "testthat", "texreg", "textshaping",
  "tibble", "tidybayes", "tidyr", "tidyselect", "tidySEM", "tidyverse", "timechange",
  "timeDate", "tinytex", "tzdb", "ucminf", "umx", "urca", "urlchecker",
  "usethis", "utf8", "uuid", "V8", "vctrs", "viridis", "viridisLite",
  "visNetwork", "vroom", "waldo", "weights", "whisker", "withr", "xfun",
  "XML", "xml2", "xopen", "xtable", "yaml", "zip", "zoo"
)

required_packages <- unique(required_packages)

installed <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed)

if (length(missing_packages) == 0) {
  message("All ", length(required_packages), " packages are already installed.")
} else {
  message("Installing ", length(missing_packages), " missing package(s):")
  message(paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

devtools::install_github("giac01/gbtoolbox")
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
