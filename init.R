# Instalar ragg con dependencias correctas
install.packages("ragg", dependencies = TRUE, repos = "http://cran.rstudio.com/")
install.packages("textshaping", dependencies = TRUE, repos = "http://cran.rstudio.com/")
# Instalar tidyverse después de ragg
install.packages("tidyverse", dependencies = TRUE, repos = "http://cran.rstudio.com/")

install.packages(c("UpSetR", "eulerr", "upsetjs", "plotly", "waiter", "visNetwork", "shinyalert", "shinycssloaders", "ggrepel", "RColorBrewer", "lubridate", "forcats", "stringr", "dplyr", "purrr", "readr", "tidyr", "tibble", "ggplot2", "tidyverse", "DT", "shinyWidgets", "shinydashboard", "shiny"))
