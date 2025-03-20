# Script para generar init.R y requirements.txt desde paquetes cargados en la sesión actual

# Paso 1: Obtener los paquetes actualmente cargados
loaded_pkgs <- .packages()

# Eliminar paquetes base (que no se instalan)
base_pkgs <- rownames(installed.packages(priority = "base"))
pkgs <- setdiff(loaded_pkgs, base_pkgs)

# Paso 2: Verificar origen de los paquetes
is_cran <- function(pkg) {
  !is.na(available.packages()[pkg, "Package"])
}

is_bioc <- function(pkg) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  !is.na(suppressWarnings(BiocManager::available()[pkg]))
}

# Clasificar paquetes
cran_pkgs <- c()
bioc_pkgs <- c()
unknown_pkgs <- c()

for (pkg in pkgs) {
  if (is_cran(pkg)) {
    cran_pkgs <- c(cran_pkgs, pkg)
  } else if (is_bioc(pkg)) {
    bioc_pkgs <- c(bioc_pkgs, pkg)
  } else {
    unknown_pkgs <- c(unknown_pkgs, pkg)
  }
}

# Mostrar resumen
cat("🟢 Paquetes CRAN:\n", paste(cran_pkgs, collapse = ", "), "\n\n")
cat("🔵 Paquetes Bioconductor:\n", paste(bioc_pkgs, collapse = ", "), "\n\n")
cat("❓ Paquetes desconocidos (posiblemente GitHub):\n", paste(unknown_pkgs, collapse = ", "), "\n\n")

# Paso 3: Crear init.R
init_lines <- c()

if (length(cran_pkgs) > 0) {
  init_lines <- c(init_lines,
                  sprintf("install.packages(c(%s))",
                          paste(sprintf('"%s"', cran_pkgs), collapse = ", ")))
}

if (length(bioc_pkgs) > 0) {
  init_lines <- c(init_lines,
                  'if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")',
                  sprintf("BiocManager::install(c(%s))",
                          paste(sprintf('"%s"', bioc_pkgs), collapse = ", ")))
}

if (length(unknown_pkgs) > 0) {
  init_lines <- c(init_lines,
                  'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")')
  for (pkg in unknown_pkgs) {
    init_lines <- c(init_lines, sprintf('# remotes::install_github("usuario/%s")  # EDITA esto manualmente', pkg))
  }
}

writeLines(init_lines, "init.R")
cat("✅ Archivo init.R generado\n")

# Paso 4: Crear requirements.txt (sólo CRAN)
writeLines(cran_pkgs, "requirements.txt")
cat("✅ Archivo requirements.txt generado\n")

















# # Crea automáticamente init.R y requirements.txt para Heroku
# 
# # Paquetes necesarios para analizar y buscar repos
# if (!requireNamespace("tools")) install.packages("tools")
# 
# # Paso 1: Leer paquetes cargados desde app.R
# app_file <- "app.R"  # Cambia si tu script se llama distinto
# lines <- readLines(app_file)
# 
# # Extraer paquetes de library() y require()
# pkg_lines <- grep("^(library|require)\\(", lines, value = TRUE)
# pkgs <- unique(gsub(".*\\(([^)]+)\\).*", "\\1", pkg_lines))
# 
# # Filtrar nombres válidos
# pkgs <- pkgs[grepl("^[a-zA-Z0-9\\.]+$", pkgs)]
# 
# # Paso 2: Verificar origen de los paquetes
# is_cran <- function(pkg) {
#   !is.na(available.packages()[pkg, "Package"])
# }
# 
# is_bioc <- function(pkg) {
#   if (!requireNamespace("BiocManager", quietly = TRUE)) {
#     install.packages("BiocManager")
#   }
#   !is.na(suppressWarnings(BiocManager::available()[pkg]))
# }
# 
# # Paso 3: Clasificar paquetes
# cran_pkgs <- c()
# bioc_pkgs <- c()
# unknown_pkgs <- c()
# 
# for (pkg in pkgs) {
#   if (is_cran(pkg)) {
#     cran_pkgs <- c(cran_pkgs, pkg)
#   } else if (is_bioc(pkg)) {
#     bioc_pkgs <- c(bioc_pkgs, pkg)
#   } else {
#     unknown_pkgs <- c(unknown_pkgs, pkg)
#   }
# }
# 
# # Mostrar resumen
# cat("🟢 Paquetes CRAN:", paste(cran_pkgs, collapse = ", "), "\n")
# cat("🔵 Paquetes Bioconductor:", paste(bioc_pkgs, collapse = ", "), "\n")
# cat("❓ Paquetes desconocidos (posiblemente GitHub):", paste(unknown_pkgs, collapse = ", "), "\n")
# 
# # Paso 4: Crear init.R
# init_lines <- c()
# 
# if (length(cran_pkgs) > 0) {
#   init_lines <- c(init_lines,
#                   sprintf("install.packages(c(%s))",
#                           paste(sprintf('"%s"', cran_pkgs), collapse = ", ")))
# }
# 
# if (length(bioc_pkgs) > 0) {
#   init_lines <- c(init_lines,
#                   'if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")',
#                   sprintf("BiocManager::install(c(%s))",
#                           paste(sprintf('"%s"', bioc_pkgs), collapse = ", ")))
# }
# 
# if (length(unknown_pkgs) > 0) {
#   init_lines <- c(init_lines,
#                   'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")')
#   # Aquí puedes agregar manualmente los repos GitHub si los conoces
#   for (pkg in unknown_pkgs) {
#     init_lines <- c(init_lines, sprintf('remotes::install_github("usuario/%s")  # Edita según el repositorio real', pkg))
#   }
# }
# 
# writeLines(init_lines, "init.R")
# cat("✅ Archivo init.R generado\n")
# 
# # Paso 5: Crear requirements.txt (solo CRAN)
# writeLines(cran_pkgs, "requirements.txt")
# cat("✅ Archivo requirements.txt generado\n")
