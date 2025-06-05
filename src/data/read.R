library(readxl)
library(here)

leer_excel <- function(ruta_relativa, hoja = 1, rango = NULL, tipos_col = NULL, 
                      valores_na = c("", "NA", "N/A", "#N/A"), 
                      saltar_filas = 0, nombres_col = TRUE) {
                        
  # Validación de entrada
  if (!is.character(ruta_relativa) || length(ruta_relativa) != 1) {
    stop("ruta_relativa debe ser un string único")
  }
  
  # Construcción de ruta absoluta
  ruta_completa <- here::here(ruta_relativa)
  
  # Verificación de existencia del archivo
  if (!file.exists(ruta_completa)) {
    stop(paste("El archivo no existe en la ruta:", ruta_completa))
  }
  
  # Verificación de extensión
  if (!grepl("\\.(xlsx|xls)$", ruta_completa, ignore.case = TRUE)) {
    stop("El archivo debe tener extensión .xlsx o .xls")
  }
  
  # Lectura del archivo con manejo de errores
  tryCatch({
    datos <- readxl::read_excel(
      path = ruta_completa,
      sheet = hoja,
      range = rango,
      col_names = nombres_col,
      col_types = tipos_col,
      na = valores_na,
      skip = saltar_filas
    )
    
    # Validación del resultado
    if (nrow(datos) == 0) {
      warning("El archivo se leyó correctamente pero no contiene datos")
    }
    
    return(as.data.frame(datos))
    
  }, error = function(e) {
    stop(paste("Error al leer el archivo:", e$message))
  })
}