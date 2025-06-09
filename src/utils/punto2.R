ImrAmx <- function(tabla_mortalidad, x, r, i, m) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        stop("x debe ser un entero no negativo")
    }
    if (!is.data.frame(tabla_mortalidad) || !all(c("x", "qx") %in% names(tabla_mortalidad))) {
        stop("tabla_mortalidad debe ser un data frame con columnas 'x' y 'qx'")
    }
    if (!is.numeric(i) || i <= 0) {
        stop("i debe ser un número positivo")
    }
    if (!is.numeric(r) || r < 0) {
        stop("r debe ser un número no negativo")
    }
    if (!is.numeric(m) || m %% 1 != 0 || m <= 0) {
        stop("m debe ser un entero positivo")
    }
    
    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    
    if (x < x_min || x > omega) {
        stop(paste("Edad", x, "está fuera del rango de la tabla"))
    }
    
    ### Constantes ###
    v <- 1/(1+i)  # factor de descuento
    im <- m * ((1+i)^(1/m) - 1)  # tasa nominal convertible m-esimalmente

    if (x == omega) {
        # En omega, muerte es inmediata con probabilidad 1
        # Ax = IAx = v, por lo que el cálculo se simplifica
        return((i/im) * v + r*((i-im)/(im^2)))
    }
    
    ### Vectorización de probabilidades de mortalidad ###
    inicio <- which(tabla_mortalidad$x == x)
    fin <- nrow(tabla_mortalidad)
    
    # Vectores de probabilidades desde edad x hasta omega
    qx_vec <- tabla_mortalidad$qx[inicio:fin] # qx_vec[j] = q_{x+j-1}
    px_vec <- 1 - qx_vec # px_vec[j] = p_{x+j-1}
    
    ### Probabilidades de supervivencia acumuladas ###
    rango_años <- length(qx_vec)
    tpx <- numeric(rango_años) # tpx[k] = _{k-1}p_x
    tpx[1] <- 1  # _{0}p_x = 1
    
    for(k in 2:rango_años) {
        tpx[k] <- tpx[k-1] * px_vec[k-1] # _{k-1}p_x = _{k-2}p_x p_{x+k-2}
    }
    
    ### Cálculo de seguros actuariales ###
    # Seguro de vida entero: Ax
    Ax <- sum(sapply(0:(omega-x), function(k) {
        v^(k+1) * tpx[k+1] * qx_vec[k+1]
    }))

    # Seguro de vida entero creciente: (IA)x
    IAx <- sum(sapply(0:(omega-x), function(k) {
        (k+1) * v^(k+1) * tpx[k+1] * qx_vec[k+1]
    }))
    
    ### Prima fraccionaria con valor asegurado creciente dentro del año asumiendo UDD ###
    ImrAmx <- (i/im) * (Ax + r*(IAx - Ax)) + r*((i-im)/(im^2))*Ax
    
    return(ImrAmx)
}