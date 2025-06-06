source("basicas.R")

D <- function(tabla_mortalidad, x, i){

    ### Validacion de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.numeric(i) || i %% 1 != 0 || i < 0) {
        print("i debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }

    ### Calculos ###
    v <- 1/(1+i) 
    lx <- lx(x, tabla_mortalidad)
    Dx <- (v**x) * lx
    
    return(Dx)
}

C <- function(tabla_mortalidad, x, i) {

    ### Validacion de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.numeric(i) || i %% 1 != 0 || i < 0) {
        print("i debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }

    ### Calculos ###
    Dx <- D(tabla_mortalidad, x, i)
    v <- 1/(1+i)
    qx <- tabla_mortalidad[tabla_mortalidad$x == x, "qx"]
    Cx <- Dx * qx * v

    return(Cx)
}

M <- function(tabla_mortalidad, x, i) {

    ### Validacion de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.numeric(i) || i %% 1 != 0 || i < 0) {
        print("i debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }

    ### Calculos ###
    rango <- 0:x
    Mx

    return(Mx)
}