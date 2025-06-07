### Prima de un seguro dotal puro ###
Axn1 <- function(tabla_mortalidad, x, n, i) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(i)) {
        print("i debe ser un número")
        return(NA)
    }
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        print("n debe ser un entero no negativo")
        return(NA)
    }

    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    if (x < x_min) {
        return(NA)
    }
    if (x > omega) {
        return(0)
    }
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }

    ### Calculos ###
    Axn1 <- tpx(n,x,tabla_mortalidad) * (1/(1+i))^n

    return(Axn1)
}

### Primas de seguros pagaderos al final del año de muerte ###
Ax <- function(tabla_mortalidad, x, i) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(i)) {
        print("i debe ser un número")
        return(NA)
    }

    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    if (x < x_min) {
        return(NA)
    }
    if (x > omega) {
        return(0)
    }
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }

    ### Calculos ###
    Ax <- 

    return(Ax)
}

Ax1n <- function(tabla_mortalidad, x, n, i) {

}

mAx1n <- function(tabla_mortalidad, m, x, n, i) {

}

### Primas de seguros pagaderos al final del mes de muerte ###
Amx <- function(x) {

}