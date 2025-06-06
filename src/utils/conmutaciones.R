source("basicas.R")

D <- function(tabla_mortalidad, x, i, l0 = 100000) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(i) || length(i) != 1) {
        print("i debe ser un número único (tasa de interés)")
        return(NA)
    }
    if (!is.numeric(l0) || l0 %% 1 != 0 || l0 <= 0) {
        print("l0 debe ser un entero positivo")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x > omega) {
        return(0)
    }
    
    ### Cálculos ###
    Dx <- (1/(1+i))^x * lx(x, tabla_mortalidad, l0)
    return(Dx)
}

N <- function(tabla_mortalidad, x, i, l0 = 100000) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(i) || length(i) != 1) {
        print("i debe ser un número único (tasa de interés)")
        return(NA)
    }
    if (!is.numeric(l0) || l0 %% 1 != 0 || l0 <= 0) {
        print("l0 debe ser un entero positivo")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x > omega) {
        return(0)
    }
    
    ### Cálculos ###
    Nx <- sum(sapply(x:omega, function(k) D(tabla_mortalidad, k, i, l0)))
    return(Nx)
}

C <- function(tabla_mortalidad, x, i, l0 = 100000) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(i) || length(i) != 1) {
        print("i debe ser un número único (tasa de interés)")
        return(NA)
    }
    if (!is.numeric(l0) || l0 %% 1 != 0 || l0 <= 0) {
        print("l0 debe ser un entero positivo")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x >= omega) {
        return(0)
    }
    
    ### Cálculos ###
    Cx <- (1/(1+i))^(x+1) * ndx(1, x, tabla_mortalidad, l0)
    return(Cx)
}

M <- function(tabla_mortalidad, x, i, l0 = 100000) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(i) || length(i) != 1) {
        print("i debe ser un número único (tasa de interés)")
        return(NA)
    }
    if (!is.numeric(l0) || l0 %% 1 != 0 || l0 <= 0) {
        print("l0 debe ser un entero positivo")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x >= omega) {
        return(0)
    }
    
    ### Cálculos ###
    Mx <- sum(sapply(x:(omega-1), function(k) C(tabla_mortalidad, k, i, l0)))
    return(Mx)
}

R <- function(tabla_mortalidad, x, i, l0 = 100000) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(i) || length(i) != 1) {
        print("i debe ser un número único (tasa de interés)")
        return(NA)
    }
    if (!is.numeric(l0) || l0 %% 1 != 0 || l0 <= 0) {
        print("l0 debe ser un entero positivo")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x > omega) {
        return(0)
    }
    
    ### Cálculos ###
    Rx <- sum(sapply(x:omega, function(k) N(tabla_mortalidad, k, i, l0)))
    return(Rx)
}