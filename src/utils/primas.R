### Prima de un seguro dotal puro ###
Axn1 <- function(tabla_mortalidad, x, n, i) {
    ### Validación de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        print("n debe ser un entero no negativo")
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
        print(paste("Edad", x, "está fuera del rango de la tabla"))
        return(NA)
    }
    if (x >= omega) {
        return(0)
    }
    if (n == 0) {
        return(1)  # Pago inmediato
    }
    if (x + n > omega) {
        return(0)  # No puede sobrevivir más allá de omega
    }

    ### Cálculos ###
    Axn1 <- tpx(n, x, tabla_mortalidad) * (1/(1+i))^n

    return(Axn1)
}

### Prima de un seguro entero por valor de 1 pagadero al final del año de muerte ###
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
        print(paste("Edad", x, "está fuera del rango de la tabla"))
        return(NA)
    }
    if (x > omega) {
        return(0)  # No hay supervivientes
    }

    ### Cálculos ###
    Ax <- sum(sapply(0:(omega-x), function(k) (1/(1+i))^(k+1) * tpx(k,x,tabla_mortalidad) * qx(x+k, tabla_mortalidad)))
    
    return(Ax)
}

### Prima de un seguro entero por valor de 1 pagadero al final del mes de muerte asumiendo UDD ###
Amx <- function(tabla_mortalidad, x, m, i) {
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
    if (!is.numeric(m) || m %% 1 != 0 || m < 0) {
        print("m debe ser un entero no negativo")
        return(NA)
    }

    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    
    if (x < x_min) {
        print(paste("Edad", x, "está fuera del rango de la tabla"))
        return(NA)
    }
    if (x > omega) {
        return(0)  # No hay supervivientes
    }

    ### Cálculos ###
    im = m * ((1+i)^(1/m) - 1)
    Amx <- (i / im) * Ax(tabla_mortalidad, x, i)
    
    return(Amx)
}

### Prima de un seguro entero por valor asegurado creciente de 1 cada año pagadero al final del año de muerte ###
IAx <- function(tabla_mortalidad, x, i) {
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
        print(paste("Edad", x, "está fuera del rango de la tabla"))
        return(NA)
    }
    if (x > omega) {
        return(0)  # No hay supervivientes
    }

    ### Cálculos ###
    IAx <- sum(sapply(0:(omega-x), function(k) (k+1)*(1/(1+i))^(k+1)*tpx(k,x,tabla_mortalidad)*qx(x+k, tabla_mortalidad)))
    
    return(IAx)
}

### Prima de un seguro entero por valor asegurado creciente de r cada año pagadero al final del año de muerte ###
IrAx <- function(tabla_mortalidad, x, r, i) {
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
    if (!is.numeric(r)) {
        print("r debe ser un número")
        return(NA)
    }

    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    
    if (x < x_min) {
        print(paste("Edad", x, "está fuera del rango de la tabla"))
        return(NA)
    }
    if (x > omega) {
        return(0)  # No hay supervivientes
    }

    ### Cálculos ###
    IrAx <- (1-r)*Ax(tabla_mortalidad, x, i) + r*IAx(tabla_mortalidad, x, i)
    
    return(IrAx)
}

### Prima de un seguro entero por valor asegurado creciente de r cada 1/m meses pagadero al final del mes de muerte ###
ImrAmx <- function(tabla_mortalidad, x, r, i) {
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
    if (!is.numeric(r)) {
        print("r debe ser un número")
        return(NA)
    }

    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    
    if (x < x_min) {
        print(paste("Edad", x, "está fuera del rango de la tabla"))
        return(NA)
    }
    if (x > omega) {
        return(0)  # No hay supervivientes
    }

    ### Cálculos ###
    im = m * ((1+i)^(1/m) - 1)
    Ax <- Ax(tabla_mortalidad, x, i)
    IAx <- IAx(tabla_mortalidad, x, i)
    ImrAmx <- 
    
    return(ImrAmx)
}