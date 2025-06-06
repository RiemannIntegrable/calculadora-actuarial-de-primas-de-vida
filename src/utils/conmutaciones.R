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
        return(0)  # D_x = 0 para x > ω pues l_x = 0
    }
    
    ### Cálculos ###
    # D_x = v^x * l_x donde v = 1/(1+i)
    # Nota: D_ω ≠ 0 en general, pues l_ω puede ser > 0
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
    # N_x = Σ_{k=x}^{ω} D_k
    # Incluimos D_ω porque puede ser > 0
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
    if (x > omega) {
        return(0)  # C_x = 0 para x > ω pues l_x = 0
    }
    
    ### Cálculos ###
    # C_x = v^{x+1} * d_x = v^{x+1} * l_x * q_x
    # Nota: C_ω ≠ 0 en general, pues l_ω * q_ω = l_ω * 1 = l_ω > 0
    qx_val <- tabla_mortalidad[tabla_mortalidad$x == x, "qx"]
    if (length(qx_val) == 0) {
        print(paste("Edad", x, "no encontrada en tabla"))
        return(NA)
    }
    
    # CORRECCIÓN: Factor de descuento correcto v^{x+1}
    Cx <- (1/(1+i))^(x+1) * lx(x, tabla_mortalidad, l0) * qx_val
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
    if (x > omega) {
        return(0)
    }
    
    ### Cálculos ###
    # M_x = Σ_{k=x}^{ω} C_k
    # Tu implementación original era correcta: incluye C_ω
    Mx <- sum(sapply(0:(omega-x), function(k) C(tabla_mortalidad, x+k, i, l0)))
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
    # R_x = Σ_{k=x}^{ω} M_k  
    # Tu implementación original era correcta
    Rx <- sum(sapply(0:(omega-x), function(k) M(tabla_mortalidad, x+k, i, l0)))
    return(Rx)
}

# Función para verificar las propiedades teóricas importantes
verificar_propiedades_omega <- function(tabla_mortalidad, i, l0 = 100000) {
    omega <- max(tabla_mortalidad$x)
    
    cat("=== VERIFICACIÓN DE PROPIEDADES EN ω ===\n\n")
    
    # Verificar q_ω = 1
    q_omega <- tabla_mortalidad[tabla_mortalidad$x == omega, "qx"]
    cat("q_ω =", q_omega, "(debe ser 1)\n")
    
    # Verificar l_ω
    l_omega <- lx(omega, tabla_mortalidad, l0)
    cat("l_ω =", l_omega, "(puede ser > 0)\n")
    
    # Verificar l_{ω+1} = 0
    l_omega_plus1 <- lx(omega + 1, tabla_mortalidad, l0)
    cat("l_{ω+1} =", l_omega_plus1, "(debe ser 0)\n\n")
    
    # Verificar D_ω
    D_omega <- D(tabla_mortalidad, omega, i, l0)
    cat("D_ω =", D_omega, "(puede ser > 0)\n")
    
    # Verificar C_ω
    C_omega <- C(tabla_mortalidad, omega, i, l0)
    cat("C_ω =", C_omega, "(puede ser > 0)\n")
    
    # Verificar C_{ω+1} = 0
    C_omega_plus1 <- C(tabla_mortalidad, omega + 1, i, l0)
    cat("C_{ω+1} =", C_omega_plus1, "(debe ser 0)\n\n")
    
    # Verificar relaciones de recurrencia cerca de ω
    if (omega > 0) {
        verificar_conmutaciones_omega(tabla_mortalidad, omega - 1, i, l0)
    }
}

verificar_conmutaciones_omega <- function(tabla_mortalidad, x, i, l0 = 100000) {
    cat("Verificación de relaciones de recurrencia para x =", x, "\n")
    
    # Verificar N_x = D_x + N_{x+1}
    N_x <- N(tabla_mortalidad, x, i, l0)
    D_x <- D(tabla_mortalidad, x, i, l0)
    N_x1 <- N(tabla_mortalidad, x+1, i, l0)
    
    cat("N_x =", N_x, "\n")
    cat("D_x + N_{x+1} =", D_x + N_x1, "\n")
    cat("Diferencia N:", abs(N_x - (D_x + N_x1)), "\n\n")
    
    # Verificar M_x = C_x + M_{x+1}
    M_x <- M(tabla_mortalidad, x, i, l0)
    C_x <- C(tabla_mortalidad, x, i, l0)
    M_x1 <- M(tabla_mortalidad, x+1, i, l0)
    
    cat("M_x =", M_x, "\n")
    cat("C_x + M_{x+1} =", C_x + M_x1, "\n")
    cat("Diferencia M:", abs(M_x - (C_x + M_x1)), "\n\n")
}