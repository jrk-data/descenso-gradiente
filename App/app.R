# ==============================================================================
# SHINY APP: DESCENSO DEL GRADIENTE INTERACTIVO (VERSIÓN LIGERA)
# Autor: EEA
# ==============================================================================

library(shiny)
library(readr)
library(dplyr)

# 1. CARGA Y PREPARACIÓN DE DATOS (GLOBAL)
# ------------------------------------------------------------------------------
# Se ejecuta una sola vez al iniciar la app
if(file.exists("kc_house_data.csv")) {
  # Intentar carga robusta
  tryCatch({
    data <- read_csv("kc_house_data.csv", show_col_types = FALSE)
  }, error = function(e) {
    data <- read.csv("kc_house_data.csv")
  })
} else {
  stop("El archivo 'kc_house_data.csv' no se encuentra en el directorio de trabajo.")
}

# Limpieza
df_clean <- data %>%
  select(price, sqft_living, bathrooms) %>%
  na.omit()

# Escalado (Z-score)
X1 <- as.vector(scale(df_clean$sqft_living))
X2 <- as.vector(scale(df_clean$bathrooms))
Y  <- as.vector(scale(df_clean$price))

# Matriz de Diseño
X_mat <- cbind(1, X1, X2)
m <- length(Y)

# --- CÁLCULO DEL MÍNIMO GLOBAL TEÓRICO (SOLUCIÓN EXACTA) ---
# Usamos la Ecuación Normal: Beta = (X'X)^-1 X'Y
# Esto nos da el punto exacto donde el error es matemáticamente el menor posible.
beta_exact <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% Y
min_b1 <- beta_exact[2]
min_b2 <- beta_exact[3]

# Calculamos el costo mínimo posible (el suelo absoluto del valle)
error_exact <- (X_mat %*% beta_exact) - Y
min_cost_val <- mean(error_exact^2)


# --- CONSTRUCCIÓN DE LA MALLA 3D CON CONTRASTE ALTO ---
grid_res <- 40 # Mayor resolución para mejor degradé
b1_grid <- seq(-1.5, 1.5, length.out = grid_res) # Ampliamos un poco el rango
b2_grid <- seq(-1.5, 1.5, length.out = grid_res)

cost_func <- function(b1, b2) {
  # Asumimos beta0 fijo en su valor óptimo (cerca de 0) para simplificar visualización 3D
  err <- (X_mat %*% c(0, b1, b2)) - Y
  return(mean(err^2))
}
Z_surface <- outer(b1_grid, b2_grid, Vectorize(cost_func))

# --- LÓGICA DE COLORES POR CUANTILES (PARA EL CONTRASTE) ---
# Promedio de altura por faceta
nrz <- nrow(Z_surface)
ncz <- ncol(Z_surface)
z_facet <- (Z_surface[-1, -1] + Z_surface[-1, -ncz] + 
              Z_surface[-nrz, -1] + Z_surface[-nrz, -ncz]) / 4

# PALETA DE ALTO CONTRASTE:
# Rojo (Mal) -> Amarillo -> Cian -> Azul -> Negro (Perfecto)
custom_colors <- colorRampPalette(c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", "#4575B4", "#081D58"))(100)

# Usamos 'cut' con 'breaks' basados en cuantiles. 
# Esto asegura que haya variación de color incluso en el fondo plano del valle.
# Forzamos más detalle en los valores bajos del error.
facet_col_indices <- cut(z_facet, breaks = quantile(z_facet, probs = seq(0, 1, length.out = 101)), include.lowest = TRUE)
final_facet_colors <- custom_colors[facet_col_indices]


# ==============================================================================
# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #FAFAFA; color: #333; }
    .shiny-input-container { background-color: white; border: 1px solid #ddd; padding: 15px; border-radius: 8px;}
  "))),
  
  titlePanel("Simulador de Convergencia: Descenso del Gradiente"),
  
  sidebarLayout(
    sidebarPanel(
      h4("🎛️ Parámetros del Algoritmo"),
      
      sliderInput("alpha", "Tasa de Aprendizaje (Alpha):", 
                  min = 0.01, max = 0.8, value = 0.1, step = 0.02),
      helpText("Nota cómo el cambio de Alpha afecta la suavidad del descenso."),
      
      hr(),
      
      h4("▶️ Animación"),
      sliderInput("iter_view", "Número de Iteración:", 
                  min = 0, max = 50, value = 0, step = 1,
                  animate = animationOptions(interval = 300)),
      
      hr(),
      h4("📍 Referencias Visuales"),
      tags$ul(
        tags$li(style="color: #081D58; font-weight: bold;", "Negro/Azul Oscuro: Zona de Error Mínimo"),
        tags$li(style="color: #D73027; font-weight: bold;", "Rojo: Zona de Error Alto"),
        tags$li(style="color: #00AA00; font-weight: bold;", "● Punto Verde: Mínimo Global Exacto")
      ),
      
      verbatimTextOutput("dist_msg")
    ),
    
    mainPanel(
      plotOutput("gd_plot", height = "650px")
    )
  )
)

# ==============================================================================
# 3. SERVIDOR
# ==============================================================================
server <- function(input, output, session) {
  
  gd_simulation <- reactive({
    alpha <- input$alpha
    n_iter <- 50
    beta <- c(0, 0, 0) # Arrancamos lejos del óptimo
    
    # Arrays para guardar historia
    hist_b1 <- numeric(n_iter + 1)
    hist_b2 <- numeric(n_iter + 1)
    hist_cost <- numeric(n_iter + 1)
    
    # Estado inicial
    hist_b1[1] <- 0; hist_b2[1] <- 0
    hist_cost[1] <- cost_func(0, 0)
    
    for(i in 1:n_iter) {
      error <- (X_mat %*% beta) - Y
      gradient <- (2/m) * (t(X_mat) %*% error)
      beta <- beta - alpha * gradient
      
      hist_b1[i+1] <- beta[2]
      hist_b2[i+1] <- beta[3]
      hist_cost[i+1] <- cost_func(beta[2], beta[3])
    }
    
    data.frame(iter = 0:n_iter, b1 = hist_b1, b2 = hist_b2, cost = hist_cost)
  })
  
  output$gd_plot <- renderPlot({
    history <- gd_simulation()
    idx <- input$iter_view + 1
    
    current_b1 <- history$b1[idx]
    current_b2 <- history$b2[idx]
    current_cost <- history$cost[idx]
    
    # --- GRÁFICO 3D ---
    par(mar = c(1, 1, 1, 1))
    
    # 1. Dibujar Superficie (Colores de alto contraste)
    P <- persp(b1_grid, b2_grid, Z_surface,
               theta = 40, phi = 30, # Ángulo cenital
               col = final_facet_colors,
               border = NA, # Sin bordes para suavidad
               shade = 0.3,
               xlab = "Beta 1 (Pies²)", ylab = "Beta 2 (Baños)", zlab = "Error (MSE)",
               main = paste("Iteración:", input$iter_view),
               cex.main = 1.5)
    
    # 2. Dibujar el MÍNIMO GLOBAL EXACTO (El objetivo) - PUNTO VERDE
    # Lo elevamos un poco (+0.01) para que no quede enterrado en la superficie
    target_pt <- trans3d(min_b1, min_b2, min_cost_val, P)
    points(target_pt, pch = 16, col = "#00FF00", cex = 2) # Punto Relleno Verde Neón
    points(target_pt, pch = 1, col = "black", cex = 2)    # Borde negro para resaltar
    text(target_pt, labels = "Mínimo Global", pos = 3, col = "black", font=2, cex=0.9)
    
    # 3. Dibujar Trayectoria hasta el momento
    path_lines <- trans3d(history$b1[1:idx], history$b2[1:idx], history$cost[1:idx] + 0.02, P)
    lines(path_lines, col = "white", lwd = 3, lty = 1) # Línea blanca resalta sobre fondo oscuro
    
    # 4. Dibujar Posición Actual (Bola Amarilla/Roja)
    current_pt <- trans3d(current_b1, current_b2, current_cost + 0.03, P)
    points(current_pt, pch = 21, bg = "yellow", col = "black", cex = 2.5, lwd = 2)
    
  })
  
  output$dist_msg <- renderText({
    history <- gd_simulation()
    idx <- input$iter_view + 1
    
    # Calcular distancia euclidiana al óptimo
    curr_b1 <- history$b1[idx]
    curr_b2 <- history$b2[idx]
    dist <- sqrt((curr_b1 - min_b1)^2 + (curr_b2 - min_b2)^2)
    
    paste0("Distancia al óptimo: ", round(dist, 4), 
           "\nCosto Actual: ", round(history$cost[idx], 4),
           "\nCosto Mínimo Posible: ", round(min_cost_val, 4))
  })
}

shinyApp(ui, server)