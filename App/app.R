library(shiny)
library(readr)
library(dplyr)
library(plotly)

# ==============================================================================
# 1. CARGA DE DATOS
# ==============================================================================
if(file.exists("kc_house_data.csv")) {
  tryCatch({
    raw_data <- read_csv("kc_house_data.csv", show_col_types = FALSE)
  }, error = function(e) { raw_data <- read.csv("kc_house_data.csv") })
} else { stop("Falta 'kc_house_data.csv'") }

# --- PREPARACIÓN: Seleccionamos solo columnas numéricas ---
numeric_cols <- raw_data %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("id", "price", "zipcode", "date"))) %>%
  names()

# ==============================================================================
# 2. UI (INTERFAZ)
# ==============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f4f6f9; }
    .nav-tabs { margin-bottom: 15px; }
    .plot-container { background: white; border: 1px solid #ddd; border-radius: 5px; padding: 5px; margin-bottom: 5px; }
    .init-controls { background-color: #e3f2fd; padding: 10px; border-radius: 5px; border: 1px solid #90caf9; margin-bottom: 10px; }
    .math-box { 
      background-color: #fff; border-left: 5px solid #2c3e50; 
      padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      font-size: 1.1em;
    }
    h4 { color: #2c3e50; font-weight: bold; font-size: 1.1em; }
  "))),
  
  withMathJax(),
  
  titlePanel("Laboratorio Simulador: El Descenso del Gradiente"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1️⃣ Datos"),
      selectizeInput("selected_vars", "Variables Predictoras (2):", 
                     choices = numeric_cols, selected = c("sqft_living", "lat"), 
                     multiple = TRUE, options = list(maxItems = 2)),
      
      h4("2️⃣ Punto de Partida"),
      div(class = "init-controls",
          div(style="display:flex; gap:5px; margin-bottom:10px;",
              actionButton("btn_random", "🎲 Aleatorio", class="btn-primary btn-sm", style="flex:1;"),
              actionButton("btn_zero", "0️⃣ Origen", class="btn-warning btn-sm", style="flex:1;")
          ),
          fluidRow(
            column(6, numericInput("init_b1", "Beta 1:", value = -2, step=0.1)),
            column(6, numericInput("init_b2", "Beta 2:", value = 2, step=0.1))
          )
      ),
      
      h4("3️⃣ Hiperparámetros"),
      sliderInput("batch_size", "Tamaño del Batch:", min = 10, max = 21613, value = 32, step = 10),
      sliderInput("alpha", "Tasa de Aprendizaje (λ):", min = 0.001, max = 1, value = 0.05, step = 0.001),
      helpText("⚠️ Un alpha muy alto puede hacer que el algoritmo 'salte' el mínimo."),
      
      hr(),
      h4("4️⃣ Control de Tiempo"),
      sliderInput("iter_view", "Iteración (Paso):", min = 0, max = 150, value = 0, step = 1,
                  animate = animationOptions(interval = 200, loop = FALSE))
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        # Pestaña 1: Visualización Principal
        tabPanel("🧠 La Montaña de Costo (Batch GD)",
                 br(),
                 h4("¿Cómo actualiza el algoritmo sus 'pasos'?", align="center"),
                 uiOutput("dynamic_math_box"),
                 fluidRow(
                   column(6, plotlyOutput("plot_cost_3d", height = "500px")),
                   column(6, plotlyOutput("plot_cost_2d", height = "500px"))
                 )
        ),
        
        # Pestaña 2: Ajuste
        tabPanel("🏠 Ajuste del Modelo",
                 br(),
                 h4("El plano intentando ajustarse a los datos", align="center"),
                 plotlyOutput("plot_data_fit", height = "600px")
        ),
        
        # Pestaña 3: Carrera
        tabPanel("🏁 Carrera de Algoritmos",
                 br(),
                 h4("Comparativa: Estabilidad vs Velocidad", align="center"),
                 fluidRow(
                   column(4, 
                          div(class="plot-container",
                              h4("Batch GD", align="center", style="color:blue"),
                              plotlyOutput("race_batch", height = "350px"),
                              verbatimTextOutput("txt_batch")
                          )
                   ),
                   column(4, 
                          div(class="plot-container",
                              h4("Mini-Batch", align="center", style="color:orange"),
                              plotlyOutput("race_mini", height = "350px"),
                              verbatimTextOutput("txt_mini")
                          )
                   ),
                   column(4, 
                          div(class="plot-container",
                              h4("SGD (Estocástico)", align="center", style="color:red"),
                              plotlyOutput("race_sgd", height = "350px"),
                              verbatimTextOutput("txt_sgd")
                          )
                   )
                 )
        )
      ),
      verbatimTextOutput("metrics")
    )
  )
)

# ==============================================================================
# 3. SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # --- LÓGICA DE BOTONES ---
  observeEvent(input$btn_random, {
    # FIX: Liberamos la semilla fija para garantizar aleatoriedad en el botón
    set.seed(NULL) 
    val1 <- round(runif(1, -2.5, 2.5), 2)
    val2 <- round(runif(1, -2.5, 2.5), 2)
    updateNumericInput(session, "init_b1", value = val1)
    updateNumericInput(session, "init_b2", value = val2)
  })
  observeEvent(input$btn_zero, {
    updateNumericInput(session, "init_b1", value = 0)
    updateNumericInput(session, "init_b2", value = 0)
  })
  
  # --- PREPARACIÓN DE DATOS ---
  dataset <- reactive({
    req(length(input$selected_vars) == 2)
    vars <- input$selected_vars
    
    df <- raw_data %>% select(price, all_of(vars)) %>% na.omit()
    # Estandarizamos para que el GD converja visualmente bien en [-3,3]
    X_mat <- cbind(1, as.vector(scale(df[[vars[1]]])), as.vector(scale(df[[vars[2]]])))
    Y <- as.vector(scale(df$price))
    
    set.seed(123)
    idx_sample <- sample(nrow(df), 300) 
    X_sample <- X_mat[idx_sample, ]
    Y_sample <- Y[idx_sample]
    
    # Cálculo Analítico del Óptimo (La "Meta")
    beta_target <- tryCatch(solve(t(X_mat)%*%X_mat) %*% t(X_mat)%*%Y, error=function(e) c(0,0,0))
    
    list(X=X_mat, Y=Y, X_sample=X_sample, Y_sample=Y_sample, 
         beta_target=beta_target, vars=vars, m=length(Y))
  })
  
  # --- SIMULACIÓN DEL GRADIENTE ---
  simulations <- reactive({
    req(dataset(), input$alpha, input$batch_size, input$init_b1, input$init_b2)
    d <- dataset(); alpha <- input$alpha; mb <- input$batch_size
    n_iter <- 150
    start_beta <- c(0, input$init_b1, input$init_b2)
    
    # Inicializar historiales
    h_b <- data.frame(iter=0:n_iter, b1=NA, b2=NA, cost=NA, b0=NA)
    h_m <- h_b; h_s <- h_b
    
    calc_cost <- function(X, Y, b) mean(((X %*% b) - Y)^2)
    
    h_b[1,] <- c(0, start_beta[2], start_beta[3], calc_cost(d$X, d$Y, start_beta), start_beta[1])
    h_m[1,] <- h_b[1,]; h_s[1,] <- h_b[1,]
    
    bb <- start_beta; bm <- start_beta; bs <- start_beta
    set.seed(123)
    
    for(i in 1:n_iter) {
      # 1. Batch GD
      gb <- (2/d$m) * (t(d$X) %*% ((d$X %*% bb) - d$Y))
      bb <- bb - alpha * gb
      
      # 2. Mini-Batch GD
      idx_m <- sample(d$m, mb)
      Xm <- d$X[idx_m,,drop=F]; Ym <- d$Y[idx_m]
      gm <- (2/length(Ym)) * (t(Xm) %*% ((Xm %*% bm) - Ym))
      bm <- bm - alpha * gm
      
      # 3. Stochastic GD (SGD)
      idx_s <- sample(d$m, 1)
      Xs <- d$X[idx_s,,drop=F]; Ys <- d$Y[idx_s]
      gs <- (2/1) * (t(Xs) %*% ((Xs %*% bs) - Ys))
      bs <- bs - alpha * gs
      
      h_b[i+1,] <- c(i, bb[2], bb[3], calc_cost(d$X, d$Y, bb), bb[1])
      h_m[i+1,] <- c(i, bm[2], bm[3], calc_cost(d$X, d$Y, bm), bm[1])
      h_s[i+1,] <- c(i, bs[2], bs[3], calc_cost(d$X, d$Y, bs), bs[1])
    }
    list(batch=h_b, mini=h_m, sgd=h_s)
  })
  
  # --- GRID PARA SUPERFICIE ---
  grid_cost <- reactive({
    req(dataset())
    d <- dataset() 
    seq_b <- seq(-3.5, 3.5, length.out=35) 
    calc <- function(b1, b2) {
      beta <- c(d$beta_target[1], b1, b2)
      mean(((d$X %*% beta) - d$Y)^2)
    }
    Z <- outer(seq_b, seq_b, Vectorize(calc))
    list(x=seq_b, y=seq_b, z=Z)
  })
  

  # --- GRÁFICO 3D CORREGIDO ---
  output$plot_cost_3d <- renderPlotly({
    req(simulations())
    g <- grid_cost(); h <- simulations()$batch; d <- dataset(); idx <- input$iter_view + 1
    ui_state <- paste(d$vars, collapse="-")
    z_lift <- 0.05
    
    target_cost <- mean(((d$X %*% d$beta_target) - d$Y)^2)
    
    plot_ly() %>%
      add_trace(x = g$x, y = g$y, z = g$z, type = "surface",
                colorscale = "Viridis", opacity = 0.85, showscale = FALSE,
                contours = list(z = list(show=TRUE, usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE)))) %>%
      add_trace(x = h$b1[1:idx], y = h$b2[1:idx], z = h$cost[1:idx] + z_lift,
                type = "scatter3d", mode = "lines", 
                line = list(color = "yellow", width = 6), name = "Trayectoria") %>%
      add_trace(x = c(h$b1[idx]), y = c(h$b2[idx]), z = c(h$cost[idx] + z_lift),
                type = "scatter3d", mode = "markers", 
                marker = list(color = "red", size = 5, line=list(color="white", width=2)), name = "Posición Actual") %>%
      add_trace(x = c(d$beta_target[2]), y = c(d$beta_target[3]), z = c(target_cost + z_lift),
                type = "scatter3d", mode = "markers", 
                marker = list(color = "#00FF00", size = 8, symbol="diamond", line=list(color="black", width=1)), 
                name = "Mínimo Global") %>%
      layout(uirevision = ui_state, 
             scene = list(
               xaxis = list(title = "Beta 1"),
               yaxis = list(title = "Beta 2"),
               zaxis = list(title = "Costo J"),
               camera = list(eye = list(x=1.6, y=1.6, z=1.3))
             ), 
             showlegend = FALSE)
  })
  
  # --- GRÁFICO 2D (CONTOUR) - Estilo de referencia ---
  output$plot_cost_2d <- renderPlotly({
    req(simulations())
    g <- grid_cost(); h <- simulations()$batch; d <- dataset(); idx <- input$iter_view + 1
    
    plot_ly(type = "contour", x = g$x, y = g$y, z = g$z, 
            colorscale = "Viridis", contours = list(start=min(g$z), end=max(g$z), size=(max(g$z)-min(g$z))/20)) %>%
      add_trace(x = h$b1[1:idx], y = h$b2[1:idx], type = "scatter", mode = "lines+markers",
                line = list(color = "yellow", width=2), marker = list(size=3, color="yellow"), name="Camino") %>%
      add_trace(x = c(h$b1[idx]), y = c(h$b2[idx]), type = "scatter", mode = "markers",
                marker = list(color = "red", size = 10, line=list(color="white", width=2)), name="Actual") %>%
      add_trace(x = c(d$beta_target[2]), y = c(d$beta_target[3]), type = "scatter", mode = "markers",
                marker = list(color = "green", size = 12, symbol="x", line=list(color="white", width=2)), name="Meta") %>%
      layout(showlegend = FALSE, xaxis=list(title="Beta 1"), yaxis=list(title="Beta 2"))
  })
  
  # --- AJUSTE PLANO ---
  output$plot_data_fit <- renderPlotly({
    req(simulations())
    d <- dataset(); h <- simulations()$batch; idx <- input$iter_view + 1
    b0 <- h$b0[idx]; b1 <- h$b1[idx]; b2 <- h$b2[idx]
    
    grid_res <- 10
    x1_seq <- seq(min(d$X_sample[,2]), max(d$X_sample[,2]), length.out=grid_res)
    x2_seq <- seq(min(d$X_sample[,3]), max(d$X_sample[,3]), length.out=grid_res)
    Z_plane <- outer(x1_seq, x2_seq, function(x,y) b0 + b1*x + b2*y)
    
    plot_ly() %>%
      add_trace(x = d$X_sample[,2], y = d$X_sample[,3], z = d$Y_sample, type = "scatter3d", mode = "markers",
                marker = list(size = 3, color = "blue", opacity=0.4), name = "Datos") %>%
      add_trace(x = x1_seq, y = x2_seq, z = Z_plane, type = "surface",
                colorscale = list(c(0, 1), c("red", "red")), opacity = 0.5, 
                showscale = FALSE, name = "Predicción") %>%
      layout(title = paste("Iteración:", input$iter_view), showlegend = FALSE)
  })
  
  # --- GRÁFICOS DE CARRERA (UNIFICADOS) ---
  draw_race <- function(g, d, h, idx, col) {
    # Usamos la misma estética que el plot_cost_2d
    plot_ly(type = "contour", x = g$x, y = g$y, z = g$z, 
            colorscale = "Viridis", showscale = FALSE,
            # Misma densidad de contornos que el principal
            contours = list(start=min(g$z), end=max(g$z), size=(max(g$z)-min(g$z))/20)) %>%
      
      # Meta (Cruz verde idéntica al principal)
      add_trace(x = c(d$beta_target[2]), y = c(d$beta_target[3]), type = "scatter", mode = "markers",
                marker = list(color = "green", size = 12, symbol = "x", line=list(color="white", width=2)), name = "Meta") %>%
      
      # Ruta (Misma línea con markers, pero color variable por algoritmo)
      add_trace(x = h$b1[1:idx], y = h$b2[1:idx], type = "scatter", mode = "lines+markers",
                line = list(color = col, width = 2), marker = list(size = 3, color = col), name = "Ruta") %>%
      
      # Actual (Bola roja con borde blanco, idéntica al principal)
      add_trace(x = c(h$b1[idx]), y = c(h$b2[idx]), type = "scatter", mode = "markers",
                marker = list(color = "red", size = 10, line=list(color="white", width=2)), name = "Actual") %>%
      
      # Layout con EJES visibles igual que el principal
      layout(margin=list(l=35, r=10, t=30, b=35), # Un poco más de margen para que entren los labels
             xaxis=list(title="Beta 1"), 
             yaxis=list(title="Beta 2"))
  }
  
  output$race_batch <- renderPlotly({ draw_race(grid_cost(), dataset(), simulations()$batch, input$iter_view+1, "blue") })
  output$race_mini <- renderPlotly({ draw_race(grid_cost(), dataset(), simulations()$mini, input$iter_view+1, "orange") })
  output$race_sgd <- renderPlotly({ draw_race(grid_cost(), dataset(), simulations()$sgd, input$iter_view+1, "red") })
  
  output$txt_batch <- renderText({ paste("Costo:", round(simulations()$batch$cost[input$iter_view+1], 4)) })
  output$txt_mini <- renderText({ paste("Costo:", round(simulations()$mini$cost[input$iter_view+1], 4)) })
  output$txt_sgd <- renderText({ paste("Costo:", round(simulations()$sgd$cost[input$iter_view+1], 4)) })
  
  output$metrics <- renderPrint({
    h <- simulations()$batch; idx <- input$iter_view + 1
    cat("Estado Batch GD -> Costo:", round(h$cost[idx], 4), "| Beta1:", round(h$b1[idx], 3), "| Beta2:", round(h$b2[idx], 3))
  })
}

shinyApp(ui, server)