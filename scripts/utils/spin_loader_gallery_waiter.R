# app.R ─────────────────────────────────────────────────────────
library(shiny)
library(waiter)

# ── Lista de spinners a mostrar ───────────────────────────────
spins <- c(
  "spin_rotating_plane", "spin_fading_circles", "spin_folding_cube",
  "spin_double_bounce", "spin_wave", "spin_wandering_cubes",
  "spin_pulse", "spin_chasing_dots", "spin_three_bounce",
  "spin_circle", "spin_rotate", "spin_solar",
  "spin_orbit", "spin_squares", "spin_cube_grid",
  "spin_circles", "spin_orbiter", "spin_pixel",
  "spin_flower", "spin_dual_ring", "spin_heart",
  "spin_ellipsis", "spin_facebook", "spin_hourglass",
  "spin_ring", "spin_ripple", "spin_terminal",
  "spin_loader", "spin_throbber", "spin_refresh",
  "spin_heartbeat", "spin_gauge", "spin_3k",
  "spin_wobblebar", "spin_atebits", "spin_whirly",
  "spin_flowers", "spin_dots", "spin_3circles",
  "spin_plus", "spin_pulsar", "spin_hexdots",
  "spin_inner_circles", "spin_pong", "spin_timer",
  "spin_ball", "spin_dual_circle", "spin_seven_circle",
  "spin_clock", "spin_pushing_shapes", "spin_fill",
  "spin_rhombus", "spin_balance", "spin_square_circle",
  "spin_circle_square", "spin_puzzle", "spin_half",
  "spin_loaders", "spin_1", "spin_2", "spin_3",
  "spin_4", "spin_5", "spin_6", "spin_google"
)

# ── Helper: pasa solo los argumentos que el spinner reconoce ──
make_spinner <- function(fname, col = "#5f62a1") {
  fn   <- get(fname)
  args <- list()
  if ("id"    %in% names(formals(fn))) args$id    <- 1   # p/ spin_loaders
  if ("color" %in% names(formals(fn))) args$color <- col # si lo admite
  do.call(fn, args)
}

# ── UI ─────────────────────────────────────────────────────────
ui <- fluidPage(
  use_waiter(),   # carga los recursos SpinKit
  
  # Cargamos la hoja de estilos de CSS-Loaders (para los que faltan)
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/loaders.css@0.1.2/loaders.min.css"
    ),
    # Estilos: fondo negro, tipografía clara, tarjetas de spinners
    tags$style(HTML("
      body            { background:#000; color:#fff; }
      .spinner-card   {
        display:inline-block; width:180px; margin:25px 15px;
        text-align:center; vertical-align:top;
        background:#1b1b1b; padding:22px 8px 18px; border-radius:8px;
        color:#e3b009;                /* color que heredan CSS-Loaders */
      }
      .spinner-card p { margin-top:14px; font-size:.8em; color:#d8d8d8; }
    "))
  ),
  
  titlePanel(tags$span(style = "color:#e3b009;",
                       "Galería de spinners — {waiter}")),
  
  # Galería
  div(
    lapply(spins, function(sp) {
      div(class = "spinner-card",
          make_spinner(sp),      # genera cada animación
          tags$p(sp))
    })
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
