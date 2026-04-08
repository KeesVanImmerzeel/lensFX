library(shiny)
library(readxl)
library(dplyr)
library(deSolve)
library(openxlsx)

# This creates an empty but valid tibble → not NULL.
# This avoids 100% of the .html problems.
#stresses_global <- tibble::tibble(
#  Date = as.Date(character()),
#  Hdiep_NAP = numeric(),
#  Hsloot_NAP = numeric(),
#  NN_mm = numeric()
#)

stresses_global <- read_excel("www/default_stresses.xlsx")
observations_global <- read_excel("www/default_observations.xlsx")


# ------------------ Modelkern: getters & afgeleiden ---------------------------

get_GWS <- function(A, B, Mv, PorFc, PorFr)  (A + B - Mv*PorFc) / PorFr
get_GV  <- function(B, PorSat)               B / PorSat
get_Kwel <- function(H1, GWS, c0)            (H1 - GWS) / c0
get_Dr <- function(GWS, HSloot, Wdr)         pmax((GWS - HSloot) / Wdr, 0)
get_DrB <- function(GV, GWS, Dr)             ifelse(GV < GWS, (GV/GWS)*Dr, Dr)
get_DrA <- function(GWS, GV, Dr)             pmax((GWS - GV) * Dr / GWS, 0)
get_OA <- function(GWS, Gr, Cklein)          ifelse(GWS < Gr, 0, (GWS - Gr)/Cklein)
# Gr = greppelniveau (m+c0)

# dynamische externe parameters per tijdstap met offsets en wrapping
step_external <- function(ext_df, step_index, dH1=0, dHSloot=0, dNN=0) {
  n <- nrow(ext_df)
  i <- ((step_index - 1) %% n) + 1
  tibble(
    H1 = ext_df$H1[i] + dH1,
    HSloot = ext_df$HSloot[i] + dHSloot,
    NN = ext_df$NN[i] + dNN
  )
}

# Afgeleiden (SpeedProc) voor constante externe parameters binnen de tijdstap
lens_derivs <- function(t, y, pars) {
  # y: c(A, B, NNcum, OAcum, DrAcum, Kwelcum, DrBcum)
  A <- y[1]; B <- y[2]
  NNcum <- y[3]; OAcum <- y[4]; DrAcum <- y[5]; Kwelcum <- y[6]; DrBcum <- y[7]
  
  # Externe parameters (constant binnen deze tijdstap)
  H1     <- pars$H1
  HSloot <- pars$HSloot
  NN     <- pars$NN
  
  # Parameters
  PorSat <- pars$PorSat; PorFr <- pars$PorFr; PorFc <- pars$PorFc
  Mv <- pars$Mv; c0 <- pars$c0; Wdr <- pars$Wdr; Cklein <- pars$Cklein; Gr <- pars$Gr
  
  # Afgeleide grootheden
  GWS  <- get_GWS(A,B,Mv,PorFc,PorFr)
  GV   <- get_GV(B, PorSat)
  Kwel <- get_Kwel(H1, GWS, c0)
  Dr   <- get_Dr(GWS, HSloot, Wdr)
  DrB  <- get_DrB(GV, GWS, Dr)
  DrA  <- get_DrA(GWS, GV, Dr)
  OA   <- get_OA(GWS, Gr, Cklein)
  
  delt <- NN - OA - DrA
  dA <- delt
  if ((delt < 0) && (A <= 0)) {
    dA <- 0.0      # geen neg. vocht in lens
    # 'delt' blijft negatief en wordt toegewezen aan dB (correctie)
  } else {
    delt <- 0      # anders wordt delt niet doorgegeven naar B
  }
  dB <- Kwel - DrB + delt
  
  dNNcum   <- NN
  dOAcum   <- OA
  dDrAcum  <- DrA
  dKwelcum <- Kwel
  dDrBcum  <- DrB
  
  list(c(dA, dB, dNNcum, dOAcum, dDrAcum, dKwelcum, dDrBcum),
       c(GWS=GWS, GV=GV, Dr=Dr, OA=OA, Kwel=Kwel, H1=H1, HSloot=HSloot))
}

# Correctiestap (na elke hoofd-tijdstap), identiek aan Delphi Correct()
correct_state <- function(A, B, state_aux, pars) {
  # state_aux bevat: BA_cum, AB_cum, ErrCorr
  BA_cum <- state_aux$BA_cum
  AB_cum <- state_aux$AB_cum
  ErrCorr <- state_aux$ErrCorr
  
  PorSat <- pars$PorSat; PorFr <- pars$PorFr; PorFc <- pars$PorFc
  Mv <- pars$Mv
  
  GWS <- get_GWS(A,B,Mv,PorFc,PorFr)
  
  # 1) A >= Amin = PorFc * (Mv - GWS)
  Amin <- PorFc * (Mv - GWS)
  Corr <- A - Amin
  if (Corr < 0) {
    # over hevelen van B naar A
    B  <- B + Corr
    A  <- Amin
    BA <- -Corr
    BA_cum <- BA_cum + BA
    if (BA_cum > Amin) BA_cum <- Amin
  } else if (Corr > 0) {
    if (Corr >= BA_cum) {
      B <- B + BA_cum
      A <- A - BA_cum
      BA_cum <- 0
    } else {
      B <- B + Corr
      A <- A - Corr
      BA_cum <- BA_cum - Corr
    }
  }
  
  # 2) B >= 0 (grensvlak niet onder top c0)
  Corr <- B
  if (Corr < 0) {
    A   <- A + Corr
    B   <- 0
    AB  <- -Corr
    AB_cum <- AB_cum + AB
  } else if (Corr > 0) {
    B <- B - AB_cum
    ErrCorr <- ErrCorr + AB_cum
    if (B < 0) {
      AB_cum  <- -B
      B       <- 0
      ErrCorr <- ErrCorr - AB_cum
    } else {
      AB_cum <- 0
    }
  }
  
  # Foutconditie zoals Delphi
  GWS_new <- get_GWS(A,B,Mv,PorFc,PorFr)
  if (GWS_new < 0) {
    stop("Grondwaterstand onder basis watervoerend pakket. Model niet geschikt voor dit probleem (Delphi-gedrag).")
  }
  
  list(A=A, B=B, BA_cum=BA_cum, AB_cum=AB_cum, ErrCorr=ErrCorr)
}

# Eén simulatie uitvoeren (baseline of scenario)
simulate_lens <- function(inp, dH1=0, dHSloot=0, dNN=0,
                          PorFr_override = NULL, c0_override=NULL,
                          Wdr_override=NULL, Cklein_override=NULL, updateProgress = NULL) {
  # Parameters
  PorSat <- inp$PorSat
  PorFr  <- if (!is.null(PorFr_override)) PorFr_override else inp$PorFr
  c0     <- if (!is.null(c0_override))    c0_override    else inp$c0
  Wdr    <- if (!is.null(Wdr_override))   Wdr_override   else inp$Wdr
  Cklein <- if (!is.null(Cklein_override)) Cklein_override else inp$Cklein
  PorFc  <- PorSat - PorFr
  Mv     <- inp$Mv
  Gr     <- inp$Gr
  
  # Initialen (zoals Delphi ResetIAandIB / Init)
  IB <- inp$I_GV  * PorSat
  IA <- inp$I_GWS * PorFr - IB + Mv * PorFc
  
  # y: [A, B, NNcum, OAcum, DrAcum, Kwelcum, DrBcum]
  y <- c(A=IA, B=IB, NN_cum=0, OA_cum=0, DrA_cum=0, Kwel_cum=0, DrB_cum=0)
  
  # correctie-buffers (objectstatus niet in y)
  BA_cum <- 0; AB_cum <- 0; ErrCorr <- 0
  
  # tijdstappen
  dt  <- inp$TimeStepSize
  n   <- floor(inp$TFinish / dt)
  if (n <= 0) stop("Geen tijdstappen (TFinish / TimeStepSize <= 0).")
  
  # resultaten (zoals WriteStatus)
  res <- tibble(
    Tijd = numeric(0),
    A = numeric(0), B = numeric(0), GWS = numeric(0), GV = numeric(0),
    NN = numeric(0), OA = numeric(0), DrA = numeric(0), Kwel = numeric(0),
    DrB = numeric(0), Dr = numeric(0), H1 = numeric(0), HSloo = numeric(0),
    NN_cum = numeric(0), OA_cum = numeric(0), DrA_cum = numeric(0),
    Kwel_cum = numeric(0), DrB_cum = numeric(0), Dr_cum = numeric(0),
    ErrTot = numeric(0)
  )
  
  
  # Use the first stresses row at t = 0
  ext0 <- step_external(inp$external, 1, dH1, dHSloot, dNN)
  
  H1     <- ext0$H1
  HSloot <- ext0$HSloot
  NN     <- ext0$NN
  
  GWS  <- get_GWS(y["A"], y["B"], Mv, PorFc, PorFr)
  GV   <- get_GV(y["B"], PorSat)
  Kwel <- get_Kwel(H1, GWS, c0)
  Dr   <- get_Dr(GWS, HSloot, Wdr)
  DrB  <- get_DrB(GV, GWS, Dr)
  DrA  <- get_DrA(GWS, GV, Dr)
  OA   <- get_OA(GWS, Gr, Cklein)
  
  res <- add_row(res,
                 Tijd = 0,
                 A = y["A"], B = y["B"], GWS = GWS, GV = GV,
                 NN = NN, OA = OA, DrA = DrA, Kwel = Kwel,
                 DrB = DrB, Dr = Dr, H1 = H1, HSloo = HSloot,
                 NN_cum = 0, OA_cum = 0, DrA_cum = 0,
                 Kwel_cum = 0, DrB_cum = 0, Dr_cum = 0,
                 ErrTot = 0
  )
  
  # Loop over hoofd-tijdstappen
  for (i in 1:n) {
    # Externe parameters voor deze stap
    ext <- step_external(inp$external, i, dH1, dHSloot, dNN)
    H1 <- ext$H1; HSloot <- ext$HSloot; NN <- ext$NN
    
    # parameters bundelen
    pars <- list(PorSat=PorSat, PorFr=PorFr, PorFc=PorFc, Mv=Mv, Gr=Gr,
                 c0=c0, Wdr=Wdr, Cklein=Cklein, H1=H1, HSloot=HSloot, NN=NN)
    
    # Integreer binnen [t, t+dt] met substeps (rk4)
    times <- seq(0, dt, length.out = max(2, inp$I_nSubStep + 1))
    out <- deSolve::ode(y=y, times=times, func=lens_derivs, parms=pars, method="rk4")
    y_end <- as.numeric(out[nrow(out), 2:8]) # A,B,NNcum,OAcum,DrAcum,Kwelcum,DrBcum
    names(y_end) <- names(y)
    
    # Correctiestap
    corr <- correct_state(y_end["A"], y_end["B"],
                          list(BA_cum=BA_cum, AB_cum=AB_cum, ErrCorr=ErrCorr), pars = c(pars, list()))
    y_end["A"] <- corr$A
    y_end["B"] <- corr$B
    BA_cum <- corr$BA_cum
    AB_cum <- corr$AB_cum
    ErrCorr <- corr$ErrCorr
    
    # Ken hulpgrootheden toe op eind van stap (zoals WriteStatus bij TEnd)
    GWS  <- get_GWS(y_end["A"], y_end["B"], Mv, PorFc, PorFr)
    GV   <- get_GV(y_end["B"], PorSat)
    Kwel <- get_Kwel(H1, GWS, c0)
    Dr   <- get_Dr(GWS, HSloot, Wdr)
    DrB  <- get_DrB(GV, GWS, Dr)
    DrA  <- get_DrA(GWS, GV, Dr)
    OA   <- get_OA(GWS, Gr, Cklein)
    Dr_cum <- y_end["DrA_cum"] + y_end["DrB_cum"]
    
    ErrA <- y_end["A"] - (IA + y_end["NN_cum"] - y_end["OA_cum"] - y_end["DrA_cum"])
    ErrB <- y_end["B"] - (IB + y_end["Kwel_cum"] - y_end["DrB_cum"])
    ErrTot <- ErrA + ErrB + ErrCorr
    
    res <- add_row(res,
                   Tijd = i*dt,
                   A = y_end["A"], B = y_end["B"], GWS = GWS, GV = GV,
                   NN = NN, OA = OA, DrA = DrA, Kwel = Kwel,
                   DrB = DrB, Dr = Dr, H1 = H1, HSloo = HSloot,
                   NN_cum = y_end["NN_cum"], OA_cum = y_end["OA_cum"], DrA_cum = y_end["DrA_cum"],
                   Kwel_cum = y_end["Kwel_cum"], DrB_cum = y_end["DrB_cum"], Dr_cum = Dr_cum,
                   ErrTot = ErrTot
    )
    
    y <- y_end
    
    # progress update
    if (!is.null(updateProgress)) {
      updateProgress(i / n, detail = paste("Step", i, "of", n))
    }
    
  }
  
  res
}

# ------------------ Server ----------------------------------------------------


server <- function(input, output, session) {
      
  # Inside the server: create a reactive that returns the global tibble
  # stresses_reactive() is never NULL
  # It starts empty
  # It updates safely when user uploads data
  # It can be read inside downloadHandler() without failure
  stresses_reactive <- reactiveVal(stresses_global)
  
  observations_reactive <- reactiveVal(observations_global) 
  # Resultaten
  res1 <- reactiveVal(NULL)    
  stats_reactive <- reactiveVal(NULL)

  
  # Update global tibble 'stresses_global' when the user uploads Excel
  observeEvent(input$stresses_file, {
    req(input$stresses_file)
    
    df <- readxl::read_excel(
      input$stresses_file$datapath,
      col_types = c("date", "numeric", "numeric", "numeric")
    ) |>
      dplyr::select(Date, Hdiep_NAP, Hsloot_NAP, NN_mm)
    
    df$Date <- as.Date(df$Date) # ensure Date class 
    
    # Update reactive value
    stresses_reactive(df)
    
    # Also update global tibble
    stresses_global <<- df
  })
  
  # Update global tibble 'observations_global' when the user uploads Excel  
  observeEvent(input$observations_file, {
    req(input$observations_file)
    
    df <- readxl::read_excel(
      input$observations_file$datapath,
      col_types = c("date", "numeric", "text")  # Date, Value, Type
    ) |>
      dplyr::select(Date, Value, Type)
    
    df$Date <- as.Date(df$Date)  # ensure Date class
    
    # Update reactive value
    observations_reactive(df)
    
    # Update global tibble
    observations_global <<- df
  })
  
  stats_reactive <- reactive({
    
    req(res1())
    req(input$stats_from, input$stats_to)
    
    df <- res1()
    
    # Convert model time to real Date
    start_date <- stresses_reactive()$Date[1]
    
    df <- df %>%
      dplyr::mutate(
        Date    = as.Date(start_date) + as.integer(Tijd),
        GWS_NAP = GWS + input$Topc0_NAP,
        GV_NAP  = GV  + input$Topc0_NAP,
        Kwel_mm = Kwel * 1000
      )
    
    
    # --- add NN from stresses ---
    
    stresses_df <- stresses_reactive() %>%
      mutate(Tijd = row_number() - 1) %>%   # aligns with Tijd = 0,1,2,...
      select(Tijd, NN_mm)
    
    df <- df %>%
      left_join(stresses_df, by = "Tijd")
    
    Mv_NAP = input$Mv_NAP
    df <- df %>%
      dplyr::mutate(
        GWS_MV = Mv_NAP - GWS_NAP,
        GV_MV  = Mv_NAP - GV_NAP
      )
    
    # Filter selected period
    df_period <- df %>%
      dplyr::filter(Date >= input$stats_from,
                    Date <= input$stats_to)
    
    validate(
      need(nrow(df_period) > 0,
           "Geen data beschikbaar in de geselecteerde periode.")
    )
    
    # Quantiles to calculate
    q_values <- c(0.01, 0.05, 0.10, 0.115, 0.25, 0.50, 0.75, 0.885, 0.90, 0.95, 0.99)
    
    
    # Build statistics table in long format
    stats_long <- tibble::tibble(
      Statistic = c(
        "Mean", "Min", "Max",
        paste0("Q", q_values * 100, "%")
      ),
      GWS_MV = c(
        mean(df_period$GWS_MV),
        min(df_period$GWS_MV),
        max(df_period$GWS_MV),
        quantile(df_period$GWS_MV, probs = q_values)
      ),
      
      GV_MV = c(
        mean(df_period$GV_MV),
        min(df_period$GV_MV),
        max(df_period$GV_MV),
        quantile(df_period$GV_MV, probs = q_values)
      ),
      
      Kwel_mm = c(
        mean(df_period$Kwel_mm),
        min(df_period$Kwel_mm),
        max(df_period$Kwel_mm),
        quantile(df_period$Kwel_mm, probs = q_values)
      ),
      NN_mm = c(
        mean(df_period$NN_mm),
        min(df_period$NN_mm),
        max(df_period$NN_mm),
        quantile(df_period$NN_mm, probs = q_values)
      )
    )
    
    stats_long
  })
  
  # Maken invoer (inp() en run)
  inp <- reactiveVal(NULL)
  observeEvent(input$run, {
    ext_df <- stresses_reactive()
    
    
    # --- CHECK FOR ERROR CONDITION ---
    if (nrow(ext_df) <= 0) {
      showModal(modalDialog(
        title = "No input data",
        "The stresses table is empty. Please upload a valid Excel with stresses table first.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()   # ⛔ Stop the observeEvent immediately
    }
    
    ext_df$H1 = ext_df$Hdiep_NAP - input$Topc0_NAP
    ext_df$HSloot = ext_df$Hsloot_NAP - input$Topc0_NAP
    ext_df$NN = ext_df$NN_mm / 1000
    Mv = input$Mv_NAP - input$Topc0_NAP
    Gr = input$Gr_NAP - input$Topc0_NAP
    inp(list(
      PorSat = input$PorSat,
      PorFr = input$PorFr,
      Mv = Mv,
      Gr = Gr, #Gr tov topc0_NAP
      c0=input$c0,
      Wdr=input$Wdr,
      Cklein=input$Cklein,
      I_GV=input$I_GV_NAP - input$Topc0_NAP,
      I_GWS=input$I_GWS_NAP - input$Topc0_NAP,
      I_nSubStep=input$I_nSubStep,
      TimeStepSize=1,
      TFinish = nrow(ext_df),
      external = ext_df
    ))
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running simulation...", value = 0)
    
    
    r <- simulate_lens(
      inp(),
      updateProgress = function(value, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
    )
    res1(r)
    showNotification("Results and plots are updated", type = "message")
  })
  
  output$view_stresses_table <- renderTable({
    df <- stresses_reactive()
    
    # Create a display version
    df_display <- df
    df_display$Date <- format(df$Date, "%d-%m-%Y")
    
    df_display
  })
  
  output$view_observations_table <- renderTable({
    df <- observations_reactive()
    
    # Create a display version
    df_display <- df
    df_display$Date <- format(df$Date, "%d-%m-%Y")
    
    df_display
  })
  
  output$view_results_table <- renderTable({
    df <- res1()
    
    # Create a display version
    df_display <- df
    #df_display$Date <- format(df$Date, "%d-%m-%Y")
    
    df_display
  })
  
  # ---------------------------
  # Plot levels
  # ---------------------------

  output$plot_GWS_GV <- plotly::renderPlotly({
    
    req(res1())
    df <- res1()
    stresses <- stresses_reactive()
    
    # =========================================================
    # DATA PREP
    # =========================================================
    start_date <- stresses$Date[1]
    
    df_plot <- df |>
      dplyr::mutate(
        Date    = as.Date(start_date) + as.integer(Tijd),
        GWS_NAP = GWS + input$Topc0_NAP,
        GV_NAP  = GV  + input$Topc0_NAP
      ) |>
      dplyr::left_join(
        stresses |>
          dplyr::select(Date, Hdiep_NAP, Hsloot_NAP),
        by = "Date"
      )
    
    # Hydraulic levels
    Mv_NAP     <- input$Mv_NAP
    Gr_NAP     <- input$Gr_NAP
    Topc0_NAP  <- input$Topc0_NAP
    EW_NAP     <- Mv_NAP - input$EW_diepte
    z_krit_NAP <- EW_NAP - input$z_krit
    
    # =========================================================
    # SPLIT GV SERIES
    # =========================================================
    df_plot <- df_plot |>
      dplyr::mutate(
        GV_red = ifelse(
          GV_NAP > EW_NAP |
            (GV_NAP > z_krit_NAP &
               abs(GWS_NAP - GV_NAP) < 0.01 &
               NN < 0),
          GV_NAP, NA_real_
        ),
        GV_orange = ifelse(
          GV_NAP <= EW_NAP &
            GV_NAP > z_krit_NAP &
            abs(GWS_NAP - GV_NAP) < 0.01 &
            !(NN < 0),
          GV_NAP, NA_real_
        ),
        GV_far = ifelse(
          GV_NAP <= EW_NAP &
            !(GV_NAP > z_krit_NAP &
                abs(GWS_NAP - GV_NAP) < 0.01),
          GV_NAP, NA_real_
        )
      )
    
    # =========================================================
    # OBSERVATIONS
    # =========================================================
    obs <- observations_reactive()
    
    if (!is.null(obs) && nrow(obs) > 0) {
      obs_plot <- obs |>
        dplyr::mutate(
          Date = as.Date(Date),
          Type = trimws(Type),
          Value_NAP = Value
        )
    } else {
      obs_plot <- NULL
    }
    
    x_min <- min(df_plot$Date, na.rm = TRUE)
    x_max <- max(df_plot$Date, na.rm = TRUE)
    
    # =========================================================
    # BASE PLOT
    # =========================================================
    p <- ggplot(df_plot, aes(x = Date)) +
      # GWS
      geom_line(
        aes(y = GWS_NAP, colour = "GWS_NAP"),
        linewidth = 0.9
      ) +
      
      # Hdiep
      geom_line(
        aes(y = Hdiep_NAP, colour = "Hdiep_NAP"),
        linewidth = 0.9
      ) +
      
      # Hsloot (NEW)
      geom_line(
        aes(y = Hsloot_NAP, colour = "Hsloot_NAP"),
        linewidth = 0.9
      ) +
      
      # Reference levels
      geom_hline(aes(yintercept = Mv_NAP, colour = "Mv_NAP"),
                 linewidth = 0.9, linetype = "dashed") +
      geom_hline(aes(yintercept = Gr_NAP, colour = "Gr_NAP"),
                 linewidth = 0.9, linetype = "dashed") +
      geom_hline(aes(yintercept = Topc0_NAP, colour = "Topc0_NAP"),
                 linewidth = 0.9, linetype = "dashed")
    
    # =========================================================
    # OBSERVATION POINTS
    # =========================================================
    if (!is.null(obs_plot)) {
      
      if ("GWS_NAP" %in% obs_plot$Type) {
        p <- p + geom_point(
          data = obs_plot |> dplyr::filter(Type == "GWS_NAP"),
          aes(x = Date, y = Value_NAP, colour = "GWS_obs"),
          size = 2, shape = 21, fill = "white"
        )
      }
      
      if ("GV_NAP" %in% obs_plot$Type) {
        p <- p + geom_point(
          data = obs_plot |> dplyr::filter(Type == "GV_NAP"),
          aes(x = Date, y = Value_NAP, colour = "GV_obs"),
          size = 2, shape = 21, fill = "white"
        )
      }
    }
    
    # =========================================================
    # ZONES
    # =========================================================
    p <- p +
      geom_rect(
        aes(xmin = x_min, xmax = x_max, ymin = EW_NAP, ymax = Mv_NAP),
        fill = "green",
        alpha = 0.10,
        inherit.aes = FALSE
      ) +
      geom_rect(
        aes(xmin = x_min, xmax = x_max, ymin = z_krit_NAP, ymax = EW_NAP),
        fill = "yellow",
        alpha = 0.10,
        inherit.aes = FALSE
      )
    
    # =========================================================
    # GV LINES (ON TOP)
    # =========================================================
    p <- p +
      geom_line(aes(y = GV_far, colour = "GV_far"),
                linewidth = 1.0, na.rm = TRUE) +
      geom_line(aes(y = GV_orange, colour = "GV_orange"),
                linewidth = 1.25, na.rm = TRUE) +
      geom_line(aes(y = GV_red, colour = "GV_red"),
                linewidth = 1.4, na.rm = TRUE)
    
    # =========================================================
    # LEGEND & THEME (Hsloot AFTER Hdiep)
    # =========================================================
    p <- p +
      scale_colour_manual(
        name = "Legend",
        values = c(
          "GWS_NAP"    = "blue",
          "Hdiep_NAP"  = "lightblue",
          "Hsloot_NAP" = "cyan4",
          "GV_red"     = "red",
          "GV_orange"  = "orange",
          "GV_far"     = "black",
          "Mv_NAP"     = "darkgreen",
          "Gr_NAP"     = "grey60",
          "Topc0_NAP"  = "orange4",
          "GWS_obs"    = "blue",
          "GV_obs"     = "black"
        ),
        breaks = c(
          "GWS_NAP",
          "Hdiep_NAP",
          "Hsloot_NAP",
          "GV_red",
          "GV_orange",
          "GV_far",
          "GWS_obs",
          "GV_obs",
          "Mv_NAP",
          "Gr_NAP",
          "Topc0_NAP"
        ),
        labels = c(
          "GWS (model)",
          "Hdiep",
          "Hsloot",
          "GV (crit. & NN<0)",
          "GV (crit.)",
          "GV (normal)",
          "GWS observation",
          "GV observation",
          "Mv",
          "Gr",
          "Top c0"
        )
      ) +
      labs(
        x = "Date",
        y = "Level (m+NAP)"
      ) +
      theme_minimal(base_size = 14)
    
    # =========================================================
    # PLOTLY — UNIFIED HOVER
    # =========================================================
    plotly::ggplotly(
      p,
      dynamicTicks = TRUE
    ) |>
      plotly::layout(
        hovermode = "x unified",
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        margin = list(l = 70, r = 150, t = 40, b = 80)
      ) |>
      plotly::config(displaylogo = FALSE)
    
  })
  
  # ---------------------------
  # Plot fluxes
  # ---------------------------
  
  output$plot_fluxes <- plotly::renderPlotly({
    
    req(res1())
    req(input$ts_select)
    
    df <- res1()
    
    # ---- Convert time + convert fluxes to mm/day ----
    df_plot <- df |>
      dplyr::mutate(
        Date = as.Date(stresses_reactive()$Date[1]) + as.integer(Tijd),
        NN   = NN   * 1000,
        OA   = OA   * 1000,
        Kwel = Kwel * 1000,
        Dr   = Dr   * 1000,
        DrB  = DrB  * 1000,
        DrA  = DrA  * 1000
      )
    
    # ---- Pivot to long tidy format ----
    df_long <- df_plot |>
      tidyr::pivot_longer(
        cols = c(NN, OA, Kwel, Dr, DrB, DrA),
        names_to = "Series",
        values_to = "Value"
      ) |>
      dplyr::filter(Series %in% input$ts_select)
    
    # ---- Colors (NN grey, others colored) ----
    color_map <- c(
      "NN"   = "grey60",
      "OA"   = "darkgreen",
      "Kwel" = "orange",
      "Dr"   = "red",
      "DrB"  = "black",
      "DrA"  = "purple"
    )
    
    # ---- Build ggplot with conditional layers ----
    p <- ggplot()
    
    # --- Add NN bars if NN is selected ---
    if ("NN" %in% input$ts_select) {
      p <- p +
        geom_col(
          data = dplyr::filter(df_long, Series == "NN"),
          aes(x = Date, y = Value, fill = Series),
          color = NA,
          show.legend = TRUE
        )
    }
    
    # --- Add lines IF any non‑NN series are selected ---
    non_nn <- setdiff(input$ts_select, "NN")
    if (length(non_nn) > 0) {
      p <- p +
        geom_line(
          data = dplyr::filter(df_long, Series %in% non_nn),
          aes(x = Date, y = Value, color = Series),
          linewidth = 0.6,
          show.legend = TRUE
        )
    }
    
    # ---- Unified legend ----
    p <- p +
      scale_color_manual(
        name = "Legend",
        values = color_map
      ) +
      scale_fill_manual(
        name = "Legend",
        values = color_map
      ) +
      labs(
        x = "Date",
        y = "Flux (mm/d)"
      ) +
      theme_minimal(base_size = 14)
    
    # ---- Convert to plotly ----
    plt <- plotly::ggplotly(p, dynamicTicks = TRUE)
    
    # ---- FIX LEGEND NAMES: "(NN,1)" → "NN" ----
    for (i in seq_along(plt$x$data)) {
      nm <- plt$x$data[[i]]$name
      nm_clean <- sub("\\,.*", "", nm)
      nm_clean <- gsub("[()]", "", nm_clean)
      plt$x$data[[i]]$name <- nm_clean
      plt$x$data[[i]]$legendgroup <- nm_clean
      plt$x$data[[i]]$showlegend <- TRUE
    }
    
    # ---- Apply layout ----
    plt <- plt %>%
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        xaxis = list(
          type = "date",
          tickangle = -45,
          showline = TRUE,
          linewidth = 1,
          linecolor = "black",
          mirror = TRUE
        ),
        yaxis = list(
          showline = TRUE,
          linewidth = 1,
          linecolor = "black",
          mirror = TRUE
        ),
        legend = list(
          title = list(text = "Legend"),
          x = 1.02,
          y = 1,
          xanchor = "left",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.9)",
          bordercolor = "black",
          borderwidth = 1
        ),
        margin = list(l = 70, r = 150, t = 40, b = 80)
      ) %>%
      plotly::config(displaylogo = FALSE)
    
    plt
  })

  
  # ---------------------------
  # DOWNLOAD SETTINGS (System + Initial + Stresses)
  # ---------------------------
  
  output$download_all_input <- downloadHandler(
    filename = function() {
      paste0("LensFX_input_", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(
        list(
          Mv_NAP     = input$Mv_NAP,
          Gr_NAP     = input$Gr_NAP,
          Topc0_NAP  = input$Topc0_NAP,
          EW_diepte  = input$EW_diepte,
          z_krit     = input$z_krit,
          PorSat     = input$PorSat,
          PorFr      = input$PorFr,
          c0         = input$c0,
          Wdr        = input$Wdr,
          Cklein     = input$Cklein,
          I_nSubStep = input$I_nSubStep,
          I_GWS_NAP  = input$I_GWS_NAP,
          I_GV_NAP   = input$I_GV_NAP
        ),
        file
      )
    }
  )
  
  output$plot_A_B <- plotly::renderPlotly({
    
    req(res1())
    df <- res1()
    
    # Convert to Date and convert A and B from meters -> millimeters
    df_plot <- df |>
      dplyr::mutate(
        Date = as.Date(stresses_reactive()$Date[1]) + as.integer(Tijd),
        A_mm = A * 1000,
        B_mm = B * 1000
      )
    
    p <- ggplot(df_plot, aes(x = Date)) +
      
      # Legend labels shown to user: "A (mm)" and "B (mm)"
      geom_line(aes(y = B_mm, colour = "B (mm)"), linewidth = 0.9) +
      geom_line(aes(y = A_mm, colour = "A (mm)"), linewidth = 0.9) +
 
      scale_colour_manual(
        name = "Legend",
        values = c(
          "B (mm)" = "red",   # FIRST in legend
          "A (mm)" = "blue"   # SECOND in legend
        )
      ) +
      
      labs(
        x = "Date",
        y = "Storage (mm)"
      ) +
      
      theme_minimal(base_size = 14)
    
    plotly::ggplotly(p, dynamicTicks = TRUE) %>%
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        
        xaxis = list(
          type      = "date",
          tickangle = -45,
          showline  = TRUE,
          linewidth = 1,
          linecolor = "black",
          mirror    = TRUE,
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.6)"
        ),
        
        yaxis = list(
          showline  = TRUE,
          linewidth = 1,
          linecolor = "black",
          mirror    = TRUE,
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.6)"
        ),
        
        legend = list(
          title = list(text = "Legend"),
          x = 1.02, y = 1,
          xanchor = "left", yanchor = "top",
          bgcolor = "rgba(255,255,255,0.9)",
          bordercolor = "black",
          borderwidth = 1
        ),
        
        margin = list(l = 70, r = 150, t = 40, b = 80)
      ) %>%
      plotly::config(displaylogo = FALSE)
    
  })
  
  output$plot_cum <- plotly::renderPlotly({
    
    req(res1())
    df <- res1()
    
    # ---- Convert to Date & convert m → mm ----
    df_plot <- df |>
      dplyr::mutate(
        Date       = as.Date(stresses_reactive()$Date[1]) + as.integer(Tijd),
        NNcum_mm   = NN_cum   * 1000,
        OAcum_mm   = OA_cum   * 1000,
        DrAcum_mm  = DrA_cum  * 1000,
        Kwelcum_mm = Kwel_cum * 1000,
        DrBcum_mm  = DrB_cum  * 1000,
        ErrTot_mm  = ErrTot   * 1000
      )
    
    # ---- Build ggplot ----
    p <- ggplot(df_plot, aes(x = Date)) +
      
      geom_line(aes(y = NNcum_mm,   colour = "NN (cum)"),   linewidth = 0.9) +
      geom_line(aes(y = OAcum_mm,   colour = "OA (cum)"),   linewidth = 0.9) +
      geom_line(aes(y = DrAcum_mm,  colour = "DrA (cum)"),  linewidth = 0.9) +
      geom_line(aes(y = Kwelcum_mm, colour = "Kwel (cum)"), linewidth = 0.9) +
      geom_line(aes(y = DrBcum_mm,  colour = "DrB (cum)"),  linewidth = 0.9) +
      geom_line(aes(y = ErrTot_mm,  colour = "ErrTot"),     linewidth = 0.9) +
      
      scale_colour_manual(
        name = "Legend",
        values = c(
          "NN (cum)"   = "grey40",
          "OA (cum)"   = "darkgreen",
          "DrA (cum)"  = "purple",
          "Kwel (cum)" = "orange",
          "DrB (cum)"  = "black",
          "ErrTot"     = "red"
        )
      ) +
      
      labs(
        x = "Date",
        y = "Volume (cumulative, mm)"
      ) +
      
      theme_minimal(base_size = 14)
    
    # ---- Convert to plotly ----
    plotly::ggplotly(p, dynamicTicks = TRUE) %>%
      plotly::layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        
        xaxis = list(
          type      = "date",
          tickangle = -45,
          showline  = TRUE,
          linewidth = 1,
          linecolor = "black",
          mirror    = TRUE,
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.6)"
        ),
        
        yaxis = list(
          showline   = TRUE,
          linewidth  = 1,
          linecolor  = "black",
          mirror     = TRUE,
          showgrid   = TRUE,
          gridcolor  = "rgba(200,200,200,0.6)"
        ),
        
        legend = list(
          title       = list(text = "Legend"),
          x           = 1.02,
          y           = 1,
          xanchor     = "left",
          yanchor     = "top",
          bgcolor     = "rgba(255,255,255,0.9)",
          bordercolor = "black",
          borderwidth = 1
        ),
        
        margin = list(l = 70, r = 150, t = 40, b = 80)
      ) %>%
      plotly::config(displaylogo = FALSE)
    
  })
  
    # ---------------------------
  # UPLOAD SETTINGS (System + Initial + Stresses)
  # ---------------------------
  observeEvent(input$upload_all_input, {
    req(input$upload_all_input)
    
    vals <- readRDS(input$upload_all_input$datapath)
    
    updateNumericInput(session, "Mv_NAP", value = vals$Mv_NAP)
    updateNumericInput(session, "Gr_NAP", value = vals$Gr_NAP)
    updateNumericInput(session, "Topc0_NAP", value = vals$Topc0_NAP)
    updateNumericInput(session, "EW_diepte", value = vals$EW_diepte)  
    updateNumericInput(session, "z_krit", value = vals$z_krit)  
    updateNumericInput(session, "PorSat", value = vals$PorSat)
    updateNumericInput(session, "PorFr", value = vals$PorFr)
    updateNumericInput(session, "c0", value = vals$c0)
    updateNumericInput(session, "Wdr", value = vals$Wdr)
    updateNumericInput(session, "Cklein", value = vals$Cklein)
    updateNumericInput(session, "I_nSubStep", value = vals$I_nSubStep)
    updateNumericInput(session, "I_GWS_NAP", value = vals$I_GWS_NAP)
    updateNumericInput(session, "I_GV_NAP", value = vals$I_GV_NAP)
  })  

  observe({
    df <- stresses_reactive()
    # Ensure Date column is Date
    if (!inherits(df$Date, "Date")) {
      suppressWarnings({
        df$Date <- as.Date(df$Date, tryFormats = c("%Y-%m-%d", "%d-%m-%Y"))
      })
    }
    if (nrow(df) > 0 && inherits(df$Date, "Date")) {
      updateDateInput(session, "stats_from", value = min(df$Date, na.rm = TRUE))
      updateDateInput(session, "stats_to",   value = max(df$Date, na.rm = TRUE))
    }
  })
  

  output$stats_table <- renderTable({
    stats_reactive()
  })
  
  observeEvent(input$clear_observations, {
    # Clear reactive value
    observations_reactive(tibble::tibble(
      Date = as.Date(character()),
      Value = numeric(),
      Type = character()
    ))
    
    # Update global storage too
    observations_global <<- tibble::tibble(
      Date = as.Date(character()),
      Value = numeric(),
      Type = character()
    )
    
    showNotification("Observations cleared.", type = "warning")
  })  
    
  output$download_stresses_excel <- downloadHandler(
    filename = function() {
      paste0("stresses_", Sys.Date(), ".xlsx")
    },
    content = function(path) {
      
      df <- stresses_reactive()
      
      # Ensure Date is class Date and NOT POSIXct
      df$Date <- as.Date(df$Date)
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "stresses")
      
      openxlsx::writeData(wb, sheet = 1, x = df)
      
      # Apply Excel date format (avoids time)
      openxlsx::addStyle(
        wb,
        sheet = 1,
        rows = 2:(nrow(df) + 1),
        cols = 1,
        style = openxlsx::createStyle(numFmt = "dd-mm-yyyy"),
        gridExpand = TRUE
      )
      
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    }
  )  

  output$download_observations_excel <- downloadHandler(
    filename = function() {
      paste0("observations_", Sys.Date(), ".xlsx")
    },
    content = function(path) {
      
      df <- observations_reactive()
      
      # Ensure Date is class Date and NOT POSIXct
      df$Date <- as.Date(df$Date)
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "stresses")
      
      openxlsx::writeData(wb, sheet = 1, x = df)
      
      # Apply Excel date format (avoids time)
      openxlsx::addStyle(
        wb,
        sheet = 1,
        rows = 2:(nrow(df) + 1),
        cols = 1,
        style = openxlsx::createStyle(numFmt = "dd-mm-yyyy"),
        gridExpand = TRUE
      )
      
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    }
  )  
  
  output$download_results_excel <- downloadHandler(
    filename = function() {
      paste0("results_", Sys.Date(), ".xlsx")
    },
    content = function(path) {
      
      req(res1())
      
      df <- res1()
      
      # Convert to real dates
      start_date <- stresses_reactive()$Date[1]
      df$Date <- as.Date(start_date) + as.integer(df$Tijd)
      
      # ---- Add converted values ----
      df$GWS_NAP  <- df$GWS + input$Topc0_NAP
      df$GV_NAP   <- df$GV  + input$Topc0_NAP
      df$A_mm     <- df$A  * 1000
      df$B_mm     <- df$B  * 1000
      
      # Fluxes (mm/day)
      df$NN_mm     <- df$NN   * 1000
      df$OA_mm     <- df$OA   * 1000
      df$Kwel_mm   <- df$Kwel * 1000
      df$Dr_mm     <- df$Dr   * 1000
      df$DrA_mm    <- df$DrA  * 1000
      df$DrB_mm    <- df$DrB  * 1000
      
      # Cumulative volumes (mm)
      df$NNcum_mm   <- df$NN_cum   * 1000
      df$OAcum_mm   <- df$OA_cum   * 1000
      df$DrAcum_mm  <- df$DrA_cum  * 1000
      df$Kwelcum_mm <- df$Kwel_cum * 1000
      df$DrBcum_mm  <- df$DrB_cum  * 1000
      df$ErrTot_mm  <- df$ErrTot   * 1000
      
      # Write to Excel
      writexl::write_xlsx(df, path)
    }
  )
  
  output$download_stats_excel <- downloadHandler(
    filename = function() {
      paste0("statistics_", Sys.Date(), ".xlsx")
    },
    content = function(path) {
      
      stats <- stats_reactive()
      req(stats)
      
      writexl::write_xlsx(stats, path)
    }
  )
    
}
