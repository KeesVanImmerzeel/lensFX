library(shiny)
library(readxl)
library(dplyr)
library(deSolve)
library(openxlsx)
library(Rcpp)
sourceCpp("src/lensFX.cpp")

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

get_GWS  <- function(A, B, Mv, PorFc, PorFr)
  get_GWS_cpp(A, B, Mv, PorFc, PorFr)

get_GV   <- function(B, PorSat)
  get_GV_cpp(B, PorSat)

get_Kwel <- function(H1, GWS, c0)
  get_Kwel_cpp(H1, GWS, c0)

get_Dr <- function(GWS, HSloot, Wdr, inf_fact)
  get_Dr_cpp(GWS, HSloot, Wdr, inf_fact)

get_DrB  <- function(GV, GWS, Dr)
  get_DrB_cpp(GV, GWS, Dr)

get_DrA  <- function(GWS, GV, Dr)
  get_DrA_cpp(GWS, GV, Dr)

get_OA   <- function(GWS, Gr, Cklein)
  get_OA_cpp(GWS, Gr, Cklein)

#Gr = greppelniveau (m+c0)


# R wrapper voor afgeleiden (SpeedProc) voor constante externe parameters binnen de tijdstap
lens_derivs <- function(t, y, pars) {
  res <- lens_derivs_cpp(t, y, pars)
  list(res$dy)
}

# R wrapper voor Correctiestap (na elke hoofd-tijdstap), identiek aan Delphi Correct()
correct_state <- function(A, B, state_aux, pars) {
  correct_state_cpp(A, B, state_aux, pars)
}

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

# Eén simulatie uitvoeren (baseline of scenario)
simulate_lens <- function(inp,
                          dH1 = 0,
                          dHSloot = 0,
                          dNN = 0,
                          PorFr_override = NULL,
                          c0_override = NULL,
                          Wdr_override = NULL,
                          inf_fact_override = NULL,
                          Cklein_override = NULL,
                          updateProgress = NULL) {   # <- KEEP THIS
  
  PorFr  <- if (!is.null(PorFr_override)) PorFr_override else inp$PorFr
  c0     <- if (!is.null(c0_override))    c0_override    else inp$c0
  Wdr    <- if (!is.null(Wdr_override))   Wdr_override   else inp$Wdr
  inf_fact    <- if (!is.null(inf_fact_override))   inf_fact_override   else inp$inf_fact
  Cklein <- if (!is.null(Cklein_override)) Cklein_override else inp$Cklein

  simulate_lens_cpp(
    PorSat = inp$PorSat,
    PorFr  = PorFr,
    Mv     = inp$Mv,
    Gr     = inp$Gr,
    c0     = c0,
    Wdr    = Wdr,
    inf_fact = inf_fact,
    Cklein = Cklein,
    I_GWS  = inp$I_GWS,  # ✅ initial groundwater level (lens)
    I_GV   = inp$I_GV,   # ✅ initial groundwater level (aquifer)
    dt     = inp$TimeStepSize,
    nstep  = inp$TFinish,
    nSub   = inp$I_nSubStep,
    H1     = inp$external$H1,
    HSloot = inp$external$HSloot,
    NN     = inp$external$NN
  )    
}

################# optimise ################
prepare_observations <- function(obs, stresses, Topc0_NAP) {
  
  stopifnot(is.data.frame(obs))
  stopifnot(nrow(stresses) > 0)
  
  start_date <- as.Date(stresses$Date[1])
  
  
  obs2 <- obs |>
    dplyr::mutate(
      Date = as.Date(Date),
      Type = trimws(Type),          # critical
      Value_model = Value - Topc0_NAP
    ) |>
    dplyr::filter(
      !is.na(Date),
      Type %in% c("GWS_NAP", "GV_NAP")
    )
  
  
  if (nrow(obs2) == 0) {
    stop("No valid observations after preprocessing (check dates and Type).")
  }
  
  obs2
} 

objective_lens <- function(par_free,
                           free_params,
                           inp,
                           obs,
                           stresses,
                           Topc0_NAP,
                           weight_GWS = 0.5) {   # <-- OPEN BRACE HERE   # NEW: relative weight for GWS {
  weight_GV <- 1 - weight_GWS
  
  # --------------------------------------------------
  # 1. Reconstruct full parameter vector
  # --------------------------------------------------
  pars_full <- c(
    c0     = inp$c0,
    Wdr    = inp$Wdr,
    Cklein = inp$Cklein
  )
  
  pars_full[free_params] <- par_free
  
  # --------------------------------------------------
  # 2. Run model
  # --------------------------------------------------
  sim <- simulate_lens(
    inp,
    c0_override     = pars_full["c0"],
    Wdr_override    = pars_full["Wdr"],
    Cklein_override = pars_full["Cklein"]
  )
  
  # --------------------------------------------------
  # 3. Prepare observations
  # --------------------------------------------------
  obs_prep <- prepare_observations(obs, stresses, Topc0_NAP)
  
  # Reconstruct model dates
  sim_sel <- sim |>
    dplyr::mutate(
      Date = as.Date(stresses$Date[1]) + as.integer(Tijd)
    ) |>
    dplyr::select(Date, GWS, GV)
  
  df <- dplyr::left_join(obs_prep, sim_sel, by = "Date")
  
  # --------------------------------------------------
  # 4. Residual selection (NO NA leakage)
  # --------------------------------------------------
  gws_rows <- df$Type == "GWS_NAP" & !is.na(df$GWS)
  gv_rows  <- df$Type == "GV_NAP"  & !is.na(df$GV)
  
  n_gws <- sum(gws_rows)
  n_gv  <- sum(gv_rows)
  
  if (n_gws + n_gv == 0) {
    warning("No observations matched model output – returning large penalty.")
    return(1e12)
  }
  
  # --------------------------------------------------
  # 5. Objective function (WEIGHTED & NORMALISED)
  # --------------------------------------------------
  
  err <- 0
  
  if (n_gws > 0 && weight_GWS > 0) {
    err_gws <- sum(
      (df$GWS[gws_rows] - df$Value_model[gws_rows])^2
    ) / n_gws
    
    err <- err + weight_GWS * err_gws
  }
  
  if (n_gv > 0 && weight_GV > 0) {
    err_gv <- sum(
      (df$GV[gv_rows] - df$Value_model[gv_rows])^2
    ) / n_gv
    
    err <- err + weight_GV * err_gv
  }
  
  # --------------------------------------------------
  # 6. Safety check
  # --------------------------------------------------
  if (!is.finite(err)) {
    stop("Objective function produced non-finite value.")
  }
  
  err
}


compute_sensitivity_oat <- function(par_full,
                                    inp,
                                    obs,
                                    stresses,
                                    Topc0_NAP,
                                    delta_rel = 0.05) {
  
  base_params <- par_full
  free_params <- names(par_full)
  
  # Base objective
  J_base <- objective_lens(
    par_free    = par_full,
    free_params = free_params,
    inp         = inp,
    obs         = obs,
    stresses    = stresses,
    Topc0_NAP   = Topc0_NAP
  )
  
  sens_list <- lapply(names(par_full), function(p) {
    
    par_up   <- par_full
    par_down <- par_full
    
    par_up[p]   <- par_full[p] * (1 + delta_rel)
    par_down[p] <- par_full[p] * (1 - delta_rel)
    
    J_up <- objective_lens(
      par_free    = par_up,
      free_params = free_params,
      inp         = inp,
      obs         = obs,
      stresses    = stresses,
      Topc0_NAP   = Topc0_NAP
    )
    
    J_down <- objective_lens(
      par_free    = par_down,
      free_params = free_params,
      inp         = inp,
      obs         = obs,
      stresses    = stresses,
      Topc0_NAP   = Topc0_NAP
    )
    
    sensitivity <- (J_up - J_down) / (2 * delta_rel * par_full[p])
    
    data.frame(
      Parameter    = p,
      Base_value   = par_full[p],
      J_base       = J_base,
      J_minus      = J_down,
      J_plus       = J_up,
      Sensitivity  = sensitivity,
      row.names    = NULL
    )
  })
  
  do.call(rbind, sens_list)
}


build_xy_fit_data <- function(sim, obs, stresses, Topc0_NAP) {
  
  if (is.null(sim) || is.null(obs) || nrow(obs) == 0) {
    return(NULL)
  }
  
  # --- model output with real dates ---
  sim_df <- sim |>
    dplyr::mutate(
      Date = as.Date(stresses$Date[1]) + as.integer(Tijd),
      GWS_NAP = GWS + Topc0_NAP,
      GV_NAP  = GV  + Topc0_NAP
    ) |>
    dplyr::select(Date, GWS_NAP, GV_NAP)
  
  # --- observations ---
  obs_df <- obs |>
    dplyr::mutate(
      Date = as.Date(Date),
      Type = trimws(Type),
      Obs_NAP = Value
    ) |>
    dplyr::filter(Type %in% c("GWS_NAP", "GV_NAP"))
  
  # --- join ---
  df <- dplyr::left_join(obs_df, sim_df, by = "Date")
  
  # --- long tidy format ---
  df_long <- dplyr::bind_rows(
    df |> dplyr::filter(Type == "GWS_NAP", !is.na(GWS_NAP)) |>
      dplyr::transmute(
        Type     = "GWS_NAP",
        Observed = Obs_NAP,
        Simulated = GWS_NAP
      ),
    
    df |> dplyr::filter(Type == "GV_NAP", !is.na(GV_NAP)) |>
      dplyr::transmute(
        Type     = "GV_NAP",
        Observed = Obs_NAP,
        Simulated = GV_NAP
      )
  )
  
  df_long
}

plot_xy_from_df <- function(df, title_suffix = "") {
  
  r2 <- compute_r2(df)
  
  r2_txt <- paste0(
    "R² all = ", sprintf("%.3f", r2$R2_all),
    " | GWS = ", sprintf("%.3f", r2$R2_GWS),
    " | GV = ", sprintf("%.3f", r2$R2_GV)
  )
  
  p <- ggplot(df, aes(x = Observed, y = Simulated)) +
    geom_abline(
      slope = 1, intercept = 0,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_point(
      data = dplyr::filter(df, Type == "GWS_NAP"),
      colour = "blue", alpha = 0.8, size = 2
    ) +
    geom_point(
      data = dplyr::filter(df, Type == "GV_NAP"),
      colour = "red", alpha = 0.8, size = 2
    ) +
    coord_equal() +
    labs(
      x = "Observed (m + NAP)",
      y = "Simulated (m + NAP)",
      title = "Observed vs Simulated levels"
    ) +
    theme_minimal(base_size = 14)
  
  list(plot = p, subtitle = paste(title_suffix, "—", r2_txt))
}

compute_r2 <- function(df) {
  
  # Guard
  if (is.null(df) || nrow(df) < 2) {
    return(list(
      R2_all = NA_real_,
      R2_GWS = NA_real_,
      R2_GV  = NA_real_
    ))
  }
  
  r2_safe <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2) return(NA_real_)
    cor(x[ok], y[ok])^2
  }
  
  list(
    R2_all = r2_safe(df$Observed, df$Simulated),
    R2_GWS = r2_safe(
      df$Observed[df$Type == "GWS_NAP"],
      df$Simulated[df$Type == "GWS_NAP"]
    ),
    R2_GV  = r2_safe(
      df$Observed[df$Type == "GV_NAP"],
      df$Simulated[df$Type == "GV_NAP"]
    )
  )
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
  res_pre_opt <- reactiveVal(NULL)
  
  # Resultaten
  res1 <- reactiveVal(NULL)    
  stats_reactive <- reactiveVal(NULL)
  
  opt_par_reactive     <- reactiveVal(NULL)
  opt_summary_reactive <- reactiveVal(NULL)
  opt_status_reactive  <- reactiveVal(NULL)
  sensitivity_reactive <- reactiveVal(NULL)
  optimisation_done <- reactiveVal(FALSE)
  opt_simulation_done <- reactiveVal(FALSE)

  
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
  #optimisation_done(FALSE)   # ✅ RESET STATE

    if (is.null(res_pre_opt())) {
      res_pre_opt(NULL)
    }
    
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
      inf_fact=input$inf_fact,
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
    
    # ✅ Capture baseline ONLY ON FIRST RUN
    if (is.null(res_pre_opt())) {
      res_pre_opt(r)
    }

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
    
    
    #x_min <- min(df_plot$Date, na.rm = TRUE)
    #x_max <- max(df_plot$Date, na.rm = TRUE)
    
    sim_start <- min(stresses$Date, na.rm = TRUE)
    sim_end   <- max(stresses$Date, na.rm = TRUE)
    
    x_min <- sim_start
    x_max <- sim_end
    
    
    # =========================================================
    # FILTER OBSERVATIONS TO SIMULATION PERIOD (PLOTTING ONLY)
    # =========================================================
 #   if (!is.null(obs_plot)) {
#      obs_plot <- obs_plot |>
#        dplyr::filter(
#          !is.na(Date),
#          Date >= sim_start,
#          Date <= sim_end
#        )
#    }
    
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
          # ---------------------------
          # SYSTEM
          # ---------------------------
          Mv_NAP     = input$Mv_NAP,
          Gr_NAP     = input$Gr_NAP,
          Topc0_NAP  = input$Topc0_NAP,
          EW_diepte  = input$EW_diepte,
          z_krit     = input$z_krit,
          PorSat     = input$PorSat,
          PorFr      = input$PorFr,
          c0         = input$c0,
          Wdr        = input$Wdr,
          inf_fact   = input$inf_fact,
          Cklein     = input$Cklein,
          
          # ---------------------------
          # INITIAL CONDITIONS
          # ---------------------------
          I_nSubStep = input$I_nSubStep,
          I_GWS_NAP  = input$I_GWS_NAP,
          I_GV_NAP   = input$I_GV_NAP,
          
          # ---------------------------
          # OPTIMISATION SETTINGS
          # ---------------------------
          opt = list(
            c0_range      = input$opt_c0_range,
            Wdr_range     = input$opt_Wdr_range,
            Cklein_range  = input$opt_Cklein_range,
            weight_gws    = input$opt_weight_gws,
            method        = input$opt_method,
            maxiter       = input$opt_maxiter,
            exclude       = input$opt_exclude_params
          ),
          
          # ---------------------------
          # OPTIONAL: RESULTS
          # ---------------------------
          optimisation_done = optimisation_done(),
          optimised_params  = opt_par_reactive(),
          opt_summary       = opt_summary_reactive()
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
    
    # ---------------------------
    # SYSTEM
    # ---------------------------
    updateNumericInput(session, "Mv_NAP", value = vals$Mv_NAP)
    updateNumericInput(session, "Gr_NAP", value = vals$Gr_NAP)
    updateNumericInput(session, "Topc0_NAP", value = vals$Topc0_NAP)
    updateNumericInput(session, "EW_diepte", value = vals$EW_diepte)
    updateNumericInput(session, "z_krit", value = vals$z_krit)
    updateNumericInput(session, "PorSat", value = vals$PorSat)
    updateNumericInput(session, "PorFr", value = vals$PorFr)
    updateNumericInput(session, "c0", value = vals$c0)
    updateNumericInput(session, "Wdr", value = vals$Wdr)
    updateNumericInput(
      session,
      "inf_fact",
      value = if (!is.null(vals$inf_fact)) vals$inf_fact else 0
    )
    updateNumericInput(session, "Cklein", value = vals$Cklein)
    
    # ---------------------------
    # INITIAL CONDITIONS
    # ---------------------------
    updateNumericInput(session, "I_nSubStep", value = vals$I_nSubStep)
    updateNumericInput(session, "I_GWS_NAP", value = vals$I_GWS_NAP)
    updateNumericInput(session, "I_GV_NAP", value = vals$I_GV_NAP)
    
    # ---------------------------
    # OPTIMISATION SETTINGS
    # ---------------------------
    if (!is.null(vals$opt)) {
      
      updateSliderInput(session, "opt_c0_range",
                        value = vals$opt$c0_range)
      
      updateSliderInput(session, "opt_Wdr_range",
                        value = vals$opt$Wdr_range)
      
      updateSliderInput(session, "opt_Cklein_range",
                        value = vals$opt$Cklein_range)
      
      updateSliderInput(session, "opt_weight_gws",
                        value = vals$opt$weight_gws)
      
      updateSelectInput(session, "opt_method",
                        selected = vals$opt$method)
      
      updateNumericInput(session, "opt_maxiter",
                         value = vals$opt$maxiter)
      
      updateCheckboxGroupInput(
        session,
        "opt_exclude_params",
        selected = vals$opt$exclude %||% character(0)
      )
    }
    
    showNotification("All model and optimisation settings restored.", type = "message")
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
      req(input$stats_from, input$stats_to)
      
      from <- format(as.Date(input$stats_from), "%Y%m%d")
      to   <- format(as.Date(input$stats_to), "%Y%m%d")
      
      paste0("statistics_", from, "_to_", to, ".xlsx")
    },
    content = function(path) {
      stats <- stats_reactive()
      req(stats)
      
      writexl::write_xlsx(stats, path)
    }
  )
  
  observeEvent(input$run_optimisation, {
    
    # --------------------------------------------------
    # 1. VALIDATION
    # --------------------------------------------------
    
    if (is.null(inp())) {
      showModal(modalDialog(
        title = "Model not initialised",
        "Please run the model first before starting optimisation.",
        easyClose = TRUE
      ))
      return()
    }
    
    obs <- observations_reactive()
    if (is.null(obs) || nrow(obs) == 0) {
      showModal(modalDialog(
        title = "No observations available",
        "Upload at least one observation (GWS or GV) before optimising.",
        easyClose = TRUE
      ))
      return()
    }
    
    all_params <- c("c0", "Wdr", "Cklein")
    excluded   <- input$opt_exclude_params
    
    if (length(excluded) >= 3) {
      showModal(modalDialog(
        title = "Invalid optimisation settings",
        "At least one parameter must remain free.",
        easyClose = TRUE
      ))
      return()
    }
    
    free_params <- setdiff(all_params, excluded)
    
    # --------------------------------------------------
    # 2. INITIAL VALUES AND BOUNDS
    # --------------------------------------------------
    
    par_full <- c(
      c0     = input$c0,
      Wdr    = input$Wdr,
      Cklein = input$Cklein
    )
    
    par0 <- par_full[free_params]
    
    lower <- c(
      c0     = input$opt_c0_range[1],
      Wdr    = input$opt_Wdr_range[1],
      Cklein = input$opt_Cklein_range[1]
    )[free_params]
    
    upper <- c(
      c0     = input$opt_c0_range[2],
      Wdr    = input$opt_Wdr_range[2],
      Cklein = input$opt_Cklein_range[2]
    )[free_params]
    
    # --------------------------------------------------
    # 3. OBJECTIVE BEFORE OPTIMISATION
    # --------------------------------------------------
    
    init_err <- objective_lens(
      par_free    = par0,
      free_params = free_params,
      inp         = inp(),
      obs         = obs,
      stresses    = stresses_reactive(),
      Topc0_NAP   = input$Topc0_NAP
    )
    
    # --------------------------------------------------
    # 4. RUN OPTIMISER (L-BFGS-B)
    # --------------------------------------------------
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Optimising parameters…", value = 0)
    
    opt <- optim(
      par         = par0,
      fn          = objective_lens,
      free_params = free_params,
      inp         = inp(),
      obs         = obs,
      stresses    = stresses_reactive(),
      Topc0_NAP   = input$Topc0_NAP,
      weight_GWS  = input$opt_weight_gws,
      method      = "L-BFGS-B",
      lower       = lower,
      upper       = upper,
      control     = list(
        maxit    = input$opt_maxiter,
        parscale = par0
      )
    )
    optimisation_done(TRUE)
    
    # --------------------------------------------------
    # 5. UPDATE NUMERIC INPUTS
    # --------------------------------------------------
    
    if ("c0" %in% names(opt$par)) {
      updateNumericInput(session, "c0", value = as.numeric(opt$par["c0"]))
    }
    if ("Wdr" %in% names(opt$par)) {
      updateNumericInput(session, "Wdr", value = as.numeric(opt$par["Wdr"]))
    }
    if ("Cklein" %in% names(opt$par)) {
      updateNumericInput(session, "Cklein", value = as.numeric(opt$par["Cklein"]))
    }
    
    
    # --------------------------------------------------
    # 9. SENSITIVITY ANALYSIS (LOCAL OAT)
    # --------------------------------------------------
    
    par_optimised <- c(
      c0     = as.numeric(opt$par["c0"]), #input$c0,
      Wdr    = as.numeric(opt$par["Wdr"]), #input$Wdr,
      Cklein = as.numeric(opt$par["Cklein"]) #input$Cklein
    )
    
    sens_df <- compute_sensitivity_oat(
      par_full  = par_optimised,
      inp       = inp(),
      obs       = obs,
      stresses  = stresses_reactive(),
      Topc0_NAP = input$Topc0_NAP,
      delta_rel = 0.05
    )
    
    sensitivity_reactive(sens_df)
    
    
    # --------------------------------------------------
    # 6. STORE OPTIMISATION RESULTS (REACTIVE)
    # --------------------------------------------------
    
    opt_par_reactive(
      data.frame(
        Parameter = names(opt$par),
        Value     = round(as.numeric(opt$par), 3),
        row.names = NULL
      )
    )
    
    final_err <- opt$value
    abs_improvement <- init_err - final_err
    rel_improvement <- if (init_err > 0)
      100 * abs_improvement / init_err else NA_real_
    
    opt_summary_reactive(
      data.frame(
        Metric = c(
          "Initial objective value",
          "Final objective value",
          "Absolute improvement",
          "Relative improvement (%)"
        ),
        Value = round(
          c(init_err, final_err, abs_improvement, rel_improvement),
          c(4, 4, 4, 2)
        ),
        row.names = NULL
      )
    )
    
    opt_status_reactive(
      paste0(
        "Optimisation completed successfully.\n\n",
        "Initial objective value : ", round(init_err, 4), "\n",
        "Final objective value   : ", round(final_err, 4), "\n\n",
        "Absolute improvement    : ", round(abs_improvement, 4), "\n",
        "Relative improvement    : ", round(rel_improvement, 2), " %\n",
        "Convergence code        : ", opt$convergence
      )
    )
    
    # --------------------------------------------------
    # 7. USER FEEDBACK
    # --------------------------------------------------
    
    if (is.finite(rel_improvement) && rel_improvement > 1) {
      showNotification(
        paste0("Optimisation improved the fit by ",
               round(rel_improvement, 1), "%."),
        type = "message"
      )
    } else {
      showNotification(
        "Optimisation completed, but improvement is small.",
        type = "warning"
      )
    }
    
    # --------------------------------------------------
    # 8. RE-RUN MODEL WITH OPTIMISED PARAMETERS (CORRECT)
    # --------------------------------------------------
    
    r_opt <- simulate_lens(
      inp(),
      c0_override     = as.numeric(opt$par["c0"]),
      Wdr_override    = as.numeric(opt$par["Wdr"]),
      Cklein_override = as.numeric(opt$par["Cklein"])
    )
    
    res1(r_opt)  
    opt_simulation_done(TRUE)

  })

  # ---------------------------------
  # Optimisation outputs
  # ---------------------------------
  
  output$opt_status <- renderText({
    req(opt_status_reactive())
    opt_status_reactive()
  })
  
  output$opt_summary <- renderTable({
    req(opt_summary_reactive())
    opt_summary_reactive()
  })
  
  output$opt_results <- renderTable({
    req(opt_par_reactive())
    opt_par_reactive()
  })
  
  output$sensitivity_table <- renderTable({
    req(sensitivity_reactive())
    sensitivity_reactive()
  })
  
  output$plot_xy_before <- plotly::renderPlotly({
    
    req(res_pre_opt())
    req(observations_reactive())
    
    df_before <- build_xy_fit_data(
      sim       = res_pre_opt(),
      obs       = observations_reactive(),
      stresses  = stresses_reactive(),
      Topc0_NAP = input$Topc0_NAP
    )
    
    validate(need(nrow(df_before) > 0,
                  "No matched observations (before optimisation)."))
    
    out <- plot_xy_from_df(df_before, "Before optimisation")
    
    plotly::ggplotly(out$plot) |>
      plotly::layout(
        title = list(
          text = paste(
            "Observed vs Simulated levels",
            "<br><span style='font-size:12px;'>", out$subtitle, "</span>"
          )
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        hovermode     = "closest"
      ) |>
      plotly::config(displaylogo = FALSE)
  })
  
  output$plot_xy_after <- plotly::renderPlotly({
    
    req(opt_simulation_done())
    req(optimisation_done())
    req(res1())
    req(observations_reactive())
    
    df_after <- build_xy_fit_data(
      sim       = res1(),
      obs       = observations_reactive(),
      stresses  = stresses_reactive(),
      Topc0_NAP = input$Topc0_NAP
    )
    
    validate(need(nrow(df_after) > 0,
                  "No matched observations (after optimisation)."))
    
    out <- plot_xy_from_df(df_after, "After optimisation")
    
    plotly::ggplotly(out$plot) |>
      plotly::layout(
        title = list(
          text = paste(
            "Observed vs Simulated levels",
            "<br><span style='font-size:12px;'>", out$subtitle, "</span>"
          )
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        hovermode     = "closest"
      ) |>
      plotly::config(displaylogo = FALSE)
  })
}
