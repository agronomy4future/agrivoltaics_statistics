# ─────────────────────────────────────────────────────────────────────────────
# Agrivoltaics Statistics App
# Updated model logic based on Block independent/dependent,
# Replicate, Annual/Perennial structure
# agronomy4future.com
# ─────────────────────────────────────────────────────────────────────────────

if(!require(lme4))         install.packages("lme4");         library(lme4)
if(!require(lmerTest))     install.packages("lmerTest");     library(lmerTest)
if(!require(shiny))        install.packages("shiny");        library(shiny)
if(!require(shinydashboard)) install.packages("shinydashboard"); library(shinydashboard)
if(!require(emmeans))      install.packages("emmeans");      library(emmeans)
if(!require(multcomp))     install.packages("multcomp");     library(multcomp)
if(!require(multcompView)) install.packages("multcompView"); library(multcompView)
if(!require(ggplot2))      install.packages("ggplot2");      library(ggplot2)
if(!require(DT))           install.packages("DT");           library(DT)
if(!require(readr))        install.packages("readr");        library(readr)
if(!require(dplyr))        install.packages("dplyr");        library(dplyr)

# ─────────────────────────────────────────────────────────────────────────────
# CASE DESCRIPTIONS
# ─────────────────────────────────────────────────────────────────────────────
CASE_DESC <- c(
  "1" = "Single crop · single row",
  "2" = "Single crop · multiple rows",
  "3" = "Single crop · single row · multiple seasons",
  "4" = "Single crop · multiple rows · multiple seasons",
  "5" = "Multiple cultivars · single row",
  "6" = "Multiple cultivars · multiple rows",
  "7" = "Multiple cultivars · single row · multiple seasons",
  "8" = "Multiple cultivars · multiple rows · multiple seasons"
)

# ─────────────────────────────────────────────────────────────────────────────
# FORMULA BUILDER
# Key logic:
#   Block independent = AV and Control have different block names
#   Block dependent   = AV and Control share the same block names (Block1)
#   Replicate         = more than one observation per Block (or Block:Row)
#   Annual            = new plants each season
#   Perennial         = same plants measured across seasons
#   Season < 4        = treat as Annual (insufficient levels for random effect)
# ─────────────────────────────────────────────────────────────────────────────
build_formula <- function(v) {

  has <- function(k) !is.null(v[[k]]) && nzchar(v[[k]]) && v[[k]] != "none"

  # ── shortcuts
  Y        <- v$y
  SITE     <- v$site
  BLOCK    <- v$block      # independent block column
  BLOCK1   <- v$block1     # dependent (shared) block column
  ROW1     <- if (has("row"))      v$row      else NULL
  SEASON   <- if (has("season"))   v$season   else NULL
  GENO     <- if (has("genotype")) v$genotype else NULL
  LOC      <- if (has("location")) v$location else NULL

  block_dep   <- isTRUE(v$block_type == "dependent")
  has_rep     <- isTRUE(v$has_replicates)
  is_peren    <- isTRUE(v$crop_type == "perennial")
  n_season    <- if (!is.null(v$n_season)) as.integer(v$n_season) else 0L

  # Perennial only meaningful when Season >= 4
  peren_valid <- is_peren && n_season >= 4

  # ── determine case number
  has_row    <- !is.null(ROW1)
  has_season <- !is.null(SEASON)
  has_geno   <- !is.null(GENO)

  case_num <-
    if      (!has_geno && !has_row && !has_season) 1L
    else if (!has_geno &&  has_row && !has_season) 2L
    else if (!has_geno && !has_row &&  has_season) 3L
    else if (!has_geno &&  has_row &&  has_season) 4L
    else if ( has_geno && !has_row && !has_season) 5L
    else if ( has_geno &&  has_row && !has_season) 6L
    else if ( has_geno && !has_row &&  has_season) 7L
    else                                           8L

  # ── build fixed effects
  fixed <- switch(as.character(case_num),
    "1" = SITE,
    "2" = c(SITE, ROW1, sprintf("%s:%s", SITE, ROW1)),
    "3" = c(SEASON, SITE, sprintf("%s:%s", SEASON, SITE)),
    "4" = c(SEASON, SITE, ROW1,
            sprintf("%s:%s", SEASON, SITE),
            sprintf("%s:%s", SEASON, ROW1),
            sprintf("%s:%s:%s", SEASON, SITE, ROW1)),
    "5" = c(SITE, GENO, sprintf("%s:%s", SITE, GENO)),
    # Cases 6,7,8: use * operator to include all interactions (avoids rank deficiency)
    "6" = sprintf("%s * %s * %s", SITE, GENO, ROW1),
    "7" = sprintf("%s * %s * %s", SITE, GENO, SEASON),
    "8" = sprintf("%s * %s * %s * %s", SITE, GENO, SEASON, ROW1)
  )

  # ── build random effects
  # Helper: block variable to use
  BLK <- if (block_dep) BLOCK1 else BLOCK

  random <- if (!has_rep) {
    # ── NO REPLICATES → lm() with Block as fixed
    # Block independent + no row + no season: must use Block1 (shared)
    # to avoid confounding with Site
    BLK_FIXED <- if (!block_dep) BLOCK1 else BLOCK1
    switch(as.character(case_num),
      "1" = BLK_FIXED,   # lm fixed blocking
      "2" = if (block_dep) BLOCK1 else BLOCK,
      "3" = BLK_FIXED,
      "4" = if (block_dep) BLOCK1 else BLOCK,
      "5" = if (block_dep) BLOCK1 else BLOCK,
      "6" = if (block_dep) BLOCK1 else BLOCK,
      "7" = BLK_FIXED,
      "8" = if (block_dep) BLOCK1 else BLOCK
    )
  } else {
    # ── HAS REPLICATES → lmer() with random effects
    switch(as.character(case_num),

      # Case 1: single crop · single row
      "1" = if (block_dep)
              sprintf("(1|%s:%s)", SITE, BLOCK1)
            else
              sprintf("(1|%s:%s)", SITE, BLOCK),

      # Case 2: single crop · multiple rows
      "2" = if (block_dep)
              c(sprintf("(1|%s)", BLOCK1),
                sprintf("(1|%s:%s:%s)", SITE, BLOCK1, ROW1))
            else
              c(sprintf("(1|%s)", BLOCK),
                sprintf("(1|%s:%s)", BLOCK, ROW1)),

      # Case 3: single crop · single row · multiple seasons
      "3" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK1),
                  sprintf("(1|%s:%s:%s)", SITE, BLOCK1, SEASON))
              else
                sprintf("(1|%s)", BLOCK1)
            } else {
              sprintf("(1|%s:%s)", SITE, BLOCK)
            },

      # Case 4: single crop · multiple rows · multiple seasons
      "4" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK1),
                  sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK1, ROW1, SEASON))
              else
                c(sprintf("(1|%s)", BLOCK1),
                  sprintf("(1|%s:%s:%s)", SITE, BLOCK1, ROW1))
            } else {
              if (peren_valid)
                c(sprintf("(1|%s:%s)", SITE, BLOCK),
                  sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK, ROW1, SEASON))
              else
                c(sprintf("(1|%s:%s)", SITE, BLOCK),
                  sprintf("(1|%s:%s)", BLOCK, ROW1))
            },

      # Case 5: multiple cultivars · single row
      "5" = if (block_dep)
              sprintf("(1|%s)", BLOCK1)
            else
              sprintf("(1|%s:%s)", SITE, BLOCK),

      # Case 6: multiple cultivars · multiple rows
      "6" = if (block_dep)
              c(sprintf("(1|%s)", BLOCK1),
                sprintf("(1|%s:%s:%s)", SITE, BLOCK1, ROW1))
            else
              c(sprintf("(1|%s:%s)", SITE, BLOCK),
                sprintf("(1|%s:%s)", BLOCK, ROW1)),

      # Case 7: multiple cultivars · single row · multiple seasons
      "7" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK1),
                  sprintf("(1|%s:%s:%s)", SITE, BLOCK1, SEASON))
              else
                sprintf("(1|%s)", BLOCK1)
            } else {
              sprintf("(1|%s:%s)", SITE, BLOCK)
            },

      # Case 8: multiple cultivars · multiple rows · multiple seasons
      "8" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK1),
                  sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK1, ROW1, SEASON))
              else
                c(sprintf("(1|%s)", BLOCK1),
                  sprintf("(1|%s:%s:%s)", SITE, BLOCK1, ROW1))
            } else {
              if (peren_valid)
                c(sprintf("(1|%s:%s)", SITE, BLOCK),
                  sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK, ROW1, SEASON))
              else
                c(sprintf("(1|%s:%s)", SITE, BLOCK),
                  sprintf("(1|%s:%s)", BLOCK, ROW1))
            }
    )
  }

  # ── Location modifier (if provided)
  if (!is.null(LOC)) {
    if (has_rep) {
      random <- c(random, sprintf("(1|%s:%s)", LOC, BLK))
    }
    fixed <- c(fixed, LOC)
  }

  # ── assemble formula string
  use_lm <- !has_rep

  if (use_lm) {
    # lm: random is actually a fixed blocking term
    formula_str <- sprintf("%s ~ %s + %s", Y,
                           paste(fixed, collapse = " + "),
                           random)
    model_fn <- "lm"
  } else {
    formula_str <- sprintf("%s ~ %s + %s", Y,
                           paste(fixed, collapse = " + "),
                           paste(random, collapse = " + "))
    model_fn <- "lmer"
  }

  # ── warnings
  warnings <- character(0)

  if (!has_rep && case_num == 1 && !block_dep) {
    warnings <- c(warnings,
      "⚠ Even though block is independent, Block1 (shared names) is used due to confounding with Site.")
  }
  if (has_season && is_peren && n_season < 4) {
    warnings <- c(warnings,
      sprintf("⚠ Perennial selected but only %d season(s) detected. Minimum 4 seasons required for reliable random effect estimation. Annual model applied instead.", n_season))
  }
  if (case_num %in% 6:8) {
    warnings <- c(warnings,
      "⚠ Plot variable not used: including Plot would absorb Genotype variance into random effects, removing Genotype significance.")
  }

  list(
    formula_str = formula_str,
    model_fn    = model_fn,
    case_num    = case_num,
    fixed       = fixed,
    random      = random,
    warnings    = warnings,
    peren_valid = peren_valid
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(title = "Agrivoltaics Stat"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("flask")),
      menuItem("About",    tabName = "about",    icon = icon("info-circle"))
    ),
    hr(),
    div(style = "padding:10px;font-size:11px;color:#8b949e;",
        "lme4 · lmerTest · emmeans", br(), "agronomy4future.com")
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      body,.content-wrapper,.main-sidebar,.sidebar{background:#0d1117!important;color:#e6edf3!important}
      .box{background:#161b22!important;border-top-color:#00d4aa!important}
      .box-header{background:#1c2230!important;color:#e6edf3!important}
      .box-title{color:#e6edf3!important;font-family:monospace}
      .nav-tabs-custom>.tab-content{background:#161b22!important}
      .nav-tabs-custom .nav-tabs li.active a{background:#1c2230!important;color:#00d4aa!important;border-top-color:#00d4aa!important}
      .nav-tabs-custom .nav-tabs li a{background:#0d1117!important;color:#8b949e!important}
      table.dataTable{background:#161b22!important;color:#e6edf3!important}
      table.dataTable thead{background:#1c2230!important;color:#8b949e!important}
      .dataTables_wrapper{color:#e6edf3!important}
      .form-control{background:#1c2230!important;color:#e6edf3!important;border-color:#2a3441!important}
      .selectize-input{background:#1c2230!important;color:#e6edf3!important;border-color:#2a3441!important}
      .selectize-dropdown{background:#1c2230!important;color:#e6edf3!important}
      .btn-primary{background:#00d4aa!important;border-color:#00a882!important;color:#000!important;font-weight:bold}
      .btn-default{background:#1c2230!important;border-color:#2a3441!important;color:#e6edf3!important}
      .formula-box{background:#090d12;border:1px solid #2a3441;border-left:3px solid #00d4aa;border-radius:8px;padding:12px 16px;font-family:monospace;font-size:12px;color:#00d4aa;margin-top:10px;word-break:break-all}
      .case-badge{display:inline-block;background:#f0a83220;border:1px solid #f0a832;color:#f0a832;font-size:11px;font-weight:bold;padding:3px 12px;border-radius:20px;margin-top:8px;font-family:monospace}
      .warn-box{background:#ff6b6b18;border:1px solid #ff6b6b55;border-radius:8px;padding:10px 14px;font-size:12px;color:#ff6b6b;margin-top:8px}
      .info-box-custom{background:#58a6ff18;border:1px solid #58a6ff44;border-radius:8px;padding:10px 14px;font-size:12px;color:#8b949e;margin-bottom:14px}
      .section-title{font-size:10px;color:#484f58;text-transform:uppercase;letter-spacing:.1em;font-weight:bold;margin:16px 0 8px;border-bottom:1px solid #2a3441;padding-bottom:4px}
      .skin-black .main-header .logo{background:#0d1117!important;border-bottom:2px solid #00d4aa!important}
      .skin-black .main-header .navbar{background:#0d1117!important}
      .skin-black .main-sidebar{background:#161b22!important}
      pre{background:#090d12!important;color:#8b949e!important;border-color:#2a3441!important;font-size:11px}
      .radio label{color:#e6edf3!important}
    "))),

    tabItems(

      # ── ANALYSIS TAB
      tabItem(tabName = "analysis",

        fluidRow(
          box(width = 12, title = "Step 01 — Upload Field Data", status = "primary",
              div(class = "info-box-custom",
                  icon("info-circle"),
                  " Upload a CSV file with your agrivoltaics field experiment data. ",
                  strong("Y, Site, Block"), " columns are required."),
              fileInput("file", NULL, accept = ".csv",
                        buttonLabel = "Browse CSV...",
                        placeholder  = "No file selected"),
              DTOutput("preview_table")
          )
        ),

        fluidRow(
          box(width = 6, title = "Step 02 — Assign Variables", status = "primary",

              # ── Required
              div(class = "section-title", "Required Variables"),
              fluidRow(
                column(4, selectInput("sel_y",    "Y — Output variable",  choices = c("— select —" = ""))),
                column(4, selectInput("sel_site",  "Site (AV / Control)", choices = c("— select —" = ""))),
                column(4, selectInput("sel_block", "Block",               choices = c("— select —" = "")))
              ),

              # ── Optional
              div(class = "section-title", "Optional Variables"),
              fluidRow(
                column(4, selectInput("sel_genotype", "Genotype", choices = c("— not used —" = "none"))),
                column(4, selectInput("sel_row",      "Row",      choices = c("— not used —" = "none"))),
                column(4, selectInput("sel_season",   "Season",   choices = c("— not used —" = "none")))
              ),
              fluidRow(
                column(4, selectInput("sel_location", "Location", choices = c("— not used —" = "none")))
              ),

              # ── Block structure
              div(class = "section-title", "Block Structure"),
              div(class = "info-box-custom",
                  "Are block names ", strong("shared"), " between AV and Control?",
                  br(),
                  "• Independent: AV uses A/B/C/D, Control uses I/II/III/IV",
                  br(),
                  "• Dependent: Both AV and Control use I/II/III/IV (same names)"),
              radioButtons("block_type", label = NULL,
                           choices  = c("Independent (different names)" = "independent",
                                        "Dependent (shared names)"      = "dependent"),
                           selected = "independent"),

              # ── Replicates
              div(class = "section-title", "Replicates"),
              div(class = "info-box-custom",
                  "Does data have replicates? (more than one observation per Block or Block:Row)"),
              radioButtons("has_replicates", label = NULL,
                           choices  = c("Yes — replicates exist" = "yes",
                                        "No  — one obs per Block/Row" = "no"),
                           selected = "yes"),

              # ── Annual / Perennial (shown only when Season is selected)
              uiOutput("crop_type_ui"),

              hr(),
              div(class = "section-title", "Detected Model Formula"),
              uiOutput("formula_preview"),
              br(),
              actionButton("btn_run", "▶ Run Analysis",
                           class = "btn-primary", style = "width:100%")
          ),

          box(width = 6, title = "Field Layout Diagram", status = "primary",
              uiOutput("layout_diagram")
          )
        ),

        uiOutput("results_ui")
      ),

      # ── ABOUT TAB
      tabItem(tabName = "about",
        box(width = 12, title = "About", status = "primary",
            p("Agrivoltaics Statistics App — Linear Mixed Models for field experiments."),
            p("Developed by JK Kim (agronomy4future.com)"),
            br(),
            p(strong("Key updates in this version:")),
            tags$ul(
              tags$li("Block independent vs dependent structure detection"),
              tags$li("Replicate presence detection"),
              tags$li("Annual vs Perennial crop distinction (Cases 3,4,7,8)"),
              tags$li("Season count auto-detection with warnings"),
              tags$li("Plot variable removed (absorbs Genotype variance)"),
              tags$li("Rank-deficiency fix using * operator for Cases 6,7,8")
            ),
            br(),
            p(strong("8 Layout Cases:")),
            lapply(seq_along(CASE_DESC),
                   function(i) p(paste0("Case ", i, ": ", CASE_DESC[i])))
        )
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(df = NULL)

  # ── Load CSV
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(read_csv(input$file$datapath, show_col_types = FALSE),
                   error = function(e) NULL)
    if (is.null(df)) return()
    rv$df <- as.data.frame(df)
    cols  <- colnames(rv$df)

    req_ch <- setNames(cols, cols)
    opt_ch <- c("— not used —" = "none", setNames(cols, cols))

    updateSelectInput(session, "sel_y",        choices = c("— select —" = "", req_ch))
    updateSelectInput(session, "sel_site",      choices = c("— select —" = "", req_ch))
    updateSelectInput(session, "sel_block",     choices = c("— select —" = "", req_ch))
    updateSelectInput(session, "sel_genotype",  choices = opt_ch)
    updateSelectInput(session, "sel_row",       choices = opt_ch)
    updateSelectInput(session, "sel_season",    choices = opt_ch)
    updateSelectInput(session, "sel_location",  choices = opt_ch)

    # Auto-detect columns
    auto <- list(
      sel_y        = c("yield","output","y","biomass","grain"),
      sel_site     = c("site","treatment","trt"),
      sel_block    = c("block","rep","replicate"),
      sel_genotype = c("genotype","cultivar","variety","cv","geno"),
      sel_row      = c("row","row1"),
      sel_season   = c("season","year"),
      sel_location = c("location","loc","field")
    )
    for (sel_id in names(auto)) {
      for (col in cols) {
        if (any(sapply(auto[[sel_id]], function(k) grepl(k, tolower(col))))) {
          updateSelectInput(session, sel_id, selected = col); break
        }
      }
    }
  })

  # ── Preview table
  output$preview_table <- renderDT({
    req(rv$df)
    datatable(head(rv$df, 5),
              options = list(dom = "t", scrollX = TRUE, pageLength = 5),
              style = "bootstrap", class = "compact")
  })

  # ── Show Annual/Perennial only when Season is selected
  output$crop_type_ui <- renderUI({
    req(input$sel_season)
    if (input$sel_season == "none") return(NULL)
    tagList(
      div(class = "section-title", "Crop Type"),
      div(class = "info-box-custom",
          "Are the same plants measured across seasons?",
          br(),
          "• Annual: new plants each season",
          br(),
          "• Perennial: same plants re-measured (requires ≥ 4 seasons)"),
      radioButtons("crop_type", label = NULL,
                   choices  = c("Annual"    = "annual",
                                "Perennial" = "perennial"),
                   selected = "annual")
    )
  })

  # ── Collect variables
  get_vars <- reactive({
    has_season <- !is.null(input$sel_season) && input$sel_season != "none"

    # count season levels
    n_season <- 0L
    if (has_season && !is.null(rv$df)) {
      n_season <- length(unique(rv$df[[input$sel_season]]))
    }

    # Block1 = same column as block when dependent; keep block for independent
    block1_col <- input$sel_block   # we always pass block col; formula builder decides

    list(
      y           = input$sel_y,
      site        = input$sel_site,
      block       = input$sel_block,
      block1      = block1_col,
      genotype    = if (!is.null(input$sel_genotype)) input$sel_genotype else "none",
      row         = if (!is.null(input$sel_row))      input$sel_row      else "none",
      season      = if (!is.null(input$sel_season))   input$sel_season   else "none",
      location    = if (!is.null(input$sel_location)) input$sel_location else "none",
      block_type  = input$block_type,
      has_replicates = (input$has_replicates == "yes"),
      crop_type   = if (!is.null(input$crop_type)) input$crop_type else "annual",
      n_season    = n_season
    )
  })

  # ── Formula preview
  output$formula_preview <- renderUI({
    v <- get_vars()
    if (!nzchar(v$y) || !nzchar(v$site) || !nzchar(v$block))
      return(div(class = "formula-box", style = "color:#484f58;font-style:italic",
                 "Select Y, Site, and Block to preview the model formula"))

    result <- tryCatch(build_formula(v), error = function(e) NULL)
    if (is.null(result)) return(NULL)

    fn_prefix <- if (result$model_fn == "lm") "lm(" else "lmer("

    warn_tags <- lapply(result$warnings, function(w)
      div(class = "warn-box", w))

    tagList(
      div(class = "formula-box",
          paste0(fn_prefix, result$formula_str, ", data = df)")),
      div(class = "case-badge",
          paste0("📐 Case ", result$case_num,
                 " — ", CASE_DESC[as.character(result$case_num)])),
      if (length(warn_tags)) tagList(warn_tags)
    )
  })

  # ── Layout diagram (reuse previous SVG generator — simplified here)
  output$layout_diagram <- renderUI({
    v <- get_vars()
    if (!nzchar(v$y) || !nzchar(v$site) || !nzchar(v$block))
      return(div(style = "color:#484f58;font-size:12px;padding:20px;text-align:center",
                 "Assign variables to preview the field layout"))
    result <- tryCatch(build_formula(v), error = function(e) NULL)
    if (is.null(result)) return(NULL)

    # Simple text summary when SVG not available
    dep_label <- if (v$block_type == "dependent") "Shared (Dependent)" else "Independent"
    rep_label <- if (v$has_replicates) "Yes" else "No"
    per_label <- if (!is.null(input$sel_season) && input$sel_season != "none") {
      if (v$crop_type == "perennial") {
        if (result$peren_valid) "Perennial ✅" else "Perennial ⚠ (<4 seasons → Annual model)"
      } else "Annual"
    } else "—"

    div(style = "background:#090d12;border-radius:8px;padding:16px;font-family:monospace;font-size:12px;",
        p(style = "color:#00d4aa;font-weight:bold;",
          paste0("Case ", result$case_num, ": ", CASE_DESC[as.character(result$case_num)])),
        tags$table(style = "width:100%;color:#8b949e;",
          tags$tr(tags$td("Block structure:"), tags$td(style="color:#e6edf3", dep_label)),
          tags$tr(tags$td("Replicates:"),      tags$td(style="color:#e6edf3", rep_label)),
          tags$tr(tags$td("Crop type:"),       tags$td(style="color:#e6edf3", per_label)),
          tags$tr(tags$td("Model function:"),  tags$td(style="color:#f0a832", result$model_fn)),
          tags$tr(tags$td("Seasons detected:"),tags$td(style="color:#e6edf3", v$n_season))
        )
    )
  })

  # ── Run Analysis
  analysis_result <- eventReactive(input$btn_run, {
    req(rv$df)
    v <- get_vars()
    req(nzchar(v$y), nzchar(v$site), nzchar(v$block))

    df     <- rv$df
    result <- build_formula(v)

    # Factor conversion
    fac_cols <- unique(c(
      v$site, v$block,
      if (v$genotype != "none") v$genotype,
      if (v$row      != "none") v$row,
      if (v$season   != "none") v$season,
      if (v$location != "none") v$location
    ))
    # Also add block1 if dependent
    if (v$block_type == "dependent") fac_cols <- unique(c(fac_cols, v$block1))

    for (col in fac_cols) {
      if (col %in% colnames(df)) df[[col]] <- as.factor(df[[col]])
    }
    df[[v$y]] <- as.numeric(df[[v$y]])

    formula_obj <- as.formula(result$formula_str)

    # Fit model
    model <- if (result$model_fn == "lm") {
      tryCatch(lm(formula_obj, data = df),
               error = function(e) stop(paste("lm() failed:", e$message)))
    } else {
      tryCatch(lmer(formula_obj, data = df, REML = TRUE),
               error = function(e) stop(paste("lmer() failed:", e$message)))
    }

    # Variance components
    vc_tbl <- if (result$model_fn == "lmer") {
      vc <- as.data.frame(VarCorr(model))
      vc_total <- sum(vc$vcov)
      vc$pct   <- round(vc$vcov / vc_total * 100, 2)
      vc$vcov  <- round(vc$vcov, 3)
      vc[, c("grp", "vcov", "pct")] |>
        setNames(c("Groups", "Variance", "% of Total"))
    } else NULL

    # ANOVA
    anova_tbl <- as.data.frame(
      if (result$model_fn == "lmer") anova(model, type = 3)
      else                           anova(model)
    )
    anova_tbl <- round(anova_tbl, 4)
    p_col <- if ("Pr(>F)" %in% colnames(anova_tbl)) "Pr(>F)" else NULL
    if (!is.null(p_col)) {
      anova_tbl$Sig <- ifelse(anova_tbl[[p_col]] < 0.001, "***",
                       ifelse(anova_tbl[[p_col]] < 0.01,  "**",
                       ifelse(anova_tbl[[p_col]] < 0.05,  "*",
                       ifelse(anova_tbl[[p_col]] < 0.1,   ".", "ns"))))
    }

    # Post-hoc
    fixed_terms       <- unlist(strsplit(paste(result$fixed, collapse = "+"), "\\+"))
    fixed_terms       <- trimws(fixed_terms)
    interaction_terms <- fixed_terms[grepl(":", fixed_terms)]
    posthoc_term      <- if (length(interaction_terms) > 0)
                           interaction_terms[which.max(nchar(interaction_terms))]
                         else v$site

    em      <- tryCatch(emmeans(model, as.formula(paste("~", posthoc_term))),
                        error = function(e) NULL)
    cld_tbl <- NULL
    if (!is.null(em)) {
      cld_tbl <- tryCatch({
        as.data.frame(cld(em, adjust = "sidak", Letters = letters, reverse = TRUE))
      }, error = function(e) as.data.frame(summary(em)))
    }

    list(model        = model,
         vc_tbl       = vc_tbl,
         anova_tbl    = anova_tbl,
         cld_tbl      = cld_tbl,
         posthoc_term = posthoc_term,
         formula_str  = result$formula_str,
         model_fn     = result$model_fn,
         case_num     = result$case_num,
         warnings     = result$warnings,
         v            = v,
         df           = df)
  })

  # ── Results UI
  output$results_ui <- renderUI({
    result <- tryCatch(analysis_result(),
                       error = function(e) {
                         fluidRow(box(width = 12, status = "danger",
                           title = "Analysis Error",
                           div(style = "padding:16px;color:#ff6b6b;font-family:monospace;",
                               p(icon("exclamation-triangle"), strong(e$message)))))
                       })
    req(!is.null(result))
    if (inherits(result, "shiny.tag.list")) return(result)

    warn_tags <- lapply(result$warnings, function(w)
      div(class = "warn-box", w))

    tagList(fluidRow(
      box(width = 12, title = "Step 03 — Analysis Results",
          status = "success",
          div(class = "case-badge",
              paste0("📐 Case ", result$case_num,
                     " — ", CASE_DESC[as.character(result$case_num)])),
          if (length(warn_tags)) tagList(br(), warn_tags),
          br(), br(),
          tabsetPanel(
            if (!is.null(result$vc_tbl))
              tabPanel("Variance Components", br(),
                       DTOutput("tbl_variance"), br(),
                       plotOutput("plot_variance", height = "300px")),
            tabPanel("Type III ANOVA", br(),
                     DTOutput("tbl_anova"), br(),
                     plotOutput("plot_anova", height = "300px")),
            tabPanel("Post-hoc (emmeans)", br(),
                     uiOutput("posthoc_title"),
                     DTOutput("tbl_posthoc"), br(),
                     plotOutput("plot_posthoc", height = "380px")),
            tabPanel("R Code", br(), verbatimTextOutput("r_code")),
            tabPanel("Model Summary", br(), verbatimTextOutput("model_summary"))
          )
      )
    ))
  })

  # ── Tables
  output$tbl_variance <- renderDT({
    req(analysis_result()); req(analysis_result()$vc_tbl)
    datatable(analysis_result()$vc_tbl,
              options = list(dom = "t", pageLength = 20),
              style = "bootstrap", class = "compact", rownames = FALSE)
  })

  output$tbl_anova <- renderDT({
    req(analysis_result())
    datatable(analysis_result()$anova_tbl,
              options = list(dom = "t", pageLength = 20, scrollX = TRUE),
              style = "bootstrap", class = "compact")
  })

  output$posthoc_title <- renderUI({
    req(analysis_result())
    div(style = "font-size:12px;color:#8b949e;margin-bottom:8px;",
        paste("Estimated marginal means for:", analysis_result()$posthoc_term,
              "| Sidak adjustment | CLD letters"))
  })

  output$tbl_posthoc <- renderDT({
    req(analysis_result()); req(analysis_result()$cld_tbl)
    tbl <- analysis_result()$cld_tbl
    tbl <- tbl[order(tbl$emmean, decreasing = TRUE), ]
    tbl <- round_df(tbl)
    datatable(tbl, options = list(dom = "t", pageLength = 30, scrollX = TRUE),
              style = "bootstrap", class = "compact", rownames = FALSE)
  })

  # ── Plots
  output$plot_variance <- renderPlot({
    req(analysis_result()); req(analysis_result()$vc_tbl)
    vc <- analysis_result()$vc_tbl
    vc$Groups <- factor(vc$Groups, levels = rev(vc$Groups))
    ggplot(vc, aes(x = Groups, y = `% of Total`,
                   fill = ifelse(grepl("Residual", Groups), "#ff6b6b", "#00d4aa"))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(round(`% of Total`, 1), "%")),
                hjust = -0.1, color = "#e6edf3", size = 3.5) +
      coord_flip() + scale_fill_identity() +
      scale_y_continuous(limits = c(0, 115)) +
      labs(title = "Variance Component Decomposition", x = NULL, y = "% of Total Variance") +
      theme_minimal(base_family = "mono") +
      theme(plot.background  = element_rect(fill = "#161b22", color = NA),
            panel.background = element_rect(fill = "#161b22", color = NA),
            panel.grid.major = element_line(color = "#2a3441"),
            panel.grid.minor = element_blank(),
            axis.text  = element_text(color = "#8b949e", size = 10),
            axis.title = element_text(color = "#8b949e", size = 10),
            plot.title = element_text(color = "#e6edf3", size = 12, face = "bold"))
  }, bg = "#161b22")

  output$plot_anova <- renderPlot({
    req(analysis_result())
    tbl <- analysis_result()$anova_tbl
    tbl$Term <- rownames(tbl)
    f_col <- if ("F value" %in% colnames(tbl)) "F value" else
             if ("F"       %in% colnames(tbl)) "F"       else NULL
    req(!is.null(f_col))
    tbl$Significant <- if ("Sig" %in% colnames(tbl)) tbl$Sig != "ns" else TRUE
    tbl <- tbl[order(tbl[[f_col]], decreasing = FALSE), ]
    tbl$Term <- factor(tbl$Term, levels = tbl$Term)
    ggplot(tbl, aes(x = Term, y = .data[[f_col]],
                    fill = ifelse(Significant, "#f0a832", "#2a3441"))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(round(.data[[f_col]], 1),
                                   if ("Sig" %in% colnames(tbl)) paste0(" ", Sig) else "")),
                hjust = -0.1, color = "#e6edf3", size = 3.2) +
      coord_flip() + scale_fill_identity() +
      scale_y_continuous(limits = c(0, max(tbl[[f_col]]) * 1.3)) +
      labs(title = "Type III ANOVA — F Values", x = NULL, y = "F value") +
      theme_minimal(base_family = "mono") +
      theme(plot.background  = element_rect(fill = "#161b22", color = NA),
            panel.background = element_rect(fill = "#161b22", color = NA),
            panel.grid.major = element_line(color = "#2a3441"),
            panel.grid.minor = element_blank(),
            axis.text  = element_text(color = "#8b949e", size = 9),
            axis.title = element_text(color = "#8b949e", size = 10),
            plot.title = element_text(color = "#e6edf3", size = 12, face = "bold"))
  }, bg = "#161b22")

  output$plot_posthoc <- renderPlot({
    req(analysis_result()); req(analysis_result()$cld_tbl)
    tbl  <- analysis_result()$cld_tbl
    tbl  <- tbl[order(tbl$emmean, decreasing = TRUE), ]
    v    <- analysis_result()$v
    term_cols <- intersect(strsplit(analysis_result()$posthoc_term, ":")[[1]], colnames(tbl))
    tbl$group_label <- if (length(term_cols) > 0)
      apply(tbl[, term_cols, drop = FALSE], 1, paste, collapse = ":")
    else rownames(tbl)
    tbl$group_label <- factor(tbl$group_label, levels = rev(tbl$group_label))
    tbl$color <- ifelse(grepl("AV|av", tbl$group_label), "#ff6b6b", "#00d4aa")
    se_col  <- if ("SE" %in% colnames(tbl)) "SE" else NULL
    grp_col <- if (".group" %in% colnames(tbl)) ".group" else NULL
    p <- ggplot(tbl, aes(x = group_label, y = emmean, fill = color)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_identity()
    if (!is.null(se_col))
      p <- p + geom_errorbar(aes(ymin = emmean - .data[[se_col]],
                                 ymax = emmean + .data[[se_col]]),
                             width = 0.2, color = "#e6edf3", linewidth = 0.6)
    if (!is.null(grp_col))
      p <- p + geom_text(aes(label = trimws(.data[[grp_col]]),
                             y = emmean + max(emmean) * 0.05),
                         color = "#f0a832", size = 4, fontface = "bold")
    p <- p +
      geom_text(aes(label = round(emmean, 1), y = emmean / 2),
                color = "#0d1117", size = 3.5, fontface = "bold") +
      coord_flip() +
      labs(title    = paste("Post-hoc:", analysis_result()$posthoc_term),
           subtitle = "Error bars = SE  |  Letters = Sidak CLD",
           x = NULL, y = paste("Estimated Mean of", v$y)) +
      theme_minimal(base_family = "mono") +
      theme(plot.background  = element_rect(fill = "#161b22", color = NA),
            panel.background = element_rect(fill = "#161b22", color = NA),
            panel.grid.major = element_line(color = "#2a3441"),
            panel.grid.minor = element_blank(),
            axis.text  = element_text(color = "#8b949e", size = 9),
            axis.title = element_text(color = "#8b949e", size = 10),
            plot.title = element_text(color = "#e6edf3", size = 12, face = "bold"),
            plot.subtitle = element_text(color = "#8b949e", size = 9))
    print(p)
  }, bg = "#161b22")

  # ── R Code output
  output$r_code <- renderText({
    req(analysis_result())
    v   <- analysis_result()$v
    fs  <- analysis_result()$formula_str
    fn  <- analysis_result()$model_fn
    pt  <- analysis_result()$posthoc_term
    warns <- analysis_result()$warnings

    warn_lines <- if (length(warns))
      paste0(paste0("# ⚠ ", warns, collapse = "\n"), "\n\n")
    else ""

    paste0(
      "# ─────────────────────────────────────────────\n",
      "# Agrivoltaics LMM Analysis\n",
      "# Generated by agrivoltaics.agronomy4future.com\n",
      "# ─────────────────────────────────────────────\n\n",
      warn_lines,
      "library(lme4)\nlibrary(lmerTest)\n",
      "library(emmeans)\nlibrary(multcomp)\n",
      "library(multcompView)\nlibrary(ggplot2)\n\n",
      "df <- read.csv('your_data.csv')\n\n",
      "# Factor conversion\n",
      paste0("df$", v$site,  " <- as.factor(df$", v$site,  ")\n"),
      paste0("df$", v$block, " <- as.factor(df$", v$block, ")\n"),
      if (v$genotype != "none") paste0("df$", v$genotype, " <- as.factor(df$", v$genotype, ")\n") else "",
      if (v$row      != "none") paste0("df$", v$row,      " <- as.factor(df$", v$row,      ")\n") else "",
      if (v$season   != "none") paste0("df$", v$season,   " <- as.factor(df$", v$season,   ")\n") else "",
      "\n# Fit model\n",
      paste0("model <- ", fn, "(", fs, ", data = df)\n\n"),
      if (fn == "lmer") "# Variance components\nprint(VarCorr(model), comp='Variance')\n\n" else "",
      "# Type III ANOVA\nprint(anova(model, type = 3))\n\n",
      "# Post-hoc\n",
      paste0("em <- emmeans(model, ~ ", pt, ")\n"),
      "cld_result <- cld(em, adjust='sidak', Letters=letters, reverse=TRUE)\n",
      "print(cld_result)\n"
    )
  })

  output$model_summary <- renderPrint({
    req(analysis_result())
    summary(analysis_result()$model)
  })
}

# ── Helper
round_df <- function(df) {
  for (col in colnames(df))
    if (is.numeric(df[[col]])) df[[col]] <- round(df[[col]], 3)
  df
}

shinyApp(ui = ui, server = server)
