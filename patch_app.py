#!/usr/bin/env python3
"""
patch_app.py
============
Agrivoltaics Shiny App patcher
Applies all modifications to the base app.R on the server.

Usage:
    python3 patch_app.py

Requirements:
    - Base app.R must exist at SRC path
    - Run on the server: root@45.55.159.71
"""

import re
import os

SRC = '/srv/shiny-server/agrivoltaics/app.R'
DST = '/srv/shiny-server/agrivoltaics/app.R'

print("Reading app.R ...")
with open(SRC, 'r') as f:
    content = f.read()
print(f"  {len(content.splitlines())} lines loaded")

# ─────────────────────────────────────────────
# PATCH 1: Add readxl library
# ─────────────────────────────────────────────
old = 'if(!require(readr))        install.packages("readr");        library(readr)\nif(!require(dplyr))        install.packages("dplyr");        library(dplyr)'
new = 'if(!require(readr))        install.packages("readr");        library(readr)\nif(!require(dplyr))        install.packages("dplyr");        library(dplyr)\nif(!require(readxl))       install.packages("readxl");       library(readxl)'
if old in content:
    content = content.replace(old, new, 1)
    print("PATCH 1 OK: readxl added")
else:
    print("PATCH 1 SKIP: readxl already present or pattern not found")

# ─────────────────────────────────────────────
# PATCH 2: Fix fileInput to accept xlsx
# ─────────────────────────────────────────────
old = 'fileInput("file", NULL, accept=".csv",\n                        buttonLabel="Browse CSV...", placeholder="No file selected")'
new = 'fileInput("file", NULL, accept=c(".csv",".xlsx",".xls"),\n                        buttonLabel="Browse CSV/Excel...", placeholder="No file selected")'
if old in content:
    content = content.replace(old, new, 1)
    print("PATCH 2 OK: fileInput accepts xlsx")
else:
    print("PATCH 2 SKIP: already patched or pattern not found")

# ─────────────────────────────────────────────
# PATCH 3: Add sheet selector UI after fileInput
# ─────────────────────────────────────────────
old = '              fileInput("file", NULL, accept=c(".csv",".xlsx",".xls"),\n                        buttonLabel="Browse CSV/Excel...", placeholder="No file selected"),\n              DTOutput("preview_table")))'
new = '              fileInput("file", NULL, accept=c(".csv",".xlsx",".xls"),\n                        buttonLabel="Browse CSV/Excel...", placeholder="No file selected"),\n              uiOutput("sheet_selector_ui"),\n              DTOutput("preview_table")))'
if old in content:
    content = content.replace(old, new, 1)
    print("PATCH 3 OK: sheet selector UI added")
else:
    print("PATCH 3 SKIP: already patched or pattern not found")

# ─────────────────────────────────────────────
# PATCH 4: Fix file upload handler to support xlsx
# ─────────────────────────────────────────────
old = '''  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(read_csv(input$file$datapath, show_col_types=FALSE), error=function(e) NULL)
    if (is.null(df)) return()
    rv$df <- as.data.frame(df)'''
new = '''  observeEvent(input$file, {
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    df <- tryCatch({
      if (ext == "xlsx" || ext == "xls") {
        as.data.frame(readxl::read_excel(input$file$datapath))
      } else {
        as.data.frame(read_csv(input$file$datapath, show_col_types=FALSE))
      }
    }, error=function(e) NULL)
    if (is.null(df)) return()
    rv$df <- df'''
if old in content:
    content = content.replace(old, new, 1)
    print("PATCH 4 OK: xlsx upload handler added")
else:
    print("PATCH 4 SKIP: already patched or pattern not found")

# ─────────────────────────────────────────────
# PATCH 5: Add sheet_selector_ui + observeEvent(sel_sheet)
# ─────────────────────────────────────────────
old = '  output$preview_table <- renderDT({'
new = '''  output$sheet_selector_ui <- renderUI({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error=function(e) NULL)
    if (is.null(sheets)) return(NULL)
    selectInput("sel_sheet", "Select Sheet", choices=sheets, selected=sheets[1])
  })

  observeEvent(input$sel_sheet, {
    req(input$file); req(input$sel_sheet)
    ext <- tolower(tools::file_ext(input$file$name))
    if (!ext %in% c("xlsx","xls")) return()
    df <- tryCatch(
      as.data.frame(readxl::read_excel(input$file$datapath, sheet=input$sel_sheet)),
      error=function(e) NULL)
    if (is.null(df)) return()
    rv$df <- df
    cols <- colnames(rv$df)
    req_ch <- setNames(cols, cols)
    opt_ch <- c("— not used —"="none", setNames(cols, cols))
    updateSelectInput(session, "sel_y",        choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_site",      choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_block",     choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_genotype",  choices=opt_ch)
    updateSelectInput(session, "sel_row",       choices=opt_ch)
    updateSelectInput(session, "sel_season",    choices=opt_ch)
  }, ignoreInit=TRUE)

  output$preview_table <- renderDT({'''
if '  output$preview_table <- renderDT({' in content:
    content = content.replace('  output$preview_table <- renderDT({', new, 1)
    print("PATCH 5 OK: sheet selector server code added")
else:
    print("PATCH 5 SKIP: already patched or pattern not found")

# ─────────────────────────────────────────────
# PATCH 6: Remove auto-select block
# ─────────────────────────────────────────────
auto = '''    auto <- list(
      sel_y        = c("yield","output","y","biomass","grain"),
      sel_site     = c("site","treatment","trt"),
      sel_block    = c("block","rep","replicate"),
      sel_genotype = c("genotype","cultivar","variety","cv","geno"),
      sel_row      = c("row","row1"),
      sel_season   = c("season","year")
    )
    for (sel_id in names(auto))
      for (col in cols)
        if (any(sapply(auto[[sel_id]], function(k) grepl(k, tolower(col))))) {
          updateSelectInput(session, sel_id, selected=col); break
        }'''
if auto in content:
    content = content.replace(auto, '', 1)
    print("PATCH 6 OK: auto-select removed")
else:
    print("PATCH 6 SKIP: already removed or pattern not found")

# ─────────────────────────────────────────────
# PATCH 7: Add Subset UI section in Step 02
# ─────────────────────────────────────────────
old = '              div(class="section-title", "Block Structure"),'
new = '''              div(class="section-title", "Subset Data (Optional)"),
              uiOutput("subset_block_ui"),
              uiOutput("subset_genotype_ui"),
              uiOutput("subset_row_ui"),
              uiOutput("subset_season_ui"),
              uiOutput("subset_preview_text"),
              div(class="section-title", "Block Structure"),'''
if old in content and 'subset_block_ui' not in content:
    content = content.replace(old, new, 1)
    print("PATCH 7 OK: subset UI section added")
else:
    print("PATCH 7 SKIP: already patched or pattern not found")

# ─────────────────────────────────────────────
# PATCH 8: Add subset server code (renderUI + filtered_df + observers)
# ─────────────────────────────────────────────
old = '  get_vars <- reactive({'
new = '''  # Per-variable subset checkboxes
  output$subset_block_ui <- renderUI({
    req(rv$df); req(input$sel_block)
    if (!nzchar(input$sel_block)) return(NULL)
    col <- input$sel_block
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_block", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  output$subset_genotype_ui <- renderUI({
    req(rv$df); req(input$sel_genotype)
    if (is.null(input$sel_genotype) || input$sel_genotype == "none") return(NULL)
    col <- input$sel_genotype
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_genotype", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  output$subset_row_ui <- renderUI({
    req(rv$df); req(input$sel_row)
    if (is.null(input$sel_row) || input$sel_row == "none") return(NULL)
    col <- input$sel_row
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_row", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  output$subset_season_ui <- renderUI({
    req(rv$df); req(input$sel_season)
    if (is.null(input$sel_season) || input$sel_season == "none") return(NULL)
    col <- input$sel_season
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_season", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  filtered_df <- reactive({
    req(rv$df)
    df <- rv$df
    if (!is.null(input$sel_block) && nzchar(input$sel_block) &&
        input$sel_block %in% colnames(df) &&
        !is.null(input$subset_block) && length(input$subset_block) > 0)
      df <- df[as.character(df[[input$sel_block]]) %in% input$subset_block, ]
    if (!is.null(input$sel_genotype) && input$sel_genotype != "none" &&
        input$sel_genotype %in% colnames(df) &&
        !is.null(input$subset_genotype) && length(input$subset_genotype) > 0)
      df <- df[as.character(df[[input$sel_genotype]]) %in% input$subset_genotype, ]
    if (!is.null(input$sel_row) && input$sel_row != "none" &&
        input$sel_row %in% colnames(df) &&
        !is.null(input$subset_row) && length(input$subset_row) > 0)
      df <- df[as.character(df[[input$sel_row]]) %in% input$subset_row, ]
    if (!is.null(input$sel_season) && input$sel_season != "none" &&
        input$sel_season %in% colnames(df) &&
        !is.null(input$subset_season) && length(input$subset_season) > 0)
      df <- df[as.character(df[[input$sel_season]]) %in% input$subset_season, ]
    df
  })

  output$subset_preview_text <- renderUI({
    req(rv$df)
    warn_msgs <- c()
    if (!is.null(input$sel_block) && nzchar(input$sel_block) &&
        !is.null(input$subset_block) && length(input$subset_block) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_block, ": at least 1 value must be selected"))
    if (!is.null(input$sel_genotype) && input$sel_genotype != "none" &&
        !is.null(input$subset_genotype) && length(input$subset_genotype) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_genotype, ": at least 1 value must be selected"))
    if (!is.null(input$sel_row) && input$sel_row != "none" &&
        !is.null(input$subset_row) && length(input$subset_row) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_row, ": at least 1 value must be selected"))
    if (!is.null(input$sel_season) && input$sel_season != "none" &&
        !is.null(input$subset_season) && length(input$subset_season) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_season, ": at least 1 value must be selected"))
    n_orig <- nrow(rv$df)
    n_filt <- nrow(filtered_df())
    tags <- list()
    if (length(warn_msgs) > 0)
      tags <- c(tags, list(div(style="margin-top:6px;padding:8px 12px;background:#fff3cd;border:1px solid #f0a832;border-radius:6px;font-size:13px;color:#856404;",
                               lapply(warn_msgs, function(w) div(w)))))
    if (n_orig != n_filt)
      tags <- c(tags, list(div(style="margin-top:6px;padding:8px 12px;background:#e8f4f8;border-radius:6px;font-size:13px;color:#2c2c2c;",
                               icon("filter"),
                               sprintf("  %d rows selected out of %d total rows", n_filt, n_orig))))
    if (length(tags) == 0) return(NULL)
    tagList(tags)
  })

  # Enforce minimum 1 selection
  observeEvent(input$subset_block, {
    req(input$sel_block); req(nzchar(input$sel_block))
    if (length(input$subset_block) == 0) {
      col <- input$sel_block
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_block", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  observeEvent(input$subset_genotype, {
    req(input$sel_genotype); req(input$sel_genotype != "none")
    if (length(input$subset_genotype) == 0) {
      col <- input$sel_genotype
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_genotype", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  observeEvent(input$subset_row, {
    req(input$sel_row); req(input$sel_row != "none")
    if (length(input$subset_row) == 0) {
      col <- input$sel_row
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_row", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  observeEvent(input$subset_season, {
    req(input$sel_season); req(input$sel_season != "none")
    if (length(input$subset_season) == 0) {
      col <- input$sel_season
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_season", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  get_vars <- reactive({'''

if '  get_vars <- reactive({' in content and 'filtered_df' not in content:
    content = content.replace('  get_vars <- reactive({', new, 1)
    print("PATCH 8 OK: subset server code added")
else:
    print("PATCH 8 SKIP: already patched or pattern not found")

# ─────────────────────────────────────────────
# PATCH 9: Use filtered_df() in analysis_result
# ─────────────────────────────────────────────
old = '    df     <- rv$df\n    result <- build_formula(v)'
new = '    df     <- filtered_df()\n    result <- build_formula(v)'
if old in content:
    content = content.replace(old, new, 1)
    print("PATCH 9 OK: filtered_df() used in analysis")
else:
    print("PATCH 9 SKIP: already patched or pattern not found")

# ─────────────────────────────────────────────
# Write output
# ─────────────────────────────────────────────
with open(DST, 'w') as f:
    f.write(content)

print(f"\nAll patches applied. Total lines: {len(content.splitlines())}")
print("Run: Rscript --vanilla -e \"parse('/srv/shiny-server/agrivoltaics/app.R')\" 2>&1 | head -3")
