if(!require(lme4)) install.packages("lme4")
library(lme4)
if(!require(lmerTest)) install.packages("lmerTest")
library(lmerTest)
if(!require(shiny)) install.packages("shiny")
library(shiny)
if(!require(shinydashboard)) install.packages("shinydashboard")
library(shinydashboard)
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)
if(!require(multcomp)) install.packages("multcomp")
library(multcomp)
if(!require(multcompView)) install.packages("multcompView")
library(multcompView)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(DT)) install.packages("DT")
library(DT)
if(!require(readr)) install.packages("readr")
library(readr)
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)


# ─────────────────────────────────────────────
#  CASE DESCRIPTIONS
# ─────────────────────────────────────────────
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

# ─────────────────────────────────────────────
#  FORMULA BUILDER
# ─────────────────────────────────────────────
build_formula <- function(v) {
  has <- function(k) !is.null(v[[k]]) && v[[k]] != "" && v[[k]] != "none"
  
  fixed <- c(); random <- c(); case_num <- 1
  
  if (!has("genotype") && !has("row") && !has("season") && !has("plot")) {
    case_num <- 1
    fixed  <- c(v$site)
    random <- c(sprintf("(1|%s:%s)", v$site, v$block))
  } else if (!has("genotype") && has("row") && !has("season")) {
    case_num <- 2
    fixed  <- c(v$site, v$row, sprintf("%s:%s", v$site, v$row))
    random <- c(sprintf("(1|%s)", v$block), sprintf("(1|%s:%s)", v$block, v$row))
  } else if (!has("genotype") && !has("row") && has("season")) {
    case_num <- 3
    fixed  <- c(v$season, v$site, sprintf("%s:%s", v$season, v$site))
    random <- c(sprintf("(1|%s)", v$block))
  } else if (!has("genotype") && has("row") && has("season")) {
    case_num <- 4
    fixed  <- c(v$season, v$site, v$row,
                sprintf("%s:%s", v$season, v$site),
                sprintf("%s:%s", v$season, v$row),
                sprintf("%s:%s:%s", v$season, v$site, v$row))
    random <- c(sprintf("(1|%s)", v$block),
                sprintf("(1|%s:%s)", v$block, v$season),
                sprintf("(1|%s:%s)", v$block, v$row))
  } else if (has("genotype") && !has("row") && !has("season")) {
    case_num <- 5
    fixed  <- c(v$site, v$genotype, sprintf("%s:%s", v$site, v$genotype))
    random <- c(sprintf("(1|%s)", v$block))
  } else if (has("genotype") && has("row") && !has("season")) {
    case_num <- 6
    fixed  <- c(v$site, v$genotype, v$row,
                sprintf("%s:%s:%s", v$site, v$genotype, v$row))
    random <- if (has("plot"))
      c(sprintf("(1|%s)", v$block), sprintf("(1|%s)", v$plot))
    else
      c(sprintf("(1|%s)", v$block))
  } else if (has("genotype") && !has("row") && has("season")) {
    case_num <- 7
    fixed  <- c(v$site, v$genotype, v$season,
                sprintf("%s:%s:%s", v$site, v$genotype, v$season))
    random <- if (has("plot"))
      c(sprintf("(1|%s)", v$block),
        sprintf("(1|%s:%s)", v$block, v$season),
        sprintf("(1|%s)", v$plot))
    else
      c(sprintf("(1|%s)", v$block),
        sprintf("(1|%s:%s)", v$block, v$season))
  } else {
    case_num <- 8
    fixed  <- c(v$site, v$genotype, v$season, v$row,
                sprintf("%s:%s:%s", v$site, v$genotype, v$season),
                sprintf("%s:%s:%s:%s", v$site, v$genotype, v$season, v$row))
    random <- if (has("plot"))
      c(sprintf("(1|%s)", v$block),
        sprintf("(1|%s)", v$plot),
        sprintf("(1|%s:%s)", v$block, v$season),
        sprintf("(1|%s:%s)", v$plot, v$season),
        sprintf("(1|%s:%s)", v$plot, v$row))
    else
      c(sprintf("(1|%s)", v$block),
        sprintf("(1|%s:%s)", v$block, v$season),
        sprintf("(1|%s:%s)", v$block, v$row))
  }
  
  if (has("location")) {
    fixed  <- c(fixed, v$location)
    random <- c(random, sprintf("(1|%s:%s)", v$location, v$block))
  }
  
  formula_str <- sprintf("%s ~ %s", v$y, paste(c(fixed, random), collapse = " + "))
  list(formula_str = formula_str, case_num = case_num, fixed = fixed, random = random)
}

# ─────────────────────────────────────────────
#  LAYOUT SVG GENERATOR
# ─────────────────────────────────────────────
get_layout_svg <- function(case_num, v) {
  has <- function(k) !is.null(v[[k]]) && v[[k]] != "" && v[[k]] != "none"
  
  site_l    <- if(has("site"))     v$site     else "Site"
  block_l   <- if(has("block"))    v$block    else "Block"
  geno_l    <- if(has("genotype")) v$genotype else "Genotype"
  row_l     <- if(has("row"))      v$row      else "Row"
  season_l  <- if(has("season"))   v$season   else "Season"
  y_l       <- if(has("y"))        v$y        else "Y"

  svgs <- list(
    "1" = sprintf('
<svg viewBox="0 0 520 210" xmlns="http://www.w3.org/2000/svg" style="max-width:100%%;font-family:monospace">
  <text x="260" y="16" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Single Row</text>
  <rect x="18" y="26" width="200" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="118" y="36" text-anchor="middle" fill="#f0a832" font-size="8" font-weight="bold">☀ Solar Panels (AV)</text>
  <rect x="18" y="44" width="90" height="56" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="63" y="62" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV Site</text>
  <text x="63" y="78" text-anchor="middle" fill="#8b949e" font-size="8">%s=AV</text>
  <rect x="118" y="44" width="90" height="56" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="163" y="62" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <text x="163" y="78" text-anchor="middle" fill="#8b949e" font-size="8">%s=Control</text>
  <line x1="10" y1="44" x2="10" y2="100" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="44" x2="16" y2="44" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="100" x2="16" y2="100" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="75" text-anchor="end" fill="#8b949e" font-size="8">%s-I</text>
  <rect x="18" y="108" width="90" height="56" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="63" y="130" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV Site</text>
  <rect x="118" y="108" width="90" height="56" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="163" y="130" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <line x1="10" y1="108" x2="10" y2="164" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="108" x2="16" y2="108" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="164" x2="16" y2="164" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="139" text-anchor="end" fill="#8b949e" font-size="8">%s-II</text>
  <rect x="240" y="44" width="260" height="50" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="370" y="62" text-anchor="middle" fill="#8b949e" font-size="9">Random effect:</text>
  <text x="370" y="78" text-anchor="middle" fill="#00d4aa" font-size="10" font-weight="bold">(1 | %s:%s)</text>
  <rect x="240" y="104" width="260" height="34" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="370" y="118" text-anchor="middle" fill="#8b949e" font-size="9">Fixed effect:</text>
  <text x="370" y="132" text-anchor="middle" fill="#ff6b6b" font-size="10" font-weight="bold">%s</text>
  <rect x="240" y="148" width="260" height="36" rx="5" fill="#f0a83220" stroke="#f0a832" stroke-width="1"/>
  <text x="370" y="163" text-anchor="middle" fill="#f0a832" font-size="9">⚠ Treatment confounded with space</text>
  <text x="370" y="177" text-anchor="middle" fill="#f0a832" font-size="9">→ LMM accounts for this</text>
</svg>', site_l, site_l, block_l, block_l, site_l, block_l, site_l),

    "2" = sprintf('
<svg viewBox="0 0 540 220" xmlns="http://www.w3.org/2000/svg" style="max-width:100%%;font-family:monospace">
  <text x="270" y="15" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Multiple Rows Within Plot</text>
  <rect x="18" y="24" width="200" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="118" y="34" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels</text>
  <rect x="18" y="42" width="200" height="86" rx="6" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="118" y="56" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV Site</text>
  <rect x="22" y="60" width="192" height="22" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="118" y="75" text-anchor="middle" fill="#58a6ff" font-size="8">%s = Side1</text>
  <rect x="22" y="84" width="192" height="22" rx="3" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <text x="118" y="99" text-anchor="middle" fill="#00d4aa" font-size="8">%s = Middle</text>
  <rect x="22" y="108" width="192" height="16" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="118" y="120" text-anchor="middle" fill="#58a6ff" font-size="8">%s = Side2</text>
  <rect x="228" y="42" width="200" height="86" rx="6" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="328" y="56" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control Site</text>
  <rect x="232" y="60" width="192" height="22" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="328" y="75" text-anchor="middle" fill="#58a6ff" font-size="8">%s = Side1</text>
  <rect x="232" y="84" width="192" height="22" rx="3" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <text x="328" y="99" text-anchor="middle" fill="#00d4aa" font-size="8">%s = Middle</text>
  <rect x="232" y="108" width="192" height="16" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="328" y="120" text-anchor="middle" fill="#58a6ff" font-size="8">%s = Side2</text>
  <line x1="8" y1="42" x2="8" y2="128" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="42" x2="14" y2="42" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="128" x2="14" y2="128" stroke="#8b949e" stroke-width="1.5"/>
  <text x="4" y="88" text-anchor="end" fill="#8b949e" font-size="8">%s</text>
  <text x="18" y="150" fill="#8b949e" font-size="9">Random: (1|%s) + (1|%s:%s)</text>
  <text x="18" y="164" fill="#8b949e" font-size="9">Fixed:  %s + %s + %s:%s</text>
  <rect x="18" y="174" width="500" height="26" rx="5" fill="#58a6ff18" stroke="#58a6ff" stroke-width="1"/>
  <text x="268" y="189" text-anchor="middle" fill="#58a6ff" font-size="9">Row position captures shading gradient under solar panels</text>
</svg>', row_l, row_l, row_l, row_l, row_l, row_l, block_l,
    block_l, block_l, row_l, site_l, row_l, site_l, row_l),

    "3" = sprintf('
<svg viewBox="0 0 540 210" xmlns="http://www.w3.org/2000/svg" style="max-width:100%%;font-family:monospace">
  <text x="270" y="14" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Single Row · Multiple Seasons</text>
  <rect x="18" y="22" width="230" height="13" rx="4" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="133" y="33" text-anchor="middle" fill="#3fb950" font-size="8" font-weight="bold">%s = 1</text>
  <rect x="18" y="40" width="100" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="68" y="50" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels</text>
  <rect x="18" y="56" width="100" height="54" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="68" y="76" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV</text>
  <rect x="128" y="56" width="100" height="54" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="178" y="76" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <line x1="8" y1="56" x2="8" y2="110" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="56" x2="14" y2="56" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="110" x2="14" y2="110" stroke="#8b949e" stroke-width="1.5"/>
  <text x="4" y="86" text-anchor="end" fill="#8b949e" font-size="8">%s</text>
  <line x1="256" y1="83" x2="278" y2="83" stroke="#3fb950" stroke-width="2" stroke-dasharray="4,3"/>
  <text x="267" y="79" text-anchor="middle" fill="#3fb950" font-size="9">→</text>
  <rect x="290" y="22" width="230" height="13" rx="4" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="405" y="33" text-anchor="middle" fill="#3fb950" font-size="8" font-weight="bold">%s = 2</text>
  <rect x="290" y="40" width="100" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="340" y="50" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels</text>
  <rect x="290" y="56" width="100" height="54" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="340" y="76" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV</text>
  <rect x="400" y="56" width="100" height="54" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="450" y="76" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <line x1="280" y1="56" x2="280" y2="110" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="280" y1="56" x2="286" y2="56" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="280" y1="110" x2="286" y2="110" stroke="#8b949e" stroke-width="1.5"/>
  <text x="276" y="86" text-anchor="end" fill="#8b949e" font-size="8">%s</text>
  <text x="18" y="130" fill="#8b949e" font-size="9">Random: (1|%s)</text>
  <text x="18" y="144" fill="#8b949e" font-size="9">Fixed:  %s + %s + %s:%s</text>
  <rect x="18" y="154" width="500" height="36" rx="6" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="268" y="169" text-anchor="middle" fill="#3fb950" font-size="9">Same plots measured across seasons</text>
  <text x="268" y="183" text-anchor="middle" fill="#3fb950" font-size="9">Block random effect absorbs spatial heterogeneity</text>
</svg>', season_l, block_l, season_l, block_l,
    block_l, season_l, site_l, season_l, site_l),

    "5" = sprintf('
<svg viewBox="0 0 560 220" xmlns="http://www.w3.org/2000/svg" style="max-width:100%%;font-family:monospace">
  <text x="280" y="14" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Multiple Cultivars · Single Row</text>
  <rect x="18" y="22" width="240" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="138" y="32" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels (AV)</text>
  <rect x="18" y="40" width="240" height="80" rx="6" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="138" y="56" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV Site</text>
  <rect x="26" y="62" width="106" height="50" rx="5" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1.2"/>
  <text x="79" y="82" text-anchor="middle" fill="#bc8cff" font-size="9" font-weight="bold">%s = cv1</text>
  <text x="79" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Block I~IV</text>
  <rect x="140" y="62" width="106" height="50" rx="5" fill="#f0a83233" stroke="#f0a832" stroke-width="1.2"/>
  <text x="193" y="82" text-anchor="middle" fill="#f0a832" font-size="9" font-weight="bold">%s = cv2</text>
  <text x="193" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Block I~IV</text>
  <rect x="296" y="40" width="240" height="80" rx="6" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="416" y="56" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control Site</text>
  <rect x="304" y="62" width="106" height="50" rx="5" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1.2"/>
  <text x="357" y="82" text-anchor="middle" fill="#bc8cff" font-size="9" font-weight="bold">%s = cv1</text>
  <text x="357" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Block I~IV</text>
  <rect x="418" y="62" width="106" height="50" rx="5" fill="#f0a83233" stroke="#f0a832" stroke-width="1.2"/>
  <text x="471" y="82" text-anchor="middle" fill="#f0a832" font-size="9" font-weight="bold">%s = cv2</text>
  <text x="471" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Block I~IV</text>
  <line x1="8" y1="40" x2="8" y2="120" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="40" x2="14" y2="40" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="120" x2="14" y2="120" stroke="#8b949e" stroke-width="1.5"/>
  <text x="4" y="83" text-anchor="end" fill="#8b949e" font-size="8">%s</text>
  <rect x="18" y="132" width="520" height="30" rx="6" fill="#bc8cff18" stroke="#bc8cff" stroke-width="1"/>
  <text x="278" y="146" text-anchor="middle" fill="#bc8cff" font-size="9" font-weight="bold">Key: %s × %s interaction</text>
  <text x="278" y="158" text-anchor="middle" fill="#bc8cff" font-size="9">Tests whether cultivars respond differently to AV shading</text>
  <text x="18" y="180" fill="#8b949e" font-size="9">Random: (1|%s)   Fixed: %s + %s + %s:%s</text>
</svg>', geno_l, geno_l, geno_l, geno_l, block_l,
    site_l, geno_l, block_l, site_l, geno_l, site_l, geno_l),

    "8" = sprintf('
<svg viewBox="0 0 580 250" xmlns="http://www.w3.org/2000/svg" style="max-width:100%%;font-family:monospace">
  <text x="290" y="13" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Multiple Cultivars · Multiple Rows · Multiple Seasons</text>
  <rect x="16" y="20" width="262" height="12" rx="3" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="147" y="30" text-anchor="middle" fill="#3fb950" font-size="8">%s = 1</text>
  <rect x="16" y="36" width="120" height="100" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.2"/>
  <text x="76" y="50" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <rect x="22" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="48" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="26" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="26" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="26" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="78" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="104" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <rect x="82" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="82" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="82" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="144" y="36" width="120" height="100" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.2"/>
  <text x="204" y="50" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <rect x="150" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="176" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="154" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="154" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="154" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="206" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="232" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <rect x="210" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="210" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="210" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <line x1="256" y1="86" x2="296" y2="86" stroke="#3fb950" stroke-width="2" stroke-dasharray="4,3"/>
  <rect x="298" y="20" width="262" height="12" rx="3" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="429" y="30" text-anchor="middle" fill="#3fb950" font-size="8">%s = 2</text>
  <rect x="298" y="36" width="120" height="100" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.2"/>
  <text x="358" y="50" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <rect x="304" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="330" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="308" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="308" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="308" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="360" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="386" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <rect x="426" y="36" width="120" height="100" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.2"/>
  <text x="486" y="50" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <rect x="432" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="458" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="488" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="514" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <text x="16" y="152" fill="#8b949e" font-size="8">Random: (1|%s) + (1|Plot) + (1|%s:%s) + (1|Plot:%s) + (1|Plot:%s)</text>
  <text x="16" y="165" fill="#8b949e" font-size="8">Fixed:  %s + %s + %s + %s + %s:%s:%s + %s:%s:%s:%s</text>
  <rect x="16" y="174" width="544" height="44" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="288" y="190" text-anchor="middle" fill="#e6edf3" font-size="9" font-weight="bold">Most complex layout — full random effect structure</text>
  <text x="288" y="204" text-anchor="middle" fill="#8b949e" font-size="9">Plot:Row captures within-plot spatial variation</text>
  <text x="288" y="216" text-anchor="middle" fill="#8b949e" font-size="9">Plot:Season captures temporal re-measurement correlation</text>
</svg>', season_l, season_l,
    block_l, block_l, season_l, season_l, row_l,
    site_l, geno_l, season_l, row_l,
    site_l, geno_l, season_l, site_l, geno_l, season_l, row_l)
  )
  
  # Cases 4, 6, 7 use simplified versions of nearby cases
  if (case_num == 4) return(svgs[["2"]])
  if (case_num == 6) return(svgs[["5"]])
  if (case_num == 7) return(svgs[["3"]])
  
  svg <- svgs[[as.character(case_num)]]
  if (is.null(svg)) return("<p style='color:#8b949e'>Layout diagram not available</p>")
  svg
}

# ─────────────────────────────────────────────
#  UI
# ─────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Agrivoltaics Stat"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("flask")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    hr(),
    div(style = "padding: 10px; font-size: 11px; color: #8b949e;",
        "lme4 · lmerTest · emmeans", br(),
        "agronomy4future.com")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body, .content-wrapper, .main-sidebar, .sidebar {
        background-color: #0d1117 !important;
        color: #e6edf3 !important;
      }
      .box { background: #161b22 !important; border-top-color: #00d4aa !important; }
      .box-header { background: #1c2230 !important; color: #e6edf3 !important; }
      .box-title { color: #e6edf3 !important; font-family: monospace; }
      .nav-tabs-custom > .tab-content { background: #161b22 !important; }
      .nav-tabs-custom .nav-tabs li.active a { background: #1c2230 !important; color: #00d4aa !important; border-top-color: #00d4aa !important; }
      .nav-tabs-custom .nav-tabs li a { background: #0d1117 !important; color: #8b949e !important; }
      table.dataTable { background: #161b22 !important; color: #e6edf3 !important; }
      table.dataTable thead { background: #1c2230 !important; color: #8b949e !important; }
      .dataTables_wrapper { color: #e6edf3 !important; }
      .form-control { background: #1c2230 !important; color: #e6edf3 !important; border-color: #2a3441 !important; }
      .selectize-input { background: #1c2230 !important; color: #e6edf3 !important; border-color: #2a3441 !important; }
      .selectize-dropdown { background: #1c2230 !important; color: #e6edf3 !important; }
      .btn-primary { background: #00d4aa !important; border-color: #00a882 !important; color: #000 !important; font-weight: bold; }
      .btn-default { background: #1c2230 !important; border-color: #2a3441 !important; color: #e6edf3 !important; }
      .formula-box { background: #090d12; border: 1px solid #2a3441; border-left: 3px solid #00d4aa; border-radius: 8px; padding: 12px 16px; font-family: monospace; font-size: 12px; color: #00d4aa; margin-top: 10px; word-break: break-all; }
      .case-badge { display: inline-block; background: #f0a83220; border: 1px solid #f0a832; color: #f0a832; font-size: 11px; font-weight: bold; padding: 3px 12px; border-radius: 20px; margin-top: 8px; font-family: monospace; }
      .section-title { font-size: 10px; color: #484f58; text-transform: uppercase; letter-spacing: 0.1em; font-weight: bold; margin: 16px 0 8px; border-bottom: 1px solid #2a3441; padding-bottom: 4px; }
      .info-notice { background: #58a6ff18; border: 1px solid #58a6ff44; border-radius: 8px; padding: 10px 14px; font-size: 12px; color: #8b949e; margin-bottom: 14px; }
      .skin-black .main-header .logo { background: #0d1117 !important; border-bottom: 2px solid #00d4aa !important; }
      .skin-black .main-header .navbar { background: #0d1117 !important; }
      .skin-black .main-sidebar { background: #161b22 !important; }
      pre { background: #090d12 !important; color: #8b949e !important; border-color: #2a3441 !important; font-size: 11px; }
    "))),
    
    tabItems(
      tabItem(tabName = "analysis",
        fluidRow(
          # ── STEP 1: UPLOAD
          box(width = 12, title = "Step 01 — Upload Field Data", status = "primary", solidHeader = FALSE,
              div(class = "info-notice",
                  icon("info-circle"), " Upload a CSV file containing your agrivoltaics field experiment data. ",
                  strong("Block"), " and ", strong("Site (AV/Control)"), " columns are required."),
              fileInput("file", NULL, accept = ".csv",
                        buttonLabel = "Browse CSV...",
                        placeholder = "No file selected"),
              DTOutput("preview_table")
          )
        ),
        
        fluidRow(
          # ── STEP 2: VARIABLES
          box(width = 6, title = "Step 02 — Assign Variables", status = "primary",
              div(class = "section-title", "Required Variables"),
              fluidRow(
                column(4, selectInput("sel_y",    "Y — Output variable", choices = c("— select —" = ""))),
                column(4, selectInput("sel_site",  "Site (AV/Control)",  choices = c("— select —" = ""))),
                column(4, selectInput("sel_block", "Block",              choices = c("— select —" = "")))
              ),
              div(class = "section-title", "Optional Variables"),
              fluidRow(
                column(4, selectInput("sel_genotype", "Genotype",  choices = c("— not used —" = "none"))),
                column(4, selectInput("sel_row",      "Row",       choices = c("— not used —" = "none"))),
                column(4, selectInput("sel_season",   "Season",    choices = c("— not used —" = "none")))
              ),
              fluidRow(
                column(4, selectInput("sel_plot",     "Plot",      choices = c("— not used —" = "none"))),
                column(4, selectInput("sel_location", "Location",  choices = c("— not used —" = "none")))
              ),
              hr(),
              div(class = "section-title", "Detected Model Formula"),
              uiOutput("formula_preview"),
              br(),
              actionButton("btn_run", "▶  Run Analysis", class = "btn-primary", style = "width:100%")
          ),
          
          # ── LAYOUT DIAGRAM
          box(width = 6, title = "Field Layout Diagram", status = "primary",
              uiOutput("layout_diagram")
          )
        ),
        
        # ── STEP 3: RESULTS
        uiOutput("results_ui")
      ),
      
      tabItem(tabName = "about",
        box(width = 12, title = "About", status = "primary",
            p("This app implements Linear Mixed Models for Agrivoltaics field experiments,"),
            p("following the statistical framework developed by JK Kim (agronomy4future.com)."),
            br(),
            p(strong("Reference:")),
            p(a("Statistical Models in Agrivoltaics (agronomy4future.com)",
                href = "https://agronomy4future.com/archives/24404", target = "_blank")),
            br(),
            p(strong("R Packages:")),
            p("lme4, lmerTest, emmeans, multcomp, multcompView, ggplot2"),
            br(),
            p(strong("8 Layout Cases supported:")),
            lapply(seq_along(CASE_DESC), function(i) p(paste0("Case ", i, ": ", CASE_DESC[i])))
        )
      )
    )
  )
)

# ─────────────────────────────────────────────
#  SERVER
# ─────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Reactive: loaded data
  rv <- reactiveValues(df = NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(read_csv(input$file$datapath, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(df)) return()
    rv$df <- as.data.frame(df)
    
    cols <- colnames(rv$df)
    req_choices <- setNames(cols, cols)
    opt_choices <- c("— not used —" = "none", setNames(cols, cols))
    
    updateSelectInput(session, "sel_y",        choices = c("— select —" = "", req_choices))
    updateSelectInput(session, "sel_site",      choices = c("— select —" = "", req_choices))
    updateSelectInput(session, "sel_block",     choices = c("— select —" = "", req_choices))
    updateSelectInput(session, "sel_genotype",  choices = opt_choices)
    updateSelectInput(session, "sel_row",       choices = opt_choices)
    updateSelectInput(session, "sel_season",    choices = opt_choices)
    updateSelectInput(session, "sel_plot",      choices = opt_choices)
    updateSelectInput(session, "sel_location",  choices = opt_choices)
    
    # Auto-detect
    auto <- list(
      sel_y        = c("yield","output","y","biomass","grain"),
      sel_site     = c("site","av_site","treatment","trt"),
      sel_block    = c("block","rep","replicate"),
      sel_genotype = c("genotype","cultivar","variety","cv","geno"),
      sel_row      = c("row","rows"),
      sel_season   = c("season","year"),
      sel_plot     = c("plot"),
      sel_location = c("location","loc","field")
    )
    for (sel_id in names(auto)) {
      for (col in cols) {
        if (any(sapply(auto[[sel_id]], function(k) grepl(k, tolower(col))))) {
          updateSelectInput(session, sel_id, selected = col)
          break
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
  
  # ── Get vars helper
  get_vars <- reactive({
    list(
      y        = input$sel_y,
      site     = input$sel_site,
      block    = input$sel_block,
      genotype = input$sel_genotype,
      row      = input$sel_row,
      season   = input$sel_season,
      plot     = input$sel_plot,
      location = input$sel_location
    )
  })
  
  # ── Formula preview
  output$formula_preview <- renderUI({
    v <- get_vars()
    if (v$y == "" || v$site == "" || v$block == "") {
      return(div(class = "formula-box", style = "color:#484f58; font-style:italic",
                 "Select Y, Site, and Block to preview the model formula"))
    }
    result <- tryCatch(build_formula(v), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    tagList(
      div(class = "formula-box", paste0("lmer(", result$formula_str, ", data = df)")),
      div(class = "case-badge", paste0("📐 Case ", result$case_num, " — ", CASE_DESC[as.character(result$case_num)]))
    )
  })
  
  # ── Layout diagram
  output$layout_diagram <- renderUI({
    v <- get_vars()
    if (v$y == "" || v$site == "" || v$block == "") {
      return(div(style = "color:#484f58; font-size:12px; padding:20px; text-align:center",
                 "Assign variables to preview the field layout diagram"))
    }
    result <- tryCatch(build_formula(v), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    svg_html <- get_layout_svg(result$case_num, v)
    tagList(
      div(style = "background:#090d12; border-radius:8px; padding:12px;",
          HTML(svg_html))
    )
  })
  
  # ── Run analysis
  analysis_result <- eventReactive(input$btn_run, {
    req(rv$df)
    v <- get_vars()
    req(v$y != "", v$site != "", v$block != "")
    
    df <- rv$df
    
    # Factor conversion
    # ── Factor level validation
    check_cols <- Filter(Negate(is.null), list(
      v$site, v$block,
      if(v$genotype != "none") v$genotype else NULL,
      if(v$row      != "none") v$row      else NULL,
      if(v$season   != "none") v$season   else NULL,
      if(v$plot     != "none") v$plot     else NULL,
      if(v$location != "none") v$location else NULL
    ))
    for (col in check_cols) {
      n_levels <- length(unique(na.omit(df[[col]])))
      if (n_levels < 2) {
        stop(paste0("Variable '", col, "' has only ", n_levels, " unique value. ",
                    "Each variable must have at least 2 levels. ",
                    "Please check your column assignments."))
      }
    }

    fac_cols <- c(v$site, v$block,
                  if(v$genotype != "none") v$genotype,
                  if(v$row      != "none") v$row,
                  if(v$season   != "none") v$season,
                  if(v$plot     != "none") v$plot,
                  if(v$location != "none") v$location)
    for (col in fac_cols) df[[col]] <- as.factor(df[[col]])
    df[[v$y]] <- as.numeric(df[[v$y]])
    
    result <- build_formula(v)
    formula_obj <- as.formula(result$formula_str)
    
    # Fit model
    model <- tryCatch(
      lmer(formula_obj, data = df, REML = TRUE),
      error = function(e) stop(paste("Model fitting failed:", e$message))
    )
    
    # Variance components
    vc <- as.data.frame(VarCorr(model))
    vc_total <- sum(vc$vcov)
    vc$pct <- round(vc$vcov / vc_total * 100, 2)
    vc$vcov <- round(vc$vcov, 3)
    colnames(vc)[colnames(vc) == "grp"]  <- "Groups"
    colnames(vc)[colnames(vc) == "vcov"] <- "Variance"
    vc <- vc[, c("Groups", "Variance", "pct")]
    colnames(vc)[3] <- "% of Total"
    
    # ANOVA
    anova_tbl <- as.data.frame(anova(model, type = 3))
    anova_tbl <- round(anova_tbl, 4)
    anova_tbl$Sig <- ifelse(anova_tbl[["Pr(>F)"]] < 0.001, "***",
                     ifelse(anova_tbl[["Pr(>F)"]] < 0.01,  "**",
                     ifelse(anova_tbl[["Pr(>F)"]] < 0.05,  "*",
                     ifelse(anova_tbl[["Pr(>F)"]] < 0.1,   ".", "ns"))))
    
    # Post-hoc: pick most relevant term
    fixed_terms <- result$fixed
    interaction_terms <- fixed_terms[grepl(":", fixed_terms)]
    posthoc_term <- if (length(interaction_terms) > 0) {
      interaction_terms[which.max(nchar(interaction_terms))]
    } else {
      v$site
    }
    posthoc_formula <- as.formula(paste("~", posthoc_term))
    em <- tryCatch(emmeans(model, posthoc_formula), error = function(e) NULL)
    
    cld_tbl <- NULL
    if (!is.null(em)) {
      cld_tbl <- tryCatch({
        cld_res <- cld(em, adjust = "sidak", Letters = letters, reverse = TRUE)
        as.data.frame(cld_res)
      }, error = function(e) as.data.frame(summary(em)))
    }
    
    list(
      model       = model,
      vc          = vc,
      anova_tbl   = anova_tbl,
      cld_tbl     = cld_tbl,
      posthoc_term= posthoc_term,
      formula_str = result$formula_str,
      case_num    = result$case_num,
      v           = v,
      df          = df
    )
  })
  
  # ── Results UI
  output$results_ui <- renderUI({
    result <- tryCatch(
      analysis_result(),
      error = function(e) {
        return(tagList(fluidRow(
          box(width = 12, status = "danger", title = "Analysis Error",
              div(style = "padding:16px;",
                  p(icon("exclamation-triangle"),
                    style = "color:#ff6b6b; font-size:14px; font-family:monospace;",
                    strong(e$message)),
                  br(),
                  p(style = "color:#8b949e; font-size:12px;",
                    "Please check your variable assignments and ensure each variable has at least 2 unique levels.")
              )
          )
        )))
      }
    )
    req(!is.null(result))
    if (inherits(result, "shiny.tag.list")) return(result)
    
    tagList(
      fluidRow(
        box(width = 12, title = "Step 03 — Analysis Results", status = "success", solidHeader = FALSE,
            div(class = "case-badge",
                paste0("📐 Case ", analysis_result()$case_num, " — ",
                       CASE_DESC[as.character(analysis_result()$case_num)])),
            br(), br(),
            tabsetPanel(
              tabPanel("Variance Components",
                       br(),
                       DTOutput("tbl_variance"),
                       br(),
                       plotOutput("plot_variance", height = "300px")),
              tabPanel("Type III ANOVA",
                       br(),
                       DTOutput("tbl_anova"),
                       br(),
                       plotOutput("plot_anova", height = "300px")),
              tabPanel("Post-hoc (emmeans)",
                       br(),
                       uiOutput("posthoc_title"),
                       DTOutput("tbl_posthoc"),
                       br(),
                       plotOutput("plot_posthoc", height = "380px")),
              tabPanel("R Code",
                       br(),
                       verbatimTextOutput("r_code")),
              tabPanel("Model Summary",
                       br(),
                       verbatimTextOutput("model_summary"))
            )
        )
      )
    )
  })
  
  # ── Variance table
  output$tbl_variance <- renderDT({
    req(analysis_result())
    datatable(analysis_result()$vc,
              options = list(dom = "t", pageLength = 20),
              style = "bootstrap", class = "compact", rownames = FALSE)
  })
  
  # ── ANOVA table
  output$tbl_anova <- renderDT({
    req(analysis_result())
    datatable(analysis_result()$anova_tbl,
              options = list(dom = "t", pageLength = 20, scrollX = TRUE),
              style = "bootstrap", class = "compact")
  })
  
  # ── Post-hoc title
  output$posthoc_title <- renderUI({
    req(analysis_result())
    div(style = "font-size:12px; color:#8b949e; margin-bottom:8px;",
        paste("Estimated marginal means for:", analysis_result()$posthoc_term,
              "| Sidak adjustment | CLD letters"))
  })
  
  # ── Post-hoc table
  output$tbl_posthoc <- renderDT({
    req(analysis_result())
    req(analysis_result()$cld_tbl)
    tbl <- analysis_result()$cld_tbl
    tbl <- tbl[order(tbl$emmean, decreasing = TRUE), ]
    tbl <- round_df(tbl)
    datatable(tbl,
              options = list(dom = "t", pageLength = 30, scrollX = TRUE),
              style = "bootstrap", class = "compact", rownames = FALSE)
  })
  
  # ── Variance plot
  output$plot_variance <- renderPlot({
    req(analysis_result())
    vc <- analysis_result()$vc
    vc$Groups <- factor(vc$Groups, levels = rev(vc$Groups))
    ggplot(vc, aes(x = Groups, y = `% of Total`,
                   fill = ifelse(grepl("Residual", Groups), "#ff6b6b", "#00d4aa"))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(round(`% of Total`, 1), "%")),
                hjust = -0.1, color = "#e6edf3", size = 3.5) +
      coord_flip() +
      scale_fill_identity() +
      scale_y_continuous(limits = c(0, 115)) +
      labs(title = "Variance Component Decomposition",
           x = NULL, y = "% of Total Variance") +
      theme_minimal(base_family = "mono") +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major = element_line(color = "#2a3441"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color = "#8b949e", size = 10),
        axis.title = element_text(color = "#8b949e", size = 10),
        plot.title = element_text(color = "#e6edf3", size = 12, face = "bold")
      )
  }, bg = "#161b22")
  
  # ── ANOVA plot
  output$plot_anova <- renderPlot({
    req(analysis_result())
    tbl <- analysis_result()$anova_tbl
    tbl$Term <- rownames(tbl)
    tbl$Significant <- tbl$Sig != "ns"
    tbl <- tbl[order(tbl[["F value"]], decreasing = FALSE), ]
    tbl$Term <- factor(tbl$Term, levels = tbl$Term)
    ggplot(tbl, aes(x = Term, y = `F value`,
                    fill = ifelse(Significant, "#f0a832", "#2a3441"))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(round(`F value`, 1), " ", Sig)),
                hjust = -0.1, color = "#e6edf3", size = 3.2) +
      coord_flip() +
      scale_fill_identity() +
      scale_y_continuous(limits = c(0, max(tbl[["F value"]]) * 1.25)) +
      labs(title = "Type III ANOVA — F Values",
           x = NULL, y = "F value") +
      theme_minimal(base_family = "mono") +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major = element_line(color = "#2a3441"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color = "#8b949e", size = 9),
        axis.title = element_text(color = "#8b949e", size = 10),
        plot.title = element_text(color = "#e6edf3", size = 12, face = "bold")
      )
  }, bg = "#161b22")
  
  # ── Post-hoc plot
  output$plot_posthoc <- renderPlot({
    req(analysis_result())
    req(analysis_result()$cld_tbl)
    tbl <- analysis_result()$cld_tbl
    tbl <- tbl[order(tbl$emmean, decreasing = TRUE), ]
    
    v <- analysis_result()$v
    
    # Create group label
    term_cols <- strsplit(analysis_result()$posthoc_term, ":")[[1]]
    term_cols <- term_cols[term_cols %in% colnames(tbl)]
    if (length(term_cols) > 0) {
      tbl$group_label <- apply(tbl[, term_cols, drop = FALSE], 1,
                                function(x) paste(x, collapse = ":"))
    } else {
      tbl$group_label <- rownames(tbl)
    }
    tbl$group_label <- factor(tbl$group_label, levels = rev(tbl$group_label))
    
    # Color by AV vs Control
    tbl$color <- ifelse(grepl("AV|av", tbl$group_label), "#ff6b6b", "#00d4aa")
    
    # SE column
    se_col <- if ("SE" %in% colnames(tbl)) "SE" else if ("se" %in% colnames(tbl)) "se" else NULL
    
    # .group column
    grp_col <- if (".group" %in% colnames(tbl)) ".group" else NULL
    
    p <- ggplot(tbl, aes(x = group_label, y = emmean, fill = color)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_identity()
    
    if (!is.null(se_col)) {
      p <- p + geom_errorbar(aes(ymin = emmean - .data[[se_col]],
                                  ymax = emmean + .data[[se_col]]),
                              width = 0.2, color = "#e6edf3", linewidth = 0.6)
    }
    if (!is.null(grp_col)) {
      p <- p + geom_text(aes(label = trimws(.data[[grp_col]]),
                              y = emmean + (max(emmean) * 0.05)),
                          color = "#f0a832", size = 4, fontface = "bold")
    }
    
    p <- p +
      geom_text(aes(label = round(emmean, 1), y = emmean / 2),
                color = "#0d1117", size = 3.5, fontface = "bold") +
      coord_flip() +
      labs(title = paste("Post-hoc Estimated Means:", analysis_result()$posthoc_term),
           subtitle = "Error bars = SE | Letters = Sidak CLD",
           x = NULL, y = paste("Estimated Mean of", v$y)) +
      theme_minimal(base_family = "mono") +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major = element_line(color = "#2a3441"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color = "#8b949e", size = 9),
        axis.title = element_text(color = "#8b949e", size = 10),
        plot.title    = element_text(color = "#e6edf3", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#8b949e", size = 9)
      )
    print(p)
  }, bg = "#161b22")
  
  # ── R Code
  output$r_code <- renderText({
    req(analysis_result())
    v <- analysis_result()$v
    fs <- analysis_result()$formula_str
    pt <- analysis_result()$posthoc_term
    
    paste0(
'# ─────────────────────────────────────────────\n',
'#  Agrivoltaics LMM Analysis\n',
'#  Generated by agrivoltaics.agronomy4future.com\n',
'#  Reference: agronomy4future.com/archives/24404\n',
'# ─────────────────────────────────────────────\n\n',
'# Install packages (if needed)\n',
'# install.packages(c("lme4","lmerTest","emmeans","multcomp","multcompView","ggplot2"))\n\n',
'library(lme4)\n',
'library(lmerTest)\n',
'library(emmeans)\n',
'library(multcomp)\n',
'library(multcompView)\n',
'library(ggplot2)\n\n',
'# Load data\n',
'df <- read.csv("your_data.csv")\n\n',
'# Factor conversion\n',
paste0('df$', v$site,  ' <- as.factor(df$', v$site,  ')\n'),
paste0('df$', v$block, ' <- as.factor(df$', v$block, ')\n'),
if(v$genotype != "none") paste0('df$', v$genotype, ' <- as.factor(df$', v$genotype, ')\n') else '',
if(v$row      != "none") paste0('df$', v$row,      ' <- as.factor(df$', v$row,      ')\n') else '',
if(v$season   != "none") paste0('df$', v$season,   ' <- as.factor(df$', v$season,   ')\n') else '',
'\n',
'# Fit Linear Mixed Model\n',
paste0('model <- lmer(', fs, ', data = df)\n\n'),
'# Variance components\n',
'print(VarCorr(model), comp = "Variance")\n\n',
'# Type III ANOVA\n',
'print(anova(model, type = 3))\n\n',
'# Post-hoc analysis\n',
paste0('em <- emmeans(model, ~ ', pt, ')\n'),
'cld_result <- cld(em, adjust = "sidak", Letters = letters, reverse = TRUE)\n',
'print(cld_result)\n\n',
'# Visualization\n',
'ggplot(as.data.frame(cld_result),\n',
paste0('       aes(x = reorder(', strsplit(pt,":")[[1]][1], ', emmean), y = emmean)) +\n'),
'  geom_col(fill = "#00d4aa", width = 0.6) +\n',
'  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +\n',
'  coord_flip() +\n',
paste0('  labs(title = "Post-hoc: ', pt, '",\n'),
paste0('       x = NULL, y = "', v$y, '") +\n'),
'  theme_minimal()\n'
    )
  })
  
  # ── Model summary
  output$model_summary <- renderPrint({
    req(analysis_result())
    summary(analysis_result()$model)
  })
}

# ── Helper
round_df <- function(df) {
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) df[[col]] <- round(df[[col]], 3)
  }
  df
}

shinyApp(ui = ui, server = server)
