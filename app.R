# ============================================================
#  Victorian Waterways – Interactive Narrative Visualisation
#  FIT5147 DVP  |  Grace Meghna Didla
#  R Shiny | ggplot2 | Leaflet | plotly
# ============================================================

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(tidyr)
library(readr)
library(dplyr)

# ── Colour palette ───────────────────────────────────────────
COL_GIPP   <- "#2196F3"   # blue  – Gippsland Lakes
COL_WEST   <- "#4CAF50"   # green – Western Port
COL_ACCENT <- "#FF5722"   # orange – event highlights
BG_DARK    <- "#0d1b2a"
BG_CARD    <- "#112233"
TEXT_LIGHT <- "#e8f4f8"

# Portfolio packaging: original coursework prototype; see README for data limitations.
set.seed(5147)
# ── Water quality data ─────────────
# Annual time series 1990-2025
# ── Real data ─────────────────────────────────────────────────
water_quality <- read_csv("water_quality_clean.csv", show_col_types = FALSE)

annual_data <- water_quality %>%
  group_by(year, region) %>%
  summarise(
    chl_a     = mean(chl_a,    na.rm = TRUE),
    total_n   = mean(n_total,  na.rm = TRUE),
    total_p   = mean(p_total,  na.rm = TRUE),
    do        = mean(do_mg,    na.rm = TRUE),
    ph        = mean(ph,       na.rm = TRUE),
    turbidity = mean(turb,     na.rm = TRUE),
    .groups = "drop"
  )

season_data <- water_quality %>%
  filter(!is.na(chl_a), !is.na(n_total)) %>%
  mutate(season = factor(season, levels = c("Summer","Autumn","Winter","Spring")))


# Monitoring site locations
sites <- tribble(
  ~site,                ~lat,      ~lng,       ~region,           ~mean_chla, ~n_obs,
  "Lake Wellington",    -38.095,   147.267,   "Gippsland Lakes", 17.2,       487,
  "Lake Victoria",      -37.948,   147.600,   "Gippsland Lakes", 11.8,       312,
  "Lake King North",    -37.860,   147.900,   "Gippsland Lakes", 9.4,        298,
  "Lake King South",    -37.920,   147.960,   "Gippsland Lakes", 8.1,        301,
  "Lake Reeve East",    -38.050,   147.380,   "Gippsland Lakes", 7.3,        189,
  "Lake Reeve West",    -38.070,   147.170,   "Gippsland Lakes", 6.9,        172,
  "Metung",             -37.867,   147.832,   "Gippsland Lakes", 8.8,        143,
  "Hastings",           -38.305,   145.193,   "Western Port",    1.3,        392,
  "Barralier Island",   -38.430,   145.375,   "Western Port",    0.9,        318,
  "Corinella",          -38.558,   145.467,   "Western Port",    1.8,        320
)

# Vegetation correlation data (2014-2023)
veg_years <- 2014:2023
veg_data  <- bind_rows(
  tibble(year=veg_years, region="Western Port",
         cover = seq(18,35,length.out=10) + rnorm(10,0,1.5),
         chl_a = seq(1.8,0.7,length.out=10) + rnorm(10,0,0.15),
         total_p = seq(28,12,length.out=10) + rnorm(10,0,2)),
  tibble(year=veg_years, region="Gippsland Lakes",
         cover = seq(20,32,length.out=10) + rnorm(10,0,1.5),
         chl_a = seq(9,11,length.out=10) + rnorm(10,0,1.5),
         total_p = seq(58,65,length.out=10) + rnorm(10,0,5))
)

# Disturbance events for annotations
events_df <- tribble(
  ~year, ~label,               ~y_pos,
  2002,  "2002 Floods",         16.5,
  2008,  "2008 Algal Bloom",    24.8,
  2011,  "La Niña 2010-11",     21.5,
  2020,  "Black Summer Fires",  5.0
)

# ── Theme ─────────────────────────────────────────────────────
theme_ww <- function() {
  theme_minimal(base_size=13) +
    theme(
      plot.background  = element_rect(fill=BG_CARD, color=NA),
      panel.background = element_rect(fill=BG_CARD, color=NA),
      panel.grid.major = element_line(color="#1e3a4a", linewidth=0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color=TEXT_LIGHT),
      axis.text        = element_text(color="#a0c4d8"),
      axis.title       = element_text(color=TEXT_LIGHT, size=11),
      plot.title       = element_text(color=TEXT_LIGHT, face="bold", size=13),
      plot.subtitle    = element_text(color="#a0c4d8", size=10),
      legend.background = element_rect(fill=BG_CARD, color=NA),
      legend.text      = element_text(color=TEXT_LIGHT),
      legend.title     = element_text(color=TEXT_LIGHT),
      strip.text       = element_text(color=TEXT_LIGHT, face="bold")
    )
}

region_colours <- c("Gippsland Lakes"=COL_GIPP, "Western Port"=COL_WEST)

# ── UI ────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$title("Victorian Waterways"),
    tags$style(HTML(sprintf("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&family=Playfair+Display:wght@700&display=swap');

      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background: %s;
        color: %s;
        font-family: 'Inter', sans-serif;
        scroll-behavior: smooth;
      }

      /* ── Hero ── */
      #hero {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
        padding: 40px 20px;
        background: linear-gradient(180deg, #061525 0%%, #0d1b2a 100%%);
        position: relative;
      }
      #hero h1 {
        font-family: 'Playfair Display', serif;
        font-size: clamp(2.2rem, 5vw, 4rem);
        color: #e8f4f8;
        letter-spacing: 2px;
        margin-bottom: 10px;
      }
      #hero .tagline {
        font-size: 1.15rem;
        color: #7ec8e3;
        margin-bottom: 30px;
        max-width: 600px;
        line-height: 1.7;
      }
      .scroll-hint {
        margin-top: 60px;
        color: #4a8fa8;
        font-size: 0.85rem;
        letter-spacing: 2px;
        animation: bounce 2s infinite;
      }
      @keyframes bounce {
        0%%,100%%{transform:translateY(0)} 50%%{transform:translateY(8px)}
      }

      /* ── Chapter nav ── */
      #chapter-nav {
        position: sticky;
        top: 0;
        z-index: 100;
        background: rgba(10,22,35,0.95);
        backdrop-filter: blur(8px);
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 6px;
        padding: 10px 20px;
        border-bottom: 1px solid #1e3a4a;
        flex-wrap: wrap;
      }
      .ch-btn {
        background: transparent;
        border: 1px solid #1e5070;
        color: #7ec8e3;
        border-radius: 20px;
        padding: 6px 18px;
        font-size: 0.82rem;
        cursor: pointer;
        transition: all 0.2s;
        letter-spacing: 0.5px;
      }
      .ch-btn:hover, .ch-btn.active {
        background: #1e5070;
        color: #e8f4f8;
        border-color: #2196F3;
      }

      /* ── Chapter sections ── */
      .chapter {
        max-width: 1200px;
        margin: 0 auto;
        padding: 60px 24px;
      }
      .chapter-header {
        margin-bottom: 32px;
      }
      .chapter-number {
        font-size: 0.75rem;
        letter-spacing: 3px;
        color: #4a8fa8;
        text-transform: uppercase;
        margin-bottom: 6px;
      }
      .chapter-title {
        font-family: 'Playfair Display', serif;
        font-size: clamp(1.6rem, 3vw, 2.4rem);
        color: #e8f4f8;
        margin-bottom: 12px;
      }
      .chapter-intro {
        font-size: 1rem;
        color: #a0c4d8;
        line-height: 1.8;
        max-width: 750px;
      }
      .chapter-divider {
        border: none;
        border-top: 1px solid #1e3a4a;
        margin: 0;
      }

      /* ── Cards ── */
      .card {
        background: %s;
        border: 1px solid #1e3a4a;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 18px;
      }
      .card-title {
        font-size: 0.8rem;
        letter-spacing: 2px;
        text-transform: uppercase;
        color: #4a8fa8;
        margin-bottom: 14px;
      }

      /* ── Stat pills ── */
      .stat-row {
        display: flex;
        gap: 14px;
        flex-wrap: wrap;
        margin-bottom: 20px;
      }
      .stat-pill {
        background: #0a1e30;
        border: 1px solid #1e3a4a;
        border-radius: 8px;
        padding: 12px 18px;
        flex: 1;
        min-width: 130px;
        text-align: center;
      }
      .stat-value {
        font-size: 1.5rem;
        font-weight: 700;
        color: #e8f4f8;
      }
      .stat-label {
        font-size: 0.75rem;
        color: #7ec8e3;
        margin-top: 3px;
      }
      .stat-pill.gipp .stat-value { color: %s; }
      .stat-pill.west .stat-value { color: %s; }

      /* ── Controls ── */
      .controls-row {
        display: flex;
        gap: 14px;
        flex-wrap: wrap;
        align-items: flex-end;
        margin-bottom: 18px;
      }
      .control-group {
        display: flex;
        flex-direction: column;
        gap: 6px;
      }
      .control-label {
        font-size: 0.75rem;
        letter-spacing: 1px;
        text-transform: uppercase;
        color: #4a8fa8;
      }
      .shiny-input-container { margin-bottom: 0 !important; }
      .selectize-input, .selectize-dropdown {
        background: #0a1e30 !important;
        color: #e8f4f8 !important;
        border: 1px solid #1e5070 !important;
        border-radius: 6px !important;
      }
      .irs--shiny .irs-bar { background: #2196F3; }
      .irs--shiny .irs-handle { background: #2196F3; border-color: #2196F3; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #2196F3; }
      .irs--shiny .irs-line { background: #1e3a4a; }
      .irs--shiny .irs-grid-text { color: #a0c4d8; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { color: #7ec8e3; }

      /* ── Two-col grid ── */
      .two-col { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; }
      .three-col { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 14px; }
      @media(max-width:768px) {
        .two-col, .three-col { grid-template-columns: 1fr; }
      }

      /* ── Narrative text block ── */
      .narrative-block {
        background: linear-gradient(135deg, #0a1e30 0%%, #112030 100%%);
        border-left: 3px solid %s;
        border-radius: 0 8px 8px 0;
        padding: 18px 22px;
        margin: 16px 0;
        line-height: 1.8;
        font-size: 0.95rem;
        color: #c8dde8;
      }
      .narrative-block strong { color: #e8f4f8; }
      .narrative-block.orange { border-left-color: %s; }
      .narrative-block.green  { border-left-color: %s; }

      /* ── Event badge ── */
      .event-badge {
        display: inline-block;
        padding: 3px 10px;
        border-radius: 12px;
        font-size: 0.72rem;
        font-weight: 600;
        letter-spacing: 0.5px;
        margin: 2px;
      }
      .badge-flood  { background: #1565C0; color: #e3f2fd; }
      .badge-fire   { background: #BF360C; color: #fbe9e7; }
      .badge-nina   { background: #1B5E20; color: #e8f5e9; }
      .badge-recent { background: #4A148C; color: #f3e5f5; }

      /* ── Footer ── */
      #footer {
        text-align: center;
        padding: 40px 20px;
        color: #4a8fa8;
        font-size: 0.8rem;
        border-top: 1px solid #1e3a4a;
        line-height: 2;
      }
      #footer a { color: #7ec8e3; text-decoration: none; }
    ",
    BG_DARK, TEXT_LIGHT, BG_CARD, COL_GIPP, COL_WEST,
    COL_GIPP, COL_ACCENT, COL_WEST
    ))
  ),

  # ── HERO ──────────────────────────────────────────────────
  div(id="hero",
    h1("VICTORIAN WATERWAYS"),
    div(class="narrative-block", style="max-width:850px;",
      tags$strong("Coursework prototype — data limitations"),
      p("Water-quality charts use the supplied coursework CSV. Vegetation plots use simulated data; map site statistics are illustrative hard-coded values. Numerical narrative and event explanations are original coursework text and have not been independently validated. These sections are not evidence of measured correlations or causal effects.")
    ),
    div(class="tagline",
      "Two estuaries. One crisis. A 35-year record of water quality across",
      tags$strong("Gippsland Lakes"), " and ", tags$strong("Western Port"),
      " — and the floods, fires and blooms that shaped them."
    ),
    div(class="stat-row", style="justify-content:center; margin-top:30px;",
      div(class="stat-pill gipp",
        div(class="stat-value","35"), div(class="stat-label","Years of data")),
      div(class="stat-pill gipp",
        div(class="stat-value","2,877"), div(class="stat-label","Observations")),
      div(class="stat-pill west",
        div(class="stat-value","10"), div(class="stat-label","Monitoring sites")),
      div(class="stat-pill west",
        div(class="stat-value","6"), div(class="stat-label","Water quality indicators"))
    ),
    div(class="scroll-hint", "▼  SCROLL TO EXPLORE  ▼")
  ),

  # ── STICKY NAV ────────────────────────────────────────────
  div(id="chapter-nav",
    tags$button(class="ch-btn", onclick="scrollToId('ch1')", "Chapter 1 — The Contrast"),
    tags$button(class="ch-btn", onclick="scrollToId('ch2')", "Chapter 2 — The Cycle"),
    tags$button(class="ch-btn", onclick="scrollToId('ch3')", "Chapter 3 — Where It Hits"),
    tags$button(class="ch-btn", onclick="scrollToId('footer')", "About & Data")
  ),

  # ── CHAPTER 1: THE CONTRAST ───────────────────────────────
  tags$hr(class="chapter-divider"),
  div(id="ch1",
    div(class="chapter",
      div(class="chapter-header",
        div(class="chapter-number","Chapter 1"),
        div(class="chapter-title","The Contrast"),
        div(class="chapter-intro",
          "Two water bodies separated by 200 km of coastline and yet worlds apart in water quality.",
          " Gippsland Lakes has shown chronic eutrophication driven by agricultural nutrient loading,",
          " while Western Port maintains comparatively healthy conditions year-round."
        )
      ),

      # Controls
      div(class="controls-row",
        div(class="control-group",
          div(class="control-label","Indicator"),
          selectInput("ch1_indicator","",
            choices = c("Chlorophyll-a (µg/L)"="chl_a",
                        "Total Nitrogen (µg/L)"="total_n",
                        "Total Phosphorus (µg/L)"="total_p",
                        "Dissolved Oxygen (mg/L)"="do",
                        "pH"="ph"),
            selected="chl_a", width="220px")
        ),
        div(class="control-group",
          div(class="control-label","Year range"),
          sliderInput("ch1_years","", min=1990, max=2025,
                      value=c(1990,2025), sep="", width="360px")
        )
      ),

      # Time series plot
      div(class="card",
        div(class="card-title","Long-term trends (1990–2025)"),
        plotlyOutput("ts_plot", height="320px")
      ),

      div(class="narrative-block",
        tags$strong("Chlorophyll-a — the headline finding:"),
        " Gippsland Lakes averaged 10.0 µg/L over 35 years compared to Western Port's 1.2 µg/L.",
        " That is an 8× difference in algal biomass — a persistent signature of chronic eutrophication",
        " driven by agricultural catchment runoff from the Mitchell and Thomson Rivers."
      ),

      # Seasonal boxplots
      div(class="card",
        div(class="card-title","Seasonal variation — does the gap narrow in any season?"),
        div(class="two-col",
          plotlyOutput("box_chla", height="260px"),
          plotlyOutput("box_tn",   height="260px")
        )
      ),

      div(class="narrative-block orange",
        tags$strong("Spring is the danger season:"),
        " Chlorophyll-a in Gippsland Lakes peaks in Spring, when warming temperatures and abundant",
        " sunlight combine with chronic nutrient enrichment to trigger algal growth.",
        " Western Port remains low and stable across all four seasons."
      )
    )
  ),

  # ── CHAPTER 2: THE CYCLE ──────────────────────────────────
  tags$hr(class="chapter-divider"),
  div(id="ch2",
    div(class="chapter",
      div(class="chapter-header",
        div(class="chapter-number","Chapter 2"),
        div(class="chapter-title","The Cycle — Pulse, Destroy and Repeat"),
        div(class="chapter-intro",
          "Water quality in Gippsland Lakes is not gradually declining — it is being",
          " periodically devastated by discrete disturbance events, then slowly recovering.",
          " Floods, bushfires, and La Niña events each flush massive nutrient loads into the lakes",
          " before the system has fully recovered from the previous shock."
        )
      ),

      # Event badges
      div(style="margin-bottom:20px;",
        div(style="font-size:0.8rem;color:#4a8fa8;margin-bottom:8px;","KEY DISTURBANCE EVENTS"),
        tags$span(class="event-badge badge-flood","2002 — Gippsland Floods"),
        tags$span(class="event-badge badge-fire","2006–07 Bushfires → 2008 Algal Bloom"),
        tags$span(class="event-badge badge-nina","2010–11 La Niña"),
        tags$span(class="event-badge badge-recent","2019–20 Black Summer Fires")
      ),

      div(class="card",
        div(class="card-title","Chlorophyll-a & Nutrient loading — Gippsland Lakes (2000–2015)"),
        p(style="font-size:0.8rem;color:#7ec8e3;margin-bottom:10px;",
          "Click on any spike annotation to see event details"),
        plotlyOutput("pulse_plot", height="360px")
      ),

      div(class="narrative-block orange",
        tags$strong("The 2008 bloom was a watershed moment:"),
        " The 2006–07 Great Divide bushfires stripped catchment vegetation and left vast quantities",
        " of ash and nutrients exposed. When rains returned, a catastrophic flush followed —",
        " total nitrogen rose from 362 µg/L (2006) to 997 µg/L (2008), and chlorophyll-a peaked",
        " at 24.1 µg/L — the highest annual mean on record. The lakes turned blue-green and",
        " significant fish kills were documented."
      ),

      # Recovery chart
      div(class="card",
        div(class="card-title","Recovery trajectory — does the system return to baseline?"),
        plotlyOutput("recovery_plot", height="280px")
      ),

      div(class="narrative-block",
        tags$strong("Incomplete recovery between events:"),
        " After each peak, chlorophyll-a declines — but never fully back to pre-disturbance levels",
        " before the next event arrives. This points to a pattern of",
        tags$strong("incremental long-term degradation punctuated by acute shocks."),
        " The system is resilient but not invulnerable."
      ),

      # Veg correlation
      div(class="card",
        div(class="card-title","Illustrative vegetation relationships — simulated data"),
        div(class="two-col",
          plotlyOutput("veg_chla_plot", height="260px"),
          plotlyOutput("veg_tp_plot",   height="260px")
        )
      ),

      div(class="narrative-block green",
        tags$strong("Western Port: vegetation is a meaningful buffer."),
        " A statistically significant negative correlation exists between fringing vegetation cover and",
        " chlorophyll-a (r = −0.748, p = 0.013), total phosphorus (r = −0.677, p = 0.032),",
        " and turbidity (r = −0.682, p = 0.030) in Western Port.",
        tags$br(),
        tags$strong("Gippsland Lakes: local vegetation cannot compete with catchment-scale forces."),
        " No significant correlation was found for any indicator — the system is too heavily impacted",
        " for local riparian buffer strips to offset the scale of nutrient loading from inland."
      )
    )
  ),

  # ── CHAPTER 3: WHERE IT HITS ──────────────────────────────
  tags$hr(class="chapter-divider"),
  div(id="ch3",
    div(class="chapter",
      div(class="chapter-header",
        div(class="chapter-number","Chapter 3"),
        div(class="chapter-title","Where It Hits — The Spatial Story"),
        div(class="chapter-intro",
          "Not all parts of Gippsland Lakes suffer equally.",
          " A clear west-to-east gradient of algal intensity follows the flow of nutrient-rich water",
          " from inland agricultural catchments toward the sea.",
          " Meanwhile, Western Port's three sites remain consistently clear."
        )
      ),

      div(class="controls-row",
        div(class="control-group",
          div(class="control-label","Map variable"),
          selectInput("map_var","",
            choices=c("Mean Chlorophyll-a"="mean_chla"),
            selected="mean_chla", width="220px")
        ),
        div(class="control-group",
          div(class="control-label","Year (algal intensity map)"),
          sliderInput("map_year","",min=1990,max=2025,value=2008,step=1,sep="",width="300px")
        )
      ),

      div(class="card",
        div(class="card-title","Illustrative site map — hard-coded example statistics"),
        p(style="font-size:0.8rem;color:#7ec8e3;margin-bottom:10px;",
          "Hover over a bubble to see site details"),
        leafletOutput("site_map", height="420px")
      ),

      div(class="narrative-block orange",
        tags$strong("Lake Wellington drives the Gippsland average:"),
        " The inland western sites (Lake Wellington: 17.2 µg/L mean) receive the first nutrient flush",
        " from the Mitchell and Thomson Rivers, carrying runoff from dairy and cropping lands.",
        " Concentrations decrease eastward as the water dilutes through the connected lake system."
      ),

      # Recent data
      div(class="card",
        div(class="card-title","Recent conditions (2018–2025) — are things improving?"),
        plotlyOutput("recent_plot", height="300px")
      ),

      div(class="narrative-block",
        tags$strong("2023–2024 shows the lowest chlorophyll-a values in 35 years (3.8–4.0 µg/L)"),
        " — a genuinely encouraging signal. But the 2021–22 total nitrogen spike (1,154 and 1,027 µg/L)",
        " — echoing the 2008 bloom pattern — confirms the system remains highly vulnerable",
        " to catchment disturbance. The Black Summer fires of 2019–20 left their mark.",
        tags$br(), tags$br(),
        "Long-term improvement requires both", tags$strong("catchment-scale land management"),
        " and", tags$strong("climate change adaptation"),
        " to reduce the frequency and intensity of future bushfire events."
      )
    )
  ),

  # ── FOOTER ────────────────────────────────────────────────
  tags$hr(class="chapter-divider"),
  div(id="footer",
    p(tags$strong("Data Sources")),
    p(
      tags$a(href="https://discover.data.vic.gov.au/dataset/gippsland-lakes-water-quality-data",
             target="_blank", "EPA Victoria — Gippsland Lakes Water Quality Data"),
      " · ",
      tags$a(href="https://www.data.vic.gov.au/data/dataset/epa-western-port-water-quality-data",
             target="_blank", "EPA Victoria — Western Port Water Quality Data"),
      " · ",
      tags$a(href="https://www.abs.gov.au/statistics/environment/environmental-accounts/national-ecosystem-accounts-experimental-estimates/2020-21",
             target="_blank", "ABS — National Ecosystem Accounts"),
      " · ",
      tags$a(href="https://digital.atlas.gov.au/maps/cffa074a84f042dfa910fa9049a786e0/about",
             target="_blank", "BoM — Geofabric Surface Hydrology Network V3.3")
    ),
    p("Monitoring period: 1990–2025  ·  2,877 surface observations  ·  10 sites"),
    p("FIT5147 Data Exploration and Visualisation — Semester 1, 2026"),
    p("Grace Meghna Didla")
  ),

  # JS: smooth scroll + active nav
  tags$script(HTML("
    function scrollToId(id) {
      document.getElementById(id).scrollIntoView({behavior:'smooth'});
    }
    window.addEventListener('scroll', function() {
      var chapters = ['ch1','ch2','ch3'];
      var btns = document.querySelectorAll('.ch-btn');
      var current = -1;
      chapters.forEach(function(id, i) {
        var el = document.getElementById(id);
        if (el && el.getBoundingClientRect().top <= 80) current = i;
      });
      btns.forEach(function(b,i){ b.classList.toggle('active', i===current); });
    });
  "))
))

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Helper: filter annual data by year range + indicator
  filtered_ts <- reactive({
    annual_data %>%
      filter(year >= input$ch1_years[1],
             year <= input$ch1_years[2])
  })

  ind_label <- reactive({
    switch(input$ch1_indicator,
      chl_a   = "Chlorophyll-a (µg/L)",
      total_n = "Total Nitrogen (µg/L)",
      total_p = "Total Phosphorus (µg/L)",
      do      = "Dissolved Oxygen (mg/L)",
      ph      = "pH"
    )
  })

  # ── Chapter 1: time series ──────────────────────────────
  output$ts_plot <- renderPlotly({
    df <- filtered_ts()
    ind <- input$ch1_indicator
    
    gipp <- df %>% dplyr::filter(region == "Gippsland Lakes")
    west <- df %>% dplyr::filter(region == "Western Port")
    
    plot_ly() %>%
      add_trace(data=gipp, x=~year, y=~.data[[ind]],
                type="scatter", mode="lines+markers",
                name="Gippsland Lakes",
                line=list(color=COL_GIPP, width=2.5),
                marker=list(color=COL_GIPP, size=5),
                hovertemplate=paste0("Gippsland Lakes<br>Year: %{x}<br>",ind_label(),": %{y:.2f}<extra></extra>")) %>%
      add_trace(data=west, x=~year, y=~.data[[ind]],
                type="scatter", mode="lines+markers",
                name="Western Port",
                line=list(color=COL_WEST, width=2.5),
                marker=list(color=COL_WEST, size=5),
                hovertemplate=paste0("Western Port<br>Year: %{x}<br>",ind_label(),": %{y:.2f}<extra></extra>")) %>%
      layout(
        paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
        font=list(color=TEXT_LIGHT),
        xaxis=list(title="Year", color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        yaxis=list(title=ind_label(), color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        legend=list(font=list(color=TEXT_LIGHT), bgcolor=BG_CARD)
      )
  })
  
  # ── Chapter 1: seasonal boxplots ───────────────────────
  output$box_chla <- renderPlotly({
    df <- water_quality %>%
      dplyr::filter(!is.na(chl_a)) %>%
      mutate(season = factor(season, levels=c("Summer","Autumn","Winter","Spring")))
    
    plot_ly(df, x=~season, y=~chl_a, color=~region, type="box",
            colors=c("Gippsland Lakes"=COL_GIPP, "Western Port"=COL_WEST),
            hovertemplate="Region: %{fullData.name}<br>Season: %{x}<br>Chl-a: %{y:.2f}<extra></extra>") %>%
      layout(
        paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
        font=list(color=TEXT_LIGHT),
        xaxis=list(title=NULL, color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        yaxis=list(title="Chlorophyll-a (log µg/L)", color=TEXT_LIGHT,
                   gridcolor="#1e3a4a", type="log"),
        legend=list(font=list(color=TEXT_LIGHT), bgcolor=BG_CARD),
        boxmode="group"
      )
  })
  
  output$box_tn <- renderPlotly({
    df <- water_quality %>%
      dplyr::filter(!is.na(n_total)) %>%
      mutate(season = factor(season, levels=c("Summer","Autumn","Winter","Spring")))
    
    plot_ly(df, x=~season, y=~n_total, color=~region, type="box",
            colors=c("Gippsland Lakes"=COL_GIPP, "Western Port"=COL_WEST),
            hovertemplate="Region: %{fullData.name}<br>Season: %{x}<br>Total N: %{y:.0f}<extra></extra>") %>%
      layout(
        paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
        font=list(color=TEXT_LIGHT),
        xaxis=list(title=NULL, color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        yaxis=list(title="Total Nitrogen (log µg/L)", color=TEXT_LIGHT,
                   gridcolor="#1e3a4a", type="log"),
        legend=list(font=list(color=TEXT_LIGHT), bgcolor=BG_CARD),
        boxmode="group"
      )
  })
  # ── Chapter 2: pulse disturbance plot ──────────────────
  output$pulse_plot <- renderPlotly({
    df <- filtered_ts()
    ind <- input$ch1_indicator
    
    gipp <- df %>% filter(region == "Gippsland Lakes")
    west <- df %>% filter(region == "Western Port")
    
    plot_ly() %>%
      add_trace(data=gipp, x=~year, y=~.data[[ind]],
                type="scatter", mode="lines+markers",
                name="Gippsland Lakes",
                line=list(color=COL_GIPP, width=2.5),
                marker=list(color=COL_GIPP, size=5),
                hovertemplate=paste0("Gippsland Lakes<br>Year: %{x}<br>Value: %{y:.2f}<extra></extra>")) %>%
      add_trace(data=west, x=~year, y=~.data[[ind]],
                type="scatter", mode="lines+markers",
                name="Western Port",
                line=list(color=COL_WEST, width=2.5),
                marker=list(color=COL_WEST, size=5),
                hovertemplate=paste0("Western Port<br>Year: %{x}<br>Value: %{y:.2f}<extra></extra>")) %>%
      layout(
        paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
        font=list(color=TEXT_LIGHT),
        xaxis=list(title="Year", color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        yaxis=list(title=ind_label(), color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        legend=list(font=list(color=TEXT_LIGHT), bgcolor=BG_CARD)
      )
  })

  # ── Chapter 2: recovery plot ────────────────────────────
  output$recovery_plot <- renderPlotly({
    df <- annual_data %>% filter(region=="Gippsland Lakes")

    mean_val <- mean(df$chl_a, na.rm=TRUE)

    p <- plot_ly(df, x=~year, y=~chl_a, type="scatter", mode="lines+markers",
                 line=list(color=COL_GIPP, width=2),
                 marker=list(color=COL_GIPP, size=5),
                 name="Gippsland Lakes Chl-a",
                 hovertemplate="Year: %{x}<br>Chl-a: %{y:.1f} µg/L<extra></extra>") %>%
      add_trace(x=range(df$year), y=c(mean_val,mean_val),
                type="scatter", mode="lines",
                line=list(color="#FF5722", width=1.5, dash="dash"),
                name="35-yr mean (10.0 µg/L)",
                hoverinfo="skip") %>%
      layout(
        paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
        font=list(color=TEXT_LIGHT),
        xaxis=list(title="Year", color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        yaxis=list(title="Mean Chlorophyll-a (µg/L)", color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        legend=list(font=list(color=TEXT_LIGHT), bgcolor=BG_CARD),
        annotations=list(
          list(x=2023.5, y=4.0, text="2023-24<br>lowest ever",
               showarrow=TRUE, arrowcolor="#81c784", font=list(color="#81c784",size=10)),
          list(x=2021, y=11.5, text="Post Black<br>Summer",
               showarrow=TRUE, arrowcolor=COL_ACCENT, font=list(color=COL_ACCENT,size=10))
        )
      )
    p
  })

  # ── Chapter 2: vegetation correlation plots ─────────────
  make_veg_plot <- function(yvar, ytitle) {
    ggplot(veg_data, aes(x=cover, y=.data[[yvar]], color=region,
                         text=paste0(region,"\nYear: ",year,
                                     "\nCover: ",round(cover,1),
                                     "\n",ytitle,": ",round(.data[[yvar]],2)))) +
      geom_point(size=2.5, alpha=0.8) +
      geom_smooth(method="lm", se=FALSE, linewidth=0.8) +
      scale_color_manual(values=region_colours) +
      labs(x="Mean Fringing Vegetation Cover (%)", y=ytitle, title=ytitle) +
      theme_ww() + theme(legend.position="none")
  }
  output$veg_chla_plot <- renderPlotly({
    p <- make_veg_plot("chl_a","Chlorophyll-a (µg/L)")
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
             font=list(color=TEXT_LIGHT))
  })
  output$veg_tp_plot <- renderPlotly({
    p <- make_veg_plot("total_p","Total Phosphorus (µg/L)")
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
             font=list(color=TEXT_LIGHT))
  })

  # ── Chapter 3: leaflet map ──────────────────────────────
  output$site_map <- renderLeaflet({
    pal <- colorNumeric(
      palette = c("#1a6b8a","#2196F3","#64b5f6","#ffcc80","#FF5722","#b71c1c"),
      domain = sites$mean_chla
    )

    leaflet(sites) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(lng=146.5, lat=-38.2, zoom=8) %>%
      addCircleMarkers(
        ~lng, ~lat,
        radius = ~sqrt(mean_chla) * 4.5,
        color  = ~pal(mean_chla),
        fillColor = ~pal(mean_chla),
        fillOpacity = 0.75,
        weight = 2,
        opacity = 0.9,
        popup  = ~paste0(
          "<div style='background:#112233;color:#e8f4f8;padding:10px;border-radius:6px;",
          "font-family:Inter,sans-serif;min-width:160px;'>",
          "<strong style='color:#7ec8e3;'>", site, "</strong><br/>",
          "<span style='color:#a0c4d8;'>Region:</span> ", region, "<br/>",
          "<span style='color:#a0c4d8;'>Mean Chl-a:</span> <strong>",
          mean_chla, " µg/L</strong><br/>",
          "<span style='color:#a0c4d8;'>Observations:</span> ", n_obs,
          "</div>"
        )
      ) %>%
      addLegend(
        position="bottomright",
        pal=pal, values=~mean_chla,
        title="Mean Chl-a (µg/L)",
        opacity=0.8,
        labFormat=labelFormat(suffix=" µg/L")
      )
  })

  # ── Chapter 3: recent conditions ───────────────────────
  output$recent_plot <- renderPlotly({
    df <- annual_data %>% filter(year >= 2018)

    plot_ly() %>%
      add_trace(data=filter(df, region=="Gippsland Lakes"),
                x=~year, y=~chl_a, type="scatter", mode="lines+markers",
                name="Gippsland Lakes — Chl-a",
                line=list(color=COL_GIPP,width=2.5),
                marker=list(color=COL_GIPP,size=8),
                hovertemplate="Year: %{x}<br>Chl-a: %{y:.2f} µg/L<extra></extra>") %>%
      add_trace(data=filter(df, region=="Western Port"),
                x=~year, y=~chl_a, type="scatter", mode="lines+markers",
                name="Western Port — Chl-a",
                line=list(color=COL_WEST,width=2.5),
                marker=list(color=COL_WEST,size=8),
                hovertemplate="Year: %{x}<br>Chl-a: %{y:.2f} µg/L<extra></extra>") %>%
      add_trace(x=c(2018,2025), y=c(10,10),
                type="scatter", mode="lines",
                line=list(color="#FF5722",width=1.5,dash="dot"),
                name="Gippsland 35-yr mean",
                hoverinfo="skip") %>%
      layout(
        paper_bgcolor=BG_CARD, plot_bgcolor=BG_CARD,
        font=list(color=TEXT_LIGHT),
        xaxis=list(title="Year", color=TEXT_LIGHT, gridcolor="#1e3a4a",
                   tickmode="linear", dtick=1),
        yaxis=list(title="Chlorophyll-a (µg/L)", color=TEXT_LIGHT, gridcolor="#1e3a4a"),
        legend=list(font=list(color=TEXT_LIGHT), bgcolor=BG_CARD),
        annotations=list(
          list(x=2021, y=11.5, text="Post-Black Summer<br>nitrogen spike",
               showarrow=TRUE, arrowcolor=COL_ACCENT,
               font=list(color=COL_ACCENT,size=10), ay=-40)
        )
      )
  })
}

shinyApp(ui=ui, server=server)



