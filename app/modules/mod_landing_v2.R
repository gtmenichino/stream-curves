## ── Module: Landing Page v2 (Tab 1) ──────────────────────────────────────────────
## 4-phase workflow orientation page.

library(shiny)
library(bslib)

mod_landing_v2_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## ── Overview (full width) ────────────────────────────────────────────────
    card(
      class = "border-primary mb-3",
      card_header(class = "bg-primary text-white", "StreamCurves"),
      card_body(
        p("This application guides you through a structured, 4-phase evaluation
           of stratification variables and reference curve development for geomorphic
           metrics. Upload your own dataset to begin the workflow and run
           the analysis."),
        p("For each metric, you will explore candidate stratifications, verify
           consistency across metrics, confirm your selection, then build and
           finalize reference curves scored on a 0\u20131 scale.")
      )
    ),

    ## ── Two-column layout: Roadmap (left) + How to Use (right) ─────
    layout_columns(
      col_widths = c(7, 5),

      ## Left column — 4-Phase Roadmap
      card(
        card_header("4-Phase Workflow"),
        card_body(
          div(
            class = "workflow-roadmap",
            workflow_phase_card(1, "Phase 1: Explore",
              "Initial screening and effect size analysis",
              "binoculars",
              detail = tagList(
                p("Run Kruskal-Wallis tests and compute effect sizes for candidate
                   stratifications. Review boxplots, mark candidates as Promising,
                   Possible, or Not Promising."),
                p(tags$strong("Per-metric:"), "Select a metric, pick stratifications,
                   and run screening + effect size in a single pass.")
              )),
            workflow_phase_arrow(),
            workflow_phase_card(2, "Phase 2: Compare",
              "Cross-metric consistency analysis",
              "grid-3x3",
              detail = tagList(
                p("After completing Phase 1 for at least 2 metrics, compare stratification
                   performance across all metrics using a support score heatmap."),
                p(tags$strong("Cross-metric:"), "Identify broad-use candidates that
                   perform consistently vs. metric-specific ones.")
              )),
            workflow_phase_arrow(),
            workflow_phase_card(3, "Phase 3: Verify",
              "Focused verification of finalist stratifications",
              "check2-square",
              detail = tagList(
                p("For each metric, verify the top candidate stratifications through
                   pattern stability (LOESS), feasibility assessment (sample sizes),
                   and interpretability review."),
                p(tags$strong("Per-metric:"), "Confirm or reject finalist stratifications
                   and select one to carry forward into model building.")
              )),
            workflow_phase_arrow(),
            workflow_phase_card(4, "Phase 4: Finalize",
              "Empirical scoring curves from reference-site distributions",
              "graph-up",
              detail = tagList(
                p("Compute descriptive statistics (Q25, Q75, IQR) per stratum and
                   build the piecewise-linear scoring curve directly from the empirical
                   distribution of reference-standard sites."),
                p(tags$strong("No model fitting required"), " \u2014 thresholds come
                   directly from the observed data.")
              ))
          )
        )
      ),

      ## Right column — How to Use
      card(
        class = "border-info",
        card_header(class = "bg-info text-white", "How to Use This App"),
        card_body(
          tags$ol(
            tags$li("Start at ", tags$strong("Data & Setup"), " to review the dataset,
                     upload custom data, or load a previous session."),
            tags$li("Open ", tags$strong("Reference Curves"), " to track progress across metrics,
                     edit carried-forward stratifications, and recompute reference curves."),
            tags$li("Expand a metric row and use ", tags$strong("Open Phase"),
                     " to launch the Phase 1, Phase 2, Phase 3, or Phase 4 workspace for that workflow step."),
            tags$li("Use the ", tags$strong("Phase 1"), " workspace to run screening for the metric and
                     mark promising candidates."),
            tags$li("Open ", tags$strong("Phase 2"), " to compare stratification consistency across metrics
                     after screening at least 2 metrics."),
            tags$li("Use ", tags$strong("Phase 3"), " to review pattern stability, feasibility,
                     and interpretability before confirming a stratification."),
            tags$li("Use ", tags$strong("Phase 4"), " to review descriptive statistics and
                     finalize the scoring curve."),
            tags$li("Use ", tags$strong("Summary / Export"), " from the Reference Curves toolbar
                     to download results and generate reports.")
          ),
          tags$hr(),
          div(
            class = "d-flex align-items-center gap-2",
            actionButton(ns("reset_analysis"), "Reset App",
                         class = "btn btn-outline-danger",
                         icon = icon("arrows-rotate")),
            span(class = "text-muted", style = "font-size: 0.85rem;",
                 "Clears loaded data, configuration changes, and all completed analyses.")
          )
        )
      )
    )
  )
}

mod_landing_v2_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Reset Analysis confirmation ───────────────────────────────────────
    observeEvent(input$reset_analysis, {
      showModal(modalDialog(
        title = "Reset App",
        "This will return the app to its startup state. Loaded data,
         configuration changes, cached results, the decision log, and
         all phase outputs will be cleared. This cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_analysis"), "Reset App",
                       class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset_analysis, {
      removeModal()
      reset_app_to_startup(rv)
      showNotification("App reset to startup state.",
                       type = "message", duration = 3)
    })
  })
}


## ── Helper functions for phase roadmap rendering ──────────────────────────────

workflow_phase_card <- function(number, title, description, icon_name = "circle", detail = NULL) {
  detail_el <- NULL
  if (!is.null(detail)) {
    detail_el <- tags$details(
      style = "margin-top: 4px; font-size: 0.82rem;",
      tags$summary(style = "cursor: pointer; color: #0d6efd;", "More detail"),
      div(style = "margin-top: 4px;", detail)
    )
  }

  phase_color <- switch(as.character(number),
    "1" = "#0d6efd",
    "2" = "#6610f2",
    "3" = "#198754",
    "4" = "#fd7e14",
    "#0d6efd"
  )

  div(
    class = "workflow-step d-flex align-items-start mb-1",
    div(
      class = "workflow-number flex-shrink-0 me-3",
      style = sprintf("width: 36px; height: 36px; border-radius: 50%%;
                        background-color: %s; color: white; display: flex;
                        align-items: center; justify-content: center;
                        font-weight: 700; font-size: 0.9rem;", phase_color),
      as.character(number)
    ),
    div(
      tags$strong(title, style = "font-size: 0.95rem;"),
      tags$div(class = "text-muted", style = "font-size: 0.82rem;", description),
      detail_el
    )
  )
}

workflow_phase_arrow <- function() {
  div(
    style = "margin-left: 16px; color: #adb5bd; font-size: 1.1rem; line-height: 1;",
    "\u2502"
  )
}
