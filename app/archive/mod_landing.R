## ── Module: Landing Page (Tab 1) ──────────────────────────────────────────────
## Orientation page with workflow roadmap and layer explanations.

library(shiny)
library(bslib)

mod_landing_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## ── Overview (full width) ────────────────────────────────────────────────
    card(
      class = "border-primary mb-3",
      card_header(class = "bg-primary text-white", "Reference & Regional Curve Development"),
      card_body(
        p("This application guides you through a structured, multi-layer evaluation
           of stratification variables for geomorphic reference curves. It ships with
           built-in reference data and also accepts custom CSV/XLSX uploads."),
        p("For each metric, you will evaluate candidate stratifications through
           5 evaluation layers, make a justified decision, build and select models,
           run diagnostics, and generate reference curves scored on a 0\u20131 scale.")
      )
    ),

    ## ── Two-column layout: Roadmap (left) + How to Use / Reset (right) ─────
    layout_columns(
      col_widths = c(7, 5),

      ## Left column — Workflow Roadmap
      card(
        card_header("Workflow Roadmap"),
        card_body(
          div(
            class = "workflow-roadmap",
            workflow_step_card(1, "Data & Setup",
              "Review dataset, configure session, run bulk screening"),
            workflow_arrow(),
            workflow_step_card(2, "Layer 1: Statistical Significance",
              "Kruskal-Wallis tests for group differences",
              detail = tagList(
                p("Tests whether grouping sites by a stratification variable produces
                   statistically different metric distributions using the Kruskal-Wallis
                   test (overall) and pairwise Wilcoxon rank-sum tests."),
                p(tags$strong("Screens for:"), "Stratifications with no detectable group differences (p > 0.10).")
              )),
            workflow_arrow(),
            workflow_step_card(3, "Layer 2: Effect Size",
              "Quantify strength of group separation",
              detail = tagList(
                p("Quantifies the practical magnitude of group differences using
                   epsilon-squared (KW effect size), eta-squared (ANOVA), and
                   rank-biserial r for two-group comparisons."),
                p(tags$strong("Screens for:"), "Stratifications with negligible effect sizes despite statistical significance.")
              )),
            workflow_arrow(),
            workflow_step_card(4, "Layer 3: Pattern Stability",
              "LOESS-based trend assessment",
              detail = tagList(
                p("Examines whether metric-predictor relationships show smooth,
                   interpretable trends via LOESS fitting. Classifies patterns as
                   monotonic, humped, flat, or noisy."),
                p(tags$strong("Screens for:"), "Stratifications that fragment data without improving trend clarity.")
              )),
            workflow_arrow(),
            workflow_step_card(5, "Layer 4: Practical Relevance",
              "Ecological and management judgment",
              detail = tagList(
                p("Expert judgment on whether a stratification is ecologically meaningful,
                   understandable to practitioners, and defensible in documentation.
                   Scored on a 1\u20135 Likert scale across 5 criteria."),
                p(tags$strong("Screens for:"), "Statistically supported but practically meaningless distinctions.")
              )),
            workflow_arrow(),
            workflow_step_card(6, "Layer 5: Operational Feasibility",
              "Sample size adequacy and implementability",
              detail = tagList(
                p("Evaluates whether sample sizes are adequate in each group, data
                   completeness is sufficient, and the stratification is implementable
                   at scale without creating too many final curves."),
                p(tags$strong("Screens for:"), "Stratifications with sparse cells, missing data, or excessive complexity.")
              )),
            workflow_arrow(),
            workflow_step_card(7, "Stratification Decision",
              "Consolidated evaluation and final selection",
              detail = tagList(
                p("Evaluates whether a stratification variable performs consistently
                   across multiple metrics. Available after completing Layers 1\u20134
                   for at least 2 metrics."),
                p(tags$strong("Informs:"), "Whether to adopt a single stratification scheme across the assessment.")
              )),
            workflow_arrow(),
            workflow_step_card(8, "Model Building & Selection",
              "Best subsets regression, BIC ranking"),
            workflow_arrow(),
            workflow_step_card(9, "Diagnostics",
              "Residual checks, assumption testing"),
            workflow_arrow(),
            workflow_step_card(10, "Reference Curve",
              "IQR-based scoring thresholds")
          )
        )
      ),

      ## Right column — How to Use (with reset button inside)
      card(
        class = "border-info",
        card_header(class = "bg-info text-white", "How to Use This App"),
        card_body(
          tags$ol(
            tags$li("Start at ", tags$strong("Data & Setup"), " to review the dataset, upload custom data, or load a previous session."),
            tags$li("Navigate to ", tags$strong("Metric Analysis"), " and select a metric from the sidebar."),
            tags$li("Work through each evaluation layer sequentially \u2014 the step tracker shows your progress."),
            tags$li("At the Decision step, review the consolidated comparison and select a stratification (or none)."),
            tags$li("Continue through model building, selection, diagnostics, and reference curve generation."),
            tags$li("Mark the metric complete and repeat for additional metrics."),
            tags$li("Use ", tags$strong("Cross-Metric Consistency"), " to compare stratification performance across metrics."),
            tags$li("Export results from the ", tags$strong("Export"), " tab.")
          ),
          tags$hr(),
          div(
            class = "d-flex align-items-center gap-2",
            actionButton(ns("reset_analysis"), "Reset Analysis",
                         class = "btn btn-outline-danger",
                         icon = icon("arrows-rotate")),
            span(class = "text-muted", style = "font-size: 0.85rem;",
                 "Clears all completed work across every metric and tab.")
          )
        )
      )
    )
  )
}

mod_landing_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Reset Analysis confirmation ───────────────────────────────────────
    observeEvent(input$reset_analysis, {
      showModal(modalDialog(
        title = "Reset Analysis",
        "This will clear all completed work for every metric, including
         cached results, the decision log, and cross-metric consistency.
         This cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_analysis"), "Reset Everything",
                       class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset_analysis, {
      removeModal()
      reset_all_analysis(rv)
      showNotification("All analysis reset \u2014 starting fresh.",
                       type = "message", duration = 3)
    })
  })
}


## ── Helper functions for roadmap rendering ──────────────────────────────────

workflow_step_card <- function(number, title, description, detail = NULL) {
  detail_el <- NULL
  if (!is.null(detail)) {
    detail_el <- tags$details(
      style = "margin-top: 4px; font-size: 0.82rem;",
      tags$summary(style = "cursor: pointer; color: #0d6efd;", "More detail"),
      div(style = "margin-top: 4px;", detail)
    )
  }
  div(
    class = "workflow-step d-flex align-items-start mb-1",
    div(
      class = "workflow-number flex-shrink-0 me-3",
      style = "width: 36px; height: 36px; border-radius: 50%; background-color: #0d6efd;
               color: white; display: flex; align-items: center; justify-content: center;
               font-weight: 700; font-size: 0.9rem;",
      as.character(number)
    ),
    div(
      tags$strong(title, style = "font-size: 0.95rem;"),
      tags$div(class = "text-muted", style = "font-size: 0.82rem;", description),
      detail_el
    )
  )
}

workflow_arrow <- function() {
  div(
    style = "margin-left: 16px; color: #adb5bd; font-size: 1.1rem; line-height: 1;",
    "\u2502"
  )
}
