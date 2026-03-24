## ── Module: Layer 3 — Pattern Stability (Step 3) ─────────────────────────────
## LOESS-based assessment of metric-predictor relationships within strata.

library(shiny)
library(bslib)
library(DT)

mod_layer3_pattern_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 3: Pattern Stability (Layer 3)",
      p("This layer examines whether metric-predictor relationships show smooth,
         interpretable trends. LOESS smoothing is used to classify each relationship
         as monotonic, humped, flat, or noisy."),
      p(tags$strong("What to review:"),
        tags$ul(
          tags$li("Is there a smooth trend, or is the pattern noisy and irregular?"),
          tags$li("Does the stratification improve or fragment the trend?"),
          tags$li("Are there large gaps in predictor coverage within groups?"),
          tags$li("Is the relationship consistent across stratification groups?")
        )),
      p(tags$strong("How to decide:"), "Stable monotonic patterns support stratification use.
         Noisy or flat patterns suggest the stratification does not improve interpretability.")
    ),

    uiOutput(ns("pattern_ui")),
    uiOutput(ns("nav_buttons"))
  )
}

mod_layer3_pattern_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Compute pattern stability ───────────────────────────────────────────
    pattern_results <- reactive({
      req(rv$current_metric, rv$layer2_effect_sizes)

      mc <- rv$metric_config[[rv$current_metric]]
      predictor_keys <- mc$allowed_predictors
      if (is.null(predictor_keys)) return(NULL)

      ## Use the selected/kept strats from earlier layers
      strat_keys <- unique(rv$layer2_effect_sizes$stratification)

      all_results <- list()
      all_plots <- list()

      for (sk in c("none", strat_keys)) {
        sk_actual <- if (sk == "none") NULL else sk
        res <- tryCatch(
          assess_pattern_stability(
            rv$data, rv$current_metric, sk_actual,
            predictor_keys, rv$metric_config,
            rv$strat_config, rv$predictor_config
          ),
          error = function(e) list(results = tibble::tibble(), plots = list())
        )
        if (nrow(res$results) > 0) {
          all_results <- c(all_results, list(res$results))
        }
        all_plots <- c(all_plots, res$plots)
      }

      combined <- dplyr::bind_rows(all_results)
      rv$layer3_patterns <- list(results = combined, plots = all_plots)
      list(results = combined, plots = all_plots)
    })

    ## ── Pattern UI ──────────────────────────────────────────────────────────
    output$pattern_ui <- renderUI({
      res <- pattern_results()
      req(res, nrow(res$results) > 0)

      plot_names <- names(res$plots)

      tagList(
        ## Results summary table
        card(
          card_header("Pattern Stability Summary"),
          card_body(DT::DTOutput(ns("pattern_table")))
        ),

        ## Scatterplot gallery
        if (length(plot_names) > 0) {
          card(
            card_header("Scatterplots with LOESS Smoothing"),
            card_body(
              selectInput(ns("plot_select"), "Select plot:",
                          choices = plot_names, width = "400px"),
              plotOutput(ns("scatter_plot"), height = "450px")
            )
          )
        },

        ## Reviewer scoring
        card(
          card_header("Pattern Quality Scoring"),
          card_body(
            p(class = "text-muted", "Rate overall pattern quality for each retained stratification."),
            uiOutput(ns("scoring_ui"))
          )
        )
      )
    })

    ## ── Pattern results table ───────────────────────────────────────────────
    output$pattern_table <- DT::renderDT({
      res <- pattern_results()
      req(res)

      display_df <- res$results |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            if (sk == "none") return("(Unstratified)")
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          pred_display = sapply(predictor, function(pk) {
            rv$predictor_config[[pk]]$display_name %||% pk
          }),
          loess_r_squared = round(loess_r_squared, 3)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          Predictor = pred_display,
          Shape = pattern_shape,
          `LOESS R-sq` = loess_r_squared,
          `Sign Changes` = n_sign_changes,
          Stability = stability_rating
        )

      DT::datatable(
        display_df,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "Stability",
          backgroundColor = DT::styleEqual(
            c("stable", "marginal", "unstable"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    ## ── Scatterplot render ──────────────────────────────────────────────────
    output$scatter_plot <- renderPlot({
      res <- pattern_results()
      req(res, input$plot_select, input$plot_select %in% names(res$plots))
      res$plots[[input$plot_select]]
    })

    ## ── Scoring UI ──────────────────────────────────────────────────────────
    output$scoring_ui <- renderUI({
      es <- rv$layer2_effect_sizes
      req(es)
      strat_keys <- unique(es$stratification)

      lapply(strat_keys, function(sk) {
        sc <- rv$strat_config[[sk]]
        div(
          class = "d-flex align-items-center gap-3 mb-2",
          tags$strong(sc$display_name %||% sk, style = "min-width: 200px;"),
          radioButtons(
            ns(paste0("pattern_score_", sk)), NULL,
            choices = c("Strong" = "strong", "Acceptable" = "acceptable",
                        "Weak" = "weak", "Reject" = "reject"),
            selected = "acceptable",
            inline = TRUE
          ),
          textInput(ns(paste0("pattern_note_", sk)), NULL,
                    placeholder = "Pattern rationale...", width = "300px")
        )
      })
    })

    ## ── Navigation ──────────────────────────────────────────────────────────
    output$nav_buttons <- renderUI({
      res <- pattern_results()
      if (is.null(res)) return(NULL)

      div(
        class = "d-flex justify-content-between mt-3",
        actionButton(ns("back"), "\u25c2 Back to Effect Size",
                     class = "btn btn-outline-secondary"),
        actionButton(ns("proceed"), "Proceed to Practical Relevance \u25b8",
                     class = "btn btn-primary btn-proceed")
      )
    })

    list(
      proceed = reactive(input$proceed),
      back = reactive(input$back)
    )
  })
}
