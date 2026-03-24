## ── Module: Layer 2 — Effect Size (Step 2) ───────────────────────────────────
## Quantifies the practical magnitude of stratification group differences.

library(shiny)
library(bslib)
library(DT)

mod_layer2_effect_size_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## ── Header / Purpose ────────────────────────────────────────────────────
    explanation_card(
      "Step 2: Effect Size (Layer 2)",
      p("This layer quantifies the practical magnitude of group differences.
         A statistically significant result (Layer 1) may have a negligible
         effect size, especially with small samples."),
      p(tags$strong("What to review:"), "Epsilon-squared (KW effect size), eta-squared
         (variance explained), and for 2-group comparisons, rank-biserial r."),
      p(tags$strong("How to decide:"), "Effect sizes: <0.01 negligible, 0.01\u20130.06 small,
         0.06\u20130.14 medium, >0.14 large. Mark strong or weak accordingly.")
    ),

    uiOutput(ns("effect_ui")),
    uiOutput(ns("nav_buttons"))
  )
}

mod_layer2_effect_size_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Compute effect sizes ────────────────────────────────────────────────
    effect_results <- reactive({
      req(rv$current_metric, rv$layer1_screening)

      ## Only compute for stratifications that passed reviewer filter (or all if no reviewer)
      strat_keys <- unique(rv$layer1_screening$results$stratification)
      if (!is.null(rv$layer1_reviewer)) {
        kept <- rv$layer1_reviewer |>
          dplyr::filter(keep_status != "discard") |>
          dplyr::pull(stratification)
        strat_keys <- intersect(strat_keys, kept)
      }

      if (length(strat_keys) == 0) return(NULL)

      es <- compute_effect_sizes(
        rv$data, rv$current_metric, strat_keys,
        rv$metric_config, rv$strat_config
      )
      rv$layer2_effect_sizes <- es
      rv$all_layer2_results[[rv$current_metric]] <- es
      es
    })

    ## ── Effect size UI ──────────────────────────────────────────────────────
    output$effect_ui <- renderUI({
      es <- effect_results()
      req(es, nrow(es) > 0)

      tagList(
        ## Bar chart
        card(
          card_header("Effect Size by Stratification"),
          card_body(plotOutput(ns("bar_chart"), height = "350px"))
        ),

        ## Results table
        card(
          card_header("Effect Size Details"),
          card_body(DT::DTOutput(ns("effect_table")))
        ),

        ## Reviewer controls
        card(
          card_header("Reviewer Assessment"),
          card_body(
            p(class = "text-muted", "Rate each stratification's effect size strength."),
            lapply(seq_len(nrow(es)), function(i) {
              sk <- es$stratification[i]
              sc <- rv$strat_config[[sk]]
              div(
                class = "d-flex align-items-center gap-3 mb-2",
                tags$strong(sc$display_name %||% sk, style = "min-width: 200px;"),
                radioButtons(
                  ns(paste0("strength_", sk)), NULL,
                  choices = c("Strong" = "strong", "Borderline" = "borderline", "Weak" = "weak"),
                  selected = switch(es$effect_size_label[i],
                    large = "strong", medium = "strong",
                    small = "borderline", "weak"),
                  inline = TRUE
                )
              )
            })
          )
        )
      )
    })

    ## ── Bar chart ───────────────────────────────────────────────────────────
    output$bar_chart <- renderPlot({
      es <- effect_results()
      req(es, nrow(es) > 0)

      plot_df <- es |>
        dplyr::filter(!is.na(epsilon_squared)) |>
        dplyr::mutate(
          strat_label = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          fill_color = dplyr::case_when(
            effect_size_label == "large" ~ "#27ae60",
            effect_size_label == "medium" ~ "#2980b9",
            effect_size_label == "small" ~ "#f39c12",
            TRUE ~ "#95a5a6"
          )
        )

      if (nrow(plot_df) == 0) return(NULL)

      ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(strat_label, epsilon_squared),
        y = epsilon_squared,
        fill = effect_size_label
      )) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::scale_fill_manual(
          values = c("large" = "#27ae60", "medium" = "#2980b9",
                     "small" = "#f39c12", "negligible" = "#95a5a6"),
          name = "Effect Size"
        ) +
        ggplot2::geom_hline(yintercept = c(0.01, 0.06, 0.14),
                            linetype = "dashed", color = "gray50", alpha = 0.7) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste0("Effect Size: ", rv$metric_config[[rv$current_metric]]$display_name),
          x = NULL,
          y = expression(epsilon^2 ~ "(Epsilon-squared)")
        ) +
        ggplot2::theme_minimal()
    })

    ## ── Effect size table ───────────────────────────────────────────────────
    output$effect_table <- DT::renderDT({
      es <- effect_results()
      req(es)

      display_df <- es |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          epsilon_squared = round(epsilon_squared, 4),
          eta_squared = round(eta_squared, 4),
          rank_biserial_r = round(rank_biserial_r, 4),
          variance_explained_pct = round(variance_explained_pct, 2)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          `Epsilon-sq` = epsilon_squared,
          `Eta-sq` = eta_squared,
          `Rank-biserial r` = rank_biserial_r,
          `Var. Explained %` = variance_explained_pct,
          Label = effect_size_label
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## ── Navigation ──────────────────────────────────────────────────────────
    output$nav_buttons <- renderUI({
      es <- effect_results()
      if (is.null(es)) return(NULL)

      div(
        class = "d-flex justify-content-between mt-3",
        actionButton(ns("back"), "\u25c2 Back to Significance",
                     class = "btn btn-outline-secondary"),
        actionButton(ns("proceed"), "Proceed to Pattern Stability \u25b8",
                     class = "btn btn-primary btn-proceed")
      )
    })

    list(
      proceed = reactive(input$proceed),
      back = reactive(input$back)
    )
  })
}
