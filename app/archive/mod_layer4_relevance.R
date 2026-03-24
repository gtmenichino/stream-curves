## ── Module: Layer 4 — Practical Relevance (Step 4) ───────────────────────────
## Expert judgment scoring form for ecological and management relevance.

library(shiny)
library(bslib)

mod_layer4_relevance_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 4: Practical Relevance (Layer 4)",
      p("This layer captures expert judgment about whether a stratification is
         ecologically meaningful and useful for management. Statistical significance
         and effect size are necessary but not sufficient \u2014 the stratification
         must also be interpretable and defensible."),
      p(tags$strong("What to review:"), "Consider each question from an ecological
         and practical perspective. Score honestly \u2014 these scores inform the final
         decision composite."),
      p(tags$strong("How to decide:"), "Mean score >= 3.5 = High relevance,
         2.5\u20133.4 = Medium, < 2.5 = Low.")
    ),

    uiOutput(ns("relevance_ui")),
    uiOutput(ns("nav_buttons"))
  )
}

mod_layer4_relevance_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Build scoring forms ─────────────────────────────────────────────────
    output$relevance_ui <- renderUI({
      req(rv$layer2_effect_sizes)
      strat_keys <- unique(rv$layer2_effect_sizes$stratification)

      tagList(
        lapply(strat_keys, function(sk) {
          sc <- rv$strat_config[[sk]]
          strat_name <- sc$display_name %||% sk

          card(
            card_header(paste0("Scoring: ", strat_name)),
            card_body(
              ## Q1: Ecological controls
              div(
                class = "mb-3",
                tags$label(class = "form-label",
                           "Does this stratification align with known ecological controls?"),
                sliderInput(ns(paste0("q1_", sk)), NULL,
                            min = 1, max = 5, value = 3, step = 1,
                            ticks = TRUE, width = "300px")
              ),
              ## Q2: Practitioner understanding
              div(
                class = "mb-3",
                tags$label(class = "form-label",
                           "Is it understandable to practitioners?"),
                sliderInput(ns(paste0("q2_", sk)), NULL,
                            min = 1, max = 5, value = 3, step = 1,
                            ticks = TRUE, width = "300px")
              ),
              ## Q3: Defensibility
              div(
                class = "mb-3",
                tags$label(class = "form-label",
                           "Would it be defensible in documentation?"),
                sliderInput(ns(paste0("q3_", sk)), NULL,
                            min = 1, max = 5, value = 3, step = 1,
                            ticks = TRUE, width = "300px")
              ),
              ## Q4: Curve interpretability
              div(
                class = "mb-3",
                tags$label(class = "form-label",
                           "Does it improve interpretability of the reference curve?"),
                sliderInput(ns(paste0("q4_", sk)), NULL,
                            min = 1, max = 5, value = 3, step = 1,
                            ticks = TRUE, width = "300px")
              ),
              ## Q5: Management utility
              div(
                class = "mb-3",
                tags$label(class = "form-label",
                           "Is it useful for management or assessment application?"),
                sliderInput(ns(paste0("q5_", sk)), NULL,
                            min = 1, max = 5, value = 3, step = 1,
                            ticks = TRUE, width = "300px")
              ),
              ## Rationale
              textAreaInput(ns(paste0("rationale_", sk)),
                           "Rationale (required if any score < 3):",
                           placeholder = "Explain your reasoning...",
                           width = "100%", rows = 2)
            )
          )
        })
      )
    })

    ## ── Navigation ──────────────────────────────────────────────────────────
    output$nav_buttons <- renderUI({
      req(rv$layer2_effect_sizes)

      div(
        class = "d-flex justify-content-between mt-3",
        actionButton(ns("back"), "\u25c2 Back to Pattern Stability",
                     class = "btn btn-outline-secondary"),
        actionButton(ns("proceed"), "Proceed to Feasibility \u25b8",
                     class = "btn btn-primary btn-proceed")
      )
    })

    ## ── Collect scores on proceed ───────────────────────────────────────────
    observeEvent(input$proceed, {
      req(rv$layer2_effect_sizes)
      strat_keys <- unique(rv$layer2_effect_sizes$stratification)

      scores <- purrr::map_dfr(strat_keys, function(sk) {
        q1 <- input[[paste0("q1_", sk)]] %||% 3
        q2 <- input[[paste0("q2_", sk)]] %||% 3
        q3 <- input[[paste0("q3_", sk)]] %||% 3
        q4 <- input[[paste0("q4_", sk)]] %||% 3
        q5 <- input[[paste0("q5_", sk)]] %||% 3
        rationale <- input[[paste0("rationale_", sk)]] %||% ""

        mean_score <- mean(c(q1, q2, q3, q4, q5))
        classification <- dplyr::case_when(
          mean_score >= 3.5 ~ "high",
          mean_score >= 2.5 ~ "medium",
          TRUE ~ "low"
        )

        tibble::tibble(
          metric = rv$current_metric,
          stratification = sk,
          q1_ecological = q1,
          q2_practitioner = q2,
          q3_defensible = q3,
          q4_interpretability = q4,
          q5_management = q5,
          mean_score = round(mean_score, 2),
          relevance_classification = classification,
          rationale = rationale
        )
      })

      rv$layer4_relevance <- scores
      rv$all_layer4_results[[rv$current_metric]] <- scores
    })

    list(
      proceed = reactive(input$proceed),
      back = reactive(input$back)
    )
  })
}
