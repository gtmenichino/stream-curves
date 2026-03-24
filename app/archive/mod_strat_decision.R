## ── Module: Stratification Decision (Step 6) ─────────────────────────────────
## Consolidated comparison of all layers + final stratification selection.
## Replaces the decision panel from the old mod_strat_screen.R.

library(shiny)
library(bslib)
library(DT)

mod_strat_decision_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 6: Stratification Decision",
      p("This step consolidates results from all 5 evaluation layers into a single
         comparison view. Review the decision flow matrix, consider the system
         recommendation, and select the stratification (or none) to carry forward
         into model building."),
      p(tags$strong("What to review:"), "The matrix of pass/caution/fail across layers.
         Green cells indicate a layer was passed; yellow = caution; red = failed."),
      p(tags$strong("How to decide:"), "Select the stratification with the strongest
         overall profile. If no candidate passes most layers, select 'None'.")
    ),

    uiOutput(ns("decision_ui"))
  )
}

mod_strat_decision_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Build consolidated comparison ───────────────────────────────────────
    consolidated <- reactive({
      req(rv$layer1_screening, rv$layer2_effect_sizes)

      strat_keys <- unique(rv$layer2_effect_sizes$stratification)

      ## Layer 1: significance
      l1 <- rv$layer1_screening$results |>
        dplyr::filter(stratification %in% strat_keys) |>
        dplyr::select(stratification, p_value, classification)

      ## Layer 2: effect size
      l2 <- rv$layer2_effect_sizes |>
        dplyr::filter(stratification %in% strat_keys) |>
        dplyr::select(stratification, epsilon_squared, effect_size_label)

      ## Layer 3: pattern stability (summary)
      l3_summary <- if (!is.null(rv$layer3_patterns) && nrow(rv$layer3_patterns$results) > 0) {
        rv$layer3_patterns$results |>
          dplyr::filter(stratification %in% strat_keys) |>
          dplyr::group_by(stratification) |>
          dplyr::summarise(
            n_stable = sum(stability_rating == "stable", na.rm = TRUE),
            n_total = dplyr::n(),
            stability_pct = round(n_stable / n_total * 100, 0),
            .groups = "drop"
          )
      } else {
        tibble::tibble(stratification = strat_keys, n_stable = NA_integer_,
                       n_total = NA_integer_, stability_pct = NA_real_)
      }

      ## Layer 4: practical relevance
      l4 <- if (!is.null(rv$layer4_relevance) && nrow(rv$layer4_relevance) > 0) {
        rv$layer4_relevance |>
          dplyr::filter(stratification %in% strat_keys) |>
          dplyr::select(stratification, mean_score, relevance_classification)
      } else {
        tibble::tibble(stratification = strat_keys,
                       mean_score = NA_real_, relevance_classification = NA_character_)
      }

      ## Layer 5: feasibility
      l5 <- if (!is.null(rv$layer5_feasibility) && nrow(rv$layer5_feasibility) > 0) {
        rv$layer5_feasibility |>
          dplyr::filter(stratification %in% strat_keys) |>
          dplyr::select(stratification, min_group_n, feasibility_flag)
      } else {
        tibble::tibble(stratification = strat_keys,
                       min_group_n = NA_integer_, feasibility_flag = NA_character_)
      }

      ## Join all layers
      combined <- l1 |>
        dplyr::left_join(l2, by = "stratification") |>
        dplyr::left_join(l3_summary, by = "stratification") |>
        dplyr::left_join(l4, by = "stratification") |>
        dplyr::left_join(l5, by = "stratification")

      ## Auto-recommendation via extended composite score
      auto_decision <- make_stratification_decisions(
        rv$layer1_screening$results,
        rv$layer1_screening$pairwise,
        rv$metric_config,
        rv$strat_config,
        effect_sizes = rv$layer2_effect_sizes,
        relevance_scores = rv$layer4_relevance,
        feasibility = rv$layer5_feasibility
      ) |> dplyr::filter(metric == rv$current_metric)

      rv$strat_decision_auto <- auto_decision

      list(combined = combined, auto_decision = auto_decision)
    })

    ## ── Decision UI ─────────────────────────────────────────────────────────
    output$decision_ui <- renderUI({
      con <- consolidated()
      req(con)

      combined <- con$combined
      auto_dec <- con$auto_decision

      ## Build radio choices
      strat_keys <- combined$stratification
      choices <- c(
        setNames(strat_keys, sapply(strat_keys, function(sk) {
          sc <- rv$strat_config[[sk]]
          p_row <- combined |> dplyr::filter(stratification == sk)
          p_label <- if (nrow(p_row) > 0 && !is.na(p_row$p_value[1])) {
            sprintf(" (p = %.4f)", p_row$p_value[1])
          } else {
            ""
          }
          paste0(sc$display_name %||% sk, p_label)
        })),
        "none" = "None \u2014 skip stratification"
      )

      ## Pre-select recommendation
      selected_val <- if (!is.null(auto_dec) && nrow(auto_dec) > 0 &&
                          auto_dec$decision_type == "single" &&
                          !is.na(auto_dec$selected_strat)) {
        auto_dec$selected_strat
      } else {
        "none"
      }

      tagList(
        ## ── Consolidated comparison table ─────────────────────────────────
        card(
          card_header("Consolidated Comparison"),
          card_body(DT::DTOutput(ns("comparison_table")))
        ),

        ## ── Decision flow matrix (heatmap) ────────────────────────────────
        card(
          card_header("Decision Flow Matrix"),
          card_body(plotOutput(ns("decision_matrix"), height = "300px"))
        ),

        ## ── System recommendation ─────────────────────────────────────────
        card(
          class = if (!is.null(auto_dec) && isTRUE(auto_dec$needs_review)) {
            "border-warning"
          } else {
            "border-success"
          },
          card_header(
            class = if (!is.null(auto_dec) && isTRUE(auto_dec$needs_review)) {
              "bg-warning text-dark"
            } else {
              "bg-success text-white"
            },
            "System Recommendation"
          ),
          card_body(
            if (!is.null(auto_dec) && nrow(auto_dec) > 0 && auto_dec$decision_type == "single") {
              sc <- rv$strat_config[[auto_dec$selected_strat]]
              tagList(
                p(tags$strong("Recommended: "), sc$display_name %||% auto_dec$selected_strat),
                p("Composite scoring: significance \u00d7 0.35 + sample size \u00d7 0.10 +
                   simplicity \u00d7 0.10 + effect size \u00d7 0.20 + relevance \u00d7 0.15 +
                   feasibility check"),
                p(sprintf("p-value: %.4f | groups: %d | min group n: %d",
                           auto_dec$selected_p_value,
                           auto_dec$selected_n_groups,
                           auto_dec$selected_min_n)),
                if (isTRUE(auto_dec$needs_review)) {
                  div(class = "alert alert-warning mt-2 mb-0",
                      tags$strong("Needs review: "), auto_dec$review_reason)
                }
              )
            } else {
              p("No stratification reached significance. Consider skipping stratification.")
            }
          )
        ),

        ## ── User decision ─────────────────────────────────────────────────
        card(
          card_header("Your Decision"),
          card_body(
            radioButtons(ns("strat_choice"), "Select stratification:",
                         choices = choices, selected = selected_val),
            radioButtons(ns("strat_mode"), "Stratification mode:",
                         choices = c("Covariate" = "covariate", "Subset" = "subset"),
                         selected = "covariate", inline = TRUE),
            textAreaInput(ns("final_rationale"), "Justification (required):",
                          placeholder = "Explain your stratification choice...",
                          width = "100%", rows = 3),
            div(
              class = "d-flex justify-content-between mt-3",
              actionButton(ns("back"), "\u25c2 Back to Feasibility",
                           class = "btn btn-outline-secondary"),
              actionButton(ns("confirm"), "Confirm Stratification and Proceed \u25b8",
                           class = "btn btn-primary btn-proceed")
            )
          )
        )
      )
    })

    ## ── Comparison table ────────────────────────────────────────────────────
    output$comparison_table <- DT::renderDT({
      con <- consolidated()
      req(con)

      display_df <- con$combined |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          p_value = round(p_value, 4),
          epsilon_squared = round(epsilon_squared, 4),
          mean_score = round(mean_score, 2)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          `p-value` = p_value,
          `Significance` = classification,
          `Effect (eps-sq)` = epsilon_squared,
          `Effect Label` = effect_size_label,
          `Stability %` = stability_pct,
          `Relevance` = mean_score,
          `Rel. Class` = relevance_classification,
          `Min n` = min_group_n,
          `Feasibility` = feasibility_flag
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## ── Decision matrix heatmap ─────────────────────────────────────────────
    output$decision_matrix <- renderPlot({
      con <- consolidated()
      req(con)

      combined <- con$combined

      ## Build long-form matrix data
      matrix_data <- purrr::map_dfr(seq_len(nrow(combined)), function(i) {
        row <- combined[i, ]
        sk <- row$stratification
        strat_label <- rv$strat_config[[sk]]$display_name %||% sk

        tibble::tibble(
          stratification = strat_label,
          layer = c("Significance", "Effect Size", "Pattern", "Relevance", "Feasibility"),
          status = c(
            if (!is.na(row$p_value) && row$p_value < 0.05) "pass"
            else if (!is.na(row$p_value) && row$p_value < 0.10) "caution"
            else "fail",

            if (!is.na(row$epsilon_squared) && row$epsilon_squared >= 0.06) "pass"
            else if (!is.na(row$epsilon_squared) && row$epsilon_squared >= 0.01) "caution"
            else "fail",

            if (!is.na(row$stability_pct) && row$stability_pct >= 50) "pass"
            else if (!is.na(row$stability_pct) && row$stability_pct >= 25) "caution"
            else if (is.na(row$stability_pct)) "caution"
            else "fail",

            if (!is.na(row$mean_score) && row$mean_score >= 3.5) "pass"
            else if (!is.na(row$mean_score) && row$mean_score >= 2.5) "caution"
            else if (is.na(row$mean_score)) "caution"
            else "fail",

            if (!is.na(row$feasibility_flag) && row$feasibility_flag == "feasible") "pass"
            else if (!is.na(row$feasibility_flag) && row$feasibility_flag == "marginal") "caution"
            else if (is.na(row$feasibility_flag)) "caution"
            else "fail"
          )
        )
      })

      matrix_data$layer <- factor(matrix_data$layer,
        levels = c("Significance", "Effect Size", "Pattern", "Relevance", "Feasibility"))

      ggplot2::ggplot(matrix_data, ggplot2::aes(
        x = layer, y = stratification, fill = status
      )) +
        ggplot2::geom_tile(color = "white", linewidth = 1) +
        ggplot2::scale_fill_manual(
          values = c("pass" = "#27ae60", "caution" = "#f39c12", "fail" = "#e74c3c"),
          name = "Status"
        ) +
        ggplot2::labs(title = "Decision Flow Matrix", x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = ggplot2::element_text(size = 10),
          panel.grid = ggplot2::element_blank()
        )
    })

    ## ── Store decision on confirm ───────────────────────────────────────────
    observeEvent(input$confirm, {
      choice <- input$strat_choice
      rationale <- input$final_rationale %||% ""
      strat_mode <- input$strat_mode %||% "covariate"

      if (choice == "none") {
        rv$strat_decision_user <- tibble::tibble(
          metric = rv$current_metric,
          decision_type = "none",
          selected_strat = NA_character_,
          selected_p_value = NA_real_,
          selected_n_groups = NA_integer_,
          selected_min_n = NA_integer_,
          runner_up_strat = NA_character_,
          runner_up_p_value = NA_real_,
          needs_review = FALSE,
          review_reason = NA_character_,
          notes = rationale
        )
      } else {
        auto_dec <- rv$strat_decision_auto
        if (!is.null(auto_dec) && nrow(auto_dec) > 0 &&
            !is.na(auto_dec$selected_strat) && auto_dec$selected_strat == choice) {
          rv$strat_decision_user <- auto_dec |>
            dplyr::mutate(notes = rationale)
        } else {
          res <- rv$layer1_screening$results
          row <- res |> dplyr::filter(stratification == choice)
          rv$strat_decision_user <- tibble::tibble(
            metric = rv$current_metric,
            decision_type = "single",
            selected_strat = choice,
            selected_p_value = if (nrow(row) > 0) row$p_value[1] else NA_real_,
            selected_n_groups = if (nrow(row) > 0) row$n_groups[1] else NA_integer_,
            selected_min_n = if (nrow(row) > 0) row$min_group_n[1] else NA_integer_,
            runner_up_strat = NA_character_,
            runner_up_p_value = NA_real_,
            needs_review = FALSE,
            review_reason = NA_character_,
            notes = rationale
          )
        }
      }

      ## ── Append to decision log ──────────────────────────────────────────
      auto_dec <- rv$strat_decision_auto
      auto_recommended <- if (!is.null(auto_dec) && nrow(auto_dec) > 0 &&
                              auto_dec$decision_type == "single") {
        auto_dec$selected_strat
      } else {
        "none"
      }

      ## Collect layer scores
      l2_row <- if (!is.null(rv$layer2_effect_sizes)) {
        rv$layer2_effect_sizes |> dplyr::filter(stratification == choice)
      } else {
        tibble::tibble()
      }
      l4_row <- if (!is.null(rv$layer4_relevance)) {
        rv$layer4_relevance |> dplyr::filter(stratification == choice)
      } else {
        tibble::tibble()
      }
      l5_row <- if (!is.null(rv$layer5_feasibility)) {
        rv$layer5_feasibility |> dplyr::filter(stratification == choice)
      } else {
        tibble::tibble()
      }

      new_entry <- tibble::tibble(
        entry_id = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1)),
        timestamp = Sys.time(),
        reviewer_name = "",
        metric = rv$current_metric,
        decision_stage = "stratification",
        selected_strat = if (choice == "none") NA_character_ else choice,
        auto_recommended = auto_recommended,
        user_agreed = (choice == auto_recommended),
        strat_mode = strat_mode,
        layer1_p_value = if (choice != "none") {
          res_row <- rv$layer1_screening$results |> dplyr::filter(stratification == choice)
          if (nrow(res_row) > 0) res_row$p_value[1] else NA_real_
        } else NA_real_,
        layer2_effect_size = if (nrow(l2_row) > 0) l2_row$epsilon_squared[1] else NA_real_,
        layer2_effect_label = if (nrow(l2_row) > 0) l2_row$effect_size_label[1] else NA_character_,
        layer3_stability = NA_character_,
        layer4_relevance_score = if (nrow(l4_row) > 0) l4_row$mean_score[1] else NA_real_,
        layer5_feasibility = if (nrow(l5_row) > 0) l5_row$feasibility_flag[1] else NA_character_,
        selected_predictors = NA_character_,
        selected_bic = NA_real_,
        diagnostics_overall = NA_character_,
        rationale = rationale,
        notes = ""
      )

      rv$decision_log <- dplyr::bind_rows(rv$decision_log, new_entry)
    })

    list(
      proceed = reactive(input$confirm),
      back = reactive(input$back)
    )
  })
}
