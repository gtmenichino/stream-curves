## ── Module: Phase 3 — Final Stratification Verification ─────────────────────────
## Combines pattern stability + feasibility + interpretability into focused review.
## Per-metric: verify finalist stratifications and confirm selection.

library(shiny)
library(bslib)
library(DT)

mod_phase3_verification_ui <- function(id, dialog_mode = FALSE) {
  ns <- NS(id)
  uiOutput(ns("phase3_page"))
}

mod_phase3_verification_server <- function(id, rv, parent_session = NULL, dialog_mode = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    prev_metric <- reactiveVal(NULL)
    verify_data <- reactiveVal(NULL)

    restore_verify_state <- function(metric = rv$current_metric) {
      if (is.null(metric) || identical(metric, "")) {
        verify_data(NULL)
        return(invisible(NULL))
      }

      needs_phase1 <- metric_needs_phase1_artifact_refresh(rv, metric)
      needs_phase3 <- metric_needs_phase3_artifact_refresh(rv, metric)
      total_steps <- sum(needs_phase1, needs_phase3)

      if (total_steps > 0) {
        withProgress(message = "Loading Phase 3 workspace...", value = 0, {
          if (isTRUE(needs_phase1)) {
            incProgress(0, detail = "Regenerating Phase 1 boxplots...")
            ensure_metric_phase1_artifacts(rv, metric)
            incProgress(1 / total_steps)
          }
          if (isTRUE(needs_phase3)) {
            incProgress(0, detail = "Regenerating Phase 3 verification outputs...")
            ensure_metric_phase3_artifacts(rv, metric)
            incProgress(1 / total_steps)
          }
        })
      }

      verify_data(get_metric_phase3_display_state(rv, metric))
      invisible(NULL)
    }

    ## ── Data gate: show alert or full page ────────────────────────────────────
    output$phase3_page <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())

      if (isTRUE(dialog_mode)) {
        return(div(
          class = "workspace-phase-body",
          explanation_card(
            "Phase 3: Final Stratification Verification",
            p("Does the top candidate stratification hold up under focused review?"),
            p("Verify finalist stratifications through pattern stability (LOESS),
               feasibility assessment (sample sizes), and interpretability. Then
               confirm your selection to carry forward into Phase 4."),
            p(tags$strong("Requires:"), " Phase 1 completed for this metric.")
          ),
          uiOutput(ns("metric_info_brief")),
          uiOutput(ns("verification_ui"))
        ))
      }

      layout_sidebar(
        ## ── Sidebar ─────────────────────────────────────────────────────────────
        sidebar = sidebar(
          width = 280,
          title = "Phase 3: Verify",
          uiOutput(ns("metric_picker")),
          uiOutput(ns("metric_info_brief"))
        ),

        ## ── Main content ────────────────────────────────────────────────────────
        tagList(
          explanation_card(
            "Phase 3: Final Stratification Verification",
            p("Does the top candidate stratification hold up under focused review?"),
            p("Verify finalist stratifications through pattern stability (LOESS),
               feasibility assessment (sample sizes), and interpretability. Then
               confirm your selection to carry forward into Phase 4."),
            p(tags$strong("Requires:"), " Phase 1 completed for this metric.")
          ),

          uiOutput(ns("verification_ui"))
        )
      )
    })

    ## ── Metric picker ─────────────────────────────────────────────────────────
    output$metric_picker <- renderUI({
      mc <- rv$metric_config
      families <- list()
      for (mk in names(mc)) {
        fam <- mc[[mk]]$metric_family
        if (fam == "categorical") next
        fam_label <- switch(fam,
          continuous = "Continuous", proportion = "Proportion",
          count = "Count", fam)
        families[[fam_label]] <- c(families[[fam_label]], setNames(mk, mc[[mk]]$display_name))
      }

      selectInput(ns("metric_select"), "Select Metric:",
                  choices = families,
                  selected = rv$current_metric %||% "perRiffle")
    })

    ## Metric change
    observeEvent(input$metric_select, {
      old_metric <- prev_metric()
      new_metric <- input$metric_select
      if (!is.null(old_metric) && identical(old_metric, new_metric)) return()

      if (!is.null(old_metric) && old_metric != "") {
        save_metric_phase_state(rv, old_metric)
      }
      rv$current_metric <- new_metric
      prev_metric(new_metric)
      restore_metric_phase_state(rv, new_metric)
      restore_verify_state(new_metric)
    }, ignoreInit = TRUE)

    observeEvent(rv$workspace_modal_nonce, {
      if (isTRUE(dialog_mode) && identical(rv$workspace_modal_type, "phase3")) {
        restore_verify_state(rv$current_metric)
      }
    }, ignoreInit = TRUE)

    ## Brief metric info
    output$metric_info_brief <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      req(mc)
      div(
        class = "metric-info-card mt-2",
        p(tags$strong(mc$display_name), tags$br(),
          tags$span(class = "text-muted", mc$metric_family, " | ", mc$units))
      )
    })

    ## ── Get finalist stratifications ──────────────────────────────────────────
    finalists <- reactive({
      req(rv$current_metric)
      metric <- rv$current_metric

      ## Union of Phase 1 "Promising"/"Possible" + Phase 2 carry-forward selections
      p1_cands <- rv$phase1_candidates[[metric]]
      p2_selected <- get_metric_phase2_passed(rv, metric)

      strats <- character(0)
      p1_status <- list()
      p2_tier <- list()

      if (!is.null(p1_cands) && nrow(p1_cands) > 0) {
        kept <- p1_cands |>
          dplyr::filter(candidate_status %in% c("promising", "possible"))
        strats <- unique(kept$stratification)
        for (i in seq_len(nrow(p1_cands))) {
          p1_status[[p1_cands$stratification[i]]] <- p1_cands$candidate_status[i]
        }
      }

      if (length(p2_selected) > 0) {
        strats <- unique(c(strats, p2_selected))
      }

      if (!is.null(rv$phase2_ranking) && nrow(rv$phase2_ranking) > 0) {
        for (i in seq_len(nrow(rv$phase2_ranking))) {
          p2_tier[[rv$phase2_ranking$stratification[i]]] <- rv$phase2_ranking$tier[i]
        }
      }

      if (length(strats) == 0) return(NULL)

      tibble::tibble(
        stratification = strats,
        phase1_status = sapply(strats, function(sk) p1_status[[sk]] %||% "unknown"),
        phase2_tier = sapply(strats, function(sk) p2_tier[[sk]] %||% "N/A")
      )
    })

    ## ── Main verification UI ──────────────────────────────────────────────────
    output$verification_ui <- renderUI({
      fin <- finalists()

      if (is.null(fin) || nrow(fin) == 0) {
        return(div(
          class = "alert alert-info mt-3",
          icon("info-circle"),
          " No finalist stratifications found. Complete Phase 1 for this metric first,
            and mark at least one stratification as Promising or Possible."
        ))
      }

      tagList(
        ## Finalist table
        card(
          card_header("Finalist Stratifications"),
          card_body(
            p(class = "text-muted", "Select which finalists to verify."),
            lapply(seq_len(nrow(fin)), function(i) {
              sk <- fin$stratification[i]
              sc <- rv$strat_config[[sk]]
              div(
                class = "d-flex align-items-center gap-3 mb-2",
                checkboxInput(ns(paste0("verify_", sk)),
                              sc$display_name %||% sk, value = TRUE),
                status_badge(
                  switch(fin$phase1_status[i],
                    promising = "pass", possible = "caution", "not_applicable"),
                  paste0("P1: ", fin$phase1_status[i])
                ),
                if (fin$phase2_tier[i] != "N/A") {
                  status_badge(
                    switch(fin$phase2_tier[i],
                      "Broad-Use Candidate" = "pass",
                      "Metric-Specific Candidate" = "caution",
                      "not_applicable"),
                    paste0("P2: ", fin$phase2_tier[i])
                  )
                }
              )
            }),
            actionButton(ns("run_verification"), "Run Verification Checks",
                         class = "btn btn-primary mt-2", icon = icon("play"))
          )
        ),

        ## Verification results
        uiOutput(ns("verify_results_ui"))
      )
    })

    ## ── Run verification ──────────────────────────────────────────────────────
    observeEvent(input$run_verification, {
      fin <- finalists()
      req(fin)

      checked_strats <- fin$stratification[sapply(fin$stratification, function(sk) {
        isTRUE(input[[paste0("verify_", sk)]])
      })]
      req(length(checked_strats) > 0)

      mc <- rv$metric_config[[rv$current_metric]]
      predictor_keys <- mc$allowed_predictors

      withProgress(message = "Running verification...", value = 0, {
        ## Pattern stability
        all_pattern_results <- list()
        all_pattern_plots <- list()

        for (sk in c("none", checked_strats)) {
          sk_actual <- if (sk == "none") NULL else sk
          incProgress(0.3 / (length(checked_strats) + 1))
          res <- tryCatch(
            assess_pattern_stability(
              rv$data, rv$current_metric, sk_actual,
              predictor_keys, rv$metric_config,
              rv$strat_config, rv$predictor_config
            ),
            error = function(e) list(results = tibble::tibble(), plots = list())
          )
          if (nrow(res$results) > 0) {
            all_pattern_results <- c(all_pattern_results, list(res$results))
          }
          all_pattern_plots <- c(all_pattern_plots, res$plots)
        }

        pattern_combined <- dplyr::bind_rows(all_pattern_results)
        rv$phase3_patterns <- list(results = pattern_combined, plots = all_pattern_plots)

        ## Feasibility
        incProgress(0.3, detail = "Assessing feasibility...")
        feas <- assess_feasibility(rv$data, checked_strats, rv$strat_config)
        rv$phase3_feasibility <- feas
      })

      if (is.null(rv$metric_phase_cache[[rv$current_metric]])) {
        rv$metric_phase_cache[[rv$current_metric]] <- list()
      }
      rv$metric_phase_cache[[rv$current_metric]]$phase3_patterns <- rv$phase3_patterns
      rv$metric_phase_cache[[rv$current_metric]]$phase3_feasibility <- rv$phase3_feasibility
      rv$metric_phase_cache[[rv$current_metric]]$phase3_artifact_mode <- "full"

      verify_data(list(
        strats = checked_strats,
        patterns = rv$phase3_patterns,
        feasibility = rv$phase3_feasibility
      ))
    })

    ## ── Verification results UI ───────────────────────────────────────────────
    output$verify_results_ui <- renderUI({
      vd <- verify_data()
      if (is.null(vd)) return(NULL)

      tagList(
        ## Part A: Focused boxplots
        if (!is.null(rv$phase1_screening) && length(rv$phase1_screening$plots) > 0) {
          finalist_plots <- rv$phase1_screening$plots[
            intersect(names(rv$phase1_screening$plots), vd$strats)]
          if (length(finalist_plots) > 0) {
            do.call(
              navset_card_tab,
              c(
                list(title = "Part A: Focused Boxplots"),
                lapply(names(finalist_plots), function(sk) {
                  nav_panel(
                    title = rv$strat_config[[sk]]$display_name %||% sk,
                    plotOutput(ns(paste0("bp_", sk)), height = "450px")
                  )
                })
              )
            )
          }
        },

        ## Part B: Pattern Stability
        if (!is.null(vd$patterns) && nrow(vd$patterns$results) > 0) {
          tagList(
            card(
              card_header("Part B: Pattern Stability"),
              card_body(DT::DTOutput(ns("pattern_table")))
            ),
            if (length(vd$patterns$plots) > 0) {
              card(
                card_header("Pattern Scatterplots"),
                card_body(
                  selectInput(ns("pattern_plot_select"), "Select plot:",
                              choices = names(vd$patterns$plots), width = "400px"),
                  plotOutput(ns("pattern_plot"), height = "400px")
                )
              )
            },
            ## Pattern quality scoring
            card(
              card_header("Pattern Quality Scoring"),
              card_body(
                lapply(vd$strats, function(sk) {
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
                    )
                  )
                })
              )
            )
          )
        },

        ## Part C: Feasibility Assessment
        if (!is.null(vd$feasibility) && nrow(vd$feasibility) > 0) {
          tagList(
            card(
              card_header("Part C: Feasibility Assessment"),
              card_body(DT::DTOutput(ns("feas_table")))
            ),
            card(
              card_header("Qualitative Assessment"),
              card_body(
                lapply(vd$strats, function(sk) {
                  sc <- rv$strat_config[[sk]]
                  feas_row <- vd$feasibility |> dplyr::filter(stratification == sk)
                  auto_flag <- if (nrow(feas_row) > 0) feas_row$feasibility_flag[1] else "unknown"

                  div(
                    class = "mb-3 p-3 border rounded",
                    tags$h6(sc$display_name %||% sk,
                            status_badge(
                              switch(auto_flag,
                                feasible = "pass", marginal = "caution",
                                infeasible = "fail", "not_applicable"),
                              auto_flag
                            )),
                    checkboxGroupInput(
                      ns(paste0("feas_checks_", sk)), NULL,
                      choices = c(
                        "Sample sizes adequate in each group" = "adequate_n",
                        "Group boundaries understandable" = "clear_boundaries",
                        "Stratification granularity appropriate" = "appropriate_grain",
                        "Can be explained to method users" = "explainable",
                        "Does not create too many final curves" = "manageable_curves"
                      ),
                      inline = FALSE
                    )
                  )
                })
              )
            )
          )
        },

        ## Part D: Interpretability
        card(
          card_header("Part D: Interpretability"),
          card_body(
            lapply(vd$strats, function(sk) {
              sc <- rv$strat_config[[sk]]
              div(
                class = "mb-3 p-3 border rounded",
                tags$h6(sc$display_name %||% sk),
                radioButtons(
                  ns(paste0("interp_eco_", sk)),
                  "Ecologically defensible?",
                  choices = c("Yes" = "yes", "No" = "no"),
                  selected = "yes", inline = TRUE
                ),
                radioButtons(
                  ns(paste0("interp_prac_", sk)),
                  "Understandable to practitioners?",
                  choices = c("Yes" = "yes", "No" = "no"),
                  selected = "yes", inline = TRUE
                ),
                radioButtons(
                  ns(paste0("interp_curves_", sk)),
                  "Manageable number of curves?",
                  choices = c("Yes" = "yes", "No" = "no"),
                  selected = "yes", inline = TRUE
                ),
                bslib::accordion(
                  bslib::accordion_panel(
                    "Advanced Scoring (Likert)",
                    sliderInput(ns(paste0("likert_eco_", sk)),
                                "Ecological controls alignment:", 1, 5, 3, 1, width = "250px"),
                    sliderInput(ns(paste0("likert_prac_", sk)),
                                "Practitioner understanding:", 1, 5, 3, 1, width = "250px"),
                    sliderInput(ns(paste0("likert_def_", sk)),
                                "Defensible in documentation:", 1, 5, 3, 1, width = "250px"),
                    sliderInput(ns(paste0("likert_interp_", sk)),
                                "Curve interpretability:", 1, 5, 3, 1, width = "250px"),
                    sliderInput(ns(paste0("likert_mgmt_", sk)),
                                "Management utility:", 1, 5, 3, 1, width = "250px")
                  ),
                  open = FALSE
                )
              )
            })
          )
        ),

        ## Verification decision panel
        card(
          class = "border-primary",
          card_header(class = "bg-primary text-white", "Verification Decision"),
          card_body(
            p(class = "text-muted", "Rate each finalist and select one for model building."),
            lapply(vd$strats, function(sk) {
              sc <- rv$strat_config[[sk]]
              div(
                class = "d-flex align-items-center gap-3 mb-2",
                tags$strong(sc$display_name %||% sk, style = "min-width: 200px;"),
                radioButtons(
                  ns(paste0("verify_status_", sk)), NULL,
                  choices = c("Verified" = "verified",
                              "Verified-with-Caution" = "caution",
                              "Not Recommended" = "not_recommended"),
                  selected = "verified",
                  inline = TRUE
                )
              )
            }),
            tags$hr(),
            radioButtons(ns("final_strat_choice"), "Select stratification for Phase 4:",
                         choices = c(
                           setNames(vd$strats, sapply(vd$strats, function(sk) {
                             rv$strat_config[[sk]]$display_name %||% sk
                           })),
                           "none" = "None \u2014 skip stratification"
                         ),
                         selected = {
                           current_choice <- get_metric_phase3_selected(rv, rv$current_metric)
                           if (current_choice %in% c(vd$strats, "none")) current_choice else vd$strats[1]
                         }),
            radioButtons(ns("strat_mode"), "Stratification mode:",
                         choices = c("Covariate" = "covariate", "Subset" = "subset"),
                         selected = "covariate", inline = TRUE),
            textAreaInput(ns("justification"), "Justification (required):",
                          placeholder = "Explain your stratification choice...",
                          width = "100%", rows = 3),
            div(
              class = "d-flex justify-content-end mt-3",
              actionButton(ns("confirm_phase3"), "Confirm and Proceed to Phase 4 \u25b8",
                           class = "btn btn-primary btn-proceed")
            )
          )
        )
      )
    })

    ## ── Render boxplots ───────────────────────────────────────────────────────
    observe({
      vd <- verify_data()
      req(vd)
      if (!is.null(rv$phase1_screening) && length(rv$phase1_screening$plots) > 0) {
        for (sk in vd$strats) {
          local({
            local_sk <- sk
            output[[paste0("bp_", local_sk)]] <- renderPlot({
              rv$phase1_screening$plots[[local_sk]]
            })
          })
        }
      }
    })

    ## ── Pattern table ─────────────────────────────────────────────────────────
    output$pattern_table <- DT::renderDT({
      vd <- verify_data()
      req(vd, nrow(vd$patterns$results) > 0)

      display_df <- vd$patterns$results |>
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

    ## Pattern plot
    output$pattern_plot <- renderPlot({
      vd <- verify_data()
      req(vd, input$pattern_plot_select, input$pattern_plot_select %in% names(vd$patterns$plots))
      vd$patterns$plots[[input$pattern_plot_select]]
    })

    ## ── Feasibility table ─────────────────────────────────────────────────────
    output$feas_table <- DT::renderDT({
      vd <- verify_data()
      req(vd, nrow(vd$feasibility) > 0)

      display_df <- vd$feasibility |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          })
        ) |>
        dplyr::select(
          Stratification = strat_display,
          Levels = n_levels,
          `Min Group n` = min_group_n,
          `Max Group n` = max_group_n,
          `% Sparse` = pct_sparse_cells,
          `Completeness %` = data_completeness_pct,
          Flag = feasibility_flag
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "Flag",
          backgroundColor = DT::styleEqual(
            c("feasible", "marginal", "infeasible"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    ## ── Confirm Phase 3 ──────────────────────────────────────────────────────
    observeEvent(input$confirm_phase3, {
      vd <- verify_data()
      req(vd)

      choice <- input$final_strat_choice
      rationale <- input$justification %||% ""
      strat_mode <- input$strat_mode %||% "covariate"

      ## Store verification results per metric
      rv$phase3_verification[[rv$current_metric]] <- list(
        finalists = vd$strats,
        pattern_results = vd$patterns,
        feasibility_results = vd$feasibility,
        verification_status = setNames(
          sapply(vd$strats, function(sk) input[[paste0("verify_status_", sk)]] %||% "verified"),
          vd$strats
        ),
        selected_strat = choice,
        justification = rationale
      )

      if (is.null(rv$metric_phase_cache[[rv$current_metric]])) {
        rv$metric_phase_cache[[rv$current_metric]] <- list()
      }
      rv$metric_phase_cache[[rv$current_metric]]$phase3_patterns <- rv$phase3_patterns
      rv$metric_phase_cache[[rv$current_metric]]$phase3_feasibility <- rv$phase3_feasibility
      rv$metric_phase_cache[[rv$current_metric]]$phase3_artifact_mode <- "full"

      ## Set strat_decision_user (same field name — Phase 4 reads this)
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
        l1_results <- rv$all_layer1_results[[rv$current_metric]]
        row <- if (!is.null(l1_results)) {
          l1_results |> dplyr::filter(stratification == choice)
        } else {
          tibble::tibble()
        }

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
      rv$metric_phase_cache[[rv$current_metric]]$strat_decision_user <- rv$strat_decision_user

      ## Append to decision log
      new_entry <- tibble::tibble(
        entry_id = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1)),
        timestamp = Sys.time(),
        reviewer_name = "",
        metric = rv$current_metric,
        decision_stage = "stratification",
        phase = "phase3",
        selected_strat = if (choice == "none") NA_character_ else choice,
        auto_recommended = NA_character_,
        user_agreed = NA,
        strat_mode = strat_mode,
        layer1_p_value = if (!is.null(rv$phase1_screening)) {
          row <- rv$phase1_screening$results |> dplyr::filter(stratification == choice)
          if (nrow(row) > 0) row$p_value[1] else NA_real_
        } else NA_real_,
        layer2_effect_size = if (!is.null(rv$phase1_effect_sizes)) {
          row <- rv$phase1_effect_sizes |> dplyr::filter(stratification == choice)
          if (nrow(row) > 0) row$epsilon_squared[1] else NA_real_
        } else NA_real_,
        layer2_effect_label = NA_character_,
        layer3_stability = NA_character_,
        layer4_relevance_score = NA_real_,
        layer5_feasibility = if (!is.null(vd$feasibility) && nrow(vd$feasibility) > 0 && choice != "none") {
          row <- vd$feasibility |> dplyr::filter(stratification == choice)
          if (nrow(row) > 0) row$feasibility_flag[1] else NA_character_
        } else NA_character_,
        selected_predictors = NA_character_,
        selected_bic = NA_real_,
        diagnostics_overall = NA_character_,
        rationale = rationale,
        notes = ""
      )
      rv$decision_log <- dplyr::bind_rows(rv$decision_log, new_entry)

      showNotification(
        paste0("Phase 3 confirmed for ", rv$metric_config[[rv$current_metric]]$display_name,
               ". Proceed to Phase 4."),
        type = "message", duration = 3
      )
      notify_workspace_refresh(rv)
      removeModal(session = parent_session %||% session)
      launch_workspace_modal(rv, "phase4", rv$current_metric)
    })
  })
}
