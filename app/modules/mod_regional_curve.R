## ── Module: Regional Curves (Page 3) ─────────────────────────────────────────
## Power-function log-log curves for bankfull metrics.

library(shiny)
library(bslib)
library(DT)

REGIONAL_RESPONSES <- c(
  "Bankfull Width (ft)"  = "BW_ft",
  "Bankfull Depth (ft)"  = "BD_ft",
  "Bankfull Area (ft\u00b2)" = "BA_ft2"
)

REGIONAL_PREDICTORS <- c(
  "Drainage Area (km\u00b2)" = "DA_km2",
  "Drainage Area (mi\u00b2)" = "DA_mi2"
)

mod_regional_curve_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("regional_page"))
}

mod_regional_curve_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    regional_result <- reactiveVal(NULL)

    ## ── Data gate: show alert or full page ────────────────────────────────────
    output$regional_page <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())

      tagList(
        explanation_card(
          "Regional / Hydraulic Geometry Curves",
          p("Regional curves describe the power-function relationship between bankfull
             channel dimensions and drainage area: Y = a \u00d7 X^b. These are fit
             via log-log linear regression and are a separate workstream from the
             standard scoring curves."),
          p("Select a response variable, predictor, and optional stratification,
             then fit the curve.")
        ),

        ## Exploration
        card(
          card_header("Exploration"),
          card_body(
            layout_column_wrap(
              width = 1 / 2,
              selectInput(ns("response"), "Response Variable:",
                          choices = REGIONAL_RESPONSES),
              uiOutput(ns("exploration_strat_picker_ui"))
            ),
            uiOutput(ns("exploration_boxplots_ui"))
          )
        ),

        ## Curve Settings
        card(
          card_header("Curve Settings"),
          card_body(
            layout_column_wrap(
              width = 1 / 2,
              selectInput(ns("predictor"), "Predictor Variable:",
                          choices = REGIONAL_PREDICTORS),
              uiOutput(ns("stratify_ui"))
            ),
            div(
              actionButton(ns("fit_curve"), "Fit Regional Curve",
                           class = "btn btn-primary mt-2",
                           icon = icon("chart-line"))
            )
          )
        ),

        ## Results
        uiOutput(ns("results_ui"))
      )
    })

    ## ── Dynamic exploration strat picker ─────────────────────────────────────
    output$exploration_strat_picker_ui <- renderUI({
      base_choices <- character(0)
      cg_choices <- character(0)
      for (sk in names(rv$strat_config)) {
        sc <- rv$strat_config[[sk]]
        if (is.null(sc$type) || sc$type != "single") next
        if (is.null(sc$column_name) || !sc$column_name %in% names(rv$data)) next
        if (isTRUE(sc$is_custom_grouping)) {
          cg_choices <- c(cg_choices, setNames(sk, sc$display_name %||% sk))
        } else {
          base_choices <- c(base_choices, setNames(sk, sc$display_name %||% sk))
        }
      }

      if (length(cg_choices) > 0) {
        choices <- list("Base" = base_choices, "Custom Groupings" = cg_choices)
      } else {
        choices <- base_choices
      }

      ## Default: the 4 original strats that exist in choices
      defaults <- intersect(
        c("Ecoregion", "DACAT", "StreamType2"),
        unname(base_choices)
      )

      shinyWidgets::pickerInput(
        ns("exploration_strats"), "Stratifications:",
        choices = choices, selected = defaults, multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          selectedTextFormat = "count > 3",
          countSelectedText = "{0} of {1} selected",
          noneSelectedText = "None selected"
        )
      )
    })

    ## ── Dynamic stratify dropdown ───────────────────────────────────────────
    output$stratify_ui <- renderUI({
      base_choices <- c("None" = "none")
      custom_choices <- character(0)
      for (sk in names(rv$strat_config)) {
        sc <- rv$strat_config[[sk]]
        if (is.null(sc$type) || sc$type != "single") next
        if (is.null(sc$column_name) || !sc$column_name %in% names(rv$data)) next
        if (isTRUE(sc$is_custom_grouping)) {
          custom_choices <- c(custom_choices, setNames(sk, sc$display_name %||% sk))
        } else {
          base_choices <- c(base_choices, setNames(sk, sc$display_name %||% sk))
        }
      }

      if (length(custom_choices) > 0) {
        choices <- list("Base" = base_choices, "Custom Groupings" = custom_choices)
      } else {
        choices <- base_choices
      }

      selectInput(ns("stratify"), "Stratify by:", choices = choices)
    })

    ## ── Exploration boxplots (reactive to response + strat picker) ──────────
    exploration_boxplots <- reactive({
      req(input$response, input$exploration_strats)
      response_col <- input$response
      response_label <- names(REGIONAL_RESPONSES)[REGIONAL_RESPONSES == response_col]
      if (length(response_label) == 0) response_label <- response_col

      strat_keys <- input$exploration_strats

      boxplots <- list()
      for (sk in strat_keys) {
        sc <- rv$strat_config[[sk]]
        if (is.null(sc) || is.null(sc$column_name)) next
        if (!sc$column_name %in% names(rv$data)) next
        bp <- build_regional_boxplot(
          data = rv$data,
          response_col = response_col,
          response_label = response_label,
          strat_col = sc$column_name,
          strat_label = sc$display_name %||% sk,
          pairwise_comparisons = sc$pairwise_comparisons
        )
        if (!is.null(bp)) boxplots[[sk]] <- bp
      }
      boxplots
    })

    output$exploration_boxplots_ui <- renderUI({
      bps <- exploration_boxplots()
      req(length(bps) > 0)

      bp_tabs <- lapply(names(bps), function(sk) {
        sc <- rv$strat_config[[sk]]
        tab_label <- if (!is.null(sc)) (sc$display_name %||% sk) else sk
        nav_panel(
          title = tab_label,
          plotOutput(ns(paste0("rc_bp_", sk)), height = "450px")
        )
      })
      do.call(navset_card_tab, c(list(title = "Stratification Boxplots"), bp_tabs))
    })

    observe({
      bps <- exploration_boxplots()
      req(length(bps) > 0)
      for (sk in names(bps)) {
        local({
          local_sk <- sk
          output[[paste0("rc_bp_", local_sk)]] <- renderPlot({
            exploration_boxplots()[[local_sk]]
          })
        })
      }
    })

    ## ── Fit curve ─────────────────────────────────────────────────────────────
    observeEvent(input$fit_curve, {
      req(input$response, input$predictor)

      group_var <- if (is.null(input$stratify) || input$stratify == "none") NULL else input$stratify

      result <- tryCatch(
        fit_regional_curve(
          rv$data,
          response_var = input$response,
          predictor_var = input$predictor,
          group_var = group_var
        ),
        error = function(e) {
          showNotification(
            paste("Error fitting regional curve:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          NULL
        }
      )

      if (is.null(result)) {
        regional_result(NULL)
        return()
      }

      regional_result(result)
    })

    ## ── Results UI ──────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- regional_result()
      req(res)

      tagList(
        ## Log-log scatter plot
        if (!is.null(res$plot)) {
          card(
            card_header("Log-Log Scatter Plot"),
            card_body(plotOutput(ns("regional_plot"), height = "500px"))
          )
        },

        ## Model summary table
        card(
          card_header("Model Summary"),
          card_body(DT::DTOutput(ns("model_summary_table")))
        ),

        ## Mark complete
        div(
          class = "d-flex justify-content-end mt-3",
          actionButton(ns("mark_complete"), "Mark Complete \u2713",
                       class = "btn btn-success",
                       icon = icon("check"))
        )
      )
    })

    output$regional_plot <- renderPlot({
      res <- regional_result()
      req(res, !is.null(res$plot))
      res$plot
    })

    output$model_summary_table <- DT::renderDT({
      res <- regional_result()
      req(res)

      display_df <- res$model_summary |>
        dplyr::mutate(
          equation = ifelse(
            !is.na(coefficient_a),
            sprintf("%s = %.3f \u00d7 %s^%.3f",
                     response, coefficient_a, predictor, exponent_b),
            "N/A"
          ),
          r_squared = round(r_squared, 4),
          adj_r2 = round(adj_r2, 4),
          p_value = round(p_value, 6)
        ) |>
        dplyr::select(group_level, equation, n_obs, r_squared, adj_r2, p_value, fit_status)

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## Mark complete
    observeEvent(input$mark_complete, {
      res <- regional_result()
      req(res)

      key <- paste0("regional_", input$response)
      rv$completed_metrics[[key]] <- list(
        type = "regional",
        response = input$response,
        predictor = input$predictor,
        stratify = input$stratify,
        model_summary = res$model_summary,
        plot = res$plot
      )

      showNotification(
        paste0("Regional curve for ", input$response, " marked complete!"),
        type = "message", duration = 3
      )
    })
  })
}
