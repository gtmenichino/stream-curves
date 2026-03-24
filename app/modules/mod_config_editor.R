## -- Module: Configuration Editor ──────────────────────────────────────────────
## Modal-based editor for all 5 YAML config registries.
## Uses copy-replace pattern for all reactiveValues mutations.
## Modal is shown from within the server (not from the caller) so that
## updateSelectInput() runs in the same flush cycle as showModal().

library(shiny)
library(bslib)
library(DT)

## ══════════════════════════════════════════════════════════════════════════════
## Inline builder (returns navset_pill for embedding directly in a page)
## ══════════════════════════════════════════════════════════════════════════════

build_config_inline <- function(ns) {
  navset_pill(
      ## ── Metrics pill ──────────────────────────────────────────────────────
      nav_panel(
        title = "Metrics",
        icon = bsicons::bs_icon("graph-up"),
        div(
          class = "mt-3",
          ## Summary table
          DT::DTOutput(ns("metric_summary_table")),
          tags$hr(),
          ## Edit form
          layout_column_wrap(
            width = 1 / 2,
            div(
              selectInput(ns("metric_key"), "Select Metric:", choices = NULL),
              ## Read-only fields
              tags$label(class = "form-label", "Column Name"),
              div(class = "config-readonly mb-2", textOutput(ns("m_column_name"), inline = TRUE)),
              tags$label(class = "form-label", "Units"),
              div(class = "config-readonly mb-2", textOutput(ns("m_units"), inline = TRUE)),
              tags$label(class = "form-label", "Family"),
              div(class = "config-readonly mb-2", textOutput(ns("m_family"), inline = TRUE))
            ),
            div(
              textInput(ns("m_display_name"), "Display Name:", value = ""),
              radioButtons(ns("m_higher_is_better"), "Higher is Better:",
                           choices = c("TRUE" = "TRUE", "FALSE" = "FALSE", "NULL" = "NULL"),
                           selected = "TRUE", inline = TRUE),
              numericInput(ns("m_min_sample_size"), "Min Sample Size:", value = 10, min = 3, max = 50),
              selectInput(ns("m_preferred_transform"), "Preferred Transform:",
                          choices = c("none", "log"), selected = "none"),
              radioButtons(ns("m_strat_mode"), "Stratification Mode:",
                           choices = c("covariate", "subset", "auto"),
                           selected = "covariate", inline = TRUE)
            )
          ),
          layout_column_wrap(
            width = 1 / 2,
            div(
              checkboxInput(ns("m_include_in_summary"), "Include in Summary", value = TRUE),
              checkboxInput(ns("m_best_subsets_allowed"), "Best Subsets Allowed", value = TRUE),
              checkboxInput(ns("m_count_model"), "Count Model (read-only)", value = FALSE)
            ),
            div(
              textAreaInput(ns("m_notes"), "Notes:", value = "", rows = 2, resize = "vertical")
            )
          ),
          layout_column_wrap(
            width = 1 / 2,
            div(
              shinyWidgets::pickerInput(
                ns("m_allowed_strats"), "Allowed Stratifications:",
                choices = NULL, multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE, liveSearch = TRUE,
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} of {1} selected",
                  noneSelectedText = "None selected"
                )
              )
            ),
            div(
              shinyWidgets::pickerInput(
                ns("m_allowed_preds"), "Allowed Predictors:",
                choices = NULL, multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE, liveSearch = TRUE,
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} of {1} selected",
                  noneSelectedText = "None selected"
                )
              )
            )
          ),
          ## Action buttons
          div(
            class = "d-flex gap-2 mt-2",
            actionButton(ns("save_metric"), "Save Changes",
                         class = "btn btn-primary", icon = icon("save")),
            actionButton(ns("toggle_add_metric"), "Add Metric",
                         class = "btn btn-outline-success", icon = icon("plus")),
            actionButton(ns("remove_metric"), "Remove Metric",
                         class = "btn btn-outline-danger", icon = icon("trash"))
          ),
          ## Add Metric sub-panel (hidden by default, toggled via server)
          uiOutput(ns("add_metric_panel"))
        )
      ),

      ## ── Stratifications pill ──────────────────────────────────────────────
      nav_panel(
        title = "Stratifications",
        icon = bsicons::bs_icon("layers"),
        div(
          class = "mt-3",
          DT::DTOutput(ns("strat_summary_table")),
          tags$hr(),
          ## Custom Groupings section
          tags$h6("Custom Groupings", class = "fw-bold mb-1"),
          uiOutput(ns("custom_grouping_list")),
          actionButton(ns("toggle_custom_grouping"), "Create Custom Grouping",
                       class = "btn btn-outline-success btn-sm mt-1 mb-3",
                       icon = icon("object-group")),
          tags$hr(),
          div(
            class = "d-flex justify-content-end mb-3",
            actionButton(ns("toggle_add_strat"), "Add New Stratification",
                         class = "btn btn-outline-success", icon = icon("plus"))
          ),
          uiOutput(ns("add_strat_panel")),
          div(
            selectInput(ns("strat_key"), "Edit Stratification:", choices = NULL),
            textInput(ns("s_display_name"), "Display Name:", value = ""),
            numericInput(ns("s_min_group_size"), "Min Group Size:", value = 5, min = 1, max = 30),
            tags$label(class = "form-label", "Detected Levels"),
            div(class = "config-readonly mb-2", textOutput(ns("s_levels"), inline = TRUE)),
            textAreaInput(ns("s_notes"), "Notes:", value = "", rows = 2, resize = "vertical"),
            div(
              class = "d-flex gap-2 mt-2",
              actionButton(ns("save_strat"), "Save Changes",
                           class = "btn btn-primary", icon = icon("save")),
              actionButton(ns("remove_strat"), "Remove Stratification",
                           class = "btn btn-outline-danger", icon = icon("trash"))
            )
          )
        )
      ),

      ## ── Predictors pill ───────────────────────────────────────────────────
      nav_panel(
        title = "Predictors",
        icon = bsicons::bs_icon("bullseye"),
        div(
          class = "mt-3",
          DT::DTOutput(ns("pred_summary_table")),
          tags$hr(),
          selectInput(ns("pred_key"), "Edit Predictor:", choices = NULL),
          layout_column_wrap(
            width = 1 / 2,
            div(
              textInput(ns("p_display_name"), "Display Name:", value = ""),
              selectInput(ns("p_missing_data_rule"), "Missing Data Rule:",
                          choices = c("error", "warn"), selected = "error"),
              textAreaInput(ns("p_notes"), "Notes:", value = "", rows = 2, resize = "vertical"),
              actionButton(ns("save_pred"), "Save Changes",
                           class = "btn btn-primary mt-2", icon = icon("save"))
            ),
            div(
              tags$label(class = "form-label", "Column Name"),
              div(class = "config-readonly mb-2", textOutput(ns("p_column_name"), inline = TRUE)),
              tags$label(class = "form-label", "Derived"),
              div(class = "config-readonly mb-2", textOutput(ns("p_derived"), inline = TRUE)),
              tags$label(class = "form-label", "Derivation"),
              div(class = "config-readonly mb-2", textOutput(ns("p_derivation"), inline = TRUE)),
              tags$label(class = "form-label", "Source Columns"),
              div(class = "config-readonly mb-2", textOutput(ns("p_source_cols"), inline = TRUE)),
              tags$label(class = "form-label", "Expected Range"),
              div(class = "config-readonly mb-2", textOutput(ns("p_expected_range"), inline = TRUE))
            )
          )
        )
      ),

      ## ── Factor Recodes pill ───────────────────────────────────────────────
      nav_panel(
        title = "Factor Recodes",
        icon = bsicons::bs_icon("arrow-left-right"),
        div(
          class = "mt-3",
          uiOutput(ns("recode_accordion"))
        )
      ),

      ## ── Outputs pill ──────────────────────────────────────────────────────
      nav_panel(
        title = "Outputs",
        icon = bsicons::bs_icon("file-earmark-text"),
        div(
          class = "mt-3",
          uiOutput(ns("output_toggles")),
          actionButton(ns("save_outputs"), "Save Output Settings",
                       class = "btn btn-primary mt-3", icon = icon("save"))
        )
      )
  )
}


## ══════════════════════════════════════════════════════════════════════════════
## Modal builder (wraps inline content in a modal dialog)
## ══════════════════════════════════════════════════════════════════════════════

build_config_modal <- function(ns) {
  modalDialog(
    title = "Configuration Editor",
    size = "xl",
    easyClose = TRUE,
    footer = modalButton("Close"),
    class = "config-editor-modal",
    build_config_inline(ns)
  )
}


## ══════════════════════════════════════════════════════════════════════════════
## Server
## ══════════════════════════════════════════════════════════════════════════════

mod_config_editor_server <- function(id, rv, open_trigger = reactive(NULL),
                                     init_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Track toggle states for add panels
    show_add_metric <- reactiveVal(FALSE)
    show_add_strat <- reactiveVal(FALSE)

    ## Helper: increment config version and notify
    bump_config_version <- function() {
      rv$config_version <- isolate(rv$config_version) + 1L
      if (length(isolate(rv$completed_metrics)) > 0) {
        showNotification(
          "Configuration changed. Completed metrics may need re-analysis.",
          type = "warning", duration = 5
        )
      }
    }

    ## ── Helper: build grouped choices and subtext for pickers ─────────────
    build_strat_choices <- function(sc) {
      groups <- list(Single = list(), Paired = list())
      for (sk in names(sc)) {
        s <- sc[[sk]]
        label <- s$display_name %||% sk
        type_group <- if (identical(s$type, "paired")) "Paired" else "Single"
        groups[[type_group]] <- c(groups[[type_group]], setNames(sk, label))
      }
      Filter(function(x) length(x) > 0, groups)
    }

    build_strat_subtext <- function(sc, dat) {
      vapply(names(sc), function(sk) {
        s <- sc[[sk]]
        col <- s$column_name
        if (identical(s$type, "paired")) {
          "Paired interaction"
        } else if (!is.null(col) && col %in% names(dat)) {
          "In data"
        } else {
          "Not in data"
        }
      }, character(1), USE.NAMES = FALSE)
    }

    build_pred_choices <- function(pc) {
      groups <- list(Measured = list(), Derived = list())
      for (pk in names(pc)) {
        p <- pc[[pk]]
        label <- p$display_name %||% pk
        type_group <- if (isTRUE(p$derived)) "Derived" else "Measured"
        groups[[type_group]] <- c(groups[[type_group]], setNames(pk, label))
      }
      Filter(function(x) length(x) > 0, groups)
    }

    build_pred_subtext <- function(pc, dat) {
      vapply(names(pc), function(pk) {
        p <- pc[[pk]]
        col <- p$column_name
        if (!is.null(col) && col %in% names(dat)) "In data" else "Not in data"
      }, character(1), USE.NAMES = FALSE)
    }

    ## ── Populate all dropdowns ────────────────────────────────────────────
    ## Called after showModal() in the same flush cycle so that DOM elements
    ## exist when the update messages arrive.
    populate_all_dropdowns <- function() {
      mc <- isolate(rv$metric_config)
      sc <- isolate(rv$strat_config)
      pc <- isolate(rv$predictor_config)

      ## Metric selector (grouped by family)
      families <- list()
      for (mk in names(mc)) {
        fam <- mc[[mk]]$metric_family
        fam_label <- switch(fam,
          continuous  = "Continuous",
          proportion  = "Proportion",
          count       = "Count",
          categorical = "Categorical",
          fam
        )
        families[[fam_label]] <- c(families[[fam_label]],
                                    setNames(mk, mc[[mk]]$display_name))
      }
      updateSelectInput(session, "metric_key", choices = families)

      ## Strat/pred picker choices for metrics pill
      dat <- isolate(rv$data)
      strat_grouped <- build_strat_choices(sc)
      strat_subtext <- build_strat_subtext(sc, dat)
      shinyWidgets::updatePickerInput(
        session, "m_allowed_strats",
        choices = strat_grouped,
        choicesOpt = list(subtext = strat_subtext)
      )

      pred_grouped <- build_pred_choices(pc)
      pred_subtext <- build_pred_subtext(pc, dat)
      shinyWidgets::updatePickerInput(
        session, "m_allowed_preds",
        choices = pred_grouped,
        choicesOpt = list(subtext = pred_subtext)
      )

      ## Strat selector
      strat_sel_choices <- setNames(
        names(sc),
        sapply(sc, function(s) s$display_name %||% "?")
      )
      updateSelectInput(session, "strat_key", choices = strat_sel_choices)

      ## Pred selector
      pred_sel_choices <- setNames(
        names(pc),
        sapply(pc, function(p) p$display_name %||% "?")
      )
      updateSelectInput(session, "pred_key", choices = pred_sel_choices)
    }

    ## ── Inline initialization trigger ────────────────────────────────────
    ## When config editor is rendered inline (not in a modal), the parent
    ## fires this trigger after the UI is flushed to populate dropdowns.
    observeEvent(init_trigger(), {
      populate_all_dropdowns()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    ## ── Modal open trigger ───────────────────────────────────────────────
    ## The caller passes a reactive wrapping the button click. We show the
    ## modal here (creating DOM), then populate dropdowns (updates find DOM).
    observeEvent(open_trigger(), {
      show_add_metric(FALSE)
      show_add_strat(FALSE)
      showModal(build_config_modal(ns))
      populate_all_dropdowns()
    }, ignoreInit = TRUE)


    ## ════════════════════════════════════════════════════════════════════════
    ## METRICS PILL
    ## ════════════════════════════════════════════════════════════════════════

    ## Metrics summary table
    output$metric_summary_table <- DT::renderDT({
      mc <- rv$metric_config
      rows <- lapply(names(mc), function(mk) {
        m <- mc[[mk]]
        data.frame(
          Key = mk,
          Display = m$display_name %||% mk,
          Family = m$metric_family %||% "",
          Units = m$units %||% "",
          Strats = length(m$allowed_stratifications %||% list()),
          Predictors = length(m$allowed_predictors %||% list()),
          Included = if (isTRUE(m$include_in_summary)) "Yes" else "No",
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, rows)

      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "compact stripe",
        selection = "none"
      ) |>
        DT::formatStyle("Included",
          backgroundColor = DT::styleEqual(c("Yes", "No"), c("#d4edda", "#f8d7da")))
    })

    ## Load selected metric into form
    observeEvent(input$metric_key, {
      req(input$metric_key)
      mc <- rv$metric_config[[input$metric_key]]
      req(mc)

      updateTextInput(session, "m_display_name", value = mc$display_name %||% "")

      hib <- mc$higher_is_better
      hib_val <- if (isTRUE(hib)) "TRUE" else if (isFALSE(hib)) "FALSE" else "NULL"
      updateRadioButtons(session, "m_higher_is_better", selected = hib_val)

      updateNumericInput(session, "m_min_sample_size", value = mc$min_sample_size %||% 10)
      updateSelectInput(session, "m_preferred_transform",
                        selected = mc$preferred_transform %||% "none")
      updateRadioButtons(session, "m_strat_mode",
                          selected = mc$stratification_mode %||% "covariate")

      updateCheckboxInput(session, "m_include_in_summary",
                          value = isTRUE(mc$include_in_summary))
      updateCheckboxInput(session, "m_best_subsets_allowed",
                          value = isTRUE(mc$best_subsets_allowed))
      updateCheckboxInput(session, "m_count_model",
                          value = isTRUE(mc$count_model))

      updateTextAreaInput(session, "m_notes", value = mc$notes %||% "")

      ## Re-populate strat/pred pickers with correct selections for this metric
      sc <- rv$strat_config
      pc <- rv$predictor_config
      dat <- rv$data

      strat_grouped <- build_strat_choices(sc)
      strat_subtext <- build_strat_subtext(sc, dat)
      shinyWidgets::updatePickerInput(
        session, "m_allowed_strats",
        choices = strat_grouped,
        selected = mc$allowed_stratifications %||% character(0),
        choicesOpt = list(subtext = strat_subtext)
      )

      pred_grouped <- build_pred_choices(pc)
      pred_subtext <- build_pred_subtext(pc, dat)
      shinyWidgets::updatePickerInput(
        session, "m_allowed_preds",
        choices = pred_grouped,
        selected = mc$allowed_predictors %||% character(0),
        choicesOpt = list(subtext = pred_subtext)
      )
    }, ignoreInit = TRUE)

    ## Read-only metric displays
    output$m_column_name <- renderText({
      req(input$metric_key)
      rv$metric_config[[input$metric_key]]$column_name %||% ""
    })
    output$m_units <- renderText({
      req(input$metric_key)
      rv$metric_config[[input$metric_key]]$units %||% ""
    })
    output$m_family <- renderText({
      req(input$metric_key)
      rv$metric_config[[input$metric_key]]$metric_family %||% ""
    })

    ## Save metric (copy-replace)
    observeEvent(input$save_metric, {
      req(input$metric_key)
      mk <- input$metric_key
      mc <- isolate(rv$metric_config)

      hib_raw <- input$m_higher_is_better
      hib <- if (hib_raw == "TRUE") TRUE else if (hib_raw == "FALSE") FALSE else NULL

      mc[[mk]]$display_name <- input$m_display_name
      mc[[mk]]$higher_is_better <- hib
      mc[[mk]]$min_sample_size <- input$m_min_sample_size
      mc[[mk]]$preferred_transform <- input$m_preferred_transform
      mc[[mk]]$stratification_mode <- input$m_strat_mode
      mc[[mk]]$include_in_summary <- input$m_include_in_summary
      mc[[mk]]$best_subsets_allowed <- input$m_best_subsets_allowed
      mc[[mk]]$notes <- input$m_notes
      mc[[mk]]$allowed_stratifications <- input$m_allowed_strats
      mc[[mk]]$allowed_predictors <- input$m_allowed_preds

      rv$metric_config <- mc
      bump_config_version()
      showNotification(paste0("Metric '", mk, "' saved."), type = "message", duration = 3)
    })

    ## ── Add Metric toggle ────────────────────────────────────────────────
    observeEvent(input$toggle_add_metric, {
      show_add_metric(!show_add_metric())
    })

    output$add_metric_panel <- renderUI({
      if (!show_add_metric()) return(NULL)

      dat <- rv$data
      mc <- rv$metric_config

      ## Find numeric columns not already registered as metrics
      existing_cols <- sapply(mc, function(m) m$column_name)
      candidate_cols <- names(dat)[sapply(dat, is.numeric)]
      candidate_cols <- setdiff(candidate_cols, existing_cols)

      if (length(candidate_cols) == 0) {
        return(div(
          class = "border rounded p-3 mt-3 bg-light",
          tags$strong("Add New Metric"),
          div(class = "text-muted mt-2", "No unregistered numeric columns in current data.")
        ))
      }

      div(
        class = "border rounded p-3 mt-3 bg-light",
        tags$strong("Add New Metric"),
        tags$hr(class = "my-2"),
        layout_column_wrap(
          width = 1 / 2,
          div(
            selectInput(ns("add_metric_col"), "Column:", choices = c("", candidate_cols)),
            textInput(ns("add_metric_display"), "Display Name:", value = ""),
            selectInput(ns("add_metric_family"), "Metric Family:",
                        choices = c("continuous", "proportion", "count"),
                        selected = "continuous")
          ),
          div(
            radioButtons(ns("add_metric_hib"), "Higher is Better:",
                         choices = c("TRUE" = "TRUE", "FALSE" = "FALSE", "NULL" = "NULL"),
                         selected = "NULL", inline = TRUE),
            numericInput(ns("add_metric_min_n"), "Min Sample Size:",
                         value = 10, min = 3, max = 50),
            actionButton(ns("confirm_add_metric"), "Confirm Add",
                         class = "btn btn-success mt-2", icon = icon("check"))
          )
        )
      )
    })

    ## Auto-fill display name for new metric
    observeEvent(input$add_metric_col, {
      if (!is.null(input$add_metric_col) && input$add_metric_col != "") {
        updateTextInput(session, "add_metric_display", value = input$add_metric_col)
      }
    }, ignoreInit = TRUE)

    ## Confirm add metric
    observeEvent(input$confirm_add_metric, {
      req(input$add_metric_col, input$add_metric_col != "")
      col <- input$add_metric_col
      dat <- isolate(rv$data)
      if (!col %in% names(dat)) {
        showNotification("Column not found in data.", type = "error")
        return()
      }

      display <- if (nchar(input$add_metric_display) > 0) input$add_metric_display else col
      family <- input$add_metric_family %||% "continuous"
      hib_raw <- input$add_metric_hib
      hib <- if (hib_raw == "TRUE") TRUE else if (hib_raw == "FALSE") FALSE else NULL
      min_n <- input$add_metric_min_n %||% 10

      new_metric <- list(
        display_name = display,
        column_name = col,
        units = "",
        metric_family = family,
        higher_is_better = hib,
        monotonic_linear = TRUE,
        allowed_predictors = names(isolate(rv$predictor_config)),
        allowed_stratifications = names(isolate(rv$strat_config)),
        preferred_transform = "none",
        min_sample_size = min_n,
        best_subsets_allowed = TRUE,
        count_model = (family == "count"),
        include_in_summary = TRUE,
        notes = paste0("Added at runtime (", Sys.Date(), ")")
      )

      mc <- isolate(rv$metric_config)
      mc[[col]] <- new_metric
      rv$metric_config <- mc

      bump_config_version()
      show_add_metric(FALSE)
      populate_all_dropdowns()
      showNotification(paste0("Metric '", col, "' added."), type = "message", duration = 3)
    })

    ## Remove metric (runtime-added only)
    observeEvent(input$remove_metric, {
      req(input$metric_key)
      mk <- input$metric_key
      mc <- isolate(rv$metric_config)

      notes <- mc[[mk]]$notes %||% ""
      if (!grepl("Added at runtime", notes, fixed = TRUE)) {
        showNotification("Only runtime-added metrics can be removed.",
                         type = "warning", duration = 4)
        return()
      }

      mc[[mk]] <- NULL
      rv$metric_config <- mc

      bump_config_version()
      populate_all_dropdowns()
      showNotification(paste0("Metric '", mk, "' removed."), type = "message", duration = 3)
    })


    ## ════════════════════════════════════════════════════════════════════════
    ## STRATIFICATIONS PILL
    ## ════════════════════════════════════════════════════════════════════════

    ## Summary table
    output$strat_summary_table <- DT::renderDT({
      sc <- rv$strat_config
      dat <- rv$data

      rows <- lapply(names(sc), function(sk) {
        s <- sc[[sk]]
        col <- s$column_name
        available <- !is.null(col) && col %in% names(dat)
        n_levels <- if (available) length(unique(dat[[col]])) else NA_integer_
        min_n <- if (available) {
          min(table(dat[[col]]), na.rm = TRUE)
        } else {
          NA_integer_
        }

        data.frame(
          Key = sk,
          Display = s$display_name %||% sk,
          Column = col %||% "(paired)",
          Type = s$type %||% "single",
          Levels = n_levels,
          MinGroupN = min_n,
          Available = if (available || s$type == "paired") "Yes" else "No",
          stringsAsFactors = FALSE
        )
      })

      df <- do.call(rbind, rows)

      DT::datatable(
        df,
        options = list(pageLength = 12, scrollX = TRUE, dom = "t"),
        rownames = FALSE,
        class = "compact stripe",
        selection = "none"
      ) |>
        DT::formatStyle("Available",
          backgroundColor = DT::styleEqual(c("Yes", "No"), c("#d4edda", "#f8d7da")))
    })

    ## Load selected strat into form
    observeEvent(input$strat_key, {
      req(input$strat_key)
      s <- rv$strat_config[[input$strat_key]]
      req(s)
      updateTextInput(session, "s_display_name", value = s$display_name %||% "")
      updateNumericInput(session, "s_min_group_size", value = s$min_group_size %||% 5)
      updateTextAreaInput(session, "s_notes", value = s$notes %||% "")
    }, ignoreInit = TRUE)

    output$s_levels <- renderText({
      req(input$strat_key)
      s <- rv$strat_config[[input$strat_key]]
      col <- s$column_name
      if (!is.null(col) && col %in% names(rv$data)) {
        lvls <- sort(unique(as.character(rv$data[[col]])))
        paste(lvls, collapse = ", ")
      } else if (!is.null(s$levels)) {
        paste(s$levels, collapse = ", ")
      } else {
        "(not available)"
      }
    })

    ## Save strat
    observeEvent(input$save_strat, {
      req(input$strat_key)
      sk <- input$strat_key
      sc <- isolate(rv$strat_config)
      sc[[sk]]$display_name <- input$s_display_name
      sc[[sk]]$min_group_size <- input$s_min_group_size
      sc[[sk]]$notes <- input$s_notes
      rv$strat_config <- sc
      bump_config_version()
      showNotification(paste0("Stratification '", sk, "' saved."),
                       type = "message", duration = 3)
    })

    ## ── Add Stratification toggle ────────────────────────────────────────
    observeEvent(input$toggle_add_strat, {
      show_add_strat(!show_add_strat())
    })

    output$add_strat_panel <- renderUI({
      if (!show_add_strat()) return(NULL)

      dat <- rv$data
      sc <- rv$strat_config

      ## Find character/factor columns not already registered
      existing_cols <- sapply(sc, function(s) s$column_name)
      candidate_cols <- names(dat)[sapply(dat, function(x) is.character(x) || is.factor(x))]
      candidate_cols <- setdiff(candidate_cols, existing_cols)

      if (length(candidate_cols) == 0) {
        return(div(
          class = "border rounded p-3 mt-2 bg-light",
          div(class = "text-muted", "No unregistered factor/character columns in current data.")
        ))
      }

      div(
        class = "border rounded p-3 mt-2 bg-light",
        selectInput(ns("add_strat_col"), "Column:", choices = c("", candidate_cols)),
        uiOutput(ns("add_strat_preview")),
        textInput(ns("add_strat_display"), "Display Name:", value = ""),
        numericInput(ns("add_strat_min_n"), "Min Group Size:", value = 5, min = 1, max = 30),
        checkboxGroupInput(ns("add_strat_metrics"), "Add to Metrics:",
                           choices = NULL),
        actionButton(ns("confirm_add_strat"), "Confirm Add",
                     class = "btn btn-success mt-2", icon = icon("check"))
      )
    })

    ## Preview levels when column is selected
    output$add_strat_preview <- renderUI({
      req(input$add_strat_col, input$add_strat_col != "")
      col <- input$add_strat_col
      dat <- rv$data
      if (!col %in% names(dat)) return(NULL)

      counts <- table(dat[[col]], useNA = "ifany")
      items <- paste0(names(counts), " (n=", counts, ")")

      div(
        class = "config-readonly mb-2",
        tags$strong("Detected levels: "),
        paste(items, collapse = ", ")
      )
    })

    ## Populate metric choices when add strat panel opens
    observeEvent(show_add_strat(), {
      if (show_add_strat()) {
        mc <- isolate(rv$metric_config)
        non_cat <- names(mc)[sapply(mc, function(m) m$metric_family != "categorical")]
        choices <- setNames(non_cat, sapply(non_cat, function(mk) mc[[mk]]$display_name))
        updateCheckboxGroupInput(session, "add_strat_metrics",
                                 choices = choices, selected = non_cat)
      }
    }, ignoreInit = TRUE)

    ## Auto-fill display name
    observeEvent(input$add_strat_col, {
      if (!is.null(input$add_strat_col) && input$add_strat_col != "") {
        updateTextInput(session, "add_strat_display", value = input$add_strat_col)
      }
    }, ignoreInit = TRUE)

    ## Confirm add strat
    observeEvent(input$confirm_add_strat, {
      req(input$add_strat_col, input$add_strat_col != "")
      col <- input$add_strat_col
      dat <- isolate(rv$data)
      if (!col %in% names(dat)) {
        showNotification("Column not found in data.", type = "error")
        return()
      }

      lvls <- sort(unique(as.character(dat[[col]])))
      display <- if (nchar(input$add_strat_display) > 0) input$add_strat_display else col
      min_n <- input$add_strat_min_n %||% 5

      ## Build pairwise comparisons
      pairs <- if (length(lvls) >= 2 && length(lvls) <= 8) {
        combn(lvls, 2, simplify = FALSE)
      } else {
        list()
      }

      new_strat <- list(
        display_name = display,
        column_name = col,
        type = "single",
        levels = as.list(lvls),
        min_group_size = min_n,
        pairwise_comparisons = pairs,
        notes = paste0("Added at runtime (", Sys.Date(), ")")
      )

      ## Add to strat_config (copy-replace)
      sc <- isolate(rv$strat_config)
      sk <- col  # use column name as key
      sc[[sk]] <- new_strat
      rv$strat_config <- sc

      ## Add to selected metrics' allowed_stratifications
      selected_metrics <- input$add_strat_metrics
      if (length(selected_metrics) > 0) {
        mc <- isolate(rv$metric_config)
        for (mk in selected_metrics) {
          existing <- mc[[mk]]$allowed_stratifications %||% character(0)
          if (!sk %in% existing) {
            mc[[mk]]$allowed_stratifications <- c(existing, sk)
          }
        }
        rv$metric_config <- mc
      }

      bump_config_version()
      show_add_strat(FALSE)
      populate_all_dropdowns()
      showNotification(paste0("Stratification '", sk, "' added."),
                       type = "message", duration = 3)
    })

    ## Remove stratification (only runtime-added ones)
    observeEvent(input$remove_strat, {
      req(input$strat_key)
      sk <- input$strat_key
      sc <- isolate(rv$strat_config)

      notes <- sc[[sk]]$notes %||% ""
      if (!grepl("Added at runtime", notes, fixed = TRUE)) {
        showNotification("Only runtime-added stratifications can be removed.",
                         type = "warning", duration = 4)
        return()
      }

      ## Remove from strat_config
      sc[[sk]] <- NULL
      rv$strat_config <- sc

      ## Remove from all metrics' allowed_stratifications
      mc <- isolate(rv$metric_config)
      for (mk in names(mc)) {
        existing <- mc[[mk]]$allowed_stratifications
        if (!is.null(existing) && sk %in% existing) {
          mc[[mk]]$allowed_stratifications <- setdiff(existing, sk)
        }
      }
      rv$metric_config <- mc

      bump_config_version()
      populate_all_dropdowns()
      showNotification(paste0("Stratification '", sk, "' removed."),
                       type = "message", duration = 3)
    })


    ## ── Custom Groupings ──────────────────────────────────────────────────────

    ## Render list of existing custom groupings
    output$custom_grouping_list <- renderUI({
      cg <- rv$custom_groupings
      if (length(cg) == 0) {
        return(tags$p(class = "text-muted small mb-1", "No custom groupings yet."))
      }

      tags$div(
        lapply(names(cg), function(cg_key) {
          info <- cg[[cg_key]]
          group_labels <- paste(names(info$collapse_map), collapse = ", ")
          base_display <- info$base_display_name %||% info$base_key
          div(
            class = "d-flex align-items-start justify-content-between mb-1 p-2 border rounded",
            div(
              tags$strong(info$display_name),
              tags$br(),
              tags$span(class = "text-muted small",
                        paste0("Base: ", base_display)),
              tags$br(),
              tags$span(class = "text-muted small",
                        paste0("Groups: ", group_labels))
            ),
            actionButton(ns(paste0("delete_cg_", cg_key)), NULL,
                         class = "btn btn-outline-danger btn-sm py-0 px-1",
                         icon = icon("trash"))
          )
        })
      )
    })

    ## Open custom grouping modal
    observeEvent(input$toggle_custom_grouping, {
      ## Build base stratification choices (single-type, non-custom, column in data)
      base_choices <- character(0)
      for (sk in names(rv$strat_config)) {
        sc <- rv$strat_config[[sk]]
        if (isTRUE(sc$is_custom_grouping)) next
        if (is.null(sc$type) || sc$type != "single") next
        if (is.null(sc$column_name) || !sc$column_name %in% names(rv$data)) next
        base_choices <- c(base_choices, setNames(sk, sc$display_name))
      }

      if (length(base_choices) == 0) {
        showNotification("No base stratifications available.", type = "warning")
        return()
      }

      showModal(modalDialog(
        title = "Create Custom Grouping",
        size = "l",
        easyClose = TRUE,
        selectInput(ns("cg_base_strat"), "Base Stratification:",
                    choices = base_choices),
        uiOutput(ns("cg_level_info")),
        numericInput(ns("cg_num_groups"), "Number of groups:", value = 2, min = 2, max = 10),
        uiOutput(ns("cg_level_assignments")),
        textInput(ns("cg_display_name"), "Display Name:",
                  placeholder = "e.g., Bed Material (Coarse/Gravel/Fine)"),
        numericInput(ns("cg_min_group_size"), "Min Group Size:", value = 5, min = 1, max = 20),
        uiOutput(ns("cg_preview_output")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("cg_preview"), "Preview", class = "btn btn-outline-info",
                       icon = icon("eye")),
          actionButton(ns("cg_create"), "Create Grouping", class = "btn btn-success",
                       icon = icon("plus"))
        )
      ))
    })

    ## Show available levels with sample sizes
    output$cg_level_info <- renderUI({
      req(input$cg_base_strat)
      sc <- rv$strat_config[[input$cg_base_strat]]
      req(sc, sc$column_name %in% names(rv$data))

      col_data <- rv$data[[sc$column_name]]
      col_data <- col_data[!is.na(col_data)]
      level_counts <- sort(table(col_data), decreasing = TRUE)

      level_labels <- paste0(names(level_counts), " (n=", level_counts, ")")
      tags$p(class = "text-muted small",
             tags$strong("Available levels: "), paste(level_labels, collapse = ", "))
    })

    ## Dynamic group assignment inputs
    output$cg_level_assignments <- renderUI({
      req(input$cg_base_strat, input$cg_num_groups)
      sc <- rv$strat_config[[input$cg_base_strat]]
      req(sc, sc$column_name %in% names(rv$data))

      col_data <- rv$data[[sc$column_name]]
      all_levels <- sort(unique(as.character(col_data[!is.na(col_data)])))
      n_groups <- min(input$cg_num_groups, length(all_levels))

      tagList(
        lapply(seq_len(n_groups), function(i) {
          div(
            class = "mb-2",
            layout_column_wrap(
              width = 1 / 2,
              textInput(ns(paste0("cg_group_name_", i)),
                        paste0("Group ", i, " name:"),
                        value = ""),
              selectInput(ns(paste0("cg_group_levels_", i)),
                          paste0("Group ", i, " levels:"),
                          choices = all_levels,
                          multiple = TRUE)
            )
          )
        })
      )
    })

    ## Preview handler
    observeEvent(input$cg_preview, {
      req(input$cg_base_strat, input$cg_num_groups)
      sc <- rv$strat_config[[input$cg_base_strat]]
      req(sc, sc$column_name %in% names(rv$data))

      col_data <- rv$data[[sc$column_name]]
      all_levels <- sort(unique(as.character(col_data[!is.na(col_data)])))
      n_groups <- min(input$cg_num_groups, length(all_levels))

      ## Collect group definitions
      collapse_map <- list()
      group_names <- character(0)
      assigned_levels <- character(0)
      errors <- character(0)

      for (i in seq_len(n_groups)) {
        gname <- trimws(input[[paste0("cg_group_name_", i)]] %||% "")
        glevels <- input[[paste0("cg_group_levels_", i)]]

        if (gname == "") {
          errors <- c(errors, paste0("Group ", i, " name is empty."))
        } else if (gname %in% group_names) {
          errors <- c(errors, paste0("Duplicate group name: '", gname, "'."))
        }
        group_names <- c(group_names, gname)

        dup_levels <- intersect(glevels, assigned_levels)
        if (length(dup_levels) > 0) {
          errors <- c(errors, paste0("Level(s) assigned to multiple groups: ",
                                     paste(dup_levels, collapse = ", ")))
        }
        assigned_levels <- c(assigned_levels, glevels)
        collapse_map[[gname]] <- glevels
      }

      unassigned <- setdiff(all_levels, assigned_levels)
      if (length(unassigned) > 0) {
        errors <- c(errors, paste0("Unassigned levels: ", paste(unassigned, collapse = ", ")))
      }

      output$cg_preview_output <- renderUI({
        if (length(errors) > 0) {
          div(class = "alert alert-danger mt-2",
              tags$strong("Validation errors:"),
              tags$ul(lapply(errors, tags$li)))
        } else {
          ## Compute trial grouping
          trial_col <- forcats::fct_collapse(factor(col_data), !!!collapse_map)
          size_table <- as.data.frame(table(trial_col))
          names(size_table) <- c("Group", "n")
          min_size <- input$cg_min_group_size %||% 5

          div(class = "alert alert-info mt-2",
              tags$strong("Preview \u2014 group sizes:"),
              tags$table(
                class = "table table-sm table-bordered mt-1 mb-0",
                tags$thead(tags$tr(tags$th("Group"), tags$th("n"), tags$th("Status"))),
                tags$tbody(
                  lapply(seq_len(nrow(size_table)), function(r) {
                    flag <- if (size_table$n[r] < min_size) {
                      tags$span(class = "text-danger", "Below minimum")
                    } else {
                      tags$span(class = "text-success", "OK")
                    }
                    tags$tr(
                      tags$td(as.character(size_table$Group[r])),
                      tags$td(size_table$n[r]),
                      tags$td(flag)
                    )
                  })
                )
              )
          )
        }
      })
    })

    ## Create handler
    observeEvent(input$cg_create, {
      req(input$cg_base_strat, input$cg_num_groups)
      sc <- rv$strat_config[[input$cg_base_strat]]
      req(sc, sc$column_name %in% names(rv$data))

      col_data <- rv$data[[sc$column_name]]
      all_levels <- sort(unique(as.character(col_data[!is.na(col_data)])))
      n_groups <- min(input$cg_num_groups, length(all_levels))
      base_key <- input$cg_base_strat

      ## Collect group definitions
      collapse_map <- list()
      group_names <- character(0)
      assigned_levels <- character(0)
      errors <- character(0)

      for (i in seq_len(n_groups)) {
        gname <- trimws(input[[paste0("cg_group_name_", i)]] %||% "")
        glevels <- input[[paste0("cg_group_levels_", i)]]

        if (gname == "") errors <- c(errors, paste0("Group ", i, " name is empty."))
        else if (gname %in% group_names) errors <- c(errors, paste0("Duplicate group name: '", gname, "'."))
        group_names <- c(group_names, gname)

        dup_levels <- intersect(glevels, assigned_levels)
        if (length(dup_levels) > 0) {
          errors <- c(errors, paste0("Level(s) in multiple groups: ", paste(dup_levels, collapse = ", ")))
        }
        assigned_levels <- c(assigned_levels, glevels)
        if (length(glevels) > 0) collapse_map[[gname]] <- glevels
      }

      unassigned <- setdiff(all_levels, assigned_levels)
      if (length(unassigned) > 0) {
        errors <- c(errors, paste0("Unassigned levels: ", paste(unassigned, collapse = ", ")))
      }

      if (length(errors) > 0) {
        showNotification(
          paste0("Cannot create grouping: ", errors[1]),
          type = "error", duration = 5
        )
        return()
      }

      ## Generate unique key
      counter <- rv$custom_grouping_counter[[base_key]] %||% 0L
      counter <- counter + 1L
      rv$custom_grouping_counter[[base_key]] <- counter
      new_key <- paste0(base_key, "_custom_", counter)
      new_col <- new_key

      ## Ensure no column collision
      if (new_col %in% names(rv$data)) {
        showNotification("Column name collision \u2014 try again.", type = "error")
        return()
      }

      ## Create data column
      rv$data[[new_col]] <- forcats::fct_collapse(
        factor(col_data), !!!collapse_map
      )

      ## Build display name
      display_name <- trimws(input$cg_display_name %||% "")
      if (display_name == "") {
        display_name <- paste0(sc$display_name %||% base_key, " (Grouped)")
      }

      ## Compute group sizes for registry
      group_sizes <- as.list(table(rv$data[[new_col]]))

      ## Build pairwise comparisons
      new_levels <- names(collapse_map)
      pairwise <- if (length(new_levels) >= 2) {
        combn_mat <- utils::combn(new_levels, 2)
        lapply(seq_len(ncol(combn_mat)), function(j) {
          list(group1 = combn_mat[1, j], group2 = combn_mat[2, j])
        })
      } else {
        list()
      }

      ## Add to strat_config
      rv$strat_config[[new_key]] <- list(
        display_name = display_name,
        column_name = new_col,
        type = "single",
        levels = new_levels,
        min_group_size = input$cg_min_group_size %||% 5L,
        pairwise_comparisons = pairwise,
        is_custom_grouping = TRUE,
        base_stratification = base_key
      )

      ## Add to ALL metrics' allowed_stratifications
      mc <- isolate(rv$metric_config)
      for (mk in names(mc)) {
        allowed <- mc[[mk]]$allowed_stratifications %||% character(0)
        if (!(new_key %in% allowed)) {
          mc[[mk]]$allowed_stratifications <- c(allowed, new_key)
        }
      }
      rv$metric_config <- mc

      ## Register in custom_groupings
      rv$custom_groupings[[new_key]] <- list(
        key = new_key,
        column_name = new_col,
        display_name = display_name,
        base_key = base_key,
        base_display_name = sc$display_name %||% base_key,
        collapse_map = collapse_map,
        group_sizes = group_sizes,
        min_group_size = input$cg_min_group_size %||% 5L
      )

      ## Close modal, refresh config editor state, and notify
      removeModal()
      bump_config_version()
      populate_all_dropdowns()
      showNotification(
        paste0("Created custom grouping: ", display_name),
        type = "message", duration = 4
      )
    })

    ## Delete custom grouping handlers
    observe({
      cg <- rv$custom_groupings
      lapply(names(cg), function(cg_key) {
        local({
          local_key <- cg_key
          observeEvent(input[[paste0("delete_cg_", local_key)]], {
            info <- rv$custom_groupings[[local_key]]
            if (!is.null(info)) {
              ## Remove data column
              if (info$column_name %in% names(rv$data)) {
                rv$data[[info$column_name]] <- NULL
              }
              ## Remove from strat_config
              rv$strat_config[[local_key]] <- NULL
              ## Remove from all metrics' allowed_stratifications
              mc <- isolate(rv$metric_config)
              for (mk in names(mc)) {
                allowed <- mc[[mk]]$allowed_stratifications
                if (!is.null(allowed) && local_key %in% allowed) {
                  mc[[mk]]$allowed_stratifications <- setdiff(allowed, local_key)
                }
              }
              rv$metric_config <- mc
              ## Remove cached screening results referencing this key
              for (mk in names(rv$all_layer1_results)) {
                cached <- rv$all_layer1_results[[mk]]
                if (!is.null(cached) && local_key %in% cached$stratification) {
                  rv$all_layer1_results[[mk]] <- cached |>
                    dplyr::filter(stratification != local_key)
                }
              }
              ## Remove from registry
              rv$custom_groupings[[local_key]] <- NULL
            }
            bump_config_version()
            populate_all_dropdowns()
            showNotification(
              paste0("Deleted custom grouping: ", info$display_name),
              type = "message", duration = 3
            )
          }, once = TRUE, ignoreInit = TRUE)
        })
      })
    })


    ## ════════════════════════════════════════════════════════════════════════
    ## PREDICTORS PILL
    ## ════════════════════════════════════════════════════════════════════════

    ## Summary table
    output$pred_summary_table <- DT::renderDT({
      pc <- rv$predictor_config
      dat <- rv$data

      rows <- lapply(names(pc), function(pk) {
        p <- pc[[pk]]
        col <- p$column_name
        available <- !is.null(col) && col %in% names(dat)
        data.frame(
          Key = pk,
          Display = p$display_name %||% pk,
          Column = col %||% "",
          Type = p$type %||% "",
          Derived = if (isTRUE(p$derived)) "Yes" else "No",
          Available = if (available) "Yes" else "No",
          stringsAsFactors = FALSE
        )
      })

      df <- do.call(rbind, rows)

      DT::datatable(
        df,
        options = list(pageLength = 20, scrollX = TRUE, dom = "t"),
        rownames = FALSE,
        class = "compact stripe",
        selection = "none"
      ) |>
        DT::formatStyle("Available",
          backgroundColor = DT::styleEqual(c("Yes", "No"), c("#d4edda", "#f8d7da")))
    })

    observeEvent(input$pred_key, {
      req(input$pred_key)
      p <- rv$predictor_config[[input$pred_key]]
      req(p)
      updateTextInput(session, "p_display_name", value = p$display_name %||% "")
      updateSelectInput(session, "p_missing_data_rule",
                        selected = p$missing_data_rule %||% "error")
      updateTextAreaInput(session, "p_notes", value = p$notes %||% "")
    }, ignoreInit = TRUE)

    output$p_column_name <- renderText({
      req(input$pred_key)
      rv$predictor_config[[input$pred_key]]$column_name %||% ""
    })
    output$p_derived <- renderText({
      req(input$pred_key)
      p <- rv$predictor_config[[input$pred_key]]
      if (isTRUE(p$derived)) "Yes" else "No"
    })
    output$p_derivation <- renderText({
      req(input$pred_key)
      rv$predictor_config[[input$pred_key]]$derivation %||% "(none)"
    })
    output$p_source_cols <- renderText({
      req(input$pred_key)
      sc <- rv$predictor_config[[input$pred_key]]$source_columns
      if (!is.null(sc)) paste(sc, collapse = ", ") else "(none)"
    })
    output$p_expected_range <- renderText({
      req(input$pred_key)
      er <- rv$predictor_config[[input$pred_key]]$expected_range
      if (!is.null(er)) paste(er, collapse = " - ") else "(none)"
    })

    ## Save predictor
    observeEvent(input$save_pred, {
      req(input$pred_key)
      pk <- input$pred_key
      pc <- isolate(rv$predictor_config)
      pc[[pk]]$display_name <- input$p_display_name
      pc[[pk]]$missing_data_rule <- input$p_missing_data_rule
      pc[[pk]]$notes <- input$p_notes
      rv$predictor_config <- pc
      bump_config_version()
      showNotification(paste0("Predictor '", pk, "' saved."),
                       type = "message", duration = 3)
    })


    ## ════════════════════════════════════════════════════════════════════════
    ## FACTOR RECODES PILL
    ## ════════════════════════════════════════════════════════════════════════

    output$recode_accordion <- renderUI({
      frc <- rv$factor_recode_config
      if (length(frc) == 0) {
        return(div(class = "text-muted", "No factor recode rules defined."))
      }

      panels <- lapply(names(frc), function(rk) {
        r <- frc[[rk]]
        collapse_map <- r$collapse_map %||% list()

        ## Display collapse map as formatted text
        map_items <- lapply(names(collapse_map), function(new_lvl) {
          old_lvls <- collapse_map[[new_lvl]]
          div(
            class = "mb-1",
            tags$strong(new_lvl), " \u2190 ",
            tags$span(class = "text-muted",
                      paste(old_lvls, collapse = ", "))
          )
        })

        bslib::accordion_panel(
          title = paste0(rk, " (", r$source_column, " \u2192 ", r$target_column, ")"),
          div(
            tags$label(class = "form-label", "Source Column"),
            div(class = "config-readonly mb-2", r$source_column %||% ""),
            tags$label(class = "form-label", "Target Column"),
            div(class = "config-readonly mb-2", r$target_column %||% ""),
            tags$label(class = "form-label", "Collapse Map"),
            div(class = "border rounded p-2 mb-2", map_items),
            tags$label(class = "form-label", "Notes"),
            div(class = "config-readonly mb-2", r$notes %||% "(none)")
          )
        )
      })

      bslib::accordion(
        id = ns("recode_acc"),
        open = FALSE,
        !!!panels
      )
    })


    ## ════════════════════════════════════════════════════════════════════════
    ## OUTPUTS PILL
    ## ════════════════════════════════════════════════════════════════════════

    output$output_toggles <- renderUI({
      oc <- rv$output_config
      if (is.null(oc)) return(div(class = "text-muted", "No output config loaded."))

      sections <- list()

      ## Per-metric outputs
      if (!is.null(oc$per_metric_outputs)) {
        items <- lapply(names(oc$per_metric_outputs), function(ok) {
          o <- oc$per_metric_outputs[[ok]]
          checkboxInput(ns(paste0("out_pm_", ok)), ok,
                        value = isTRUE(o$enabled))
        })
        sections <- c(sections, list(
          tags$h6("Per-Metric Outputs"),
          tagList(items),
          tags$hr()
        ))
      }

      ## Summary outputs
      if (!is.null(oc$summary_outputs)) {
        items <- lapply(names(oc$summary_outputs), function(ok) {
          o <- oc$summary_outputs[[ok]]
          checkboxInput(ns(paste0("out_sum_", ok)), ok,
                        value = isTRUE(o$enabled))
        })
        sections <- c(sections, list(
          tags$h6("Summary Outputs"),
          tagList(items),
          tags$hr()
        ))
      }

      ## Report products
      if (!is.null(oc$report_products)) {
        items <- lapply(names(oc$report_products), function(ok) {
          o <- oc$report_products[[ok]]
          label <- paste0(ok, " (Tier ", o$tier %||% "?", ", ", o$format %||% "?", ")")
          checkboxInput(ns(paste0("out_rpt_", ok)), label,
                        value = isTRUE(o$enabled))
        })
        sections <- c(sections, list(
          tags$h6("Report Products"),
          tagList(items)
        ))
      }

      tagList(sections)
    })

    ## Save output settings
    observeEvent(input$save_outputs, {
      oc <- isolate(rv$output_config)

      ## Per-metric outputs
      if (!is.null(oc$per_metric_outputs)) {
        for (ok in names(oc$per_metric_outputs)) {
          val <- input[[paste0("out_pm_", ok)]]
          if (!is.null(val)) oc$per_metric_outputs[[ok]]$enabled <- val
        }
      }

      ## Summary outputs
      if (!is.null(oc$summary_outputs)) {
        for (ok in names(oc$summary_outputs)) {
          val <- input[[paste0("out_sum_", ok)]]
          if (!is.null(val)) oc$summary_outputs[[ok]]$enabled <- val
        }
      }

      ## Report products
      if (!is.null(oc$report_products)) {
        for (ok in names(oc$report_products)) {
          val <- input[[paste0("out_rpt_", ok)]]
          if (!is.null(val)) oc$report_products[[ok]]$enabled <- val
        }
      }

      rv$output_config <- oc
      bump_config_version()
      showNotification("Output settings saved.", type = "message", duration = 3)
    })

  })
}
