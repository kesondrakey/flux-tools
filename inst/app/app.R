# pkgs <- c("shiny", "plotly", "dplyr", "htmltools")
# for (p in pkgs) {
#   if (!requireNamespace(p, quietly = TRUE)) {
#     install.packages(p)
#   }
# }
# Now they’re guaranteed to be installed:
library(shiny)
library(plotly)
library(dplyr)

# Allow larger uploads (here: up to 100 MB)
options(shiny.maxRequestSize = 100 * 1024^2)

# ─────────────────────────────────────────────────────────────────────────────
# Helper: turn an integer offset (e.g. -5) into an “Etc/GMT” string
make_local_tz <- function(offset) {
  if (offset == 0) return("UTC")
  sign_chr <- if (offset < 0) "+" else "-"
  abs_hr   <- abs(offset)
  paste0("Etc/GMT", sign_chr, abs_hr)
}

ui <- fluidPage(
  titlePanel("fluxtools: Interactive QA/QC with Code Generator"),
  uiOutput("subtitle"),
  sidebarLayout(
    sidebarPanel(
      width = 4,

      # 1) file upload
      fileInput(
        "csv_file",
        "Upload Ameriflux-style .csv:",
        accept = ".csv"
      ),

      # 1a) YEAR selector (populated after upload)
      selectizeInput(
        "year_sel",
        "Select Year(s):",
        choices  = NULL,
        multiple = TRUE,
        options  = list(
          placeholder    = "– upload a file to see year(s) –",
          onInitialize   = I("function() { this.setValue('All'); }"),
          plugins        = list("remove_button")
        )
      ),


      hr(),

      # 2) dropdowns for x‐ and y‐variables
      selectInput("yvar", "Y-axis:", choices = NULL),
      selectInput("xvar", "X-axis:", choices = NULL),

      # 3) “Add current selection” and “Reset Current Selection” side by side
      fluidRow(
        column(
          width = 6,
          actionButton("add_sel", "Add current selection to accumulated", width = "100%")
        ),
        column(
          width = 6,
          actionButton(
            "clear_sel",
            HTML("<span style='color:#474747; font-weight:bold;'>Reset Current Selection</span>"),
            width = "100%"
          )
        )
      ),

      # 4) “Remove from accumulated” and “Reset ALL Filters” side by side
      fluidRow(
        column(
          width = 6,
          actionButton("remove_acc", "Remove from accumulated", width = "100%")
        ),
        column(
          width = 6,
          actionButton(
            "reset_all",
            HTML("<span style='color:#2E2E2E; font-weight:bold;'>Reset ALL Filters</span>"),
            width = "100%"
          )
        )
      ),

      # 5) a bit of spacing, then “Confirm Remove” on its own line
      tags$br(),
      actionButton("remove", "Confirm Remove"),

      hr(),

      # 6) Outlier slider + buttons
      sliderInput(
        "sd_thresh",
        "Highlight points beyond (σ):",
        min   = 0,
        max   = 3,
        value = 0,    # ← default is 0σ
        step  = 1
      ),
      checkboxInput("show_reg", "Show regression line & R²", value = TRUE),
      fluidRow(
        column(
          width = 6,
          actionButton("add_outliers", "Add all ±σ outliers", width = "100%")
        ),
        column(
          width = 6,
          actionButton("clear_outliers", "Clear ±σ outlier selection", width = "100%")
        )
      ),

      hr(),

      # 7) Preview selection
      h4("Preview selection:"),
      div(
        style = "max-height:200px; overflow-y:auto; padding:4px; border:1px solid #ddd;",
        tableOutput("preview")
      ),

      hr(),

      # 8) Current‐selection code
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        h4("Current‐selection code:"),
        # Single‐line JS, no stray line breaks:
        #this code is to make the "copy all" button work!
        tags$button(
          "Copy all",
          type = "button",
          onclick = HTML("
    (function() {
      var txtNode = document.getElementById('code_current');
      if (!txtNode) {
        alert('Could not find element to copy!');
        return;
      }
      var textToCopy = txtNode.innerText;

      // First, try modern Clipboard API
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(textToCopy)
          .then(function() { /* success – silently do nothing */ })
          .catch(function(err) {
            // If Clipboard API fails, fall back
            fallbackCopyTextToClipboard(textToCopy);
          });
      } else {
        // If Clipboard API unavailable, fall back
        fallbackCopyTextToClipboard(textToCopy);
      }

      // Fallback implementation:
      function fallbackCopyTextToClipboard(text) {
        var textArea = document.createElement('textarea');
        textArea.value = text;
        // Avoid scrolling to bottom
        textArea.style.position = 'fixed';
        textArea.style.top =  '0';
        textArea.style.left = '0';
        textArea.style.width = '1px';
        textArea.style.height = '1px';
        textArea.style.padding = '0';
        textArea.style.border = 'none';
        textArea.style.outline = 'none';
        textArea.style.boxShadow = 'none';
        textArea.style.background = 'transparent';
        document.body.appendChild(textArea);
        textArea.select();
        try {
          var successful = document.execCommand('copy');
          if (!successful) {
            alert('Fallback: Oops, unable to copy');
          }
        } catch (err) {
          alert('Fallback: Oops, unable to copy');
        }
        document.body.removeChild(textArea);
      }
    })();
  ")
        )
        #end button code
      ),
      div(
        style = paste0(
          "max-height: 180px; overflow-y: auto; padding: 4px; ",
          "border: 1px solid #ddd; background: #f9f9f9;"
        ),
        verbatimTextOutput("code_current")
      ),

      hr(),

      # 9) Accumulated‐selection code
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        h4("Accumulated‐selection code:"),
        #this code is to make the "copy all" button work!
        tags$button(
          "Copy all",
          type = "button",
          onclick = HTML("
    (function() {
      var txtNode = document.getElementById('code_all');
      if (!txtNode) {
        alert('Could not find element to copy!');
        return;
      }
      var textToCopy = txtNode.innerText || txtNode.textContent;

      // First, try modern Clipboard API
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(textToCopy)
          .catch(function(err) {
            // If Clipboard API fails, fall back
            fallbackCopyTextToClipboard(textToCopy);
          });
      } else {
        // If Clipboard API unavailable, fall back
        fallbackCopyTextToClipboard(textToCopy);
      }

      // Fallback implementation:
      function fallbackCopyTextToClipboard(text) {
        var textArea = document.createElement('textarea');
        textArea.value = text;
        // Avoid scrolling to bottom
        textArea.style.position = 'fixed';
        textArea.style.top =  '0';
        textArea.style.left = '0';
        textArea.style.width = '1px';
        textArea.style.height = '1px';
        textArea.style.padding = '0';
        textArea.style.border = 'none';
        textArea.style.outline = 'none';
        textArea.style.boxShadow = 'none';
        textArea.style.background = 'transparent';
        document.body.appendChild(textArea);
        textArea.select();
        try {
          var successful = document.execCommand('copy');
          if (!successful) {
            alert('Fallback: Oops, unable to copy');
          }
        } catch (err) {
          alert('Fallback: Oops, unable to copy');
        }
        document.body.removeChild(textArea);
      }
    })();
  ")
        )
        #end button code

      ),
      div(
        style = paste0(
          "max-height: 180px; overflow-y: auto; padding: 4px; ",
          "border: 1px solid #ddd; background: #f9f9f9;"
        ),
        verbatimTextOutput("code_all")
      ),


      hr(),

      # # 10) Removed data points (as a code snippet)
      # h4("Removed data points:"),
      # div(
      #   style="max-height:180px; overflow-y:auto; padding:4px; border:1px solid #ddd; background:#f9f9f9;",
      #   verbatimTextOutput("removed_code")
      #   # style = "max-height:150px; overflow-y:auto; padding:4px; border:1px solid #ddd; background:#f9f9f9;",
      #   # verbatimTextOutput("removed_code")
      # ),
      #
      # hr(),

      # 11) Download & Reset‐Data buttons
      fluidRow(
        column(
          width = 6,
          downloadButton("download_data", "Download cleaned CSV", width = "100%")
        ),
        column(
          width = 6,
          actionButton("reset_data", "Reset Data", width = "100%")
        )
      )
    ),

    mainPanel(
      width = 8,
      plotlyOutput("qc_plot", height = "80vh")
    )
  )
)


server <- function(input, output, session) {
  output$subtitle <- renderUI({
    req(input$yvar)
    tags$h5(
      paste("Filtering out:", input$yvar),
      style = "color:#555; margin-top:-10px; margin-bottom:20px;"
    )
  })

  # 1) “Injected” offset → local_tz
  offset   <- getOption("shiny.initialOffset", default = -5)
  local_tz <- make_local_tz(offset)

  # 2a) Read raw CSV
  raw_df <- reactive({
    req(input$csv_file)
    read.csv(
      input$csv_file$datapath,
      stringsAsFactors = FALSE,
      colClasses = c(TIMESTAMP_START = "character"),
      na.strings = "-9999"
    )
  })

  # 2b) Parse TIMESTAMP_START → POSIXct in local_tz + keep .row index
  shifted_df <- reactive({
    df0 <- raw_df()
    req(df0)
    df0 %>%
      mutate(
        raw_ts = TIMESTAMP_START,
        ts_str = substr(gsub("[^0-9]", "", raw_ts), 1, 12),
        TIMESTAMP_START = as.POSIXct(ts_str, "%Y%m%d%H%M", tz = local_tz),
        .row            = row_number()
      )
  })



  # 2c) Keep a reactiveValues copy of the current data, plus an immutable original
  rv      <- reactiveValues(df = NULL)
  orig_df <- reactiveVal(NULL)

  # ────────────────────────────────────────────────────────────────────────────
  # When a new file arrives, place it into rv$df, record original copy,
  # and populate “year_sel” with “All” + each unique year
  # ────────────────────────────────────────────────────────────────────────────
  observeEvent(shifted_df(), {
    rv$df <- shifted_df()
    orig_df(shifted_df())

    all_years <- sort(unique(format(rv$df$TIMESTAMP_START, "%Y")))
    # add “All” in front of every other year
    choices_with_all <- c("All", all_years)

    updateSelectizeInput(
      session,
      "year_sel",
      choices  = c("All", all_years),
      selected = "All"
    )
  })

  # 3) Reactive: df_by_year() filters rv$df by whichever years the user picked.
  df_by_year <- reactive({
    req(rv$df, input$year_sel)
    # If the user has "All" selected *and* no other year, return the full data:
    if (identical(input$year_sel, "All")) {
      return(rv$df)
    }

    # Otherwise, drop "All" (if present) and filter by the remaining years:
    chosen_years <- setdiff(input$year_sel, "All")

    rv$df %>%
      filter(format(TIMESTAMP_START, "%Y") %in% chosen_years)
  })


  # Track manual selections / removals
  sel_keys       <- reactiveVal(integer(0))
  removed_ts     <- reactiveValues()   # for “accumulated‐selection code”
  outlier_keys   <- reactiveVal(integer(0))

  # Track exactly which timestamps have been Confirm Removed
  confirmed_ts <- reactiveValues()

  # Helper: lasso‐selected rows
  selected_keys <- reactive({
    sel <- event_data("plotly_selected", source = "qc_plot")
    if (is.null(sel)) return(integer(0))
    sel$key
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Whenever the Y‐variable changes, clear the “current selection” and outliers
  # ────────────────────────────────────────────────────────────────────────────
  observeEvent(input$yvar, {
    req(input$yvar)                # ← ensure yvar is non‐NULL/non‐empty
    sel_keys(integer(0))
    #outlier_keys(integer(0))
    session$resetBrush("qc_plot")

    # Re‐populate sel_keys() from removed_ts for this new yvar
    # (removed_ts[[ yvar ]] might be a character vector of ts_str values)
    current_ts <- removed_ts[[ input$yvar ]] %||% character()
    if (length(current_ts)) {
      matching_rows <- which(df_by_year()$ts_str %in% current_ts)
      sel_keys(matching_rows)
    } else {
      sel_keys(integer(0))
    }
  })


  # ────────────────────────────────────────────────────────────────────────────
  # Rebuild xvar/yvar dropdowns whenever new data arrives
  # ────────────────────────────────────────────────────────────────────────────
  observe({
    df <- df_by_year()
    req(df)

    num_cols <- df %>%
      select(-TIMESTAMP_START, -raw_ts, -ts_str, -.row) %>%
      select(where(is.numeric)) %>%
      names()

    x_choices <- c("TIMESTAMP_START", num_cols)
    y_choices <- num_cols

    updateSelectInput(
      session, "xvar",
      choices = x_choices,
      selected =
        if (!is.null(input$xvar) && input$xvar %in% x_choices) {
          input$xvar
        } else {
          "TIMESTAMP_START"
        }
    )

    updateSelectInput(
      session, "yvar",
      choices = y_choices,
      selected =
        if (!is.null(input$yvar) && input$yvar %in% y_choices) {
          input$yvar
        } else {
          y_choices[1]
        }
    )
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Compute residuals & flag ±σ outliers
  # ────────────────────────────────────────────────────────────────────────────
  df_clean <- reactive({
    df0 <- df_by_year()
    req(df0, input$xvar, input$yvar, input$sd_thresh)

    df1 <- df0 %>%
      filter(
        !is.na(.data[[input$xvar]]),
        !is.na(.data[[input$yvar]])
      )

    fit0 <- lm(reformulate(input$xvar, input$yvar), data = df1)

    df1 %>%
      mutate(
        fitted = predict(fit0, newdata = .),
        resid  = .data[[input$yvar]] - fitted,
        sigma  = sd(resid, na.rm = TRUE),
        flag   = if_else(
          abs(resid) > input$sd_thresh * sigma,
          "outlier", "inlier"
        )
      )
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Button logic: add/remove outliers & manual selection accumulation
  # ────────────────────────────────────────────────────────────────────────────
  observeEvent(input$add_outliers, {
    ok <- df_clean() %>% filter(flag == "outlier") %>% pull(.row)
    outlier_keys(unique(c(isolate(outlier_keys()), ok)))
    sel_keys(unique(c(isolate(sel_keys()), ok)))

    ts  <- df_by_year() %>% filter(.row %in% ok) %>% pull(ts_str)
    old <- removed_ts[[input$yvar]] %||% character()
    removed_ts[[input$yvar]] <- unique(c(old, ts))
  })

  observeEvent(input$clear_outliers, {
    old_out <- isolate(outlier_keys())
    if (length(old_out) == 0) return()

    sel_keys(setdiff(isolate(sel_keys()), old_out))
    outlier_keys(integer(0))

    ts_out   <- df_by_year() %>% filter(.row %in% old_out) %>% pull(ts_str)
    existing <- removed_ts[[input$yvar]] %||% character()
    removed_ts[[input$yvar]] <- setdiff(existing, ts_out)
  })

  observeEvent(input$add_sel, {
    keys <- selected_keys()
    if (!length(keys)) return()

    sel_keys(unique(c(isolate(sel_keys()), keys)))
    ts    <- df_by_year() %>% filter(.row %in% keys) %>% pull(ts_str)
    old   <- removed_ts[[input$yvar]] %||% character()
    removed_ts[[input$yvar]] <- unique(c(old, ts))
  })

  observeEvent(input$remove_acc, {
    keys <- selected_keys()
    if (!length(keys)) return()

    new_sel <- setdiff(isolate(sel_keys()), keys)
    sel_keys(new_sel)

    ts_vals <- df_by_year() %>% filter(.row %in% keys) %>% pull(ts_str)
    old     <- removed_ts[[input$yvar]] %||% character()
    removed_ts[[input$yvar]] <- setdiff(old, ts_vals)
  })

  observeEvent(input$clear_sel, {
    sel_keys(integer(0))
  })

  observeEvent(input$reset_all, {
    sel_keys(integer(0))
    outlier_keys(integer(0))
    # Clear the Plotly brush so that selected_keys() immediately becomes integer(0)
    session$resetBrush("qc_plot")

    for (nm in names(reactiveValuesToList(removed_ts))) {
      removed_ts[[nm]] <- NULL
    }
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Render the Plotly scatter (with event_register)
  # ────────────────────────────────────────────────────────────────────────────
  output$qc_plot <- renderPlotly({
    df0 <- df_by_year()
    req(df0, input$xvar, input$yvar)


    dfc <- df0 %>%
      filter(
        !is.na(.data[[input$xvar]]),
        !is.na(.data[[input$yvar]])
      ) %>%
      {
        fit0 <- lm(reformulate(input$xvar, input$yvar), data = .)
        mutate(
          .,
          fitted = predict(fit0, newdata = .),
          resid  = .data[[input$yvar]] - fitted,
          sigma  = sd(resid, na.rm = TRUE),
          flag   = if_else(
            abs(resid) > input$sd_thresh * sigma,
            "outlier", "inlier"
          )
        )
      }

    p <- plot_ly(
      data   = dfc,
      x      = ~.data[[input$xvar]],
      y      = ~.data[[input$yvar]],
      key    = ~.row,
      source = "qc_plot",
      mode   = "markers",
      type   = "scatter",
      marker = list(color = "#228833", opacity = 0.6)
    )

    # Plot the ±σ outliers as red
    if (input$sd_thresh > 0) {
      p <- p %>%
        add_trace(
          data       = filter(dfc, flag == "outlier"),
          x          = ~.data[[input$xvar]],
          y          = ~.data[[input$yvar]],
          mode       = "markers",
          type       = "scatter",
          marker     = list(color = "#ee6677", opacity = 0.8),
          showlegend = FALSE
        )
    }

    # Plot the “accumulated” (orange) points on top
    if (length(sel_keys()) > 0) {
      p <- p %>%
        add_trace(
          data       = dfc %>% filter(.row %in% sel_keys()),
          x          = ~.data[[input$xvar]],
          y          = ~.data[[input$yvar]],
          mode       = "markers",
          type       = "scatter",
          marker     = list(color = "orange", size = 10),
          inherit    = FALSE,
          showlegend = FALSE
        )
    }

    # R2 value for ALL points,
    # then fit a second time on (all points minus accumulated selections).
    if (input$show_reg && input$xvar != "TIMESTAMP_START") {
      # 1) R² on ALL points (even the ±σ outliers)
      df_all <- df0 %>%
        filter(
          !is.na(.data[[input$xvar]]),
          !is.na(.data[[input$yvar]])
        )

      if (nrow(df_all) >= 2) {
        fit_all <- lm(reformulate(input$xvar, input$yvar), data = df_all)
        r2_all  <- round(summary(fit_all)$r.squared, 2)

        # Add a gray regression line for all points:
        xseq_all <- seq(
          min(df_all[[input$xvar]], na.rm = TRUE),
          max(df_all[[input$xvar]], na.rm = TRUE),
          length.out = 100
        )
        preds_all <- predict(fit_all, newdata = setNames(data.frame(xseq_all), input$xvar))


        p <- p %>%
          add_lines(
            x          = xseq_all,
            y          = preds_all,
            inherit    = FALSE,
            line       = list(color = "gray50"),
            showlegend = FALSE
          ) %>%
          add_annotations(
            xref        = "paper",
            yref        = "paper",
            x           = 0.02,
            y           = 1.00,
            xanchor     = "left",
            yanchor     = "bottom",
            text        = paste0("<b>R² (all points) = ", r2_all, "</b>"),
            showarrow   = FALSE,
            font        = list(size = 12),
            bgcolor     = "#F3919C",
            bordercolor = "black"
          )
      }

      # 2) R² with accumulated points dropped
      acc_sel <- isolate(sel_keys())
      if (length(acc_sel) > 0) {
        # Build a dataset that excludes the .row indices in acc_sel
        df_drop_sel <- df0 %>%
          filter(
            !is.na(.data[[input$xvar]]),
            !is.na(.data[[input$yvar]])
          ) %>%
          filter(!(.row %in% acc_sel))

        if (nrow(df_drop_sel) >= 2) {
          fit_sel <- lm(reformulate(input$xvar, input$yvar), data = df_drop_sel)
          r2_sel  <- round(summary(fit_sel)$r.squared, 2)
        } else {
          r2_sel <- NA_real_
        }

        p <- p %>%
          add_annotations(
            xref        = "paper",
            yref        = "paper",
            x           = 0.02,
            y           = 0.96,
            xanchor     = "left",
            yanchor     = "bottom",
            text        = paste0("<b>R² (sel dropped) = ", r2_sel, "</b>"),
            showarrow   = FALSE,
            font        = list(size = 12),
            bgcolor     = "#FFC65C",
            bordercolor = "black"
          )
      }
    }

    p %>%
      layout(
        dragmode = "select",
        xaxis    = if (input$xvar == "TIMESTAMP_START") {
          list(type = "date", tickformat = "%b %d\n%H:%M", title = input$xvar)
        } else {
          list(title = input$xvar)
        },
        yaxis = list(title = input$yvar)
      ) %>%
      event_register("plotly_selected")
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Preview table (same as before)
  # ────────────────────────────────────────────────────────────────────────────
  output$preview <- renderTable({
    keys <- selected_keys()
    if (length(keys) == 0) keys <- sel_keys()
    if (length(keys) == 0) return(NULL)

    local_label <- sprintf("Timestamp (UTC%+d)", offset)

    df_by_year() %>%
      filter(.row %in% keys) %>%
      mutate(
        !!local_label := format(
          TIMESTAMP_START,
          "%Y-%m-%d %H:%M",
          tz = local_tz
        )
      ) %>%
      select(all_of(local_label), !!sym(input$yvar), raw_ts) %>%
      setNames(c(local_label, input$yvar, "raw_ts"))
  }, sanitize.text.function = identity)

  # ────────────────────────────────────────────────────────────────────────────
  # Current‐selection code
  # ────────────────────────────────────────────────────────────────────────────
  output$code_current <- renderText({
    keys <- selected_keys()
    if (length(keys) == 0) keys <- sel_keys()
    if (length(keys) == 0) {
      return("

<!-- draw a box or lasso (or click “Add current selection”) to see its code here -->

")
    }
    sel_ts <- df_by_year() %>% filter(.row %in% keys) %>% pull(ts_str)
    conds  <- paste0("TIMESTAMP_START == '", sel_ts, "' ~ NA_real_", collapse = ",\n      ")
    paste0(
      "df <- df %>%\n",
      "  mutate(\n",
      "    ", input$yvar, " = case_when(\n",
      "      ", conds, ",\n",
      "      TRUE ~ ", input$yvar, "\n",
      "    )\n",
      "  )"
    )
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Accumulated‐selection code
  # ────────────────────────────────────────────────────────────────────────────
  output$code_all <- renderText({
    all_removals <- reactiveValuesToList(removed_ts)
    all_removals <- all_removals[vapply(all_removals, length, FUN.VALUE = integer(1)) > 0]
    if (length(all_removals) == 0) {
      return("

<!-- click “Add current selection” or “Add all ±σ outliers” → see code here -->

")
    }
    snippets <- lapply(names(all_removals), function(var) {
      ts    <- all_removals[[var]]
      conds <- paste0("TIMESTAMP_START == '", ts, "' ~ NA_real_", collapse = ",\n      ")
      paste0(
        "df <- df %>%\n",
        "  mutate(\n",
        "    ", var, " = case_when(\n",
        "      ", conds, ",\n",
        "      TRUE ~ ", var, "\n",
        "    )\n",
        "  )"
      )
    })
    paste(unlist(snippets), collapse = "\n\n")
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Removed‐points code snippet (only those Confirm Removed)
  # ────────────────────────────────────────────────────────────────────────────
  output$removed_code <- renderText({
    # pulled_ts is the character‐vector of ts_str values that we have already “confirmed” as removed, for the current y‐variable
    pulled_ts <- removed_ts[[input$yvar]] %||% character()
    if (length(pulled_ts) == 0) {
      return("<!-- no points have been “Confirmed Remove” yet -->")
    }

    # build a case_when(...) string using TIMESTAMP_START
    conds <- paste0(
      "TIMESTAMP_START == '", pulled_ts, "' ~ NA_real_",
      collapse = ",\n      "
    )
    paste0(
      "df <- df %>%\n",
      "  mutate(\n",
      "    ", input$yvar, " = case_when(\n",
      "      ", conds, ",\n",
      "      TRUE ~ ", input$yvar, "\n",
      "    )\n",
      "  )"
    )
  })


  # ────────────────────────────────────────────────────────────────────────────
  # DOWNLOAD HANDLER for “Download cleaned CSV”
  # ────────────────────────────────────────────────────────────────────────────
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("flux_cleaned_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # 1) Read exactly the user’s original upload, TIMESTAMP_START still text:
      base_df <- raw_df()   # reactive() that does read.csv(datapath, colClasses=TIMESTAMP_START="character", …)

      # 2) Grab the “helper” copy (POSIXct timestamps + any y’s set to NA):
      helper <- df_by_year()

      # 3) Overwrite only the _non‐TIMESTAMP_START_ columns in base_df.
      #    This way, TIMESTAMP_START stays exactly as it was in the raw file.
      for (colname in setdiff(names(base_df), "TIMESTAMP_START")) {
        if (colname %in% names(helper)) {
          base_df[[colname]] <- helper[[colname]]
        }
      }

      # 4) Finally, write out just the columns that came from the user’s original CSV.
      write.csv(base_df, file, row.names = FALSE, na = "NA")
    }
  )



  # ────────────────────────────────────────────────────────────────────────────
  # Confirm Remove → set selected rows’ y‐value to NA, record them in confirmed_ts
  # ────────────────────────────────────────────────────────────────────────────
  observeEvent(input$remove, {
    old_sel   <- as.integer(isolate(selected_keys()))
    acc_sel   <- as.integer(isolate(sel_keys()))
    out_sel   <- as.integer(isolate(outlier_keys()))
    to_remove <- union(old_sel, union(acc_sel, out_sel))

    valid_rows <- seq_len(nrow(df_by_year()))
    to_remove  <- intersect(to_remove, valid_rows)
    if (!length(to_remove)) return()

    # (1) Grab their ts_str before setting to NA
    just_ts <- df_by_year() %>%
      slice(to_remove) %>%
      pull(ts_str)

    # (2) Set the y‐variable to NA for those rows
    df_by_year()[to_remove, input$yvar] <- NA_real_

    # (3) Record them under confirmed_ts[[ yvar ]]
    old_confirmed <- confirmed_ts[[input$yvar]] %||% character()
    confirmed_ts[[input$yvar]] <- unique(c(old_confirmed, just_ts))

    # (4) Also remove them from the “accumulated‐selection” list
    old_removed <- removed_ts[[input$yvar]] %||% character()
    removed_ts[[input$yvar]] <- setdiff(old_removed, just_ts)

    # (5) Clear brushes, sel_keys(), and outlier_keys()
    session$resetBrush("qc_plot")
    sel_keys(integer(0))
    outlier_keys(integer(0))
  })

  # ────────────────────────────────────────────────────────────────────────────
  # Reset Data → restore df_by_year() to orig_df() and clear all removal records
  # ────────────────────────────────────────────────────────────────────────────
  observeEvent(input$reset_data, {
    df0 <- orig_df()
    req(df0)
    df_by_year() <- df0

    for (nm in names(reactiveValuesToList(removed_ts))) {
      removed_ts[[nm]] <- NULL
    }
    for (nm in names(reactiveValuesToList(confirmed_ts))) {
      confirmed_ts[[nm]] <- NULL
    }
    sel_keys(integer(0))
    outlier_keys(integer(0))
    session$resetBrush("qc_plot")
  })
}

shinyApp(ui, server)

