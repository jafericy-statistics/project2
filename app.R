# app.R —  Banking Data App

#define pathway for data
data_path <- "C:\\Users\\jacob\\git\\NCSU\\project2\\data\\combined.csv"   # <-- update to your CSV; switch to readRDS for .rds

#suppress package messages to denote if there are any issues regarding the filters, etc.
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinycssloaders)
  library(DT)
  library(tidyverse)
  library(janitor)
  library(skimr)
  library(scales)
})

#define theme
theme_set(theme_minimal(base_size = 13))

#category helper functions, helped against downstream errors before for cat variables
is_categorical <- function(x) {
  is.factor(x) || is.character(x) || dplyr::n_distinct(x, na.rm = TRUE) <= 20
}

#function that gets unique cat values (levels) from var
safe_levels <- function(x) {
  if (is.factor(x)) levels(x) else sort(unique(x))
}

#function reads in data automatically called regardless of extension (csv, txt, rds) from defined path
load_data <- function(path) {
  switch(tolower(tools::file_ext(path)),
         csv = , txt = readr::read_csv(path, show_col_types = FALSE),
         rds = readRDS(path),
         stop("This file extention is not supported!: ", tools::file_ext(path))
  )
}

#clean dataset and identify num and cat columns
df_raw <- load_data(data_path)
df <- df_raw %>% clean_names()

num_cols <- names(select(df, where(is.numeric)))
cat_cols <- names(select(df, where(is_categorical)))

#for cat vars, create tibble for unique levels/combinations
cat_meta <- tibble(
  var = cat_cols,
  nlev = map_int(cat_cols, ~ n_distinct(df[[.x]], na.rm = TRUE))
) %>% arrange(abs(nlev - 6))

#let's set a default selection for the variables both cat and num
cat1_default <- if (nrow(cat_meta) >= 1) cat_meta$var[1] else NA_character_
cat2_default <- if (nrow(cat_meta) >= 2) cat_meta$var[2] else NA_character_
num1_default <- if (length(num_cols) >= 1) num_cols[1] else NA_character_
num2_default <- if (length(num_cols) >= 2) num_cols[2] else NA_character_

#let's create the ui structure which we started with a previous commit
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  #title
  titlePanel("Banking Data App"),
  
  #this defines sidebars
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Subset Data"),
      helpText("Choose categories and numeric ranges, then click 'Apply Filters'."),
      
      uiOutput("cat1_ui"),
      uiOutput("cat2_ui"),
      tags$hr(),
      
      #select inputs for the sidebar
      selectInput("num1_var", "Numeric Variable 1", choices = c("", num_cols), selected = num1_default),
      uiOutput("num1_slider_ui"),
      
      selectInput("num2_var", "Numeric Variable 2", choices = c("", num_cols), selected = num2_default),
      uiOutput("num2_slider_ui"),
      
      tags$hr(),
      
      #action button to define a filter and produce an action.
      actionButton("apply_filters", "Apply Filters", class = "btn btn-primary w-100"),
      br(), br(),
      helpText("Data updates only when you press 'Apply Filters'.")
    ),
    
    #defines the main structure where we add other tabs
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        #add tabs for about, graphics and data download
        tabPanel(
          title = "About",
          value = "about_tab",
          h3("About This App"),
          p("This app provides basic data analysis, summary statistics and outputted graphs."),
          p("Please use the sidebar to subset by categories and numeric ranges. Click 'Apply Filters' to update."),
          tags$ul(
            tags$li("About Tab: Basic overview and instructions."),
            tags$li("Data Download Tab: View the data and download as CSV."),
            tags$li("Data Exploration Tab: Compute tables, summaries, and plots.")
          ),
          tags$hr(),
          #em("Optionally add a data-source link or image here if your rubric requires it.")
        ),
        
        #same as above
        tabPanel(
          title = "Data Download",
          value = "download_tab",
          h3("View & Download Data"),
          withSpinner(DTOutput("data_table"), type = 4),
          br(),
          downloadButton("download_csv", "Download CSV")
        ),
        
        tabPanel(
          title = "Data Exploration",
          value = "explore_tab",
          h3("Summaries & Plots"),
          
          fluidRow(
            column(
              width = 6,
              radioButtons(
                "summary_type",
                "Summary Type",
                choices = c("Categorical tables" = "cat", "Numeric summaries by group" = "num"),
                inline = TRUE
              )
            ),
            column(
              width = 6,
              selectInput("cat_group_var", "Categorical Grouping", choices = c("", cat_cols), selected = cat1_default),
              selectInput("cat_facet_var",  "Categorical Summary", choices = c("", cat_cols), selected = cat2_default)
            )
          ),
          
          fluidRow(
            column(6, selectInput("num_y_var", "Y (numeric)", choices = c("", num_cols), selected = num2_default)),
            column(6, selectInput("num_x_var", "X (numeric)", choices = c("", num_cols), selected = num1_default))
          ),
          
          tags$hr(),
          
          #after selecting cat and sum type, this displays a section for one-two way contingency tables
          conditionalPanel(
            condition = "input.summary_type == 'cat'",
            h4("One/Two-way Contingency Tables"),
            withSpinner(DTOutput("one_way_tables"), type = 4),
            br(),
            withSpinner(DTOutput("two_way_table"), type = 4)
          ),
          
          #displays table of numeric summaries which are by group. Load with a spinner, dependent on input.
          conditionalPanel(
            condition = "input.summary_type == 'num'",
            h4("Numeric Summaries by Group"),
            withSpinner(DTOutput("numeric_by_group"), type = 4)
          ),
          
          tags$hr(),
          #header for plits and adding plot outputs
          h4("Plots"),
          fluidRow(
            column(6, withSpinner(plotOutput("plot_hist_by_group"), type = 4)),
            column(6, withSpinner(plotOutput("plot_box_facet"), type = 4))
          ),
          fluidRow(
            column(6, withSpinner(plotOutput("plot_scatter_smooth"), type = 4)),
            column(6, withSpinner(plotOutput("plot_prop_bar"), type = 4))
          ),
          fluidRow(
            column(6, withSpinner(plotOutput("plot_heatmap_corr"), type = 4)),
            column(6, withSpinner(plotOutput("plot_density_facet"), type = 4))
          )
        )
      )
    )
  )
)

#define server structure
server <- function(input, output, session) {
  
  #we need to dynamically create the dropdown for filtering by cat variable 
  # we want to list all levels with "All" option on levs
  output$cat1_ui <- renderUI({
    if (!is.na(cat1_default)) {
      levs <- safe_levels(df[[cat1_default]])
      selectizeInput(
        "cat1_values", label = paste0("Filter: ", cat1_default),
        choices = c("All" = ".all.", levs),
        selected = ".all.",
        multiple = TRUE,
        options = list(placeholder = "Select levels")
      )
    } else {
      helpText("No categorical variable for Filter 1.")
    }
  })
  
  output$cat2_ui <- renderUI({
    if (!is.na(cat2_default)) {
      levs <- safe_levels(df[[cat2_default]])
      selectizeInput(
        "cat2_values", label = paste0("Filter: ", cat2_default),
        choices = c("All" = ".all.", levs),
        selected = ".all.",
        multiple = TRUE,
        options = list(placeholder = "Select levels")
      )
    } else {
      helpText("No suitable categorical variable for Filter 2.")
    }
  })
  
  # Dynamic sliders for numeric selection
  output$num1_slider_ui <- renderUI({
    #make sure num1_var exists and is not empty
    req(input$num1_var, input$num1_var != "")
    
    #calc min and max values of num variable
    rng <- range(df[[input$num1_var]], na.rm = TRUE)
    sliderInput("num1_range", paste0("Range: ", input$num1_var),
                min = rng[1], max = rng[2], value = rng, step = (rng[2]-rng[1])/100)
  })
  
  #same as above with variables for num2
  output$num2_slider_ui <- renderUI({
    req(input$num2_var, input$num2_var != "")
    rng <- range(df[[input$num2_var]], na.rm = TRUE)
    sliderInput("num2_range", paste0("Range: ", input$num2_var),
                min = rng[1], max = rng[2], value = rng, step = (rng[2]-rng[1])/100)
  })
  
  #store val dynamic updating
  rv <- reactiveValues(data = df)
  
  observeEvent(input$apply_filters, {
    d <- df
    
    #filter process for cat1, not super clean sorry!
    if (!is.na(cat1_default) && !is.null(input$cat1_values)) {
      if (!(".all." %in% input$cat1_values)) {
        d <- d %>% filter(.data[[cat1_default]] %in% input$cat1_values)
      }
    }
    #filter process for cat2, not super clean sorry!
    if (!is.na(cat2_default) && !is.null(input$cat2_values)) {
      if (!(".all." %in% input$cat2_values)) {
        d <- d %>% filter(.data[[cat2_default]] %in% input$cat2_values)
      }
    }
    #filtering the data which includes rows and ranges on select num ranges dep on slider
    if (!is.null(input$num1_var) && nzchar(input$num1_var) && !is.null(input$num1_range)) {
      d <- d %>% filter(between(.data[[input$num1_var]], input$num1_range[1], input$num1_range[2]))
    }
    
    #same as above
    if (!is.null(input$num2_var) && nzchar(input$num2_var) && !is.null(input$num2_range)) {
      d <- d %>% filter(between(.data[[input$num2_var]], input$num2_range[1], input$num2_range[2]))
    }
    
    rv$data <- d
  }, ignoreInit = TRUE)
  
  current_data <- reactive({
    if (is.null(rv$data)) df else rv$data
  })
  
  #need to output the data after filter
  output$data_table <- renderDT({
    dat <- current_data()
    validate(need(nrow(dat) > 0, "After filtering, there are no rows. Please adjust filters and click 'Apply Filters'."))
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE), filter = "top")
  })
  
  #download to csv, handler for data filter.
  output$download_csv <- downloadHandler(
    filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(current_data(), file)
    }
  )
  
  #data exploration with freq and percent on cat vars 
  output$one_way_tables <- renderDT({
    dat <- current_data()
    cats <- names(select(dat, where(is_categorical)))
    validate(need(length(cats) > 0, "There are no categorical variables to tabulate."))
    ow <- purrr::map_dfr(head(cats, 3), function(v) {
      dat %>%
        count(.data[[v]], name = "n") %>%
        mutate(pct = n / sum(n), variable = v) %>%
        relocate(variable)
    })
    datatable(ow, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #same as above
  output$two_way_table <- renderDT({
    dat <- current_data()
    cats <- names(select(dat, where(is_categorical)))
    validate(need(length(cats) >= 2, "Need at least two categorical variables for a two-way table."))
    c1 <- cats[1]; c2 <- cats[2]
    tw <- dat %>%
      tabyl(.data[[c1]], .data[[c2]]) %>%
      adorn_totals(where = c("row", "col")) %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting(digits = 1) %>%
      adorn_ns()
    datatable(tw, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$numeric_by_group <- renderDT({
    dat <- current_data()
    grp <- input$cat_group_var
    
    #choose a grouping cat var and select if group var not in data
    validate(
      need(!is.null(grp) && nzchar(grp), "Choose a grouping categorical variable (top right)."),
      need(grp %in% names(dat), "Selected grouping variable not found in data.")
    )
    numvars <- names(select(dat, where(is.numeric)))
    validate(need(length(numvars) > 0, "No numeric variables available."))
    sum_tab <- dat %>%
      group_by(.data[[grp]]) %>%
      
      #calc summ stats, need to clean up var names
      summarise(
        across(
          all_of(numvars),
          list(
            n = ~sum(!is.na(.x)),
            mean = ~mean(.x, na.rm = TRUE),
            sd = ~sd(.x, na.rm = TRUE),
            median = ~median(.x, na.rm = TRUE),
            p25 = ~quantile(.x, 0.25, na.rm = TRUE),
            p75 = ~quantile(.x, 0.75, na.rm = TRUE)
          ),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
    datatable(sum_tab, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #outputting plot vars
  output$plot_hist_by_group <- renderPlot({
    dat <- current_data()
    xvar <- input$num_x_var
    gvar <- input$cat_group_var
    validate(need(nzchar(xvar), "Pick X (numeric) for the histogram."))
    validate(need(xvar %in% names(dat), "X var not found."))
    ggplot(dat, aes(x = .data[[xvar]], fill = if (nzchar(gvar)) .data[[gvar]] else NULL)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      labs(
        title = paste("Histogram of", xvar, if (nzchar(gvar)) paste("by", gvar) else ""),
        x = xvar, y = "Count", fill = if (nzchar(gvar)) gvar else NULL
      )
  }, res = 120)
  
  #output boxplot
  output$plot_box_facet <- renderPlot({
    dat <- current_data()
    yvar <- input$num_y_var
    gvar <- input$cat_group_var
    fvar <- input$cat_facet_var
    validate(need(nzchar(yvar), "Pick Y (numeric) for the boxplot."))
    validate(need(nzchar(gvar), "Pick a grouping categorical for the x-axis."))
    validate(need(all(c(yvar, gvar) %in% names(dat)), "Selected variables not found."))
    p <- ggplot(dat, aes(x = .data[[gvar]], y = .data[[yvar]], fill = .data[[gvar]])) +
      geom_boxplot(outlier.alpha = 0.5) +
      labs(title = paste("Boxplot of", yvar, "across", gvar), x = gvar, y = yvar) +
      theme(legend.position = "none")
    if (nzchar(fvar) && fvar != gvar && fvar %in% names(dat)) {
      p <- p + facet_wrap(as.formula(paste("~", fvar))) +
        labs(subtitle = paste("Faceted by", fvar))
    }
    p
  }, res = 120)
  
  #more outputs of charts & graphs
  output$plot_scatter_smooth <- renderPlot({
    dat <- current_data()
    xvar <- input$num_x_var
    yvar <- input$num_y_var
    gvar <- input$cat_group_var
    fvar <- input$cat_facet_var
    validate(need(nzchar(xvar) && nzchar(yvar), "Pick X and Y (numeric) for the scatter plot."))
    validate(need(all(c(xvar, yvar) %in% names(dat)), "Selected variables not found."))
    p <- ggplot(dat, aes(x = .data[[xvar]], y = .data[[yvar]])) +
      geom_point(aes(color = if (nzchar(gvar)) .data[[gvar]] else NULL), alpha = 0.7, na.rm = TRUE) +
      geom_smooth(method = "loess", se = TRUE, na.rm = TRUE) +
      labs(
        title = paste("Scatter of", yvar, "vs", xvar),
        x = xvar, y = yvar, color = if (nzchar(gvar)) gvar else NULL
      )
    if (nzchar(fvar) && fvar != gvar && fvar %in% names(dat)) {
      p <- p + facet_wrap(as.formula(paste("~", fvar))) +
        labs(subtitle = paste("Faceted by", fvar))
    }
    p
  }, res = 120)
  
  output$plot_prop_bar <- renderPlot({
    dat <- current_data()
    gvar <- input$cat_group_var
    fvar <- input$cat_facet_var
    validate(need(nzchar(gvar), "Pick a grouping categorical for proportional bars."))
    validate(need(nzchar(fvar) && fvar != gvar, "Pick a second categorical (Facet by) different from grouping."))
    validate(need(all(c(gvar, fvar) %in% names(dat)), "Selected variables not found."))
    ggplot(dat, aes(x = .data[[gvar]], fill = .data[[fvar]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = paste("Composition of", gvar, "by", fvar),
        x = gvar, y = "Percent", fill = fvar
      )
  }, res = 120)
  
  output$plot_heatmap_corr <- renderPlot({
    dat <- current_data()
    nums <- names(select(dat, where(is.numeric)))
    validate(need(length(nums) >= 2, "Need at least two numeric variables for a correlation heatmap."))
    cor_mat <- cor(select(dat, all_of(nums)), use = "pairwise.complete.obs")
    cor_long <- as_tibble(cor_mat, rownames = "var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "cor")
    ggplot(cor_long, aes(x = var1, y = var2, fill = cor)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +
      labs(title = "Correlation Heatmap of Numeric Variables", x = NULL, y = NULL, fill = "r") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, res = 120)
  
  output$plot_density_facet <- renderPlot({
    dat <- current_data()
    xvar <- input$num_x_var
    gvar <- input$cat_group_var
    fvar <- input$cat_facet_var
    validate(need(nzchar(xvar), "Pick X (numeric) for the density plot."))
    validate(need(xvar %in% names(dat), "Selected X variable not found."))
    p <- ggplot(dat, aes(x = .data[[xvar]])) +
      geom_density(aes(fill = if (nzchar(gvar)) .data[[gvar]] else NULL), alpha = 0.5, na.rm = TRUE) +
      labs(
        title = paste0("Density of ", xvar, if (nzchar(gvar)) paste0(" by ", gvar) else ""),
        x = xvar, y = "Density", fill = if (nzchar(gvar)) gvar else NULL
      )
    if (nzchar(fvar) && fvar != gvar && fvar %in% names(dat)) {
      p <- p + facet_wrap(as.formula(paste("~", fvar))) +
        labs(subtitle = paste("Faceted by", fvar))
    }
    p
  }, res = 120)
}

#run the application
shinyApp(ui, server)
