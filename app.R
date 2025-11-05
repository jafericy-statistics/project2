#app.R build basic structure and add functionality later
#mostly placeholder to combine with graphing later.

# ====== USER SETTING ======
data_path <- "C:\\Users\\jacob\\git\\NCSU\\project2\\data\\combined.csv"   # <-- update to your CSV; switch to readRDS for .rds

# ====== PACKAGES ======
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

theme_set(theme_minimal(base_size = 13))



suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

#basic test for cols
cat_cols <- c("Group", "Segment", "Type")
num_cols <- c("Metric 1", "Metric 2", "Metric 3")

#sliders contruct, in progress
cat1_default <- cat_cols[1]
cat2_default <- cat_cols[2]
num1_default <- num_cols[1]
num2_default <- num_cols[2]

#simple css for style. looked good on site.
placeholder_css <- "
.placeholder {
  background: #f6f7f9;
  border: 1px dashed #c8cdd4;
  border-radius: 6px;
  padding: 16px;
  color: #7a869a;
  text-align: center;
  height: 260px;
  display: flex;
  align-items: center;
  justify-content: center;
}
.placeholder.sm { height: 160px; }
"

#basic wireframe for the project
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML(placeholder_css))),
  titlePanel("Banking Data App"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Subset Data"),
      helpText("Placeholder for controls."),
      
      #additional filter placeholders
      selectizeInput(
        "cat1_values",
        label = paste0("Filter: ", cat1_default),
        choices = c("All", "A", "B", "C"),
        selected = "All",
        multiple = TRUE,
        options = list(placeholder = "Select levels (could be all? - need to test)")
      ),
      selectizeInput(
        "cat2_values",
        label = paste0("Filter: ", cat2_default),
        choices = c("All", "X", "Y", "Z"),
        selected = "All",
        multiple = TRUE,
        options = list(placeholder = "Select levels (could be all? - need to test)")
      ),
      tags$hr(),
      
      # Numeric variable selectors + fixed sliders
      selectInput("num1_var", "Num Var 1", choices = c("", num_cols), selected = num1_default),
      sliderInput("num1_range", "Range: Num Var 1", min = 0, max = 100, value = c(20, 80)),
      
      selectInput("num2_var", "Num Var 2", choices = c("", num_cols), selected = num2_default),
      sliderInput("num2_range", "Range: Num Var 2", min = 0, max = 100, value = c(10, 90)),
      
      tags$hr(),
      actionButton("apply_filters", "apply filters (need to test)", class = "btn btn-primary w-100"),
      br(), br(),
      helpText("this needs to be tested still, not data functions.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          title = "About",
          value = "about_tab",
          h3("About This App (Need to test)"),
          p("This is a UI-only wireframe. Need to add more"),
          tags$ul(
            tags$li("This application serves as a blueprint for builing interactive Shiny dashboards."),
            tags$li("It provides a complete user interface that allows the users to look through and explore the data in detail."),
            tags$li("It also provides basic summaries and plots.")
          ),
          tags$hr(),
          div(class = "placeholder sm", "image output or data-source link placeholder")
        ),
        
        tabPanel(
          title = "Data Download",
          value = "download_tab",
          h3("View & Download Data (need to test)"),
          div(class = "placeholder", "Table"),
          br(),
          actionButton("download_csv", "Download CSV (testing)", class = "btn btn-outline-secondary")
        ),
        
        tabPanel(
          title = "Data Exploration",
          value = "explore_tab",
          h3("Exploratory Summaries & Plots (need to test)"),
          
          fluidRow(
            column(
              width = 6,
              radioButtons(
                "summary_type",
                "Summary Type",
                choices = c("Cat tables" = "cat", "Num summaries by group" = "num"),
                inline = TRUE
              )
            ),
            column(
              width = 6,
              selectInput("cat_group_var", "group (cat for summaries / color)", choices = c("", cat_cols), selected = cat1_default),
              selectInput("cat_facet_var",  "facet by cat", choices = c("", cat_cols), selected = cat2_default)
            )
          ),
          
          fluidRow(
            column(6, selectInput("num_y_var", "Y (num)", choices = c("", num_cols), selected = num2_default)),
            column(6, selectInput("num_x_var", "X (num)", choices = c("", num_cols), selected = num1_default))
          ),
          
          tags$hr(),
          
          conditionalPanel(
            condition = "input.summary_type == 'cat'",
            h4("cont Tables"),
            div(class = "placeholder sm", "one-way tables"),
            br(),
            div(class = "placeholder sm", "two-way table")
          ),
          
          conditionalPanel(
            condition = "input.summary_type == 'num'",
            h4("num summ - group need to test"),
            div(class = "placeholder sm", "num summaries")
          ),
          
          tags$hr(),
          
          h4("Plots"),
          fluidRow(
            column(6, div(class = "placeholder", "hist by group")),
            column(6, div(class = "placeholder", "boxplot - facet"))
          ),
          fluidRow(
            column(6, div(class = "placeholder", "scatter + smooth")),
            column(6, div(class = "placeholder", "prop bar"))
          ),
          fluidRow(
            column(6, div(class = "placeholder", "corr heatmap")),
            column(6, div(class = "placeholder", "density - facet"))
          )
        )
      )
    )
  )
)

# -----------------------------
# SERVER — intentionally empty (no data, no outputs)
# -----------------------------
server <- function(input, output, session) {
  # this is the placeholder. I have code elsewhere.
}

# -----------------------------
# RUN
# -----------------------------
shinyApp(ui, server)

