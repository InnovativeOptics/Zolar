#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import dplyr
#' @noRd


oem_data <- googlesheets4::read_sheet(sheet_id, sheet = "laser_info") %>%
  filter(`Laser Mfg` == "Zolar")


loupe_data <- googlesheets4::read_sheet(sheet_id, sheet = "Loupe_types", col_types = "c")


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fluid(

      useShinyjs(),
      theme = bs_add_variables(
        bs_theme(
          version = 5,
          base_font = font_google("Karla"),
          bg = "white",
          fg = "black",
          primary = "red"
        ),
        "border-radius" = "5px"
      ),
      br(),
      div(class="shadow p-3 mb-5 bg-body rounded",
          fluidRow(column(12,align='center',
                          h2(strong("Dentistry")),
                          h3("Search for loupe-compatible laser eye protection by selecting a laser device
                                               and loupe design")))),
      fluidRow(
        column(
          6,
          offset=0,
          align = 'center',
          selectInput(selectize = T,
                      width = "100%",
                      inputId = "mfg_dent",
                      label = h4(strong("Laser Manufacturer")),
                      choices = sort(unique(oem_data$`Laser Mfg`)),
                      selected = NULL
          )
        ),
        column(
          6,
          offset=0,
          align = 'center',
          selectInput(selectize = T,
                      width = "100%",
                      inputId = "mod_dent",
                      label = h4(strong("Laser Model")),
                      choices = sort(unique(oem_data$`Laser Model`)),
                      selected = NULL
          )
        )),
      fluidRow(
        column(4,
               align = 'center',
               selectInput(
                 selectize = T,
                 width = "100%",
                 "lmfg",
                 h5(strong("Loupe Designer")),
                 choices = sort(unique(loupe_data$Mfg)), selected = NULL
               )),
        column(4, align = 'center',
               selectInput(
                 selectize = T,
                 width = "100%",
                 "lmod",
                 h5(strong("Design")),
                 choices = NULL)),
        column(4, align = 'center',
               selectInput(
                 selectize = T,
                 width = "100%",
                 "size",
                 h5(strong("Size")),
                 choices = NULL))
      ),

      fluidRow(
        column(
          12,
          align = "center",
          actionButton("run_dent",
                       icon = icon("magnifying-glass"),
                       style='box-shadow: 1px 1px 15px 0px darkgrey;
                       padding-left:50px;padding-right:50px;
                       padding-top:6px;
                       padding-bottom:1px;
                       font-size:80%',
                       h5(strong("Search")),
                       class = "btn-primary"))),



      conditionalPanel(
        condition = "input.run_dent",


        fluidRow(column(12, align = "center",
                        uiOutput("contact_info"))
                 ## h1(strong("To place an order, please, call Innovative Optics")))
        ),
        fluidRow(column(12, align = "center",
                        h5(tableOutput("table_loupe")))
        ),
        h1(
          htmlOutput("graphs_dent")))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Zolar Loupe Search Tool"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}









