#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import purrr
#' @import shinyjs
#' @noRd



options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)


googledrive::drive_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")
googlesheets4::gs4_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")


sheet_id <- googledrive::drive_get("Dental_data")$id


our_data <- googlesheets4::read_sheet(sheet_id, sheet = "Lens_details") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)),
         `Price(from)` = scales::dollar(as.numeric(`Price(from)`)))


oem_data <- googlesheets4::read_sheet(sheet_id, sheet = "laser_info") %>%
  filter(`Laser Mfg` == "Zolar")


app_server <- function(input, output, session) {
  observeEvent(input$lmfg,{
    mfg_filt <- loupe_data %>%
      filter(Mfg == input$lmfg)

    updateSelectInput(inputId = "lmod",
                      choices = sort(unique(mfg_filt$Mod)))

    if (length(mfg_filt$Mod) == 1) {
      hide(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mfg_filt$Size))
    }
    else {
      show(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mfg_filt$Size))
    }

  })



  observeEvent(input$lmod,{
    req(input$lmfg)
    mod_filt <- loupe_data %>%
      filter(Mfg == input$lmfg,
             Mod == input$lmod)
    if (length(mod_filt$Mod) == 1) {
      hide(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mod_filt$Size))
    }
    else {
      show(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mod_filt$Size))
    }
  })
  filt_data_loupe <- reactive({
    req(input$lmfg)
    req(input$lmod)
    req(input$size)


    wavelength_df <- oem_data %>%
      filter(`Laser Mfg` == input$mfg_dent & `Laser Model`== input$mod_dent)

    wavelength <- wavelength_df$Wavelengths

    loupe_data %>%
      filter(Mfg == input$lmfg &
               Mod == input$lmod &
               Size == input$size) %>%
      mutate("Selected Device" = paste0(input$mod_dent," (", wavelength, ")"),
             "Selected Loupes" = paste0(`Mfg`, " ", `Mod`, " (", `Size`, ") "),
             "Compatible Loupe Insert" = `Insert Part Number`,
             .keep = "none")%>%
      distinct()

  })

  output$contact_info <- renderUI({
    if(input$lmfg == "Andau"){
      h1(strong("To place an order, please, call Andau Customer Service 1-844andau88"))
    } else{
      h1(strong("Please call Innovative Optics at 763-425-7789 with any questions"))    }
  })

  output$table_loupe <- renderTable(
    striped = T,
    hover = T,
    bordered = T,
    spacing = c("s"),
    width = "auto",
    align = "c",
    {tibble(
      filt_data_loupe()[,1:2],
      "Compatible Loupe Insert" = paste0(filt_data_loupe()[,3], ".", filt_data_laser()[[1]]$Lens))}
  )


  observeEvent(input$mfg_dent,{
    dent_mod <- oem_data %>%
      filter(`Laser Mfg` == input$mfg_dent)
    updateSelectInput(inputId = "mod_dent",
                      choices = sort(unique(dent_mod$`Laser Model`)))

  })

  filt_data_laser <- eventReactive(c(input$mod_dent, input$lmfg),{
    dent_mod <- oem_data %>%
      filter(`Laser Model` == input$mod_dent)
    result <- map(unique(dent_mod$`Eyewear Lens Compatible`), ~tibble(filter(our_data, Lens == .x)))

    ## Make the price NA for Andau
    ##if(input$lmfg == "Andau") {
    ##  result <- lapply(result, function(x) {
    ##    x[['Price(from)']] <- "<br>"
    ##    x
    ##  })
    ##}
    ##result
  })

  ### get wavelengths




  filt_data_combined <- reactive({
    req(filt_data_laser())
    req(filt_data_loupe())

    filt_data_laser <- filt_data_laser()
    filt_data_loupe <- filt_data_loupe()

    print("Compatible Loupe Insert\n")
    print(filt_data_loupe$`Compatible Loupe Insert`)

    #call library to test for lens, change image and link to corresponding landing page
    DentalLibrary::generate_filt_data_combined(filt_data_laser, filt_data_loupe)


    #################################################################################################
    ### Test for Pi1, change image and landing page
    #################################################################################################

  })

  output$graphs_dent <- renderUI({
    req(filt_data_laser())
    req(filt_data_loupe())

    filt_data_laser <- filt_data_combined()$filt_data_laser
    filt_data_loupe <- filt_data_combined()$filt_data_loupe

    print(filt_data_laser[[1]]$Website)

    ##print(filt_data_loupe$`Compatible Loupe Insert`)
    ## print(all(sapply(filt_data_laser, "[[", 'Lens') == "Pi1"))

    html_code <- map(1:length(filt_data_laser), ~HTML(
      c(
        '<div class="shadow p-3 mb-5 bg-body rounded">
      <div class="row">
      <div class="col-sm-5" id="left-section">
        <div align="left">
        <p>CLICK HERE TO ORDER</p>
        <a href="',
        filt_data_laser[[.x]]$Website,
        '", target = "_blank", title = "', filt_data_laser[[.x]]$Lens,'frame styles">',
        paste0(filt_data_loupe[,3], ".", filt_data_laser[[1]]$Lens),'
        </a>
        </div>
        <div align="center">
        <a href="',
        filt_data_laser[[.x]]$Website,
        '", target = "_blank", title = "', filt_data_laser[[.x]]$Lens,'frame styles">
        <img src="',
        filt_data_laser[[.x]]$Image,
        '", width = 40%>
        </a>
        <a href="',
        filt_data_laser[[.x]]$Website,
        '", target = "_blank", title = "', filt_data_laser[[.x]]$Lens,'frame styles">
        <img src="',
        filt_data_laser[[.x]]$Graph,
        '", width = 100%>
        </a>

        </div>
        </div>
        <div class="col-sm-7">

        <dl>
        <dt style="font-size:0.55em", align="left"><strong>Lens Material</strong></dt><dd style="font-size:0.55em", align="left"> ',filt_data_laser[[.x]]$Material,'</dd>

        <dt style="font-size:0.55em", align="left"><strong>Optical Density Marking </strong></dt><dd style="font-size:0.55em", align="left">',filt_data_laser[[.x]]$OD,'</dd>
        <dt style="font-size:0.55em", align="left"><strong>VLT </strong></dt> <dd style="font-size:0.55em", align="left">',filt_data_laser[[.x]]$VLT,'</dd>
        </dl>
        </div>
        </div>
      </div>'
      )
    ))

#    if(input$lmfg == "Andau"){
#      html_code <- map(html_code,
#                       ~ gsub('<div class="col-sm-5" id="left-section">', '<div class="col-sm-5" id="left-section" style="visibility:hidden">', .x) %>%
#                         HTML())
#    }

    if(all(sapply(filt_data_laser, "[[", 'Website') %in%
           c("https://innovativeoptics.com/pi1-laser-glasses-frames/", "https://innovativeoptics.com/pi17-laser-glasses-frames/",
             "https://innovativeoptics.com/pi23-laser-glasses-frames/", "https://innovativeoptics.com/pi19-laser-glasses-frames/"))){

      pattern <- 'href=" https://innovativeoptics.com/pi(1|17|23|19)-laser-glasses-frames/ ", '

      html_code <- map(html_code, ~ gsub(pattern, '', .x)  %>%
                         HTML())

      pattern2 <- 'CLICK HERE TO ORDER'

      html_code <- map(html_code, ~ gsub(pattern2, 'To order: call 763-425-7789', .x)  %>%
                         HTML())
    }


    html_code

  })

  observeEvent(input$run_dent, {
    hide(anim = T,
         time = 0.33,
         animType = "slide",
         "run_dent")
  })
}





