##### Load required packages ###################################################

library(magrittr)
library(OptGS)
options(shiny.sanitize.errors = T)

##### UI #######################################################################
ui <- shinydashboard::dashboardPage(
  ##### Dashboard: Header ######################################################
  shinydashboard::dashboardHeader(
    title      = "OptGS",
    titleWidth = 175
  ),
  ##### Dashboard: Sidebar #####################################################
  shinydashboard::dashboardSidebar(
    width = 175,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text    = "Home",
        tabName = "home",
        icon    = shiny::icon(name = "home")
      ),
      shinydashboard::menuItem(
        text    = "Design",
        tabName = "design",
        icon    = shiny::icon(name = "list-alt",
                              lib  = "glyphicon")
      ),
      shinydashboard::menuItem(
        text    = "About",
        tabName = "about",
        icon    = shiny::icon(name = "question")
      ),
      shinydashboard::menuItem(
        text    = "Source code",
        icon    = shiny::icon(name = "file-code-o"),
        href    = "https://github.com/mjg211/OptGS/"
      )
    )
  ),
  ##### Dashboard: Body ########################################################
  shinydashboard::dashboardBody(
    #tags$head(includeScript("google-analytics.js")),
    shinydashboard::tabItems(
      ##### Tab: Home ##########################################################
      shinydashboard::tabItem(
        tabName = "home",
        h1(strong("OptGS:"),
           "Optimal and near-optimal group-sequential designs for clinical ",
           "trials with continuous outcomes"),
        p("Welcome to the R Shiny graphical user interface (GUI) to the R ",
          "package OptGS, which is currently available from:"),
        a(href = "https://github.com/mjg211/OptGS",
          "https://github.com/mjg211/OptGS"),
        p(""),
        p("Within R, OptGS provides a suite of functions to assist with the ",
          "design, analysis, and visualization of randomized two-arm ",
          "group-sequential clinical trials with continuous outcome variables.",
          " Specifically, support is provided to perform sample size ",
          "calculations for each of the most popular applicable (non-optimal) ",
          "designs, along with optimal and near-optimal designs. Additional ",
          "functions allow point estimates, p-values, and confidence intervals",
          " to be determined for possible results in these designs. Plotting ",
          "functions also permit the informative depiction of several ",
          "important quantities."),
        p("At present, this GUI supports execution of certain commands for ",
          "design determination and plot production. Additional functionality ",
          "will be added over time."),
        p("See the 'Design' tab on the sidebar for code execution, or the",
          "'About' tab for further information on the GUI.")
      ),
      ##### Tab: Design (Normal) ###############################################
      shinydashboard::tabItem(
        tabName = "design",
        ##### Row 1: Design parameters & Design summary ########################
        shiny::fluidRow(
          shinydashboard::box(
            shiny::withMathJax(),
            shinyalert::useShinyalert(),
            shinyFeedback::useShinyFeedback(),
            shinyjs::useShinyjs(),
            id          = "design_parameters",
            title       = "Design parameters",
            width       = 4,
            solidHeader = T,
            status      = "primary",
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
            shiny::sliderInput(
              inputId = "design_J",
              label   = "Maximum number of stages:",
              min     = 2,
              max     = 5,
              value   = 2,
              step    = 1
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_J",
                size    = "m",
                colour  = "black"
              ),
            shiny::numericInput(
              inputId = "design_alpha",
              label   = "Desired type-I error-rate:",
              value   = 0.05,
              min     = 0,
              max     = 1,
              step    = 0.01
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_alpha",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_beta",
              label   = "Desired type-II error-rate:",
              value   = 0.2,
              min     = 0,
              max     = 1,
              step    = 0.025
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_beta",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_delta",
              label   = "Treatment effect to power for:",
              value   = 0.2,
              min     = 0,
              max     = NA,
              step    = 0.1
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_delta",
                size    = "m",
                colour  = "black"
              ),
            shiny::numericInput(
              inputId = "design_sigma0",
              label   = "Standard deviation in the control arm:",
              value   = 1,
              min     = 0,
              max     = NA,
              step    = 0.25
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_sigma0",
                size    = "m",
                colour  = "black"
              ),
            shiny::numericInput(
              inputId = "design_sigma1",
              label   = "Standard deviation in the experimental arm:",
              value   = 1,
              min     = 0,
              max     = NA,
              step    = 0.25
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_sigma1",
                size    = "m",
                colour  = "black"
              ),
            shiny::numericInput(
              inputId = "design_ratio",
              label   = "Allocation ratio:",
              value   = 1,
              min     = 0,
              max     = NA,
              step    = 0.25
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_ratio",
                size    = "m",
                colour  = "black"
              ),
            shiny::selectInput(
              inputId = "design_shape",
              label   = "Stopping boundary shape:",
              choices = c("Haybittle-Peto" = "haybittle_peto",
                          "Near-optimal"   = "near_optimal",
                          "Power-family"   = "power_family",
                          "Triangular"     = "triangular",
                          "Wang-Tsiatis"   = "wang_tsiatis"),
              selected = "near_optimal"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_shape",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_Delta"),
            shinyWidgets::prettySwitch(
              inputId = "design_quantile_sub",
              label   = "Use quantile substitution",
              status  = "info",
              value   = F,
              slim    = T
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_quantile_sub",
                size    = "m",
                colour  = "black"
              ),
            shinyWidgets::prettySwitch(
              inputId = "design_integer_n",
              label   = "Require integer sample sizes",
              status  = "info",
              value   = F,
              slim    = T
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_integer_n",
                size    = "m",
                colour  = "black"
              ),
            shinyWidgets::prettySwitch(
              inputId = "design_plots",
              label   = "Produce plots",
              status  = "info",
              value   = T,
              slim    = T
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_plots",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_density"),
            shiny::hr(),
            shiny::actionButton(
              inputId = "design_reset",
              label   = "  Reset inputs  ",
              icon    = shiny::icon(name = "eraser"),
              width   = "100%"
            ),
            shiny::hr(),
            shiny::uiOutput("design_warning"),
            shiny::actionButton(
              inputId = "design_update",
              label   = "  Update outputs  ",
              icon    = shiny::icon(name = "check-square-o"),
              width   = "100%"
            ),
            shiny::hr(),
            shiny::textInput(
              inputId = "design_filename",
              label   = "Report filename:",
              value   = "OptGS_design"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_filename",
                size    = "m",
                colour  = "black"
              ),
            tags$head(tags$style(".full_width{width:100%;}")),
            shiny::radioButtons(
              inputId = "design_format",
              label   = "Download format",
              choices = c("PDF"  = "pdf",
                          "HTML" = "html",
                          "Word" = "word"),
              selected = "pdf",
              inline   = T
            ),
            shiny::downloadButton(
              outputId = "design_report",
              label    = "  Download report  ",
              class    = "full_width"
            )
          ),
          shinydashboard::box(
            title       = "Design summary",
            width       = 8,
            solidHeader = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::withMathJax(
                shiny::htmlOutput("design_summary")
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        ##### Row 2: Value box outputs #########################################
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("design_n_box"),
          shinydashboard::valueBoxOutput("design_alpha_box"),
          shinydashboard::valueBoxOutput("design_power_box")
        ),
        ##### Row 3: Operating characteristics summary #########################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Operating characteristics summary",
            width       = 12,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shiny::column(
              width = 12,
              align = "center",
              shinycssloaders::withSpinner(
                DT::DTOutput("design_table",
                             height = "500px"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          )
        ),
        ##### Rows 4-6: Plots ##################################################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Stopping boundaries",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput("design_boundaries"),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          ),
          shinydashboard::box(
            title       = "Power curve",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput("design_power"),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Expected sample size curve",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput("design_ess"),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          ),
          shinydashboard::box(
            title       = "Median sample size curve",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput("design_median"),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Stopping probabilities: By stage and decision",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput("design_stopping_1"),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          ),
          shinydashboard::box(
            title       = "Stopping probabilities: By stage",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput("design_stopping_2"),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        ##### Row 8: Session information #######################################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Session Information",
            status      = "primary",
            solidHeader = T,
            width       = 12,
            collapsible = T,
            collapsed   = T,
            shiny::verbatimTextOutput("design_debug")
          )
        )
      ),
      ##### Tab: About #########################################################
      shinydashboard::tabItem(
        tabName = "about",
        h1("About"),
        p("This graphical user interface (GUI) is built upon (and in to)",
          "v.2.0.0 of the R package OptGS, written by James Wason and Michael",
          "Grayling (Newcastle University)."),
        p("The first-line response to a possible bug should be to submit it as",
          "a 'New issue' at:"),
        a(href = "https://github.com/mjg211/OptGS/issues",
          "https://github.com/mjg211/OptGS/issues"),
        p(),
        p("If the issue is more complex, or a patch is not provided in",
          "reasonable time, please contact James Wason at",
          "james.wason@newcastle.ac.uk, or Michael Grayling at",
          "michael.grayling@newcastle.ac.uk. Similarly, please feel free to",
          "contact with suggestions for new features, or for further support",
          "with using the package or GUI."),
        p("If you use OptGS, please cite it with:"),
        p("Wason JMS (2015) OptGS: An R package for finding near-optimal",
          "group-sequential designs.", em("J Stat Soft"),
          HTML("<b>66</b>(2)<b>:</b>1-13."), "DOI: 10.18637/jss.v066.i02."),
        p(),
        p("A selection of references related to the methodology used in",
          "OptGS are given below."),
        h4("References")

      )
    )
  ),
  title = "OptGS",
  skin  = "blue"
)

##### Server ###################################################################
server <- function(input, output, session) {

  ##### Initial set-up #########################################################

  shinyhelper::observe_helpers(withMathJax = T)

  ##### Design: shinyFeedback warning messages ################################

  shiny::observeEvent(input$design_alpha, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_alpha",
      condition = any(input$design_alpha <= 0,
                      input$design_alpha >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_beta, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_beta",
      condition = any(input$design_beta <= 0,
                      input$design_beta >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_delta, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_delta",
      condition = (input$design_delta <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_sigma0, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_sigma0",
      condition = (input$design_sigma0 <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_sigma1, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_sigma1",
      condition = (input$design_sigma1 <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_ratio, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_ratio",
      condition = (input$design_ratio <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(c(input$design_DeltaE,
                        input$design_DeltaF), {
    shinyFeedback::feedbackDanger(
      inputId   = "design_DeltaE",
      condition = input$design_DeltaE >= 1,
      text      = "Must be strictly less than 1")
    shinyFeedback::feedbackDanger(
      inputId   = "design_DeltaF",
      condition = input$design_DeltaF >= 1,
      text      = "Must be strictly less than 1")
  })

  shiny::observeEvent(input$design_filename, {
    shinyFeedback::feedbackWarning(
      inputId   = "design_filename",
      condition = any(strsplit(input$design_filename,
                               split = "")[[1]] %in%
                        c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text      = paste0('It is generally inadvisable to use the characters /',
                         ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Design: Dynamic UI elements ############################################

  output$design_Delta <- renderUI({
    if (input$design_shape == "wang_tsiatis") {
      shiny::numericInput(
        inputId = "design_DeltaWT",
        label   = "Stopping boundary's shape parameter:",
        value   = 0.5,
        min     = NA,
        max     = NA,
        step    = 0.1
      )
    } else if (input$design_shape == "power_family") {
      shiny::tagList(
        shiny::numericInput(
          inputId = "design_DeltaE",
          label   = "Efficacy boundary's shape parameter:",
          value   = 0.5,
          min     = NA,
          max     = 1,
          step    = 0.1
        ),
        shiny::numericInput(
          inputId = "design_DeltaF",
          label   = "Futility boundary's shape parameter:",
          value   = 0.5,
          min     = NA,
          max     = 1,
          step    = 0.1
        )
      )
    }
  })

  output$design_warning <- renderUI({
    if (all(input$design_J %in% c(4, 5),
            input$design_shape == "near_optimal")) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_density <- renderUI({
    if (input$design_plots) {
      shiny::selectInput(
        inputId = "design_density",
        label   = "Plot quality:",
        choices = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                    "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_reset, {
    shinyjs::reset("design_parameters")
  })

  ##### Design: des() ##########################################################

  des <- shiny::eventReactive(input$design_update, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Identifying design",
                 value   = 0)
    if (input$design_shape != "near_optimal") {
      if (input$design_shape == "wang_tsiatis") {
        Delta <- input$design_DeltaWT
      } else if (input$design_shape == "power_family") {
        Delta <- c(input$design_DeltaE, input$design_DeltaF)
      } else {
        Delta <- 0
      }
      design  <-
        OptGS::des_gs(J            = input$design_J,
                      alpha        = input$design_alpha,
                      beta         = input$design_beta,
                      delta        = input$design_delta,
                      sigma0       = input$design_sigma0,
                      sigma1       = input$design_sigma1,
                      ratio        = input$design_ratio,
                      shape        = input$design_shape,
                      Delta        = Delta,
                      quantile_sub = input$design_quantile_sub,
                      integer_n    = input$design_integer_n)
    } else {
      design <-
        OptGS::des_nearopt(J            = input$design_J,
                           alpha        = input$design_alpha,
                           beta         = input$design_beta,
                           delta        = input$design_delta,
                           sigma0       = input$design_sigma0,
                           sigma1       = input$design_sigma1,
                           ratio        = input$design_ratio,
                           w            = c(1, 0, 0, 0),
                           quantile_sub = input$design_quantile_sub,
                           integer_n    = input$design_integer_n)
    }
    design$opchar_og <- design$opchar
    progress$inc(amount  = 0.25,
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_summary.html"),
      params        = list(J            = input$design_J,
                           alpha        = input$design_alpha,
                           beta         = input$design_beta,
                           delta        = input$design_delta,
                           sigma0       = input$design_sigma0,
                           sigma1       = input$design_sigma1,
                           ratio        = input$design_ratio,
                           shape        = input$design_shape,
                           quantile_sub = input$design_quantile_sub,
                           integer_n    = input$design_integer_n,
                           n0           = design$n0,
                           n1           = design$n1,
                           e            = design$e,
                           f            = design$f,
                           opchar       = design$opchar,
                           plots        = input$design_plots)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_summary_modified.html")
    )
    progress$inc(amount  = 0.25,
                 message = "Rendering plots")
    design$boundaries       <- plot(design, output = T)$plots$J
    if (input$design_plots) {
      opchar                <-
        opchar(design,
               tau = seq(-input$design_delta, 2*input$design_delta,
                         length.out = as.numeric(input$design_density)))
      design$opchar         <- rbind(design$opchar, opchar$opchar)
      plots                 <- plot(opchar, output = T)
      design$ess            <- plots$plots$`ESS(tau)`
      design$median         <- plots$plots$`MSS(tau)`
      design$power          <- plots$plots$`P(tau)`
      design$stopping_1     <- plots$plots$rejection
      design$stopping_2     <- plots$plots$stopping
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      row.names(design$opchar) <-
                 c("<i>H</i><sub>0</sub>",
                   "argmax<sub><i>&tau;</i></sub><i>ESS</i>(<i>&tau;</i>)",
                   "<i>H</i><sub>1</sub>",
                   paste0("Op. Char. #", 1:as.numeric(input$design_density)))
    } else {
      design$ess            <- design$median     <- design$power      <-
                               design$stopping_1 <- design$stopping_2 <- NULL
      row.names(design$opchar) <-
        c("<i>H</i><sub>0</sub>",
          "argmax<sub><i>&tau;</i></sub><i>ESS</i>(<i>&tau;</i>)",
          "<i>H</i><sub>1</sub>",
          paste0("Op. char. #", 1:as.numeric(input$design_density)))
    }
    seq_J                   <- 1:input$design_J
    colnames(design$opchar) <-
      c("<i>&tau;</i>",
        paste0("<i>", c("P", "ESS", "SDSS", "MSS"), "</i>(<i>&tau;</i>)"),
        paste0("<i>E</i><sub>", seq_J, "</sub>(<i>&tau;</i>)"),
        paste0("<i>F</i><sub>", seq_J, "</sub>(<i>&tau;</i>)"),
        paste0("<i>S</i><sub>", seq_J, "</sub>(<i>&tau;</i>)"),
        paste0("cum{<i>S</i><sub>", seq_J, "</sub>(<i>&tau;</i>)}"),
        "max <i>n</i>")
    progress$inc(amount  = 0.25,
                 message = "Outputting results")
    design
  })

  ##### Design: Value boxes ####################################################

  output$design_n_box <- shinydashboard::renderValueBox({
    input$design_update
    shinydashboard::valueBox(
      value    = round(des()$n[1], 1),
      subtitle = "Stage-wise sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_alpha_box <- shinydashboard::renderValueBox({
    input$design_update
    print(des()$opchar)
    if (des()$opchar[1, 2] <=
        shiny::isolate(input$design_alpha) + 1e-4) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(des()$opchar[1, 2], 3),
      subtitle = "Type-I error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_power_box <- shinydashboard::renderValueBox({
    input$design_update
    if (des()$opchar[3, 2] >=
        1 - shiny::isolate(input$design_beta) - 1e-3) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(1 - des()$opchar[3, 2], 3),
      subtitle = "Type-II error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  ##### Design: Summary #######################################################

  output$design_summary <- shiny::renderUI({
    input$design_update
    n <- des()$n
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_summary_modified.html")
      )
    )
  })

  ##### Design: Table ##########################################################

  output$design_table <- DT::renderDT({
    DT::datatable(
      round(des()$opchar, 3),
      escape        = F,
      fillContainer = T
    )
  })

  ##### Design: Plots ##########################################################

  output$design_boundaries <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$boundaries
    }
  })

  output$design_ess <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$ess
    }
  })

  output$design_median <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$median
    }
  })

  output$design_power <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$power
    }
  })

  output$design_stopping_1 <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$stopping_1
    }
  })

  output$design_stopping_2 <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$stopping_2
    }
  })

  ##### Design: Report #########################################################

  output$design_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_filename, sep = '.',
            switch(input$design_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_report.Rmd")
      file.copy("design_report.Rmd", tempReport, overwrite = T)
      params     <- list(J            = des()$K,
                         alpha        = des()$alpha,
                         beta         = des()$beta,
                         delta        = des()$delta,
                         sigma0       = des()$sigma0,
                         sigma1       = des()$sigma1,
                         ratio        = des()$ratio,
                         shape        = input$design_shape,
                         quantile_sub = des()$quantile_sub,
                         integer_n    = des()$integer_n,
                         plots        = input$design_plots,
                         n0           = des()$n0,
                         n1           = des()$n1,
                         opchar       = des()$opchar_og,
                         e            = des()$e,
                         f            = des()$f,
                         boundaries   = des()$boundaries,
                         power        = des()$power,
                         ess          = des()$ess,
                         median       = des()$median,
                         stopping_1   = des()$stopping_1,
                         stopping_2   = des()$stopping_2)
      if (input$design_format == "pdf") {
        format   <- "pdf_document"
      } else if (input$design_format == "html") {
        format   <- "html_document"
      } else {
        format   <- "word_document"
      }
      rmarkdown::render(tempReport,
                        output_format = format,
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Session Info ###########################################################

  output$design_debug <- shiny::renderPrint({
    utils::sessionInfo()
  })

  ##### Close set-up ###########################################################

  session$onSessionEnded(stopApp)

}

shiny::shinyApp(ui, server)
