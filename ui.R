
library("shiny")
library("shinythemes")

inputs <- function() {
  wellPanel(fluidRow(
    column(
      width = 4,
      checkboxGroupInput(
        "language",
        "Language",
        c("French (fr)" = "fr",
          "Dutch (nl)" = "nl"),
        selected = c("nl", "fr"),
        inline = FALSE
      )
    ),
    column(
      width = 4,
      checkboxGroupInput(
        "lexicon",
        "Lexicon",
        c("General" = "General",
          "Financial" = "Financial"),
        selected = c("Financial"),
        inline = FALSE
      )
    ),
    column(width = 4,
     checkboxGroupInput(
       "desk",
       "Desk",
       c(
         "Belgium" = "BIN INT",
         "Abroad" = "BTL EXT",
         "Sport" = "SPN SPF"
       ),
       selected = c()
     ))
  ))
}

navbarPage(
  title = "Sentometrics",
  header = "",
  theme = shinythemes::shinytheme("flatly"),
  tabPanel(
    "News",
    tags$style(type = "text/css"),
    div(style = "padding: 5px 5px 20px 5px",
      fluidRow(column(width = 12,
                    HTML("<b> March 2018 </b>")),
             column(
               width = 12,
               h4(
                 "Swiss National Science Foundation (SNSF) funding for Sentometrics research of David Ardia and Samuel Borms."
               )
             )),
    hr(),
    fluidRow(column(width = 12,
                    HTML(
                      "<b> November 2017 </b>"
                    )),
             column(width = 12,
                    h4(helpText(
                      a(
                        "Release of R package 'sentometrics'.",
                        href = "https://github.com/sborms/sentometrics",
                        target = "_blank"
                      )
                    )))),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> October 2017 </b>")),
             column(
               width = 12,
               h4(
                 "Innoviris Team Up full funding support granted to Sentometrics in partnership with Belga and Finvex."
               )
             )),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> June 2017 </b>")),
             column(width = 12,
                    h4(helpText(
                      a(
                        "Swissuniversities funding for Sentometrics research of Keven Bluteau.",
                        href = "https://www.linkedin.com/feed/update/urn:li:activity:6276875594202386432",
                        target = "_blank"
                      )
                    )))),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> May 2017 </b>")),
             column(width = 12,
                    h4(helpText(
                      a(
                        "White paper on Sentometrics released on SSRN.",
                        href = "https://www.linkedin.com/feed/update/urn:li:activity:6274895843879456768",
                        target = "_blank"
                      )
                    )))),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> May 2017 </b>")),
             column(width = 12,
                    h4(helpText(
                      a(
                        "Google Summer of Code 2017 funding for Samuel Borms to create the R package 'sentometrics'.",
                        href = "https://summerofcode.withgoogle.com/projects/#5832364795101184",
                        target = "_blank"
                      )
                    )))),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> April 2017 </b>")),
             column(
               width = 12,
               h4(
                 "Green light for first stage of Innoviris Team Up development of Sentometrics together 
                 with Belga and Finvex."
               )
             ))
    )
  ),
  tabPanel(
    "Team",
    tags$style(type = "text/css"),
    fluidRow(
      column(width = 12,
               HTML(paste0("<h4>Follow the team's research at ", 
                           "<a href='http://www.sentometrics.com' target = '_blank'>sentometrics.com</a>", 
                           " to stay up-to-date on how we apply econometrics
                           to textual sentiment!</h4>"))
           )
      ),
    fluidRow(
      column(width = 12,
                    h3("Academic Team")),
      column(width = 4,
             div(
               column(width = 11,
                      h4("Kris Boudt (Vrije Universiteit Brussel)"))
             )),
      column(width = 4,
             div(
               column(width = 11,
                      h4("David Ardia (Université de Neuchâtel)"))
             ))
    ),
    fluidRow(
      column(width = 4,
             div(
               column(width = 3,
                      imageOutput(
                        "boudt", width = "150px", height = "170px"
                      )),
               column(
                 offset = 1,
                 width = 8,
                 HTML(
                   "Prof. Kris Boudt is Associate Professor at the Vrije Universiteit Brussel with extensive
                   experience in (financial) econometrics. The past several years, he has been developing statistical
                   tools to analyse sentiment present in texts, like corporate publications. He also holds a position at
                   the Vrije Universiteit Amsterdam."
                 )
                 )
                 )),
      column(width = 4,
             div(
               column(width = 3,
                      imageOutput(
                        "ardia", width = "150px", height = "170px"
                      )),
               column(
                 offset = 1,
                 width = 8,
                 HTML(
                   "Prof. David Ardia is Assistant Professor at the Université de Neuchâtel as well as the Université
                   Laval. He is a specialist in the modelling of financial risks, with very strong computational skills,
                   who has been working with Prof. Boudt for close to a decade."
                 )
                 )
                 ))
               ),
    fluidRow(
          column(width = 4,
               div(column(
                 width = 12,
                 h4("Keven Bluteau (UniNE/VUB)")
               ))),
          column(width = 4,
               div(column(
                 width = 12,
                 h4("Samuel Borms (UniNE/VUB)")
               ))),
          column(width = 4,
               div(column(
                 width = 12,
                 h4("Andres Algaba (VUB)")
               ))),
          column(width = 4,
                 column(width = 3,
                        imageOutput(
                          "bluteau", width = "140px", height = "120px"
                        )),
                 column(
                   offset = 0,
                   width = 9,
                   HTML(
                     "Keven Bluteau is a PhD student, working on volatility modelling and how to use textual sentiment 
                     to forecast economic and financial variables."
                    )
                   )
                 ),
          column(width = 4,
                 column(width = 3,
                        imageOutput(
                          "borms", width = "140px", height = "120px"
                        )),
                 column(
                   offset = 0,
                   width = 9,
                   HTML(
                     "Samuel Borms is a PhD student devoted to textual sentiment analysis. He wrote the R software 
                     package 'sentometrics' during a Google Summer of Code project."
                    )
                   )
                 ),
          column(width = 4,
                 column(width = 3,
                        imageOutput(
                          "algaba", width = "140px", height = "120px"
                        )),
                 column(
                   offset = 0,
                   width = 9,
                   HTML(
                     "Andres Algaba is a PhD student who studies the time variation in financial time series. His latest
                     research focuses on text-based ESG indices."
                    )
                   )
                 ),
        column(width = 12,
                    h3(
                      "Industrial Partners"
                    )),
            column(width = 2,
                      column(width = 11,
                             imageOutput(
                               "belga", width = "150px", height = "200px"
                             )
                    )),
             column(width = 2,
                      column(
                        width = 11,
                        imageOutput("finvex", width = "150px", height = "200px")
                      )
             ))
      ),
  tabPanel(
    'Visualisation',
    tags$style(
      type = "text/css",
      HTML(
        ".shiny-output-error-validation {
        color: blue;
        font-size: 12px;
        }"
      )
    ),
    div(style = "font-size: 11px; padding-top: 0px",
        verticalLayout(
          fluidRow(column(
            width = 3,
            selectInput(
              "keywords",
              "Select Topic of Interest",
              c(
                "Politics" = "Political",
                "Terrorism" = "Terrorism",
                "Finance" = "Financial",
                "Football" = "Football",
                "Economy" = "Economy"
              ),
              multiple = FALSE,
              width = "100%",
              selected = c("Financial")
            )
          )),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Single Index",
              fluidRow(div(
                style = "font-weight: bold",
                column(width = 3,
                       tags$h4("Parameters")),
                column(width = 7,
                       tags$h4(
                         "Global Textual Sentiment Index"
                       )),
                column(width = 2,
                       tags$h4("Statistics"))
              )),
              fluidRow(
                column(
                  width = 3,
                  wellPanel(
                    style = "padding: 10px",
                    sliderInput(
                      "wLan",
                      "Weight to French Documents (%)",
                      min = 0,
                      max = 100,
                      value = 70
                    ),
                    sliderInput(
                      "wLex",
                      "Weight to the 'General' Lexicon (%)",
                      min = 0,
                      max = 100,
                      value = 45
                    )
                  ),
                  div(
                    column(
                      width = 5,
                      radioButtons(
                        "normalised2",
                        "Normalisation",
                        c("Yes" = TRUE,
                          "No" = FALSE),
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 7,
                      numericInput(
                        "minWords2",
                        "Min. Words per Document",
                        value = 150,
                        step = 10,
                        width = "90%"
                      )
                    )
                  ),
                  div(
                    column(
                      offset = 5,
                      width = 7,
                      numericInput(
                        "minDocs2",
                        "Min. Documents per Day",
                        value = 3,
                        step = 1,
                        width = "90%"
                      )
                    )
                  ),
                  div(column(
                    offset = 5,
                    width = 7,
                    numericInput(
                      "nSMA2",
                      "Moving Average Period",
                      value = 90,
                      min = 5,
                      max = 180,
                      width = "90%"
                    )
                  ))
                ),
                column(
                  width = 7,
                  div(style = "position: relative",
                      plotOutput("plotFull")),
                  div(
                    column(
                      width = 6,
                      dateRangeInput(
                        "dates2",
                        "Select Time Horizon",
                        min = "2010-01-01",
                        max = "2016-12-31",
                        start = "2010-01-01",
                        end = "2016-12-31",
                        format = "dd-mm-yyyy"
                      )
                    ),
                    column(
                      offset = 4,
                      width = 2,
                      downloadButton('exportFull',
                                     'Download Time Series', class = "exp"),
                      tags$head(
                        tags$style(
                          ".exp{background-color:white;} .exp{color: black;}
                          .exp{font-size: 9px;} .exp{padding: 4px}"
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 2,
                  div(
                    style = "font-size: 100%",
                    uiOutput("meanSent"),
                    uiOutput("nDocsFull"),
                    uiOutput("nWordsFull")
                  )
                )
              )
              
            ),
            tabPanel(
              "Multiple Indices",
              fluidRow(div(
                style = "font-weight: bold",
                column(width = 3,
                       tags$h4("Parameters")),
                column(width = 7,
                       tags$h4("Textual Sentiment Indices")),
                column(width = 2,
                       tags$h4("Statistics"))
              )),
              fluidRow(
                column(
                  width = 3,
                  inputs(),
                  div(
                    column(
                      width = 5,
                      radioButtons(
                        "normalised",
                        "Normalisation",
                        c("Yes" = TRUE,
                          "No" = FALSE),
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 7,
                      numericInput(
                        "minWords",
                        "Min. Words per Document",
                        value = 150,
                        step = 10,
                        width = "90%"
                      )
                    )
                  ),
                  div(
                    column(
                      offset = 5,
                      width = 7,
                      numericInput(
                        "minDocs",
                        "Min. Documents per Day",
                        value = 3,
                        step = 1,
                        width = "90%"
                      )
                    )
                  ),
                  div(column(
                    offset = 5,
                    width = 7,
                    numericInput(
                      "nSMA",
                      "Moving Average Period",
                      value = 90,
                      min = 5,
                      max = 180,
                      width = "90%"
                    )
                  ))
                ),
                column(
                  width = 7,
                  div(style = "position: relative",
                      plotOutput("plot")),
                  div(
                    column(
                      width = 6,
                      dateRangeInput(
                        "dates",
                        "Select Time Horizon",
                        min = "2010-01-01",
                        max = "2016-12-31",
                        start = "2010-01-01",
                        end = "2016-12-31",
                        format = "dd-mm-yyyy"
                      )
                    ),
                    column(
                      offset = 4,
                      width = 2,
                      downloadButton('export',
                                     'Download Time Series', class = "exp"),
                      tags$head(
                        tags$style(
                          ".exp{background-color:white;} .exp{color: black;}
                        .exp{font-size: 9px;} .exp{padding: 4px}"
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 2,
                  div(
                    style = "font-size: 100%",
                    tableOutput("stats"),
                    uiOutput("nDocs"),
                    uiOutput("nWords")
                  )
                )
              )
            ),
            tabPanel(
              "Influential Articles",
              fluidRow(div(style = "font-weight: bold",
                           column(
                             width = 4,
                             tags$h4("Parameters")
                           )
                          )
                       ),
              fluidRow(
                column(
                  width = 4,
                  numericInput(
                    "minWords3",
                    "Min. Words per Document",
                    value = 150,
                    step = 10,
                    width = "90%"
                  )
                ),
                column(
                  width = 4,
                  numericInput(
                    "nLeads",
                    "Number of Articles",
                    value = 5,
                    step = 1,
                    width = "90%"
                  )
                ),
                div(style = "padding-top: 0px",
                    column(
                      width = 3,
                      dateRangeInput(
                        "dates3",
                        "Select Time Horizon",
                        min = "2010-01-01",
                        max = "2016-12-31",
                        start = "2010-01-01",
                        end = "2016-12-31",
                        format = "dd-mm-yyyy"
                      )
                    ))
              ),
              fluidRow(
                div(
                  column(width = 12,
                         tags$h4("French Articles")),
                  column(width = 6,
                         tags$h5("General Lexicon")),
                  column(width = 6,
                         tags$h5("Financial Lexicon")),
                  column(width = 6,
                         tableOutput("GenFR")),
                  column(width = 6,
                         tableOutput("FinFR"))
                ),
                div(
                  column(width = 12,
                         tags$h4("Dutch Articles")),
                  div(
                    column(width = 6,
                           tags$h5("General Lexicon")),
                    column(width = 6,
                           tags$h5("Financial Lexicon")),
                    column(width = 6,
                           tableOutput("GenNL")),
                    column(width = 6,
                           tableOutput("FinNL"))
                  )
                )
              )
            )
          )
        ))
  )
)

