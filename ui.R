####   Shiny ui                       ####
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main section
# ------------------

ui <- navbarPage(
  "GraduateEmployment",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "GraduatEmploymentSG",
      img(src = "sg0.jpg", width = "100%", class = "bg"),
    )),
    
    tags$br(),
    
    

    ####  Panel: Main>Summary             ####

    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        #### Panel: Main>Summary>Tables & Pie Chart ####

        # ranking $ pie chart section
        
        sidebarLayout(
          sidebarPanel(
            h3("Dữ liệu theo năm"),
            tags$br(),
            selectInput(
              "checkYear",
              "Chọn năm",
              #choices = list("2018", "2017", "2016",
                             #"2015", "2014", "2013"),
              #selected = "2018"
              choices = data_e$year
            )
          ),
          
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel("Xếp hạng", tableOutput("datahead")),
              tabPanel("Số sinh viên tốt nghiệp", plotOutput(outputId = "piePlot"))
            ),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
        
        
        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------
            
            h3("Tổng quan về dữ liệu"),
            tags$br(),
            setSliderColor(c("#000000 ", "#000000"), c(1, 2)),
            sliderInput(
              "incomeRange",
              label = "Phạm vi lương",
              min = 1600,
              max = 5000,
              value = c(1600, 5000)
            ),
            # setSliderColor(c("fca311 ", "#fca311"), c(1, 2)),
            sliderInput(
              "employRange",
              label = "Phạm vi tỉ lệ tốt nghiệp",
              min = 0,
              max = 100,
              value = c(0, 100)
            ),
            selectInput(
              "checkYearGroup",
              "Chọn năm",
              choices = data$year,
              selected = "2018",
              multiple = TRUE
            ),
            
            #checkboxGroupInput("checkYear", label = "Select Year",
            #                  choices = list("2013", "2014", "2015", "2016", "2017", "2018"),
            #                 selected = list("2013", "2014", "2015", "2016", "2017", "2018"), inline = TRUE),
            
            actionButton("actionDT", "Kết quả", class = "btn btn-warning"),
          ),
          mainPanel(
            h3("Bảng thống kê dữ liệu"),
            tags$br(),
            dataTableOutput("myTable"),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
      ),
      
      

      #### Panel: Main>Plots                      ####

      
      tabPanel(
        "So sánh trực quan",
        
        # --------------------
        # density plot section
        # --------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Bảng điều khiển mật độ"),
            tags$br(),
            selectInput(
              "selectvar",
              label = "Chọn giá trị hiển thị",
              choices = c(
                "Basic Montly Salary (Median)" = "basic_monthly_median",
                "Fulltime Employment Rate" = "employment_rate_ft_perm"
              ),
              selected = "basic monthly mean"
            ),
            
            checkboxGroupInput(
              "checkGroup",
              label = "Chọn trường đại học",
              choices = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              ),
              selected = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              )
            ),
          ),
          mainPanel(
            h3("Phân bổ"),
            plotlyOutput(outputId = "densityPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # bar plot section
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Bảng điều khiển thanh"),
            tags$br(),
            radioButtons(
              "radio",
              label = "Chọn trường đại học",
              choices = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              ),
              selected = "Nanyang Technological University"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Thu nhập trung bình theo trường (tổng hợp)"),
            plotlyOutput(outputId = "uniPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # box plot section
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Bảng điều khiển hộp"),
            tags$br(),
            checkboxGroupInput(
              "checkGroupbox",
              label = "Chọn trường đại học",
              choices = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              ),
              selected = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              )
            ),
            
            tags$hr()
          ),
          mainPanel(
            h3("So sánh thu nhập trung bình (tổng hợp)"),
            plotlyOutput(outputId = "boxPlot"),
            tags$br(),
            tags$br(),
            tags$br(),
          )
        ),
        
        tags$hr(),
        
        # --------------------
        # Scatter plot section
        # --------------------
        
        
        fluidPage(fluidRow(
          h3("Tỷ lệ việc làm toàn thời gian so với thu nhập trung bình theo trường đại học năm 2018"),
          align = "center",
          plotlyOutput(outputId = "scatPlot", width = "100%"),
          div(style = "height:400px")
        )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        
      ),
      
      
      ################################################
      #### Panel: Main>Details                    ####
      ################################################
      
      tabPanel(
        "Chi tiết các trường đại học",
        h3("Thu nhập của sinh viên tốt nghiệp và tỷ lệ việc làm theo năm", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(
                4,
                selectInput(
                  "detailUniversity",
                  label = "Chọn trường đại học",
                  choices = unique(data$university),
                  selected = "National University of Singapore",
                  width = 400
                ),
              ),
              column(
                4,
                selectInput(
                  "detailSchool",
                  "Chọn ngành",
                  choices = "",
                  selected = "",
                  width = 400
                )
              ),
              column(4,
                     column(
                       8,
                       selectInput(
                         "detailMajor",
                         "Chọn chương trình",
                         choices = "",
                         selected = "",
                         width = 400
                       )
                     ),
                     column(
                       4,
                       tags$br(),
                       actionButton("detailFilter", "Filter", class = "btn btn-warning btn-sm")
                     ))
            )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        
        fluidRow(
          column(4, tableOutput("detailTable")),
          column(4, h5("Montly Median Income", align="center"), plotOutput(outputId = "detailPlot", height = "300px")),
          column(4, h5("Fulltime Employment rate", align="center"), plotOutput(outputId = "detailPlotem", height = "300px"))
        ),
        
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br()
      )
    )
  ),
  
  
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  #tabPanel("Documentation",
   #        fluidPage(htmlOutput("doc"))),
  
  ################################################
  #### Panel: About                           ####
  ################################################
  #tabPanel("About",
   #        fluidPage(htmlOutput("abo")))
)
