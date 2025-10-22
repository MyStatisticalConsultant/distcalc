#################################################
#
#  D I S T R I B U T I O N   C A L C U L A T O R
#
#################################################
library(shiny)
library(shinydashboard)
library(openintro)
library(markdown)
#
source('www/chiTail.R')
source('www/FTail.R')
source('www/normTail.R')
#
defaults = list("tail" = "lower",
                "lower_bound" = "open",
                "upper_bound" = "open")
#
# #####################################
#               B O D Y 
# #####################################

body <- dashboardBody(
  tabItems(
    #
    # Intro tab contains information about application. 
    #
    tabItem("intro", 
	         fluidRow(
			  box(
                solidHeader=TRUE, collapsible = FALSE, width = 12, 
				tags$div(tags$br(), tags$br(), tags$h2("About the distribution calculator"), tags$h4("The Distribution Calculator makes it easy to:", tags$p(" "), tags$ol(tags$li("Calculate areas under a specific distribution curve."),tags$li("Simulate random samples from a distribution."),tags$li("Calculate critical values for a specific distribution."), tags$li("Learn R codes used to generate results. ")), tags$p("Access the calculator by clicking on", tags$i("Distribution Calculator"), "on the left sidebar. This will display the output in four boxes.")))
              )), 
			fluidRow(
			  box(
                title="Box 1: Areas under a distribution", status="primary", solidHeader=TRUE, collapsible = TRUE, tags$div(
			    tags$h5("To calculate the area under a distribution curve, enter the following information in the sidebar:", tags$p(" "),  tags$ol(tags$li("Specify a distribution."),tags$li("Specify the parameters of the distribution (e.g. mean and standard deviation for Normal distribution) by using the drop-down list. The required parameters will change, depending on your choice of distribution."), tags$li("Indicate whether you want to find the area above a certain value (", tags$i("Upper Tail"), "), below a certain value (", tags$i("Lower Tail"),"), between two values (", tags$i("Middle"), "), or outside two values (", tags$i("Both Tails"), ") by using the", tags$i("Find area"), "drop down menu."), tags$li("Enter the value of a. For instance, if you want to find the area above 10, enter 10. If you have chosen", tags$i("Middle"), "or", tags$i("Both Tails"), "in step 3, two drop-down lists will appear, and you need to enter two values, a and b")), tags$p("The area will be shaded and the size of the area will be shown under the curve in Box 1.")))		
              ),			  
			  box(
                title="Box 2: Simulation of random samples from a distribution", status="success", solidHeader=TRUE, collapsible = TRUE, tags$div(
              tags$h5("To simulate and display the distribution of random samples from a specific distribution:", tags$p(" "),  tags$ol(tags$li("Specify the distribution and the parameters in the sidebar."), tags$li("Select the number of observations by using the slider in Box 2."), tags$li("Insert a density curve over the histogram by clicking on", tags$i("Draw density curve"), "in Box 2.")))		  
              ))),
			fluidRow(
			  box(
                title="Box 3: Calculate critical values", status="warning", collapsible = TRUE, tags$div(tags$h5("To calculate the critical values (i.e. the points which cut off a given probability) for a specific distribution:", tags$p(" "),  tags$ol(tags$li("Specify the distribution and the parameters in Box 3."),tags$li("Specify a probability"),tags$li("Select whether the probability relates to the upper tail, the lower tail or both tails.")), tags$p("The critical value(s) will be shown at the bottom of Box 3. ")))
              ),	
			  box(
                title="Box 4: R codes", status="danger", solidHeader=TRUE, collapsible = TRUE, tags$div(
                tags$h5("R codes used to generate results:", tags$p(" "), tags$ol(tags$li("List of basic R functions used in this application is given here."), tags$li("To learn more about specific R function and its arguments type the question mark followed by the function name (e.g. ?hist) in the RStudio console."))))			  
              ))),
			
			tabItem("calculator", 
			fluidRow(
			  box(
                title="Box 1: Areas under a distribution", status="primary", solidHeader=TRUE, collapsible = FALSE, height=600, 
                plotOutput("plot"),
                div(textOutput("area"), align = "center", style="font-size:150%;")
              ),			  
			  box(
                title="Box 2: Simulation of random samples from a distribution", status="success", solidHeader=TRUE, collapsible = FALSE, height=600, 
              plotOutput("hist"),
			  uiOutput("slider"),
			  uiOutput("drawDensity")			  
              )),
			fluidRow(
			  tabBox(
                title="Box 3: Calculate critical values", id="tabbox1", height=600, 

				tabPanel("Normal", h4("Critical values for normal distribution"), numericInput("cvNormMean", "Mean:", value=0, step=1), numericInput("cvNormSD", "Standard deviation:", value=1, min=0.1, step=1), numericInput("cvProbNorm","Probability", value=0.05, min=0, max=1, step=0.01), radioButtons("tailsNorm", "Tails:", c("Lower tail" = "lowertail", "Upper Tail" = "uppertail", "Both tails" = "bothtails"), selected="bothtails", inline = TRUE), p("Critical value(s):"), div(textOutput("cvNorm"),style="font-size:150%;")),

				tabPanel("Binomial", h4("Critical values for binomial distribution"), numericInput("cvBinSize", "Number of observations (n):", value=10, step=1, min=1), numericInput("cvBinProb", "Probability of success (p):", value=0.5, min=0, max=1, step=0.1), numericInput("cvProbBin","Probability", value=0.05, min=0, max=1, step=0.01), radioButtons("tailsBin", "Tails:", c("Lower tail" = "lowertail", "Upper Tail" = "uppertail", "Both tails" = "bothtails"), selected="bothtails", inline = TRUE), p("Critical value(s):"), div(textOutput("cvBin"),style="font-size:150%;")), 

				tabPanel("t", h4("Critical values for t-distribution"), numericInput("cvTDf", "Degrees of freedom:", value=10, step=1), numericInput("cvProbT","Probability", value=0.05, min=0, max=1, step=0.01), radioButtons("tailsT", "Tails:", c("Lower tail" = "lowertail", "Upper Tail" = "uppertail", "Both tails" = "bothtails"), selected="bothtails", inline = TRUE), p("Critical value(s):"), div(textOutput("cvT"),style="font-size:150%;")), 

				tabPanel("Chi-squared", h4("Critical values for chi-squared distribution"), numericInput("cvChisqDf", "Degrees of freedom:", value=10, step=1), numericInput("cvProbChisq","Probability", value=0.05, min=0, max=1, step=0.01), radioButtons("tailsChisq", "Tails:", c("Lower tail" = "lowertail", "Upper Tail" = "uppertail", "Both tails" = "bothtails"), selected="uppertail", inline = TRUE), p("Critical value(s):"), div(textOutput("cvChisq"),style="font-size:150%;")), 

				tabPanel("F", h4("Critical values for F-distribution"), numericInput("cvFDf1", "Degrees of freedom (1):", value=10, step=1), numericInput("cvFDf2", "Degrees of freedom (2):", value=10, step=1), numericInput("cvProbF","Probability", value=0.05, min=0, max=1, step=0.01), radioButtons("tailsF", "Tails:", c("Lower tail" = "lowertail", "Upper Tail" = "uppertail", "Both tails" = "bothtails"), selected="uppertail", inline = TRUE), p("Critical value(s):"), div(textOutput("cvF"),style="font-size:150%;"))
              ), 
			  box(
                title="Box 4: R codes", status="danger", solidHeader=TRUE, collapsible = FALSE, height=600, 
              includeMarkdown("www/include.md")			  
              )			  
			  )
			)
    ))

# #######################################
#      U S E R   I N T E R F A C E
# #######################################
ui <- dashboardPage(
  dashboardHeader(title = "Distribution Calculator"),
  
  dashboardSidebar(
    # 
    #
    # 
    sidebarMenu(id="tabs", menuItem("About Calculator", tabName = "intro", icon = icon("info-circle")), menuItem("Distribution Calculator", tabName = "calculator", icon = icon("calculator")),
    #radio button or dropdown?

    selectInput(inputId = "dist",
                label = h5("Distribution:"),
                choices = c("Normal"      = "rnorm",
                            "Binomial"    = "rbinom",
                            "t"           = "rt",
                            "F"           = "rf",
                            "Chi-Squared" = "rchisq"),
                selected = "rnorm"),

    br(),

    uiOutput("mean"),
    uiOutput("sd"),
    uiOutput("df1"),
    uiOutput("df2"),
    uiOutput("n"),
    uiOutput("p"),

    div(p("Requested probability:"), style="text-align:center;color:white;"),
    div(textOutput("model"),style="text-align:center;font-size:125%;"),
    
    uiOutput("tail"),
    uiOutput("lower_bound"),
    uiOutput("upper_bound"),
    
    uiOutput("a"),
    uiOutput("b")                
    )
  ),
  body
)

# #######################################
#             S E R V E R
# #######################################
server <- function(input, output, session) 
{
  output$tail = renderUI(
  {
    #print("tail")
    if (input$dist == "rbinom")
    {
      selectInput(inputId = "tail",
                  label = "Find area:",
                  choices = c("Lower Tail"="lower", 
                              "Upper Tail"="upper", 
                              "Both Tails"="both",
                              "Middle"="middle",
                              "Equality"="equal"),
                  selected = "lower")
    }
    else if (input$dist == "rf" | input$dist == "rchisq"){
      selectInput(inputId = "tail",
                  label = "Find area:",
                  choices = c("Upper Tail"="upper"),
                  selected = "upper")
    }
    else
    {
      selectInput(inputId = "tail",
                  label = "Find area:",
                  choices = c("Lower Tail"="lower", 
                              "Upper Tail"="upper", 
                              "Both Tails"="both",
                              "Middle"="middle"),
                  selected = "lower")
    }
  })

  output$lower_bound = renderUI(
  {
    #print("lower bound")

    if (input$dist == "rbinom")
    {
      if (is.null(input$tail))
      {
        shiny:::flushReact()
        return()
      }

      if (input$tail %in% c("both","middle"))
      {
        symbol <- "<="
		selectInput(inputId = "lower_bound",
                    label = "Lower bound:",
                    choices = c("< (less than)" = "open", 
                                "<= (less than or equal to)" = "closed"),
                    selected = "open")
      }
      else if (input$tail == "lower")
      {
        selectInput(inputId = "lower_bound",
                    label = "Bound:",
                    choices = c("< (less than)" = "open", 
                                "<= (less than or equal to)" = "closed"),
                    selected = "open")
      }
      else if (input$tail == "upper")
      {
		selectInput(inputId = "lower_bound",
                    label = "Bound:",
                    choices = c("> (greater than)" = "open", 
                                ">= (greater than or equal to)" = "closed"),
                    selected = "open")
      }
    }
  })

  output$upper_bound = renderUI(
  {
    #print("upper bound")

    if (input$dist == "rbinom")
    {
      if (is.null(input$tail))
      {
        shiny:::flushReact()
        return()
      }

      if (input$tail == "middle")
      {
        selectInput(inputId = "upper_bound",
                    label = "Upper bound:",
                    choices = c("< (less that)" = "open", 
                                "<= (less than or equal to)" = "closed"),
                    selected = "open")
      }
      else if (input$tail == "both")
      {
        selectInput(inputId = "upper_bound",
                    label = "Upper bound:",
                    choices = c("> (greater than)" = "open", 
                                ">= (greater than or equal to)" = "closed"),
                    selected = "open")
      }
    }
  })

  get_model_text = reactive(
  {
    if (is.null(input$tail)){
      shiny:::flushReact()
      return()
    }

    low_less = "<"
    low_greater = ">"

    up_less = "<"
    up_greater = ">"

    if (input$dist == "rbinom" & input$tail != "equal")
    {
      if (is.null(input$lower_bound))
      {
        shiny:::flushReact()
        return()
      }

      if (input$lower_bound == "closed")
      {
        low_less = "<="
        low_greater = ">="
      }

      if (input$tail %in% c("middle","both"))
      { 
        if (is.null(input$upper_bound)){
          shiny:::flushReact()
          return()
        }

        if (input$upper_bound == "closed")
        {
          up_less = "<="
          up_greater = ">="
        }
      }
    }

    text = ""
    if (length(input$tail) != 0)
    {
      if (input$tail == "lower")
      {
        # P(X < a)
        text = paste0("P(X ", low_less, " a)")
      }
      else if (input$tail == "upper")
      {
        # P(X > a)
        text = paste0("P(X ", low_greater, " a)")
      }
      else if (input$tail == "middle")
      {
        # P(a < X < b)
        text = paste0("P(a ", low_less, " X ", up_less, " b)")
      }
      else if (input$tail == "both")
      {
        # P(X < a or X > b)
        text = paste0("P(X ", low_less, " a or X ", up_greater, " b)")
      }
      else if (input$tail == "equal")
      {
        # P(X = a)
        text = paste0("P(X = a)")
      }
    }

    return(text)
  })

  output$model = renderText(
  {
    #print("model")

    get_model_text()
  })

  #######################
  # Normal distribution #
  #######################

  output$mean = renderUI(
  {
    #print("mean")
    if (input$dist == "rnorm")
    {
numericInput("mu","Mean", value=0, min=-1000, max=1000, step=1)

	# sliderInput("mu",
                  # "Mean",
                  # value = 0,
                  # min = -500,
                  # max = 500,
				  # step=0.1)
    }
  })
    
  output$sd = renderUI(
  {
    #print("sd")
    if (input$dist == "rnorm")
    {
numericInput("sd","Standard deviation", value=1, min=0.1, max=1000, step=1)
      # sliderInput("sd",
                  # "Standard deviation",
                  # value = 1,
                  # min = 0.1,
                  # max = 300,
                  # step=0.1)
    }
  })
  
  ##########################
  # t, F, X^2 distribution #
  ##########################

  output$df1 = renderUI(
  {
    #print("df1")
    if (input$dist %in% c("rt","rchisq","rf"))
    {
      sliderInput(ifelse(input$dist %in% c("rt","rchisq"), "df","df1"),
                  "Degrees of freedom",
                  value = 10,
                  min = 1,
                  max = 100)
    }
  })
  
  output$df2 = renderUI(
  {
    #print("df2")
    if (input$dist == "rf")
    {
      sliderInput("df2",
                  "Degrees of freedom (2)",
                  value = 10,
                  min = 1,
                  max = 50)
    }
  })


  #########################
  # Binomial distribution #
  #########################

  output$n = renderUI(
  {
    #print("n")
    if (input$dist == "rbinom")
    {
numericInput("n","n (number of observations)", value=10, min=1, max=1000, step=1)	
      # sliderInput("n",
                  # "n (number of observations)",
                  # value = 10,
                  # min = 1,
                  # max = 1000,
                  # step = 1)
    }
  })

  output$p = renderUI(
  {
    #print("p")
    if (input$dist == "rbinom")
    {
numericInput("p","p (probability of success)", value=0.5, min=0, max=1, step=0.01)	
      # sliderInput("p",
                  # "p (probability of success)",
                  # value = 0.5,
                  # min = 0,
                  # max = 1,
                  # step = .01)
    }
  })

  output$a = renderUI(
  {
    #print("a - lower bound")

    value = 1
    min = 0
    max = 1
    step = 1

    if (input$dist == "rnorm")
    {
      find_normal_step = function(sd)
      {
        10^round(log(7*sd/100,10))
      }

      if (is.null(input$mu) | is.null(input$sd)){
        shiny:::flushReact()
        return()
      }

      mu = input$mu
      sd = input$sd
      if (is.null(mu)) mu = 0
      if (is.null(sd)) sd = 1

      value = mu - 1.96 * sd
      min   = mu - 4 * sd
      max   = mu + 4 * sd
      step  = find_normal_step(sd)
      if (mu == 0 & sd == 1) {step = .01}
    }
    else if (input$dist == "rt")
    {
      value = -1.96 
      min   = -6
      max   = 6
      step  = 0.01
    }
    else if (input$dist == "rf")
    {
      value = round(qf(.95,as.numeric(input$df1),as.numeric(input$df2)),digits=2)
      min   = 0
      max   = round(qf(.995,as.numeric(input$df1),as.numeric(input$df2))*1.05,digits=2)
      step  = 0.01
    }
    else if (input$dist == "rchisq")
    {
      value = round(qchisq(.95,as.numeric(input$df)),digits=2)
      min   = 0
      max   = round(qchisq(.995,as.numeric(input$df)),digits=2)
      step  = 0.01
    }
    else if (input$dist == "rbinom")
    {
      if (is.null(input$n)){
        shiny:::flushReact()
        return()
      }

      value = round(input$n/4)
      min = 0
      max = input$n
      step = 1
    }
numericInput("a","a", value=value, min=min, max=max, step=step)
    # sliderInput("a", "a",
                # value = value,
                # min   = min,
                # max   = max,
                # step  = step)
  })

  output$b = renderUI(
  {
    #print("b - upper bound")
     
    if (is.null(input$tail))
    {
      shiny:::flushReact()
      return()
    }
    
    if (input$tail %in% c("middle","both"))
    {
      value = 1
      min = 0
      max = 1
      step = 1

      if (input$dist == "rnorm")
      {
        find_normal_step = function(sd)
        {
          10^round(log(7*sd/100,10))
        }

        if (is.null(input$mu) | is.null(input$sd)){
          shiny:::flushReact()
          return()
        }

        mu = input$mu
        sd = input$sd
        if (is.null(mu)) mu = 0
        if (is.null(sd)) sd = 1

        value = mu + 1.96 * sd
        min   = mu - 4 * sd
        max   = mu + 4 * sd
        step  = find_normal_step(sd)
		if (mu == 0 & sd == 1) {step = .01}
      }
      else if (input$dist == "rt")
      {
        value = 1.96 
        min   = -6
        max   = 6
        step  = 0.01
      }
      else if (input$dist == "rbinom")
      {
        if (is.null(input$n)){
          shiny:::flushReact()
          return()
        }

        value = round(input$n*3/4)
        min = 0
        max = input$n
        step = 1
      }
numericInput("b","b", value=value, min=min, max=max, step=step)
      # sliderInput("b", "b",
                  # value = value,
                  # min   = min,
                  # max   = max,
                  # step  = step)
    }
  })  

  ############
  # Plotting #
  ############
  
  output$plot = renderPlot(
  { 
    #print("plot")

    if (is.null(input$tail) | is.null(input$a))
    {
      shiny:::flushReact()
      return()
    }

    L = NULL
    U = NULL

    error = FALSE

    if (input$tail == "lower" | input$tail == "equal")
    {
      L = input$a 
    }
    else if (input$tail == "upper")
    {
      U = input$a 
    }
    else if (input$tail %in% c("both","middle"))
    {
      if (is.null(input$b)){
        shiny:::flushReact()
        return()
      }
      
      L = input$a
      U = input$b

      if (L > U)
        error = TRUE
    }

    if (error)
    {
      plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
      text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=1.5)
    }
    else
    {
      if (input$dist == "rnorm" | input$dist == "rt") 
      {
        M = NULL
        if (input$tail == "middle")
        {
          M = c(L,U)
          L = NULL
          U = NULL
        }

        if(input$dist == "rnorm")
        {
          if(is.null(input$mu) | is.null(input$sd))
          {
            shiny:::flushReact()
            return()
          }

          normTail(m=input$mu, s=input$sd, L=L, U=U, M=M, axes=3, cex.axis=1.5)
          title(main="Normal Distribution")
        }
        else if (input$dist == "rt")
        {
          if(is.null(input$df))
          {
            shiny:::flushReact()
            return()
          }

          normTail(m=0, s=1, df=input$df, L=L, U=U, M=M, axes=3, cex.axis=1.5)
          title(main="t Distribution")
        }
      }
        else if (input$dist == "rchisq")
        {
          if(is.null(input$df))
          {
            shiny:::flushReact()
            return()
          }
          M = NULL
          if (input$tail == "middle")
          {
            M = c(L,U)
            L = NULL
            U = NULL
          }
          
          chiTail(U=U, df=input$df, xlim = c(0,round(qchisq(.995,input$df),digits=2)+1))
          title(main="Chi^2 Distribution")
        }
        else if (input$dist == "rf")
        {        
          if(is.null(input$df1) | is.null(input$df2))
          {
            shiny:::flushReact()
            return()
          }
        
          M = NULL
          if (input$tail == "middle")
          {
            M = c(L,U)
            L = NULL
            U = NULL
          }
                   
          FTail(U=U,df_n=input$df1, df_d=input$df2)
          title(main="F Distribution")
        }
      else if (input$dist == "rbinom")
      {
        if(  is.null(input$n)
           | is.null(input$p)
           | is.null(input$lower_bound))
        {
          shiny:::flushReact()
          return()
        }

        if(input$tail %in% c("both","middle") & is.null(input$upper_bound))
        {
          shiny:::flushReact()
          return()
        }

        d = dbinom(0:input$n,input$n,input$p)

        plot(0,0,type='n',xlim=c(-0.5,input$n+0.5),ylim=c(0,max(d)),
             xlab="",ylab="", axes=FALSE)
        axis(1, cex.axis=1.5)
        axis(2, cex.axis=1.5)
        title(main=paste("Binomial Distribution"))

        for (k in 1:length(d)) 
        {
            col = NA

            if (input$tail == "lower")
            {
              if (input$lower_bound == "open"   & k-1 <  L) col = "#569BBD"
              if (input$lower_bound == "closed" & k-1 <= L) col = "#569BBD"
            }
            else if (input$tail == "upper")
            {
              if (input$lower_bound == "open"   & k-1 >  U) col = "#569BBD"
              if (input$lower_bound == "closed" & k-1 >= U) col = "#569BBD"
            }
            else if (input$tail == "equal")
            {
              if (k-1 == L) col = "#569BBD"
            }
            else if (input$tail == "both")
            {
              if (input$lower_bound == "open"   & input$upper_bound == "open"   & (k-1 <  L | k-1 >  U)) col = "#569BBD"
              if (input$lower_bound == "open"   & input$upper_bound == "closed" & (k-1 <  L | k-1 >= U)) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "open"   & (k-1 <= L | k-1 >  U)) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "closed" & (k-1 <= L | k-1 >= U)) col = "#569BBD"
            }
            else if (input$tail == "middle")
            {
              if (input$lower_bound == "open"   & input$upper_bound == "open"   & k-1 >  L & k-1 <  U) col = "#569BBD"
              if (input$lower_bound == "open"   & input$upper_bound == "closed" & k-1 >  L & k-1 <= U) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "open"   & k-1 >= L & k-1 <  U) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "closed" & k-1 >= L & k-1 <= U) col = "#569BBD"
            }

            p = matrix(c(-1.5+k,0, -0.5+k,0, -0.5+k,d[k], -1.5+k,d[k], -1.5+k,0),ncol=2,byrow=TRUE)
          
            polygon(p, col=col)
        }
      }
    }
  })

#######################
# Plotting simulation #
#######################
  
#
# Display slider for the sample size in simulation
#  
  output$slider <- renderUI({
    sliderInput("simSize", "Number of observations:", min=2, max=100, value=30)
   })
#
# Display checkbox if you want density curve to be added to the histogram. 
#  
   output$drawDensity <- renderUI({
       checkboxInput("dens", "Draw density curve", FALSE)
   })

  output$hist = renderPlot({

          if (input$dist == "rnorm") 
	      {
		  if (is.null(input$mu) | is.null(input$sd) | is.null(input$simSize)){
          shiny:::flushReact()
          return()
           }
		  histData <- rnorm(input$simSize, input$mu, input$sd)
          hist(histData, col = 'darkgray', border = 'white', main = "Random samples from normal distribution", xlab = "data")
		  if(input$dens ==TRUE){
		  hist(histData, prob=TRUE, col = 'darkgray', border = 'white', main = "Random samples from normal distribution", xlab = "data")
		  lines(density(histData), col="blue", lwd = 2)}
		  } 

		   if(input$dist == "rt")
		   {
		    if (is.null(input$df) | is.null(input$simSize)){
            shiny:::flushReact()
            return()
            }
		   histData <- rt(input$simSize, input$df)
           hist(histData, col = 'darkgray', border = 'white', main = "Random samples from t-distribution", xlab = "data")
		    if(input$dens ==TRUE){
		   hist(histData, prob=TRUE, col = 'darkgray', border = 'white', main = "Random samples from t-distribution", xlab = "data")
		   lines(density(histData), col="blue", lwd = 2)}
			}
		      
		   if(input$dist == "rbinom")
           {
		   if (is.null(input$n) | is.null(input$p) | is.null(input$simSize)){
            shiny:::flushReact()
            return()
           }
		   histData <- rbinom(input$simSize, input$n, input$p)
           hist(histData, col = 'darkgray', border = 'white', main = "Random samples from binomial distribution", xlab = "data")
		    if(input$dens ==TRUE){
		   hist(histData, prob=TRUE, col = 'darkgray', border = 'white', main = "Random samples from binomial distribution", xlab = "data")
		   lines(density(histData), col="blue", lwd = 2)}
			}

		   if(input$dist == "rf")
           {
		   if (is.null(input$df1) | is.null(input$df2) | is.null(input$simSize)){
            shiny:::flushReact()
            return()
           }
		   histData <- rf(input$simSize, input$df1, input$df2)
           hist(histData, col = 'darkgray', border = 'white', main = "Random samples from F-distribution", xlab = "data")
		    if(input$dens ==TRUE){
		   hist(histData, prob=TRUE, col = 'darkgray', border = 'white', main = "Random samples from F-distribution", xlab = "data")
		   lines(density(histData), col="blue", lwd = 2)}
			}

		   if(input$dist == "rchisq")
           {
		   if (is.null(input$df) | is.null(input$simSize)){
            shiny:::flushReact()
            return()
           }
		   histData <- rchisq(input$simSize, input$df)
           hist(histData, col = 'darkgray', border = 'white', main = "Random samples from chi-squared distribution", xlab = "data")
		    if(input$dens ==TRUE){
		   hist(histData, prob=TRUE, col = 'darkgray', border = 'white', main = "Random samples from chi-squared distribution", xlab = "data")
		   lines(density(histData), col="blue", lwd = 2)}
			}		
 })
   
################
# Calculations #
################

  output$area = renderText(
  {
    if (is.null(input$tail) | is.null(input$a))
    {
      shiny:::flushReact()
      return()
    }

    L = input$a
    U = NULL

    if (input$tail %in% c("both","middle")) 
    {
      if (is.null(input$b))
      {
        shiny:::flushReact()
        return()
      }

      U = input$b
      
      error = FALSE
      if (L>U) error = TRUE
      if (error){
        return()
      }
    }
    
    f = function() NULL

    if (input$dist == "rnorm")
    {
      if (is.null(input$mu) | is.null(input$sd))
      {
        shiny:::flushReact()
        return()
      }

      f = function(x) pnorm(x,input$mu,input$sd)
    }  
    else if (input$dist == "rt")
    {
      if (is.null(input$df))
      {
        shiny:::flushReact()
        return()
      }
      
      f = function(x) pt(x,input$df)
    }
    else if (input$dist == "rchisq"){
      if (is.null(input$df))
      {
        shiny:::flushReact()
        return()
      }
      
      f = function(x) pchisq(x,input$df)
    }
    else if (input$dist == "rf"){
      if (is.null(input$df1) | is.null(input$df2))
      {
        shiny:::flushReact()
        return()
      }
      
      f = function(x) pf(x,input$df1,input$df2)
    }    
    else if (input$dist == "rbinom")
    {
      if (is.null(input$n) | is.null(input$p) | is.null(input$lower_bound))
      {
        shiny:::flushReact()
        return()
      }

      if (input$tail == "equal")
      {
        f = function(x) dbinom(x,input$n,input$p)
      }
      else
      {
        f = function(x) pbinom(x,input$n,input$p)
      
        if (input$tail %in% c("lower","both") & input$lower_bound == "open") L = L-1
        if (input$tail %in% c("upper")        & input$lower_bound == "closed") L = L-1
        if (input$tail %in% c("middle")       & input$lower_bound == "closed") L = L-1

        if (input$tail %in% c("both","middle")) 
        {
          if (is.null(input$upper_bound))
          {
            shiny:::flushReact()
            return()
          }

          if (input$tail == "both"   & input$upper_bound == "closed") U = U-1
          if (input$tail == "middle" & input$upper_bound == "open") U = U-1
        } 
      }
    }

    val = NA
    if (input$tail == "lower")
      val = f(L)
    else if (input$tail == "upper")
      val = 1-f(L)
    else if (input$tail == "equal")
      val = f(L)
    else if (input$tail == "both")
      val = f(L) + (1-f(U))
    else if (input$tail == "middle")
      val = f(U) - f(L)
    
    text = paste(get_model_text(),"=",signif(val,3))
  
    
    text = sub("a",input$a,text)
    if (input$tail %in% c("both","middle")) 
      text = sub("b",input$b,text)
    
    text
  })

############################
# Calculate critical values
############################

        output$cvNorm <- renderText({
                if (is.null(input$tails))
                    {
                    shiny:::flushReact()}				 
				 if(input$tailsNorm == "bothtails") {out <- format(round(qnorm(c(0.5*input$cvProbNorm, 1-0.5*input$cvProbNorm), input$cvNormMean, input$cvNormSD), digits=4), nsmall=4)}
				 else if(input$tailsNorm == "uppertail") {out <- format(round(qnorm(1-input$cvProbNorm, input$cvNormMean, input$cvNormSD), digits=4), nsmall=4)}
				 else {out <- format(round(qnorm(input$cvProbNorm, input$cvNormMean, input$cvNormSD, lower.tail = TRUE), digits=4), nsmall=4)}
				 })

        output$cvBin <- renderText({
				 if(input$tailsBin == "bothtails") {out <- format(round(qbinom(c(0.5*input$cvProbBin, 1-0.5*input$cvProbBin), input$cvBinSize, input$cvBinProb), digits=4), nsmall=4)}
				 else if(input$tailsBin == "uppertail") {out <- format(round(qbinom(1-input$cvProbBin, input$cvBinSize, input$cvBinProb), digits=4), nsmall=4)}
				 else {out <- format(round(qbinom(input$cvProbBin, input$cvBinSize, input$cvBinProb, lower.tail = TRUE), digits=4), nsmall=4)}
				 })

        output$cvT <- renderText({
				 if(input$tailsT == "bothtails") {out <- format(round(qt(c(0.5*input$cvProbT, 1-0.5*input$cvProbT), input$cvTDf), digits=4), nsmall=4)}
				 else if(input$tailsT == "uppertail") {out <- format(round(qt(1-input$cvProbT, input$cvTDf), digits=4), nsmall=4)}
				 else {out <- format(round(qt(input$cvProbT, input$cvTDf, lower.tail = TRUE), digits=4), nsmall=4)}
				 })

        output$cvChisq <- renderText({
				 if(input$tailsChisq == "bothtails") {out <- format(round(qchisq(c(0.5*input$cvProbChisq, 1-0.5*input$cvProbChisq), input$cvChisqDf), digits=4), nsmall=4)}
				 else if(input$tailsChisq == "uppertail") {out <- format(round(qchisq(1-input$cvProbChisq, input$cvChisqDf), digits=4), nsmall=4)}
				 else {out <- format(round(qchisq(input$cvProbChisq, input$cvChisqDf, lower.tail = TRUE), digits=4), nsmall=4)}
				 })

        output$cvF <- renderText({
				 if(input$tailsF == "bothtails") {out <- format(round(qf(c(0.5*input$cvProbF, 1-0.5*input$cvProbF), input$cvFDf1, input$cvFDf2), digits=4), nsmall=4)}
				 else if(input$tailsF == "uppertail") {out <- format(round(qf(1-input$cvProbF, input$cvFDf1, input$cvFDf2), digits=4), nsmall=4)}
				 else {out <- format(round(qf(input$cvProbF, input$cvFDf1, input$cvFDf2, lower.tail = TRUE), digits=4), nsmall=4)}
				 })	 
}
shinyApp(ui, server)