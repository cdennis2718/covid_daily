# Copyright 2020 Christopher Dennis
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

options(stringsAsFactors=FALSE)

loadData = function(deployed=FALSE) {
  
  if (deployed) {
      D1 <<- read.csv(url("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"))
      D2 <<- read.csv(url("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
      D3 <<- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))
  } else {
      D1 <<- read.csv("./covid_confirmed_usafacts.csv")
      D2 <<- read.csv("./covid_deaths_usafacts.csv")
      D3 <<- read.csv("./owid-covid-data.csv")
  }

  names(D3)[2] <<- "County.Name"
  names(D2)[2] <<- "County.Name" #tables have different keys
  
  D1 <<- D1[D1$County.Name != "Statewide Unallocated",]
  D2 <<- D2[D2$County.Name != "Statewide Unallocated",]
  
  D1$County.Name = as.character(D1$County.Name)
  D2$County.Name = as.character(D2$County.Name)
  
  ### For some reason, read.csv reads a few integer columns as strings
  for (i in 5:length(D1)) {
    D1[,i] <<- as.numeric(D1[,i])
  }
  for (i in 5:length(D2)) {
    D2[,i] <<- as.numeric(D2[,i])
  } 
  last_index <<- length(names(D1))
  last_date_string <<- names(D1)[last_index]
  ldstr_len <<- nchar(last_date_string)
  last_date_string <<- substr(last_date_string,2,ldstr_len)
}

movingAverage <- function(x, n=7, centered=TRUE) {
# this function I copied from 
# http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
# had to fix edge values, should rewrite b/c seems to be off center
    if (centered) {
        before <- floor  ((n-1)/2)
        after  <- ceiling((n-1)/2)
    } else {
        before <- n-1
        after  <- 0
    }

    # Track the sum and count of number of non-NA items
    s     <- rep(0, length(x))
    count <- rep(0, length(x))
    
    # Add the centered data 
    new <- x
    # Add to count list wherever there isn't a 
    count <- count + !is.na(new)
    # Now replace NA_s with 0_s and add to total
    new[is.na(new)] <- 0
    s <- s + new
    
    # Add the data from before
    i <- 1
    while (i <= before) {
        # This is the vector with offset values to add
        new   <- c(rep(NA, i), x[1:(length(x)-i)])

        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        
        i <- i+1
    }

    # Add the data from after
    i <- 1
    while (i <= after) {
        # This is the vector with offset values to add
        new   <- c(x[(i+1):length(x)], rep(NA, i))
       
        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        
        i <- i+1
    }
     
    # Drop edges
    out = s/count
    L = length(out)
    drop = c(1:before, (L-after+1):L)
    out[drop] = NA
    return(out)
}
### above function was misbehaving
### so I 
#movingAverage = function(x) {
#  out = filter(x,rep(1/7,7),sides=2)
#  return (out)
#}

plotInternational = function(D,location, cases_or_deaths) {

  D = D[D$location == location,]

  if (cases_or_deaths == "Confirmed Cases") {
    these_data = D$new_cases_per_million
    main_string = paste("Daily New Cases in",location)
    ylab_string = "Daily new cases per million"
  } else {
    these_data = D$new_deaths_per_million
    main_string = paste("Daily New Deaths in",location)
    ylab_string = "Daily new deaths per million"
  }


  if (prod(is.na(these_data))) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("No data available for", location),cex=2)
    print("No Data for")
    print(location)
    return(NULL)
  }

  # movingAverage ignores missing data
  # e.g. test case Mexico
  weekly_rolling = movingAverage(these_data)
  
  these_dates = as.Date(D$date)
  first_date = min(these_dates) # TODO wear something nice
  
  numeric_date = as.numeric(these_dates - first_date)

  plot(numeric_date, these_data, pch=18,
        main = main_string,
        ylab = ylab_string,
        xlab = paste("Days since", first_date))
 
  lines(numeric_date, weekly_rolling, type="l", col="blue", lwd=3)
   
  legend("topleft",legend=c("7 Day Average"), col = c("blue"), lty = c(1), lwd=3)
}

plotUsa = function(D,label) {

  usa_data = D[, 5:dim(D)[2]]
  total_cases = apply(usa_data, 2, sum)
  L = length(total_cases)

  daily_new = c(0)

  for (i in 2:L) {
    these_cases = total_cases[i] - total_cases[i - 1]
    daily_new = c(daily_new, these_cases)
  }

  weekly_rolling = movingAverage(daily_new)
  print(length(weekly_rolling))
  print(length(daily_new))
  if (label == "Confirmed Cases") {
    y_label = "New Cases"
  } else {
    y_label = "Deaths"
  }
  main_string = paste("Daily ",label," in USA", sep="")
  plot(0:(L-1),daily_new, pch = 18, main = main_string,
      xlab = "Days Since 1.22.2020", ylab=y_label
  )
  legend("topleft",legend=c("7 Day Average"), col = c("blue"), lty = c(1),lwd=3)
  lines(weekly_rolling, col="blue",lwd=3)
}

plotCounty = function(D, State, County.Name, label) {
  
  total_cases = D[D$State == State & 
                  D$County.Name == County.Name,]
  total_cases = as.numeric(total_cases)
  total_cases = total_cases[5:length(total_cases)] 

  L = length(total_cases)

  daily_new = c(0)

  for (i in 2:L) {
     these_cases = total_cases[i] - total_cases[i - 1]
     daily_new = c(daily_new, these_cases)
  }

  weekly_rolling = movingAverage(daily_new)
  if (label == "Confirmed Cases") {
    y_label = "New Cases"
  } else {
    y_label = "Deaths"
  }
   main_string = paste("Daily ", label, " in ",
                       County.Name,
                       ", ",
                       State,
                       sep=""
                      )
   plot(
      0:(L-1),daily_new, pch = 18, main = main_string,
      xlab = "Days since 1.22.2020", ylab=y_label
   )
   legend(
      "topleft",legend=c("7 Day Average"), 
      col = c("blue"), lty = c(1), lwd=3
   )
   lines(weekly_rolling, col="blue",lwd=3)
}

plotState = function(D, State, label) {

  state_data = D[D$State == State, 5:dim(D)[2]]
  total_cases = apply(state_data, 2, sum)

  L = length(total_cases)

  daily_new = c(0)

  for (i in 2:L) {
    these_cases = total_cases[i] - total_cases[i - 1]
    daily_new = c(daily_new, these_cases)
  }

  weekly_rolling = movingAverage(daily_new)
  if (label == "Confirmed Cases") {
    y_label = "New Cases"
  } else {
    y_label = "Deaths"
  }
  main_string = paste("Daily ", label, " in ", State, sep="")
  plot(0:(L-1),daily_new, pch = 18, main = main_string,
      xlab = "Days Since 1.22.2020", ylab=y_label
  )

  legend("topleft",legend=c("7 Day Average"), col = c("blue"), lty = c(1), lwd=3)
  lines(weekly_rolling, col="blue",lwd=3)
}

doPlot = function(D1,D2, State, County.Name, label) {
  if (label == "Deaths") {
    D = D2
  } else {
    D = D1
  }

  if (County.Name == "*" && State == "*") {
    plotUsa(D, label)
  }
  else if (County.Name == "*") {
    plotState(D, State, label)
  }
  else {
    plotCounty(D, State, County.Name, label)
  }
# wrapper for county / state / US plotting functions
}

loadInterval = 6*60*60
lastLoadTime = Sys.time()

D1 = data.frame()
D2 = data.frame()
D3 = data.frame()
loadData()

ui <- fluidPage(
  
  headerPanel(
    'COVID-19: Daily'
  ),
  HTML("<hr>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
       Copyright 2020 <a href='mailto:cdennis2718@gmail.com'>Christopher Dennis</a><hr>"),
  tabsetPanel(

    tabPanel(
      title="US State/County",      
      sidebarLayout(
        sidebarPanel(
          selectInput('state', 'Select State:', 
            c("*", unique(D1$State)), selected="*"),
          selectInput('county', 'Select County:', 
            c("*", unique(D1$County.Name)), selected="*"),
          radioButtons('county_cases_or_deaths', 'Data to Plot:',
                       choices = c("Confirmed Cases", "Deaths"),
                       selected = "Confirmed Cases"),

          HTML("Source Data: <a href='https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'>
                Confirmed</a> | "),
          HTML("<a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv'>
              Deaths</a>")
        ),
        mainPanel(

          plotOutput('confirmed_county'),
          HTML(paste("Plotted data runs through", last_date_string)),
          HTML(paste("<br>Last data pull was", lastLoadTime), 'GMT')
        )
      )
    ),

    tabPanel(title="International",
      sidebarLayout(
        sidebarPanel(
          selectInput('location', 'Select Location:', 
            c(unique(D3$location)), selected="World"),

          radioButtons('international_cases_or_deaths', 'Data to Plot:',
                       choices = c("Confirmed Cases", "Deaths"),
                       selected = "Confirmed Cases"),

          HTML("Source Data: <a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv'>
                Confirmed</a>")
        ),
        mainPanel(

          plotOutput('international'),
          HTML(paste("Plotted data runs through", last_date_string)),
          HTML(paste("<br>Last data pull was", lastLoadTime), 'GMT')
        )
      )
    ),
    tabPanel(title="About",
      HTML("
        <div style='margin:5% 8% 5% 8%; padding: 2%; width: 80%; background-color:#EEE'><b>COVID-19: Daily</b> is a simple data visualization app that lets you 
        plot COVID-19 data from different locations.
        <p>
        <br>
        The United States county-level data come from 
        <a href='https://www.usafacts.com'>www.usafacts.com</a>
        (<a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv'>covid_confirmed_usafacts.csv</a>, 
        <a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv'>covid_deaths_usafacts.csv</a>),
        and the international data come from 
        <a href='https://www.ourworldindata.org'>
          www.ourworldindata.org
        </a>
         (<a href='https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'>owid-covid-data.csv</a>).
        <p>This project is written in <a href='https://www.r-project.org'>R</a> using the
        <a href='https://shiny.rstudio.com/'>Shiny</a> framework. 
        
        The code is on <a href='https://github.com/cdennis2718/covid_daily'>GitHub</a>.
        <p>If you have suggestions, comments, or questions, please email me at <a href='mailto:cdennis2718@gmail.com'>cdennis2718@gmail.com</a>.
        
        </div>"
      )
    )
  )
)


server <- function(input, output, session) {
  
  observe({
    # every second this observer checks to see if an interval 
    # has passed since last data download;
    # if so, it downloads the data
    # data is a few megabytes
    # unfortunately on shinyapps the app goes to sleep
    # so data will stil be downloaded on page load
    # however this prevents it from being downloaded EVERY reload

    invalidateLater(10000)
    thisTime = Sys.time()
    if (thisTime - lastLoadTime > 21600) {
      lastLoadTime <<- thisTime
      loadData()
    }
  }) 
  
  observe({
    updateSelectInput(
      session, "county", "Select County:",
      choices = c("*",unique(D1$County.Name[D1$State == input$state]))
    )
  })

  output$confirmed_county = renderPlot({

    doPlot(D1,D2,input$state,input$county,input$county_cases_or_deaths)
  })

  output$international = renderPlot({
    plotInternational(D3, input$location, input$international_cases_or_deaths)
  })
}

shinyApp(ui = ui, server = server)
