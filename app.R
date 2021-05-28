# Copyright 2020, 2021 Christopher Dennis
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
library("dplyr")


renderMilestones = function(ylim) {
  abline(v=332, col="forestgreen")
  text(x=332 - 10, y=ylim[2] * 0.35, 
       labels="first public US vacc.", 
       srt=90, col="forestgreen", cex=1.3)
  abline(v=483, col="forestgreen")
  text(x=483 - 10, y=ylim[2] * 0.6, 
       labels="CDC relaxes mask recomm.", 
       srt=90, col="forestgreen", cex=1.3)
}

loadData = function(deployed=TRUE) {
  # deployed = TRUE downloads data on first run
  # after that, it downloads data on page load after so many hours 
  
  if (deployed) {
      D1 <<- read.csv(url("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"))
      D2 <<- read.csv(url("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
      D3 <<- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))
  } else {
      D1 <<- read.csv("./covid_confirmed_usafacts.csv")
      D2 <<- read.csv("./covid_deaths_usafacts.csv")
      D3 <<- read.csv("./owid-covid-data.csv")
  }

  names(D1)[2] <<- "County.Name"
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
  D3$date = as.Date(D3$date)
   EU_locations = c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark", 
          "Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
          "Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands", 
          "Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden")

   #unpack relevant EU data
   EU_cases = D3[D3$location %in% EU_locations,
                      c("location", "date", "new_cases")]
   EU_deaths = D3[D3$location %in% EU_locations,
                      c("location", "date", "new_deaths")]

   EU_pop = D3[D3$location %in% EU_locations,
               c("location", "population")]


   locs = c()
   pops = c()
   for (loc in unique(EU_pop$location)) {
      pop = EU_pop$population[EU_pop$location == loc][1]
      locs = c(locs, loc)
      pops = c(pops, pop)
   }
   EU_pop = data.frame
   total_EU_pop = sum(pops)
   # re-factorize
   EU_cases$date = as.Date(EU_cases$date)
   EU_deaths$date = as.Date(EU_deaths$date)
   EU_cases$location = factor(as.character(EU_cases$location))
   EU_deaths$location = factor(as.character(EU_deaths$location))

   #Belgium = EU_cases[EU_cases$location=="Belgium",]
   #France = EU_cases[EU_cases$location=="France",]

   ## split and aggregate

   these_dates = as.Date(unique(D3$date))
   joined = data.frame(date=these_dates)
   for (loc in split(EU_cases, EU_cases$location)) {
      loc = loc[c("date","new_cases")]
      joined = full_join(joined, loc, by="date",copy=TRUE)
   }
   for (colnum in 2:length(joined)) {
      these = is.na(joined[ , colnum])
      joined[these,colnum] = 0
   }

   EU_new_cases = apply(joined[2:length(joined)], 1, sum)
   EU_new_cases_per_million = EU_new_cases / total_EU_pop * 1E6
   EU_new_cases_per_million[EU_new_cases_per_million < 0] = 0
   ### do it again for deaths

   joined = data.frame(date=these_dates)
   for (loc in split(EU_deaths, EU_deaths$location)) {
      loc = loc[c("date","new_deaths")]
      joined = full_join(joined, loc, by="date",copy=TRUE)
   }
   for (colnum in 2:length(joined)) {
      these = is.na(joined[ , colnum])
      joined[these,colnum] = 0
   }

   EU_new_deaths = apply(joined[2:length(joined)], 1, sum)
   EU_new_deaths_per_million = EU_new_deaths / total_EU_pop * 1E6
   EU_new_deaths_per_million[EU_new_deaths_per_million < 0] = 0
   # create EU data frame to join to D3
   dummy_frame = data.frame(matrix(NA, ncol=ncol(D3), nrow=length(EU_new_cases)))
   names(dummy_frame) = names(D3)
   dummy_frame$iso_code = "EU"
   dummy_frame$location = "European Union"
   dummy_frame$continent = "Europe"
   dummy_frame$date = as.Date(these_dates)
   dummy_frame$new_cases = EU_new_cases
   dummy_frame$new_deaths = EU_new_deaths
   dummy_frame$new_cases_per_million = EU_new_cases_per_million
   dummy_frame$new_deaths_per_million = EU_new_deaths_per_million
   dummy_frame$population = total_EU_pop

   D3 <<- rbind(D3, dummy_frame)

}

movingAverage <- function(x, n=14) {
  
  l = length(x)
  out = rep(NA, l)
  
  span = n/2
  num_back = ceiling(span)
  num_forward = floor(span)
  first = num_back
  last = l - num_forward

  for (this in first:last) {
    these = x[(this-num_back+1) : (this+num_forward)]
    out[this] = mean(these) 
  }

  out
}


plotInternational = function(D,location,
                             cases_or_deaths) {

  D1 = D[D$location == location,]
  D = D1[order(D1$date),]
  if (cases_or_deaths == "Confirmed Cases") {
    these_data = D$new_cases_per_million
    main_string = paste("Daily New Cases in",location)
    ylab_string = "Daily new cases per million"
  } else {
    these_data = D$new_deaths_per_million
    main_string = paste("Daily New Deaths in",location)
    ylab_string = "Daily new deaths per million"
  }
  these_data = sapply(these_data, function(x) max(x,0))

  if (prod(is.na(these_data))) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("No data available for", location),cex=2)
    print("No Data for")
    print(location)
    return(NULL)
  }

  # movingAverage ignores missing data
  # e.g. test case Mexico
  
  these_dates = as.Date(D$date)
  weekly_rolling = movingAverage(these_data)
  first_date = min(these_dates) 
  
  numeric_date = as.numeric(these_dates - first_date)

  plot(numeric_date, these_data, pch=18,
        main = main_string,
        ylab = ylab_string,
        xlab = paste("Days since", first_date),
        col = "purple")
 
  lines(numeric_date, weekly_rolling, type="l", col="blue", lwd=3) 
  legend("topleft",legend=c("14 Day Average"), col = c("blue"), lty = c(1), lwd=3)
}

plotMultinational = function(D,location1, location2, cases_or_deaths) {

   D1 = D[D$location == location1,]
   D2 = D[D$location == location2,]
   D1 = D1[order(D1$date),]
   D2 = D2[order(D2$date),]

   if (cases_or_deaths == "Confirmed Cases") {
      loc1_series = D1$new_cases_per_million
      loc2_series = D2$new_cases_per_million
      main_string = paste("Daily New Cases in",location1,"and",location2)
      ylab_string = "Daily new cases per million"
   } else {
      loc1_series = D1$new_deaths_per_million
      loc2_series = D2$new_deaths_per_million
      main_string = paste("Daily New Deaths in",location1,"and",location2)
      ylab_string = "Daily new deaths per million"
   }
   loc1_series = sapply(loc1_series, function(x) max(x,0))
   loc2_series = sapply(loc2_series, function(x) max(x,0))
   if (prod(is.na(loc1_series))) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No data available for", location1),cex=1.5)
      print("No Data for")
      print(location1)
      return(NULL)
   }
   if (prod(is.na(loc2_series))) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No data available for", location2),cex=1.5)
      print("No Data for")
      print(location2)
      return(NULL)
   }
 
   # movingAverage ignores missing data
   # e.g. test case Mexico
   weekly_rolling1 = movingAverage(loc1_series)
   weekly_rolling2 = movingAverage(loc2_series)

   loc1_dates = as.Date(D1$date)
   loc2_dates = as.Date(D2$date)
   all_dates = sort(unique(c(loc1_dates,loc2_dates)))

   first_date1 = min(loc1_dates)
   first_date2 = min(loc2_dates)
   last_date1 = max(loc1_dates)
   last_date2 = max(loc2_dates)
   first_date = min(first_date1, first_date2)
   last_date = max(last_date1,last_date2)
   
   numeric_dates1 = as.numeric(loc1_dates - first_date)
   numeric_dates2 = as.numeric(loc2_dates - first_date)
   xlim = c(0, as.numeric(last_date) - as.numeric(first_date) + 1)
   ylim = c(0, 1.2 * max(weekly_rolling1[!is.na(weekly_rolling1)],
                         weekly_rolling2[!is.na(weekly_rolling2)]))

   plot(numeric_dates1, weekly_rolling1, 
         type="l", 
         lwd=3,
         col="blue",
         main = main_string,
         xlim = xlim,
         ylim = ylim,
         ylab = ylab_string,
         xlab = paste("Days since", first_date1)
         )

   lines(numeric_dates2, weekly_rolling2, lwd=3,col="red")

   legend("topleft",legend=c(location1, location2), col = c("blue","red"), lty = c(1), lwd=3)

}

plotUsa = function(D,label) {

  usa_data = D[, 5:dim(D)[2]]
  total_cases = apply(usa_data, 2, sum)
  L = length(total_cases)

  daily_new = c(0)

  for (i in 2:L) {
    these_cases = max(total_cases[i] - total_cases[i - 1], 0)
    daily_new = c(daily_new, these_cases)
  }
  
  weekly_rolling = movingAverage(daily_new)
  if (label == "Confirmed Cases") {
    y_label = "New Cases"
  } else {
    y_label = "Deaths"
  }
  ylim = c(0, 1.2 * max(weekly_rolling[!is.na(weekly_rolling)]))
  main_string = paste("Daily ",label," in USA", sep="")
  plot(1:L,daily_new, pch = 18, main = main_string,
      xlab = "Days Since 1.21.2020", ylab=y_label,
      ylim = ylim, col="purple"
  )
  legend("topleft",legend=c("14 Day Average"), col = c("blue"), lty = c(1),lwd=3)
  lines(weekly_rolling, col="blue",lwd=3)
  
  renderMilestones(ylim)
}

plotCounty = function(D, State, County.Name, label) {
  
  total_cases = D[D$State == State & 
                  D$County.Name == County.Name,]
  total_cases = as.numeric(total_cases)
  total_cases = total_cases[5:length(total_cases)] 

  L = length(total_cases)

  daily_new = c(0)

  for (i in 2:L) {
     these_cases = max(total_cases[i] - total_cases[i - 1], 0)
     daily_new = c(daily_new, these_cases)
  }

  weekly_rolling = movingAverage(daily_new)
  print(weekly_rolling)
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
  ylim = c(0, 1.2 * max(weekly_rolling[!is.na(weekly_rolling)]))
  plot( 
    1:L, daily_new, 
    pch = 18, main = main_string,
    xlab = "Days since 1.21.2020", ylab=y_label, ylim=ylim,
    col="purple"
  )
  legend(
    "topleft",legend=c("14 Day Average"), 
    col = c("blue"), lty = c(1), lwd=3
  )

  lines(weekly_rolling, col="blue",lwd=3)
  
  renderMilestones(ylim)
}

plotState = function(D, State, label) {

  state_data = D[D$State == State, 5:dim(D)[2]]
  total_cases = apply(state_data, 2, sum)

  L = length(total_cases)

  daily_new = c(0)

  for (i in 2:L) {
    these_cases = max(total_cases[i] - total_cases[i - 1], 0)
    daily_new = c(daily_new, these_cases)
  }

  weekly_rolling = movingAverage(daily_new)
  if (label == "Confirmed Cases") {
    y_label = "New Cases"
  } else {
    y_label = "Deaths"
  }

  main_string = paste("Daily ", label, " in ", State, sep="")
  ylim = c(0, 1.2 * max(weekly_rolling[!is.na(weekly_rolling)]))
  plot(1:L, daily_new, pch = 18, main = main_string,
      xlab = "Days Since 1.21.2020", ylab=y_label, ylim=ylim,
      col="purple"
  )

  legend("topleft",legend=c("14 Day Average"), col = c("blue"), lty = c(1), lwd=3)
  lines(weekly_rolling, col="blue",lwd=3)
  
  renderMilestones(ylim)
  
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
  
}

loadInterval = 4*60*60
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
       Copyright 2020, 2021 <a href='mailto:cdennis2718@gmail.com'>Christopher Dennis</a><hr>"),
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

          HTML("Source Data: <a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv'>
                Confirmed</a> | "),
          HTML("<a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv'>
              Deaths</a>")
        ),
        mainPanel(

          plotOutput('confirmed_county'),
          HTML(paste("<br>Data are current as of", lastLoadTime), 'GMT')
        )
      )
    ),

    tabPanel(title="International",
      sidebarLayout(
        sidebarPanel(
          selectInput('location', 'Select Location:', 
            c(sort(unique(D3$location))), selected="European Union"),

          radioButtons('international_cases_or_deaths', 'Data to Plot:',
                       choices = c("Confirmed Cases", "Deaths"),
                       selected = "Confirmed Cases"),

          HTML("Source Data: <a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv'>
                All</a>")
        ),
        mainPanel(

          plotOutput('international'),
          HTML(paste("<br>Data are current as of", lastLoadTime), 'GMT')
        )
      )
    ),
    tabPanel(title="Country Comparison",
      sidebarLayout(
        sidebarPanel(
          selectInput('comparison_loc1', 'Select Location 1:', 
            c(sort(unique(D3$location))), selected="United States"),
          selectInput('comparison_loc2', 'Select Location 2:', 
            c(sort(unique(D3$location))), selected="European Union"),
          
          radioButtons('comparison_cases_or_deaths', 'Data to Plot:',
                       choices = c("Confirmed Cases", "Deaths"),
                       selected = "Confirmed Cases"),

          HTML("Source Data: <a href='https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'>
                All</a>")
        ),
        mainPanel(

          plotOutput('comparison'),
          #HTML(paste("Plotted data runs through", last_date_string)),
          HTML(paste("<br>Data are current as of", lastLoadTime), 'GMT')
        )
      )
    ), 
    tabPanel(title="About",
      HTML("
        <div style='margin:5% 8% 5% 8%; padding: 2%; width: 80%; background-color:#EEE'>

          <p><b>COVID-19: Daily</b> is a simple data visualization app that lets you 
          plot COVID-19 data from different locations.
          <p>
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
    # every minute this observer checks to see if an interval 
    # has passed since last data download;
    # if so, it downloads the data
    # data is a few megabytes
    # unfortunately on shinyapps the app goes to sleep
    # so data will stil be downloaded on page load
    # however this prevents it from being downloaded EVERY reload

    invalidateLater(60000)
    thisTime = Sys.time()

    if (difftime(thisTime, lastLoadTime,units="sec") > loadInterval) {
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

  output$comparison = renderPlot({
    plotMultinational(D3, input$comparison_loc1, input$comparison_loc2, input$comparison_cases_or_deaths)
  })

}

shinyApp(ui = ui, server = server)
