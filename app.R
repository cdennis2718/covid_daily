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


plotUsa = function(D,label) {

  usa_data = D[, 5:dim(D)[2]]
  total_cases = apply(usa_data, 2, sum)
  L = length(total_cases)

  daily_new = c(0)

  for (i in 2:L) {
    these_cases = total_cases[i] - total_cases[i - 1]
    daily_new = c(daily_new, these_cases)
  }

  weekly_rolling = filter(daily_new, rep(1/7, 7), sides=2)

  if (label == "Confirmed Cases") {
    y_label = "New Cases"
  } else {
    y_label = "Deaths"
  }
  main_string = paste("Daily ",label," in USA", sep="")
  plot(0:(L-1),daily_new, pch = 18, main = main_string,
      xlab = "Days Since 01-22-2020", ylab=y_label
  )
  text(30,max(daily_new) * 0.85, "Copyright 2020",
    col="blue")
  text(30,max(daily_new) * 0.8, "Christopher Dennis",
    col="blue")
  legend("topleft",legend=c("7 Day Average"), col = c("blue"), lty = c(1))
  lines(weekly_rolling, col="blue")
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

   weekly_rolling = filter(daily_new, rep(1/7, 7), sides=2)

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
      xlab = "Days since 01-22-2020", ylab=y_label
   )
   legend(
      "topleft",legend=c("7 Day Average"), 
      col = c("blue"), lty = c(1)
   )
  text(30,max(daily_new) * 0.85, "Copyright 2020",
    col="blue")
  text(30,max(daily_new) * 0.8, "Christopher Dennis",
    col="blue")
   lines(weekly_rolling, col="blue")

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

  weekly_rolling = filter(daily_new, rep(1/7, 7), sides=2)
  if (label == "Confirmed Cases") {
    y_label = "New Cases"
  } else {
    y_label = "Deaths"
  }
  main_string = paste("Daily ", label, " in ", State, sep="")
  plot(0:(L-1),daily_new, pch = 18, main = main_string,
      xlab = "Days Since 01-22-2020", ylab=y_label
  )

  legend("topleft",legend=c("7 Day Average"), col = c("blue"), lty = c(1))
  text(30,max(daily_new) * 0.85, "Copyright 2020",
    col="blue")
  text(30,max(daily_new) * 0.8, "Christopher Dennis",
    col="blue")
  lines(weekly_rolling, col="blue")


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

D1 = read.csv("covid_confirmed_usafacts.csv")
D2 = read.csv("covid_deaths_usafacts.csv")
names(D2)[2] = "County.Name" #tables have different keys

cleanD = function(D) {
  D = D[D$County.Name != "Statewide Unallocated",]
  D$State = as.character(D$State)
  D$County.Name = as.character(D$County.Name)
  return(D)
}

D1 = cleanD(D1)
D2 = cleanD(D2)

last_index = length(names(D1))
last_date_string = names(D1)[last_index]
ldstr_len = nchar(last_date_string)
last_date_string = substr(last_date_string,2,ldstr_len)


ui <- fluidPage(
  
  headerPanel('COVID-19: Daily'),

  sidebarPanel(
    HTML("Copyright 2020 <a href='mailto:cdennis2718@gmail.com'>Christopher Dennis</a>"),
    HTML("<hr>"),
    selectInput('state', 'Select State:', 
      c("*", unique(D1$State)), selected="*"),
    selectInput('county', 'Select County:', 
      c("*", unique(D1$County.Name)), selected="*"),
    radioButtons('cases_or_deaths', 'Data to Plot',
                 choices = c("Confirmed Cases", "Deaths"),
                 selected = "Confirmed Cases"),

    HTML("Datasets: <a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv'>
          Confirmed</a> | "),
    HTML("<a href='https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv'>
        Deaths</a>")
  ),
  mainPanel(
    p(paste("Plotting data from 1.22.2020 to ", last_date_string, sep="")),
    plotOutput('confirmed')
        )
)
server <- function(input, output, session) {

  downloadData = reactiveTimer(600000)
  observe({
    updateSelectInput(
      session, "county", "Select County:",
      choices = c("*",unique(D1$County.Name[D1$State == input$state]))
    )
  })
  
  observe({
    downloadData()
    D1 <<- read.csv(url("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"))
    D2 <<- read.csv(url("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
    names(D2)[2] <<- "County.Name" #tables have different keys

    D1 <<- cleanD(D1)
    D2 <<- cleanD(D2)

    last_index <<- length(names(D1))
    last_date_string <<- names(D1)[last_index]
    ldstr_len <<- nchar(last_date_string)
    last_date_string <<- substr(last_date_string,2,ldstr_len)
  })

  output$confirmed = renderPlot({
    doPlot(D1,D2,input$state,input$county,input$cases_or_deaths)
  }
  )
}

shinyApp(ui = ui, server = server)
