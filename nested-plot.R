library(reactable)
library(tidyverse)
library(plotly)
library(glue)
library(htmltools)
library(sparkline)

# nest the data
car_df <- mtcars %>%
  group_by(cyl) %>% 
  summarize(mpg = list(mpg)) %>% 
  dplyr::mutate(
    Sparkline = NA
  )



# What does this look like? -----------------------------------------------

# What does this look like?
car_df

# What about the actual data?
car_df$mpg[[1]]

length(car_df$mpg[[1]])

car_df

car_df$mpg[[2]] %>% length()
car_df$mpg[[3]] %>% length()


# Initial Table -----------------------------------------------------------

# plot go
car_df %>% 
  reactable()

# Add a sparkline

car_df %>% 
  reactable(
    # Define columns
    # hide two columns and create a sparkline from the nested MPG data
    columns = list(
      mpg = colDef(show = FALSE),
      Sparkline = colDef(cell = function(value, index){
        sparkline(car_df$mpg[[index]], chartRangeMin = 0, chartRangeMax = max(mtcars$mpg))
      })
    )
  )

# Plots -------------------------------------------------------------------


car_plot <- function(index){
  match_cyl <- car_df$cyl[index]
  plots_data <- filter(mtcars, cyl == match_cyl)
  plot_col <- palette("ggplot2")[index]
  p1 <- ggplot(plots_data, aes(x = mpg, y = disp)) +
      geom_point(data = mtcars, color = "grey", alpha = 0.5) +
      geom_point(color = plot_col) +
      theme_minimal() +
      scale_x_continuous(limits = c(min(mtcars$mpg), max(mtcars$mpg))) +
      scale_y_continuous(limits = c(min(mtcars$disp), max(mtcars$disp))) +
      labs(title = glue::glue("Plot for {match_cyl} cylinders."),
           x = "MPG", y = "Displacement")
    p1
}

# Test it out
car_plot(3)


# More reactable ----------------------------------------------------------


car_df %>% 
  reactable(
    # Define columns
    # hide two columns and create a sparkline from the nested MPG data
    columns = list(
      mpg = colDef(show = FALSE),
      Sparkline = colDef(cell = function(value, index){
        sparkline(car_df$mpg[[index]], chartRangeMin = 0, chartRangeMax = max(mtcars$mpg))
      })
    ),
    
    # Details creates a dropdown area that we can fill with a div
    details = function(index){
      car_plot(index)
    }
  )


# Ok maybe plotly? --------------------------------------------------------

car_plot_plotly <- function(index){
  match_cyl <- car_df$cyl[index]
  plots_data <- filter(mtcars, cyl == match_cyl)
  plot_col <- palette("ggplot2")[index]
 
  p1 <- ggplot(plots_data, aes(x = mpg, y = disp)) +
      geom_point(data = mtcars, color = "grey", alpha = 0.5) +
      geom_point(color = plot_col) +
      theme_minimal() +
      scale_x_continuous(limits = c(min(mtcars$mpg), max(mtcars$mpg))) +
      scale_y_continuous(limits = c(min(mtcars$disp), max(mtcars$disp))) +
      labs(title = glue::glue("Plot for {match_cyl} cylinders."),
           x = "MPG", y = "Displacement",
           caption = "@thomas_mock w/ {reactable}")
    
    plotly::ggplotly(p1)
}

car_plot_plotly(3)


# What about getting it inside reactable? ---------------------------------


car_plot_js <- function(index){
  match_cyl <- car_df$cyl[index]
  plots_data <- filter(mtcars, cyl == match_cyl)
  plot_col <- palette("ggplot2")[index]
  
  htmltools::div(style = "padding: 30px", {
    p1 <- ggplot(plots_data, aes(x = mpg, y = disp)) +
      geom_point(data = mtcars, color = "grey", alpha = 0.5) +
      geom_point(color = plot_col) +
      theme_minimal() +
      scale_x_continuous(limits = c(min(mtcars$mpg), max(mtcars$mpg))) +
      scale_y_continuous(limits = c(min(mtcars$disp), max(mtcars$disp))) +
      labs(title = glue::glue("Plot for {match_cyl} cylinders."),
           x = "MPG", y = "Displacement",
           caption = "@thomas_mock w/ {reactable}")
    
    plotly::ggplotly(p1)
  }
  )
}

car_plot_js(1) 


# What is this??? ---------------------------------------------------------

listviewer::jsonedit(car_plot_js(1))

x_ob <- car_plot_js(1)

x_ob$children[[1]]$x$data[[1]]$x
x_ob$children[[1]]$x$data[[1]]$y


# We can get the data back out!

tibble(x = x_ob$children[[1]]$x$data[[1]]$x,
       y = x_ob$children[[1]]$x$data[[1]]$y) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

# Let's try one more time! ------------------------------------------------

car_df %>% 
  reactable(
    compact = TRUE,
    # Define columns
    # hide two columns and create a sparkline from the nested MPG data
    columns = list(
      mpg = colDef(show = FALSE),
      Sparkline = colDef(cell = function(value, index){
        sparkline(car_df$mpg[[index]], chartRangeMin = 0, chartRangeMax = max(mtcars$mpg))
      })
    ),
    
    # Details creates a drop down area that we can fill with a div
    details = function(index){
      # use the car_plot_js function!
      car_plot_js(index)
    }
  )
