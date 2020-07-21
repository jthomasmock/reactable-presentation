library(tidyverse)
library(reactable)
library(htmltools)


# Read in the data --------------------------------------------------------

raw_pets <- read_csv("animal-complaints.csv") %>% 
  janitor::clean_names()

glimpse(raw_pets)


# Make it wider for a table -----------------------------------------------

wide_data <- raw_pets %>% 
  separate(date_received, into = c("month", "year"), sep = " ") %>% 
  mutate(electoral_division = str_extract(electoral_division, "[:digit:]+") %>% 
           as.integer(),
         month_name = factor(month, levels = month.name, labels = month.abb),
         month_num = match(month, month.name)) %>% 
  count(year, month_name, complaint_type, animal_type) %>% 
  arrange(month_name, year) %>% 
  pivot_wider(names_from = month_name, values_from = n) %>%
  filter(animal_type == "dog", between(year, 2014, 2019)) %>%
  select(-animal_type) %>% 
  arrange(complaint_type)

wide_data %>% 
  print(n = 36)


# Make pretty colors ------------------------------------------------------

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

green_pal <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

green_pal(0.1)
green_pal(0.9)

# Something more helpful --------------------------------------------------

scales::show_col("blue3")
scales::show_col("red4")

# Using our palette -------------------------------------------------------

green_pal(0.1) %>% scales::show_col()
green_pal(0.5) %>% scales::show_col()
green_pal(0.9) %>% scales::show_col()


# Not the most efficient --------------------------------------------------

seq(0.01, 0.99, 0.05)

seq(0.01, 0.99, 0.05) %>% 
  green_pal() %>% 
  scales::show_col()


# Basic table -------------------------------------------------------------

wide_data %>% 
  reactable()


# Too much data -----------------------------------------------------------

wide_sum_df <- wide_data %>% 
  select(complaint_type, everything()) %>% 
  group_by(complaint_type) %>% 
  summarize(year = paste(min(year), str_sub(max(year), -2), sep = " - "),
            across(c(Jan:Dec), mean)) %>% 
  ungroup() %>% 
  mutate(across(c(Jan:Dec), ~round(.x, digits = 1))) 

wide_sum_df

wide_sum_df %>% 
  reactable()


# More complex ------------------------------------------------------------

wide_sum_df %>% 
  reactable(
    compact = TRUE,
    defaultColDef = colDef(
      minWidth = 60,
      align = "right",
      format = colFormat(digits = 1)
    ),
    list(
      complaint_type = colDef("Complaint",
                              width = 200,
                              align = "left"
      ),
      year = colDef("Year",
                    width = 80,
                    align = "left"
      )))


# Let's add some more data back in ----------------------------------------

wide_data %>% 
  filter(complaint_type == "Attack") %>% 
  select(complaint_type, everything())

unique(wide_data$complaint_type)


# Select by position ------------------------------------------------------

unique(wide_data$complaint_type)[1]
unique(wide_data$complaint_type)[2]
unique(wide_data$complaint_type)[3]
unique(wide_data$complaint_type)[4]


more_data <- wide_data %>% 
  filter(complaint_type == wide_data$complaint_type[1]) %>% 
  select(complaint_type, everything())

more_data


# Basic more data plot ----------------------------------------------------

reactable(more_data)


# More complexity ---------------------------------------------------------

reactable(
  more_data,
  defaultColDef = colDef(
    minWidth = 60,
      style = function(value){
        if (!is.numeric(value)) return()
        tall_in_data <- more_data %>% 
          pivot_longer(cols = Jan:Dec, 
                       names_to = "months", 
                       values_to = "values") %>% 
          pull(values)
        
        normalized <- (value - min(tall_in_data)) / (max(tall_in_data) - min(tall_in_data))
        color <- green_pal(normalized)
        list(background = color)
      }),
    columns = list(
      year = colDef("Year"),
      complaint_type = colDef("",width = 250)))


# Ok let's break that down! -----------------------------------------------

tall_ex_data <- more_data %>% 
  pivot_longer(cols = Jan:Dec, 
               names_to = "months", 
               values_to = "values") %>% 
  pull(values)

tall_ex_data

normalized_ex <- (tall_ex_data - min(tall_ex_data)) / (max(tall_ex_data) - min(tall_ex_data))

normalized_ex

color_ex <- green_pal(normalized_ex)

color_ex

color_ex %>% scales::show_col()


# OK big step! ------------------------------------------------------------

# Let's put everything back together

# Remember this? ----------------------------------------------------------

wide_sum_df %>% 
  reactable(
    defaultColDef = colDef(
      minWidth = 60,
      align = "right",
      format = colFormat(digits = 1)
    ),
    list(
      complaint_type = colDef("Complaint",
                              width = 200,
                              align = "left"
      ),
      year = colDef("Year",
                    width = 80,
                    align = "left"
      )))


# And this? ---------------------------------------------------------------

reactable(
  more_data,
  defaultColDef = colDef(
    minWidth = 60,
    style = function(value){
      if (!is.numeric(value)) return()
      tall_in_data <- more_data %>% 
        pivot_longer(cols = Jan:Dec, 
                     names_to = "months", 
                     values_to = "values") %>% 
        pull(values)
      
      normalized <- (value - min(tall_in_data)) / (max(tall_in_data) - min(tall_in_data))
      color <- green_pal(normalized)
      list(background = color)
    }),
  columns = list(
    year = colDef("Year"),
    complaint_type = colDef("",width = 250)))

# Let's put them together! ------------------------------------------------

# To put them together, we'll need to use a function
# We just adjusted our code from previously

nest_table <- function(index){
  # filter to just the complaint_type of interest
  in_data <- wide_data %>% 
    filter(complaint_type == unique(wide_data$complaint_type)[index]) %>% 
    select(complaint_type, everything())
  
  htmltools::div(
    style = list("padding: 30px", "align: right"),
    reactable(
      defaultSortOrder = "desc",
      in_data,
      defaultColDef = colDef(
        minWidth = 40,
        style = function(value){
          if (!is.numeric(value)) return()
          tall_in_data <- in_data %>% 
            pivot_longer(cols = Jan:Dec, 
                         names_to = "months", 
                         values_to = "values") %>% 
            pull(values)
          
          normalized <- (value - min(tall_in_data)) / (max(tall_in_data) - min(tall_in_data))
          color <- green_pal(normalized)
          list(background = color)
        }),
      columns = list(
        year = colDef("Year"),
        complaint_type = colDef("",width = 250))))
}


# Use the function --------------------------------------------------------


wide_sum_df %>% 
  reactable(
    compact = TRUE,
    defaultSortOrder = "desc",
    defaultColDef = colDef(
      minWidth = 40,
      align = "right",
      format = colFormat(digits = 1)
    ),
    columns = list(
      complaint_type = colDef("Complaint",
                              width = 200,
                              align = "left"
      ),
      year = colDef("Year",
                    width = 80,
                    align = "left"
      )),
    details = function(index){
      nest_table(index)
     })
