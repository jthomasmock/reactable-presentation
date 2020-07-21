library(dplyr)

data <- MASS::Cars93[18:47, ] %>%
  mutate(ID = as.character(18:47), Date = seq(as.Date("2019-01-01"), by = "day", length.out = 30)) %>%
  select(ID, Date, Manufacturer, Model, Type, Price)

sales_by_mfr <- group_by(data, Manufacturer) %>%
  summarize(Quantity = n(), Sales = sum(Price))

reactable(
  sales_by_mfr,
  details = function(index) {
    sales <- filter(data, Manufacturer == sales_by_mfr$Manufacturer[index]) %>% 
      select(-Manufacturer)
    tbl <- reactable(sales, outlined = TRUE, highlight = TRUE, fullWidth = FALSE)
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)