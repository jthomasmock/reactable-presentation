
library(reactable)

narrow_car <- mtcars[c("cyl", "mpg", "disp")]
reactable(
  narrow_car,
  groupBy = "cyl",
  columns = list(
    mpg = colDef("MPG",
                 aggregate = "mean", format = colFormat(digits = 1)
    ),
    disp = colDef(
      "Disp", 
      aggregate = JS("function(values, rows) {
        const low = Math.min(...values)
        const high = Math.max(...values)
        return [low, high].join(' - ')
                       }"))))
