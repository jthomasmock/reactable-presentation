library(reactable)

# Just a few columns ------------------------------------------------------

narrow_car <- mtcars[c("cyl", "mpg", "disp")]

# Raw javascript ----------------------------------------------------------

js4shiny::repl_js()

testfun1 = function(values, rows) {
  
  const low = Math.min(...values)
  const high = Math.max(...values)
  return [low, high].join(' - ')
}

testfun2 = function (values, rows) {
  let low = values[0]
  let high = values[0]
  for (let v of values) {
    if (v < low) low = v
    if (v > high) high = v
  }
  return [low, high].join(' - ')
}

//values = [15,10,7,6,9]

//testfun(values)
//testfun2(values)

//console.log(testfun(values), testfun2(values))


# Javascript as functions -------------------------------------------------


# But wait that's javascript?
# Thank goodness for functions!

loop_fun <- JS("function (values, rows) {
      let low = values[0]
      let high = values[0]
      for (let v of values) {
       if (v < low) low = v
       if (v > high) high = v
     }
     return [low, high].join(' - ')
   }")

agg_fun <- JS("function(values, rows) {

        const low = Math.min(...values)
        const high = Math.max(...values)
        return [low, high].join(' - ')
                       }")

# Call the javascript -----------------------------------------------------

reactable(
  narrow_car,
  groupBy = "cyl",
  columns = list(
    mpg = colDef("MPG",
      aggregate = loop_fun
    ),
    disp = colDef(
      "Disp", 
      aggregate = agg_fun)))

