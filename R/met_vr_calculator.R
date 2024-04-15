met_vr_calculator <- function(var = "met", kcal = 1, weigth = 1, time = 1, bpm = 1){

  if(!is.element(var, c("met", "vr")))
    stop("Please provide a valid variable name: either 'met' or 'vr'")

  if(var == "met") {

    kcal / weigth / time

  } else {

    exp(1.162 + 0.021 * bpm) * 0.06

  }

}
