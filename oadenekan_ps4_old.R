
# use debug(), traceback(), and browser()

# problem 1
myFunction<-function(door_choice, car_location){
  
  if (door_choice == car_location){ 
    x<-TRUE
  } 
  else { 
    x<-FALSE 
  }
  return(x)
  
}
myFunction(sample(1:3, 1), sample(1:3, 1))
# Should return a TRUE if these samples are equal and
# a false if they are not

# problem 2

# validity check
check_door = function(object) {
  errors = character()
  chosenDoor = object@chosenDoor
  chosenDoorValid = (chosenDoor > 0) && (chosenDoor < 4)
  if (!chosenDoorValid) {
    msg <- paste("chosenDoor is not 1, 2, or 3 ", sep = "")
    errors <- c(errors, msg)
  }
  
  carDoor = object@carDoor
  carDoorValid = (carDoor > 0) && (carDoor < 4)
  if (!carDoorValid) {
    msg <- paste("carDoor is not 1, 2, or 3", sep = "")
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) {
    return(TRUE)
  }
  else {
    return(errors)
  }
}
setClass("door", representation(chosenDoor = "integer",
                                carDoor = "integer",
                                switch = "logical"), 
         validity = check_door)

# source: http://adv-r.had.co.nz/S4.html
