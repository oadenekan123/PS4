########################################################
#Let's edit the final function
eval<-function(chosenDoor, carDoor,x){#This function evaluates the choices vs. the random draw
chosenDoor<-sample(1:3,1)# we assign both values random numbers 1-3  
carDoor<-sample(1:3,1)
if(choice==draw){return(T)} else{return(F)}
return(x)
}
eval()
########################################################
#(1)
chosenDoor<-sample(1:3,1)
carDoor<-sample(1:3,1)
switch<-c(TRUE,FALSE)
setClass(Class="door", #Here we define our class and slots
         representation=representation(
           chosenDoor="numeric",#Stating the type of object which can exist in the class(a number)
           carDoor="numeric",
           switch="logical"
         ),
         prototype=prototype(
           chosenDoor=c(), #Proclaiming the form of the object (how many; list or matrix, etc)
           carDoor=c(),
           switch=c()
         )
)

setValidity("door", function(object){
  test1<-(is.numeric(chosenDoor))
  test2<-(is.numeric(carDoor))
  test3<-(is.logical(switch))
if(!test1 || !test2 || !test3){return("Not a valid input")}
  })
is.integer(sample(1:3,1))

game1<-new("door", chosenDoor=1L, carDoor=2L, switch=T)
chosenDoor<-sample(1:3,1)
carDoor<-sample(1:3,1)
switch<-c(TRUE,FALSE)

setMethod("initialize", "door", function(.Object,...){
  value=callNextMethod()#Preliminary method calling a metho
  validObject(value)#We tell it to cycle through the next method for command
  return(value)
})


setGeneric("PlayGame", #Here we do set up a generic for our game
           function(object="door"){
           standardGeneric("PlayGame")
           })




setMethod("PlayGame", "door",#Here we give the generic an actual argument to run
          function(object){#same as function in S3
            carDoor<-object@carDoor#Maybe these are slots..?
            chosenDoor<-object@chosenDoor
            switch <- object@switch
            draw1<-as.integer(sample(1:3,1))
            carDoor<-as.integer(sample(1:3,1))
            
            condition<-c(draw1,carDoor)#This is the condition which will later say delete this door if it is first draw or the same as carDoor
            options<-c(1:3) #This will be part of the conditions for draw 2
            condition
            if(isTRUE(switch)==F){chosenDoor<-draw1}#Here we say if we don't switch than put decision in chosen door
            else{deletedoor<-sample(subset(options, !(options %in% condition)), 1) #Here we create a variable made up of
            cannotchoose<-c(draw1, deletedoor)# a list of the doors which are not to be drawn from
            possibledoor<-options[!options %in% cannotchoose]#Here we set up the options for candidates who chose to switch
            chosenDoor<-sample(possibledoor,1)}#Here we put whatever happens (random sample of the options left) in chosenDoor
            if(chosenDoor==carDoor){winner<-TRUE}#Here we see if the player wins 
            else {winner<-FALSE}
            print(winner)
            
            })
game1 <-new("door", chosenDoor=1L, carDoor=2L, switch=TRUE)

            
   
            
           
PlayGame(game1)

