#1 Students

SortingHat<-function(name){ #takes in argument name
  courage<-sample(1:100,1) #create a value for each trait
  ambition<-sample(1:100,1)
  intelligence<-sample(1:100,1)
  effort<-sample(1:100,1)
  nameclass<-c(courage, ambition, intelligence, effort)
  class(nameclass)<-"student"#assign "student" as the class
  nameclass
}

Zoe<-SortingHat("Zoe")
Zoe
#2 Sorter
a<-as.numeric(SortingHat("Luna"))
X<-diag(nrow=4)

sort<-function(name){
  UseMethod("sort", name)
}

###generic.class
#create a method with an object of calss student and a 4x4 matrix
X<-diag(nrow=4)# this will be used as the argument
sort.student<- function(name,X=diag(nrow=4)){#set default X
  a<-sample(1:100,4) # a is a vector of the attributes
  score<-X%*%a##calculate X^ta
  if(max(scoresvec)==scoresvec[1]){# if the first value of score is largest, gryffindor
    print("GRYFFINDOR!")
  } else if(max(scoresvec)==scoresvec[2]){#if the second value of score is largest, slytherin
    print("SLYTHERIN!")
  } else if ((max(scoresvec)==scoresvec[3])){#if the third value of score is largest, ravenclaw
    print('RAVENCLAW!')
  } else if (max(scoresvec)==scoresvec[4]){#if the fourth value of score is largest, hufflepuff
    print("Hufflepuff...")
  }
}
sort.student(Zoe)

#3 
nameclass<-function(nameofstudent){ 
  class(nameofstudent)<-c("student", sort.student(name)) #assign two classes to one object
}
nameclass(Zoe)
class(Zoe)
Zoe


#4
#create environments
Gryffindor_Tower<-new.env()
Black_Lake<-new.env()
Ravenclaw_Tower<-new.env()
Basement<-new.env()
#create generic "curfew"
curfew<-function(x){
  UseMethod("curfew",x)
}

#create method for each house
#Gryffindor

#class(name1)[[2]] %in% c("Hufflepuff...")

curfew.Gryffindor<-function(yourname){
 nameclass(yourname)
  if (class(yourname)[[2]] %in% c("GRYFFINDOR!")){
   Gryffindor_Tower<-yourname #assign the name to the location if the class is of the proper house 
 } else{
   print("Intruder alert!")
 }
}
curfew.Gryffindor(Zoe)
# Slytherin
curfew.Slytherin<-function(yourname){
  nameclass(yourname)
  if (class(yourname)[[2]] %in% c("SLYTHERIN!")){
    Black_Lake<-yourname
  } else{
    print("Intruder alert!")
  }
}

#Ravenclaw
curfew.Ravenclaw<-function(yourname){
  nameclass(yourname)
  if (class(yourname)[[2]] %in% c("RAVENCLAW!")){
    Ravenclaw_Tower<-yourname
  } else{
    print("Intruder alert!")
  }
}

#HUfflepuff
curfew.Hufflepuff<-function(yourname){
  nameclass(yourname)
  if (class(yourname)[[2]] %in% c("Hufflepuff...")){
    Basement<-yourname
  } else{
    print("Intruder alert!")
  }
}

#######################################
#1. create the object door, which is a vector of 1,2, and 3, with class "door"
pick<-structure(sample(1:3,1), class="door")

#2. 
#To create a method for door, door needs to be a generic first(?)

PlayGame<-function(x){ 
  UseMethod("PlayGame",x)
}


#Create the method
PlayGame.door<-function(x){ #create method "PlayGame" for objects of class door 
  #x is a number 1, 2, or 3 chosen by the player
  car<-sample(1:3,1) #this will randomly assign a number to car
  if(x==car){ #if loop to test if the player's number is the car door
    print("Congratulations! You picked the correct door")
  } else {
    print("Enjoy your goat!")
  }
}
PlayGame.door(2)


#######################
#S4 
#This creates the class door, correct?

setClass(Class="doors",
         representation = representation(
          picked="numeric", #class of the slot
          car="numeric"
        ),
         prototype= prototype(
          picked=sample(1:3,1), #name of slot = data
          car=sample(1:3,1)
        )
)

setValidity("doors", function(x){
  test<-any(x@picked==1 | x@picked==2 |x@picked==3) #object must be 1,2, or 3
  if(test==F){
    print("Not a valid value")
  }
}
)


setMethod("initialize", "doors", function(.Object, ...){
  value=callNextMethod() #have faith 
  validObject(value)
  return(value)
})
new("doors")

new("doors", picked="m") #test the representation ; this will throw an error 
new("doors", picked=9) #test the validity; this will throw an error

#constructor

#What is the difference? I can "see it" when I run simdoor, but do not understand it conceptually 




#validation function

#New generic always needed in S4
setGeneric("PlayGame", function(x) { #set the name of the argument
             standardGeneric("PlayGame")
           })


#New method
testdoor<-new("doors", picked=3) #must specify slot
testdoor@picked==2

setMethod("PlayGame", "doors",function(x){ #the x must be the same as the genernic
            #x is a number 1, 2, or 3 chosen by the player
            car<-sample(1:3,1) #this will randomly assign a number to car
            if(x@picked==car){ #if loop to test if the player's number is the car door
              print("Congratulations! You picked the correct door  -- S4")
            } else {
              print("I got here")
              print(x@picked)
              print(car)
              print("Enjoy your goat -- S4!")
            }
          
          x@car<-car
          return(x)
}
          )

testdoor
PlayGame(testdoor)

