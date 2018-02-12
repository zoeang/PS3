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
   Gryffindor_Tower<-yourname
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
door<-structure(c(1:3), class="door")

#2. 
#take a value stored in door
picked<-sample(door,1)
#draw a random number
car<-sample(1:3,1)

#compare
if(picked==car){
  print("Congratulations! You picked the correct door")
} else {
  print("Enjoy your goat!")
}

#To create a method for door, door needs to be a generic first(?)
#create generic "door"
####What method is door assigned to?
doors<-function(x){
  UseMethod("doors")
}


#Create the method
door.PlayGame<-function(x){
  #x is a number 1, 2, or 3 chosen by the player
  car<-sample(1:3,1) #this will randomly assign a number to car
  if(x==car){ #if loop to test if the player's number is the car door
    print("Congratulations! You picked the correct door")
  } else {
    print("Enjoy your goat!")
  }
}
door.PlayGame(2)


#######################
#S4 
#This creates the class door, correct?
#setClass(Class="doors",
         #representation = representation(
          # picked="numeric",
          # car="numeric"
        # ),
        # prototype= prototype(
          # picked=sample(1:3,1),
          # car=sample(1:3,1)
        # )
#)
#new("doors")

#constructor
setClass("doors",
         contains="numeric"
        )
simdoor<-new("doors", 1)
simdoor


#validation function
setValidity("doors", function(object){
  test<-all(object[1]==1 | object[1]==2 |object[1]==3)
  if(!test==object[1]){
    print("Not a valid value")
  }
}
)

#New method
setMethod("PlayGame", "door", function(x){
  #x is a number 1, 2, or 3 chosen by the player
  car<-sample(1:3,1) #this will randomly assign a number to car
  if(x==car){ #if loop to test if the player's number is the car door
    print("Congratulations! You picked the correct door")
  } else {
    print("Enjoy your goat!")
  }
}
)
