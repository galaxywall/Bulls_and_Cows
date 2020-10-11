#Week 2 Pair Exercises for Tianheng Huang and Hsiang Wang

####1. Bulls & Cow functions

#Define the function bulls_and_cows() below

Bulls_and_Cows<-function(){
  #Define a function to randomly pick a vector of 4 digits 
  generate_computer_vector<-function(){
    sample(0:9,4)
  }
  
  #Define a function to ask the user to input a guess and tell the user 
  #what their guess is as well as how many guesses are left
  get_guess<-function(){
    numbers_string<-readline("Please enter four number>")
    user_choice<-as.integer(unlist(strsplit(numbers_string,"")))
    print(paste("You guessed", numbers_string))
    print(paste("You have",10-index,"guesses left."))
    return(user_choice)
  }
  
  #Define a function to calculate the number of bulls and cows
  number_bulls_and_cows<-function(user_choice,computer_choice){
    #Define a function to calculate the number of bulls
    number_bulls<-function(user_choice,computer_choice){
      sum(user_choice==computer_choice)
    } 
    
    number_cows<-function(user_choice,computer_choice){
      length(intersect(computer_choice,user_choice))-
        sum(user_choice==computer_choice)
    }
    
    #Calculate the number of bulls and cows
    bull<-number_bulls(user_choice,computer_choice)
    cow<-number_cows(user_choice,computer_choice)
    return(c(bull,cow))
    
    
  }
  
  #Define a function to print the computer's response to the guess
  do_response<-function(bull,cow,index){
    #Tell the user that they've won as well as how many guesses they've made and           
    #end the while loop if the guess is correct
    if (bull==4){
      print(paste("You've won after",index,"guesse(s)."))
      break #Break the loop if the guess is correct
      
      #Tell the user how many bulls and cows there are as well as how many 
      #guesses remain if the guess is incorrect
    }else{
     
      
      print(paste("Your guess is incorrect with",bull,"bulls and",cow,"cows after"
                  ,index,"guesses.There are",10-index,"guesses left."))
      
    }
    
    #Tell the user that the game is ended as well as the correct answer after 10           
    #incorrect guesses
    if (index==10){
      print("You have not got the right answer after 10 guesses.")
      print(paste("The game is ended. The correct answer is:",
                  paste(as.character(computer_choice),collapse = "")))
    }
  }
  
  #Randomly pick a vector of 4 digits and store it in the variable 
  #computer_choice
  computer_choice<-generate_computer_vector()
  
  
  
  #Create the variable index to keep track of the number of guesses and set it 
  #to 0
  index<-0
  
  ####Create a while loop that can iterate for no more than 10 times###
  while (index<10){
    
    #Increment the number of guesses
    index<-index+1
    
    #Get the guess and store it in the variable user-choice
    user_choice<-get_guess()
  
 
    #Use the variable bull to store the number of bulls
    bull<-number_bulls_and_cows(user_choice,computer_choice)[1]
    
    #Use the variable cow to store the number of cows
    cow<-number_bulls_and_cows(user_choice,computer_choice)[2]
    
    #Print the computer's response
    do_response(bull,cow,index)
    
  }
}