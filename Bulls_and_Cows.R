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
  get_guess<-function(computer_choice){
    print(computer_choice)
    
    j<-0
    #Give the user chances to input again if their guess either does not have
    #a length of 4 or has duplicates; end the game if there are three invalid
    #guesses in a row
    while(j<3){
      numbers_string<-readline("Please enter four number>")
      user_choice<-as.integer(unlist(strsplit(numbers_string,"")))
      if (length(user_choice)!=4||sum(duplicated(user_choice))>0){
        j<-j+1
        message("Invalid guess!")
        if(length(user_choice)!=4){
          message("The guess must have exactly four digits!")
        }
        if(sum(duplicated(user_choice))>0){
          message("All of the four digits must be different!")
        }
        if(j==3){
          break
        }
        message("Try again!")
        
      }else{
        message(paste("You guessed", numbers_string))
        message(paste("You have",10-index,"guesses left."))
        break
        
      }
    }
    if(j==3){
      message("Oops! You have entered three invalid guesses in a row!")
      message(paste("The game is ended. The correct answer is:",
                    paste(as.character(computer_choice),collapse = "")))
      helper<-1
      
    }else{
      helper<-0
    }
    return(list(user_choice,helper))
    
    
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
      helper<-1
      message(paste("You've won after",index,"guesse(s)!"))

      #Tell the user how many bulls and cows there are as well as how many 
      #guesses remain if the guess is incorrect
    }else{
      helper<-0
      
      message(paste("Your guess is incorrect with",bull,"bulls and",cow,"cows after"
                  ,index,"guesses."))
      
    }
    
    #Tell the user that the game is ended as well as the correct answer after 10           
    #incorrect guesses
    if (index==10){
      message("You have not got the right answer after 10 guesses.")
      message(paste("The game is ended. The correct answer is:",
                  paste(as.character(computer_choice),collapse = "")))
    }
    return(helper)
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
    choice<-get_guess(computer_choice)
    user_choice<-choice[[1]]
    #End the program if there are three incorrect inputs in a row
    helper<-choice[[2]]
    if(helper==1){
      break
    }
   
  
 
    #Use the variable bull to store the number of bulls
    bull<-number_bulls_and_cows(user_choice,computer_choice)[1]
    
    #Use the variable cow to store the number of cows
    cow<-number_bulls_and_cows(user_choice,computer_choice)[2]
    
    #Print the computer's response
    helper<-do_response(bull,cow,index)
    if(helper==1){
      break
    }
  }
}