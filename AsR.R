# Exercise 1

coin_flips = rbinom(500, 1, 0.3)
range_n = 500
generate_plot= function(sample, n, p){
  plot(cumsum(sample), n, xlab="Xn", ylab="n", main="Plot of X against n", type="l")
  # par(new=TRUE)
  lines(n*p, n, col="red", type="l")
}


get_relative_error = function(sample, n, p){
  vector_n = c(1:n)
  error_vector = abs(cumsum(sample) - vector_n*p)/vector_n*p
  plot(vector_n, error_vector, xlab="n", ylab="error", type="l")
}

# Exercise 3 - argument n = number or students chosen at random

birthday_experiments = function(n, expes){
  occurences = 0
  #sams_matrix = 
  for(i in 1:expes){
    sam = sample(1:365, n, replace=TRUE)
    num_days = 0
    for(day in sam){
      repeats = length(which(sam==day))
      if(repeats > 2){
        num_days = num_days + 1
      }
    }
    if(num_days > 0){
      occurences = occurences + 1
    }
  }
  return(occurences/expes)
}
print(birthday_experiments(100, 100))
prob = numeric(200)
for(i in 1:200){
  prob[i] = birthday_experiments(i, 100)
}
#print(prob)
plot(prob, 1:200)

estimate_n = function(n, expes){
  prob = numeric(n)
  least_n = 1
  for(i in 1:n){
    prob[i] = birthday_experiments(i, expes)
  }
  while(prob[least_n] < 0.7){
    least_n = least_n + 1
  }
  print(least_n)
}
estimate_n(200, 100)

count_days = function(){
  vec_days = numeric(100)
  for(i in 1:100){
    sam = ceiling(runif(75, 1, 365))
    num_days = 0
    for(day in sam){
      occurences = length(which(sam==day))
      if(occurences > 2){
        num_days = num_days + 1
      }
    }
    vec_days[i] = num_days
  }
  return(vec_days)
}

#hist(count_days())

averages = numeric(100)
for(i in 1:100){
  averages[i] = mean(count_days())
}
#hist(averages)