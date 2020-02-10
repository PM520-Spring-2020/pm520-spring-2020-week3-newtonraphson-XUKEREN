my solution to inclass exercise
================
Keren Xu
2/9/2020

``` r
# a function we will work with
F1<-function(x){
  return(c(x^2,2*x)) 
  # note that the function returns two numbers. 
  # The first is f(x); 
  # the second is the derivative, f'(x)
}

#define a function F2(x)=sin(x)
F2<-function(x){
  return(c(sin(x), cos(x))) 
  # note that the function returns two numbers. 
  # The first is f(x); 
  # the second is the derivative, f'(x)
}
#define F3(x)=(x-2)^3-6*x
F3<-function(x){
  return(c((x-2)^3-6*x, 3*(x-2)^2-6)) 
  # note that the function returns two numbers. 
  # The first is f(x); 
  # the second is the derivative, f'(x)
}
#define F4(x)=cos(x)-x### 
# (All functions need to return f(x) and f’(x))
F4<-function(x){
  return(c(cos(x)-x, -sin(x)-1)) 
  # note that the function returns two numbers. 
  # The first is f(x); 
  # the second is the derivative, f'(x)
}

library(shape)


# Define your Newton-Raphson function  
NewtonRaphson<-function(func, StartingValue, Tolerance = 1*10^-9, MaxNumberOfIterations = 100){
  #initialize a variable, Deviation (say), to record |f(x)| so that you know how far away you are from 0. 
  #(So initialize it to some arbitrary large number)
  #Set up a counter, i, to record how many iterations you have performed. Set it equal to 0 
  # Initialize the values of x and f(x)
  i <- 0
  x <- StartingValue
  fx <- func(x)
  Deviation <- abs(fx[1])
  
  #Set up a while loop until we hit the required target accuracy or the max. number of steps
  while ((i < MaxNumberOfIterations) && (Deviation > Tolerance))
  {
    # Record the value of f(x) and f’(x), for the current x value. 

    # To be safe, check that the function and it's derivative are defined at X (either could be NaN if you are unlucky)
    if ((fx[1] == "NaN") || (fx[2] == "NaN")) {
      cat("\nFunction or derivative not defined error.\n")
      break
    }
    
    #Find the next X-value using Newton-Raphson's formula. Let's call that value X
    x <- x - fx[1]/fx[2]
    # calculate Deviation<- |f(x)-0|
    fx <- func(x)  
    Deviation <- abs(fx[1])
    # increase the value of your iteration counter
    i<-i+1
    
    # if you like, have the program write out how it is getting on
    cat(paste("\nIteration ",i,":   X=",x,"  Y=",fx[1]))
    
    # If you are feeling fancy, add some line segments to the screen to show where it just went
    # See the 'fixed points' code for a reminder of how to do that.

        
  }
  
  # output the result
  if (Deviation<Tolerance){
    cat(paste("\nFound the root point: ", x, " after ", i, "iterations"))
  }else{ 
    cat(paste("\nConvergence failure. Deviation: ",Deviation, "after ", i,  "iterations"))}    
  
  # have the function return the answer
  return(x)
}

NewtonRaphson(F1,10,1e-3,40)
```

    ## 
    ## Iteration  1 :   X= 5   Y= 25
    ## Iteration  2 :   X= 2.5   Y= 6.25
    ## Iteration  3 :   X= 1.25   Y= 1.5625
    ## Iteration  4 :   X= 0.625   Y= 0.390625
    ## Iteration  5 :   X= 0.3125   Y= 0.09765625
    ## Iteration  6 :   X= 0.15625   Y= 0.0244140625
    ## Iteration  7 :   X= 0.078125   Y= 0.006103515625
    ## Iteration  8 :   X= 0.0390625   Y= 0.00152587890625
    ## Iteration  9 :   X= 0.01953125   Y= 0.0003814697265625
    ## Found the root point:  0.01953125  after  9 iterations

    ## [1] 0.01953

``` r
NewtonRaphson(F2,10,1e-3,40)
```

    ## 
    ## Iteration  1 :   X= 9.35163917254091   Y= 0.0730735989952735
    ## Iteration  2 :   X= 9.42490865376483   Y= -0.000130692995079232
    ## Found the root point:  9.42490865376483  after  2 iterations

    ## [1] 9.425

``` r
NewtonRaphson(F3,10,1e-3,40)
```

    ## 
    ## Iteration  1 :   X= 7.56989247311828   Y= 127.379330322233
    ## Iteration  2 :   X= 6.10695791925136   Y= 32.6307361646652
    ## Iteration  3 :   X= 5.3753485533609   Y= 6.20318000683711
    ## Iteration  4 :   X= 5.15521318197001   Y= 0.48003626931828
    ## Iteration  5 :   X= 5.13509946189492   Y= 0.00382129831852751
    ## Iteration  6 :   X= 5.1349367603068   Y= 2.48970923877323e-07
    ## Found the root point:  5.1349367603068  after  6 iterations

    ## [1] 5.135

``` r
NewtonRaphson(F4,10,1e-3,40)
```

    ## 
    ## Iteration  1 :   X= -13.7709942015466   Y= 14.129038734246
    ## Iteration  2 :   X= 199.351177515304   Y= -199.490677048078
    ## Iteration  3 :   X= -20202.9263428707   Y= 20202.1339120406
    ## Iteration  4 :   X= 31592.3417716633   Y= -31591.4575853959
    ## Iteration  5 :   X= 10059.5801158358   Y= -10058.6001365764
    ## Iteration  6 :   X= 1671.11920265431   Y= -1670.14077517026
    ## Iteration  7 :   X= -433.898315132253   Y= 434.83472916073
    ## Iteration  8 :   X= 236.002746755287   Y= -236.930183145637
    ## Iteration  9 :   X= -142.468449187508   Y= 142.011985535541
    ## Iteration  10 :   X= -67.3195764947609   Y= 67.096796157757
    ## Iteration  11 :   X= -33.3442558989409   Y= 32.9942915588176
    ## Iteration  12 :   X= 488.412047959535   Y= -488.517461424786
    ## Iteration  13 :   X= -87192.766475474   Y= 87193.3034347951
    ## Iteration  14 :   X= 470338.177317152   Y= -470338.786021136
    ## Iteration  15 :   X= -1806200.56611734   Y= 1806200.55551752
    ## Iteration  16 :   X= -903074.919714458   Y= 903075.44197124
    ## Iteration  17 :   X= -415660.652885552   Y= 415659.706687214
    ## Iteration  18 :   X= 198845.361552023   Y= -198845.187778095
    ## Iteration  19 :   X= 98660.6410352068   Y= -98661.1156669039
    ## Iteration  20 :   X= 46186.4738861786   Y= -46186.131468033
    ## Iteration  21 :   X= -717822.574692017   Y= 717823.572293582
    ## Iteration  22 :   X= 53382.1653438673   Y= -53381.190099763
    ## Iteration  23 :   X= 9667.61847800938   Y= -9668.20997438169
    ## Iteration  24 :   X= -40247.6747261115   Y= 40246.9303798498
    ## Iteration  25 :   X= -16115.8372800678   Y= 16116.6985499669
    ## Iteration  26 :   X= -5429.41860157464   Y= 5430.15267429256
    ## Iteration  27 :   X= 11490.6804043297   Y= -11490.3798499786
    ## Iteration  28 :   X= -237028.894886287   Y= 237028.467534195
    ## Iteration  29 :   X= 2234212.86467304   Y= -2234211.87364219
    ## Iteration  30 :   X= 263371.102779561   Y= -263370.717920755
    ## Iteration  31 :   X= -3155938.07966242   Y= 3155938.69023232
    ## Iteration  32 :   X= 12014099.7659542   Y= -12014100.4251462
    ## Iteration  33 :   X= 5156637.15915107   Y= -5156637.42974626
    ## Iteration  34 :   X= 2529309.88924434   Y= -2529309.41568488
    ## Iteration  35 :   X= 1184477.52134564   Y= -1184478.47714492
    ## Iteration  36 :   X= 269129.590151753   Y= -269129.926553189
    ## Iteration  37 :   X= 130525.615646341   Y= -130525.325029615
    ## Iteration  38 :   X= -2893662.42265049   Y= 2893661.98668846
    ## Iteration  39 :   X= -1370654.41869339   Y= 1370653.52483562
    ## Iteration  40 :   X= 1113991.84484455   Y= -1113992.20519281
    ## Convergence failure. Deviation:  1113992.20519281 after  40 iterations

    ## [1] 1113992
