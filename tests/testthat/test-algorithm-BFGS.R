library(testthat)
BFGS.R = system.file("BFGS.R", package="templr")

# test read

expect_equal(read.algorithm(BFGS.R,"help"),"General-purpose Optimization")

# test parse

algo = parse.algorithm(BFGS.R)


# test run 1D

f = function(x) sin(x)

o_ref = optim(par = (pi/2-pi/3)/2,
              f,
              lower=-pi/3,upper=pi/2,
              method="L-BFGS-B",control=list(maxit=10))
o_run = run.algorithm(BFGS.R,
                       f,
                       list(x=list(min=-pi/3,max=pi/2)),
                      work_dir=tempdir())

argmin = as.numeric(gsub(".*<argmin>","",gsub("</argmin>.*","",o_run)))
expect_equal(argmin,o_ref$par)


# test run 2D

branin01 = function(x) {
    x1 <- x[1] * 15 - 5
    x2 <- x[2] * 15
    (x2 - 5/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10
}

o_ref = optim(par=c(.5,.5),
              fn=branin01,
              lower=c(0,0),upper=c(1,1),
              method="L-BFGS-B",control=list(maxit=10))

o_run  = run.algorithm(BFGS.R,
                     branin01,
                     list(x1=list(min=0,max=1),x2=list(min=0,max=1)),options=list(maxit=10),
                     work_dir=tempdir())

argmin = as.numeric(strsplit(gsub(".*<argmin>","",gsub("</argmin>.*","",o_run)),",")[[1]])
expect_equal(argmin,o_ref$par)


# test run 2D outside [0,1]^d

branin = function(x) {
    x1 <- x[1] 
    x2 <- x[2]
    (x2 - 5/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10
}

o_ref = optim(par=c(2.5,7.5),
              fn=branin,
              lower=c(-5,0),upper=c(10,15),
              method="L-BFGS-B",control=list(maxit=10))

o_run  = run.algorithm(BFGS.R,
                     branin,
                     list(x1=list(min=-5,max=10),x2=list(min=0,max=15)),
                     work_dir=tempdir())

argmin = as.numeric(strsplit(gsub(".*<argmin>","",gsub("</argmin>.*","",o_run)),",")[[1]])
expect_equal(argmin,o_ref$par)
