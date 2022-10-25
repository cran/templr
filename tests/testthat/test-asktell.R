library(testthat)
library(future)


# Test basic read/write
Sys.sleep(1)

x=matrix(runif(10),ncol=2)

future(evaluator=plan("multisession"),lazy=FALSE,{
    x=ask_X(id=1)
    tell_Y(x,id=1)
})
ask = ask_Y(x, id=1)
plan("sequential")

expect_equal(ask,x)


# Test basic f(X) where f may be called from another session
Sys.sleep(1)

x=matrix(runif(10),ncol=2)
f=function(X) rowSums(sin(X))
ref = f(x)

future(evaluator=plan("multisession"),lazy=FALSE,{
    x=ask_X(id=2)
    tell_Y(f(x),id=2)
    })
ask = ask_Y(x,id=2)
plan("sequential")

expect_equal(ask,ref)


# Test uniroot using future to emulate multiple R sessions
Sys.sleep(1)

f=function(x) sin(x)-0.5
r_ref=uniroot(f=f, interval = c(0,pi/2.1))

bg=future::future(evaluator=plan("multisession"),lazy=FALSE,{
    while (TRUE) {
        x=ask_X(timeout = 10,id=3)
        tell_Y(f(x),id=3)
    }
})
r=uniroot(f=function(...)ask_Y(id=3,...), interval = c(0,pi/2.1))
plan("sequential")

expect_equal(r,r_ref)


# Test optim using future to emulate multiple R sessions
Sys.sleep(1)

f=sin
o_ref=optim(par=.5, fn=f, lower=0, upper=pi, method="L-BFGS-B")

future::future(evaluator=plan("multisession"),lazy = F,{
    while(TRUE) {
        x=ask_X(timeout = 10,id=4)
        tell_Y(f(x),id=4)
    }
})
o=optim(par=.5, fn=function(...)ask_Y(id=4,...), lower=0, upper=pi, method="L-BFGS-B")
plan("sequential")

expect_equal(o,o_ref)


# Test optim+grad using future to emulate multiple R sessions
Sys.sleep(1)

f=sin
g=function(x) (f(x+0.001)-f(x))/0.001
o_ref=optim(par=.5, fn=f, gr = g, lower=0, upper=pi, method="L-BFGS-B")

future::future(evaluator=plan("multisession"),lazy=FALSE,{
    while(TRUE) {
        x=ask_X(timeout = 10,id=5)
        tell_Y(f(x),id=5)
    }
})
future::future(evaluator=plan("multisession"),lazy=FALSE,{
    while(TRUE) {
        dx=ask_dX(timeout = 10,id=6)
        tell_dY(f(dx),id=6)
    }
})
o=optim(par=.5, fn=function(...)ask_Y(id=5,...), gr=function(...)ask_dY(id=6,...), lower=0, upper=pi, method="L-BFGS-B")
plan("sequential")

expect_equal(o,o_ref)


# Test optim with constraints using future to emulate multiple R sessions
Sys.sleep(1)

if (requireNamespace("mco")) {

    f <- function(x) {
        x1 <- x[,1]*15-5   
        x2 <- x[,2]*15     
        cbind(
            (x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
            , x[,2]-x[,1]) # : constraint x2>x1
    }
    
    o_ref = mco::nsga2(fn=function(x)f(x)[,1], idim=2,odim=1,
                       constraints = function(x)f(x)[,2], cdim=1,
                       lower.bounds = rep(0,2),upper.bounds = rep(1,2),
                       generations = 5,
                       vectorized=TRUE)

    future::future(evaluator=plan("multisession"),lazy=FALSE,{
        while(TRUE) {
            x=ask_X(timeout = 10,id=7)
            tell_Y(f(x),id=7)
        }
    })
    o = mco::nsga2(fn=function(x)ask_Y(x,id=7)[,1], idim=2,odim=1,
                   constraints = function(x)ask_Y(x,id=7)[,2], cdim=1,
                   lower.bounds = rep(0,2),upper.bounds = rep(1,2),
                   generations = 5,
                   vectorized=TRUE)
    plan("sequential")
    
    expect_equal(o,o_ref)

} else message("skip mco test")