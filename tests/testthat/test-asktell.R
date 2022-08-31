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
