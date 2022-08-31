library(testthat)
Brent.R = system.file("Brent.R", package="templr")

# test read

expect_equal(substr(read.algorithm(Brent.R,"help"),0,5),"Brent")

# test parse

algo = parse.algorithm(Brent.R)


# test run

f = function(x) sin(x)-0.7

run = run.algorithm(Brent.R,f,list(x=list(min=0,max=pi/2)),
                    work_dir=tempdir())

root = as.numeric(gsub(".*<root>","",gsub("</root>.*","",run)))
expect_equal(f(root),0, tolerance = as.numeric(algo$options$ytol))
