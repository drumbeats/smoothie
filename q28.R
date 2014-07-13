f <- function(x) {
	print(x)
        g <- function(y) {
		y + z
		print (y+z)
        }
        z <- 4
        x + g(x)
}

z <- 10
f(3)

