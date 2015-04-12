pseudoLog10 <- function(x) { asinh(x/2)/log(10) }

# source: http://www.r-bloggers.com/modeling-trick-the-signed-pseudo-logarithm/

# 1. pseudoLog10(x) is defined for all real x.
# 2. pseudoLog10(0) = 0.
# 3. pseudoLog10(-x) = -pseudoLog10(x).
# 4. pseudoLog10(x) is monotone in x.
# 5. For x such that |x| is large: pseudoLog10(x) is very near sign(x)*log10(|x|).