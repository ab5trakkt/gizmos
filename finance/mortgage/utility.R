#
# MIT License
# 
# Copyright (c) 2017 ab5trakkt
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

# Calculate k^-1 + k^-2 + .. + k^-n
inv_sum_k <- function(k, n)
{
  s <- 0
  for (j in seq(1,n))
  {
    s <- s + k^-j
  }
  return (s)
}


# Calculate 1 + k + k^2 + .. + k^n
sum_k <- function(k, n)
{
  s <- 0
  for (j in seq(0,n))
  {
    s <- k*s+1
  }
  return (s)
}

i_eff <- function(i,T)
{
  1.0+(i/100/T)
}
