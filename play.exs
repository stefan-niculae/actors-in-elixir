defmodule Play do
  def pythagorean(n) when n > 0 do
    # list comprehension
    for a <- 1..n,  # produs cartezian [1..n]^3
        b <- 1..n,
        c <- 1..n,
        a + b + c <= n,  # doua filtre
        a*a + b*b == c*c,
        do: {a, b, c}
    end
end

IO.puts Play.pythagorean 5
