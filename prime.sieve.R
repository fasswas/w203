prime.sieve = function(n) {
  primes = c(2:n)
  i = 1
  repeat {
    p = primes[i]
    if (p > floor(n^0.5)) break;      
    primes = primes[primes == p | primes %% p != 0]
    i = i + 1
    if (i > length(primes)) break
  }
  
  return(primes)
}

