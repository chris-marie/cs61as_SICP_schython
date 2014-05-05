def fib(n):
    if n == 0 or n == 1:                # fib(0) = 0, fib(1) = 1
        return n
    else:
        return fib(n-1) + fib(n-2)      # fib(n) = fib(n-1) + fib(n-2)

