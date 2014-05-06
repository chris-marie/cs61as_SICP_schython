def __factorial(x, memo):
    if x <= 1:
        return x
    else:
        return x * memo(x-1)

factorial = memoize(__factorial)


def __fib(x, memo):
    if x <=1:
        return x
    else: 
        return memo(x-1) + memo(x-2)

fib_memo = memoize(__fib)
