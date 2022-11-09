def foo(l=[]):
    return l

print(foo([1,2,3]))
print(foo())
print(foo())

result = foo()
result.append(99)

print(foo())

foo().append(98)
print(foo())