x = 5;

def func1():
    global x;
    x = 10;
    print(x);
;

print(func1());
print(x);



def func2():
    global y;
    y = 10;
    print(x);
;

print(func2());
print(y);