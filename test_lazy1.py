def s(y=8):
    print(1);
    return y + 1;
    ;

def func(x=5):
    y = x + 2;
    return y;
    ;

print(func(s(4)));
print(func());
