def s(y=8):
    print(1);
    return y + 1;
    ;

def func(x=s()):
    return 12;
    ;

print(func(s(4)));
print(func());
