def range(a=0, b=10, step=1):
    if a == b:
        return [];
    else:
        return [a] + range(a + step, b, step);
    ;
;

def len(l=[]):
    x = 0;
    for i in l:
        x = x + 1;
    ;
    return x;
;

def swap(arr=[], i=0, j=1):
    res = [];
    r = range(0, len(arr));
    for x in r:
        if x == i:
            res = res + [arr[j]];
        else:
            if x == j:
                res = res + [arr[i]];
            else:
                res = res + [arr[x]];
            ;
        ;
        print(res);
    ;
    return res;
;
 
def bubbleSort(arr=[]):
    n = len(arr);

    swapped = False;
    for i in range(0, n):
        for j in range(0, n-i-1):
            if arr[j] > arr[j + 1]:
                swapped = True;
                arr = swap(arr, j, j+1);
                print(arr);
            else:
                pass;
            ;
        ;
        if swapped == False:
            break;
        else:
            continue;
        ;
    ;

    return arr;
;

arr = [64, 34, 25, 12, 22, 11, 90];
print(arr);
sorted = bubbleSort(arr);
print(sorted);