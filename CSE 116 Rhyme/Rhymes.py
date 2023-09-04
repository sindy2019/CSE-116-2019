def openFile(filename):

    with open(filename) as f:
        for line in f:
            print(line)


print(openFile("dictionary.txt"))


def findRhymes(a, b):
    if len(a) != len(b):
        return False
    elif(len(a) == len(b) == 0):
        return True
    else:
        for i in range(len(a)):
            if a[i] != b[i]:
                return False

        return True


print(findRhymes(["zoss, zoster"], ["zoss, zoster"]))
print(findRhymes(["zoss, zoster"], ["zoss, zostr"]))
print(findRhymes([1, 2, 3], [1, 5, 6]))
print(findRhymes([], [1]))




