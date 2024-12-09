import sys

digits = '1234567890'
written_digits = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'zero']
wd_to_d = {
    'one' : '1',
    'two' : '2',
    'three' : '3',
    'four' : '4',
    'five' : '5',
    'six' : '6',
    'seven' : '7',
    'eight' : '8',
    'nine' : '9',
    'zero' : '0',
}

numbers = []

while True:
    try:
        line = input()
        print(line)
        
        # Check if line contains a written digit
        min_d = {'digit': '', 'index': sys.maxsize}
        max_d = {'digit': '', 'index': -sys.maxsize}
        
        # Determine first the digit that appears first
        for wd in written_digits:
            i = line.find(wd)
            if i != -1 and i < min_d['index']:
                min_d['digit'] = wd_to_d[wd]
                min_d['index'] = i
        
        for i in range(len(line)):
            c = line[i]
            if c in digits:
                if i < min_d['index']:
                    min_d['digit'] = c
                    min_d['index'] = i
                    break
        
        max_d['digit'] = min_d['digit']
        max_d['index'] = min_d['index']
        for wd in written_digits:
            i = line.rfind(wd)
            if i != -1 and i > max_d['index']:
                print('found max digit:', wd, ', index:', i)
                max_d['digit'] = wd_to_d[wd]
                max_d['index'] = i
        
        for i in range(len(line)-1, -1, -1):
            c = line[i]
            if c in digits:
                if i > max_d['index']:
                    max_d['digit'] = c
                    max_d['index'] = i
                    break
        
        print(min_d)
        print(max_d)
        
        if min_d['index'] == sys.maxsize or max_d['index'] == -sys.maxsize:
            print('Fatal error')
            break
        
        number = min_d['digit'] + max_d['digit']
        if len(number) < 2:
            print('Fatal error')
            break
        
        numbers.append(number)
    
    except EOFError as e:
        print('EOF')
        break

print(numbers)
sum = 0
for number in numbers:
    sum += int(number)
print(sum)