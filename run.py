from os import system, listdir, path
from sys import argv
import subprocess


yellow = "\u001b[33m"
green = "\u001b[32m"
red = "\u001b[31m"
blue = "\u001b[34m"
cyan = "\u001b[36m"
reset = "\u001b[0m"
magenta = "\u001b[35m"
bgwhite = ""#\u001b[47;1m"

running = False

if len(argv) < 2:
    dirr = "examples/bad"
else:
    dirr = "examples/" + argv[1]
    while dirr.endswith("/"):
        dirr = dirr[:-1]
    if dirr.endswith("good"):
        running = True
if len(argv) < 3:
    cmd = ["stack", "run"]
else:
    cmd = argv[2].split(" ")

if system("stack build") != 0:
    exit()

passed = 0
total = 0
def runFile(f):
    global total, passed
    total += 1
    print(magenta + "FILE " + yellow + f + cyan)
    
    system("cat " + f)
    print("\n" + magenta + "FILE END")
    pat = f[:-3] + "input"
    inFile = open(pat, 'r') if running and path.exists(pat) else None 

    res = subprocess.run(cmd + [f], stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=inFile)
    if res.returncode == 0:
        print(green)
        passed += 0 if running else 1
    else:
        print(red)

    
    print(bgwhite)
    out = res.stdout.decode("utf-8")
    print(out)
    print(res.stderr.decode("utf-8"))
    if running:
        outFile = open(f[:-3] + "output", 'r').read()
        if outFile == out:
            print(green + "OUTPUT OK")
            passed += 1
        else:
            print(red + "OUTPUT DIFFERS")
            

    print(reset)
    a = input("Press Enter to continue, q to quit...\n")
    if 'q' in a:
        return True
    print("")
    return False

print(reset)

for f in listdir(dirr):
    if f.endswith(".lat") and runFile(dirr + "/" + f):
        break

print(reset)

print("\n\n\nPassed: [%s%s%s/%s%s]" % (green if passed == total else red if passed == 0 else yellow, passed, reset, green, total))

