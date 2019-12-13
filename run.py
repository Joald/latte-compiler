from os import system, listdir
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
if len(argv) < 2:
    dirr = "examples/bad"
else:
    dirr = "examples/" + argv[1]
    while dirr.endswith("/"):
        dirr = dirr[:-1]
if len(argv) < 3:
    cmd = ["stack", "run"]
else:
    cmd = argv[2].split(" ")

if system("stack build") != 0:
    exit()

def runFile(f):
    print(magenta + "FILE " + yellow + f + cyan)
    
    system("cat " + f)
    print("\n" + magenta + "FILE END")
    res = subprocess.run(cmd + [f], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if res.returncode == 0:
        print(green)
    else:
        print(red)
    print(bgwhite)
    print(res.stdout.decode("utf-8"))
    print(res.stderr.decode("utf-8"))
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
