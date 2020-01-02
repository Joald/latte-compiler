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
    cmd = ["./runfile.sh"]
else:
    cmd = argv[2].split(" ")


passed = 0
total = 0

quitting = False
resetting = True
autoplaying = False

def runFile(f):
    global total, passed, quitting, resetting, autoplaying
    total += 1
    print(magenta + "FILE " + yellow + f + cyan)
    
    system("cat " + f)
    print("\n" + magenta + "FILE END")
    pat = f[:-4] 
    inpat = pat + ".input"
    inFile = open(inpat, 'r') if running and path.exists(inpat) else None 

    res = subprocess.run(cmd + [f], stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=inFile)
    if res.returncode == 0:
        print(green)
        system("rm " + pat)
        system("rm " + pat + ".s")

        passed += 0 if running else 1
    else:
        autoplaying = False
        print(red)

    
    print(bgwhite)
    out = res.stdout.decode("utf-8")
    print(out)
    print(res.stderr.decode("utf-8"))
    if running:
        outFile = open(pat + ".output", 'r').read()
        if outFile == out:
            print(green + "OUTPUT OK")
            passed += 1
        else:
            autoplaying = False
            print(red + "OUTPUT DIFFERS")
            print(reset + "Expected:")
            print(green + outFile)
            print(reset + "Got:")
            print(yellow + out)

            

    print(reset)
    if not autoplaying:
        a = input("Press Enter to continue, q to quit, a to autoplay, r to restart...\n")
        if 'q' in a:
            quitting = True
            return
        if 'a' in a:
            autoplaying = True
            return
        if 'r' in a:
            passed = 0
            total = 0
            resetting = True
            return

    print("")

print(reset)


while resetting:
    if system("stack build") != 0:
        exit()
    resetting = False
    for f in listdir(dirr):
        if f.endswith(".lat"):
            runFile(dirr + "/" + f)
            if quitting or resetting:
                break

print(reset)

print("\n\n\nPassed: [%s%s%s/%s%s%s]" % (green if passed == total else red if passed == 0 else yellow, passed, reset, green, total, reset))

