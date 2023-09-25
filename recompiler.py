import sys

jonesForth = False    #  set to True if recompiling Jonesforth 32-bit output

words = {
    "@": "FETCH",
    "<>": "NEQUAL",
    "':'": "COLCONST",
    "ID.": "IDDOT",
    "0BRANCH": "ZBRANCH",
    "?IMMEDIATE": "QIMMEDIATE",
    'S"': "LITSTRING-CONST",
    ">DFA": "TDFA",
    "'": "TICK",
    '"': "DQCONST",
    "=": "EQUAL",
    "!": "STORE",
    "CFA>": "CFAT",
    "';'": "SEMICONST",
    "1+": "INCR",
    "1-": "DECR",
    "2+": "INCR2",
    "2-": "DECR2",
    ";": "EXIT",
    ">": "GT",
    "<": "LT",
    ">=": "GTE",
    "<=": "LTE",
    "0=": "ZEQUAL",
    "0<": "ZLT",
    "0>": "ZGT",
    "+": "ADD",
    "-": "SUB",
    ",": "COMMA",
    ".": "DOT",
    "*": "MULT",
    "DSP@": "DSPFETCH",
    "DSP!": "DSPSTORE",
    "C@": "FETCHBYTE",
    "C!": "STOREBYTE",
    "'\"'": "DQCONST",
    "?DUP": "QDUP"
}

buf=""

def isint(s):
    try: 
        int(s)
    except ValueError:
        return False
    else:
        return True

def skipspace():
    global buf
    while (buf!="") and (buf[0]==' '):
        buf = buf[1:]


def nextword():
    global buf
    word = ""
    skipspace()

    if buf=="":
        return None

    while (buf[0]!=' '):
        word = word + buf[0]
        buf = buf[1:]
        if buf=="":
            return word

    return word

def getstring():
    global buf
    s = ""
    skipspace()
    while (buf[0]!='"'):
        s = s + buf[0]
        buf = buf[1:]
    buf = buf[1:]    # skip the "
    return s

def getoffset():
    global buf
    s = ""
    skipspace()
    while buf[0]!=")":
        s = s + buf[0]
        buf = buf[1:]
    buf = buf[1:]    # skip the )
    s = s.lstrip("(").strip()
    return int(s)


def main():
    global buf

    if jonesForth:
        # Jonesforth is 32-bit. We are 16-bit
        words["4+"] = "INCR2"
        words["4-"] = "DECR2"

    buf = sys.stdin.read().strip()

    w = nextword()
    if w!=":":
      raise Exception("Did not start with colon")

    funcName = nextword()

    func = []
    immed = False
    w = nextword()
    while (w != None):
        w = words.get(w,w)
        if (w=="IMMEDIATE"):
            immed = True
            w = nextword()
            continue

        if isint(w):
            func.append( "\t\tcw_lit %04XH" % int(w))
            w = nextword()
            continue

        if (w=="("):
            i = getoffset()
            if jonesForth:
                func.append( '\t\tdw %04XH' % (i/2) )        # Jonesforth is 32-bit. We are 16-bit
            else:
                func.append( '\t\tdw %04XH' % i )
            w = nextword()
            continue

        if w=="LITSTRING-CONST":
            s = getstring()
            func.append( '\t\tcw LITSTRING')
            func.append( '\t\tdw %04XH' % len(s) )
            func.append( '\t\tdb "%s"' % s )
        else:
            func.append( "\t\tcw %s" % w )

        w = nextword()

    print( "name_%s: linklast <fill me in>" % funcName)
    if immed:
        print('\t\tdb %d|F_IMMED,"%s"' % (len(funcName),funcName))
    else:
        print('\t\tdb %d,"%s"' % (len(funcName), funcName))

    print("cw_%s:\tcodeword_DOCOL" % funcName)

    for element in func:
        print(element)


if __name__ == "__main__":
    main()
            

