import re
def insert_module(name, use_import=False):
    lastlinewithmod = 0
    lastline = len(snip.buffer)
    for i in range(0, lastline):
        line = snip.buffer[i]
        mo = re.search(r'^(use|require|package)\s+([a-zA-Z0-9:]+)', line)
        # snip.buffer[lastline:lastline] = ["HELLO %d '%s' '%s'" % (i, line, repr(mo))]
        if mo:
            if not re.search(r'^namespace::', mo[2]):
                lastlinewithmod = i
                if mo[1] in ['use', 'require'] and mo[2] == name:
                    return 1
        snip.buffer[lastlinewithmod+1:lastlinewithmod+1] = ['use %s%s;' % (name, "" if use_import else " ()")]
