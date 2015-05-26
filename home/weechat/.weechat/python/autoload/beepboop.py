import weechat
import re
from collections import namedtuple

weechat.register(
    "beepboop",
    "Elecsys Corporation Frimware Team",
    "1.0",
    "Public Domain",
    "Handles beepboop encoding/decoding",
    "",
    ""
)

beepboop = {
    ".": "BOOP",
    "-": "BEEP",
}

morse = {
    "A": ".-",
    "B": "-...",
    "C": "-.-.",
    "D": "-..",
    "E": ".",
    "F": "..-.",
    "G": "--.",
    "H": "....",
    "I": "..",
    "J": ".---",
    "K": "-.-",
    "L": ".-..",
    "M": "--",
    "N": "-.",
    "O": "---",
    "P": ".--.",
    "Q": "--.-",
    "R": ".-.",
    "S": "...",
    "T": "-",
    "U": "..-",
    "V": "...-",
    "W": ".--",
    "X": "-..-",
    "Y": "-.--",
    "Z": "--..",
}
inv_morse = {}

# convert -. to BEEPBOOP
for a in morse.keys():
    trans = ""
    b = morse[a]
    for c in range(0,len(b)):
        trans += beepboop[b[c:c+1]]
    morse[a] = trans
    inv_morse[trans] = a

sound_sep = ""
char_sep = " "
word_sep = " "


def privmsg_mod_cb(data, modifier, modifier_data, string):
    msgmatch = re.match(r"^:([^ ]+) PRIVMSG ([^ ]+) :(.*)", string)
    if not msgmatch:
        return string
    msgwho = msgmatch.group(1)
    msgwhere = msgmatch.group(2)
    msgmsg = msgmatch.group(3)
    aftermsg = ""

    i = 0
    while True:
        if i == len(msgmsg):
            break
        retest = re.search("(?:BEEP|BOOP)+ {0,1}", msgmsg[i:])
        if retest:
            morse_test = retest.group(0).strip()
            start, end = retest.span()
            if morse_test in inv_morse:
                aftermsg += msgmsg[i:(i+start)] + inv_morse[morse_test]
            else:
                aftermsg += msgmsg[i:(i+start)] + retest.group(0)
            i += end
        else:
            aftermsg += msgmsg[i:]
            break

    msgprefix = ""
    if aftermsg != msgmsg:
        msgprefix = "[BEEPBOOP]: "

    return ":%s PRIVMSG %s :%s%s" % (msgwho, msgwhere, msgprefix, aftermsg)

def encode_beepboop(text):
    res = ""

    text = text.upper()
    for c in text:
        if c == " ":
            res += word_sep
            continue
        if c in morse:
            res += morse[c]
            res += char_sep
    return res

def beepboop_cb(data, buffer, args):
    weechat.command(buffer, encode_beepboop(args))
    return weechat.WEECHAT_RC_OK

weechat.hook_modifier("irc_in_privmsg", "privmsg_mod_cb", "")
weechat.hook_command("beepboop", "converts text to beep boop language",
        "<msg>",
        "The message to convert to message",
        "%(filters_names)",
        "beepboop_cb", "")
