import weechat
import re
from collections import namedtuple

weechat.register(
    "ijchain",
    "Andrew Ruder",
    "1.0",
    "Public Domain",
    "Processes ijchain (also see /getijchainsnames) messages",
    "",
    ""
)

WaitingNamesInfo = namedtuple("WaitingNamesInfo",
["server", "channel", "mynick", "theirnick", "buffer"]
)

waiting_on_names = None

def privmsg_mod_cb(data, modifier, modifier_data, string):
    global waiting_on_names
    #weechat.prnt("", "MOD data:%s" % data)
    #weechat.prnt("", "MOD modifier:%s" % modifier)
    #weechat.prnt("", "MOD modifier_data:%s" % modifier_data)
    #weechat.prnt("", "MOD string:%s" % string)
    nick = weechat.info_get("irc_nick_from_host", string)
    server = modifier_data
    channel = string.split(" ")[2]

    if not re.match(r"^ijchain\d*$", nick):
        return string

    msgmatch = re.match(r"^:([^ ]+) PRIVMSG ([^ ]+) :(.*)", string)
    if not msgmatch:
        return string
    msgwho = msgmatch.group(1)
    msgwhere = msgmatch.group(2)
    msgmsg = msgmatch.group(3)

    if waiting_on_names and \
       channel == waiting_on_names.mynick and \
       nick == waiting_on_names.theirnick:
        result = []
        for newnickmatch in re.finditer(r"[^ ]+", msgmsg):
            newnick = newnickmatch.group(0)
            result.append(":%s!~%s@jabber JOIN :%s" % (newnick, newnick, waiting_on_names.channel))
        waiting_on_names = None
        return "\n".join(result)

    msgaction = re.match(r"^\001ACTION ([^ ]+) (.*)\001$", msgmsg)
    msgforwardedmsg = re.match(r"^\<([^ ]+)\> (.*)$", msgmsg)
    if msgaction:
        theirnick = msgaction.group(1)
        theirmsg = msgaction.group(2)
        # weechat.prnt("", "%s %s %s" % (theirnick, theirmsg, channel))
        if theirmsg == "has become available":
            return ":%s!~%s@jabber JOIN :%s" % (theirnick, theirnick, channel)
        elif theirmsg == "has left":
            return ":%s!~%s@jabber PART %s :Left jabber" % (theirnick, theirnick, channel)
        else:
            return ":%s!~%s@jabber PRIVMSG %s :\001ACTION %s\001" % (theirnick, theirnick, channel, theirmsg)
    elif msgforwardedmsg:
        theirnick = msgforwardedmsg.group(1)
        theirmsg = msgforwardedmsg.group(2)
        return ":%s!~%s@jabber PRIVMSG %s :%s" % (theirnick, theirnick, channel, theirmsg)

    return string

def expire_getnames_cb(data, remaining_calls):
    global waiting_on_names

    if not waiting_on_names:
        return weechat.WEECHAT_RC_OK
    if data == ("%s.%s" % (waiting_on_names.server, waiting_on_names.theirnick)):
        weechat.prnt(waiting_on_names.buffer, "Timed out waiting for names")
        waiting_on_names = None
    return weechat.WEECHAT_RC_OK

def getnames_cb(data, buffer, args):
    global waiting_on_names

    plugin = weechat.buffer_get_string(buffer, "plugin")
    if plugin != "irc":
        weechat.prnt(buffer, "I don't handle plugin %s" % plugin)
        return weechat.WEECHAT_RC_ERROR
    buffertype = weechat.buffer_get_string(buffer, "localvar_type")
    if (buffertype != "channel"):
        weechat.prnt(buffer, "You aren't in a channel (type: %s)" % buffertype)
        return weechat.WEECHAT_RC_ERROR

    waiting_on_names = WaitingNamesInfo(
        server = weechat.buffer_get_string(buffer, "localvar_server"),
        channel = weechat.buffer_get_string(buffer, "localvar_channel"),
        mynick = weechat.buffer_get_string(buffer, "localvar_nick"),
        theirnick = args,
        buffer = buffer)

    weechat.command(buffer, "/msg %s names" % args)
    weechat.hook_timer(5000, 0, 1, "expire_getnames_cb",
            "%s.%s" % (waiting_on_names.server, waiting_on_names.theirnick))

    return weechat.WEECHAT_RC_OK

weechat.hook_modifier("irc_in_privmsg", "privmsg_mod_cb", "")
weechat.hook_command("getijchainsnames", "Queries ijchains bot for names",
        "<name>",
        "Queries the bot named <name> for jabber names",
        "%(filters_names)",
        "getnames_cb", "")
