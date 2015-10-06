import neovim
import threading
import time
import greenlet
import subprocess
import os
import re
import selectors
import sys
import logging

DEBUG = False

try:
    from Queue import Queue, Empty
except ImportError:
    from queue import Queue, Empty

def debug(vim, s):
    s = re.sub('(["\\\\])', lambda x: '\\' + x.group(1), s)
    s = re.sub('\n', '\\n', s)
    s = re.sub('\r', '\\r', s)

    if DEBUG:
        vim.session.threadsafe_call(lambda: vim.command('echomsg "%s"' % s))

class LineToExpression(threading.Thread):
    def __init__(self, f, q, vim):
        self.f = f
        self.q = q
        self.vim = vim
        super(LineToExpression, self).__init__()
        self.daemon = True

    def run(self):
        debug(self.vim, "This is a test - %s" % repr(self.f))
        try:
            debug(self.vim, "Starting ... %s %s" % (repr(self.f), repr(self.q)))
            for l in iter(self.f.readline, b''):
                debug(self.vim, "Got a line - %s" % repr(l))
                l = l.decode(encoding='utf-8', errors='ignore')
                # Handle vim escaping (\ and ")
                l = re.sub('(["\\\\|])', lambda x: '\\' + x.group(1), l)
                l = re.sub("\n", '\\\\n', l)
                l = re.sub("\r", '\\\\r', l)
                self.q.put(l)
        except IOError:
            pass

class Searcher(threading.Thread):
    def __init__(self, vim, info, quit):
        self.vim = vim
        self.info = info
        self.quit = quit
        self.first = True
        self.lock = threading.Lock()
        super(Searcher, self).__init__()
        self.daemon = True
        self.commands = []

    def append_quickfix(self):
        debug(self.vim, "In append quickfix")
        with self.lock:
            debug(self.vim, "got lock")
            my_commands = self.commands
            self.commands = []

        debug(self.vim, repr(my_commands))
        for c in my_commands:
            debug(self.vim, c)
            self.vim.command(c)

    def process_lines(self, lines):
        debug(self.vim, repr(lines))
        debug(self.vim, "waiting lock")
        with self.lock:
            debug(self.vim, "got lock")
            for l in lines:
                debug(self.vim, "processing %s" % l)
                if self.first:
                    self.commands.append('cexpr "%s"' % l)
                    self.first = False
                else:
                    self.commands.append('caddexpr "%s"' % l)
        debug(self.vim, repr(self.commands))
        self.vim.session.threadsafe_call(self.append_quickfix)

    def run(self):
        pattern = self.info['pattern']
        cwd = self.info['cwd']
        prog = self.info['prog']
        (path, prog_type) = os.path.split(prog)
        args = self.info['args']
        ic = self.info['ic']
        sc = self.info['sc']

        if prog_type in "pt ag ack".split():
            if sc == 1 and ic == 1:
                args.insert(0, '--smart-case')
            elif ic == 1:
                args.insert(0, '-i')
            args.extend(['--nogroup', '--', pattern])
        elif prog_type == "grep":
            args.insert(0, '-r')
            if sc == 1 and ic == 1:
                if not re.search('[ABCDEFGHIJKLMNOPQRSTUVWXYZ]', pattern):
                    args.insert(0, '-i')
            if ic == 1:
                args.insert(0, '-i')
            args.insert(0, '-r')
            args.extend(['--', pattern, '.'])
        else:
            args.append(pattern)

        args.insert(0, prog)

        with subprocess.Popen(args, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, cwd=cwd) as proc:
            q = Queue()
            LineToExpression(proc.stdout, q, self.vim).start()
            LineToExpression(proc.stderr, q, self.vim).start()

            while True:
                lines = []
                try:
                    start = time.clock()
                    loop_time = 0.25
                    while True:
                        wait_time = loop_time - (time.clock() - start)
                        if (wait_time > 0.0):
                            lines.append(q.get(True, wait_time))
                        else:
                            break
                except Empty:
                    pass
                if len(lines) > 0:
                    self.process_lines(lines)
                if self.quit.wait(1):
                    break
        debug(self.vim, 'done?')

@neovim.plugin
class AsyncSearch(object):
    def __init__(self, vim):
        self.vim = vim
        self.searcher = None

    @neovim.command('Grep', nargs=1)
    def async_search(self, args):
        info = {
            "prog": self.vim.eval('g:grep#prog'),
            "args": self.vim.eval('g:grep#args').split(),
            "ic": self.vim.eval('&ic'),
            "sc": self.vim.eval('&sc'),
            "pattern": args[0],
            "cwd": self.vim.eval('getcwd()')
        }

        if self.searcher:
            self.searcher.quit.set()
            self.searcher.join()
        self.vim.command('cgetexpr "Searching in background..."')
        self.searcher = Searcher(self.vim, info, threading.Event())
        self.searcher.start()
