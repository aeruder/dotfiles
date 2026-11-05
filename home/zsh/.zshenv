#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

if [ -e ~/.zshenv.local.early ]; then
    source ~/.zshenv.local.early
fi
#
# Editors
#

export SUDO_EDITOR=e
export EDITOR=e
export VISUAL=e
export LESS=-FmqXR
export PAGER=less
export DBIC_TRACE_PROFILE=console

if [ -x /bin/systemctl ]; then
export XDG_RUNTIME_DIR="/run/user/$UID"
export DBUS_SESSION_BUS_ADDRESS="unix:path=${XDG_RUNTIME_DIR}/bus"
fi

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Paths
#

if [ -e /usr/share/source-highlight/src-hilite-lesspipe.sh ]; then
    export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
fi
if [ -e /usr/bin/src-hilite-lesspipe.sh ]; then
    export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
fi

# Deal with konsole's broken TERM setting
if [ "$TERM" != "screen" ] && [ "$TERM" != "screen-256color" ]; then
  if [ "$COLORTERM" = "gnome-terminal" ] || [ -n "$KONSOLE_PROFILE_NAME" ]; then
      export TERM=xterm-256color
  fi
fi

source ~/.dotfiles/zsh/func/refresh-fpath
refresh-path
refresh-manpath
refresh-ldpath

#
# Temporary Files
#

# if [[ ! -d "$TMPDIR" ]]; then
#   export TMPDIR="/tmp/$USER"
#   mkdir -p -m 700 "$TMPDIR"
# fi

# TMPPREFIX="${TMPDIR%/}/zsh"
# if [[ ! -d "$TMPPREFIX" ]]; then
#   mkdir -p "$TMPPREFIX"
# fi

# GTK stuff
export GTK_IM_MODULE="xim"

export PERLBREW_ROOT="$HOME"/.perl5
if [ -e "$PERLBREW_ROOT"/etc/bashrc ]; then
  source "$PERLBREW_ROOT"/etc/bashrc
fi

if command -v pyenv >/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

if which rg 2> /dev/null > /dev/null ; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden -g !.git'
elif which fd 2> /dev/null > /dev/null ; then
  export FZF_DEFAULT_COMMAND='fd --type file --hidden --exclude .git'
fi

if [ -e ~/.zshenv.local ]; then
    source ~/.zshenv.local
fi

export GOPATH="$HOME"/go
export PATH="$PATH":"$GOPATH"/bin
