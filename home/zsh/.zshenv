#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export SUDO_EDITOR=e
export EDITOR=e
export VISUAL=e
export LESS=-FmqXR
export PAGER=less

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

# Read in from .zsh-pre-manpaths and .zsh-post-manpaths
if [ -z "$MANPATH" ]; then
    unset MANPATH
    if ( which manpath >/dev/null 2>&1 ); then
        export MANPATH="`manpath`"
    else
        export MANPATH="`man -w`"
    fi
fi

manpath=( "${(s%:%)MANPATH}" )
[ -e ~/.zsh-pre-manpaths ] || touch ~/.zsh-pre-manpaths
[ -e ~/.zsh-post-manpaths ] || touch ~/.zsh-post-manpaths

prepaths=( ~/.dotfiles/man )
prepaths+=( ${(f)"$(<~/.zsh-pre-manpaths)"} )

postpaths=( )
postpaths+=( ${(f)"$(<~/.zsh-post-manpaths)"} )

for a in "${(Oa)prepaths[@]}"; do
    manpath=( "$a" "${manpath[@]}" )
done
for a in "${postpaths[@]}"; do
    manpath+=( "$a" )
done
manpath=( ${(u)manpath[@]} )
export MANPATH="${(j%:%)manpath}"

if [ "`uname`" = "Darwin" ]; then
    eval `/usr/libexec/path_helper -s`
fi

# Read in from .zsh-pre-paths and .zsh-post-paths
[ -e ~/.zsh-pre-paths ] || touch ~/.zsh-pre-paths
[ -e ~/.zsh-post-paths ] || touch ~/.zsh-post-paths

prepaths=( ~/.dotfiles/bin )
prepaths+=( ${(f)"$(<~/.zsh-pre-paths)"} )

postpaths=( )
postpaths+=( ${(f)"$(<~/.zsh-post-paths)"} )

for a in "${(Oa)prepaths[@]}"; do
    path=( "$a" "${path[@]}" )
done
for a in "${postpaths[@]}"; do
    path+=( "$a" )
done
path=( ${(u)path[@]} )

# Read in from .zsh-pre-ldpaths and .zsh-post-ldpaths
[ -e ~/.zsh-pre-ldpaths ] || touch ~/.zsh-pre-ldpaths
[ -e ~/.zsh-post-ldpaths ] || touch ~/.zsh-post-ldpaths

ldpath=( "${(s%:%)LD_LIBRARY_PATH}" )
prepaths=( )
prepaths+=( ${(f)"$(<~/.zsh-pre-ldpaths)"} )

postpaths=( )
postpaths+=( ${(f)"$(<~/.zsh-post-ldpaths)"} )

for a in "${(Oa)prepaths[@]}"; do
    ldpath=( "$a" "${ldpath[@]}" )
done
for a in "${postpaths[@]}"; do
    ldpath+=( "$a" )
done
ldpath=( ${(u)ldpath[@]} )
export LD_LIBRARY_PATH="${(j%:%)ldpath}"

#
# Temporary Files
#

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

export GTK_IM_MODULE="xim"
