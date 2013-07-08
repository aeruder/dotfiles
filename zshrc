# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="aeruder"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git namedir)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

##################################
# add our battery to the prompt
##################################
function battery_percent() {
    local a b
    a=$(</sys/class/power_supply/BAT1/energy_now)
    b=$(</sys/class/power_supply/BAT1/energy_full)
    echo "$(((a * 100) / b))%%"
}

PR_FLAGS+=(battery_percent)

##################################
# add directory size to our prompt
##################################
function byte_size() {
    local str sizes divs ans

    divs=0
    str=${1:-0}
    sizes=( "b" "k" "M" "G" )

    while (( str >= 100.0 )); do
        divs=$(( divs + 1 ))
        str=$(( str / 1024. ))
    done

    str="${(l.3.. .)"${${str[1,3]}%%.}"}"
    echo ${str}${sizes[divs+1]}
}

function pr_filesize() {
    local files
    files=(${(f)"$(stat -L +size -- *(.ND) /dev/null /dev/null)"})

    files=`byte_size $(( ${(j: + :)${(@)files##* }} ))`
    files=${${files%% #}## #}

    echo $files
}

PR_FLAGS+=(pr_filesize)

##################################
# Add zsh options and zsh builtins
##################################

# Necessary for pr_filesize, loads a stat builtin
# to zsh
zmodload zsh/stat

# Typing a directory name causes zsh to auto-convert
# it into cd <dir>
setopt autocd

# Automatically push directory onto the directory stack
setopt autopushd

# Let's you know about running jobs before closing the shell
setopt checkjobs

# pr_filesize needs extended glob
setopt extendedglob

# When there are duplicates in the history, expire
# those first.  That way you only end up with one
# ls in your history instead of 100
setopt histexpiredupsfirst

# Don't include duplicate commands in the
# history
# setopt histfindnodups

# Ignore all dups so that they will
# get removed
# setopt histignorealldups

# If a command starts with a space, don't put it in the
# history
setopt histignorespace

# When you use history expansion (!*) retype the command
# instead of just immediately running it
setopt histverify

# Allow comments on the command-line
setopt interactivecomments

# Allow multiple direction
setopt multios

# Don't beep
setopt nobeep

# Don't send HUP signal when closing shell
setopt nohup

# When there is no match pass the command through
setopt nonomatch

# Allow substitution in the prompt, necessary
# for almost all the oh-my-zsh themes
setopt prompt_subst

# Don't include duplicate directories in
# directory stack
setopt pushdignoredups

# Swap meaning of +/- in PUSHD history
setopt pushdminus

# Don't print the directory after pushd/popd
setopt pushdsilent

# pushd with no arguments is pushd $HOME
setopt pushdtohome

# Don't prompt for correction ever
setopt nocorrect
setopt nocorrectall

##################################
# Alias
##################################

alias damn='man'
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias grep='grep --color=auto'
alias rgrep='grep --color=auto -r'
alias egrep='grep --color=auto -E'
alias irb='irb --readline -r irb/completion'
alias dh='dirs -v'

alias -g CA="2>&1 | cat -A"
alias -g CNT='| wc -l'
alias -g DN='/dev/null'
alias -g EH='|& head'
alias -g EL='|& less'
alias -g ET='|& tail'
alias -g F='| fmt -'
alias -g G='| grep --color=auto'
alias -g H='| head'
alias -g LL="2>&1 | less"
alias -g L="| less"
alias -g LS='| less -S'
alias -g NE="2> /dev/null"
alias -g NS='| sort -n'
alias -g NUL="> /dev/null 2>&1"
alias -g S='| sort'
alias -g T='| tail'
alias -g US='| sort -u'

##################################
# Keybindings
##################################

bindkey -v
bindkey '^R' history-incremental-search-backward
bindkey -M viins 'jk' vi-cmd-mode

function vim_mode_flag() {
    echo $vim_mode
}
PR_FLAGS+=vim_mode_flag

vim_ins_mode="INS"
vim_cmd_mode="CMD"
vim_mode=$vim_ins_mode

function zle-keymap-select {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
  zle reset-prompt
}
zle -N zle-keymap-select

function zle-line-finish {
  vim_mode=$vim_ins_mode
}
zle -N zle-line-finish

##################################
# sudo vim -> sudoedit
##################################
function sudo() {
    if [ "$1" = vim ]; then
        shift
        sudo -e "$@"
    else
        command sudo "$@"
    fi
}

##################################
# Some options
##################################
HISTFILE=$HOME/.zsh_history
HISTSIZE=900000000
SAVEHIST=100000
DIRSTACKSIZE=20

. ~/.zshenv
