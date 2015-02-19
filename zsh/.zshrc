source ~/.dotfiles/.misc/antigen/antigen.zsh

antigen bundle https://github.com/aeruder/zsh-namedir.git
antigen theme  https://github.com/aeruder/zsh-prompt-aeruder.git aeruder
antigen bundle https://github.com/aeruder/zsh-prompt-aeruder.git modules/exitcode
antigen bundle https://github.com/aeruder/zsh-prompt-aeruder.git modules/git
antigen bundle https://github.com/aeruder/zsh-prompt-aeruder.git modules/diskspace
antigen bundle https://github.com/aeruder/zsh-prompt-aeruder.git modules/vimode
antigen apply

# Automatically push directory onto the directory stack
setopt autopushd

# When there are duplicates in the history, expire
# those first.  That way you only end up with one
# ls in your history instead of 100
setopt histexpiredupsfirst

# If a command starts with a space, don't put it in the
# history
setopt histignorespace

# When you use history expansion (!*) retype the command
# instead of just immediately running it
setopt histverify

# Allow comments on the command-line
setopt interactivecomments

# Don't send HUP signal when closing shell
setopt nohup

# Don't beep
setopt nobeep

# Don't report on jobs when shell exit.
unsetopt checkjobs

# Don't run all background jobs at a lower priority.
unsetopt bgnice

# Don't include duplicate directories in
# directory stack
setopt pushdignoredups

# Swap meaning of +/- in PUSHD history
setopt pushdminus

# Don't print the directory after pushd/popd
setopt pushdsilent

# pushd with no arguments is pushd $HOME
setopt pushdtohome

# When a glob doesn't match, just pass it to the command
unsetopt nomatch

# Allow brace character class list expansion.
setopt braceccl

# Combine zero-length punctuation characters (accents)
# with the base character.
setopt combiningchars

# Don't print a warning message if a mail file has been accessed.
unsetopt mailwarning

# Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'.
setopt rcquotes

# List jobs in the long format by default.
setopt longlistjobs

# Attempt to resume existing job before creating a new process.
setopt autoresume

# Report status of background jobs immediately.
setopt notify

##################################
# Smart URLs
##################################

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

##################################
# add our battery to the prompt
##################################
function battery_percent() {
    local a b
    a=$(</sys/class/power_supply/BAT1/energy_now)
    b=$(</sys/class/power_supply/BAT1/energy_full)
    echo "$(((a * 100) / b))%%"
}

if [ `hostname` = tanooki ]; then
    PR_FLAGS+=(battery_percent)
fi

# Sets terminal window and tab titles.
# # Return if requirements are not found.
# if [[ "$TERM" == (dumb|linux|*bsd*) ]]; then
#   return 1
# fi
# 
# # Sets the terminal or terminal multiplexer window title.
# function set-window-title {
#   local title_format{,ted}
#   zstyle -s ':prezto:module:terminal:window-title' format 'title_format' || title_format="%s"
#   zformat -f title_formatted "$title_format" "s:$argv"
# 
#   if [[ "$TERM" == screen* ]]; then
#     title_format="\ek%s\e\\"
#   else
#     title_format="\e]2;%s\a"
#   fi
# 
#   printf "$title_format" "${(V%)title_formatted}"
# }
# 
# # Sets the terminal tab title.
# function set-tab-title {
#   local title_format{,ted}
#   zstyle -s ':prezto:module:terminal:tab-title' format 'title_format' || title_format="%s"
#   zformat -f title_formatted "$title_format" "s:$argv"
# 
#   printf "\e]1;%s\a" ${(V%)title_formatted}
# }
# 
# # Sets the tab and window titles with a given command.
# function _terminal-set-titles-with-command {
#   emulate -L zsh
#   setopt EXTENDED_GLOB
# 
#   # Get the command name that is under job control.
#   if [[ "${2[(w)1]}" == (fg|%*)(\;|) ]]; then
#     # Get the job name, and, if missing, set it to the default %+.
#     local job_name="${${2[(wr)%*(\;|)]}:-%+}"
# 
#     # Make a local copy for use in the subshell.
#     local -A jobtexts_from_parent_shell
#     jobtexts_from_parent_shell=(${(kv)jobtexts})
# 
#     jobs "$job_name" 2>/dev/null > >(
#       read index discarded
#       # The index is already surrounded by brackets: [1].
#       _terminal-set-titles-with-command "${(e):-\$jobtexts_from_parent_shell$index}"
#     )
#   else
#     # Set the command name, or in the case of sudo or ssh, the next command.
#     local cmd="$1"
#     local truncated_cmd="${cmd/(#m)?(#c60,)/${MATCH[1,54]}...}"
#     unset MATCH
# 
#     set-window-title "$cmd"
#     set-tab-title "$truncated_cmd"
#   fi
# }
# 
# # Sets the tab and window titles with a given path.
# function _terminal-set-titles-with-path {
#   emulate -L zsh
#   setopt EXTENDED_GLOB
# 
#   local absolute_path="${${1:a}:-$PWD}"
#   local abbreviated_path="${absolute_path/#$HOME/~}"
#   local truncated_path="${abbreviated_path/(#m)?(#c15,)/...${MATCH[-12,-1]}}"
#   unset MATCH
# 
#   set-window-title "$abbreviated_path"
#   set-tab-title "$truncated_path"
# }
# 
# # Sets the Terminal.app proxy icon.
# function _terminal-set-terminal-app-proxy-icon {
#   printf '\e]7;%s\a' "file://$HOST${${1:-$PWD}// /%20}"
# }
# 
# # Do not override precmd/preexec; append to the hook array.
# autoload -Uz add-zsh-hook
# 
# # Set up the Apple Terminal.
# if [[ "$TERM_PROGRAM" == 'Apple_Terminal' ]] \
#   && ( ! [[ -n "$STY" || -n "$TMUX" || -n "$DVTM" ]] )
# then
#   # Sets the Terminal.app current working directory before the prompt is
# 	# displayed.
#   add-zsh-hook precmd _terminal-set-terminal-app-proxy-icon
# 
# 	# Unsets the Terminal.app current working directory when a terminal
# 	# multiplexer or remote connection is started since it can no longer be
#   # updated, and it becomes confusing when the directory displayed in the title
#   # bar is no longer synchronized with real current working directory.
# 	function _terminal-unset-terminal-app-proxy-icon {
#     if [[ "${2[(w)1]:t}" == (screen|tmux|dvtm|ssh|mosh) ]]; then
#       _terminal-set-terminal-app-proxy-icon ' '
#     fi
# 	}
# 	add-zsh-hook preexec _terminal-unset-terminal-app-proxy-icon
# 
#   # Do not set the tab and window titles in Terminal.app since it sets the tab
#   # title to the currently running process by default and the current working
#   # directory is set separately.
# 	return
# fi
# 
# # Set up non-Apple terminals.
# if zstyle -t ':prezto:module:terminal' auto-title \
#   && ( ! [[ -n "$STY" || -n "$TMUX" ]] )
# then
# 	# Sets the tab and window titles before the prompt is displayed.
# 	add-zsh-hook precmd _terminal-set-titles-with-path
# 
# 	# Sets the tab and window titles before command execution.
# 	add-zsh-hook preexec _terminal-set-titles-with-command
# fi

# ls
if is-callable 'dircolors'; then
    if [[ -s "$HOME/.dir_colors" ]]; then
        eval "$(dircolors "$HOME/.dir_colors")"
    else
        eval "$(dircolors)"
    fi
else
    # Define colors for BSD ls.
    export LSCOLORS='ExfxcxdxBxGxDxaBagacad'

    # Define colors for the completion system.
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=36;01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'
fi

# COMPLETION
# Add zsh-completions to $fpath.
fpath=("${0:h}/external/src" $fpath)

# Load and initialize the completion system ignoring insecure directories.
autoload -Uz compinit && compinit -i

#
# Options
#

setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt PATH_DIRS           # Perform path search even on command names with slashes.
setopt AUTO_MENU           # Show completion menu on a succesive tab press.
setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.

#
# Styles
#

# Use caching to make completion for cammands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

# Case-insensitive (all), partial-word, and then substring completion.
if zstyle -t ':prezto:module:completion:*' case-sensitive; then
  zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  setopt CASE_GLOB
else
  zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  unsetopt CASE_GLOB
fi

# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions.
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Increase the number of errors based on the length of the typed word.
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Don't complete unavailable commands.
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting.
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# History
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Environmental Variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# Populate hostname completion.
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# Don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
  dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
  hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
  mailman mailnull mldonkey mysql nagios \
  named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
  operator pcap postfix postgres privoxy pulse pvm quagga radvd \
  rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media Players
zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

# Mutt
if [[ -s "$HOME/.mutt/aliases" ]]; then
  zstyle ':completion:*:*:mutt:*' menu yes select
  zstyle ':completion:*:mutt:*' users ${${${(f)"$(<"$HOME/.mutt/aliases")"}#alias[[:space:]]}%%[[:space:]]*}
fi

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

##################################
# Alias
##################################

alias damn='man'
alias show='xdg-open'
alias rm='rm -i'
alias mv='mv -i'

if [ "`uname`" != "Darwin" ]; then
  alias cp='cp -i --reflink=auto'
else
  alias cp='cp -i'
fi

alias grep='grep --color=auto'
alias rgrep='grep --color=auto -r'
alias egrep='grep --color=auto -E'
alias irb='irb --readline -r irb/completion'
alias dh='dirs -v'
alias vim='e'
alias emacs='e'

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
bindkey -M isearch 'jk' "accept-search ; vi-cmd-mode"
bindkey "^?" backward-delete-char

##################################
# sudo vim -> sudoedit
##################################
function sudo() {
    if [ "$1" = "vim" ] || [ "$1" = "$VISUAL" ]; then
        shift
        sudo -e "$@"
    else
        command sudo "$@"
    fi
}

HISTFILE=$HOME/.zsh_history
HISTSIZE=900000000
SAVEHIST=100000
DIRSTACKSIZE=20

if [ -e ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
