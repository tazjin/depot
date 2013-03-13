zmodload -a colors
zmodload -a autocomplete
autoload -U age && age

export EDITOR='subl -w'

[ -x /usr/bin/dircolors ] && eval $(dircolors)
zstyle ':completion:*' menu select=2
zstyle ':completion:*' verbose true
zstyle ':completion:*' extra-verbose true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate

autoload -Uz compinit && compinit

setopt autocd autopushd pushdminus pushdsilent pushdtohome
setopt HIST_REDUCE_BLANKS HIST_IGNORE_SPACE SHARE_HISTORY inc_append_history
setopt no_hup no_clobber print_exit_value
setopt extendedglob glob_dots
setopt correct
setopt completealiases
setopt prompt_subst

case $TERM in
    rxvt|*term)
        precmd() { print -Pn "\e]0;%m:%~\a" }
        preexec () { print -Pn "\e]0;$1\a" }
    ;;
esac

autoload -U colors && colors
#vincent: PS1="%{%B$fg[blue]%}%n%{$reset_color%B%}@%{%b$fg[magenta]%}%1~ %{$reset_color%}\$(vcprompt -f '%b:%r ')%# "

#PS1="%n@%1~ \$(vcprompt -f '%b:%r ') %# "
PS1="%{$fg[green]%}%n%{$fg[cyan]%}@%{$fg[magenta]%}%1~ %{$fg[yellow]%}\$(vcprompt -f '%b:%r ')%{$fg[cyan]%}%# %{$reset_color%}"
RPS1="%{$fg[cyan]%}%B[%M:%d]%b%{$reset_color%}"


alias ls="ls -liFG"

autoload -Uz compinit && compinit

source ~/.zsh/plugins/history-substring-search.zsh
source ~/.zsh/plugins/git.plugin.zsh
source ~/.zsh/plugins/git-extras.plugin.zsh

# Customize to your needs...
export PATH=/Users/vincent/Library/Haskell/bin:/Users/vincent/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/munki

source /opt/boxen/env.sh


alias google.py="/Users/vincent/Source/management-scripts/google-apps/google.py"

alias runhoogle="screen -dm hoogle server -p 4000"

alias irssi="screen -S irssi irssi"

HISTFILE="$HOME/.zsh_history"
