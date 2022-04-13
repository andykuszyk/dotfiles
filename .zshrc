# Add executable locations to PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:/usr/local/bin:/home/andy/go/bin"
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:/home/andy/.tfenv/bin"
export PATH="$PATH:$HOME/node-v16.13.1-linux-x64/bin"
export PATH="$PATH:/opt/homebrew/lib/ruby/gems/3.0.0/bin"
export PATH="$PATH:$HOME/.cargo/bin"

# Oh my zsh configuration.
export ZSH="$HOME/.oh-my-zsh"

# Use default zsh theme.
ZSH_THEME="robbyrussell"

# Mac OS and Linux specific configuration.
if [[ "$(uname)" == "Darwin" ]]; then
    site_packages="$(python3 -m site | grep lib | grep site-packages | grep homebrew | grep -v '@' | sed "s/.*\(\/opt.*\)',/\1/g")"
    python_bin=$(python3 -m site | grep USER_BASE | sed "s/.*\(\/Users.*\)'.*/\1/g")/bin
    export PATH="$PATH:$python_bin"

    # Tmux powerline setup.
    export TMUX_POWERLINE_CONF_PATH="$site_packages/powerline/bindings/tmux/powerline.conf"

    # Powerline setup.
    . "$site_packages/powerline/bindings/zsh/powerline.zsh"

    # Use gnu sed.
    export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
else
    # Powerline setup.
    . $HOME/.local/lib/python3.8/site-packages/powerline/bindings/zsh/powerline.zsh

    # Tmux powerline setup.
    export TMUX_POWERLINE_CONF_PATH="/home/andy/.local/lib/python3.8/site-packages/powerline/bindings/tmux/powerline.conf"
    powerline-config tmux setup
fi

# Zsh plugins.
plugins=(
    zsh-autosuggestions # Auto-completion prompts.
    colored-man-pages   # Colourised man pages. 
    tmux                # Basic tmux support.
    fzf-tab             # Fuzzy file finding with tab.
)

# Kubectl context display in the right-promtpt
if [[ -e $HOME/repos/zsh-kubectl-prompt/kubectl.zsh ]]; then
    autoload -U colors; colors
    source $HOME/repos/zsh-kubectl-prompt/kubectl.zsh
    f3="-" # Also include the f3 expiry time if it's in the shell environment.
    RPROMPT='%{$fg[blue]%}($ZSH_KUBECTL_PROMPT) - $(if [[ -n $F3_SESS_EXPIRY ]]; then date -u -d @$(($F3_SESS_EXPIRY-$(date +%s))) +"%T"; fi)%{$reset_color%}'
fi

# Include oh my zsh.
source $ZSH/oh-my-zsh.sh

# Disable venv prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=

# Aliases.
alias ip=ipython
alias gl='git log -n 100 --oneline --graph --all'
alias idea='intellij-idea-ultimate . &> /dev/null &!'
alias gol='goland . &> /dev/null &!'
alias pycharm='pycharm-professional . &> /dev/null &!'
alias cdf='cd ~/go/src/github.com/form3tech/'
alias cdr='cd ~/repos/andykuszyk'
if [[ "$(which kubectl)" != "kubectl not found" ]]; then
    alias k='kubectl'
    source <(kubectl completion zsh)
fi
if [[ "$(which f3)" != "f3 not found" ]]; then
    alias f='f3 auth login'
    alias fa='f3 aws cli'
    alias fp='f3 github pr'
    alias fad='f3 aws cli development -w'
fi
alias x='exit'
alias gs='git status'
alias gap='git add -p'
alias gp='git push'
alias gc='git commit'
alias gd='git diff'
alias gcm='git checkout master'
alias rn='tmux rename-window $(pwd | sed "s/.*\///g")'
complete -F __start_kubectl k

# Use private form3 libraries with Go.
export GOPRIVATE=github.com/form3tech/*

# Initialise sdkman
source "$HOME/.sdkman/bin/sdkman-init.sh"

# fzf-tab settings
zstyle ':fzf-tab:complete:*:*' fzf-preview 'bat $realpath 2> /dev/null || colorls $realpath 2> /dev/null'

# set zsh auto suggestion colour to something compatible with vim
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=7"

# Initialise gvm
source $HOME/.gvm/scripts/gvm

# Disable auto-update on every brew command
export HOMEBREW_NO_AUTO_UPDATE=1
