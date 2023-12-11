# Add executable locations to PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:/usr/local/bin:/home/andy/go/bin"
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:/home/andy/.tfenv/bin"
export PATH="$PATH:$HOME/node-v16.13.1-linux-x64/bin"
export PATH="$PATH:/opt/homebrew/lib/ruby/gems/3.0.0/bin"
export PATH="$PATH:/opt/homebrew/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/go/bin"

# Oh my zsh configuration.
export ZSH="$HOME/.oh-my-zsh"

# Use default zsh theme.
ZSH_THEME="robbyrussell"

# Mac OS and Linux specific configuration.
if [[ "$(uname)" == "Darwin" ]]; then
    # Use gnu sed.
    export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
fi

# Zsh plugins.
plugins=(
    zsh-autosuggestions # Auto-completion prompts.
    colored-man-pages   # Colourised man pages. 
    fzf-tab             # Fuzzy file finding with tab.
)

# Include oh my zsh.
source $ZSH/oh-my-zsh.sh

# Disable venv prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=

# Aliases.
alias ip=ipython
alias gl='git log -n 100 --oneline --graph --all'
alias cdr='cd ~/repos/andykuszyk'
alias x='exit'
alias gs='git status'
alias gap='git add -p'
alias gp='git push'
alias gc='git commit'
alias gd='git diff'
alias gcm='git checkout master'
alias e="emacsclient -n"
complete -F __start_kubectl k

# Use Emacs as default editor
export EDITOR=emacsclient

# kubectl command completion
if which kubectl; then
    alias k='kubectl'
    source <(kubectl completion zsh)
fi

# AWS CLI command completion
if which aws; then
    autoload bashcompinit && bashcompinit
    autoload -Uz compinit && compinit
    complete -C '/usr/local/bin/aws_completer' aws
fi

# Initialise sdkman
source "$HOME/.sdkman/bin/sdkman-init.sh"

# set zsh auto suggestion colour to something compatible with emacs
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=241"

# Initialise gvm
if [[ -a $HOME/.gvm/scripts/gvm ]]; then
    source $HOME/.gvm/scripts/gvm
fi

# Disable auto-update on every brew command
export HOMEBREW_NO_AUTO_UPDATE=1
