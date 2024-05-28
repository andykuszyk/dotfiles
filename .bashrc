# Add executable locations to PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:/usr/local/bin:/home/andy/go/bin"
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:/home/andy/.tfenv/bin"
export PATH="$PATH:$HOME/node-v16.13.1-linux-x64/bin"
export PATH="$PATH:/opt/homebrew/lib/ruby/gems/3.0.0/bin"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/go/bin"

# Mac OS and Linux specific configuration.
if [[ "$(uname)" == "Darwin" ]]; then
    # Use gnu sed.
    export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
fi

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
alias ll='ls -la'

# Use Emacs as default editor
export EDITOR=emacsclient

# kubectl command completion
if $(which kubectl > /dev/null); then
    alias k='kubectl'
    source <(kubectl completion bash)
    complete -F __start_kubectl k
fi

# AWS CLI command completion
if $(which aws > /dev/null); then
    complete -C $(which aws_completer) aws
fi

# Initialise sdkman
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Initialise gvm
if [[ -a $HOME/.gvm/scripts/gvm ]]; then
    source $HOME/.gvm/scripts/gvm
fi

# Disable auto-update on every brew command
export HOMEBREW_NO_AUTO_UPDATE=1

# Prompt customisation
export PS1='$ '
