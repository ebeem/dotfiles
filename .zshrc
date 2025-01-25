# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# $1 is the name of the plugin
# $2 is the github url
install_plugin() {
    if [ ! -e $ZSH"custom/plugins/$1" ]
    then
      echo "cloning $2 into $ZSH""custom/plugins/$1"
      sudo git clone $2 $ZSH"custom/plugins/$1"
    fi
}

install_plugin "zsh-autosuggestions" "https://github.com/zsh-users/zsh-autosuggestions"
install_plugin "zsh-completions" "https://github.com/zsh-users/zsh-completions"
install_plugin "zsh-syntax-highlighting" "https://github.com/zsh-users/zsh-syntax-highlighting"

export plugins=(
  zsh-autosuggestions
  zsh-completions
  zsh-syntax-highlighting
  git
  catimg
  command-not-found
  extract
  python
)

ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/emacs
#export ANDROID_HOME=/mnt/storage/android
#export ANDROID_SDK_ROOT=$ANDROID_HOME
#export PATH=/home/ebeem/workspace/git/flutter/bin::$PATH
export PATH=home/ebeem/.dotnet/tools:$PATH
export PATH=/opt/android-sdk/tools/bin:$PATH
export PATH=~/.emacs.d/bin:~/.bin:$PATH
export PATH=~/.config/emacs/bin:~/.bin:$PATH
export PATH=~/.dotnet/tools:$PATH
export STEAM_COMPAT_CLIENT_INSTALL_PATH=/home/ebeem/.steam
export XDG_CONFIG_HOME=~/.config
source $ZSH/oh-my-zsh.sh
export QT_QPA_PLATFORMTHEME="qt5ct"
export GUILE_LOAD_PATH=/usr/local/share/guile/site/3.0

alias em="/usr/bin/emacsclient -nw"
neofetch

autoload -Uz vcs_info # enable vcs_info
precmd () { vcs_info } # always load before displaying the prompt
zstyle ':vcs_info:*' formats ' %s(%F{green}%b%f)' # git(main)
PS1='
%n@%m %F{blue}%/%f$vcs_info_msg_0_
$ '

source $HOME/.aliases
# Created by `pipx` on 2024-04-20 09:45:54
export PATH="$PATH:/home/ebeem/.local/bin"
