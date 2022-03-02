full_path=$(realpath $0)
dir_path=$(dirname $full_path)
root_path=$(dirname $dir_path)

# enable and start services (if they are not enabled already)
# enable dkb-next (keyboard rgb)
# $1 is the name of the service
enable_system_service() {
    if [ $(systemctl is-enabled $1) != 'enabled' ]
    then
        systemctl enable --now $1
    fi
}

enable_user_service() {
    if [ $(systemctl --user is-enabled $1) != 'enabled' ]
    then
        systemctl --user enable --now $1
    fi
}

enable_system_service "ckb-next-daemon.service"
enable_system_service "NetworkManager.service"
enable_system_service "bluetooth.service"
enable_user_service "mpd.service"

# install doom emacs if doesn't exist
if [ ! -e ~/.emacs.d/bin/doom ]
then
    echo "installing doom-emacs"
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
fi

# overwrite gnome settings
dconf load /org/gnome/ < $root_path/.gnome
if [ $SHELL != $(which zsh) ]
then
    chsh -s $(which zsh)
fi

cd $root_path
stow ./
