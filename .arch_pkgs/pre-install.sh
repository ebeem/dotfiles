# install yay if it doesn't exist (to install packages from AUR)

# enable multilib (only needed for gaming machines, steam + wine)
sudo sed -z -i 's|#\[multilib\]\n#Include = /etc/pacman.d/mirrorlist|\[multilib\]\nInclude = /etc/pacman.d/mirrorlist|g' /etc/pacman.conf

# enable coloring and parallel download
sudo sed -z -i 's|#Color|Color|g' /etc/pacman.conf
sudo sed -z -i 's|#ParallelDownloads = 5|ParallelDownloads = 15|g' /etc/pacman.conf

if ! command -v paru &> /dev/null;
then
    echo "installing paru (AUR)"
    sudo pacman -S --needed base-devel git
    cd /tmp
    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si
fi
