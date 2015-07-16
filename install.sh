sudo apt-get install xmonad suckless-tools xmobar feh git
cd ~/
git clone https://github.com/RichardH92/xmonad-config.git
mv ~/xmonad-config ~/.xmonad
cp ~/.xmonad/.xmobarrc ~/.xmobarrc
chmod +x ~/.xinitrc
ln -s ~/.xinitrc ~/.xsession