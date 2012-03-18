                                              __                             ___ __        
    .--.--..--------..-----..-----..---.-..--|  |______.----..-----..-----..'  _|__|.-----.
    |_   _||        ||  _  ||     ||  _  ||  _  |______|  __||  _  ||     ||   _|  ||  _  |
    |__.__||__|__|__||_____||__|__||___._||_____|      |____||_____||__|__||__| |__||___  |
                                                                                    |_____|

XMonad is my window manager of choice, and this is the git repository where my
configurations are stored.

> xmonad is a dynamically tiling X11 window manager that is written and configured 
> in Haskell. In a normal WM, you spend half your time aligning and searching for 
> windows. xmonad makes work easier, by automating this. 
> <small>[Xmonad](http://xmonad.org)</small>

## Basic information

My XMonad configuration highlights are the following :

* Using `Super` key as modifier, instead of the default `Alt`
  
  As I'm most of the time on a [bepo](http://bepo.fr), I'm remapping almost all
  the keys to get the same behavior on a bepo keyboard and on a qwerty keyboard.

* The status bar is powered by xmobar
* Heavily use of Prompt and ScratchPad (using a customized version of the 
  ScratchPadKeys module from pbrisbin)
* Topic usage (replacement of workspace)
  Lots of topics, and most of them with a custom layout (and default application)
* Per-workspace layout and more default layout than stock ones

### Modules

Modules that lies in `./lib`

* From [pbrisbin](https://github.com/pbrisbin/xmonad-config)
  * Dzen : Easier dzen definitions
  * ScratchPadKeys : Importable scratchpads ()
* Utils : few useful things

## Dependencies

* ghc 6 or 7
* xmonad 0.10
* xmonad-contrib 0.10
* xmobar with `--flags="with_xft"`

* dzen2
* conky(-cli)
* dmenu (suckless-tools on debian)

I'm using [debian](http://debian.org) testing with the haskell package from
unstable (which are quite stable). I am using `cabal-install` on debian stable
to get the last version on ghc 6. This configuration should works on any other
Linux distribution (or even *BSD* and other unixes).

## Try it

Backup your original configuration

    mv $HOME/.xmonad $HOME/.xmonad.bak

Clone the repository

    git clone git://github.com/vdemeester/xmonad-config.git $HOME/.xmonad

Verify everything compiles before actually restarting:

    cd ~/.xmonad && ghci -ilib xmonad.hs

This will notify you of any problems.

Press `Ctrl-d` to exit ghci, then `M-q` to restart xmonad.
