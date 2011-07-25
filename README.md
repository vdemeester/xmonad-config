# My xmonad config

XMonad is my window manager of choice, and this is the git repository where my
configurations are stored.

## Basic information

My XMonad configuration highlights are the following :

* Keep default keymapping for actions (except I use the `Super` key as modifier)
  
  As I'm most of the time on a [bepo](http://bepo.fr), I'm remapping almost all
  the keys to get the same behavior on a bepo keyboard and on a qwerty keyboard.

* The status bar is powered by dzen2 and conky(-cli).
* Notifications are using dzen2 too
* Heavily use of Prompt and ScratchPad (using a customized version of the 
  ScratchPadKeys module from pbrisbin)
* Named-based workspace (1-media, 2-chat, …)
* Per-workspace layout and more default layout than stock ones

Dzen2 (with conky) status bar is launched when xmonad start and will be killed
and re-launched if xmonad is re-launched.

### Modules

Modules that lies in `./lib`

* From [pbrisbin](https://github.com/pbrisbin/xmonad-config)
  * Dzen : Easier dzen definitions
  * ScratchPadKeys : Importable scratchpads ()
* Utils : few useful things

## Dependencies

* ghc 6 or 7
* xmonad 0.9.2
* xmonad-contrib 0.9.2

* dzen2
* conky(-cli)
* dmenu (suckless-tools on debian)

I'm using [debian](http://debian.org) testing with the haskell package from
unstable (which are quite stable). This configuration should works on any other
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
