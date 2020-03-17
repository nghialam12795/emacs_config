<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>
<p align="center"><a href="https://www.gnu.org/software/emacs/"><b>GNU Emacs</b></a></p>
<p align="center">
	<a href="https://unlicense.org/"><img src="https://img.shields.io/badge/License-UNLICENSE-blue"/></a>
	<a href=""><img src="https://img.shields.io/badge/Maintain%3F-Forever-brightgreen"/></a>
	<a href="https://www.gnu.org/software/emacs/"><img src="https://img.shields.io/badge/Emacs-26.3-blueviolet"/></a>
</p>

This Emacs configuration is a collection of various Emacs Veteran with the aim of being a beautiful, simple & powerful IDEs.
---
<p align="center">This repository contains all my GNU Emacs configuration.</p>


### Welcome!

This my Emacs Configuration for my daily tasks. Please keep in mind that this configuration may not be suitable for some person.
The package settings are grouped in a logical manner, and I've documented as detailed as possible what each code snippet does in my `config.org` file.

If you are totally new to Emacs. I'd recommend to use [doom-emacs](https://github.com/hlissner/doom-emacs) instead of mine.

Then, I advise you to take code blocks from the [`config.org`](https://github.com/nghialam12795/emacs_config/blob/master/config.org)
file for your own configuration file, and evaluate them with `eval-last-sepx` (`C-x C-e`). Be careful to take pairs of parentheses.

### Installation
Back up your `~/.emacs.d/` first (if you have one):

```
mv ~/.emacs.d/ ~/.emacs.d.bak/
```

Git clone my configuration to your new `~/.emacs.d/` :

```
# For Emacs 26 and below
git clone https://github.com/nghialam12795/emacs_config.git ~/.emacs.d

# For Emacs 27
git clone https://github.com/nghialam12795/emacs_config.git ~/.config/emacs/
```

### "Rolling" Release
I will constantly push new commits as soon as I discover new things suitable for this configuration.

### How the config structure works

The `init.el` requires `config.el`, which is produced by org-babel and `config.org`. This allows me to put most of my configuration in an Org file with literate programming style (good for documenting code usage!).
