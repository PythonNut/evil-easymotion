evil-easymotion
===============
[![MELPA](http://melpa.org/packages/evil-easymotion-badge.svg)](http://melpa.org/#/evil-easymotion)

In which we give [Lokaltog/**vim-easymotion**](https://github.com/Lokaltog/vim-easymotion) a streak of malevolence.

Suppose you're a proud user of `evil`, but you have a nasty habit: when you want to move ten lines down, you spam <kbd>j</kbd> ten times instead of using the ergonomically superior <kbd>10</kbd><kbd>j</kbd>. With `evil-easymotion` you can invoke <kbd>SPC</kbd><kbd>j</kbd>, and this plugin will put a target character on every possible position. Type the character on the target and **_wham_**! you have teleported there.

**Obligatory screencast**

![screencast](img/evil-easymotion-demo.gif)

Currently most motions are supported, and it's easy to define your own easymotions. Easymotions can also be composed with operators e.g. <kbd>d</kbd><kbd>SPC</kbd><kbd>j</kbd> to delete a bunch of lines.

Help Wanted
===========

I've been struggling with a strange [error](https://github.com/PythonNut/evil-easymotion/issues/19) that I can't seem to reliably reproduce. I don't know what causes it. If you can help (even by just finding a reliable way to reproduce), that would be awesome.

Usage
=====
To define easymotions for all motions that evil defines by default, add
```emacs
(evilem-default-keybindings "SPC")
```
This binds all motions under the prefix `SPC` in `evil-motion-state-map`. This is not done by default for motions defined manually. You will need to supply the prefix.

Or, if you prefer more granular control:
```emacs
(evilem-define (kbd "SPC w") 'evil-forward-word-begin)
```

You can always drop by [the wiki](https://github.com/PythonNut/evil-easymotion/wiki) for more tips.

Contributing
============
I _invite_ you to open issues about whatever you find lacking in `evil-easymotion`! I will try to get to you in a timely fasion (probably only good until 2016).

Pull requests are welcome as well!

Credits
=======
I'm deeply indebted to:
* [abo-abo](https://github.com/abo-abo) for authoring [`avy`](https://github.com/abo-abo/avy), on which this package depends.
* [Lokaltog](https://github.com/Lokaltog) for authoring [`vim-easymotion`](https://github.com/Lokaltog/vim-easymotion) and creating the paradigm which this package emulates.
