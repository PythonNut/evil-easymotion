evil-easymotion
===============
[![MELPA](http://melpa.org/packages/evil-easymotion-badge.svg)](http://melpa.org/#/evil-easymotion)

In which we give easymotion a streak of malevolence. The goal is to behave approximately like [Lokaltog/**vim-easymotion**](https://github.com/Lokaltog/vim-easymotion).

**Obligatory screencast**

![screencast](img/evil-easymotion-demo.gif)

Usage
=====
Currently most motions are supported, and it's easy to define your own easymotions.

```emacs
(evilem-define (kbd "SPC w") evil-forward-word-begin)
```
To define easymotions for all motions that evil defines by default, add
```emacs
(evilem-default-keybindings "SPC")
```
This binds all motions under the prefix `SPC` in `evil-motion-state-map`. This is not done by default for motions defined manually. You will need to supply the prefix.

Not all motions can be made easy (`evil-goto-line` for example).

Credits
=======
I'm deeply indebted to [abo-abo](https://github.com/abo-abo/) for authoring [abo-abo/**ace-link**](https://github.com/abo-abo/ace-link/), which inspired this package. In particular, the function `ali-generic` which allows `ace-jump-mode` to jump to arbitrary points.
