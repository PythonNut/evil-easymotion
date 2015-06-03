evil-easymotion
===============
[![MELPA](http://melpa.org/packages/evil-easymotion-badge.svg)](http://melpa.org/#/evil-easymotion)

In which we give [Lokaltog/**vim-easymotion**](https://github.com/Lokaltog/vim-easymotion) a streak of malevolence.

**Obligatory screencast**

![screencast](img/evil-easymotion-demo.gif)

Notice
======

`evil-easymotion` has recently replaced its [`ace-jump-mode`](https://github.com/winterTTr/ace-jump-mode) backend with [`avy`](https://github.com/abo-abo/avy). Avy has several advantages, including:

1. Multiple character target display
2. More customization options
3. A cleaner codebase
4. More sophisticated algorithms (coming!)

(1) is of course, the most important because it paves the way for implementing the features of EasyMotion 2.0. I hope the transition hasn't been too rough. If you encounter any problems, please tell.

Usage
=====
Currently most motions are supported, and it's easy to define your own easymotions.

```emacs
(evilem-define (kbd "SPC w") 'evil-forward-word-begin)
```
To define easymotions for all motions that evil defines by default, add
```emacs
(evilem-default-keybindings "SPC")
```
This binds all motions under the prefix `SPC` in `evil-motion-state-map`. This is not done by default for motions defined manually. You will need to supply the prefix.

If you use [`evil-leader`](https://github.com/cofi/evil-leader), then you can bind motions like this:

```emacs
(evil-leader/set-key
  "j" (evilem-create 'next-line))
```
_(Note that the actual binding is more complex, as it handles things like the Emacs `goal-column` and such. See the code for details)_

If you want to access the motions in other evil states, you can easily use a redirection keybind
```emacs
(define-key evil-insert-state-map (kbd "M-SPC") (lookup-key evil-motion-state-map (kbd "SPC")))
```

Contributing
============
I _invite_ you to open issues about whatever you find lacking in `evil-easymotion`! I will try to get to you in a timely fasion (probably only good until 2016).

Pull requests are welcome as well!

Credits
=======
I'm deeply indebted to:
* [abo-abo](https://github.com/abo-abo) for authoring [`avy`](https://github.com/abo-abo/avy), on which this package depends.
* [Lokaltog](https://github.com/Lokaltog) for authoring [`vim-easymotion`](https://github.com/Lokaltog/vim-easymotion) and creating the paradigm which this package emulates.
