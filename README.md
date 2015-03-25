evil-easymotion
===============
[![MELPA](http://melpa.org/packages/evil-easymotion-badge.svg)](http://melpa.org/#/evil-easymotion)

In which we give [Lokaltog/**vim-easymotion**](https://github.com/Lokaltog/vim-easymotion) a streak of malevolence.

**Obligatory screencast**

![screencast](img/evil-easymotion-demo.gif)

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

Not all motions can be made easy (`evil-goto-line` for example).

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
* [abo-abo](https://github.com/abo-abo/) for authoring [`ace-link`](https://github.com/abo-abo/ace-link/), which inspired this package. In particular, the function `ali-generic` which allows `ace-jump-mode` to jump to arbitrary points.
* [winterTTR](https://github.com/winterTTr) for authoring [`ace-jump-mode`](https://github.com/winterTTr/ace-jump-mode), on which this package depends.
* [Lokaltog](https://github.com/Lokaltog) for authoring [`vim-easymotion`](https://github.com/Lokaltog/vim-easymotion) and creating the paradigm which this package emulates.
