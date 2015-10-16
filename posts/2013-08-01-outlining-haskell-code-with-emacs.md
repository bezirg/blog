---
title: Outlining Haskell Code with Emacs
author: Nikolaos Bezirgiannis
tags: haskell, emacs, ide 
---

There are wonderful tools right now for coding Haskell in Emacs,
such as the de-facto [haskell-mode](https://github.com/haskell/haskell-mode) and
the excellent [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/),
that easily enables [Emacs Flymake](http://www.emacswiki.org/emacs/FlyMake) for Haskell. 
I am not going to show you how to setup and configure these packages, as I am sure
there [are](http://www.haskell.org/haskellwiki/Emacs) [many](http://emacsclub.github.io/html/haskell.html) [places](http://chrisdone.com/posts/haskell-emacs) in the net you can look for this. 

In my earlier days of programming, where I mostly had to deal with Python and Java code,
I found [Hideshow](http://www.emacswiki.org/emacs/HideShow) 
to be an excellent companion when coding on these languages in Emacs. Hideshow can
automatically hide blocks of code and on demand show it to the user whenever she wants. An example in
Python will convince you:


~~~ {.python}
class Bird(object):
    def __init__(self, **opts):
    def sing(self):
        song = make_random_pattern()
        mouth = self.open_mouth()
        self.write(mouth, song)
    def open_mouth(self):
    def write(self, m, s):

def create_flock():
~~~

Now we hide the irrelevant code for the moment and focus on our point-of-interest, which is the `sing` method.
It would be great if we could do the same for Haskell code! We can enable Hideshow for Haskell by putting
this into our emacs startup file (usually `~/.emacs`):

~~~ {.lisp}
(add-hook 'haskell-mode-hook (lambda () 
                                (hs-minor-mode)
                                ;(hideshowvis-enable) ;; uncomment this if u have hideshowvis
                             ))
~~~

Fortunately Hideshow comes bundled with Emacs nowadays, so you don't have to install something.
If you are after a better visual representation of the folded code, you should install
an enhancement of Hideshow, that is [hideshow-vis](http://www.emacswiki.org/emacs/hideshowvis.el).

After enabling it we get by default folded code for Haskell records, as in this example.

Before folding:

~~~ {.haskell}
data State = State {
    _runway_isfree :: Bool
  , _on_the_ground :: Int
  , _in_the_air :: Int
  , _total_flies :: Int
  , _seed :: Int
  } deriving (Eq, Show)
~~~

After folding:

~~~ {.haskell}
data State = State {(7)..} deriving (Eq, Show)
~~~

which is kind of nice, but not that big a difference. 

We could do better by utilizing another package of Emacs, which is called [OutlineMode](http://www.emacswiki.org/emacs/OutlineMode).
OutlineMode folds multiple levels of text, which can come pretty handy, for example when dealing
with Markdown and its headers. Here, we are gonna exploit OutlineMode for Haskell code,
and fold blocks belonging to `do`, `mdo` and `where` clauses. The intuition is this:

Before folding:

~~~ {.haskell}
main = do
    print "hello"
    do
        print f
    return ()

f = x
    where
    x = "world"

~~~ {.haskell}

After folding:

~~~ {.haskell}
main = do...

f = print x
    where...
~~~    

Don't mind the nonsense code. The good thing is that Outline can deal with nested blocks of `do`'s and `where`'s. On the example
above we can cycle the folding of the `main` function and get another view of the code:


~~~ {.haskell}
main = do
    print "hello"
    do...
    return ()
~~~


## Configuring Emacs Outline for Haskell

First get the nice `outline-magic` package from [here](https://github.com/tj64/outline-magic/blob/master/outline-magic.el).
This will let you use only one keybinding to cycle between foldings.

Then, enable it and bind it to Ctrl-Tab with:

~~~ {.lisp}
(eval-after-load 'outline
  '(progn
    (require 'outline-magic)
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))
~~~

Next, put this piece of code to your emacs configuration:

~~~ {.lisp}
(defun haskell-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "    \n")
      (current-column))))

(add-hook 'haskell-mode-hook (lambda ()
                               (outline-minor-mode)
                               (set (make-local-variable 'outline-regexp)
                                    (rx (* anything) (or "do" "mdo" "where")
                                        symbol-end))
                               (set (make-local-variable 'outline-level) #'haskell-outline-level)
                               ))
~~~

Happy Haskell coding!
