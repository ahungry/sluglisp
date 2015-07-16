stump-touchy-mode-line
======================

Why it should be hard to use STUMPWM on tablets?

This package makes it easy to define "buttons" in the mode-line:
when certain area on a mode line is clicked, certain function gets called.

Of course, main power of this lies in what functions you exactly bind to mode-line buttons.
Relevant snippet of my .stumpwmrc looks like

```lisp
(ql:quickload 'stump-touchy-mode-line)

(setf *space-between-buttons* 0.25) ; there should be one quarter of button's length between buttons
(setf *min-button-length* 20) ; and minimum button length should be 20 pixels
(set-touchy-mode-line pull-hidden-next ; circle through windows in a group
		      bring-lx-terminal ; bring lxterminal to front, create one, if it does not exist
		      circle-through-splits ; circles through layouts I commonly use
                                            ; one frame, two vertical frames, two horizontal frames
		      fnext ; circle through frames on a screen
		      nil ; empty space
		      nil ; empty space
		      toggle-virt-keyboard-mode ; force two horizontal frames, bring cellwriter (virtual keyboard)
                                                ; to the lower frame
		      gnext ; circle through groups
                      )
```

Essentially, functions BRING-LX-TERMINAL and TOGGLE-VIRT-KEYBOARD-MODE already
allow further customizations using only touchscreen, everything else is just a convenience.


