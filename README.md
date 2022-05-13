# Readme

## Run emacs in Terminal Mode

```
emacs -nw
```

# File Tree
You can add a file tree to the left hand side using a community package. However there are better ways of dealing with this

# Buffer
- an object in emacs that holds text and information
- it may or may not be displayed in a window
- There are a number of special buffers
  - *scratch* - like a blank sheet of paper
  - *Messages*
  - *Warnings*

# You can have a tab per buffer
global-tab-line-mode

# Buffers
## Switching Buffers
You can switch between buffers
```
C-x b
```
## Killing buffers
```
C-x k
```
You can kill multiples with
```
M-x kill-some-buffers
```

# Window and Frames

## Window
- A window is like a "pane". It always displays a buffer. It may be split.
- A window displays a buffer

## Creating Multiple windows
- To split vertically
```
C-x 2
```
## Moving between windows
- move to the "other" window
```
C-x o
```

- to find a file in another window
```
C-x 4 f
```
## Deleting Windows
- to delete the window that you are in
```
C-x 0
```

- to delete all other windows
```
C-x 1
```

## Frame
Frames each have their own toolbars and menus.
Most folks dont use multiple frames. You will probably used multiple window


# Key Bindings
You will of ten see people write out keybindings in a very specific format.
You have some sort of special key followed by a dash followed by a character.
Eg C-s or M-x

## Special Characters
- C - Ctrl
- M - Alt (meta in emacs lingo)
- S - Shift
- s - Super (windows key)

## Main key prefixes
- C-x - This is a prefix for of Emacs' primary key bindings
- C-c - this is considered to be a combination of bindings created by active major and minor modes or by the user

# Open and Save
- ```C-x C-f``` - find file command
- ```C-x C-s``` - save the current buffer
- ```C-x C-w``` - save buffer to a different file (save as)

# Switching Buffers
- ```C-x b```   - switch buffer
- ```C-x C-b``` - open a buffer list in its own window 
- ```C-x <-```  - cycle previous buffer
- ```C-x ->```  - cycle to next buffer

# Cutting and Copying Text
In emacs to "kill" text means to "cut" it.
- ```C-w``` - copy selection to kill ring and delete text
- ```C-y``` - yank (paste) the most recent item in the kill ring

## CUA Mode
if you must have the old C-c, C-x C-v copy cut paste behavior, you can turn it on . If you go to the options menu in emacs, you can click the use CUA keys. Or you can use ```M-x cua-mode```

$ Undo and Redo
- ```C-_``` or ```C-/```
- in evil mode redo is C-M-_ (C-M-S--) ie control alt shift dash

# Cancel an operation
- C-g quits (esc the way I have evil mode set up)

# Learning more Key Bindings
- RUn commmand describe-bindings
- Run the command describe-key (C-h k)

# The Help System
- The whole emacs manual is built in to emacs and you can find help on anything.
- If you go to the help menu, you have access to everything

# Configuring Emacs
- Customization UI
  - shows you options for emacs plus any installed packages ```M-x customize```
