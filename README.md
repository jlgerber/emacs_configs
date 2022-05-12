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