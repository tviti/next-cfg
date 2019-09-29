
# Table of Contents

1.  [next-cfg](#org6013f1c):next_browser:
    1.  [Author: Taylor Viti](#orgb02c785)
    2.  [Features](#org35aaf3c)
        1.  [Automatically determine the `dbus` socket's location on macOS](#org5d2291a)
        2.  [Make buffer deletion prompt more consistent w/ Emacs](#org60d8ba4)
        3.  [`delete-all-buffers`](#org547bab4)
        4.  [`open-home-dir`](#org738310e)
        5.  [Vim `ex` style command abbreviations](#org5cd2110)
        6.  [Use `C-[` like `ESCAPE`](#orgb732959)
        7.  ["Hot-swapping" and version controlling `bookmark-db` files](#orgf426508):bookmarks:
    3.  [`README.org` TODO-list](#org2bd06ad)
        1.  [Literate style init file?](#orga1b9f35)


<a id="org6013f1c"></a>

# next-cfg     :next_browser:


<a id="orgb02c785"></a>

## Author: Taylor Viti

A repo for version controlling my `next-browser` init/config file(s).

For more information on `next-browser`, see:

-   <https://github.com/next-browser/next>
-   <https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org>


<a id="org35aaf3c"></a>

## Features


<a id="org5d2291a"></a>

### Automatically determine the `dbus` socket's location on macOS

macOS doesn't define the env var `DBUS_SESSION_BUS_ADDRESS` on it's own, and
I have also noticed that often times `DBUS_LAUNCHD_SESSION_BUS_SOCKET` will
be pointing to the wrong location, so I query the value of the latter and
then use it to set the former when `next` starts


<a id="org60d8ba4"></a>

### Make buffer deletion prompt more consistent w/ Emacs

The default behavior of `C-x k` in Emacs (at least with `evil-mode` active)
is to query the user for a buffer to delete, with the default being the
active buffer, while in `next`, the completion function for the
`delete-buffer` command explicitly selects a non-active buffer as the default
for deletion. Here, I use a modified completion function that retains the
Emacs behavior (the command implementing this is un-creatively termed
`my-delete-buffer`).


<a id="org547bab4"></a>

### `delete-all-buffers`

The command `delete-all-buffers` will delete ALL buffers except for the
currently active one.


<a id="org738310e"></a>

### `open-home-dir`

The current file manager implementation felt a little un-intuitive and clunky
to me, so when I need to open a local `html` file, I often just start by
calling `open-home-dir`, and then link-hint my way to where I need to be.


<a id="org5cd2110"></a>

### Vim `ex` style command abbreviations

In Emacs, I rely heavily on the `b` and `e` `ex` commands for swapping
buffers and opening files. Here, they are aliases for `switch-buffer` and
`set-url-new-buffer`. Of course, unlike in Vim and Emacs, they don't
actually take args in `next`.

1.  TODO Better implementation of `def-cmd-alias`

    I'm guessing that my method of implementing the aliases is probably a
    dirty hack, so I should redo that at some point.


<a id="orgb732959"></a>

### Use `C-[` like `ESCAPE`

This one isn't an *actual* alias for `ESCAPE`, but will do the same thing in
`vi-normal-mode`, `vi-insert-mode`, and `minibuffer-mode`.


<a id="orgf426508"></a>

### "Hot-swapping" and version controlling `bookmark-db` files     :bookmarks:

The command `select-bookmark-db` allows you to change `bookmark-db-path`
(i.e. the "active" bookmark database file) on the fly, via a minibuffer
prompt implementing `file-manager-mode`. The selected file will be created if
it doesn't already exist. If a `.git` directory is found in the directory
housing the selected file, the command `git add <bookmark-db>` is called.

The command `bookmark-db-push` will call the following commands in sequence:

1.  `git ... add --update`
2.  `git ... commit -m "bookmark-db-push"`
3.  `git ... push origin master`

where `...` denotes the flags `--git-dir=<db-dir>.git --work-tree=<db-dir>`.
The command `bookmark-db-pull` will perform the same sequence, but with the
commit message of (2) as `bookmark-db-pull`, and (3) as a `pull` instead.
Both commands will print a warning to the repl (**not** the minibuffer) if no
`.git` folder is detected in the bookmark-db folder.

These commands will all assume that `origin` is setup to be accessed over
ssh, and that the requisite ssh-key has already been added to the ssh-agent
(i.e. they are very primitive, and rely on the assumption that you will not
be prompted for a username/password at any point when calling `git`).

1.  TODO Allow user to specify remote and branch

2.  TODO Display git command output in minibuffer

3.  TODO Password prompts

4.  TODO Select-bookmark-db should glob for .db files

5.  TODO Add a cmd to move/copy bookmarks between .db files


<a id="org2bd06ad"></a>

## `README.org` TODO-list


<a id="orga1b9f35"></a>

### TODO Literate style init file?

Vindarel's *literate style* init file using `erudite` is really damned
slick. Should we do the same thing?

