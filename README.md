
# Table of Contents

1.  [Features](#orgc4aa039)
    1.  [Automatically determine the `dbus` socket's location on macOS](#org39035b9)
    2.  [Vim `ex` style command abbreviations](#org5d2ba55)
        1.  [Update ex-command filter to use multi-select](#org9413b54)
    3.  ["Hot-swapping" and version controlling `bookmark-db` files](#org85d671f):bookmarks:
        1.  [Select-bookmark-db should glob for database files](#org39443bf)
        2.  [Update bookmark-db-mv and bookmark-db-cp to use multi-select](#org2899447)
        3.  [Better system for git interaction](#org5ec590c)
        4.  [Should we use command hooks for git interaction?](#org80a4e2e)
        5.  [Allow user to specify remote and branch](#org42d2e83)
        6.  [Display git command output in minibuffer](#org3a7f173)
        7.  [Password prompts](#org24830a1)
    4.  [Use `C-[` like `ESCAPE`](#org69f3a20)
    5.  [`delete-all-buffers`](#org8e38976)
    6.  [`open-home-dir`](#org2ebda33)
2.  [`README.org` TODO-list](#orgd61c527)
    1.  [Literate style init file?](#org00eb691)

A repo for version controlling my `next-browser` init/config file(s).

For more information on `next-browser`, see:

-   <https://github.com/next-browser/next>
-   <https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org>


<a id="orgc4aa039"></a>

# Features


<a id="org39035b9"></a>

## Automatically determine the `dbus` socket's location on macOS

macOS doesn't define the env var `DBUS_SESSION_BUS_ADDRESS` on it's own, and
I have also noticed that often times `DBUS_LAUNCHD_SESSION_BUS_SOCKET` will
be pointing to the wrong location, so I query the value of the latter and
then use it to set the former when `next` starts.


<a id="org5d2ba55"></a>

## Vim `ex` style command abbreviations

In Emacs, I rely heavily on the `b` and `e` `ex` commands for swapping
buffers and opening files. Here, they are aliases for `switch-buffer` and
`set-url-new-buffer`. The `ex-style` aliases live in their own command list
though, and are thus not callable from `execute-command` prompt (i.e. they
are invisible to the `M-x` prompt). To access them, call
`execute-command-or-ex` (accessible via `:`, like in Vi or `evi-mode`).

The `execute-command-or-ex` prompt should allow you to access all the
*non*-ex-style commands as well (just like `execute-command`). It also
implements a very primitive/hackish/experimental attempt at passing
tab-completable arguments to the ex-style commands. For example, after
entering `b SPACE`, the prompt should bring up the same completion list as
`switch-buffer`.


<a id="org9413b54"></a>

### TODO Update ex-command filter to use multi-select


<a id="org85d671f"></a>

## "Hot-swapping" and version controlling `bookmark-db` files     :bookmarks:

The command `select-bookmark-db` allows you to change `bookmark-db-path`
(i.e. the "active" bookmark database file) on the fly, via a minibuffer
prompt implementing `file-manager-mode`. The selected file will be created if
it doesn't already exist. If a `.git` directory is found in the directory
housing the selected file, the command `git add <bookmark-db>` is called,
followed by steps (1) and (2) of `bookmark-db-push`.

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

The commands `bookmark-db-cp` and `bookmark-db-mv` can be used to duplicate
and move bookmarks (resp.) between your different database files. They will
first prompt the user for an entry in the currently active bookmark
database, then for a path to the destination database. The state of the
bookmark repo is committed at the start of the command (before any changes
have occurred), and then upon command completion. Like `select-bookmark-db`,
the destination database will be created and added to the repo if it does
not already exist.


<a id="org39443bf"></a>

### TODO Select-bookmark-db should glob for database files


<a id="org2899447"></a>

### TODO Update bookmark-db-mv and bookmark-db-cp to use multi-select


<a id="org5ec590c"></a>

### TODO Better system for git interaction

-   I can't help but feel that the current system is a little excessive with
    commit frequency.


<a id="org80a4e2e"></a>

### TODO Should we use command hooks for git interaction?

-   It may be elegant to call the start/end repo updates in the entry/exit
    command hooks (e.g. for `bookmark-db-mv` and `bookmark-db-cp`). One
    possible downside though, is that since the git interaction is not coded
    explicitly in the function body, it may become more challenging to
    understand what is going on if these things get more complicated (and I
    tend to be stupid so&#x2026;)


<a id="org42d2e83"></a>

### TODO Allow user to specify remote and branch


<a id="org3a7f173"></a>

### TODO Display git command output in minibuffer


<a id="org24830a1"></a>

### TODO Password prompts


<a id="org69f3a20"></a>

## Use `C-[` like `ESCAPE`

The chord `C-[` is set to spoof an `ESCAPE` keypress to the browser core, so
you can use it for all the same things (e.g. going back to `vi-normal` mode,
or closing the minibuffer prompt).


<a id="org8e38976"></a>

## `delete-all-buffers`

The command `delete-all-buffers` will delete ALL buffers except for the
currently active one.


<a id="org2ebda33"></a>

## `open-home-dir`

The current file manager implementation felt a little un-intuitive and clunky
to me, so when I need to open a local `html` file, I often just start by
calling `open-home-dir`, and then link-hint my way to where I need to be.


<a id="orgd61c527"></a>

# `README.org` TODO-list


<a id="org00eb691"></a>

## TODO Literate style init file?

Vindarel's *literate style* init file using `erudite` is really damned
slick. Should we do the same thing?

