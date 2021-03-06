# swivb - Switch to buffer like ivy

## Description

This package extract the `ivy-virtual-buffer` feature to its own so you don't
depend on the whole ivy for completion.

If you have projectile project opened it will show them as well.

It add as well an optional "function mode", where you can advanced switching
configuration to switch to a buffer or launch a function.

Simply launch the interactive function swivb and it will show you the current
buffer, the recent list files from `recentf` and the `bookmarks`.

## Config

You can configure the variable `swivb-actions` to have special actions, for
example :

```lisp
(setq swivb-actions '((:name "*Gnus*" :function "gnus" :buffer "*Group*")))
```

This will add an item called `*Gnus*` and launch the function `gnus` if you choose
it, unless the buffer `*Group*` already exist it will then switch to it.

Additionally you can override swbuf switch or find-file function in argument,
for example :

```lisp
(swivb 'find-file-other-tab 'swivb-switch-to-buffer-or-focus-tab)
```

The `swivb-switch-to-buffer-or-focus-tab` function will focus a tab if the
buffer name exist or `switch-to-bufer` it.

## License

[MIT](./LICENSE)
