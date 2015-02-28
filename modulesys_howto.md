# Module system usage

This document explains module system usage. For a complete documentation read
[Modules](https://github.com/vasil-sd/shen-libs/wiki/Modules) wiki page.

## How to setup module system

The simplest way to get module system sources is to clone shen-libs repo into
some directory, say `shen-libs`:

    git clone https://github.com/vasil-sd/shen-libs.git shen-libs

So in `shen-libs` directory you will see `modulesys.shen` and directories of
libraries.

You may want to store your own libraries in separate directory, e.g.
`my-libs`. Create that directory.

Then write into your Shen init file (if you have one):

    (load "$shen-libs/modulesys.shen")
    (module.add "$my-libs")
    (module.add "$shen-libs")

Where `$shen-libs` is a full path of your `shen-libs` directory and `$my-libs`
is a full path of your `my-lib`. First line loads module system. Second and
third lines set a list of paths where module system will search modules.

## How to use a module

When `modulesys.shen` is loaded and `*modules-paths*` appropriately set then
to load modules `mod1` and `mod2` you need to type

    (module.use ["mod1" "mod2"])

or 

    (use-modules ["mod1" "mod2"])

If in development process you modified `mod2` module then it won't be reloaded
by `module.use`. In that case you have to use

    (module.reload mod2)

## How to create a module

You have implemented something and want to make it a module.

First, give it a name. In order to avoid name clashes look over modules in
shen-libs. For instance the chosen name is `foo`. And you want to store it in
`my-libs` directory (listed in `modules.*paths*`).

So create a directory `foo` in `my-libs`. Put your module sources there. To
make the module available for module system create in `my-libs` a module
description (manifest) file `module.shen`. Its contents may look like this:

    (register-module [[author: "Me"]
                      [license: "GPL"]
                      [desc: "A module implementing X."]
                      [depends: "bar" "baz"]
                      [load: "src.shen" "src1.shen" "src2.shen"]])

where `src.shen`, `src1.shen`, `src2.shen` are sources of module `foo`. This
manifest tells module system that a module `foo` depends on modules `bar` and
`baz` and to use it Shen needs to load files `src.shen`, `src1.shen`,
`src2.shen`.

For example of a working module see `defstruct` in shen-libs.
