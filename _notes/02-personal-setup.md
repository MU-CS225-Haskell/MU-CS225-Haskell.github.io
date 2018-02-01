---
layout: page
title: Personal Setup
permalink: /personal-setup/
visible: true
---

I use [Atom](https://atom.io/) with [lots of
extensions](https://atom.io/packages/atom-haskell) and a local copy of `ghc-mod`
built for each project. Works pretty nice for me. Here's what I run to set up
a new project:

```zsh
> stack new {project-name} {template}
> cd {project-name}
> stack setup  # only run this if this is your very first stack project
> stack build ghc-mod  # first time will take a disgustingly long time
> atom .
```

I want to point out here that there are some issues with compiling ghc-mod on
`lts-10.*`, so until it's working again I'd recommend heading into your
`stack.yaml` and changing your resolver to `lts-9.21`. If you want to set this
on project creation, try:

```zsh
> stack --resolver lts-9.21 new {project-name} {template}
```
