## Orville Project

To build this project you would require [Haskell] (https://www.haskell.org/ghcup/) installed. 

Try to setup [GHCup] (https://www.haskell.org/ghcup/guide/) and downloading the different versions of toolchains.

## Build Project With Stack 

`stack new` project name [OrvilleProject]
`

`cd project` folder [OrvilleProject]
`

`stack setup`
`

`stack build`
`

`stack run`


## Steps To Add Orville GitHub Repository

Go to the stack.yaml file

Add

extra-deps:
    `git : [.git]`
    `commit: [sha]`
    `subdirs: [orville-psotgresql]`
