# lc3-vm

F# implmenetation of [LC3 VM](https://en.wikipedia.org/wiki/Little_Computer_3), based on [this](https://justinmeiners.github.io/lc3-vm/) tutorial.

## Running

To run the vm, build in Release configuration (Debug will throw a StackOverflowException since tail recursion optimisations are not performed during compilation) and provide a path to the .obj file.

```
.\lc3-vm.exe .\2048.obj
```

Some of the programs that can be ran are [2048](https://github.com/rpendleton/lc3-2048) and [Rogue](https://github.com/justinmeiners/lc3-rogue)
