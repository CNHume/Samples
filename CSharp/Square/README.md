# Application Notes for the Square Chess Engine
by Christopher Hume, updated 2021-05-22
## Repositories
Source code for the Sentient Project is maintained in the following GitHub Repository:

* Code for the Square Chess Engine can be found in GitHub/Samples/CSharp/Square

Square is a Universal Chess Interface (UCI) Engine.  Please see the [UCI Command Interface](https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/) by Stefan Meyer-Kahlen.


## Copyright and Licensing
Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.

This program comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to redistribute it under certain conditions.  See **COPYING.txt** and **COPYING_LESSER.txt** for details.

## Solution and Project Structure
There is an ASP.Net Core solution named Square consisting of the following projects:

* Square - The ASP.Net Core Console Application and Startup Project
* HeapSort - Heap Sort Library Project
* HeapTest - Test Project for the Heap Sort Library

### Target Framework
All projects in the solution target the **.NET 5.0 SDK**.

**Note:** Microsoft rebranded ASP.Net Core as NET5.0 as of 2020-11-10.

When using a **Preview Version** of the .Net SDK, consider enable the **Include prerelease** checkbox when applying NuGet Package Updates.  It is also sometimes helpful to actually delete the various **obj** and **bin** folders and to perform a **Rebuild All** prior to perrforming an *Add > New Scaffolded Item...* command.

## References

### The Programming Chess Wiki
* [Chess Programming Wiki Main Page](https://www.chessprogramming.org/Main_Page)
