# Fable.TypescriptProvider

**The goal of this project is to provide an easier way to interop with Typescript Libraries without manually generating and maintain ts2fable generated type definitions**

**THIS IS HIGHLY EXPERIMENTAL. DO NOT USE IN ANY PRODUCTION CODE!!!**

*This is currently only tested on win 10 on my machine with my current setup. See this as a sneak peek. If things don't work after checkout there is a huge chance I made a monumental mistake in the code. Also consider, that when working with this, the usual "I can't compile the TP while using the TP in my test project" is also included. Linux/mac support is potentially available but also not tested*

## Status Updates

*feb 2021*

I'm working on more type support and a "compile-test-suite" for a bunch of npm packages.
React Component support is also a main motivation for this project.

I'm not an expert in OSS projects so there are no guidelines on how to contribute or anything. We are a small community and if you like to help you can just open an issue or maybe find me in the fable/f# slack channels.
## Prerequisites

**ts2fable-json-export**

ts2fable adjusted npm package which supports json exports for the generated types. This is ts2fable with the single adjustment to export the result as a json instead of writing .fs files. I hope i can integrate this directly to ts2fable when this project is more stable and useful.

install:

```
npm i -g ts2fable-json-export
```

**cloned version of this project**

this is not a nuget package (yet)


## Usage

You can start by using the "sample/Simple" example. The TypeProvider assembly was added manually.


```fsharp
type LeftPad = Fable.TypescriptProvider.Import<"default", "left-pad", DEV_FABLE_LIB_VER = "3.1.1">

let paddedString = LeftPad.leftPad(U2.Case1 "Test", 0.0, None)
```

the code is then compiled with fable (only tested on 3.1.1)

The resulting js looks like this:

```js

import left$002Dpad from "left-pad";

export const padded = left$002Dpad("Test", 0, (void 0));

```

## Using other people's work

This is work done primarily by looking at the Fable.JsonProvider for guidance and looking at some ts2fable stuff and smashing those things together. So other people did the heavy lifting! I guess its safe to say that most of this work was done by the [Fable Compiler](https://github.com/fable-compiler) Team (esp. alfonsogarciacaro)


## Why is think is project is necessary

No matter which compile 2 js language you use, none (I know of) can interop with javascript (i mean typescript) without maintaining custom interface definitions or generating some types which have to be part of your build pipeline. It should be easier to just use this vast ecosystem of proven libraries for the front end and leverage them with ease.

There are also some other upsides with the TP approach:

1. You can see if the package is installed
2. Any npm package update with changing type definitions will result in compile errors (which is good)
3. No additional step is necessary to just use some typescript libraries

## Beware!

This approach is potentially a dead end for some interop cases or it may be even unusable in certain scenarios (only the future will tell). 

Even the syntax may deviate from existing f# approaches like Feliz. This does not mean that those approaches won't be supported some time but are also limited by the TP.

There is also a huge chance that naming, code structure, dead code, silly comments, unhelpful error messages or the overall approach may change between commits without any notice (at least at the moment)