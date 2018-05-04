# Fcd+Remill

Fcd+Remill is a fork of the original native program decompiler [fcd](http://zneak.github.io/fcd/). It has a three stage decompilation architecture which uses [Remill](https://github.com/trailofbits/remill) for translating x86 and amd64 machine code (including AVX and AVX512) into [LLVM bitcode](http://llvm.org/docs/LangRef.html), performs transformation passes over the bitcode and then uses the [pattern-independent structuring](https://github.com/trailofbits/fcd/blob/master/docs/NoMoreGotos.pdf) algorithm to a goto-free C-like output.

The design philosophy behind the project is to provide a relatively small and easily hackable codebase with great interoperability with other LLVM and Remill-based projects.

Future goals include AArch64 support, support for Fcd+Remill usable as a Python module and steadily improved recovery and presentation of advanced native code constructs, such as jump-tables and virtual calls.

## Build Status

|       | master |
| ----- | ------ |
| Linux |        |

## Getting Help

If you are experiencing undocumented problems with Fcd+Remill then ask for help in the `#binary-lifting` channel of the [Empire Hacking Slack](https://empireslacking.herokuapp.com/).

## Supported Platforms

Fcd+Remill is supported on Linux platforms and has been tested on Ubuntu 16.04.

## Dependencies

Most of Fcd+Remill's dependencies can be provided by the [cxx-common](https://github.com/trailofbits/cxx-common) repository. Trail of Bits hosts downloadable, pre-built versions of cxx-common, which makes it substantially easier to get up and running with Remill. Nonetheless, the following table represents most of Fcd+Remill's dependencies.

| Name | Version | 
| ---- | ------- |
| [Git](https://git-scm.com/) | Latest |
| [CMake](https://cmake.org/) | 3.2+ |
| [Google Flags](https://github.com/google/glog) | Latest |
| [Google Log](https://github.com/google/glog) | Latest |
| [LLVM](http://llvm.org/) | 3.5 - 5.0 |
| [Clang](http://clang.llvm.org/) | 3.5 - 5.0 |
| [Remill](https://github.com/trailofbits/remill) | Latest |
| [Python](https://www.python.org/) | 2.7 |
| [Zlib](https://www.zlib.net/) | Latest |
| Unzip | Latest |

## Getting and Building the Code

### On Linux

First, update aptitude and get install the baseline dependencies.

```shell
sudo apt-get update
sudo apt-get upgrade

sudo apt-get install \
     git \
     python2.7 \
     wget \
     realpath \
     build-essential \
     libtinfo-dev \
     python-dev \
     libz-dev \
     lsb-release
```

The next step is to clone the Remill repository. We then clone the Fcd repository into the tools subdirectory of Remill. This is kind of like how Clang and LLVM are distributed separately, and the Clang source code needs to be put into LLVM's tools directory.

```shell
git clone https://github.com/trailofbits/remill.git
cd remill/tools/
git clone https://github.com/trailofbits/fcd.git
```

Finally, we build Remill along with Fcd. This script will create another directory, `remill-build`, in the current working directory. All remaining dependencies needed by Remill will be built in the `remill-build` directory.

```shell
cd ../../
./remill/scripts/build.sh
```

To try out Fcd+Remill you can do the following, given an `amd64/linux` binary of your choice.

```shell
./remill-build/tools/fcd/fcd -arch amd64 -os linux mybinary.out
```
