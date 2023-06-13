# common-tones

This is `common-tones`. `common-tones` is a fork of CLM-5 (https://ccrma.stanford.edu/software/clm/).

Threading has been rewritten to use `bordeaux-threads` and the main ffi interface has been rewritten to use `cffi`.

The build process has been entirely rewritten to work with ASDF3 and Quicklisp.

Sound *probably* doesn't work. I wouldn't call this "production-ready" but all of the mathematical/signal functions DO work. If you're on Unix/Linux/MacOS (but not M1) it might be able to connect to the audio system but I haven't gotten to that part of the code yet. Same with the run loop. Use those at your own risk...

The main `mus` file has been broken out into `generators/*` to isolate issues in debugging.

Todo’s include updating the `run` macro and the `defins` file, as well as automating the full build process, but from a portability perspective this should be a lot more portable. Once I finish that I plan to add modern linkages to the audio stack and update the code to use 64-bit floats as an option.

This is available on Ultralisp (https://ultralisp.org) via `(ql:quickload :common-tones)`.

Please open an issue if you find a bug.

This library should be a drop in replacement (at the moment) for clm on Lisp implementations that support ASDF3.
