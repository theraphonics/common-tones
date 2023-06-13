# common-tones

This is `common-tones`. `common-tones` is a fork of CLM-5.

Threading has been rewritten to use `bordeaux-threads` and the main ffi interface has been rewritten to use `cffi`.

The build process has been entirely rewritten to work with ASDF3 and Quicklisp.

The main `mus` file has been broken out into `generators/*` to isolate issues in debugging.

Todo’s include updating the `run` macro and the `defins` file, as well as automating the full build process, but from a portability perspective this should be a lot more portable. Once I finish that I plan to add modern linkages to the audio stack and update the code to use 64-bit floats as an option.

This is available on ultralisp via `(ql:quickload :common-tones)`.

Please open an issue if you find a bug.

This library should be a drop in replacement (at the moment) for clm on Lisp implementations that support ASDF3.

