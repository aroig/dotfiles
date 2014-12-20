# clang-format.el

This package allows to filter code through clang-format to fix its formatting.
clang-format is a tool that formats C/C++/Obj-C code according to a set of
style options, see [Clang-Format Style Options][style].
Note that clang-format 3.4 or newer is required.

The main functionality provided by this package is contained in
`clang-format-region`.  If called interactively, the region (or the
current buffer if there is no active region) is formatted according to
`clang-format-style'.

You can use `clang-format-executable` to specify the name or location
of the clang-format executable.

## [License](LICENSE)

Copyright © 2014 Johann Klähn

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.


[style]: http://clang.llvm.org/docs/ClangFormatStyleOptions.html
