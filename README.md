### Haskell cabal cli helpers
Create, rename and delete modules with cli.

<code>hcli add-exe -m Common</code>
Will create file app/Common.hs and add line to the other-modules in the project cabal file executable section.
Currently works with only one executable.

<code>hcli add-test -m Options.Common</code>
Will create file tests/Options/Common.hs and add line to the other-modules in the project cabal file test-suite section.
Currently works with only one test-suite.

