# Version 1.1.49

* Updating p1_utils to version 1.0.25.
* Test for CVE-2022-25236 compilance

# Version 1.1.48

* Generate documentation before publishing to hex
* Load the NIFs in the on_load callback, to support restart
* Add always_encode field to #attr{}

# Version 1.1.47

* Updating p1_utils to version 1.0.23.
* Switch from using Travis to Github Actions as CI
* Fix compatibility with OTP24

# Version 1.1.46

* Updating p1_utils to version 1.0.22.

# Version 1.1.45

* Updating p1_utils to version 1.0.21.

# Version 1.1.44

* Get back compatibility with Erlang older than 20
* Add JSON encode/decode functions generation
* Update hex to compile ejabberd with rebar3

# Version 1.1.43

* Updating p1_utils to version 1.0.20.

# Version 1.1.42

* Fix compilation with Erlang/OTP 23.0

# Version 1.1.41

* Updating p1_utils to version 1.0.19.

# Version 1.1.40

* Fix issues with travis testing

# Version 1.1.39

* Updating p1_utils to version 1.0.18.
* Update copyright year

# Version 1.1.38

* Updating p1_utils to version 1.0.17.

# Version 1.1.37

* Updating p1_utils to version 1.0.16.
* Update XMLRPC codec
* Fail with 'badarg' on unknown records

# Version 1.1.36

* Updating p1_utils to version 1.0.15.

# Version 1.1.35

* Updating p1_utils to version 1.0.14.
* Add contribution guide

# Version 1.1.34

* Updating p1_utils to version 1.0.13.

# Version 1.1.33

* Updating p1_utils to version 6ff85e8.
* Fix incompatibility with OTP21

# Version 1.1.32

* Don't crash when trying to encode xmlcdata

# Version 1.1.31

* Updating p1_utils to version 1.0.12.

# Version 1.1.30

* Improve detection of rebar3
* Define p1\_utils as application dependency

# Version 1.1.29

* Updating p1_utils to version 1.0.11.
* Fix compilation with rebar3
* Get rid of $\_xmls label

# Version 1.1.28
* Include Makefile in package generated for hex

# Version 1.1.27

* Freeze dependencies in mix.lock file to be more friendly with hex.pm
* Fix ambiguous Elixir syntax in mix.exs

# Version 1.1.26

* Simplify pretty printer generation
* Generate get_els/1 and set_els/2
* The pretty printer should traverse elements recursively
* Extra test for too big input

# Version 1.1.25

* Invalidate sorted data when generating stanza-too-big-error

# Version 1.1.24

* Updating p1_utils to version 1.0.10.
* Make XML generator work on R19.3+

# Version 1.1.23

* depends on p1_utils-1.0.9

# Version 1.1.22

* Fix md5 sum calculation of modules for OTP17 (Evgeniy Khramtsov)
* Fix type spec for fxml_stream:parse_element/1 (Evgeniy Khramtsov)

# Version 1.1.21

* Add code for building on FreeBSD (Dave Cottlehuber)

# Version 1.1.20

* Make XML generator working on OTP 18 (Evgeniy Khramtsov)

# Version 1.1.19

* Add checks for empty string (Paweł Chmielowski)
* Remove unused code (Paweł Chmielowski)
* Load locally build .so file when performing tests (Paweł Chmielowski)

# Version 1.1.18

* Use p1_utils 1.0.6 (Paweł Chmielowski)
* fix xref with otp 17 (Paweł Chmielowski)

# Version 1.1.17

* Add 'undefined' type to some record fields type specs (Evgeniy Khramtsov)

# Version 1.1.16

* Improve XML generator (Evgeniy Khramtsov)

# Version 1.1.15

* Update to p1_utils 1.0.5 (Mickaël Rémond)

# Version 1.1.14

* Erlang OTP R19 compliance (Paweł Chmielowski)
* Fix compilation on rebar3 (Paweł Chmielowski)

# Version 1.1.13

* Use p1_utils 1.0.4 (Mickaël Rémond)

# Version 1.1.12

* Generator improvements (Evgeny Khramtsov)

# Version 1.1.11

* Now properly includes Elixir lib/ directory in hex.pm package (Mickaël Rémond)

# Version 1.1.10

* Split build in two steps to fix link step on Ubuntu (Paweł Chmielowski - Mickaël Rémond)
* Clean Makefile.mix to remove duplicated code (Paweł Chmielowski)

# Version 1.1.9

* Fix Linux build with Mix (Paweł Chmielowski)

# Version 1.1.8

* Package priv/lib structure to make sure everything is properly build by mix (Mickaël Rémond)

# Version 1.1.7

* Fix indent issue in Mix Makefile (Mickaël Rémond)

# Version 1.1.6

* Add missing Makefile.mix file in rebar hex.pm package description (Mickaël Rémond)
* Make sure priv dir is created when building with mix and included in package dir list (Mickaël Rémond)

# Version 1.1.4

This is an Elixir friendly update:

* Add ability to return maps instead of xmlel record (Paweł Chmielowski)
* Add ability to tell parser to return Elixir structs instead of records (Mickaël Rémond)
* Add Elixir tests (Mickaël Rémond)

# Version 1.1.3

* Memory optimizations (Paweł Chmielowski)
* Update to latest version of p1_utils (Mickaël Rémond)
* Erlang OTP R18 compliance (Evgeniy Khramtsov)

# Version 1.1.2

* Application is now called fast_xml (Mickaël Rémond)

# Version 1.1.1

* Support for both rebar and rebar3 (Mickaël Rémond)
* Huge performance and memory improvements (Paweł Chmielowski)
* Normalize namespace prefixed elements (Paweł Chmielowski)
* Document how to run tests (Mickaël Rémond)
* Architecture documentation in README.md (Mickaël Rémond)
* Introduce Elixir Quickcheck tests (Mickaël Rémond)
* Support C code coverage (Paweł Chmielowski)
* Better test case coverage (Evgeniy Khramtsov)
* Continuous integration with Travis CI and Coveralls (Paweł Chmielowski - Mickaël Rémond)
* Test refactoring (Evgeniy Khramtsov)
* Save cflags/ldflags passed to configure (Paweł Chmielowski)
* Move code for locating nif files to p1_utils package (Paweł Chmielowski)
* Improve code for locating .so part (Paweł Chmielowski)
* Do not check Expat presence via m4 macro (Evgeniy Khramtsov)

# Version 1.1.0

* Initial release on Hex.pm (Mickaël Rémond)
