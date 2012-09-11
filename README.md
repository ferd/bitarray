bitarray
========

Bitarray is a NIF library to provide an alternative to `hipe_bifs:bitarray*` functions, which become unusable in certain circumstances (platform doesn't support HiPE, halfword emulator).

Build
-----
`./rebar compile`

Test
----
`./rebar get-deps compile && ./rebar ct skip_deps=true`
