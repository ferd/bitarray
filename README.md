bitarray
========

Bitarray is a NIF library to provide an alternative to `hipe_bifs:bitarray*` functions, which become unusable in certain circumstances (platform doesn't support HiPE, halfword emulator).

Build
-----
`./rebar compile`

Test
----
`./rebar get-deps compile && ./rebar ct skip_deps=true`

Usage
-----

The bitarrays are 0-indexed (based on HiPE's bitarrays), and run with three functions: `new/2`, `update/3`, and `sub/2`:

    %% 20 bits array, initialized to false
    Bits = bitarray:new(20, false),
    %% flips the bits 0, 13 and 19 to true
    bitarray:update(Bits, 0, true),  % lower boundary
    bitarray:update(Bits, 13, true),
    bitarray:update(Bits, 19, true), % max boundary for 20 bits
    %% Reads the 6th and 13th bits:
    false = bitarray:sub(Bits, 6),
    true = bitarray:sub(Bits, 13).

Note that there is also a `new/1` function that defaults to `new(N, false)`.

Performance
-----------

The `bitarray` application is, depending on the test run, on par with HiPE bitarrays in the best cases, and nearly twice as slow in the worst case. Initial allocation for a bitarray is the worst part, being nearly 10x slower than their HiPE counterparts.

Note that they're generally much faster than any in-Erlang attempt at things, such as using binaries if you need to update rather frequently.

This application may therefore be seen as a backup plan for the HiPE bitarrays, not a performance improvement.
