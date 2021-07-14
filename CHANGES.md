## v3.0.0 2021-07-04

- Upgrade to latest reparse version (3.1.0)
- Flatten module `Part_header` to `part_header`
- Remove push based parser - `parse_parts`.
- Implement reader/pull based multipart parser so that is is more composable as a library.
- Remove dependency on `lwt`, `reparse-lwt`
- Introduce functor `Make` to make multipart parser

## v2.0.1 2021-06-27

- Relax CRFL token on the first boundary value line

## v2.0.0 2021-06-26

- Upgrade to `reparse v3.0.0`
- Change API to streaming api 

## v1.1.0 2021-04-07

- Ensure compatibility with reparse v2.1.0

## v1.0.1 2020-04-12

- Fixes error prone equal - #10.

## v1.0.0 2020-30-11

- First release.
