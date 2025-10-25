# DreamBerd-Interpretor-work-in-progress-
Working on making an interpretor for dreamberd, no promises, and it won't support html or regex at least for now (probably never, but again, no promises).

> :warning: *Everything is a parody. This is based on Dreamberd (now named The Gulf of Mexico). There are many jokes and parody aspects here, but none of them are really serious, and don't take it as such please! The code is also intentionally dodgy in places (this is not meant to be production quality code).*

For now I'm instead referring to it as Rust++.

Features, not bugs:
 * You may be asking why there isn't any importants. Well, the tariffs simply cost too much, so we'll export instead (correction; I may have both, and everything requires an export and import, only having one or the other results in a runtime or compile time error (depends on how nice I am, aka really just how lazy I am--very))!
 * A Standard Library!!! Does it work? idk, nothing runs yet, so it can't not work. Anyways, in the claims people often make about Rust, if it compiles, it has to work, right? Oh, also, because of the export/import rules, you have to define a header called 'main.rpp' to be able to use it. The Std-lib code is just attached to the end of the users code before tokenization when being compiled into an intermediary form.
