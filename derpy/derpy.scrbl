#lang scribble/manual

@title{Derpy: Presentation Room Control}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

Derpy comprises of several executables that convert vendor-specific device
control protocols to simple ZeroMQ interfaces better suitable for user-facing
control interface programming.

All executables create two ZeroMQ sockets upon their launch. One for receiving
client commands (in @racket['router] mode, listening on the @tt{rpc-endpoint})
and the other for publishing changes (in @racket['pub] mode, listening on the
@tt{pub-endpoint}).

Usually, no confirmations are sent back through the @tt{rpc-endpoint}.
Notifications are pushed through the @tt{pub-endpoint} instead. This might
complicate automation but suffices for human control.


@include-section["atlona-driver.scrbl"]
@include-section["tesira-driver.scrbl"]
@include-section["epson-driver.scrbl"]
@include-section["pex-driver.scrbl"]


@; vim:set ft=scribble sw=2 ts=2 et:
