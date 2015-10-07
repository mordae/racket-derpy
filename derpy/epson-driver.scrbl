#lang scribble/manual

@title[#:tag "epson"]{@exec{epson-driver}: Epson Projector Control}

This driver was written to control the Epson @tt{EB-Z} projector series.
It does not present all functions of the hardware.

@section[#:tag "epson-arguments"]{Command Line Arguments}

@itemlist[
  @item{
    @Flag{r}, @DFlag{rpc-endpoint} --- Endpoint to receive commands on.
    Defaults to @tt{tcp://127.0.0.1:46111}.
  }

  @item{
    @Flag{p}, @DFlag{pub-endpoint} --- Endpoint to push notifications from.
    Defaults to @tt{tcp://127.0.0.1:46211}.
  }

  @item{
    @Flag{i}, @DFlag{identity} --- Identity for both endpoints.
    Defaults to @tt{epson}.
  }
]

Apart from these flags, the @exec{epson-driver} command expects an address
of the device to connect to via its @tt{TCP/IP} interface.
For example:

@commandline{epson-driver --identity projector-1 10.8.0.21}

Would connect to the device and assume the @tt{projector-1} identity.


@section[#:tag "epson-commands"]{Command Messages}

@itemlist[
  @item{
    To request immediate notification about projector status:

    @racketblock[(hasheq 'request "status")]
  }

  @item{
    To bring projection on-line or off-line respectively.

    @racketblock[(hasheq 'request "online!")
                 (hasheq 'request "offline!")]
  }

  @item{
    To control aspect ratio of projection:

    @racketblock[(hasheq 'request "set-aspect!"
                         'aspect aspect)]

    Valid @racket[aspect] values are listed below, but not all of them
    are supported by all projector types:

    @itemlist[
      @item{@racket["normal"] --- fit vertically}
      @item{@racket["4:3"] --- force 4:3}
      @item{@racket["16:9"] --- force 16:9}
      @item{@racket["auto"] --- useless magic}
      @item{@racket["full"] --- deform to fit}
      @item{@racket["zoom"] --- fit horizontally}
      @item{@racket["native"] --- no scaling}
    ]
  }

  @item{
    To control freezing of currently projected picture:

    @racketblock[(hasheq 'request "set-freeze!"
                         'freeze? freeze?)]

    In order to freeze the picture, pass @racket[#t] for @racket[freeze?].
    To cancel, pass @racket[#f].
  }

  @item{
    To control blanking of projection (closing a shutter while leaving
    the lamps running):

    @racketblock[(hasheq 'request "set-mute!"
                         'mute? mute?)]

    In order to blank the picture, pass @racket[#t] for @racket[mute?].
    To cancel, pass @racket[#f].

    Note that since the projector closes an actual, physical shutter in order
    to blank the picture, there is an audible noise to be heard. This means
    that blanking projector during input switch and similar operations aiming
    to improve user experience will probably not yield the desired results.
  }
]


@section[#:tag "epson-messages"]{Published Messages}

@itemlist[
  @item{
    When the device is running, following status message is published every
    5 seconds:

    @racketblock[(hasheq 'full (hasheq 'status status
                                       'aspect aspect
                                       'mute? mute?
                                       'freeze? freeze?))]

    Where @racket[aspect] is the current aspect ratio (see above),
    @racket[mute?] is set to @racket[#t] when blanking is active,
    @racket[freeze?] is set to @racket[#t] when the picture is frozen,
    and @racket[status] represents current power status.

    Possible power statuses are:

    @itemlist[
      @item{@racket["offline"] --- projector standing by, network off}
      @item{@racket["online"] --- projector running, lamp active}
      @item{@racket["warmup"] --- projector starting, lamp pending}
      @item{@racket["cooldown"] --- projector shutting down, lamp cooling}
      @item{@racket["standby"] --- projector standing by, network on}
      @item{@racket["abnormal"] --- projector state abnormal}
      @item{@racket["av-standby"] --- projector standing by (A/V)}
    ]
  }

  @item{
    When either @racket[aspect], @racket[mute?] or @racket[freeze?] changes,
    partial status update is published immediately:

    @racketblock[(hasheq 'delta (hasheq 'aspect aspect))
                 (hasheq 'delta (hasheq 'freeze? freeze?))
                 (hasheq 'delta (hasheq 'mute? mute?))]
  }
]


@; vim:set ft=scribble sw=2 ts=2 et:
