#lang scribble/manual

@(define pex-url "http://www.apolloart.cz/index.php?jazyk=en&sekce=produkty")

@title[#:tag "pex"]{@exec{pex-driver}: PEx Power Control}

This driver was written to control the @link[pex-url]{ApolloArt PEx Devices}
for power and lighting control. Namely, it supports their relays and faders.

@section[#:tag "pex-arguments"]{Command Line Arguments}

@itemlist[
  @item{
    @Flag{r}, @DFlag{rpc-endpoint} --- Endpoint to receive commands on.
    Defaults to @tt{tcp://127.0.0.1:46103}.
  }

  @item{
    @Flag{p}, @DFlag{pub-endpoint} --- Endpoint to push notifications from.
    Defaults to @tt{tcp://127.0.0.1:46203}.
  }

  @item{
    @Flag{i}, @DFlag{identity} --- Identity for both endpoints.
    Defaults to @tt{pex}.
  }
]

Apart from these flags, the @exec{pex-driver} command expects a path
to serial port device file to open and an identifier of the bank to scan.
For example:

@commandline{pex-driver --identity power /dev/ttyS3 0}

Would open the @tt{/dev/ttyS3} device, assume the @tt{power} identity and
scan for relays and faders in the bank number @tt{0}.


@section[#:tag "pex-commands"]{Command Messages}

@itemlist[
  @item{
    To request immediate notification about status of all detected relays
    and faders on the bus:

    @racketblock[(hasheq 'request "status")]
  }

  @item{
    To control relay power status:

    @racketblock[(hasheq 'request "set-on!"
                         'relay relay-number
                         'on? on?)]

    Where @racket[relay-number] represents a relay identifier reported
    over the publisher socket, possibly in reply to a @racket["status"]
    request. The @racket[on?] boolean flag indicates whether the relay
    should be turned on.
  }

  @item{
    To control power level of a fader:

    @racketblock[(hasheq 'request "set-level!"
                         'fader fader-number
                         'level level)]

    Where @racket[fader-number] represents a fader identifier reported
    over the publisher socket, possibly in reply to a @racket["status"]
    request. The @racket[level] natural number ranging from @racket[0] to
    @racket[99] represents the desired power level.

    Setting the @racket[level] to @racket[0] will turn the fader power off
    completely. Please note that some level settings are ignored by the fader
    device. It is suggested to experiment with your setup.
  }

  @item{
    To gradually change power level of a fader:

    @racketblock[(hasheq 'request "fade-to-level!"
                         'fader fader-number
                         'level level)]

    This request is mostly identical to @racket["set-level!"], but instead
    of setting the desired level immediately, fades to the desired level
    in a single second.

    As with @racket["set-level!"], some levels might not register. Please
    experiment with your setup to learn what levels work for you. Also note
    that it might not be possible to fade out completely.
  }
]


@section[#:tag "pex-messages"]{Published Messages}

@itemlist[
  @item{
    Every few seconds, the driver publishes a complete status of all
    relays and faders found on the bus:

    @racketblock[(hasheq 'full (hasheq 'relays (list (hasheq 'id 1
                                                             'on? #t) ...)
                                       'faders (list (hasheq 'id 2
                                                             'level 50) ...)))]
  }

  @item{
    When a relay has been toggled or a fader has been adjusted a
    delta update is emitted:

   @racketblock[(hasheq 'delta (hasheq 'relay (hasheq 'id 1 'on? #f)))
                (hasheq 'delta (hasheq 'fader (hasheq 'id 2 'level 33)))]
  }
]


@; vim:set ft=scribble sw=2 ts=2 et:
