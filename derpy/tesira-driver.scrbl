#lang scribble/manual

@(define tesira-url "http://www.biamp.com/products/tesira/")

@title{@exec{tesira-driver}: Networked Audio Mixer}

This driver was written to control the @link[tesira-url]{Biamp Tesira}
devices. These devices act as a fully programmable network-capable audio
mixers.

Since capabilities of these devices are really wide, the driver is not
able to cover them all and instead focuses on a single key area.
Volume and mute control on a central matrix mixer.

@section[#:tag "tesira-arguments"]{Command Line Arguments}

@itemlist[
  @item{
    @Flag{r}, @DFlag{rpc-endpoint} --- Endpoint to receive commands on.
    Defaults to @tt{tcp://127.0.0.1:46102}.
  }

  @item{
    @Flag{p}, @DFlag{pub-endpoint} --- Endpoint to push notifications from.
    Defaults to @tt{tcp://127.0.0.1:46202}.
  }

  @item{
    @Flag{i}, @DFlag{identity} --- Identity for both endpoints.
    Defaults to @tt{tesira}.
  }
]

Apart from these flags, the @exec{tesira-driver} command expects an address
of the device to connect to via its telnet interface and name of the mixer
element to control. For example:

@commandline{tesira-driver --identity audio 10.0.0.135 Mixer2}

Would connect to the device, scan the @tt{Mixer2} object and assume the
@tt{audio} identity.


@section[#:tag "tesira-commands"]{Command Messages}

@itemlist[
  @item{
    To request immediate notification about mixer status:

    @racketblock[(hasheq 'request "status")]

    It takes some time to get all the information from the device.
    Asking for status too frequently could cause control slowdown.
  }

  @item{
    To change volume on an input or output line respectively:

    @racketblock[(hasheq 'request "set-input-level!"
                         'input input
                         'level level)

                 (hasheq 'request "set-output-level!"
                         'output output
                         'level level)]

    Where @racket[input] and @racket[output] are numbers of the mixer input or
    output line in question, counting from @racket[0]. The @racket[level] is
    the volume in @tt{dB}.
    Valid volumes range from @racket[-100.0] to @racket[12.0].
  }

  @item{
    To mute an input or output line completely (without affecting the
    configured volume level):

    @racketblock[(hasheq 'request "set-input-mute!"
                         'input input
                         'mute? mute?)

                 (hasheq 'request "set-output-mute!"
                         'output output
                         'mute? mute?)]

    Where @racket[input] and @racket[output] are numbers of the mixer input or
    output line in question, counting from @racket[0]. The @racket[mute?]
    boolean flag indicates whether the line should be muted or not.
  }

  @item{
    To change input or output line label:

    @racketblock[(hasheq 'request "set-input-label!"
                         'input input
                         'label label)

                 (hasheq 'request "set-output-label!"
                         'output output
                         'label label)]

    Where @racket[input] and @racket[output] are numbers of the mixer input or
    output line in question, counting from @racket[0]. The @racket[label] is a
    string with new name for that particular line.
  }
]


@section[#:tag "tesira-messages"]{Published Messages}

@itemlist[
  @item{
    When the device is running, a simple beacon is sent:

    @racketblock[(hasheq 'status "online")]
  }

  @item{
    When an input or output level has been modified:

    @racketblock[(hasheq 'delta (hasheq 'input input-number
                                        'status (hasheq 'label "Mic 1"
                                                        'level 0.0
                                                        'mute? #f)))

                 (hasheq 'delta (hasheq 'output output-number
                                        'status (hasheq 'label "Speaker L"
                                                        'level 0.0
                                                        'mute? #f)))]
  }

  @item{
    When full status have been requested:

    @racketblock[(hasheq 'full (hasheq 'inputs (list
                                                 (hasheq 'label "Mic 1"
                                                         'level 0.0
                                                         'mute? #f) ...)
                                       'outputs (list
                                                  (hasheq 'label "Speaker L"
                                                          'level 0.0
                                                          'mute? #f) ...)))]
  }
]


@; vim:set ft=scribble sw=2 ts=2 et:
