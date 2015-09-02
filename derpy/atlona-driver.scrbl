#lang scribble/manual

@(define atlona-url "http://atlona.com/product/at-pro2hd1616m/")

@title[#:tag "atlona"]{@exec{atlona-driver}: HDMI to HDBase-T Matrix Switcher}

This driver was written to control the @link[atlona-url]{Atlona AT-PRO2HD1616M}
HDMI to HDBase-T matrix switcher. Matrix switcher is basically a device that
allows routing of multiple inputs to multiple outputs. In this case, 16 HDMI
video inputs to 16 HDBase-T video outputs.

@section[#:tag "atlona-arguments"]{Command Line Arguments}

@itemlist[
  @item{
    @Flag{r}, @DFlag{rpc-endpoint} --- Endpoint to receive commands on.
    Defaults to @tt{tcp://127.0.0.1:46101}.
  }

  @item{
    @Flag{p}, @DFlag{pub-endpoint} --- Endpoint to push notifications from.
    Defaults to @tt{tcp://127.0.0.1:46201}.
  }

  @item{
    @Flag{i}, @DFlag{identity} --- Identity for both endpoints.
    Defaults to @tt{atlona}.
  }
]

Apart from these flags, the @exec{atlona-driver} command expects a path
to serial port device file to open. For example:

@commandline{atlona-driver --identity video /dev/ttyS3}

Would open the @tt{/dev/ttyS3} device and assume the @tt{video} identity.


@section[#:tag "atlona-commands"]{Command Messages}

@itemlist[
  @item{
    To request immediate notifications about both power and matrix status:

    @racketblock[(hasheq 'request "status")]

    Unlike elsewhere, this won't cause both @racket['status] and
    @racket['matrix] to be reported in a single @racket['full] notification.
    Two separate @racket['delta] notifications are sent one after another.
    This might eventually change.
  }

  @item{
    To connect an input to an output:

    @racketblock[(hasheq 'request "connect!"
                         'input input
                         'output output)]

    Where both @racket[input] and @racket[output] are integers ranging from
    @racket[0] to @racket[15]. The input will replace current video source of
    specified output. Routing a single input to multiple outputs will cause
    the source to re-calculate resolution due to @tt{EDID} changes.

    It is also possible to give @racket[input] of @racket['null]. Doing so is
    equivalent to the next request type and effectively removes video from
    specified output port.
  }

  @item{
    To remove source from an output:

    @racketblock[(hasheq 'request "disable!"
                         'output output)]
  }

  @item{
    To route input @racket[0] to output @racket[0] and so on:

    @racketblock[(hasheq 'request "default!")]
  }

  @item{
    To reset the matrix to factory defaults:

    @racketblock[(hasheq 'request "reset!")]
  }

  @item{
    To put the matrix in standby mode (saving power):

    @racketblock[(hasheq 'request "offline!")]
  }

  @item{
    To wake the matrix from standby mode (to display video again):

    @racketblock[(hasheq 'request "online!")]
  }
]


@section[#:tag "atlona-messages"]{Published Messages}

@itemlist[
  @item{
    When the matrix is running and routing video, it reports its power
    status as:

    @racketblock[(hasheq 'delta (hasheq 'status "online"))]
  }

  @item{
    When the matrix has been put to standby mode or it has been just turned
    on, it reports its power status as:

   @racketblock[(hasheq 'delta (hasheq 'status "offline"))]
  }

  @item{
    To inform about current setup of the switching matrix:

    @racketblock[(hasheq 'delta (hasheq 'matrix '(0 1 ... 15)))]

    Where every position represents an output (from @racket[0] to @racket[15])
    and the value its current input port.
  }
]


@; vim:set ft=scribble sw=2 ts=2 et:
