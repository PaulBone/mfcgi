% Mercury-fcgi library binding
% Streams interface
%
% This binding is a simple wrapping of the C interface.
%
% Copyright (C) 2014 Ebrahim Azarisooreh
% Copyright (C) 2014 Paul Bone
% Distributed under the BSD 3-clause license, see LICENSE.
%

:- module mfcgi.streams.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module string.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- type fcgi_stream.

    % Set exit status
    %
    % Set the exit status for the current request.  The underlying API
    % associates exit status with streams.
    %
:- pred fcgx_set_exit_status(fcgi_stream::in, int::in, io::di, io::uo) is det.

    % A read can either be successful, partial (EOF occured after reading at
    % least one byte) or unsuccessful (EOF).
    %
:- type fcgi_read_result(T)
    --->    ok(T)
    ;       partial(T)
    ;       eof.

:- inst fcgi_read_result_no_partial
    --->    ok(ground)
    ;       eof.

    % Attempt to read a byte from the input stream and returns it.  %
    % NOTE that this wraps the FCGX_GetChar function of the FCGI API.
    % However its comment describes a byte, so in this API we name the
    % predicate using "Byte' and hope that this reduces confusion.  We hope
    % that the FCGI API code comment is correct and the function name is
    % incorrect.
    %
:- pred fcgx_get_byte(fcgi_stream::in,
    fcgi_read_result(int)::out(fcgi_read_result_no_partial),
    io::di, io::uo) is det.

:- type fcgi_unget_result
    --->    ok
    ;       error.

    % Attempt to putback a byte from the input stream.
    %
    % As above, this predicate wraps FCGX_UnGetChar.
    %
:- pred fcgx_unget_byte(fcgi_stream::in, int::in, fcgi_unget_result::out,
    io::di, io::uo) is det.

% We do not implement FCGX_GetStr at the moment as there is no convenient
% and fast translation into Mercury.  Use repeated calls to fcgx_get_byte
% instead.

% XXX: FCGX_GetLine.  We have to check behaviour regarding null bytes before
% implementing this.

    % Write a string to the given stream.
    %
:- pred fcgx_puts(fcgi_stream::in, string::in, bool::out, io::di, io::uo)
    is det.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
   #include \"fcgiapp.h\"
").

%--------------------------------------------------------------------------%

:- pragma foreign_type("C", fcgi_stream, "FCGX_Stream*",
    [can_pass_as_mercury_type]).

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    fcgx_set_exit_status(Stream::in, Status::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io, thread_safe],
"
    FCGX_SetExitStatus(Status, Stream);
").

%--------------------------------------------------------------------------%

fcgx_get_byte(Stream, Result, !IO) :-
    get_byte(Stream, Byte, Result0, !IO),
    (
        Result0 = yes,
        Result = ok(Byte)
    ;
        Result0 = no,
        Result = eof
    ).

:- pred get_byte(fcgi_stream::in, int::out, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_byte(Stream::in, Byte::out, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Byte = FCGX_GetChar(Stream);
    if (Byte == EOF) {
        Result = MR_NO;
    } else {
        Result = MR_YES;
    }
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    fcgx_unget_byte(Stream::in, Byte::in, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    int result = FCGX_UnGetChar(Byte, Stream);
    if (result == EOF) {
        Result = MR_NO;
    } else {
        Result = MR_YES;
    }
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_puts(Stream::in, Str::in, Success::out,
    _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io, thread_safe],
"
    Success = FCGX_PutStr(Str, strlen(Str), Stream) >= 0 ? MR_YES : MR_NO;
").

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
