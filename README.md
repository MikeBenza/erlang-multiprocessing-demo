processes
=====

An OTP application to demonstrate Erlang's massive multiprocessing ability.  This application
will launch a bunch of processes that communicate with each other.  Each process will find
5 other processes to send messages to.  Every second they will send a message to each of their
5 "siblings."  Every second each one will report how many messages they sent/received.  Every
**three** seconds the total number of sent/received messages will print to the terminal.

## Getting Started

1. Install erlang and rebar3
2. Build:

    ```bash
    $ rebar3 compile
    ```

3. Run:

    ```bash
    $ rebar3 shell
    ```

4. Launch processes:
    
    ```
    1> process_manager:start_n_processes(5).

This has been tested with up to 100000 processes without a problem.


## Notes

I think there may be an issue with recording how many messages are sent since it seems about 0.5%
too low.
