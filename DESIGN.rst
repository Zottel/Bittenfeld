==========================
Bittenfeld Design Document
==========================

Diagram
-------
Look at my diagram, my diagram is amazing::

  +-------+  +-------+  +-------+
  | Plugin|  | Plugin|  | Plugin|
  +-------+  +-------+  +-------+
    ||         ||         ||
  +--------------------------------------+
  | Interconnect                         |
  +--------------------------------------+
               ||                 ||
  +----------------+ +----------------+
  |    Network ||  | |    Network ||  |
  |            ||  | |            ||  |
  | +------------+ | | +------------+ |
  | | Reconnect  | | | | Reconnect  | |
  | +------------+ | | +------------+ |
  |            ||  | |            ||  |
  | +------------+ | | +------------+ |
  | | Network    | | | | Network    | |
  | | Logic      | | | | Logic      | |
  | +------------+ | | +------------+ |
  |            ||  | |            ||  |
  | +------------+ | | +------------+ |
  | | Connection | | | | Connection | |
  | |  & Parser  | | | |  & Parser  | |
  | +------------+ | | +------------+ |
  +----------------+ +----------------+


Layers
------
Bittenfeld will have a clear separation between layers:

Network
  Handles the connection to an IRC network.
  That includes:

  - TCP
  - reconnect
  - parsing
  - TLS certificate?
  - nick, user and password
  - ping/pong

Logic
  - pi

Auth
  Not sure if.

