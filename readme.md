Kali Engine (C) Martok
https://github.com/martok/kali/

For copyright information see file LICENSE.

General
=======
Runs on Delphi 7.
Newer/Older versions and FPC untested.

This engine is composed of different subsystems, each handling separate aspects.
The overarching focus is on old-school graphics at very low resoulutions.
It provides only the most basic functions. Most advanced techniques are left to the
implementation.

While usage is best seen in the examples provided, it may be neccessary to understand
the basic concepts first. The engine provides a TScreen object that is essentially a
fusion of TApplication and exactly one TForm. It provides several entry points in the
form of event handlers that can be used to build the implementation.
The most important idea is the strict distinction between graphics and logic. This
is represented by the OnTick and OnRender events. Both occur sequentially in the same
loop, but you should not assume that. Future versions may further decouple them.

Ticks can be rate-limited to save computation power. In any case, the event always
provides a measurement of time elapsed since the last tick, allowing for timebased
movement.
Render cycles are passed a Surface which can be drawn on, and should not do any
logic (except maybe state-independent animations).

A similar idea applies to the server, but there is of course no Render function.
The client and server use a set of Events, just like the Engine does.
Communication is based on Entity Messages (notifications from server to client) and
Entity Calls (commands from client to server). It is the implementations responsibility
to manage entities and route messages if required. Parameters can be passed as
Variant arrays, the Net subsystem takes care of converting data and transmitting/
receiving/decoding it back into Variant array form.

Usually, a client will issue a Call on user interaction. The server then processes it,
updates its internal states and then sends all clients Messages notifying them of the new
state if required. This allows for relatively 'stupid' clients, only holding information
essential to rendering and user interaction. Only the server needs all information
required for Tick processing.


Subsystems
----------
*   **Engine**

    The Core graphics and game engine. Provides startup code and resource loading
    as well as graphics and mouse/keyboard interaction.

*   **Net**

    Client and Threaded Server base classes for an entity message based server
    as well as a generic Entity base class. This does the most basic tasks required
    for every server, such as handshaking and packet creation. It also provides
    server states for more high-level Lobby/In-Game distinctions.


Examples
========
The package provides some examples to demonstrate the general use of the engine.
You'll notice a recurring theme in there, since they were all built upon the (most
basic) "game" example by extending upon and stripping out of it.

The Star Wars character graphics are from a sprite collection I downloaded ages ago;
if you recognise the source: please tell me, I would love to give credit!

/example/game
--------------
The most basic thing you can do. Shows setup and the logic behind seperate render
and tick functions. Also, it shows key interaction for walking and rudimentary
(and physically completely wrong ;-) ) jumping.

/example/network
----------------
A chat client/server, also demoing the menu system. Has possibly the weirdest menu
structure ever and does no error handling at all, but should show the general idea
of how to use the net subsystem.


Externals
=========
This engine requires the following external libraries in case the respective subsystem
is to be used:

* Engine
    * GraphicEx by Mike Lische - [Web Site](http://www.soft-gems.net/index.php/libs/graphicex-library)
* Net
    * NarsesBFPA by Narses - [Forum Post](http://www.delphi-forum.de/topic_TNBFPA+v112++SocketKompos+mit+Protokollfunktionen_71223.html)
    * MessageThread by delfiphan - [Forum Post](http://www.delphi-library.de/topic_Ereignisorientierte+ThreadsMessage+Handling_90333.html)


History
=======
git commit information is all there is, at least for now.

