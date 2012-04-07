======
 HScD
======

*HScD* is a command line tool and library to access SoundCloud_.

Usage
=====

::

    hscd
      -t URL   --track=URL    Indicate the URL of the track to be downloaded.
      -o FILE  --output=FILE  Output File
      -i URL   --info=URL     Get info about the resource pointed by the URL
      -r URL   --resolve=URL  Resolve the SoundCloud's API URL for an arbitrary URL. Supports users, tracks, sets, groups and apps
      -h       --help         Show usage info

Installation
============

Clone the source code using hg or git and run cabal install from the main directory.

.. _SoundCloud: http://soundcloud.com/
