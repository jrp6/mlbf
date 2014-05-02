mlbf
====

My Little BrainFuck

An interpreter of brainfuck. Supports an infinite amount of 8-bit cells (values ∈ [0, 255] ∩ *Z*), but modifying to support other cell sizes should be trivial.

Usage
-----

Compile using the compile-command specified in main.hs.
Run bin/mlbf. When the prompt 'file> ' appears,
enter the path to the brainfuck source.

TODO
----

* More sophisticated handling on reaching EOF or invalid programs.
