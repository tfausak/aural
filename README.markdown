# Aural

Aural parses and generates Rocket League replays.

[![Build status badge][]][build status]

:warning:
This was an experiment to parse Rocket League replays without any external dependencies.
I got as far as round-tripping UTF-8 and JSON.
The next steps would be parsing the byte-level data (header) and then bit-level (content).
However that would be a lot of effort for next to no benefit.
If you want to parse Rocket League replays, use Rattletrap instead:
<https://github.com/tfausak/rattletrap>.

[Build status badge]: https://travis-ci.org/tfausak/aural.svg?branch=master
[build status]: https://travis-ci.org/tfausak/aural
