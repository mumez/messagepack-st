| package |
package := Package name: 'MessagePack Install'.
package paxVersion: 1;
	basicComment: 'MessagePack for Dolphin Smalltalk
Copyright (C) 2011 Masashi Umezawa

MessagePack is a compact binary serialization format (http://msgpack.org/).
"Extremely efficient object serialization library for cross-language communication. It''s like JSON, but very fast and small."

More info:
http://code.google.com/p/messagepack-st/


'.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'MessagePack-Core';
	add: 'MessagePack-Dolphin-Core';
	add: 'MessagePackTest-Core';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

