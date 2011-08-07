| package |
package := Package name: 'MessagePackTest-Core'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #MpPackUnpackTestCase;
	add: #MpTestCase;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: 'MessagePack-Core';
	add: '..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #MpTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MpTestCase subclass: #MpPackUnpackTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

MpTestCase guid: (GUID fromString: '{69416983-0D62-4C02-A16E-34E1CB9AE2B8}')!
MpTestCase comment: ''!
!MpTestCase categoriesForClass!MessagePackTest-Core! !
!MpTestCase methodsFor!

atRandom: integer	"For portability"	^ (MpPortableUtil default randomClass new next * integer) truncated + 1!

collectionEquals: aCollection with: otherCollection	^MpPortableUtil default collectionEquals: aCollection with: otherCollection!

newCollection: aCollectionClass sized: size withAll: elem	^MpPortableUtil default newCollection: aCollectionClass sized: size withAll: elem! !
!MpTestCase categoriesFor: #atRandom:!private! !
!MpTestCase categoriesFor: #collectionEquals:with:!private! !
!MpTestCase categoriesFor: #newCollection:sized:withAll:!private! !

MpPackUnpackTestCase guid: (GUID fromString: '{4A008951-5058-41C9-9961-A649FF8C9B5C}')!
MpPackUnpackTestCase comment: ''!
!MpPackUnpackTestCase categoriesForClass!MessagePackTest-Core! !
!MpPackUnpackTestCase methodsFor!

complexArraysArray	^ self complexCollectionSizes collect: [:each | self createComplexArraySized: each]!

complexCollectionSizes	^ #(0 1 31 32) "For avoiding timeout"!

complexMapsArray	^ self complexCollectionSizes collect: [:each | self createComplexMapSized: each]!

createArraySized: size	| col idx |	col := OrderedCollection withAll: #(true false nil 1 -1 3.14 -3.14).	"col add: (ByteArray new: 100 withAll: 100).	col add: (Array new: 200 withAll: #(2)).	col add: (Dictionary new at: 3 put: 333; yourself)."	idx := self atRandom: col size.	^self newCollection: Array sized: size withAll: (col at: idx)!

createComplexArraySized: size	| col dict |	col := OrderedCollection new.	col add: (self newCollection: ByteArray sized: 100 withAll: 100).	col add: (self newCollection: Array sized: 200 withAll: #(2)).	col add: (Dictionary new at: 3 put: 333; yourself).		dict := Dictionary new: size.	1 to: size do: [:idx | dict at: (idx printString asByteArray) put: (self atRandom: col size)].	^dict!

createComplexMapSized: size	| col idx |	col := OrderedCollection new.	col add: (self newCollection: ByteArray sized: 100 withAll: 100).	col add: (self newCollection: Array sized: 200 withAll: #(2)).	col add: (Dictionary new at: 3 put: 333; yourself).	idx := self atRandom: col size.	^ Array new: size withAll: (col at: idx)!

createRawBytesSized: size	^ self newCollection: ByteArray sized: size withAll: (self atRandom: 255)!

createSimpleArraySized: size	| col idx |	col := OrderedCollection withAll: #(true false nil 1 -1 3.14 -3.14).	idx := self atRandom: col size.	^self newCollection: Array sized: size withAll: (col at: idx)!

createSimpleMapSized: size	| values dic |	values := #(true false nil 1 -1 3.14 -3.14).	dic := Dictionary new: size.	1 to: size do: [:idx | dic at: idx put: (values at: (idx - 1 \\ values size + 1))].	^dic	!

floatsArray	^ #(3.14159 3.141592653589793)!

integersArray	^ #(0 1 126 127 -1 -2 -31 -32 -128 -33 -32768 -129 128 32767 -2147483648 -32769 32768 2147483647 -9223372036854775808 -2147483649 2147483648 9223372036854775807 )!

mapSizes	^ #(0 1 31 32 65535 65536)!

rawBytesArray	^ self rawBytesSizes collect: [:each | self createRawBytesSized: each]!

rawBytesSizes	^ #(0 1 31 32 65535 65536)!

simpleArraysArray	^ self simpleCollectionSizes collect: [:each | self createSimpleArraySized: each]!

simpleCollectionSizes	^ #(0 1 31 32 65535 65536)!

simpleMapsArray	^ self simpleCollectionSizes collect: [:each | self createSimpleMapSized: each]!

testPackUnpackArray	"self debug: #testPackUnpackArray"	self simpleArraysArray do: [:each | | packed unpacked |		packed := each messagePacked.		self should: [packed isMemberOf: ByteArray].		unpacked := Object fromMessagePack: packed.		self should: [self collectionEquals: unpacked with: each]	]					!

testPackUnpackComplexArray	"self debug: #testPackUnpackComplexArray"	self complexArraysArray do: [:each | | packed unpacked |		packed := each messagePacked.		self should: [packed isMemberOf: ByteArray].		unpacked := Object fromMessagePack: packed.		self should: [self collectionEquals: unpacked with: each]	]					!

testPackUnpackComplexMap	"self debug: #testPackUnpackComplexMap"	self simpleMapsArray do: [:each | | packed unpacked |		packed := each messagePacked.		self should: [packed isMemberOf: ByteArray].		unpacked := Object fromMessagePack: packed.		self should: [self collectionEquals: unpacked with: each]	]					!

testPackUnpackFalse	"self debug: #testPackUnpackFalse"	| packed unpacked |	packed := false messagePacked.	self should: [packed isMemberOf: ByteArray].	self should: [packed = (ByteArray with: MpConstants false)].	unpacked := Object fromMessagePack: packed.	self should: [unpacked = false]						!

testPackUnpackFloatingPoint	"self debug: #testPackUnpackFloatingPoint"	self floatsArray do: [:each | | packed unpacked |		packed := each messagePacked.		self should: [packed isMemberOf: ByteArray].		unpacked := Object fromMessagePack: packed.		self should: [unpacked = each]	]					!

testPackUnpackInteger	"self debug: #testPackUnpackInteger"	self integersArray do: [:each | | packed unpacked |		packed := each messagePacked.		self should: [packed isMemberOf: ByteArray].		unpacked := Object fromMessagePack: packed.		self should: [unpacked = each]	]					!

testPackUnpackMap	"self debug: #testPackUnpackMap"	self simpleMapsArray do: [:each | | packed unpacked |		packed := each messagePacked.		self should: [packed isMemberOf: ByteArray].		unpacked := Object fromMessagePack: packed.		self should: [self collectionEquals: unpacked with: each]	]					!

testPackUnpackNil	"self debug: #testPackUnpackNil"	| packed unpacked |	packed := nil messagePacked.	self should: [packed isMemberOf: ByteArray].	self should: [packed = (ByteArray with: MpConstants nil)].	unpacked := Object fromMessagePack: packed.	self should: [unpacked = nil]						!

testPackUnpackRawBytes	"self debug: #testPackUnpackRawBytes"	self rawBytesArray do: [:each | | packed unpacked |		packed := each messagePacked.		self should: [packed isMemberOf: ByteArray].		unpacked := Object fromMessagePack: packed.		self should: [self collectionEquals: unpacked with: each]	]					!

testPackUnpackTrue	"self debug: #testPackUnpackTrue"	| packed unpacked |	packed := true messagePacked.	self should: [packed isMemberOf: ByteArray].	self should: [packed = (ByteArray with: MpConstants true)].	unpacked := Object fromMessagePack: packed.	self should: [unpacked = true]						! !
!MpPackUnpackTestCase categoriesFor: #complexArraysArray!fixtures!public! !
!MpPackUnpackTestCase categoriesFor: #complexCollectionSizes!private! !
!MpPackUnpackTestCase categoriesFor: #complexMapsArray!fixtures!public! !
!MpPackUnpackTestCase categoriesFor: #createArraySized:!private! !
!MpPackUnpackTestCase categoriesFor: #createComplexArraySized:!private! !
!MpPackUnpackTestCase categoriesFor: #createComplexMapSized:!private! !
!MpPackUnpackTestCase categoriesFor: #createRawBytesSized:!private! !
!MpPackUnpackTestCase categoriesFor: #createSimpleArraySized:!private! !
!MpPackUnpackTestCase categoriesFor: #createSimpleMapSized:!private! !
!MpPackUnpackTestCase categoriesFor: #floatsArray!fixtures!public! !
!MpPackUnpackTestCase categoriesFor: #integersArray!fixtures!public! !
!MpPackUnpackTestCase categoriesFor: #mapSizes!private! !
!MpPackUnpackTestCase categoriesFor: #rawBytesArray!fixtures!public! !
!MpPackUnpackTestCase categoriesFor: #rawBytesSizes!private! !
!MpPackUnpackTestCase categoriesFor: #simpleArraysArray!fixtures!public! !
!MpPackUnpackTestCase categoriesFor: #simpleCollectionSizes!private! !
!MpPackUnpackTestCase categoriesFor: #simpleMapsArray!fixtures!public! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackArray!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackComplexArray!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackComplexMap!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackFalse!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackFloatingPoint!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackInteger!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackMap!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackNil!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackRawBytes!public!testing! !
!MpPackUnpackTestCase categoriesFor: #testPackUnpackTrue!public!testing! !

"Binary Globals"!

