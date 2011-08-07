| package |
package := Package name: 'MessagePack-Dolphin-Core'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: 'MessagePack-Squeak-Core-mu.1'.


package classNames
	add: #MpDolpEncodeTypeMapper;
	add: #MpDolpPortableUtil;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: 'MessagePack-Core';
	yourself).

package!

"Class Definitions"!

MpPortableUtil subclass: #MpDolpPortableUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MpEncodeTypeMapper subclass: #MpDolpEncodeTypeMapper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

MpDolpPortableUtil guid: (GUID fromString: '{6E5B8461-C47A-4F6C-9143-D1D1FE750292}')!
MpDolpPortableUtil comment: ''!
!MpDolpPortableUtil categoriesForClass!MessagePack-Dolphin-Core! !
!MpDolpPortableUtil methodsFor!

collectionEquals: aCollection with: otherCollection
	"For testing"
	aCollection class = otherCollection class ifFalse: [^false].
	aCollection size = otherCollection size ifFalse: [^false].
	^ aCollection allSatisfy: [:each | otherCollection includes: each]!

encodeTypeMapperClass	^MpDolpEncodeTypeMapper!

readDoubleFrom: stream	| bytes |
	bytes := stream next: 8.
	^ bytes reverse doubleAtOffset: 0!

readFloatFrom: stream	"32 bit"	| bytes |
	bytes := stream next: 4.
	^ bytes reverse floatAtOffset: 0!

readInt16From: stream	| bytes |
	bytes := stream next: 2.
	^ bytes reverse swordAtOffset: 0!

readInt32From: stream	| bytes |
	bytes := stream next: 4.
	^ bytes reverse sdwordAtOffset: 0!

readInt64From: stream	| val |
	val := self readUint64From: stream.
	val > 16r7FFFFFFFFFFFFFFF ifTrue: [^val - 16r10000000000000000].
	^val!

readUint16From: stream	| bytes |
	bytes := stream next: 2.
	^ bytes reverse wordAtOffset: 0!

readUint32From: stream	| bytes |
	bytes := stream next: 4.
	^ bytes reverse dwordAtOffset: 0!

readUint64From: stream	| bytes |
	bytes := stream next: 8.
	^ ((bytes basicAt: 1) bitShift: 56) + ((bytes basicAt: 2) bitShift: 48) + ((bytes basicAt: 3) bitShift: 40) + ((bytes basicAt: 4) bitShift: 32) +
	((bytes basicAt: 5) bitShift: 24) + ((bytes basicAt: 6) bitShift: 16) + ((bytes basicAt: 7) bitShift: 8) + (bytes basicAt: 8)!

writeDouble: value to: stream	| bytes |
	bytes := ByteArray new: 8.
	bytes doubleAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse	!

writeFloat: value to: stream	| bytes |
	bytes := ByteArray new: 4.
	bytes floatAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse!

writeInt16: value to: stream	| bytes |
	bytes := ByteArray new: 2.
	bytes swordAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse!

writeInt32: value to: stream	| bytes |
	bytes := ByteArray new: 4.
	bytes sdwordAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse!

writeInt64: value to: stream	| bytes |
	bytes := ByteArray new: 8.
	bytes sqwordAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse!

writeUint16: value to: stream	| bytes |
	bytes := ByteArray new: 2.
	bytes wordAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse!

writeUint32: value to: stream	| bytes |
	bytes := ByteArray new: 4.
	bytes dwordAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse!

writeUint64: value to: stream	| bytes |
	bytes := ByteArray new: 8.
	bytes qwordAtOffset: 0 put: value.
	stream nextPutAll: bytes reverse! !
!MpDolpPortableUtil categoriesFor: #collectionEquals:with:!public!testing! !
!MpDolpPortableUtil categoriesFor: #encodeTypeMapperClass!factory!public! !
!MpDolpPortableUtil categoriesFor: #readDoubleFrom:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #readFloatFrom:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #readInt16From:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #readInt32From:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #readInt64From:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #readUint16From:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #readUint32From:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #readUint64From:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeDouble:to:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeFloat:to:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeInt16:to:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeInt32:to:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeInt64:to:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeUint16:to:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeUint32:to:!actions/stream!public! !
!MpDolpPortableUtil categoriesFor: #writeUint64:to:!actions/stream!public! !

!MpDolpPortableUtil class methodsFor!

initialize	"MpDolpPortableUtil initialize"	super initialize.	MpPortableUtil dialectSpecificClass: self! !
!MpDolpPortableUtil class categoriesFor: #initialize!class initialization!public! !

MpDolpEncodeTypeMapper guid: (GUID fromString: '{72D18F1C-ADC6-4B92-AEF5-8E7DFC2CB128}')!
MpDolpEncodeTypeMapper comment: ''!
!MpDolpEncodeTypeMapper categoriesForClass!MessagePack-Dolphin-Core! !
!MpDolpEncodeTypeMapper class methodsFor!

defineDoubleActionTo: map	map at: Float put: #writeDouble:! !
!MpDolpEncodeTypeMapper class categoriesFor: #defineDoubleActionTo:!actions for primitives!public! !

"Binary Globals"!

